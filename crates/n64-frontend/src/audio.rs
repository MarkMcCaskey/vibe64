/// Audio output using cpal.
///
/// Captures 16-bit stereo PCM samples from the N64's AI (Audio Interface)
/// and plays them through the host audio device. Resamples from the N64's
/// sample rate (typically ~32 kHz) to the host's native rate.
///
/// Audio is decoupled from the video frame loop: the N64 core invokes an
/// audio callback immediately when AI DMA fires, and the cpal stream pulls
/// from a shared ring buffer on its own thread. This matches real N64
/// hardware where the AI operates asynchronously from the VI.
use std::collections::VecDeque;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

fn f32_to_i16(v: f32) -> i16 {
    (v.clamp(-1.0, 1.0) * i16::MAX as f32) as i16
}

fn f32_to_u16(v: f32) -> u16 {
    (((v.clamp(-1.0, 1.0) + 1.0) * 0.5) * u16::MAX as f32) as u16
}

/// A chunk of N64 audio at a specific sample rate.
struct NativeChunk {
    samples: Vec<i16>,
    rate: u32,
}

struct Resampler {
    /// Native N64 audio chunks waiting to be upsampled.
    native_queue: VecDeque<NativeChunk>,
    /// Residual position in the current chunk.
    resample_pos: f64,
    /// Context from the last sample of the previous chunk for linear interpolation.
    prev_pair: Option<[f32; 2]>,
}

impl Resampler {
    fn new() -> Self {
        Self {
            native_queue: VecDeque::with_capacity(64),
            resample_pos: 0.0,
            prev_pair: None,
        }
    }

    /// Pull upsampled stereo samples (f32) into the output buffer.
    fn pull_samples(&mut self, output: &mut [f32], host_rate: u32, channels: usize) {
        let mut out_idx = 0;
        let out_len = output.len() / channels;

        while out_idx < out_len {
            if let Some(chunk) = self.native_queue.front() {
                let in_pairs = chunk.samples.len() / 2;
                if in_pairs == 0 {
                    self.native_queue.pop_front();
                    continue;
                }

                let step = chunk.rate as f64 / host_rate as f64;
                if !step.is_finite() || step <= 0.0 {
                    self.native_queue.pop_front();
                    continue;
                }

                let has_prev = self.prev_pair.is_some();
                let src_len = in_pairs + if has_prev { 1 } else { 0 };

                let sample_pair = |idx: usize, samples: &[i16], prev: [f32; 2]| -> [f32; 2] {
                    if has_prev {
                        if idx == 0 {
                            prev
                        } else {
                            let i = idx - 1;
                            [
                                samples[i * 2] as f32 / 32768.0,
                                samples[i * 2 + 1] as f32 / 32768.0,
                            ]
                        }
                    } else {
                        [
                            samples[idx * 2] as f32 / 32768.0,
                            samples[idx * 2 + 1] as f32 / 32768.0,
                        ]
                    }
                };

                let prev = self.prev_pair.unwrap_or([0.0, 0.0]);
                while out_idx < out_len && self.resample_pos + 1.0 < src_len as f64 {
                    let idx = self.resample_pos.floor() as usize;
                    let frac = (self.resample_pos - idx as f64) as f32;
                    let s0 = sample_pair(idx, &chunk.samples, prev);
                    let s1 = sample_pair(idx + 1, &chunk.samples, prev);

                    let l = s0[0] + (s1[0] - s0[0]) * frac;
                    let r = s0[1] + (s1[1] - s0[1]) * frac;

                    output[out_idx * channels] = l;
                    if channels > 1 {
                        output[out_idx * channels + 1] = r;
                    }

                    out_idx += 1;
                    self.resample_pos += step;
                }

                if self.resample_pos + 1.0 >= src_len as f64 {
                    // Finished this chunk. Carry over last sample for continuity.
                    let last = in_pairs - 1;
                    self.prev_pair = Some([
                        chunk.samples[last * 2] as f32 / 32768.0,
                        chunk.samples[last * 2 + 1] as f32 / 32768.0,
                    ]);
                    self.resample_pos -= in_pairs as f64;
                    self.native_queue.pop_front();
                }
            } else {
                // Buffer underrun: fill remaining with silence
                while out_idx < out_len {
                    output[out_idx * channels] = 0.0;
                    if channels > 1 {
                        output[out_idx * channels + 1] = 0.0;
                    }
                    out_idx += 1;
                }
            }
        }
    }
}

pub struct AudioOutput {
    _stream: cpal::Stream,
    resampler: Arc<Mutex<Resampler>>,
    host_sample_rate: u32,
    muted: Arc<AtomicBool>,
}

impl AudioOutput {
    /// Create audio output. Returns None if no audio device is available.
    pub fn new() -> Option<Self> {
        let host = cpal::default_host();
        let device = host.default_output_device()?;

        let supported = device.default_output_config().ok()?;
        let sample_format = supported.sample_format();
        let host_sample_rate = supported.sample_rate().0;
        let host_channels = supported.channels();
        let config: cpal::StreamConfig = supported.into();

        log::info!(
            "Audio: {} @ {} Hz, {} ch ({:?})",
            device.name().unwrap_or_default(),
            host_sample_rate,
            host_channels,
            sample_format,
        );

        let resampler = Arc::new(Mutex::new(Resampler::new()));
        let muted = Arc::new(AtomicBool::new(false));

        let channels = config.channels as usize;
        let stream = match sample_format {
            cpal::SampleFormat::F32 => {
                let res_ref = resampler.clone();
                let muted_ref = muted.clone();
                device
                    .build_output_stream(
                        &config,
                        move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                            if muted_ref.load(Ordering::Relaxed) {
                                data.fill(0.0);
                                return;
                            }
                            let mut res = res_ref.lock().unwrap();
                            res.pull_samples(data, host_sample_rate, channels);
                        },
                        |err| log::error!("Audio stream error: {}", err),
                        None,
                    )
                    .ok()?
            }
            cpal::SampleFormat::I16 => {
                let res_ref = resampler.clone();
                let muted_ref = muted.clone();
                device
                    .build_output_stream(
                        &config,
                        move |data: &mut [i16], _: &cpal::OutputCallbackInfo| {
                            if muted_ref.load(Ordering::Relaxed) {
                                data.fill(0);
                                return;
                            }
                            let mut res = res_ref.lock().unwrap();
                            let mut float_buf = vec![0.0f32; data.len()];
                            res.pull_samples(&mut float_buf, host_sample_rate, channels);
                            for (i, &f) in float_buf.iter().enumerate() {
                                data[i] = f32_to_i16(f);
                            }
                        },
                        |err| log::error!("Audio stream error: {}", err),
                        None,
                    )
                    .ok()?
            }
            cpal::SampleFormat::U16 => {
                let res_ref = resampler.clone();
                let muted_ref = muted.clone();
                device
                    .build_output_stream(
                        &config,
                        move |data: &mut [u16], _: &cpal::OutputCallbackInfo| {
                            if muted_ref.load(Ordering::Relaxed) {
                                data.fill(u16::MIN);
                                return;
                            }
                            let mut res = res_ref.lock().unwrap();
                            let mut float_buf = vec![0.0f32; data.len()];
                            res.pull_samples(&mut float_buf, host_sample_rate, channels);
                            for (i, &f) in float_buf.iter().enumerate() {
                                data[i] = f32_to_u16(f);
                            }
                        },
                        |err| log::error!("Audio stream error: {}", err),
                        None,
                    )
                    .ok()?
            }
            _ => {
                log::error!("Audio: unsupported host sample format {:?}", sample_format);
                return None;
            }
        };

        stream.play().ok()?;

        Some(Self {
            _stream: stream,
            resampler,
            host_sample_rate,
            muted,
        })
    }

    /// Create a callback suitable for `N64::set_audio_callback()`.
    pub fn make_callback(&self) -> Box<dyn FnMut(&[i16], u32)> {
        let resampler = self.resampler.clone();
        let host_sample_rate = self.host_sample_rate;

        Box::new(move |samples: &[i16], n64_rate: u32| {
            if samples.is_empty() || n64_rate == 0 {
                return;
            }

            let chunk = NativeChunk {
                samples: samples.to_vec(),
                rate: n64_rate,
            };

            let mut res = resampler.lock().unwrap();
            res.native_queue.push_back(chunk);

            // Cap queue at ~0.5 seconds to prevent growing unbounded.
            let max_chunks = (host_sample_rate / 512).max(10) as usize;
            while res.native_queue.len() > max_chunks {
                res.native_queue.pop_front();
            }
        })
    }

    /// Toggle mute state. Returns new muted status.
    pub fn toggle_mute(&mut self) -> bool {
        let was_muted = self.muted.load(Ordering::Relaxed);
        let new_muted = !was_muted;
        self.muted.store(new_muted, Ordering::Relaxed);
        if new_muted {
            self.resampler.lock().unwrap().native_queue.clear();
        }
        new_muted
    }
}
