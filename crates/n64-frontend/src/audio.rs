/// Audio output using cpal.
///
/// Captures 16-bit stereo PCM samples from the N64's AI (Audio Interface)
/// and plays them through the host audio device. Resamples from the N64's
/// sample rate (typically ~32 kHz) to the host's native rate.
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

fn f32_to_i16(v: f32) -> i16 {
    (v.clamp(-1.0, 1.0) * i16::MAX as f32) as i16
}

fn f32_to_u16(v: f32) -> u16 {
    (((v.clamp(-1.0, 1.0) + 1.0) * 0.5) * u16::MAX as f32) as u16
}

pub struct AudioOutput {
    _stream: cpal::Stream,
    buffer: Arc<Mutex<VecDeque<f32>>>,
    host_sample_rate: u32,
    host_channels: u16,
    /// Fractional source position carried across push calls.
    resample_pos: f64,
    /// Last stereo pair from previous push (for boundary interpolation).
    prev_pair: Option<[f32; 2]>,
    pub muted: bool,
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

        let buffer: Arc<Mutex<VecDeque<f32>>> =
            Arc::new(Mutex::new(VecDeque::with_capacity(65536)));
        let channels = config.channels as usize;
        let stream = match sample_format {
            cpal::SampleFormat::F32 => {
                let buf_ref = buffer.clone();
                device
                    .build_output_stream(
                        &config,
                        move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                            let mut buf = buf_ref.lock().unwrap();
                            for frame in data.chunks_mut(channels) {
                                let left = buf.pop_front().unwrap_or(0.0);
                                let right = buf.pop_front().unwrap_or(0.0);
                                frame[0] = left;
                                if channels > 1 {
                                    frame[1] = right;
                                }
                                for ch in frame.iter_mut().skip(2) {
                                    *ch = 0.0;
                                }
                            }
                        },
                        |err| log::error!("Audio stream error: {}", err),
                        None,
                    )
                    .ok()?
            }
            cpal::SampleFormat::I16 => {
                let buf_ref = buffer.clone();
                device
                    .build_output_stream(
                        &config,
                        move |data: &mut [i16], _: &cpal::OutputCallbackInfo| {
                            let mut buf = buf_ref.lock().unwrap();
                            for frame in data.chunks_mut(channels) {
                                let left = buf.pop_front().unwrap_or(0.0);
                                let right = buf.pop_front().unwrap_or(0.0);
                                frame[0] = f32_to_i16(left);
                                if channels > 1 {
                                    frame[1] = f32_to_i16(right);
                                }
                                for ch in frame.iter_mut().skip(2) {
                                    *ch = 0;
                                }
                            }
                        },
                        |err| log::error!("Audio stream error: {}", err),
                        None,
                    )
                    .ok()?
            }
            cpal::SampleFormat::U16 => {
                let buf_ref = buffer.clone();
                device
                    .build_output_stream(
                        &config,
                        move |data: &mut [u16], _: &cpal::OutputCallbackInfo| {
                            let mut buf = buf_ref.lock().unwrap();
                            for frame in data.chunks_mut(channels) {
                                let left = buf.pop_front().unwrap_or(0.0);
                                let right = buf.pop_front().unwrap_or(0.0);
                                frame[0] = f32_to_u16(left);
                                if channels > 1 {
                                    frame[1] = f32_to_u16(right);
                                }
                                for ch in frame.iter_mut().skip(2) {
                                    *ch = u16::MIN;
                                }
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
            buffer,
            host_sample_rate,
            host_channels,
            resample_pos: 0.0,
            prev_pair: None,
            muted: false, // unmuted by default
        })
    }

    /// Push N64 audio samples into the playback buffer.
    /// `samples`: 16-bit signed stereo PCM (L,R,L,R...) from N64 AI DMA.
    /// `n64_rate`: N64 sample rate in Hz (typically ~32000).
    pub fn push_samples(&mut self, samples: &[i16], n64_rate: u32) {
        if self.muted || samples.is_empty() || n64_rate == 0 {
            return;
        }

        // Resample from n64_rate to host_sample_rate with phase continuity
        // across calls, to avoid zipper noise at DMA boundaries.
        let in_pairs = samples.len() / 2;
        if in_pairs == 0 {
            return;
        }

        let step = n64_rate as f64 / self.host_sample_rate as f64;
        if !step.is_finite() || step <= 0.0 {
            return;
        }

        let has_prev = self.prev_pair.is_some();
        let src_len = in_pairs + if has_prev { 1 } else { 0 };
        let mut produced: Vec<f32> = Vec::with_capacity(in_pairs * 4);
        if src_len >= 2 {
            let prev = self.prev_pair.unwrap_or([0.0, 0.0]);
            let sample_pair = |idx: usize| -> [f32; 2] {
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

            let mut src_pos = self.resample_pos.max(0.0);
            while src_pos + 1.0 < src_len as f64 {
                let idx = src_pos.floor() as usize;
                let frac = (src_pos - idx as f64) as f32;
                let s0 = sample_pair(idx);
                let s1 = sample_pair(idx + 1);
                produced.push(s0[0] + (s1[0] - s0[0]) * frac);
                produced.push(s0[1] + (s1[1] - s0[1]) * frac);
                src_pos += step;
            }

            self.resample_pos = src_pos - (src_len as f64 - 1.0);
            if !self.resample_pos.is_finite() || self.resample_pos < 0.0 {
                self.resample_pos = 0.0;
            }
        }

        // Keep one input pair as interpolation context for the next push.
        let last = in_pairs - 1;
        self.prev_pair = Some([
            samples[last * 2] as f32 / 32768.0,
            samples[last * 2 + 1] as f32 / 32768.0,
        ]);

        let mut buf = self.buffer.lock().unwrap();
        buf.extend(produced);

        // Cap buffer at ~0.5 seconds to prevent growing unbounded.
        // If we're producing faster than consuming, drop oldest samples.
        let max_samples = (self.host_sample_rate as usize * self.host_channels as usize) / 2;
        while buf.len() > max_samples {
            buf.pop_front();
        }
    }

    /// Toggle mute state. Returns new muted status.
    pub fn toggle_mute(&mut self) -> bool {
        self.muted = !self.muted;
        if self.muted {
            // Clear buffer when muting to avoid stale audio on unmute
            self.buffer.lock().unwrap().clear();
            self.resample_pos = 0.0;
            self.prev_pair = None;
        }
        self.muted
    }
}
