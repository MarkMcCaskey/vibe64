/// Audio output using cpal.
///
/// Captures 16-bit stereo PCM samples from the N64's AI (Audio Interface)
/// and plays them through the host audio device. Resamples from the N64's
/// sample rate (typically ~32 kHz) to the host's native rate.

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

pub struct AudioOutput {
    _stream: cpal::Stream,
    buffer: Arc<Mutex<VecDeque<f32>>>,
    host_sample_rate: u32,
    host_channels: u16,
    pub muted: bool,
}

impl AudioOutput {
    /// Create audio output. Returns None if no audio device is available.
    /// Audio starts muted by default.
    pub fn new() -> Option<Self> {
        let host = cpal::default_host();
        let device = host.default_output_device()?;

        let config = device.default_output_config().ok()?;
        let host_sample_rate = config.sample_rate().0;
        let host_channels = config.channels();

        log::info!(
            "Audio: {} @ {} Hz, {} ch",
            device.name().unwrap_or_default(),
            host_sample_rate,
            host_channels
        );

        let buffer: Arc<Mutex<VecDeque<f32>>> =
            Arc::new(Mutex::new(VecDeque::with_capacity(65536)));
        let buf_ref = buffer.clone();

        let channels = host_channels as usize;
        let stream = device
            .build_output_stream(
                &config.into(),
                move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                    let mut buf = buf_ref.lock().unwrap();
                    for frame in data.chunks_mut(channels) {
                        // Our buffer stores stereo f32 pairs (L, R)
                        let left = buf.pop_front().unwrap_or(0.0);
                        let right = buf.pop_front().unwrap_or(0.0);
                        frame[0] = left;
                        if channels > 1 {
                            frame[1] = right;
                        }
                        // Fill extra channels with silence
                        for ch in frame.iter_mut().skip(2) {
                            *ch = 0.0;
                        }
                    }
                },
                |err| log::error!("Audio stream error: {}", err),
                None,
            )
            .ok()?;

        stream.play().ok()?;

        Some(Self {
            _stream: stream,
            buffer,
            host_sample_rate,
            host_channels,
            muted: true, // muted by default
        })
    }

    /// Push N64 audio samples into the playback buffer.
    /// `samples`: 16-bit signed stereo PCM (L,R,L,R...) from N64 AI DMA.
    /// `n64_rate`: N64 sample rate in Hz (typically ~32000).
    pub fn push_samples(&self, samples: &[i16], n64_rate: u32) {
        if self.muted || samples.is_empty() || n64_rate == 0 {
            return;
        }

        let mut buf = self.buffer.lock().unwrap();

        // Resample from n64_rate to host_sample_rate using linear interpolation.
        // Input is stereo pairs, output is stereo pairs.
        let in_pairs = samples.len() / 2;
        if in_pairs == 0 {
            return;
        }

        let ratio = self.host_sample_rate as f64 / n64_rate as f64;
        let out_pairs = (in_pairs as f64 * ratio) as usize;

        for i in 0..out_pairs {
            let src_pos = i as f64 / ratio;
            let idx = src_pos as usize;
            let frac = (src_pos - idx as f64) as f32;

            let idx0 = idx.min(in_pairs - 1);
            let idx1 = (idx + 1).min(in_pairs - 1);

            let l0 = samples[idx0 * 2] as f32 / 32768.0;
            let r0 = samples[idx0 * 2 + 1] as f32 / 32768.0;
            let l1 = samples[idx1 * 2] as f32 / 32768.0;
            let r1 = samples[idx1 * 2 + 1] as f32 / 32768.0;

            buf.push_back(l0 + (l1 - l0) * frac);
            buf.push_back(r0 + (r1 - r0) * frac);
        }

        // Cap buffer at ~0.5 seconds to prevent growing unbounded.
        // If we're producing faster than consuming, drop oldest samples.
        let max_samples = self.host_sample_rate as usize * self.host_channels as usize;
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
        }
        self.muted
    }
}
