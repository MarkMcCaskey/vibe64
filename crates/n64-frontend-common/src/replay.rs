use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;

use n64_core::memory::pif::ControllerState;

const HEADER_LINE: &str = "# n64emu_replay_v1";
const END_FRAME_PREFIX: &str = "# end_frame:";

pub enum Mode {
    None,
    Recording(BufWriter<File>),
    Replaying(VecDeque<(u64, ControllerState)>),
}

pub struct ReplayManager {
    mode: Mode,
    /// Last input state sent to the emulator (or last recorded).
    current_input: ControllerState,
    /// Last frame index seen while recording.
    last_recorded_frame: Option<u64>,
    /// Optional replay length hint (total frames).
    replay_frame_hint: Option<u64>,
    /// Number of input-change events in replay mode.
    replay_event_count: usize,
}

impl ReplayManager {
    pub fn new(record_path: Option<&Path>, replay_path: Option<&Path>) -> Self {
        assert!(
            !(record_path.is_some() && replay_path.is_some()),
            "record and replay modes are mutually exclusive",
        );

        let mode = if let Some(path) = record_path {
            let file =
                File::create(path).unwrap_or_else(|e| panic!("Failed to create replay file: {e}"));
            let mut writer = BufWriter::new(file);
            writeln!(writer, "{HEADER_LINE}")
                .unwrap_or_else(|e| panic!("Failed to write replay header: {e}"));
            Mode::Recording(writer)
        } else if let Some(path) = replay_path {
            let file =
                File::open(path).unwrap_or_else(|e| panic!("Failed to open replay file: {e}"));
            let reader = BufReader::new(file);
            let mut events = VecDeque::new();
            let mut replay_frame_hint = None;

            for line in reader.lines() {
                let line = line.unwrap_or_else(|e| panic!("Failed to read replay line: {e}"));
                if line.trim().is_empty() || line.starts_with('#') {
                    if let Some(rest) = line.strip_prefix(END_FRAME_PREFIX) {
                        replay_frame_hint = rest.trim().parse::<u64>().ok();
                    }
                    continue;
                }

                // Format: frame:buttons(hex):stick_x:stick_y
                let parts: Vec<&str> = line.split(':').collect();
                if parts.len() != 4 {
                    log::warn!("Ignoring malformed replay line: {}", line);
                    continue;
                }

                let Ok(frame) = parts[0].parse::<u64>() else {
                    log::warn!("Ignoring replay line with invalid frame: {}", line);
                    continue;
                };
                let Ok(buttons) = u16::from_str_radix(parts[1], 16) else {
                    log::warn!("Ignoring replay line with invalid buttons: {}", line);
                    continue;
                };
                let Ok(stick_x) = parts[2].parse::<i8>() else {
                    log::warn!("Ignoring replay line with invalid stick_x: {}", line);
                    continue;
                };
                let Ok(stick_y) = parts[3].parse::<i8>() else {
                    log::warn!("Ignoring replay line with invalid stick_y: {}", line);
                    continue;
                };

                events.push_back((
                    frame,
                    ControllerState {
                        buttons,
                        stick_x,
                        stick_y,
                    },
                ));
            }

            let replay_event_count = events.len();
            let replay_frame_hint =
                replay_frame_hint.or_else(|| events.back().map(|(f, _)| f.saturating_add(1)));

            log::info!(
                "Loaded replay: events={}, frame_hint={:?}",
                replay_event_count,
                replay_frame_hint
            );

            return Self {
                mode: Mode::Replaying(events),
                current_input: ControllerState::default(),
                last_recorded_frame: None,
                replay_frame_hint,
                replay_event_count,
            };
        } else {
            Mode::None
        };

        Self {
            mode,
            current_input: ControllerState::default(),
            last_recorded_frame: None,
            replay_frame_hint: None,
            replay_event_count: 0,
        }
    }

    pub fn is_replaying(&self) -> bool {
        matches!(self.mode, Mode::Replaying(_))
    }

    pub fn replay_frame_count_hint(&self) -> Option<u64> {
        self.replay_frame_hint
    }

    pub fn replay_event_count(&self) -> usize {
        self.replay_event_count
    }

    /// Update controller state for `frame`.
    ///
    /// Recording: writes only input changes.
    /// Replaying: applies all events up to `frame`.
    pub fn update_input(&mut self, frame: u64, input: &mut ControllerState) {
        match &mut self.mode {
            Mode::None => {}
            Mode::Recording(writer) => {
                self.last_recorded_frame = Some(frame);

                if input.buttons != self.current_input.buttons
                    || input.stick_x != self.current_input.stick_x
                    || input.stick_y != self.current_input.stick_y
                {
                    self.current_input.buttons = input.buttons;
                    self.current_input.stick_x = input.stick_x;
                    self.current_input.stick_y = input.stick_y;

                    writeln!(
                        writer,
                        "{}:{:04X}:{}:{}",
                        frame, input.buttons, input.stick_x, input.stick_y
                    )
                    .expect("Failed to write replay event");
                }
            }
            Mode::Replaying(events) => {
                while let Some((event_frame, _)) = events.front() {
                    if *event_frame <= frame {
                        let (_, state) = events.pop_front().expect("front just matched");
                        self.current_input = state;
                    } else {
                        break;
                    }
                }

                input.buttons = self.current_input.buttons;
                input.stick_x = self.current_input.stick_x;
                input.stick_y = self.current_input.stick_y;
            }
        }
    }
}

impl Drop for ReplayManager {
    fn drop(&mut self) {
        if let Mode::Recording(writer) = &mut self.mode {
            if let Some(last_frame) = self.last_recorded_frame {
                let frame_count = last_frame.saturating_add(1);
                let _ = writeln!(writer, "{END_FRAME_PREFIX}{}", frame_count);
            }
            let _ = writer.flush();
        }
    }
}
