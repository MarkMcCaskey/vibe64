mod audio;
mod debug_overlay;
mod gamepad;
mod tui;

use std::path::PathBuf;

use pixels::{Pixels, SurfaceTexture};
use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::window::{Window, WindowId};

use n64_core::memory::pif::buttons as n64_buttons;

const N64_WIDTH: u32 = 320;
const N64_HEIGHT: u32 = 240;
const SCALE: u32 = 2;
const SPEED_STEP_PERCENT: u32 = 25;
const MIN_SPEED_PERCENT: u32 = 25;
const MAX_SPEED_PERCENT: u32 = 400;
const DEFAULT_SPEED_PERCENT: u32 = 100;
const SPEED_MESSAGE_MS: u64 = 1400;

struct App {
    n64: n64_core::N64,
    window: Option<&'static Window>,
    pixels: Option<Pixels<'static>>,
    audio: Option<audio::AudioOutput>,
    last_frame_time: std::time::Instant,
    /// Track held arrow keys for proper analog stick (survives simultaneous presses)
    arrow_up: bool,
    arrow_down: bool,
    arrow_left: bool,
    arrow_right: bool,
    /// Current save state slot (0-9)
    save_slot: u8,
    /// Keyboard-maintained controller buttons.
    keyboard_buttons: u16,
    /// Keyboard-maintained analog stick state.
    keyboard_stick_x: i8,
    keyboard_stick_y: i8,
    /// Optional gamepad backend.
    gamepad: Option<gamepad::GamepadInput>,
    /// Emulation speed in percent (25..400, step 25).
    speed_percent: u32,
    /// Fractional frame budget for sub-1x and non-integer speed multipliers.
    speed_frame_budget: f32,
    /// If set and not expired, draw a temporary on-screen speed message.
    speed_message_until: Option<std::time::Instant>,
}

impl App {
    fn speed_multiplier(&self) -> f32 {
        self.speed_percent as f32 / 100.0
    }

    fn bump_speed(&mut self, delta_percent: i32) {
        let next = (self.speed_percent as i32 + delta_percent)
            .clamp(MIN_SPEED_PERCENT as i32, MAX_SPEED_PERCENT as i32) as u32;
        if next != self.speed_percent {
            self.speed_percent = next;
        }
        self.speed_message_until =
            Some(std::time::Instant::now() + std::time::Duration::from_millis(SPEED_MESSAGE_MS));
        eprintln!("Emulation speed: {}%", self.speed_percent);
    }

    fn reset_speed(&mut self) {
        self.speed_percent = DEFAULT_SPEED_PERCENT;
        self.speed_message_until =
            Some(std::time::Instant::now() + std::time::Duration::from_millis(SPEED_MESSAGE_MS));
        eprintln!("Emulation speed: {}%", self.speed_percent);
    }

    fn sync_controller_input(&mut self) {
        let mut gp = gamepad::GamepadState::default();
        if let Some(gamepad) = &mut self.gamepad {
            gp = gamepad.poll();
        }

        let ctrl = &mut self.n64.bus.pif.controller;
        ctrl.buttons = self.keyboard_buttons | gp.buttons;
        ctrl.stick_x = combine_axis(self.keyboard_stick_x, gp.stick_x);
        ctrl.stick_y = combine_axis(self.keyboard_stick_y, gp.stick_y);
    }
}

fn combine_axis(kb: i8, gp: i8) -> i8 {
    if gp.abs() >= kb.abs() {
        gp
    } else {
        kb
    }
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }

        let size = LogicalSize::new(N64_WIDTH * SCALE, N64_HEIGHT * SCALE);
        let attrs = Window::default_attributes()
            .with_title("N64")
            .with_inner_size(size)
            .with_min_inner_size(size);

        let window: &'static Window = Box::leak(Box::new(
            event_loop.create_window(attrs).expect("create window"),
        ));

        let physical = window.inner_size();
        let surface = SurfaceTexture::new(physical.width, physical.height, window);
        let pixels = Pixels::new(N64_WIDTH, N64_HEIGHT, surface).expect("create pixels");

        self.window = Some(window);
        self.pixels = Some(pixels);
        window.request_redraw();
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        match event {
            WindowEvent::CloseRequested => {
                self.n64.save_eeprom();
                self.n64.save_flash();
                self.n64.save_sram();
                event_loop.exit();
            }
            WindowEvent::Resized(size) => {
                if let Some(pixels) = &mut self.pixels {
                    let _ = pixels.resize_surface(size.width.max(1), size.height.max(1));
                }
            }
            WindowEvent::KeyboardInput { event, .. } => {
                if let PhysicalKey::Code(key) = event.physical_key {
                    let pressed = event.state.is_pressed();

                    // Button mapping (physical key positions — layout-independent)
                    let btn = match key {
                        KeyCode::KeyX => Some(n64_buttons::A),
                        KeyCode::KeyZ => Some(n64_buttons::B),
                        KeyCode::Space => Some(n64_buttons::Z),
                        KeyCode::Enter => Some(n64_buttons::START),
                        KeyCode::ShiftLeft => Some(n64_buttons::L),
                        KeyCode::ShiftRight => Some(n64_buttons::R),
                        KeyCode::KeyI => Some(n64_buttons::C_UP),
                        KeyCode::KeyK => Some(n64_buttons::C_DOWN),
                        KeyCode::KeyJ => Some(n64_buttons::C_LEFT),
                        KeyCode::KeyL => Some(n64_buttons::C_RIGHT),
                        KeyCode::KeyW => Some(n64_buttons::D_UP),
                        KeyCode::KeyS => Some(n64_buttons::D_DOWN),
                        KeyCode::KeyA => Some(n64_buttons::D_LEFT),
                        KeyCode::KeyD => Some(n64_buttons::D_RIGHT),
                        _ => None,
                    };
                    if let Some(b) = btn {
                        if pressed {
                            self.keyboard_buttons |= b;
                        } else {
                            self.keyboard_buttons &= !b;
                        }
                    }

                    // Analog stick from arrow keys (±80 range)
                    // Track each key independently so simultaneous presses work
                    match key {
                        KeyCode::ArrowUp => self.arrow_up = pressed,
                        KeyCode::ArrowDown => self.arrow_down = pressed,
                        KeyCode::ArrowLeft => self.arrow_left = pressed,
                        KeyCode::ArrowRight => self.arrow_right = pressed,
                        _ => {}
                    }
                    self.keyboard_stick_y = match (self.arrow_up, self.arrow_down) {
                        (true, false) => 80,
                        (false, true) => -80,
                        _ => 0,
                    };
                    self.keyboard_stick_x = match (self.arrow_right, self.arrow_left) {
                        (true, false) => 80,
                        (false, true) => -80,
                        _ => 0,
                    };

                    // Escape to quit
                    if key == KeyCode::Escape && pressed {
                        self.n64.save_eeprom();
                        self.n64.save_flash();
                        self.n64.save_sram();
                        event_loop.exit();
                    }
                    // P = save screenshot
                    if key == KeyCode::KeyP && pressed {
                        save_screenshot(&self.n64);
                    }
                    // M = toggle audio mute
                    if key == KeyCode::KeyM && pressed {
                        if let Some(audio) = &mut self.audio {
                            let muted = audio.toggle_mute();
                            log::info!("Audio: {}", if muted { "muted" } else { "unmuted" });
                        }
                    }
                    // Save state controls
                    if pressed {
                        match key {
                            KeyCode::F5 => match self.n64.save_state(self.save_slot) {
                                Ok(()) => eprintln!("State saved to slot {}", self.save_slot),
                                Err(e) => eprintln!("Save state error: {}", e),
                            },
                            KeyCode::F6 => {
                                self.save_slot = (self.save_slot + 1) % 10;
                                eprintln!("Save slot: {}", self.save_slot);
                            }
                            KeyCode::F7 => match self.n64.load_state(self.save_slot) {
                                Ok(()) => eprintln!("State loaded from slot {}", self.save_slot),
                                Err(e) => eprintln!("Load state error: {}", e),
                            },
                            KeyCode::F8 => {
                                self.save_slot = if self.save_slot == 0 {
                                    9
                                } else {
                                    self.save_slot - 1
                                };
                                eprintln!("Save slot: {}", self.save_slot);
                            }
                            // Speed controls for debugging:
                            // '-' slower, '=' faster, '0' reset to 100%.
                            KeyCode::Minus | KeyCode::NumpadSubtract => {
                                self.bump_speed(-(SPEED_STEP_PERCENT as i32));
                            }
                            KeyCode::Equal | KeyCode::NumpadAdd => {
                                self.bump_speed(SPEED_STEP_PERCENT as i32);
                            }
                            KeyCode::Digit0 | KeyCode::Numpad0 => {
                                self.reset_speed();
                            }
                            _ => {}
                        }
                    }
                    // F1-F4 = debug overlays
                    if pressed {
                        debug_overlay::handle_f_key(&mut self.n64.debug, key);
                    }

                    self.sync_controller_input();
                }
            }
            WindowEvent::RedrawRequested => {
                self.sync_controller_input();

                // Run a variable number of emulated frames per display refresh.
                // This supports speed control in 25% increments, including <1x.
                self.speed_frame_budget += self.speed_multiplier();
                let frames_to_run = self.speed_frame_budget.floor() as u32;
                self.speed_frame_budget -= frames_to_run as f32;

                for _ in 0..frames_to_run {
                    self.n64.run_frame();
                }

                // Audio is pushed asynchronously by the AI DMA callback,
                // no per-frame drain needed.

                // FPS timing
                let now = std::time::Instant::now();
                let dt = now.duration_since(self.last_frame_time).as_secs_f64() * 1000.0;
                self.last_frame_time = now;
                let pos = self.n64.debug.fps_write_pos;
                self.n64.debug.fps_samples[pos] = dt;
                self.n64.debug.fps_write_pos = (pos + 1) % 60;
                let old_frame_count = self.n64.debug.frame_count;
                self.n64.debug.frame_count += frames_to_run as u64;

                // Periodic EEPROM save (every ~5 seconds)
                if frames_to_run > 0 && (old_frame_count / 300 != self.n64.debug.frame_count / 300)
                {
                    self.n64.save_eeprom();
                    self.n64.save_flash();
                    self.n64.save_sram();
                }

                if let Some(pixels) = &mut self.pixels {
                    let frame = pixels.frame_mut();
                    blit_framebuffer(&self.n64, frame);
                    debug_overlay::draw_overlays(frame, &mut self.n64.debug, &self.n64.bus);
                    if self.speed_message_until.is_some_and(|t| now <= t) {
                        let msg = format!("Speed: {}%", self.speed_percent);
                        debug_overlay::draw_status_message(frame, &msg);
                    }
                    if let Err(e) = pixels.render() {
                        log::error!("Render error: {}", e);
                        event_loop.exit();
                    }
                }

                if let Some(window) = &self.window {
                    window.request_redraw();
                }
            }
            _ => {}
        }
    }
}

/// Save the current N64 framebuffer as a PPM screenshot.
/// Uses the VI origin, but falls back to the renderer's last completed frame
/// if the VI buffer appears empty (caught mid-frame-clear in diagnostic mode).
fn save_screenshot(n64: &n64_core::N64) {
    let vi_origin = n64.vi_origin() as usize;
    let width = n64.vi_width().max(1) as usize;
    let format = n64.vi_pixel_format();
    let rdram = n64.rdram_data();

    eprintln!(
        "Screenshot: vi_origin={:#X} width={} format={}",
        vi_origin, width, format
    );
    let snapshot = &n64.bus.renderer.best_frame_snapshot;
    eprintln!(
        "  Renderer: ci={:#X}, best_snapshot={} px",
        n64.bus.renderer.color_image_addr, n64.bus.renderer.best_frame_nonblack
    );

    // Check if VI origin has color data, or fall back to best saved snapshot
    let use_snapshot = if format == 2 && vi_origin > 0 && vi_origin + 320 * 240 * 2 < rdram.len() {
        let mut max_color = 0u16;
        for i in (0..320 * 240 * 2).step_by(64) {
            let px = u16::from_be_bytes([rdram[vi_origin + i], rdram[vi_origin + i + 1]]);
            max_color = max_color.max(px >> 1);
        }
        max_color == 0 && !snapshot.is_empty()
    } else {
        false
    };

    if use_snapshot {
        eprintln!(
            "  VI blank, using best snapshot ({} nonblack pixels)",
            n64.bus.renderer.best_frame_nonblack
        );
    }

    let fb_size = width * 240 * if format == 3 { 4 } else { 2 };
    if !use_snapshot && (vi_origin == 0 || format < 2 || vi_origin + fb_size >= rdram.len()) {
        eprintln!("  Cannot save: invalid framebuffer");
        return;
    }

    let mut ppm = b"P6\n320 240\n255\n".to_vec();
    for y in 0..240 {
        for x in 0..320usize {
            if use_snapshot {
                let off = (y * width + x) * 2;
                if off + 1 < snapshot.len() {
                    let pixel = u16::from_be_bytes([snapshot[off], snapshot[off + 1]]);
                    let r = ((pixel >> 11) & 0x1F) as u8;
                    let g = ((pixel >> 6) & 0x1F) as u8;
                    let b = ((pixel >> 1) & 0x1F) as u8;
                    ppm.push((r << 3) | (r >> 2));
                    ppm.push((g << 3) | (g >> 2));
                    ppm.push((b << 3) | (b >> 2));
                } else {
                    ppm.extend_from_slice(&[0, 0, 0]);
                }
            } else {
                match format {
                    2 => {
                        let off = vi_origin + (y * width + x) * 2;
                        let pixel = u16::from_be_bytes([rdram[off], rdram[off + 1]]);
                        let r = ((pixel >> 11) & 0x1F) as u8;
                        let g = ((pixel >> 6) & 0x1F) as u8;
                        let b = ((pixel >> 1) & 0x1F) as u8;
                        ppm.push((r << 3) | (r >> 2));
                        ppm.push((g << 3) | (g >> 2));
                        ppm.push((b << 3) | (b >> 2));
                    }
                    3 => {
                        let off = vi_origin + (y * width + x) * 4;
                        ppm.push(rdram[off]);
                        ppm.push(rdram[off + 1]);
                        ppm.push(rdram[off + 2]);
                    }
                    _ => {
                        ppm.extend_from_slice(&[0, 0, 0]);
                    }
                }
            }
        }
    }
    std::fs::write("screenshot.ppm", &ppm).ok();
    eprintln!(
        "  Saved screenshot.ppm ({})",
        if use_snapshot {
            "from snapshot"
        } else {
            "from VI"
        }
    );
}

/// Read N64 framebuffer from RDRAM and convert to RGBA8888 for display.
fn blit_framebuffer(n64: &n64_core::N64, dest: &mut [u8]) {
    let format = n64.vi_pixel_format();
    let origin = n64.vi_origin() as usize;
    let width = n64.vi_width().max(1) as usize;
    let rdram = n64.rdram_data();

    if format < 2 || origin == 0 {
        dest.fill(0);
        return;
    }

    let height = N64_HEIGHT as usize;
    let fb_width = N64_WIDTH as usize;

    for y in 0..height {
        for x in 0..fb_width {
            let dest_idx = (y * fb_width + x) * 4;
            if dest_idx + 3 >= dest.len() {
                break;
            }

            match format {
                2 => {
                    let src_offset = origin + (y * width + x) * 2;
                    if src_offset + 1 >= rdram.len() {
                        continue;
                    }
                    let pixel = u16::from_be_bytes([rdram[src_offset], rdram[src_offset + 1]]);
                    let r5 = (pixel >> 11) & 0x1F;
                    let g5 = (pixel >> 6) & 0x1F;
                    let b5 = (pixel >> 1) & 0x1F;
                    dest[dest_idx] = ((r5 << 3) | (r5 >> 2)) as u8;
                    dest[dest_idx + 1] = ((g5 << 3) | (g5 >> 2)) as u8;
                    dest[dest_idx + 2] = ((b5 << 3) | (b5 >> 2)) as u8;
                    dest[dest_idx + 3] = 0xFF;
                }
                3 => {
                    let src_offset = origin + (y * width + x) * 4;
                    if src_offset + 3 >= rdram.len() {
                        continue;
                    }
                    dest[dest_idx] = rdram[src_offset];
                    dest[dest_idx + 1] = rdram[src_offset + 1];
                    dest[dest_idx + 2] = rdram[src_offset + 2];
                    dest[dest_idx + 3] = 0xFF;
                }
                _ => {}
            }
        }
    }
}

fn main() {
    env_logger::init();

    let args: Vec<String> = std::env::args().collect();
    let use_tui = args.iter().any(|a| a == "--tui");
    let use_test = args.iter().any(|a| a == "--test");
    let use_diag = args.iter().any(|a| a == "--diag");
    let rom_path = args
        .iter()
        .skip(1)
        .find(|a| !a.starts_with("--"))
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            eprintln!("Usage: n64-frontend [--tui|--test] <rom_path>");
            std::process::exit(1);
        });

    let mut n64 = match n64_core::N64::new(&rom_path) {
        Ok(n64) => n64,
        Err(e) => {
            eprintln!("Failed to load ROM: {}", e);
            std::process::exit(1);
        }
    };

    if use_diag {
        let total_steps = std::env::var("N64_DIAG_STEPS")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(100_000_000u64);
        eprintln!(
            "=== Boot diagnostic ({}M steps) ===",
            total_steps / 1_000_000
        );

        // PC histogram: sample every 1024 steps to keep overhead low
        let mut pc_hist: std::collections::HashMap<u32, u64> = std::collections::HashMap::new();
        let mut exception_count: u64 = 0;
        let mut rsp_task_pcs: Vec<(u32, u64)> = Vec::new();
        // Track GFX vs audio RSP task types
        let mut gfx_task_count: u32 = 0;
        let mut audio_task_count: u32 = 0;
        let mut other_task_count: u32 = 0;
        let mut dp_interrupt_count: u64 = 0;
        let _sp_status_write_count: u64 = 0;
        let mut first_dp_step: u64 = 0;
        // Monitor gIntrMesgQueue for message arrivals
        let intr_q_valid_addr = 0x0033AE10usize; // validCount at queue+0x08
        let intr_q_first_addr = 0x0033AE14usize; // first at queue+0x0C
        let intr_q_msgs_addr = 0x0033AEC8usize; // msg buffer
        let mut prev_valid: u32 = 0;
        let mut msg_log: Vec<(u64, u32)> = Vec::new(); // (step, msg_value)
                                                       // === SI queue monitoring (every step) ===
                                                       // Queue A: 0x80367138 — validCount at phys 0x00367140
                                                       // Queue B: 0x80367158 — validCount at phys 0x00367160
                                                       // These are the two cap=1 queues involved in SM64's SI access.
                                                       // Log format: (step, new_validCount, pc32)
        let si_q_a_valid_addr = 0x00367140usize;
        let si_q_b_valid_addr = 0x00367160usize;
        let mut prev_si_q_a: u32 = 0;
        let mut prev_si_q_b: u32 = 0;
        let mut si_q_a_log: Vec<(u64, u32, u32)> = Vec::new();
        let mut si_q_b_log: Vec<(u64, u32, u32)> = Vec::new();
        let mut exec_dl_hits: u64 = 0;
        let mut exec_dl_first_step: u64 = 0;
        // Track SI DMA timing: (step, dram_addr, direction)
        let mut si_dma_steps: Vec<(u64, u32, u8)> = Vec::new();
        // Track MI_INTR SI transitions: (step, set_or_clear, pc32)
        let mut mi_si_log: Vec<(u64, bool, u32)> = Vec::new();
        let mut prev_mi_si: bool = false;
        // Track osRecvMesg and osSendMesg calls during EEPROM window
        // Log: (step, func: 'R'ecv/'S'end, $a0=queue_addr, $a1=msg)
        let mut mesg_calls: Vec<(u64, char, u32, u32)> = Vec::new();
        // PC trace for critical window (after last DMA wait, before blocked acquire)
        let mut pc_trace: Vec<(u64, u32)> = Vec::new();
        let mut pc_trace_active = false;
        // Non-idle PC histogram for window after last GFX task
        let mut nonidle_pc_hist: std::collections::HashMap<u32, u64> =
            std::collections::HashMap::new();
        let nonidle_window_start: u64 = 120_000_000;
        let nonidle_window_end: u64 = 200_000_000;
        // Monitor GFX completion queue at 0x8033B028 (validCount at phys 0x0033B030)
        // Decoded from: LUI $a0,0x8034; ADDIU $a0,$a0,0xB028 (sign-extends: 0x8034_0000 + (-0x4FD8) = 0x8033_B028)
        let gfx_q_valid_addr = 0x0033B030usize;
        let mut prev_gfx_q_valid: u32 = 0;
        let mut gfx_q_log: Vec<(u64, u32, u32)> = Vec::new(); // (step, validCount, pc)
                                                              // Track osRecvMesg calls (0x80322800) after last GFX task to find blocking queue
        let mut recv_mesg_log: Vec<(u64, u32, u32)> = Vec::new(); // (step, $a0=queue, $a2=block)
                                                                  // Monitor rendering gate variable at phys 0x0032D584 (virt 0x8032D584)
                                                                  // When == 0: rendering enabled. When != 0: rendering skipped.
        let render_gate_addr = 0x0032D584usize;
        let mut prev_render_gate: u8 = 0xFF;
        let mut render_gate_log: Vec<(u64, u8, u32)> = Vec::new(); // (step, value, pc)
        let mut render_gate_check_count: u64 = 0;
        let mut render_gate_check_log: Vec<(u64, u8, u64)> = Vec::new(); // (step, gate_val, check#)
        let mut game_loop_body_hits: u64 = 0;
        // Track PI DMA timing
        let mut pi_dma_steps: Vec<u64> = Vec::new();
        let mut prev_pi_count = 0u32;
        // Game thread state monitoring
        // Thread at 0x8033AA90 (phys 0x0033AA90), state u16 at offset 0x10 (phys 0x0033AAA0)
        // 1=STOP, 2=RUNNABLE, 4=RUNNING, 8=WAITING
        let game_thread_state_addr = 0x0033AAA0usize;
        let mut prev_game_thread_state: u16 = 0;
        let mut game_thread_stop_step: u64 = 0;
        // PC trace around crash window
        let mut crash_pc_trace: Vec<(u64, u32)> = Vec::new();
        // VBlank queue monitoring: 0x8033B010, validCount at phys 0x0033B018
        let vblank_q_valid_addr = 0x0033B018usize;
        let mut prev_vblank_q_valid: u32 = 0;
        let mut vblank_q_log: Vec<(u64, u32, u32)> = Vec::new();
        // Track display_and_vsync sub-calls
        let mut dav_entry_count: u64 = 0; // 0x80248090
        let mut dav_recv1_count: u64 = 0; // 0x802480B0 (GFX done recv)
        let mut dav_exec_count: u64 = 0; // 0x802480EC (exec_display_list)
        let mut dav_recv2_count: u64 = 0; // 0x8024810C (VBlank recv)
        let mut dav_log: Vec<(u64, &str, u64)> = Vec::new(); // (step, event, count)
                                                             // Press Start at 200M to open menu, release at 210M
        let start_press_step: u64 = 200_000_000;
        let start_release_step: u64 = 210_000_000;
        for i in 0..total_steps {
            // Simulate Start button press for menu testing
            if i == start_press_step {
                n64.bus.pif.controller.buttons |= n64_core::memory::pif::buttons::START;
                eprintln!("  ** Pressing START at step {}", i);
            }
            if i == start_release_step {
                n64.bus.pif.controller.buttons &= !n64_core::memory::pif::buttons::START;
                eprintln!("  ** Releasing START at step {}", i);
            }

            let pc32 = n64.cpu.pc as u32;
            if i & 0x3FF == 0 {
                *pc_hist.entry(pc32).or_default() += 1;
            }
            if pc32 == 0x80000180 {
                exception_count += 1;
            }
            // Non-idle PC histogram: sample every 256 steps, exclude idle loop
            if i >= nonidle_window_start && i < nonidle_window_end && i & 0xFF == 0 {
                if pc32 != 0x80246DDC && pc32 != 0x80246DD8 {
                    *nonidle_pc_hist.entry(pc32).or_default() += 1;
                }
            }
            // Track osRecvMesg calls after frame 3 starts
            if pc32 == 0x80322800 && i > 63_000_000 && recv_mesg_log.len() < 200 {
                let a0 = n64.cpu.gpr[4] as u32; // queue address
                let a2 = n64.cpu.gpr[6] as u32; // blocking flag
                recv_mesg_log.push((i, a0, a2));
            }
            // Track key SM64 addresses
            if pc32 == 0x80246C10 {
                exec_dl_hits += 1;
                if exec_dl_hits == 1 {
                    exec_dl_first_step = i;
                }
            }
            // Track render gate check (0x80248B78 = BEQ that gates rendering)
            if pc32 == 0x80248B78 {
                if render_gate_check_count < 10 || render_gate_check_count % 100 == 0 {
                    let rd = n64.rdram_data();
                    let gate_val = rd[render_gate_addr];
                    render_gate_check_log.push((i, gate_val, render_gate_check_count));
                }
                render_gate_check_count += 1;
            }
            // PC trace around game thread crash
            if i >= 87099000 && i <= 87103000 && crash_pc_trace.len() < 5000 {
                crash_pc_trace.push((i, pc32));
            }
            // Track function entry of game_loop body (look for call at 0x80248B68)
            if pc32 == 0x80248B68 {
                game_loop_body_hits += 1;
            }
            // Track display_and_vsync sub-calls
            match pc32 {
                0x80248090 => {
                    dav_entry_count += 1;
                    if dav_log.len() < 50 {
                        dav_log.push((i, "ENTRY", dav_entry_count));
                    }
                }
                0x802480B0 => {
                    dav_recv1_count += 1;
                    if dav_log.len() < 50 {
                        dav_log.push((i, "RECV_GFX", dav_recv1_count));
                    }
                }
                0x802480EC => {
                    dav_exec_count += 1;
                    if dav_log.len() < 50 {
                        dav_log.push((i, "EXEC_DL", dav_exec_count));
                    }
                }
                0x8024810C => {
                    dav_recv2_count += 1;
                    if dav_log.len() < 50 {
                        dav_log.push((i, "RECV_VBLANK", dav_recv2_count));
                    }
                }
                _ => {}
            }
            // Monitor rendering gate
            {
                let rd = n64.rdram_data();
                if render_gate_addr < rd.len() {
                    let val = rd[render_gate_addr];
                    if val != prev_render_gate && render_gate_log.len() < 100 {
                        render_gate_log.push((i, val, pc32));
                    }
                    prev_render_gate = val;
                }
            }
            // Game thread state monitoring (check every 16 steps)
            if i & 0xF == 0 {
                let rd = n64.rdram_data();
                if game_thread_state_addr + 1 < rd.len() {
                    let state = u16::from_be_bytes([
                        rd[game_thread_state_addr],
                        rd[game_thread_state_addr + 1],
                    ]);
                    if state != prev_game_thread_state {
                        let s = match state {
                            1 => "STOP",
                            2 => "RUNNABLE",
                            4 => "RUNNING",
                            8 => "WAITING",
                            _ => "?",
                        };
                        eprintln!(
                            "  ** Game thread state: {} → {} ({}) at step {} PC={:#010X}",
                            prev_game_thread_state, state, s, i, pc32
                        );
                        if state == 1 {
                            game_thread_stop_step = i;
                            // Dump game thread saved PC and RA
                            let thread_phys = 0x0033AA90usize;
                            if thread_phys + 0x120 < rd.len() {
                                let saved_pc = u32::from_be_bytes([
                                    rd[thread_phys + 0x11C],
                                    rd[thread_phys + 0x11D],
                                    rd[thread_phys + 0x11E],
                                    rd[thread_phys + 0x11F],
                                ]);
                                let saved_ra = u32::from_be_bytes([
                                    rd[thread_phys + 0x104],
                                    rd[thread_phys + 0x105],
                                    rd[thread_phys + 0x106],
                                    rd[thread_phys + 0x107],
                                ]);
                                let saved_sp = u32::from_be_bytes([
                                    rd[thread_phys + 0xF4],
                                    rd[thread_phys + 0xF5],
                                    rd[thread_phys + 0xF6],
                                    rd[thread_phys + 0xF7],
                                ]);
                                eprintln!("  ** Game thread STOPPED: saved_pc={:#010X} saved_ra={:#010X} saved_sp={:#010X}", saved_pc, saved_ra, saved_sp);
                            }
                        }
                        prev_game_thread_state = state;
                    }
                }
            }
            // VBlank queue monitoring
            {
                let rd = n64.rdram_data();
                if vblank_q_valid_addr + 3 < rd.len() {
                    let cur = u32::from_be_bytes([
                        rd[vblank_q_valid_addr],
                        rd[vblank_q_valid_addr + 1],
                        rd[vblank_q_valid_addr + 2],
                        rd[vblank_q_valid_addr + 3],
                    ]);
                    if cur != prev_vblank_q_valid && vblank_q_log.len() < 100 {
                        vblank_q_log.push((i, cur, pc32));
                    }
                    prev_vblank_q_valid = cur;
                }
            }
            // GFX completion queue monitoring
            {
                let rd = n64.rdram_data();
                if gfx_q_valid_addr + 3 < rd.len() {
                    let cur = u32::from_be_bytes([
                        rd[gfx_q_valid_addr],
                        rd[gfx_q_valid_addr + 1],
                        rd[gfx_q_valid_addr + 2],
                        rd[gfx_q_valid_addr + 3],
                    ]);
                    if cur != prev_gfx_q_valid && gfx_q_log.len() < 100 {
                        gfx_q_log.push((i, cur, pc32));
                    }
                    prev_gfx_q_valid = cur;
                }
            }
            // === SI queue monitoring — every step ===
            {
                let rd = n64.rdram_data();
                if si_q_a_valid_addr + 3 < rd.len() {
                    let cur_a = u32::from_be_bytes([
                        rd[si_q_a_valid_addr],
                        rd[si_q_a_valid_addr + 1],
                        rd[si_q_a_valid_addr + 2],
                        rd[si_q_a_valid_addr + 3],
                    ]);
                    if cur_a != prev_si_q_a && si_q_a_log.len() < 200 {
                        si_q_a_log.push((i, cur_a, pc32));
                    }
                    prev_si_q_a = cur_a;
                }
                if si_q_b_valid_addr + 3 < rd.len() {
                    let cur_b = u32::from_be_bytes([
                        rd[si_q_b_valid_addr],
                        rd[si_q_b_valid_addr + 1],
                        rd[si_q_b_valid_addr + 2],
                        rd[si_q_b_valid_addr + 3],
                    ]);
                    if cur_b != prev_si_q_b && si_q_b_log.len() < 200 {
                        si_q_b_log.push((i, cur_b, pc32));
                    }
                    prev_si_q_b = cur_b;
                }
            }
            let rsp_before = n64.bus.rsp.start_count;
            let si_dma_before = n64.bus.si.dma_count;
            n64.step_one();
            // Track SI DMA step timing
            if n64.bus.si.dma_count > si_dma_before && si_dma_steps.len() < 50 {
                let idx = n64.bus.si.dma_log.len().saturating_sub(1);
                let (dram, dir) = if idx < n64.bus.si.dma_log.len() {
                    n64.bus.si.dma_log[idx]
                } else {
                    (0, 0)
                };
                si_dma_steps.push((i, dram, dir));
            }
            // Track MI_INTR SI transitions
            let cur_mi_si = n64.bus.mi.intr & (1 << 1) != 0;
            if cur_mi_si != prev_mi_si && mi_si_log.len() < 200 {
                mi_si_log.push((i, cur_mi_si, n64.cpu.pc as u32));
            }
            prev_mi_si = cur_mi_si;
            // Track osRecvMesg/osSendMesg calls during EEPROM window (steps 23.4M-23.5M)
            if i >= 23_400_000 && i <= 23_500_000 && mesg_calls.len() < 500 {
                let pc_now = n64.cpu.pc as u32;
                if pc_now == 0x80322800 {
                    // osRecvMesg entry
                    let a0 = n64.cpu.gpr[4] as u32;
                    let a1 = n64.cpu.gpr[5] as u32;
                    mesg_calls.push((i, 'R', a0, a1));
                    // (PC trace trigger moved below)
                } else if pc_now == 0x80322C20 {
                    // osSendMesg entry
                    let a0 = n64.cpu.gpr[4] as u32;
                    let a1 = n64.cpu.gpr[5] as u32;
                    mesg_calls.push((i, 'S', a0, a1));
                }
            }
            // PC trace: capture critical window around 2nd EEPROM block
            // Start just before the last DMA wait returns, continue until blocked
            if i >= 23_442_870 && !pc_trace_active && pc_trace.is_empty() {
                pc_trace_active = true;
            }
            if pc_trace_active && pc_trace.len() < 500 {
                pc_trace.push((i, n64.cpu.pc as u32));
                // Stop tracing when we see the blocked osRecvMesg on Queue B
                if n64.cpu.pc as u32 == 0x80322868 {
                    // osRecvMesg blocking path
                    pc_trace_active = false;
                }
            }
            // Track PI DMA timing
            if n64.bus.pi.dma_count > prev_pi_count && pi_dma_steps.len() < 500 {
                pi_dma_steps.push(i);
                prev_pi_count = n64.bus.pi.dma_count;
            }
            if n64.bus.rsp.start_count > rsp_before {
                // Read task type from DMEM to classify
                let task_type = n64.bus.rsp.read_dmem_u32(0xFC0);
                match task_type {
                    1 => gfx_task_count += 1,
                    2 => audio_task_count += 1,
                    _ => other_task_count += 1,
                }
                // Log first 5 tasks with full DMEM header
                if rsp_task_pcs.len() < 5 {
                    let t1 = n64.bus.rsp.read_dmem_u32(0xFC4);
                    let t2 = n64.bus.rsp.read_dmem_u32(0xFF0);
                    eprintln!(
                        "  RSP task #{}: type={} flags={:#X} data_ptr={:#010X} step={} PC={:#010X}",
                        n64.bus.rsp.start_count, task_type, t1, t2, i, pc32
                    );
                }
                if rsp_task_pcs.len() < 20 {
                    rsp_task_pcs.push((pc32, i));
                }
            }
            // Monitor gIntrMesgQueue: every step for first 30M, then every 16
            if i < 30_000_000 || i & 0xF == 0 {
                let rd = n64.rdram_data();
                if intr_q_valid_addr + 3 < rd.len() {
                    let cur_valid = u32::from_be_bytes([
                        rd[intr_q_valid_addr],
                        rd[intr_q_valid_addr + 1],
                        rd[intr_q_valid_addr + 2],
                        rd[intr_q_valid_addr + 3],
                    ]);
                    if cur_valid > prev_valid && msg_log.len() < 500 {
                        let first_idx = u32::from_be_bytes([
                            rd[intr_q_first_addr],
                            rd[intr_q_first_addr + 1],
                            rd[intr_q_first_addr + 2],
                            rd[intr_q_first_addr + 3],
                        ]);
                        let msg_idx = ((first_idx + cur_valid - 1) % 16) as usize;
                        let msg_off = intr_q_msgs_addr + msg_idx * 4;
                        let msg_val = u32::from_be_bytes([
                            rd[msg_off],
                            rd[msg_off + 1],
                            rd[msg_off + 2],
                            rd[msg_off + 3],
                        ]);
                        msg_log.push((i, msg_val));
                    }
                    prev_valid = cur_valid;
                }
                // (SI queue monitoring moved to every-step block above)
            }
            // Track DP interrupt (only raised by GFX tasks in process_rsp_task)
            if n64.bus.mi.intr & (1 << 5) != 0 {
                dp_interrupt_count += 1;
                if first_dp_step == 0 {
                    first_dp_step = i + 1;
                    eprintln!("  ** DP interrupt raised at step {} PC={:#010X}", i, pc32);
                }
            }
        }

        eprintln!(
            "\n  DMAs={} RSP_tasks={} (gfx={} audio={} other={}) PC={:#010X}",
            n64.bus.pi.dma_count,
            n64.bus.rsp.start_count,
            gfx_task_count,
            audio_task_count,
            other_task_count,
            n64.cpu.pc as u32
        );
        eprintln!(
            "  VI: ctrl={:#010X} origin={:#010X} width={} h_video={:#010X}",
            n64.bus.vi.ctrl, n64.bus.vi.origin, n64.bus.vi.width, n64.bus.vi.h_video
        );

        let status = n64.cpu.cop0.regs[n64_core::cpu::cop0::Cop0::STATUS] as u32;
        let cause = n64.cpu.cop0.regs[n64_core::cpu::cop0::Cop0::CAUSE] as u32;
        let ie = status & 1 != 0;
        let exl = status & 2 != 0;
        let erl = status & 4 != 0;
        let ip = (cause >> 8) & 0xFF;
        let im = (status >> 8) & 0xFF;
        eprintln!(
            "  COP0: IE={} EXL={} ERL={} IP={:08b} IM={:08b} IP&IM={:08b}",
            ie as u8,
            exl as u8,
            erl as u8,
            ip,
            im,
            ip & im
        );
        eprintln!(
            "  MI: intr={:06b} mask={:06b} pending={}",
            n64.bus.mi.intr,
            n64.bus.mi.intr_mask,
            n64.bus.mi.interrupt_pending()
        );
        let names = ["SP", "SI", "AI", "VI", "PI", "DP"];
        eprint!("  MI set/clear:");
        for (i, name) in names.iter().enumerate() {
            if n64.bus.mi.set_counts[i] > 0 || n64.bus.mi.clear_counts[i] > 0 {
                eprint!(
                    " {}={}/{}",
                    name, n64.bus.mi.set_counts[i], n64.bus.mi.clear_counts[i]
                );
            }
        }
        eprintln!();
        eprintln!(
            "  VI: v_current={} v_intr={} v_sync={}",
            n64.bus.vi.v_current, n64.bus.vi.v_intr, n64.bus.vi.v_sync
        );
        eprintln!(
            "  SI: {} DMAs  AI: {} DMAs (dacrate={})",
            n64.bus.si.dma_count, n64.bus.ai.dma_count, n64.bus.ai.dacrate
        );
        // Audio sample counts captured from AI DMA (pushed asynchronously)
        let total = n64.bus.audio_sample_count;
        let nonzero = n64.bus.audio_nonzero_sample_count;
        let nonzero_pct = if total > 0 {
            nonzero as f64 * 100.0 / total as f64
        } else {
            0.0
        };
        eprintln!(
            "  Audio samples captured: {} total, {} nonzero ({:.1}%)",
            total, nonzero, nonzero_pct
        );
        // PI DMA start vs completion: check for lost interrupts
        let pi_starts = n64.bus.pi.dma_count as u64;
        let pi_completions = n64.bus.mi.set_counts[4] as u64; // PI interrupt fires
        let pi_pending: u64 = if n64.bus.pi.dma_busy_cycles > 0 { 1 } else { 0 };
        eprintln!(
            "  PI: {} starts, {} completions, {} pending (busy_cycles={})",
            pi_starts, pi_completions, pi_pending, n64.bus.pi.dma_busy_cycles
        );
        if pi_starts != pi_completions + pi_pending {
            eprintln!(
                "  *** PI DMA MISMATCH: {} starts != {} completions + {} pending ***",
                pi_starts, pi_completions, pi_pending
            );
        }
        eprintln!(
            "  Renderer: {} fills, {} tex_rects, {} vtx, {} tris, {} pixels, {} z_fail, {} culled",
            n64.bus.renderer.fill_rect_count,
            n64.bus.renderer.tex_rect_count,
            n64.bus.renderer.vtx_count,
            n64.bus.renderer.tri_count,
            n64.bus.renderer.pixel_count,
            n64.bus.renderer.z_fail_count,
            n64.bus.renderer.cull_count
        );
        let origin = n64.bus.vi.origin as usize;
        let rdram = n64.rdram_data();
        let fb_size = 320 * 240 * 2;
        if origin > 0 && origin + fb_size < rdram.len() {
            let nonzero = (0..fb_size).filter(|&i| rdram[origin + i] != 0).count();
            eprintln!(
                "  Framebuffer: {}% non-zero at RDRAM[{:#X}]",
                nonzero * 100 / fb_size,
                origin
            );
        }
        // Save diag screenshot
        save_screenshot(&n64);
        eprintln!(
            "  Exception entries: {}  DP interrupts: {}  SP_STATUS reads: {}",
            exception_count,
            dp_interrupt_count,
            n64.bus.rsp.status_read_count.get()
        );

        // Dump gIntrMesgQueue message buffer (addr from thread 3's queue)
        // Queue at 0x8033AE08: msgs at 0x8033AEC8, cap=16
        if 0x0033AEC8usize + 64 < rdram.len() {
            eprint!("  gIntrMesgQueue msg buffer:");
            for j in 0..16u32 {
                let off = 0x0033AEC8 + (j as usize) * 4;
                let msg = u32::from_be_bytes([
                    rdram[off],
                    rdram[off + 1],
                    rdram[off + 2],
                    rdram[off + 3],
                ]);
                if msg != 0 {
                    eprint!(" [{}]={:#X}", j, msg);
                }
            }
            eprintln!();
        }

        // gIntrMesgQueue message log
        if !msg_log.is_empty() {
            eprintln!(
                "  gIntrMesgQueue messages ({} arrivals, first 50):",
                msg_log.len()
            );
            // Count message types
            let mut msg_counts: std::collections::HashMap<u32, u32> =
                std::collections::HashMap::new();
            for &(_, val) in &msg_log {
                *msg_counts.entry(val).or_default() += 1;
            }
            for (val, count) in &msg_counts {
                eprintln!("    msg={:#X}: {} times", val, count);
            }
            // Show first 30 with timestamps
            for &(step, val) in msg_log.iter().take(30) {
                eprintln!("    step={}: msg={:#X}", step, val);
            }
        }

        // Game loop body hits
        eprintln!(
            "  Game loop body (0x80248B68): {} hits",
            game_loop_body_hits
        );
        eprintln!(
            "  Render gate check (0x80248B78): {} hits",
            render_gate_check_count
        );
        for &(step, val, check_num) in render_gate_check_log.iter() {
            eprintln!("    check #{}: step={} gate={}", check_num, step, val);
        }
        // Rendering gate (0x8032D584) transitions
        eprintln!(
            "  Rendering gate (0x8032D584) transitions ({}):",
            render_gate_log.len()
        );
        for &(step, val, pc) in render_gate_log.iter() {
            let state = if val == 0 { "RENDER" } else { "SKIP" };
            eprintln!("    step={}: val={} ({}) PC={:#010X}", step, val, state, pc);
        }
        // Also check current value
        {
            let rd = n64.rdram_data();
            let cur = rd[render_gate_addr];
            eprintln!(
                "  Rendering gate CURRENT value: {} ({})",
                cur,
                if cur == 0 { "RENDER" } else { "SKIP" }
            );
        }

        // Game thread state
        if game_thread_stop_step > 0 {
            eprintln!(
                "  *** GAME THREAD STOPPED at step {} ***",
                game_thread_stop_step
            );
        }
        eprintln!("  TLB miss count: {}", n64.cpu.tlb_miss_count);
        // Crash PC trace
        if !crash_pc_trace.is_empty() {
            eprintln!(
                "  Crash window PC trace ({} entries, steps {}..{}):",
                crash_pc_trace.len(),
                crash_pc_trace.first().unwrap().0,
                crash_pc_trace.last().unwrap().0
            );
            // Group PCs into function ranges and annotate
            let rdram = n64.rdram_data();
            for &(step, pc) in &crash_pc_trace {
                let phys = (pc & 0x1FFF_FFFF) as usize;
                let opcode = if phys + 3 < rdram.len() {
                    u32::from_be_bytes([
                        rdram[phys],
                        rdram[phys + 1],
                        rdram[phys + 2],
                        rdram[phys + 3],
                    ])
                } else {
                    0
                };
                // Annotate known OS functions
                let note = match pc {
                    0x80322800..=0x80322BFF => " [osRecvMesg]",
                    0x80322C20..=0x80322FFF => " [osSendMesg]",
                    0x80327C00..=0x80327DFF => " [__osDispatch]",
                    0x80000180 => " [EXCEPTION]",
                    _ => "",
                };
                eprintln!(
                    "    step={}: PC={:#010X} op={:#010X}{}",
                    step, pc, opcode, note
                );
            }
        }

        // display_and_vsync sub-call tracking
        eprintln!(
            "  display_and_vsync: entry={} recv_gfx={} exec_dl={} recv_vblank={}",
            dav_entry_count, dav_recv1_count, dav_exec_count, dav_recv2_count
        );
        for &(step, event, count) in dav_log.iter().take(30) {
            eprintln!("    step={}: {} #{}", step, event, count);
        }

        // VBlank queue transitions
        eprintln!(
            "  VBlank queue (0x8033B010) transitions ({}):",
            vblank_q_log.len()
        );
        for &(step, valid, pc) in vblank_q_log.iter().take(30) {
            eprintln!("    step={}: validCount={} PC={:#010X}", step, valid, pc);
        }

        // PI DMA timing
        if !pi_dma_steps.is_empty() {
            eprintln!(
                "  PI DMA timing: {} DMAs, first at step {}, last at step {}",
                pi_dma_steps.len(),
                pi_dma_steps.first().unwrap(),
                pi_dma_steps.last().unwrap()
            );
            // Show last 10 DMAs
            let start = pi_dma_steps.len().saturating_sub(10);
            for (j, &step) in pi_dma_steps[start..].iter().enumerate() {
                eprintln!("    DMA #{}: step {}", start + j + 1, step);
            }
        }

        // osRecvMesg calls after frame 3
        eprintln!(
            "  osRecvMesg calls after step 63M ({}):",
            recv_mesg_log.len()
        );
        {
            let mut queue_counts: std::collections::HashMap<u32, u32> =
                std::collections::HashMap::new();
            for &(_, q, _) in &recv_mesg_log {
                *queue_counts.entry(q).or_default() += 1;
            }
            let mut entries: Vec<_> = queue_counts.iter().collect();
            entries.sort_by(|a, b| b.1.cmp(a.1));
            for (q, count) in &entries {
                eprintln!("    queue={:#010X}: {} calls", q, count);
            }
            // Show first 20 with timestamps
            for &(step, q, block) in recv_mesg_log.iter().take(20) {
                eprintln!("    step={}: queue={:#010X} block={}", step, q, block);
            }
        }

        // GFX completion queue (0x8033B028) transitions
        eprintln!(
            "  GFX completion queue (0x8033B028) validCount transitions ({}):",
            gfx_q_log.len()
        );
        for &(step, valid, pc) in gfx_q_log.iter().take(30) {
            eprintln!("    step={}: validCount={} PC={:#010X}", step, valid, pc);
        }

        // Find callers of alloc_display_list (0x80248090) = JAL 0x0C092024
        // and config_gfx_pool (first function called in render path)
        {
            let rdram = n64.rdram_data();
            let target_opcode = 0x0C092024u32; // JAL alloc_display_list
            eprintln!("  Searching for JAL 0x80248090 (alloc_display_list):");
            for off in (0x00200000usize..0x00400000).step_by(4) {
                if off + 3 < rdram.len() {
                    let instr = u32::from_be_bytes([
                        rdram[off],
                        rdram[off + 1],
                        rdram[off + 2],
                        rdram[off + 3],
                    ]);
                    if instr == target_opcode {
                        let vaddr = 0x80000000u32 + off as u32;
                        // Dump context: 8 instructions before and 4 after
                        eprintln!("    Found at {:#010X}:", vaddr);
                        for ctx in -30i32..10 {
                            let ctx_off = off as i32 + ctx * 4;
                            if ctx_off >= 0 && (ctx_off as usize) + 3 < rdram.len() {
                                let ctx_instr = u32::from_be_bytes([
                                    rdram[ctx_off as usize],
                                    rdram[ctx_off as usize + 1],
                                    rdram[ctx_off as usize + 2],
                                    rdram[ctx_off as usize + 3],
                                ]);
                                let ctx_va = 0x80000000u32.wrapping_add(ctx_off as u32);
                                let marker = if ctx == 0 { " ★★★" } else { "" };
                                eprintln!("      {:#010X}: {:#010X}{}", ctx_va, ctx_instr, marker);
                            }
                        }
                    }
                }
            }
        }

        // Dump MIPS code around exec_display_list call site (0x802480EC)
        // to understand game thread blocking
        {
            let rdram = n64.rdram_data();
            eprintln!("  Code dump around exec_display_list call (0x802480xx):");
            // Dump 0x80248000-0x80248120 (phys 0x00248000)
            for off in (0x00248000usize..0x00248120).step_by(4) {
                if off + 3 < rdram.len() {
                    let instr = u32::from_be_bytes([
                        rdram[off],
                        rdram[off + 1],
                        rdram[off + 2],
                        rdram[off + 3],
                    ]);
                    if instr != 0 {
                        let vaddr = 0x80000000u32 + off as u32;
                        let marker = if vaddr == 0x802480EC {
                            " ★ jal exec_display_list"
                        } else {
                            ""
                        };
                        eprintln!("    {:#010X}: {:#010X}{}", vaddr, instr, marker);
                    }
                }
            }
            // Full function containing the render path (0x80248A00-0x80248D00)
            eprintln!("  Full render function (0x80248A00-0x80248D00):");
            for off in (0x00248A00usize..0x00248D00).step_by(4) {
                if off + 3 < rdram.len() {
                    let instr = u32::from_be_bytes([
                        rdram[off],
                        rdram[off + 1],
                        rdram[off + 2],
                        rdram[off + 3],
                    ]);
                    if instr != 0 {
                        let vaddr = 0x80000000u32 + off as u32;
                        eprintln!("    {:#010X}: {:#010X}", vaddr, instr);
                    }
                }
            }
        }

        // Search for ALL jal osSendMesg calls, find exec_display_list (msg=0x67)
        {
            let rdram = n64.rdram_data();
            let jal_send = 0x0C0C8B08u32; // jal osSendMesg (0x80322C20)
            let mut all_calls: Vec<(u32, Option<u32>)> = Vec::new();
            for addr in (0x200000..0x330000usize).step_by(4) {
                if addr + 3 >= rdram.len() {
                    break;
                }
                let op = u32::from_be_bytes([
                    rdram[addr],
                    rdram[addr + 1],
                    rdram[addr + 2],
                    rdram[addr + 3],
                ]);
                if op == jal_send {
                    let va = 0x80000000u32 + addr as u32;
                    // Look for a0 (queue) and a1 (msg) values in nearby instructions
                    let mut a0_val: Option<u32> = None;
                    let mut a1_val: Option<u32> = None;
                    let mut a0_lui: u32 = 0;
                    for off in (-40i32..4).step_by(4) {
                        let check = (addr as i32 + off) as usize;
                        if check + 3 >= rdram.len() {
                            continue;
                        }
                        let inst = u32::from_be_bytes([
                            rdram[check],
                            rdram[check + 1],
                            rdram[check + 2],
                            rdram[check + 3],
                        ]);
                        // lui a0($4), imm
                        if inst >> 16 == 0x3C04 {
                            a0_lui = (inst & 0xFFFF) << 16;
                        }
                        // addiu a0($4), a0($4), imm
                        if inst >> 16 == 0x2484 {
                            a0_val = Some(a0_lui.wrapping_add(inst as u16 as i16 as i32 as u32));
                        }
                        // addiu a1($5), $zero, imm: 001001_00000_00101 = 0x2405xxxx
                        if inst >> 16 == 0x2405 {
                            a1_val = Some(inst & 0xFFFF);
                        }
                        // ori a1($5), $zero, imm: 001101_00000_00101 = 0x3405xxxx
                        if inst >> 16 == 0x3405 {
                            a1_val = Some(inst & 0xFFFF);
                        }
                        // or a1, $0, $0 → a1 = 0
                        if inst == 0x00002825 {
                            a1_val = Some(0);
                        }
                    }
                    all_calls.push((va, a1_val));
                    // For msg=NULL calls, also show queue address
                    if a1_val == Some(0) && a0_val.is_some() {
                        all_calls.push((va, Some(a0_val.unwrap() | 0xF0000000)));
                        // marker
                    }
                }
            }
            eprintln!("  All jal osSendMesg calls ({} found):", all_calls.len());
            for &(va, a1) in &all_calls {
                let tag = match a1 {
                    Some(0x64) => " ← SP_COMPLETE",
                    Some(0x65) => " ← DP_COMPLETE",
                    Some(0x66) => " ← VI_VBLANK",
                    Some(0x67) => " ← START_GFX_SPTASK ★★★",
                    Some(0) => " ← msg=NULL",
                    _ => "",
                };
                eprintln!("    {:#010X}: msg={:?}{}", va, a1, tag);
            }
            // Find display_and_vsync function entry by scanning backward from 0x802480EC
            // for a function prologue (addiu sp, sp, -XX = 0x27BDxxxx with negative imm)
            let mut dav_entry: u32 = 0;
            let mut scan_addr = 0x2480E8usize; // start just before the jal
            while scan_addr >= 0x247000 {
                if scan_addr + 3 >= rdram.len() {
                    break;
                }
                let inst = u32::from_be_bytes([
                    rdram[scan_addr],
                    rdram[scan_addr + 1],
                    rdram[scan_addr + 2],
                    rdram[scan_addr + 3],
                ]);
                if inst >> 16 == 0x27BD && (inst & 0x8000) != 0 {
                    // addiu sp, sp, negative
                    dav_entry = 0x80000000 + scan_addr as u32;
                    break;
                }
                scan_addr -= 4;
            }
            eprintln!("  display_and_vsync entry: {:#010X}", dav_entry);
            // Dump from entry to past exec_display_list call
            if dav_entry != 0 {
                let start = (dav_entry - 0x80000000) as usize;
                for addr in (start..start + 0x120).step_by(4) {
                    if addr + 3 >= rdram.len() {
                        break;
                    }
                    let inst = u32::from_be_bytes([
                        rdram[addr],
                        rdram[addr + 1],
                        rdram[addr + 2],
                        rdram[addr + 3],
                    ]);
                    let va = 0x80000000u32 + addr as u32;
                    // Annotate known patterns
                    let note = if inst == 0x0C091B04 {
                        " <-- jal exec_display_list"
                    } else if inst == 0x0C0C8B08 {
                        " <-- jal osSendMesg"
                    } else if inst >> 16 == 0x27BD {
                        " <-- sp adjust"
                    } else if inst >> 26 == 3 {
                        // jal
                        let target = (inst & 0x03FFFFFF) << 2 | 0x80000000;
                        if target == 0x80322868 {
                            " <-- jal osRecvMesg"
                        } else {
                            ""
                        }
                    } else {
                        ""
                    };
                    eprintln!("      {:#010X}: {:#010X}{}", va, inst, note);
                }
            }

            // Find callers of display_and_vsync
            if dav_entry != 0 {
                let target_idx = (dav_entry & 0x03FFFFFF) >> 2;
                let jal_dav = (3u32 << 26) | target_idx;
                eprintln!(
                    "  Callers of display_and_vsync (jal {:#010X} = {:#010X}):",
                    dav_entry, jal_dav
                );
                for addr in (0x200000..0x340000usize).step_by(4) {
                    if addr + 3 >= rdram.len() {
                        break;
                    }
                    let op = u32::from_be_bytes([
                        rdram[addr],
                        rdram[addr + 1],
                        rdram[addr + 2],
                        rdram[addr + 3],
                    ]);
                    if op == jal_dav {
                        eprintln!(
                            "    {:#010X}: jal display_and_vsync",
                            0x80000000u32 + addr as u32
                        );
                    }
                }
            }
        }

        // exec_display_list call tracking
        eprintln!(
            "  exec_display_list (0x80246C68): {} hits, first at step {}",
            exec_dl_hits, exec_dl_first_step
        );

        // Non-idle PC histogram (steps {}M-{}M)
        eprintln!(
            "  Non-idle PCs (steps {}M-{}M, excluding idle loop):",
            nonidle_window_start / 1_000_000,
            nonidle_window_end / 1_000_000
        );
        {
            let mut entries: Vec<_> = nonidle_pc_hist.iter().collect();
            entries.sort_by(|a, b| b.1.cmp(a.1));
            for (j, (pc, count)) in entries.iter().enumerate().take(30) {
                let off = (**pc as usize) & 0x00FF_FFFF;
                let opcode = if off + 3 < n64.rdram_data().len() {
                    let rd = n64.rdram_data();
                    u32::from_be_bytes([rd[off], rd[off + 1], rd[off + 2], rd[off + 3]])
                } else {
                    0
                };
                eprintln!("    {:#010X}: {} hits (opcode={:#010X})", pc, count, opcode);
            }
        }

        // SI Queue A (0x80367138) transitions
        eprintln!(
            "  SI Queue A (0x80367138) transitions ({}):",
            si_q_a_log.len()
        );
        for &(step, valid, pc) in si_q_a_log.iter().take(100) {
            eprintln!("    step={}: valid={} PC={:#010X}", step, valid, pc);
        }
        // SI Queue B (0x80367158) transitions
        eprintln!(
            "  SI Queue B (0x80367158) transitions ({}):",
            si_q_b_log.len()
        );
        for &(step, valid, pc) in si_q_b_log.iter().take(100) {
            eprintln!("    step={}: valid={} PC={:#010X}", step, valid, pc);
        }

        // osRecvMesg/osSendMesg calls during EEPROM window
        eprintln!(
            "  osRecvMesg/osSendMesg during EEPROM window ({}):",
            mesg_calls.len()
        );
        for &(step, func, a0, a1) in mesg_calls.iter().take(100) {
            let queue_name = match a0 {
                0x80367138 => " [QueueA/__osSiAccess]",
                0x80367158 => " [QueueB/__osContAccess]",
                0x8033AE08 => " [gIntrMesgQueue]",
                _ => "",
            };
            eprintln!(
                "    step={}: os{}Mesg(queue={:#010X}{}, msg={:#010X})",
                step,
                if func == 'R' { "Recv" } else { "Send" },
                a0,
                queue_name,
                a1
            );
        }

        // PC trace for critical window
        if !pc_trace.is_empty() {
            eprintln!(
                "  PC trace after 4th event queue wait ({} entries):",
                pc_trace.len()
            );
            for &(step, pc) in pc_trace.iter().take(500) {
                // Annotate known functions
                let note = match pc {
                    0x80328934..=0x8032895F => " [__osSiRelAccess?]",
                    0x803288F0..=0x80328933 => " [__osContGetAccess?]",
                    0x80329150..=0x8032933F => " [EEPROM wrapper]",
                    0x80329340..=0x803293FF => " [func@0x80329340]",
                    0x80322800..=0x80322C1F => " [osRecvMesg]",
                    0x80322C20..=0x80322FFF => " [osSendMesg]",
                    0x80000180 => " [EXCEPTION]",
                    _ => "",
                };
                eprintln!("    step={}: PC={:#010X}{}", step, pc, note);
            }
        }

        // SI DMA step timing
        eprintln!("  SI DMA step timing ({}):", si_dma_steps.len());
        for &(step, dram, dir) in &si_dma_steps {
            let dir_str = if dir == 0 {
                "read(PIF→RDRAM)"
            } else {
                "write(RDRAM→PIF)"
            };
            eprintln!("    step={}: RDRAM[{:#010X}] {}", step, dram, dir_str);
        }
        // MI_INTR SI transitions
        eprintln!("  MI_INTR SI transitions ({}):", mi_si_log.len());
        for &(step, is_set, pc) in mi_si_log.iter().take(50) {
            eprintln!(
                "    step={}: {} PC={:#010X}",
                step,
                if is_set { "SET" } else { "CLEAR" },
                pc
            );
        }

        // Disassemble key EEPROM functions by known addresses
        // func@0x80328DAC = osEepromRead (from stack trace)
        // func@0x80329150 = EEPROM wrapper (from stack trace)
        // Also find __osSiInterrupt by scanning for SW to SI_STATUS (0xA4800018)
        {
            let rdram = n64.rdram_data();
            let read_u32_fn = |off: usize| -> u32 {
                if off + 3 < rdram.len() {
                    u32::from_be_bytes([rdram[off], rdram[off + 1], rdram[off + 2], rdram[off + 3]])
                } else {
                    0
                }
            };
            let decode_fn = |addr: u32, inst: u32| -> String {
                let op = inst >> 26;
                match op {
                    0 => {
                        let func = inst & 0x3F;
                        match func {
                            0x08 => "jr $ra".into(),
                            0x09 => format!("jalr ${}", (inst >> 11) & 0x1F),
                            0x21 => format!(
                                "addu ${},${},${}",
                                (inst >> 11) & 0x1F,
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F
                            ),
                            0x25 => format!(
                                "or ${},${},${}",
                                (inst >> 11) & 0x1F,
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F
                            ),
                            _ => format!("special/{:#04X}", func),
                        }
                    }
                    2 => format!(
                        "j {:#010X}",
                        ((inst & 0x03FFFFFF) << 2) | (addr & 0xF0000000)
                    ),
                    3 => format!(
                        "jal {:#010X}",
                        ((inst & 0x03FFFFFF) << 2) | (addr & 0xF0000000)
                    ),
                    4 => format!(
                        "beq ${},${},{:+}",
                        (inst >> 21) & 0x1F,
                        (inst >> 16) & 0x1F,
                        (inst as u16 as i16) * 4
                    ),
                    5 => format!(
                        "bne ${},${},{:+}",
                        (inst >> 21) & 0x1F,
                        (inst >> 16) & 0x1F,
                        (inst as u16 as i16) * 4
                    ),
                    9 => format!(
                        "addiu ${},${},{}",
                        (inst >> 16) & 0x1F,
                        (inst >> 21) & 0x1F,
                        inst as u16 as i16
                    ),
                    0xD => format!(
                        "ori ${},${},{:#X}",
                        (inst >> 16) & 0x1F,
                        (inst >> 21) & 0x1F,
                        inst & 0xFFFF
                    ),
                    0xF => format!("lui ${},{:#06X}", (inst >> 16) & 0x1F, inst & 0xFFFF),
                    0x23 => format!(
                        "lw ${},{}(${})",
                        (inst >> 16) & 0x1F,
                        inst as u16 as i16,
                        (inst >> 21) & 0x1F
                    ),
                    0x2B => format!(
                        "sw ${},{}(${})",
                        (inst >> 16) & 0x1F,
                        inst as u16 as i16,
                        (inst >> 21) & 0x1F
                    ),
                    _ => format!("op={:#04X}", op),
                }
            };

            // Disassemble func@0x80328DAC (osEepromRead)
            eprintln!("  --- osEepromRead func@0x80328DAC ---");
            let start = 0x328DACusize;
            for k in 0..150usize {
                let a = start + k * 4;
                if a + 3 >= rdram.len() {
                    break;
                }
                let inst = read_u32_fn(a);
                let va = 0x80000000u32 + a as u32;
                let note = if inst == 0x03E00008 {
                    " <-- jr $ra"
                } else if inst >> 26 == 3 {
                    let target = ((inst & 0x03FFFFFF) << 2) | 0x80000000;
                    if target == 0x80322800 {
                        " <-- jal osRecvMesg"
                    } else if target == 0x80322C20 {
                        " <-- jal osSendMesg"
                    } else {
                        ""
                    }
                } else {
                    ""
                };
                eprintln!(
                    "    {:#010X}: {:#010X}  {}{}",
                    va,
                    inst,
                    decode_fn(va, inst),
                    note
                );
                if inst == 0x03E00008 {
                    let a2 = start + (k + 1) * 4;
                    if a2 + 3 < rdram.len() {
                        let i2 = read_u32_fn(a2);
                        eprintln!(
                            "    {:#010X}: {:#010X}  {}",
                            va + 4,
                            i2,
                            decode_fn(va + 4, i2)
                        );
                    }
                    break;
                }
            }

            // Disassemble func@0x80329150 (wrapper)
            eprintln!("  --- EEPROM wrapper func@0x80329150 ---");
            let start2 = 0x329150usize;
            for k in 0..150usize {
                let a = start2 + k * 4;
                if a + 3 >= rdram.len() {
                    break;
                }
                let inst = read_u32_fn(a);
                let va = 0x80000000u32 + a as u32;
                let note = if inst == 0x03E00008 {
                    " <-- jr $ra"
                } else if inst >> 26 == 3 {
                    let target = ((inst & 0x03FFFFFF) << 2) | 0x80000000;
                    if target == 0x80322800 {
                        " <-- jal osRecvMesg"
                    } else if target == 0x80322C20 {
                        " <-- jal osSendMesg"
                    } else if target == 0x80328DAC {
                        " <-- jal osEepromRead(0x80328DAC)"
                    } else {
                        ""
                    }
                } else {
                    ""
                };
                eprintln!(
                    "    {:#010X}: {:#010X}  {}{}",
                    va,
                    inst,
                    decode_fn(va, inst),
                    note
                );
                if inst == 0x03E00008 {
                    let a2 = start2 + (k + 1) * 4;
                    if a2 + 3 < rdram.len() {
                        let i2 = read_u32_fn(a2);
                        eprintln!(
                            "    {:#010X}: {:#010X}  {}",
                            va + 4,
                            i2,
                            decode_fn(va + 4, i2)
                        );
                    }
                    break;
                }
            }

            // Find __osSiInterrupt by scanning for SW to SI_STATUS (lui+sw pattern for 0xA4800018)
            // Look for: lui $reg, 0xA480 ... sw $reg2, 0x18($reg)
            eprintln!("  --- Searching for __osSiInterrupt (SW to SI_STATUS 0xA4800018) ---");
            for addr in (0x320000..0x340000usize).step_by(4) {
                if addr + 3 >= rdram.len() {
                    break;
                }
                let inst = read_u32_fn(addr);
                // Look for "sw $X, 0x18($Y)" where Y was loaded with 0xA480xxxx
                if (inst >> 26) == 0x2B && (inst & 0xFFFF) == 0x0018 {
                    let base_reg = (inst >> 21) & 0x1F;
                    // Scan backward for "lui $base_reg, 0xA480"
                    for back in 1..20 {
                        let prev_addr = addr.saturating_sub(back * 4);
                        let prev_inst = read_u32_fn(prev_addr);
                        {
                            // Check: lui $base_reg, 0xA480
                            let lui_rt = (prev_inst >> 16) & 0x1F;
                            let lui_op = prev_inst >> 26;
                            let lui_imm = prev_inst & 0xFFFF;
                            if lui_op == 0xF && lui_rt == base_reg && lui_imm == 0xA480 {
                                let va = 0x80000000u32 + addr as u32;
                                eprintln!("    Found SW to SI_STATUS at {:#010X}", va);
                                // Find containing function
                                let mut fs = addr;
                                for _ in 0..200 {
                                    if fs < 4 {
                                        break;
                                    }
                                    fs -= 4;
                                    let fi = read_u32_fn(fs);
                                    if fi >> 16 == 0x27BD && (fi & 0x8000) != 0 {
                                        break;
                                    }
                                }
                                let fva = 0x80000000u32 + fs as u32;
                                eprintln!("  --- __osSiInterrupt candidate func@{:#010X} ---", fva);
                                for k in 0..80usize {
                                    let a = fs + k * 4;
                                    if a + 3 >= rdram.len() {
                                        break;
                                    }
                                    let fi = read_u32_fn(a);
                                    let fva2 = fva.wrapping_add((k * 4) as u32);
                                    let note = if fi >> 26 == 3 {
                                        let target = ((fi & 0x03FFFFFF) << 2) | 0x80000000;
                                        if target == 0x80322C20 {
                                            " <-- jal osSendMesg"
                                        } else if target == 0x80322800 {
                                            " <-- jal osRecvMesg"
                                        } else {
                                            ""
                                        }
                                    } else if fi == 0x03E00008 {
                                        " <-- jr $ra"
                                    } else {
                                        ""
                                    };
                                    eprintln!(
                                        "    {:#010X}: {:#010X}  {}{}",
                                        fva2,
                                        fi,
                                        decode_fn(fva2, fi),
                                        note
                                    );
                                    if fi == 0x03E00008 {
                                        let a2 = fs + (k + 1) * 4;
                                        if a2 + 3 < rdram.len() {
                                            let i2 = read_u32_fn(a2);
                                            eprintln!(
                                                "    {:#010X}: {:#010X}  {}",
                                                fva2 + 4,
                                                i2,
                                                decode_fn(fva2 + 4, i2)
                                            );
                                        }
                                        break;
                                    }
                                }
                                break; // found it
                            }
                        }
                    }
                }
            }
        }

        // Disassemble functions containing osSendMesg calls at 0x80328428 and 0x803288D8
        {
            let rdram = n64.rdram_data();
            let read_u32 = |off: usize| -> u32 {
                if off + 3 < rdram.len() {
                    u32::from_be_bytes([rdram[off], rdram[off + 1], rdram[off + 2], rdram[off + 3]])
                } else {
                    0
                }
            };
            let decode = |addr: u32, inst: u32| -> String {
                let op = inst >> 26;
                match op {
                    0 => {
                        let func = inst & 0x3F;
                        match func {
                            0x08 => "jr $ra".into(),
                            0x09 => format!("jalr ${}", (inst >> 11) & 0x1F),
                            0x21 => format!(
                                "addu ${},${},${}",
                                (inst >> 11) & 0x1F,
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F
                            ),
                            0x25 => format!(
                                "or ${},${},${}",
                                (inst >> 11) & 0x1F,
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F
                            ),
                            _ => format!("special/{:#04X}", func),
                        }
                    }
                    2 => format!(
                        "j {:#010X}",
                        ((inst & 0x03FFFFFF) << 2) | (addr & 0xF0000000)
                    ),
                    3 => format!(
                        "jal {:#010X}",
                        ((inst & 0x03FFFFFF) << 2) | (addr & 0xF0000000)
                    ),
                    4 => format!(
                        "beq ${},${},{:+}",
                        (inst >> 21) & 0x1F,
                        (inst >> 16) & 0x1F,
                        (inst as u16 as i16) * 4
                    ),
                    5 => format!(
                        "bne ${},${},{:+}",
                        (inst >> 21) & 0x1F,
                        (inst >> 16) & 0x1F,
                        (inst as u16 as i16) * 4
                    ),
                    9 => format!(
                        "addiu ${},${},{}",
                        (inst >> 16) & 0x1F,
                        (inst >> 21) & 0x1F,
                        inst as u16 as i16
                    ),
                    0xD => format!(
                        "ori ${},${},{:#X}",
                        (inst >> 16) & 0x1F,
                        (inst >> 21) & 0x1F,
                        inst & 0xFFFF
                    ),
                    0xF => format!("lui ${},{:#06X}", (inst >> 16) & 0x1F, inst & 0xFFFF),
                    0x23 => format!(
                        "lw ${},{}(${})",
                        (inst >> 16) & 0x1F,
                        inst as u16 as i16,
                        (inst >> 21) & 0x1F
                    ),
                    0x2B => format!(
                        "sw ${},{}(${})",
                        (inst >> 16) & 0x1F,
                        inst as u16 as i16,
                        (inst >> 21) & 0x1F
                    ),
                    _ => format!("op={:#04X}", op),
                }
            };
            // Dump functions containing the two osSendMesg call sites
            for &target_va in &[0x80328428u32, 0x803288D8u32] {
                let target_phys = (target_va & 0x7FFFFF) as usize;
                // Scan backward for function prologue (addiu sp, sp, -XX)
                let mut func_start = target_phys;
                for _ in 0..200 {
                    if func_start < 4 {
                        break;
                    }
                    func_start -= 4;
                    let inst = read_u32(func_start);
                    if inst >> 16 == 0x27BD && (inst & 0x8000) != 0 {
                        break;
                    }
                }
                let func_va = 0x80000000u32 + func_start as u32;
                eprintln!(
                    "  --- func@{:#010X} (contains osSendMesg at {:#010X}) ---",
                    func_va, target_va
                );
                for k in 0..80usize {
                    let a = func_start + k * 4;
                    if a + 3 >= rdram.len() {
                        break;
                    }
                    let inst = read_u32(a);
                    let va = func_va.wrapping_add((k * 4) as u32);
                    let marker = if va == target_va {
                        " <-- osSendMesg"
                    } else {
                        ""
                    };
                    eprintln!(
                        "    {:#010X}: {:#010X}  {}{}",
                        va,
                        inst,
                        decode(va, inst),
                        marker
                    );
                    if inst == 0x03E00008 {
                        // jr $ra
                        let a2 = func_start + (k + 1) * 4;
                        if a2 + 3 < rdram.len() {
                            let i2 = read_u32(a2);
                            eprintln!("    {:#010X}: {:#010X}  {}", va + 4, i2, decode(va + 4, i2));
                        }
                        break;
                    }
                }
            }
        }
        // Targeted code dumps from RDRAM (game code is in memory after boot)
        {
            let rdram = n64.rdram_data();
            let read_u32_at = |off: usize| -> u32 {
                if off + 3 < rdram.len() {
                    u32::from_be_bytes([rdram[off], rdram[off + 1], rdram[off + 2], rdram[off + 3]])
                } else {
                    0
                }
            };
            let decode_inst = |addr: u32, inst: u32| -> String {
                let op = inst >> 26;
                match op {
                    0 => {
                        let func = inst & 0x3F;
                        match func {
                            0x08 => "jr $ra".into(),
                            0x09 => format!("jalr ${}", (inst >> 11) & 0x1F),
                            0x21 => format!(
                                "addu ${},${},${}",
                                (inst >> 11) & 0x1F,
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F
                            ),
                            0x25 => format!(
                                "or ${},${},${}",
                                (inst >> 11) & 0x1F,
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F
                            ),
                            0x24 => format!(
                                "and ${},${},${}",
                                (inst >> 11) & 0x1F,
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F
                            ),
                            _ => format!("special/{:#04X}", func),
                        }
                    }
                    2 => format!(
                        "j {:#010X}",
                        ((inst & 0x03FFFFFF) << 2) | (addr & 0xF0000000)
                    ),
                    3 => {
                        let target = ((inst & 0x03FFFFFF) << 2) | (addr & 0xF0000000);
                        let tag = match target {
                            0x80322800 => " [osRecvMesg]",
                            0x80322C20 => " [osSendMesg]",
                            _ => "",
                        };
                        format!("jal {:#010X}{}", target, tag)
                    }
                    4 => format!(
                        "beq ${},${},{:+}",
                        (inst >> 21) & 0x1F,
                        (inst >> 16) & 0x1F,
                        (inst as u16 as i16) * 4
                    ),
                    5 => format!(
                        "bne ${},${},{:+}",
                        (inst >> 21) & 0x1F,
                        (inst >> 16) & 0x1F,
                        (inst as u16 as i16) * 4
                    ),
                    6 => format!(
                        "blez ${},{:+}",
                        (inst >> 21) & 0x1F,
                        (inst as u16 as i16) * 4
                    ),
                    9 => format!(
                        "addiu ${},${},{}",
                        (inst >> 16) & 0x1F,
                        (inst >> 21) & 0x1F,
                        inst as u16 as i16
                    ),
                    0xC => format!(
                        "andi ${},${},{:#X}",
                        (inst >> 16) & 0x1F,
                        (inst >> 21) & 0x1F,
                        inst & 0xFFFF
                    ),
                    0xD => format!(
                        "ori ${},${},{:#X}",
                        (inst >> 16) & 0x1F,
                        (inst >> 21) & 0x1F,
                        inst & 0xFFFF
                    ),
                    0xF => format!("lui ${},{:#06X}", (inst >> 16) & 0x1F, inst & 0xFFFF),
                    0x23 => format!(
                        "lw ${},{}(${})",
                        (inst >> 16) & 0x1F,
                        inst as u16 as i16,
                        (inst >> 21) & 0x1F
                    ),
                    0x2B => format!(
                        "sw ${},{}(${})",
                        (inst >> 16) & 0x1F,
                        inst as u16 as i16,
                        (inst >> 21) & 0x1F
                    ),
                    _ => format!("op={:#04X}", op),
                }
            };

            // 1. Exception handler around SI_STATUS write (0x803279E0 to 0x80327B00)
            eprintln!("  --- Exception handler SI dispatch (0x803279E0..0x80327B00) ---");
            let si_start = 0x3279E0usize;
            for k in 0..72usize {
                let a = si_start + k * 4;
                let inst = read_u32_at(a);
                let va = 0x80000000u32 + a as u32;
                eprintln!(
                    "    {:#010X}: {:#010X}  {}",
                    va,
                    inst,
                    decode_inst(va, inst)
                );
            }

            // 2. __osSiRawStartDma at 0x80328960
            eprintln!("  --- __osSiRawStartDma func@0x80328960 ---");
            let raw_start = 0x328960usize;
            for k in 0..80usize {
                let a = raw_start + k * 4;
                let inst = read_u32_at(a);
                let va = 0x80000000u32 + a as u32;
                eprintln!(
                    "    {:#010X}: {:#010X}  {}",
                    va,
                    inst,
                    decode_inst(va, inst)
                );
                if inst == 0x03E00008 {
                    // jr $ra
                    let a2 = raw_start + (k + 1) * 4;
                    let i2 = read_u32_at(a2);
                    eprintln!(
                        "    {:#010X}: {:#010X}  {}",
                        va + 4,
                        i2,
                        decode_inst(va + 4, i2)
                    );
                    break;
                }
            }

            // 3. Mystery function at 0x803225A0
            eprintln!("  --- func@0x803225A0 (called from SI callbacks) ---");
            let mystery_start = 0x3225A0usize;
            for k in 0..80usize {
                let a = mystery_start + k * 4;
                let inst = read_u32_at(a);
                let va = 0x80000000u32 + a as u32;
                eprintln!(
                    "    {:#010X}: {:#010X}  {}",
                    va,
                    inst,
                    decode_inst(va, inst)
                );
                if inst == 0x03E00008 {
                    let a2 = mystery_start + (k + 1) * 4;
                    let i2 = read_u32_at(a2);
                    eprintln!(
                        "    {:#010X}: {:#010X}  {}",
                        va + 4,
                        i2,
                        decode_inst(va + 4, i2)
                    );
                    break;
                }
            }

            // 4. __osSiInterrupt full dump: 0x803279C0 to 0x80327AF0
            // (the main dispatch logic after MI_INTR check)
            eprintln!("  --- __osSiInterrupt event dispatch (0x803279C0..0x80327AF0) ---");
            let si_int_start = 0x3279C0usize;
            for k in 0..76usize {
                let a = si_int_start + k * 4;
                let inst = read_u32_at(a);
                let va = 0x80000000u32 + a as u32;
                eprintln!(
                    "    {:#010X}: {:#010X}  {}",
                    va,
                    inst,
                    decode_inst(va, inst)
                );
            }

            // 5. Dump osSetEventMesg (search for it — sets __osEventStateTab entries)
            // It's typically called with (event_id, queue, msg)
            // Search for stores to 0x80335A* area (event table)
            eprintln!("  --- osSetEventMesg search (stores to event table) ---");
            // Search for the event table by looking at what the callbacks reference
            // func@0x803283F0 writes 1 to 0x80335A50, func@0x803288A0 writes 1 to 0x80335A60
            // These are likely __osSiCallBack and __osContCallBack flags
            // The event table entries are nearby. Dump 0x80335A40..0x80335A70
            eprintln!("    Event table area (0x335A40..0x335A70):");
            for off in (0x335A40usize..0x335A70).step_by(4) {
                let val = read_u32_at(off);
                if val != 0 {
                    eprintln!(
                        "      [{:#010X}] = {:#010X}",
                        0x80000000u32 + off as u32,
                        val
                    );
                }
            }

            // 6. Dump __osContRelAccess / __osSiRelAccess at 0x80328934
            eprintln!("  --- func@0x80328934 (release function) ---");
            let rel_start = 0x328934usize;
            for k in 0..30usize {
                let a = rel_start + k * 4;
                let inst = read_u32_at(a);
                let va = 0x80000000u32 + a as u32;
                eprintln!(
                    "    {:#010X}: {:#010X}  {}",
                    va,
                    inst,
                    decode_inst(va, inst)
                );
                if inst == 0x03E00008 {
                    let a2 = rel_start + (k + 1) * 4;
                    let i2 = read_u32_at(a2);
                    eprintln!(
                        "    {:#010X}: {:#010X}  {}",
                        va + 4,
                        i2,
                        decode_inst(va + 4, i2)
                    );
                    break;
                }
            }

            // 7. Dump __osContGetAccess at 0x803288F0
            eprintln!("  --- func@0x803288F0 (acquire function) ---");
            let acq_start = 0x3288F0usize;
            for k in 0..30usize {
                let a = acq_start + k * 4;
                let inst = read_u32_at(a);
                let va = 0x80000000u32 + a as u32;
                eprintln!(
                    "    {:#010X}: {:#010X}  {}",
                    va,
                    inst,
                    decode_inst(va, inst)
                );
                if inst == 0x03E00008 {
                    let a2 = acq_start + (k + 1) * 4;
                    let i2 = read_u32_at(a2);
                    eprintln!(
                        "    {:#010X}: {:#010X}  {}",
                        va + 4,
                        i2,
                        decode_inst(va + 4, i2)
                    );
                    break;
                }
            }

            // 8. Dump mystery function at 0x80329340
            eprintln!("  --- func@0x80329340 (called between osEepromRead and wrapper DMAs) ---");
            let mys_start = 0x329340usize;
            for k in 0..60usize {
                let a = mys_start + k * 4;
                let inst = read_u32_at(a);
                let va = 0x80000000u32 + a as u32;
                eprintln!(
                    "    {:#010X}: {:#010X}  {}",
                    va,
                    inst,
                    decode_inst(va, inst)
                );
                if inst == 0x03E00008 {
                    let a2 = mys_start + (k + 1) * 4;
                    let i2 = read_u32_at(a2);
                    eprintln!(
                        "    {:#010X}: {:#010X}  {}",
                        va + 4,
                        i2,
                        decode_inst(va + 4, i2)
                    );
                    break;
                }
            }

            // 9. Dump __osDispatchEvent at 0x80327B98
            eprintln!("  --- __osDispatchEvent func@0x80327B98 ---");
            let disp_start = 0x327B98usize;
            for k in 0..60usize {
                let a = disp_start + k * 4;
                let inst = read_u32_at(a);
                let va = 0x80000000u32 + a as u32;
                eprintln!(
                    "    {:#010X}: {:#010X}  {}",
                    va,
                    inst,
                    decode_inst(va, inst)
                );
                if inst == 0x03E00008 {
                    let a2 = disp_start + (k + 1) * 4;
                    let i2 = read_u32_at(a2);
                    eprintln!(
                        "    {:#010X}: {:#010X}  {}",
                        va + 4,
                        i2,
                        decode_inst(va + 4, i2)
                    );
                    break;
                }
            }
        }

        eprintln!(
            "  MI mask={:06b} (SP={} SI={} AI={} VI={} PI={} DP={})",
            n64.bus.mi.intr_mask,
            n64.bus.mi.intr_mask & 1,
            (n64.bus.mi.intr_mask >> 1) & 1,
            (n64.bus.mi.intr_mask >> 2) & 1,
            (n64.bus.mi.intr_mask >> 3) & 1,
            (n64.bus.mi.intr_mask >> 4) & 1,
            (n64.bus.mi.intr_mask >> 5) & 1
        );

        // SP_STATUS write log
        if !n64.bus.rsp.status_log.is_empty() {
            eprintln!(
                "  SP_STATUS writes (first {}):",
                n64.bus.rsp.status_log.len()
            );
            for (i, &(val, status, auto_complete)) in n64.bus.rsp.status_log.iter().enumerate() {
                let ac = if auto_complete { " AUTO-COMPLETE" } else { "" };
                eprintln!(
                    "    #{}: val={:#010X} → status={:#06X}{}",
                    i + 1,
                    val,
                    status,
                    ac
                );
            }
        }

        // RSP task submit PCs
        if !rsp_task_pcs.is_empty() {
            eprintln!("  RSP task PCs (first {}):", rsp_task_pcs.len());
            for (pc, step) in &rsp_task_pcs {
                eprintln!("    step={}: PC={:#010X}", step, pc);
            }
        }

        // === Thread analysis ===
        // Walk RDRAM to find thread structs, then dump saved PC for WAITING threads
        let read_u32 = |off: usize| -> u32 {
            if off + 3 < rdram.len() {
                u32::from_be_bytes([rdram[off], rdram[off + 1], rdram[off + 2], rdram[off + 3]])
            } else {
                0
            }
        };

        // Find __osActiveQueue by scanning common OS global locations
        // Strategy: scan for pointers that lead to valid thread chains via tlnext (+0x0C)
        //
        // OSThread layout (corrected offsets):
        //   +0x00: next, +0x04: priority, +0x08: queue, +0x0C: tlnext
        //   +0x10: state(u16), +0x12: flags(u16), +0x14: id, +0x18: fp_flag
        //   +0x1C: __OSThreadContext (29 GPRs as u64, lo/hi as u64, then u32 fields)
        //     context+0xD0: sp (u64, low 32 bits at +0xD4)
        //     context+0xE0: ra (u64, low 32 bits at +0xE4)
        //     context+0xF8: sr (u32), context+0xFC: pc (u32)
        //   Context starts at thread+0x20 (4 bytes padding after fp_flag)
        //   29 GPRs × 8 bytes (r1-r25, then r28-r31 skipping k0/k1)
        //   sp(r29) at thread+0xF0 (u64), ra(r31) at thread+0x100 (u64)
        //   lo at +0x108 (u64), hi at +0x110 (u64)
        //   sr at +0x118 (u32), PC at +0x11C (u32)
        //   cause at +0x120, badvaddr at +0x124

        let mut best_chain: Vec<(u32, u32, u16, u32, u32, u32, u32, u32)> = Vec::new();
        let mut best_chain_start = 0u32;

        for scan_addr in (0x300000..0x400000).step_by(4) {
            let ptr = read_u32(scan_addr);
            if ptr < 0x8000_0000 || ptr >= 0x8080_0000 {
                continue;
            }
            let phys = (ptr & 0x7FFFFF) as usize;
            if phys + 0x11C >= rdram.len() {
                continue;
            }

            let pri = read_u32(phys + 4);
            let state = u16::from_be_bytes([rdram[phys + 0x10], rdram[phys + 0x11]]);
            if pri > 255 || !(state == 1 || state == 2 || state == 4 || state == 8) {
                continue;
            }

            let mut chain = Vec::new();
            let mut cur = ptr;
            for _ in 0..20 {
                let p = (cur & 0x7FFFFF) as usize;
                if p + 0x11C >= rdram.len() {
                    break;
                }
                let pri = read_u32(p + 4);
                let state = u16::from_be_bytes([rdram[p + 0x10], rdram[p + 0x11]]);
                let id = read_u32(p + 0x14);
                let saved_pc = read_u32(p + 0x11C); // OS_STATE_PC
                let saved_ra = read_u32(p + 0x104); // low 32 of ra (u64 at +0x100)
                let queue = read_u32(p + 8);
                if pri > 255 {
                    break;
                }
                chain.push((cur, pri, state, id, saved_pc, saved_ra, queue, 0));
                let next = read_u32(p + 0x0C);
                if next == 0 || next < 0x8000_0000 || next >= 0x8080_0000 || next == cur {
                    break;
                }
                cur = next;
            }
            if chain.len() > best_chain.len() {
                best_chain = chain;
                best_chain_start = 0x80000000 + scan_addr as u32;
            }
        }

        if !best_chain.is_empty() {
            eprintln!(
                "  Thread list ({} threads, chain from {:#010X}):",
                best_chain.len(),
                best_chain_start
            );
            for &(vaddr, pri, state, id, saved_pc, saved_ra, queue, _) in &best_chain {
                let s = match state {
                    1 => "STOP",
                    2 => "RUNNABLE",
                    4 => "RUNNING",
                    8 => "WAITING",
                    _ => "?",
                };
                let pri_i = pri as i32;
                eprintln!(
                    "    {:#010X}: pri={:4} {:8} id={:3} PC={:#010X} RA={:#010X} queue={:#010X}",
                    vaddr, pri_i, s, id, saved_pc, saved_ra, queue
                );

                // For WAITING threads, disassemble around saved PC and queue info
                if state == 8 {
                    // Mini MIPS disassembler for call chain analysis
                    let decode_mips = |addr: u32, inst: u32| -> String {
                        let op = inst >> 26;
                        match op {
                            0 => {
                                // SPECIAL
                                let func = inst & 0x3F;
                                match func {
                                    0x08 => "jr      $ra".to_string(),
                                    0x09 => format!("jalr    ${}", (inst >> 11) & 0x1F),
                                    0x21 => format!(
                                        "addu    ${},${},${}",
                                        (inst >> 11) & 0x1F,
                                        (inst >> 21) & 0x1F,
                                        (inst >> 16) & 0x1F
                                    ),
                                    0x25 => format!(
                                        "or      ${},${},${}",
                                        (inst >> 11) & 0x1F,
                                        (inst >> 21) & 0x1F,
                                        (inst >> 16) & 0x1F
                                    ),
                                    _ => format!("special/{:#04X}", func),
                                }
                            }
                            2 => {
                                // J
                                let target = ((inst & 0x03FFFFFF) << 2) | (addr & 0xF0000000);
                                format!("j       {:#010X}", target)
                            }
                            3 => {
                                // JAL
                                let target = ((inst & 0x03FFFFFF) << 2) | (addr & 0xF0000000);
                                format!("jal     {:#010X}", target)
                            }
                            4 => format!(
                                "beq     ${},${},{:+}",
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F,
                                (inst as u16 as i16) * 4
                            ),
                            5 => format!(
                                "bne     ${},${},{:+}",
                                (inst >> 21) & 0x1F,
                                (inst >> 16) & 0x1F,
                                (inst as u16 as i16) * 4
                            ),
                            9 => format!(
                                "addiu   ${},${},{}",
                                (inst >> 16) & 0x1F,
                                (inst >> 21) & 0x1F,
                                inst as u16 as i16
                            ),
                            0xD => format!(
                                "ori     ${},${},{:#X}",
                                (inst >> 16) & 0x1F,
                                (inst >> 21) & 0x1F,
                                inst & 0xFFFF
                            ),
                            0xF => {
                                format!("lui     ${},{:#06X}", (inst >> 16) & 0x1F, inst & 0xFFFF)
                            }
                            0x23 => format!(
                                "lw      ${},{}(${})",
                                (inst >> 16) & 0x1F,
                                inst as u16 as i16,
                                (inst >> 21) & 0x1F
                            ),
                            0x2B => format!(
                                "sw      ${},{}(${})",
                                (inst >> 16) & 0x1F,
                                inst as u16 as i16,
                                (inst >> 21) & 0x1F
                            ),
                            _ => format!("op={:#04X}", op),
                        }
                    };

                    if saved_pc >= 0x8000_0000 && saved_pc < 0x8080_0000 {
                        let pc_phys = (saved_pc & 0x7FFFFF) as usize;
                        // Disassemble 8 instructions before and 4 after saved_pc
                        let start = pc_phys.saturating_sub(32);
                        let end = (pc_phys + 16).min(rdram.len().saturating_sub(3));
                        eprintln!(
                            "             --- disasm around saved_pc={:#010X} ---",
                            saved_pc
                        );
                        let mut addr = 0x80000000u32.wrapping_add(start as u32);
                        let mut off = start;
                        while off < end {
                            let inst = read_u32(off);
                            let marker = if off == pc_phys { " <-- saved_pc" } else { "" };
                            eprintln!(
                                "               {:#010X}: {:#010X}  {}{}",
                                addr,
                                inst,
                                decode_mips(addr, inst),
                                marker
                            );
                            addr = addr.wrapping_add(4);
                            off += 4;
                        }
                    }
                    // Examine the OSMesgQueue this thread is waiting on
                    // The thread's queue field points to &queue->mtqueue (for recv wait)
                    // OSMesgQueue: +0x00=mtqueue, +0x04=fullqueue, +0x08=validCount,
                    //              +0x0C=first, +0x10=msgCount, +0x14=msg_ptr
                    if queue >= 0x8000_0000 && queue < 0x8080_0000 {
                        let qp = (queue & 0x7FFFFF) as usize;
                        if qp + 0x18 < rdram.len() {
                            let valid = read_u32(qp + 8);
                            let first = read_u32(qp + 0xC);
                            let msg_count = read_u32(qp + 0x10);
                            let msg_ptr = read_u32(qp + 0x14);
                            eprintln!("             queue@{:#010X}: valid={} first={} cap={} msgs={:#010X}",
                                queue, valid, first, msg_count, msg_ptr);
                        }
                    }

                    // For the game thread (pri=10), dump caller functions
                    if pri == 10 {
                        // Collect unique function entries from stack
                        let sp_val = read_u32((vaddr & 0x7FFFFF) as usize + 0xF4);
                        if sp_val >= 0x8000_0000 && sp_val < 0x8080_0000 {
                            let sp_p = (sp_val & 0x7FFFFF) as usize;
                            let mut func_entries: Vec<u32> = Vec::new();
                            for j in 0..64usize {
                                let off = sp_p + j * 4;
                                if off + 3 >= rdram.len() {
                                    break;
                                }
                                let w = read_u32(off);
                                if w >= 0x8000_0000 && w < 0x8040_0000 {
                                    // Find function prologue
                                    let rp = (w & 0x7FFFFF) as usize;
                                    if rp >= 4 {
                                        let mut scan = rp;
                                        for _ in 0..256 {
                                            if scan < 4 {
                                                break;
                                            }
                                            scan -= 4;
                                            let inst = read_u32(scan);
                                            if inst >> 16 == 0x27BD && (inst & 0x8000) != 0 {
                                                let fe = 0x80000000 + scan as u32;
                                                if !func_entries.contains(&fe) {
                                                    func_entries.push(fe);
                                                }
                                                break;
                                            }
                                        }
                                    }
                                }
                            }
                            // Dump first 100 instructions of each unique caller function
                            for &fe in func_entries.iter().take(6) {
                                let fp = (fe & 0x7FFFFF) as usize;
                                eprintln!("             --- func@{:#010X} ---", fe);
                                for k in 0..100usize {
                                    let a = fp + k * 4;
                                    if a + 3 >= rdram.len() {
                                        break;
                                    }
                                    let inst = read_u32(a);
                                    let va = fe.wrapping_add((k * 4) as u32);
                                    let decoded = decode_mips(va, inst);
                                    eprintln!(
                                        "               {:#010X}: {:#010X}  {}",
                                        va, inst, decoded
                                    );
                                    // Stop at jr ra (function return)
                                    if inst == 0x03E00008 {
                                        // jr $ra
                                        // Print the delay slot too
                                        let a2 = fp + (k + 1) * 4;
                                        if a2 + 3 < rdram.len() {
                                            let inst2 = read_u32(a2);
                                            let va2 = fe.wrapping_add(((k + 1) * 4) as u32);
                                            eprintln!(
                                                "               {:#010X}: {:#010X}  {}",
                                                va2,
                                                inst2,
                                                decode_mips(va2, inst2)
                                            );
                                        }
                                        break;
                                    }
                                }
                            }
                        }
                    }

                    // Dump saved SP and stack frame for WAITING threads
                    let thread_phys = (vaddr & 0x7FFFFF) as usize;
                    // SP is at thread + 0xF4 (low 32 of u64 at +0xF0)
                    let sp_off = thread_phys + 0xF4;
                    // a0 is at thread + 0x20 (context) + 3*8 + 4 = +0x3C (low 32)
                    let a0_off = thread_phys + 0x3C;
                    if sp_off + 3 < rdram.len() && a0_off + 3 < rdram.len() {
                        let saved_sp = read_u32(sp_off);
                        let saved_a0 = read_u32(a0_off);
                        eprintln!("             SP={:#010X} a0={:#010X}", saved_sp, saved_a0);
                        // Walk stack: dump words looking for return addresses, with disasm
                        if saved_sp >= 0x8000_0000 && saved_sp < 0x8080_0000 {
                            let sp_p = (saved_sp & 0x7FFFFF) as usize;
                            let mut ret_addrs: Vec<(usize, u32)> = Vec::new();
                            for j in 0..64usize {
                                let off = sp_p + j * 4;
                                if off + 3 >= rdram.len() {
                                    break;
                                }
                                let w = read_u32(off);
                                // Code addresses in text segment
                                if w >= 0x8000_0000 && w < 0x8040_0000 {
                                    ret_addrs.push((j * 4, w));
                                }
                            }
                            if !ret_addrs.is_empty() {
                                eprintln!("             stack return addresses:");
                                for &(stack_off, ret_addr) in ret_addrs.iter().take(12) {
                                    let rp = (ret_addr & 0x7FFFFF) as usize;
                                    // Show the jal instruction BEFORE the return address
                                    // (ret_addr is the instruction after jal, so jal is at ret_addr-8
                                    //  due to delay slot: jal target; nop; <-- ret_addr)
                                    let mut context = String::new();
                                    if rp >= 8 && rp + 3 < rdram.len() {
                                        let jal_inst = read_u32(rp - 8);
                                        let jal_op = jal_inst >> 26;
                                        if jal_op == 3 {
                                            // JAL
                                            let target = ((jal_inst & 0x03FFFFFF) << 2)
                                                | (ret_addr & 0xF0000000);
                                            context = format!(" calls {:#010X}", target);
                                        } else if jal_op == 0 && (jal_inst & 0x3F) == 9 {
                                            // JALR
                                            context = " (jalr)".to_string();
                                        }
                                    }
                                    // Also find the function this return address is IN
                                    // by scanning backward for addiu sp,sp,-XX
                                    let mut func_entry = 0u32;
                                    if rp >= 4 {
                                        let mut scan = rp;
                                        for _ in 0..256 {
                                            if scan < 4 {
                                                break;
                                            }
                                            scan -= 4;
                                            let inst = read_u32(scan);
                                            // addiu sp, sp, negative = function prologue
                                            if inst >> 16 == 0x27BD && (inst & 0x8000) != 0 {
                                                func_entry = 0x80000000 + scan as u32;
                                                break;
                                            }
                                        }
                                    }
                                    let func_str = if func_entry != 0 {
                                        format!(" (in func@{:#010X})", func_entry)
                                    } else {
                                        String::new()
                                    };
                                    eprintln!(
                                        "               SP+{:#04X}: {:#010X}{}{}",
                                        stack_off, ret_addr, context, func_str
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        // === Queue scan: find ALL OSMesgQueue structs in BSS region ===
        // OSMesgQueue: +0x00=mtqueue(ptr), +0x04=fullqueue(ptr), +0x08=validCount(u32),
        //              +0x0C=first(u32), +0x10=msgCount(u32), +0x14=msg(ptr)
        eprintln!("  Queue scan (BSS 0x8033_0000-0x8037_0000):");
        for scan in (0x330000..0x370000).step_by(4) {
            if scan + 0x18 >= rdram.len() {
                break;
            }
            let valid_count = read_u32(scan + 8);
            let first = read_u32(scan + 0xC);
            let msg_count = read_u32(scan + 0x10);
            let msg_ptr = read_u32(scan + 0x14);
            // Heuristic: valid OSMesgQueue has reasonable msgCount and valid msg pointer
            if msg_count >= 1
                && msg_count <= 64
                && first < msg_count
                && valid_count <= msg_count
                && msg_ptr >= 0x8000_0000
                && msg_ptr < 0x8080_0000
            {
                let vaddr = 0x80000000 + scan as u32;
                let mtqueue = read_u32(scan);
                let fullqueue = read_u32(scan + 4);
                // Only show if it looks like a real queue (mtqueue is 0 or valid pointer)
                if mtqueue == 0 || (mtqueue >= 0x8000_0000 && mtqueue < 0x8080_0000) {
                    if fullqueue == 0 || (fullqueue >= 0x8000_0000 && fullqueue < 0x8080_0000) {
                        eprintln!(
                            "    {:#010X}: cap={} valid={} first={} msgs={:#010X}",
                            vaddr, msg_count, valid_count, first, msg_ptr
                        );
                    }
                }
            }
        }

        // PIF unhandled commands
        if !n64.bus.pif.unhandled_cmds.is_empty() {
            eprint!("  PIF unhandled:");
            for (cmd, count) in &n64.bus.pif.unhandled_cmds {
                eprint!(" cmd={:#04X}×{}", cmd, count);
            }
            eprintln!();
        }

        // VI register write counts
        let vi_reg_names = [
            "ctrl",
            "origin",
            "width",
            "v_intr",
            "v_current",
            "burst",
            "v_sync",
            "h_sync",
            "h_sync_leap",
            "h_video",
            "v_video",
            "v_burst",
            "x_scale",
            "y_scale",
        ];
        eprint!("  VI writes:");
        for (i, name) in vi_reg_names.iter().enumerate() {
            if n64.bus.vi.write_counts[i] > 0 {
                eprint!(" {}={}", name, n64.bus.vi.write_counts[i]);
            }
        }
        eprintln!();

        // SI DMA log
        eprintln!("  SI DMA log ({} entries):", n64.bus.si.dma_log.len());
        for (i, &(dram, dir)) in n64.bus.si.dma_log.iter().enumerate() {
            let dir_str = if dir == 0 {
                "PIF→RDRAM (read)"
            } else {
                "RDRAM→PIF (write)"
            };
            eprintln!("    #{}: RDRAM[{:#010X}] {}", i + 1, dram, dir_str);
        }

        // PI DMA log
        eprintln!("  PI DMA log ({} entries):", n64.bus.pi.dma_log.len());
        for (i, &(dram, cart, len)) in n64.bus.pi.dma_log.iter().enumerate() {
            if i < 50 {
                eprintln!(
                    "    #{}: ROM[{:#010X}] → RDRAM[{:#010X}], len={:#X}",
                    i + 1,
                    cart,
                    dram,
                    len
                );
            }
        }

        // PC histogram: top 20 most-visited addresses
        let mut top_pcs: Vec<(u32, u64)> = pc_hist.into_iter().collect();
        top_pcs.sort_by(|a, b| b.1.cmp(&a.1));
        eprintln!("  Top PCs (sampled every 1024 steps):");
        for (pc, count) in top_pcs.iter().take(20) {
            let phys = pc & 0x1FFF_FFFF;
            let opcode = if phys < 0x0080_0000 {
                u32::from_be_bytes([
                    n64.rdram_data()[phys as usize],
                    n64.rdram_data()[phys as usize + 1],
                    n64.rdram_data()[phys as usize + 2],
                    n64.rdram_data()[phys as usize + 3],
                ])
            } else {
                0
            };
            eprintln!("    {:#010X}: {} hits (opcode={:#010X})", pc, count, opcode);
        }

        n64.cpu.dump_unimpl_summary();
        save_screenshot(&n64);
        return;
    } else if use_test {
        // Test mode: run until r30 is set (50M cycles max)
        // Higher budget needed for realistic PI DMA timing (~19 cycles/byte)
        let result = n64.run_until_r30(50_000_000);
        n64.cpu.dump_unimpl_summary();
        if result == 0 {
            eprintln!("TIMEOUT: r30 never set (tests didn't finish)");
            std::process::exit(2);
        } else if result == 0xFFFF_FFFF_FFFF_FFFF {
            println!("PASS");
        } else {
            eprintln!("FAIL: test #{}", result);
            std::process::exit(1);
        }
    } else if use_tui {
        tui::run(n64);
    } else {
        let audio = audio::AudioOutput::new();
        if let Some(ref a) = audio {
            n64.set_audio_callback(a.make_callback());
        } else {
            log::warn!("Audio: no output device found, running without audio");
        }
        let event_loop = EventLoop::new().expect("create event loop");
        let gamepad = gamepad::GamepadInput::new();
        let mut app = App {
            n64,
            window: None,
            pixels: None,
            audio,
            last_frame_time: std::time::Instant::now(),
            arrow_up: false,
            arrow_down: false,
            arrow_left: false,
            arrow_right: false,
            save_slot: 0,
            keyboard_buttons: 0,
            keyboard_stick_x: 0,
            keyboard_stick_y: 0,
            gamepad,
            speed_percent: DEFAULT_SPEED_PERCENT,
            speed_frame_budget: 0.0,
            speed_message_until: None,
        };
        event_loop.run_app(&mut app).expect("run event loop");
    }
}
