mod audio;
mod debug_overlay;
mod tui;

use std::path::PathBuf;

use pixels::{Pixels, SurfaceTexture};
use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::keyboard::{KeyCode, PhysicalKey};
use winit::window::{Window, WindowId};

use n64_core::memory::pif::buttons;

const N64_WIDTH: u32 = 320;
const N64_HEIGHT: u32 = 240;
const SCALE: u32 = 2;

struct App {
    n64: n64_core::N64,
    window: Option<&'static Window>,
    pixels: Option<Pixels<'static>>,
    audio: Option<audio::AudioOutput>,
    last_frame_time: std::time::Instant,
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
            WindowEvent::CloseRequested => event_loop.exit(),
            WindowEvent::Resized(size) => {
                if let Some(pixels) = &mut self.pixels {
                    let _ = pixels.resize_surface(size.width.max(1), size.height.max(1));
                }
            }
            WindowEvent::KeyboardInput { event, .. } => {
                if let PhysicalKey::Code(key) = event.physical_key {
                    let pressed = event.state.is_pressed();
                    let ctrl = &mut self.n64.bus.pif.controller;

                    // Button mapping (physical key positions — layout-independent)
                    let btn = match key {
                        KeyCode::KeyX      => Some(buttons::A),
                        KeyCode::KeyZ      => Some(buttons::B),
                        KeyCode::Space     => Some(buttons::Z),
                        KeyCode::Enter     => Some(buttons::START),
                        KeyCode::ShiftLeft => Some(buttons::L),
                        KeyCode::ShiftRight => Some(buttons::R),
                        KeyCode::KeyI      => Some(buttons::C_UP),
                        KeyCode::KeyK      => Some(buttons::C_DOWN),
                        KeyCode::KeyJ      => Some(buttons::C_LEFT),
                        KeyCode::KeyL      => Some(buttons::C_RIGHT),
                        KeyCode::KeyW      => Some(buttons::D_UP),
                        KeyCode::KeyS      => Some(buttons::D_DOWN),
                        KeyCode::KeyA      => Some(buttons::D_LEFT),
                        KeyCode::KeyD      => Some(buttons::D_RIGHT),
                        _ => None,
                    };
                    if let Some(b) = btn {
                        if pressed { ctrl.buttons |= b; } else { ctrl.buttons &= !b; }
                    }

                    // Analog stick from arrow keys (±80 range)
                    match key {
                        KeyCode::ArrowUp    => ctrl.stick_y = if pressed { 80 } else { 0 },
                        KeyCode::ArrowDown  => ctrl.stick_y = if pressed { -80 } else { 0 },
                        KeyCode::ArrowLeft  => ctrl.stick_x = if pressed { -80 } else { 0 },
                        KeyCode::ArrowRight => ctrl.stick_x = if pressed { 80 } else { 0 },
                        _ => {}
                    }

                    // Escape to quit
                    if key == KeyCode::Escape && pressed {
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
                    // F1-F8 = debug overlays
                    if pressed {
                        debug_overlay::handle_f_key(&mut self.n64.debug, key);
                    }
                }
            }
            WindowEvent::RedrawRequested => {
                self.n64.run_frame();

                // Feed audio samples to output device
                if let Some(audio) = &self.audio {
                    let samples = self.n64.drain_audio_samples();
                    if !samples.is_empty() {
                        let rate = self.n64.audio_sample_rate();
                        audio.push_samples(&samples, rate);
                    }
                }

                // FPS timing
                let now = std::time::Instant::now();
                let dt = now.duration_since(self.last_frame_time).as_secs_f64() * 1000.0;
                self.last_frame_time = now;
                let pos = self.n64.debug.fps_write_pos;
                self.n64.debug.fps_samples[pos] = dt;
                self.n64.debug.fps_write_pos = (pos + 1) % 60;
                self.n64.debug.frame_count += 1;

                if let Some(pixels) = &mut self.pixels {
                    blit_framebuffer(&self.n64, pixels.frame_mut());
                    debug_overlay::draw_overlays(pixels.frame_mut(), &mut self.n64.debug, &self.n64.bus);
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
fn save_screenshot(n64: &n64_core::N64) {
    let origin = n64.vi_origin() as usize;
    let width = n64.vi_width().max(1) as usize;
    let format = n64.vi_pixel_format();
    let rdram = n64.rdram_data();
    let fb_size = width * 240 * if format == 3 { 4 } else { 2 };

    eprintln!("Screenshot: origin={:#X} width={} format={}", origin, width, format);
    eprintln!("  Renderer: color_image_addr={:#X} color_image_width={}",
        n64.bus.renderer.color_image_addr, n64.bus.renderer.color_image_width);

    if origin == 0 || format < 2 || origin + fb_size >= rdram.len() {
        eprintln!("  Cannot save: invalid framebuffer");
        return;
    }

    let mut ppm = b"P6\n320 240\n255\n".to_vec();
    for y in 0..240 {
        for x in 0..320usize {
            match format {
                2 => {
                    let off = origin + (y * width + x) * 2;
                    let pixel = u16::from_be_bytes([rdram[off], rdram[off + 1]]);
                    let r = ((pixel >> 11) & 0x1F) as u8;
                    let g = ((pixel >> 6) & 0x1F) as u8;
                    let b = ((pixel >> 1) & 0x1F) as u8;
                    ppm.push((r << 3) | (r >> 2));
                    ppm.push((g << 3) | (g >> 2));
                    ppm.push((b << 3) | (b >> 2));
                }
                3 => {
                    let off = origin + (y * width + x) * 4;
                    ppm.push(rdram[off]);
                    ppm.push(rdram[off + 1]);
                    ppm.push(rdram[off + 2]);
                }
                _ => { ppm.extend_from_slice(&[0, 0, 0]); }
            }
        }
    }
    std::fs::write("screenshot.ppm", &ppm).ok();
    eprintln!("  Saved screenshot.ppm");
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
    let rom_path = args.iter()
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
        let total_steps = 500_000_000u64;
        eprintln!("=== Boot diagnostic ({}M steps) ===", total_steps / 1_000_000);

        // PC histogram: sample every 1024 steps to keep overhead low
        let mut pc_hist: std::collections::HashMap<u32, u64> = std::collections::HashMap::new();
        let mut exception_count: u64 = 0;
        let mut rsp_task_pcs: Vec<(u32, u64)> = Vec::new();
        // Track GFX vs audio RSP task types
        let mut gfx_task_count: u32 = 0;
        let mut audio_task_count: u32 = 0;
        let mut other_task_count: u32 = 0;
        let mut dp_interrupt_count: u64 = 0;
        let mut sp_status_write_count: u64 = 0;
        let mut first_dp_step: u64 = 0;
        // Monitor gIntrMesgQueue for message arrivals
        let intr_q_valid_addr = 0x0033AE10usize; // validCount at queue+0x08
        let intr_q_first_addr = 0x0033AE14usize; // first at queue+0x0C
        let intr_q_msgs_addr = 0x0033AEC8usize;  // msg buffer
        let mut prev_valid: u32 = 0;
        let mut msg_log: Vec<(u64, u32)> = Vec::new(); // (step, msg_value)
        // Monitor gGfxVblankQueue — two candidate addresses:
        // Decomp symbols: 0x80367158 (phys 0x00367160 for validCount)
        // display_and_vsync's osRecvMesg arg: 0x8033B028 (phys 0x0033B030 for validCount)
        let gfx_vblank_q_valid_addr = 0x00367160usize;
        let mut prev_gfx_vblank_valid: u32 = 0;
        let mut gfx_vblank_log: Vec<(u64, u32)> = Vec::new();
        // Also monitor the REAL queue that display_and_vsync blocks on
        let real_gfx_q_valid_addr = 0x0033B030usize; // 0x8033B028 + 0x08
        let mut prev_real_gfx_valid: u32 = 0;
        let mut real_gfx_log: Vec<(u64, u32)> = Vec::new();
        // Track exec_display_list entry: scan backwards from 0x80246C68 to find function prologue
        let mut exec_dl_hits: u64 = 0;
        let mut exec_dl_first_step: u64 = 0;
        // Also track if thread5 game loop PC is in display_and_vsync range
        let mut game_vblank_q_valid_addr = 0x00367138usize + 8; // gGameVblankQueue.validCount
        let mut prev_game_vblank_valid: u32 = 0;
        let mut game_vblank_log: Vec<(u64, u32)> = Vec::new();
        for i in 0..total_steps {
            let pc32 = n64.cpu.pc as u32;
            if i & 0x3FF == 0 {
                *pc_hist.entry(pc32).or_default() += 1;
            }
            if pc32 == 0x80000180 { exception_count += 1; }
            // Track exec_display_list and its callers
            if pc32 == 0x80246C10 { // exec_display_list entry
                exec_dl_hits += 1;
                if exec_dl_hits == 1 { exec_dl_first_step = i; }
            }
            // render_init's jal exec_display_list
            if pc32 == 0x80247F94 && exec_dl_first_step == 0 {
                eprintln!("  ** render_init calls exec_display_list at step {}", i);
            }
            // display_and_vsync's jal exec_display_list
            if pc32 == 0x802480EC && exec_dl_first_step == 0 {
                eprintln!("  ** display_and_vsync calls exec_display_list at step {}", i);
            }
            // Track display_and_vsync entry region (find function start)
            // The osRecvMesg for gGfxVblankQueue should be shortly before 0x802480CC
            // Track if PC enters 0x80248040-0x80248100 range (approx display_and_vsync body)
            if pc32 >= 0x80248040 && pc32 <= 0x80248100 && exec_dl_first_step == 0 {
                // Only log first few
                static mut DAV_COUNT: u32 = 0;
                unsafe {
                    DAV_COUNT += 1;
                    if DAV_COUNT <= 20 {
                        eprintln!("  ** display_and_vsync body: step={} PC={:#010X}", i, pc32);
                    }
                }
            }
            // Monitor gGameVblankQueue (thread 5 waits on this for frame timing)
            {
                let rd = n64.rdram_data();
                if game_vblank_q_valid_addr + 3 < rd.len() {
                    let cur = u32::from_be_bytes([
                        rd[game_vblank_q_valid_addr], rd[game_vblank_q_valid_addr+1],
                        rd[game_vblank_q_valid_addr+2], rd[game_vblank_q_valid_addr+3],
                    ]);
                    if cur != prev_game_vblank_valid && game_vblank_log.len() < 50 {
                        game_vblank_log.push((i, cur));
                    }
                    prev_game_vblank_valid = cur;
                }
            }
            let rsp_before = n64.bus.rsp.start_count;
            n64.step_one();
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
                    eprintln!("  RSP task #{}: type={} flags={:#X} data_ptr={:#010X} step={} PC={:#010X}",
                        n64.bus.rsp.start_count, task_type, t1, t2, i, pc32);
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
                        rd[intr_q_valid_addr], rd[intr_q_valid_addr+1],
                        rd[intr_q_valid_addr+2], rd[intr_q_valid_addr+3]]);
                    if cur_valid > prev_valid && msg_log.len() < 500 {
                        let first_idx = u32::from_be_bytes([
                            rd[intr_q_first_addr], rd[intr_q_first_addr+1],
                            rd[intr_q_first_addr+2], rd[intr_q_first_addr+3]]);
                        let msg_idx = ((first_idx + cur_valid - 1) % 16) as usize;
                        let msg_off = intr_q_msgs_addr + msg_idx * 4;
                        let msg_val = u32::from_be_bytes([
                            rd[msg_off], rd[msg_off+1], rd[msg_off+2], rd[msg_off+3]]);
                        msg_log.push((i, msg_val));
                    }
                    prev_valid = cur_valid;
                }
                // Monitor gGfxVblankQueue (thread 5 unblocking queue)
                if gfx_vblank_q_valid_addr + 3 < rd.len() {
                    let cur = u32::from_be_bytes([
                        rd[gfx_vblank_q_valid_addr], rd[gfx_vblank_q_valid_addr+1],
                        rd[gfx_vblank_q_valid_addr+2], rd[gfx_vblank_q_valid_addr+3]]);
                    if cur != prev_gfx_vblank_valid && gfx_vblank_log.len() < 100 {
                        gfx_vblank_log.push((i, cur));
                    }
                    prev_gfx_vblank_valid = cur;
                }
                // Monitor the REAL gGfxVblankQueue (from display_and_vsync disasm: 0x8033B028)
                if real_gfx_q_valid_addr + 3 < rd.len() {
                    let cur = u32::from_be_bytes([
                        rd[real_gfx_q_valid_addr], rd[real_gfx_q_valid_addr+1],
                        rd[real_gfx_q_valid_addr+2], rd[real_gfx_q_valid_addr+3]]);
                    if cur != prev_real_gfx_valid && real_gfx_log.len() < 100 {
                        real_gfx_log.push((i, cur));
                        if cur > prev_real_gfx_valid {
                            eprintln!("  ** REAL gGfxVblankQueue ({:#010X}) primed: step={} validCount={} PC={:#010X}",
                                0x8033B028u32, i, cur, pc32);
                        }
                    }
                    prev_real_gfx_valid = cur;
                }
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

        eprintln!("\n  DMAs={} RSP_tasks={} (gfx={} audio={} other={}) PC={:#010X}",
            n64.bus.pi.dma_count, n64.bus.rsp.start_count,
            gfx_task_count, audio_task_count, other_task_count,
            n64.cpu.pc as u32);
        eprintln!("  VI: ctrl={:#010X} origin={:#010X} width={} h_video={:#010X}",
            n64.bus.vi.ctrl, n64.bus.vi.origin, n64.bus.vi.width, n64.bus.vi.h_video);

        let status = n64.cpu.cop0.regs[n64_core::cpu::cop0::Cop0::STATUS] as u32;
        let cause = n64.cpu.cop0.regs[n64_core::cpu::cop0::Cop0::CAUSE] as u32;
        let ie = status & 1 != 0;
        let exl = status & 2 != 0;
        let erl = status & 4 != 0;
        let ip = (cause >> 8) & 0xFF;
        let im = (status >> 8) & 0xFF;
        eprintln!("  COP0: IE={} EXL={} ERL={} IP={:08b} IM={:08b} IP&IM={:08b}",
            ie as u8, exl as u8, erl as u8, ip, im, ip & im);
        eprintln!("  MI: intr={:06b} mask={:06b} pending={}",
            n64.bus.mi.intr, n64.bus.mi.intr_mask, n64.bus.mi.interrupt_pending());
        let names = ["SP", "SI", "AI", "VI", "PI", "DP"];
        eprint!("  MI set/clear:");
        for (i, name) in names.iter().enumerate() {
            if n64.bus.mi.set_counts[i] > 0 || n64.bus.mi.clear_counts[i] > 0 {
                eprint!(" {}={}/{}", name, n64.bus.mi.set_counts[i], n64.bus.mi.clear_counts[i]);
            }
        }
        eprintln!();
        eprintln!("  VI: v_current={} v_intr={} v_sync={}",
            n64.bus.vi.v_current, n64.bus.vi.v_intr, n64.bus.vi.v_sync);
        eprintln!("  SI: {} DMAs  AI: {} DMAs (dacrate={})",
            n64.bus.si.dma_count, n64.bus.ai.dma_count, n64.bus.ai.dacrate);
        eprintln!("  Renderer: {} fills, {} tex_rects, {} vtx, {} tris, {} pixels, {} z_fail, {} culled",
            n64.bus.renderer.fill_rect_count,
            n64.bus.renderer.tex_rect_count,
            n64.bus.renderer.vtx_count,
            n64.bus.renderer.tri_count,
            n64.bus.renderer.pixel_count,
            n64.bus.renderer.z_fail_count,
            n64.bus.renderer.cull_count);
        let origin = n64.bus.vi.origin as usize;
        let rdram = n64.rdram_data();
        let fb_size = 320 * 240 * 2;
        if origin > 0 && origin + fb_size < rdram.len() {
            let nonzero = (0..fb_size).filter(|&i| rdram[origin + i] != 0).count();
            eprintln!("  Framebuffer: {}% non-zero at RDRAM[{:#X}]", nonzero * 100 / fb_size, origin);
        }
        eprintln!("  Exception entries: {}  DP interrupts: {}  SP_STATUS reads: {}",
            exception_count, dp_interrupt_count, n64.bus.rsp.status_read_count.get());

        // Dump gIntrMesgQueue message buffer (addr from thread 3's queue)
        // Queue at 0x8033AE08: msgs at 0x8033AEC8, cap=16
        if 0x0033AEC8usize + 64 < rdram.len() {
            eprint!("  gIntrMesgQueue msg buffer:");
            for j in 0..16u32 {
                let off = 0x0033AEC8 + (j as usize) * 4;
                let msg = u32::from_be_bytes([rdram[off], rdram[off+1], rdram[off+2], rdram[off+3]]);
                if msg != 0 {
                    eprint!(" [{}]={:#X}", j, msg);
                }
            }
            eprintln!();
        }

        // gIntrMesgQueue message log
        if !msg_log.is_empty() {
            eprintln!("  gIntrMesgQueue messages ({} arrivals, first 50):", msg_log.len());
            // Count message types
            let mut msg_counts: std::collections::HashMap<u32, u32> = std::collections::HashMap::new();
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

        // Search for ALL jal osSendMesg calls, find exec_display_list (msg=0x67)
        {
            let rdram = n64.rdram_data();
            let jal_send = 0x0C0C8B08u32; // jal osSendMesg (0x80322C20)
            let mut all_calls: Vec<(u32, Option<u32>)> = Vec::new();
            for addr in (0x200000..0x330000usize).step_by(4) {
                if addr + 3 >= rdram.len() { break; }
                let op = u32::from_be_bytes([rdram[addr], rdram[addr+1], rdram[addr+2], rdram[addr+3]]);
                if op == jal_send {
                    let va = 0x80000000u32 + addr as u32;
                    // Look for a1 value in nearby instructions
                    let mut a1_val: Option<u32> = None;
                    for off in (-40i32..4).step_by(4) {
                        let check = (addr as i32 + off) as usize;
                        if check + 3 >= rdram.len() { continue; }
                        let inst = u32::from_be_bytes([rdram[check], rdram[check+1], rdram[check+2], rdram[check+3]]);
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
                if scan_addr + 3 >= rdram.len() { break; }
                let inst = u32::from_be_bytes([rdram[scan_addr], rdram[scan_addr+1], rdram[scan_addr+2], rdram[scan_addr+3]]);
                if inst >> 16 == 0x27BD && (inst & 0x8000) != 0 { // addiu sp, sp, negative
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
                    if addr + 3 >= rdram.len() { break; }
                    let inst = u32::from_be_bytes([rdram[addr], rdram[addr+1], rdram[addr+2], rdram[addr+3]]);
                    let va = 0x80000000u32 + addr as u32;
                    // Annotate known patterns
                    let note = if inst == 0x0C091B04 { " <-- jal exec_display_list" }
                        else if inst == 0x0C0C8B08 { " <-- jal osSendMesg" }
                        else if inst >> 16 == 0x27BD { " <-- sp adjust" }
                        else if inst >> 26 == 3 { // jal
                            let target = (inst & 0x03FFFFFF) << 2 | 0x80000000;
                            if target == 0x80322868 { " <-- jal osRecvMesg" }
                            else { "" }
                        }
                        else { "" };
                    eprintln!("      {:#010X}: {:#010X}{}", va, inst, note);
                }
            }

            // Find callers of display_and_vsync
            if dav_entry != 0 {
                let target_idx = (dav_entry & 0x03FFFFFF) >> 2;
                let jal_dav = (3u32 << 26) | target_idx;
                eprintln!("  Callers of display_and_vsync (jal {:#010X} = {:#010X}):", dav_entry, jal_dav);
                for addr in (0x200000..0x340000usize).step_by(4) {
                    if addr + 3 >= rdram.len() { break; }
                    let op = u32::from_be_bytes([rdram[addr], rdram[addr+1], rdram[addr+2], rdram[addr+3]]);
                    if op == jal_dav {
                        eprintln!("    {:#010X}: jal display_and_vsync", 0x80000000u32 + addr as u32);
                    }
                }
            }
        }

        // exec_display_list call tracking
        eprintln!("  exec_display_list (0x80246C68): {} hits, first at step {}", exec_dl_hits, exec_dl_first_step);

        // gGfxVblankQueue monitoring results
        if !gfx_vblank_log.is_empty() {
            eprintln!("  gGfxVblankQueue changes ({}):", gfx_vblank_log.len());
            for &(step, valid) in gfx_vblank_log.iter().take(50) {
                eprintln!("    step={}: validCount={}", step, valid);
            }
        } else {
            eprintln!("  gGfxVblankQueue: NEVER changed from 0 (thread 5 stuck!)");
        }

        // REAL gGfxVblankQueue (0x8033B028) monitoring results
        if !real_gfx_log.is_empty() {
            eprintln!("  REAL gGfxVblankQueue (0x8033B028) changes ({}):", real_gfx_log.len());
            for &(step, valid) in real_gfx_log.iter().take(50) {
                eprintln!("    step={}: validCount={}", step, valid);
            }
        } else {
            eprintln!("  REAL gGfxVblankQueue (0x8033B028): NEVER changed from 0");
        }

        // gGameVblankQueue monitoring results
        if !game_vblank_log.is_empty() {
            eprintln!("  gGameVblankQueue changes ({}):", game_vblank_log.len());
            for &(step, valid) in game_vblank_log.iter().take(20) {
                eprintln!("    step={}: validCount={}", step, valid);
            }
        } else {
            eprintln!("  gGameVblankQueue: NEVER changed from 0");
        }
        eprintln!("  MI mask={:06b} (SP={} SI={} AI={} VI={} PI={} DP={})",
            n64.bus.mi.intr_mask,
            n64.bus.mi.intr_mask & 1, (n64.bus.mi.intr_mask >> 1) & 1,
            (n64.bus.mi.intr_mask >> 2) & 1, (n64.bus.mi.intr_mask >> 3) & 1,
            (n64.bus.mi.intr_mask >> 4) & 1, (n64.bus.mi.intr_mask >> 5) & 1);

        // SP_STATUS write log
        if !n64.bus.rsp.status_log.is_empty() {
            eprintln!("  SP_STATUS writes (first {}):", n64.bus.rsp.status_log.len());
            for (i, &(val, status, auto_complete)) in n64.bus.rsp.status_log.iter().enumerate() {
                let ac = if auto_complete { " AUTO-COMPLETE" } else { "" };
                eprintln!("    #{}: val={:#010X} → status={:#06X}{}", i+1, val, status, ac);
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
                u32::from_be_bytes([rdram[off], rdram[off+1], rdram[off+2], rdram[off+3]])
            } else { 0 }
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
            if ptr < 0x8000_0000 || ptr >= 0x8080_0000 { continue; }
            let phys = (ptr & 0x7FFFFF) as usize;
            if phys + 0x11C >= rdram.len() { continue; }

            let pri = read_u32(phys + 4);
            let state = u16::from_be_bytes([rdram[phys + 0x10], rdram[phys + 0x11]]);
            if pri > 255 || !(state == 1 || state == 2 || state == 4 || state == 8) { continue; }

            let mut chain = Vec::new();
            let mut cur = ptr;
            for _ in 0..20 {
                let p = (cur & 0x7FFFFF) as usize;
                if p + 0x11C >= rdram.len() { break; }
                let pri = read_u32(p + 4);
                let state = u16::from_be_bytes([rdram[p + 0x10], rdram[p + 0x11]]);
                let id = read_u32(p + 0x14);
                let saved_pc = read_u32(p + 0x11C);  // OS_STATE_PC
                let saved_ra = read_u32(p + 0x104);  // low 32 of ra (u64 at +0x100)
                let queue = read_u32(p + 8);
                if pri > 255 { break; }
                chain.push((cur, pri, state, id, saved_pc, saved_ra, queue, 0));
                let next = read_u32(p + 0x0C);
                if next == 0 || next < 0x8000_0000 || next >= 0x8080_0000 || next == cur { break; }
                cur = next;
            }
            if chain.len() > best_chain.len() {
                best_chain = chain;
                best_chain_start = 0x80000000 + scan_addr as u32;
            }
        }

        if !best_chain.is_empty() {
            eprintln!("  Thread list ({} threads, chain from {:#010X}):", best_chain.len(), best_chain_start);
            for &(vaddr, pri, state, id, saved_pc, saved_ra, queue, _) in &best_chain {
                let s = match state { 1 => "STOP", 2 => "RUNNABLE", 4 => "RUNNING", 8 => "WAITING", _ => "?" };
                let pri_i = pri as i32;
                eprintln!("    {:#010X}: pri={:4} {:8} id={:3} PC={:#010X} RA={:#010X} queue={:#010X}",
                    vaddr, pri_i, s, id, saved_pc, saved_ra, queue);

                // For WAITING threads, dump code at saved PC and queue info
                if state == 8 {
                    if saved_pc >= 0x8000_0000 && saved_pc < 0x8080_0000 {
                        let pc_phys = (saved_pc & 0x7FFFFF) as usize;
                        if pc_phys + 15 < rdram.len() {
                            eprint!("             code@PC:");
                            for j in 0..4u32 {
                                let op = read_u32(pc_phys + (j as usize) * 4);
                                eprint!(" {:#010X}", op);
                            }
                            eprintln!();
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

                    // Dump saved SP and stack frame for WAITING threads
                    let thread_phys = (vaddr & 0x7FFFFF) as usize;
                    // SP is at thread + 0xF4 (low 32 of u64 at +0xF0)
                    let sp_off = thread_phys + 0xF4;
                    // a0 is at thread + 0x20 (context) + 3*8 + 4 = +0x3C (low 32)
                    let a0_off = thread_phys + 0x3C;
                    if sp_off + 3 < rdram.len() && a0_off + 3 < rdram.len() {
                        let saved_sp = read_u32(sp_off);
                        let saved_a0 = read_u32(a0_off);
                        eprint!("             SP={:#010X} a0={:#010X}", saved_sp, saved_a0);
                        // Walk stack: dump 32 words looking for return addresses
                        if saved_sp >= 0x8000_0000 && saved_sp < 0x8080_0000 {
                            let sp_p = (saved_sp & 0x7FFFFF) as usize;
                            eprint!(" stack[");
                            let mut first_addr = true;
                            for j in 0..32usize {
                                let off = sp_p + j * 4;
                                if off + 3 >= rdram.len() { break; }
                                let w = read_u32(off);
                                // Show all stack words that look like code addresses
                                if w >= 0x8000_0000 && w < 0x8040_0000 {
                                    if !first_addr { eprint!(","); }
                                    eprint!(" +{:#X}:{:#010X}", j*4, w);
                                    first_addr = false;
                                }
                            }
                            eprint!("]");
                        }
                        eprintln!();
                    }
                }
            }
        }

        // === Queue scan: find ALL OSMesgQueue structs in BSS region ===
        // OSMesgQueue: +0x00=mtqueue(ptr), +0x04=fullqueue(ptr), +0x08=validCount(u32),
        //              +0x0C=first(u32), +0x10=msgCount(u32), +0x14=msg(ptr)
        eprintln!("  Queue scan (BSS 0x8033_0000-0x8037_0000):");
        for scan in (0x330000..0x370000).step_by(4) {
            if scan + 0x18 >= rdram.len() { break; }
            let valid_count = read_u32(scan + 8);
            let first = read_u32(scan + 0xC);
            let msg_count = read_u32(scan + 0x10);
            let msg_ptr = read_u32(scan + 0x14);
            // Heuristic: valid OSMesgQueue has reasonable msgCount and valid msg pointer
            if msg_count >= 1 && msg_count <= 64
                && first < msg_count
                && valid_count <= msg_count
                && msg_ptr >= 0x8000_0000 && msg_ptr < 0x8080_0000
            {
                let vaddr = 0x80000000 + scan as u32;
                let mtqueue = read_u32(scan);
                let fullqueue = read_u32(scan + 4);
                // Only show if it looks like a real queue (mtqueue is 0 or valid pointer)
                if mtqueue == 0 || (mtqueue >= 0x8000_0000 && mtqueue < 0x8080_0000) {
                    if fullqueue == 0 || (fullqueue >= 0x8000_0000 && fullqueue < 0x8080_0000) {
                        eprintln!("    {:#010X}: cap={} valid={} first={} msgs={:#010X}",
                            vaddr, msg_count, valid_count, first, msg_ptr);
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
        let vi_reg_names = ["ctrl","origin","width","v_intr","v_current","burst",
            "v_sync","h_sync","h_sync_leap","h_video","v_video","v_burst","x_scale","y_scale"];
        eprint!("  VI writes:");
        for (i, name) in vi_reg_names.iter().enumerate() {
            if n64.bus.vi.write_counts[i] > 0 {
                eprint!(" {}={}", name, n64.bus.vi.write_counts[i]);
            }
        }
        eprintln!();

        // PI DMA log
        eprintln!("  PI DMA log ({} entries):", n64.bus.pi.dma_log.len());
        for (i, &(dram, cart, len)) in n64.bus.pi.dma_log.iter().enumerate() {
            if i < 50 {
                eprintln!("    #{}: ROM[{:#010X}] → RDRAM[{:#010X}], len={:#X}", i+1, cart, dram, len);
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
            } else { 0 };
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
        if audio.is_none() {
            log::warn!("Audio: no output device found, running without audio");
        }
        let event_loop = EventLoop::new().expect("create event loop");
        let mut app = App { n64, window: None, pixels: None, audio, last_frame_time: std::time::Instant::now() };
        event_loop.run_app(&mut app).expect("run event loop");
    }
}
