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
                }
            }
            WindowEvent::RedrawRequested => {
                self.n64.run_frame();

                if let Some(pixels) = &mut self.pixels {
                    blit_framebuffer(&self.n64, pixels.frame_mut());
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
        let total_steps = 2_000_000_000u64;
        eprintln!("=== Boot diagnostic ({}M steps) ===", total_steps / 1_000_000);

        for _ in 0..total_steps {
            n64.step_one();
        }

        eprintln!("\n  DMAs={} RSP_tasks={} PC={:#010X}",
            n64.bus.pi.dma_count, n64.bus.rsp.start_count, n64.cpu.pc as u32);
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
        eprintln!("  VI: v_current={} v_intr={} v_sync={}",
            n64.bus.vi.v_current, n64.bus.vi.v_intr, n64.bus.vi.v_sync);
        eprintln!("  SI: {} DMAs  AI: {} DMAs (dacrate={})",
            n64.bus.si.dma_count, n64.bus.ai.dma_count, n64.bus.ai.dacrate);
        eprintln!("  VI: x_scale={:#X} y_scale={:#X}",
            n64.bus.vi.x_scale, n64.bus.vi.y_scale);
        eprintln!("  Renderer: color_image_addr={:#X} color_image_width={}",
            n64.bus.renderer.color_image_addr, n64.bus.renderer.color_image_width);
        eprintln!("  Viewport: scale=({:.1},{:.1},{:.1}) trans=({:.1},{:.1},{:.1})",
            n64.bus.renderer.viewport_scale[0], n64.bus.renderer.viewport_scale[1],
            n64.bus.renderer.viewport_scale[2],
            n64.bus.renderer.viewport_trans[0], n64.bus.renderer.viewport_trans[1],
            n64.bus.renderer.viewport_trans[2]);

        // Show where the CPU is idling
        eprintln!("  Idle PC={:#010X}", n64.cpu.pc as u32);
        eprintln!("  Renderer: {} fills, {} tex_rects ({} skipped), {} vtx, {} tris",
            n64.bus.renderer.fill_rect_count,
            n64.bus.renderer.tex_rect_count,
            n64.bus.renderer.tex_rect_skip,
            n64.bus.renderer.vtx_count,
            n64.bus.renderer.tri_count);

        eprintln!("  Fog: mul={} off={} color=({},{},{},{})",
            n64.bus.renderer.fog_multiplier, n64.bus.renderer.fog_offset,
            n64.bus.renderer.fog_color[0], n64.bus.renderer.fog_color[1],
            n64.bus.renderer.fog_color[2], n64.bus.renderer.fog_color[3]);
        eprintln!("  Lights: {} dir, ambient=[{},{},{}] dir0=[{},{},{}] col0=[{},{},{}]",
            n64.bus.renderer.num_dir_lights,
            n64.bus.renderer.light_colors[n64.bus.renderer.num_dir_lights as usize][0],
            n64.bus.renderer.light_colors[n64.bus.renderer.num_dir_lights as usize][1],
            n64.bus.renderer.light_colors[n64.bus.renderer.num_dir_lights as usize][2],
            n64.bus.renderer.light_dirs[0][0], n64.bus.renderer.light_dirs[0][1], n64.bus.renderer.light_dirs[0][2],
            n64.bus.renderer.light_colors[0][0], n64.bus.renderer.light_colors[0][1], n64.bus.renderer.light_colors[0][2],
        );
        let origin = n64.bus.vi.origin as usize;
        let rdram = n64.rdram_data();
        let fb_size = 320 * 240 * 2;
        if origin > 0 && origin + fb_size < rdram.len() {
            let nonzero = (0..fb_size).filter(|&i| rdram[origin + i] != 0).count();
            eprintln!("  Framebuffer: {}% non-zero at RDRAM[{:#X}]", nonzero * 100 / fb_size, origin);
        }

        // Save framebuffers as PPM screenshots for visual inspection
        let save_ppm = |base: usize, path: &str, rdram: &[u8]| {
            if base > 0 && base + fb_size < rdram.len() {
                let mut ppm_bytes = b"P6\n320 240\n255\n".to_vec();
                for y in 0..240 {
                    for x in 0..320 {
                        let off = base + (y * 320 + x) * 2;
                        let pixel = u16::from_be_bytes([rdram[off], rdram[off + 1]]);
                        let r = ((pixel >> 11) & 0x1F) as u8;
                        let g = ((pixel >> 6) & 0x1F) as u8;
                        let b = ((pixel >> 1) & 0x1F) as u8;
                        ppm_bytes.push((r << 3) | (r >> 2));
                        ppm_bytes.push((g << 3) | (g >> 2));
                        ppm_bytes.push((b << 3) | (b >> 2));
                    }
                }
                std::fs::write(path, &ppm_bytes).ok();
                let nonzero_px = (0..320*240).filter(|&i| {
                    let off = base + i * 2;
                    let pixel = u16::from_be_bytes([rdram[off], rdram[off + 1]]);
                    pixel >> 1 != 0 // ignore alpha-only pixels
                }).count();
                eprintln!("  Saved {} ({} visible pixels)", path, nonzero_px);
            }
        };
        save_ppm(origin, "framebuffer_vi.ppm", rdram);
        save_ppm(n64.bus.renderer.color_image_addr as usize, "framebuffer_render.ppm", rdram);

        n64.cpu.dump_unimpl_summary();
        return;
    } else if use_test {
        // Test mode: run until r30 is set (5M cycles max)
        let result = n64.run_until_r30(5_000_000);
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
        let event_loop = EventLoop::new().expect("create event loop");
        let mut app = App { n64, window: None, pixels: None };
        event_loop.run_app(&mut app).expect("run event loop");
    }
}
