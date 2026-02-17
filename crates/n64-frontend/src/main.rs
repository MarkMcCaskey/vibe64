mod tui;

use std::path::PathBuf;

use pixels::{Pixels, SurfaceTexture};
use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::WindowEvent;
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowId};

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
        let total_steps = 50_000_000u64;
        eprintln!("=== Boot diagnostic ({}M steps) ===", total_steps / 1_000_000);

        for _ in 0..total_steps {
            n64.step_one();
        }

        eprintln!("  DMAs={} RSP_tasks={} PC={:#010X}",
            n64.bus.pi.dma_count, n64.bus.rsp.start_count, n64.cpu.pc as u32);
        eprintln!("  VI: ctrl={:#010X} origin={:#010X} width={} h_video={:#010X}",
            n64.bus.vi.ctrl, n64.bus.vi.origin, n64.bus.vi.width, n64.bus.vi.h_video);

        let origin = n64.bus.vi.origin as usize;
        let rdram = n64.rdram_data();
        let fb_size = 320 * 240 * 2;
        if origin > 0 && origin + fb_size < rdram.len() {
            let nonzero = (0..fb_size).filter(|&i| rdram[origin + i] != 0).count();
            eprintln!("  Framebuffer: {}% non-zero at RDRAM[{:#X}]", nonzero * 100 / fb_size, origin);
        }

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
