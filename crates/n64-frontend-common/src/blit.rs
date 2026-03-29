pub const N64_WIDTH: u32 = 320;
pub const N64_HEIGHT: u32 = 240;

/// Compute source X from display X using VI x_scale.
///
/// VI x_scale bits [11:0] = source pixels per TV pixel (u2.10 fixed-point).
/// Standard 320×240: x_add=0x200 (0.5), mapping 320 source → 640 TV pixels.
/// Our display is 320 wide (half the 640 TV width), so we multiply by 2.
#[inline(always)]
fn vi_scale_x(display_x: usize, x_add: usize) -> usize {
    let add = if x_add == 0 { 0x200 } else { x_add };
    (display_x * 2 * add) >> 10
}

/// Compute source Y from display Y using VI y_scale.
///
/// VI y_scale bits [11:0] = source lines per TV line (u2.10 fixed-point).
/// Non-interlaced (most N64 games): y_add=0x400 (1.0), 240 source → 240 TV.
/// Our display is 240 lines (matching non-interlaced TV), so factor is 1.
#[inline(always)]
fn vi_scale_y(display_y: usize, y_add: usize) -> usize {
    let add = if y_add == 0 { 0x400 } else { y_add };
    (display_y * add) >> 10
}

/// Read N64 framebuffer from RDRAM and convert to RGBA8888 for display.
///
/// Applies VI x_scale/y_scale to support games that render at non-standard
/// resolutions (e.g., Quake II renders at 160×120 with 2× VI upscaling).
pub fn blit_framebuffer(n64: &n64_core::N64, dest: &mut [u8]) {
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

    // VI scale factors: bits [11:0] are the u2.10 source-pixels-per-TV-pixel
    let x_add = (n64.vi_x_scale() & 0xFFF) as usize;
    let y_add = (n64.vi_y_scale() & 0xFFF) as usize;

    for y in 0..height {
        let src_y = vi_scale_y(y, y_add);
        for x in 0..fb_width {
            let src_x = vi_scale_x(x, x_add);
            let dest_idx = (y * fb_width + x) * 4;
            if dest_idx + 3 >= dest.len() {
                break;
            }

            match format {
                2 => {
                    let src_offset = origin + (src_y * width + src_x) * 2;
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
                    let src_offset = origin + (src_y * width + src_x) * 4;
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

/// Save the current N64 framebuffer as a PPM screenshot.
pub fn save_screenshot(n64: &n64_core::N64) {
    let vi_origin = n64.vi_origin() as usize;
    let width = n64.vi_width().max(1) as usize;
    let format = n64.vi_pixel_format();
    let rdram = n64.rdram_data();
    let x_add = (n64.vi_x_scale() & 0xFFF) as usize;
    let y_add = (n64.vi_y_scale() & 0xFFF) as usize;

    eprintln!(
        "Screenshot: vi_origin={:#X} width={} format={} x_scale={:#X} y_scale={:#X}",
        vi_origin, width, format, n64.vi_x_scale(), n64.vi_y_scale()
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
    for y in 0..240usize {
        let src_y = vi_scale_y(y, y_add);
        for x in 0..320usize {
            let src_x = vi_scale_x(x, x_add);
            if use_snapshot {
                let off = (src_y * width + src_x) * 2;
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
                        let off = vi_origin + (src_y * width + src_x) * 2;
                        if off + 1 >= rdram.len() {
                            ppm.extend_from_slice(&[0, 0, 0]);
                            continue;
                        }
                        let pixel = u16::from_be_bytes([rdram[off], rdram[off + 1]]);
                        let r = ((pixel >> 11) & 0x1F) as u8;
                        let g = ((pixel >> 6) & 0x1F) as u8;
                        let b = ((pixel >> 1) & 0x1F) as u8;
                        ppm.push((r << 3) | (r >> 2));
                        ppm.push((g << 3) | (g >> 2));
                        ppm.push((b << 3) | (b >> 2));
                    }
                    3 => {
                        let off = vi_origin + (src_y * width + src_x) * 4;
                        if off + 3 >= rdram.len() {
                            ppm.extend_from_slice(&[0, 0, 0]);
                            continue;
                        }
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
