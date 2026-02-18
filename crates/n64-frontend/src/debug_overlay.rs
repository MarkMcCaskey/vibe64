/// Debug overlay system for the N64 emulator.
///
/// Draws text, wireframe, depth buffer, and diagnostic info onto the
/// pixels RGBA8888 buffer AFTER the normal framebuffer blit, so it
/// never corrupts emulation state.

use n64_core::bus::map::Interconnect;
use n64_core::debug::DebugState;
use winit::keyboard::KeyCode;

const WIDTH: usize = 320;
const HEIGHT: usize = 240;

// ═══════════════════════════════════════════════════════════
// 8x8 Bitmap Font (ASCII 32-126, public domain VGA/CP437)
// ═══════════════════════════════════════════════════════════

const FONT_CHAR_W: usize = 6; // render width (skip 2 rightmost cols for tighter spacing)
const FONT_CHAR_H: usize = 8;
const FONT_FIRST: u8 = 32;

/// 8x8 bitmap font data. Each char = 8 bytes, one per row, MSB = left.
/// Covers ASCII 32 (' ') through 126 ('~'). 95 characters.
#[rustfmt::skip]
const FONT: [u8; 95 * 8] = [
    // 32 ' '
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    // 33 '!'
    0x18,0x18,0x18,0x18,0x18,0x00,0x18,0x00,
    // 34 '"'
    0x6C,0x6C,0x24,0x00,0x00,0x00,0x00,0x00,
    // 35 '#'
    0x6C,0xFE,0x6C,0x6C,0xFE,0x6C,0x00,0x00,
    // 36 '$'
    0x18,0x7E,0xC0,0x7C,0x06,0xFC,0x18,0x00,
    // 37 '%'
    0xC6,0xCC,0x18,0x30,0x66,0xC6,0x00,0x00,
    // 38 '&'
    0x38,0x6C,0x38,0x76,0xDC,0xCC,0x76,0x00,
    // 39 '''
    0x18,0x18,0x30,0x00,0x00,0x00,0x00,0x00,
    // 40 '('
    0x0C,0x18,0x30,0x30,0x30,0x18,0x0C,0x00,
    // 41 ')'
    0x30,0x18,0x0C,0x0C,0x0C,0x18,0x30,0x00,
    // 42 '*'
    0x00,0x66,0x3C,0xFF,0x3C,0x66,0x00,0x00,
    // 43 '+'
    0x00,0x18,0x18,0x7E,0x18,0x18,0x00,0x00,
    // 44 ','
    0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x30,
    // 45 '-'
    0x00,0x00,0x00,0x7E,0x00,0x00,0x00,0x00,
    // 46 '.'
    0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x00,
    // 47 '/'
    0x06,0x0C,0x18,0x30,0x60,0xC0,0x00,0x00,
    // 48 '0'
    0x7C,0xCE,0xDE,0xF6,0xE6,0xC6,0x7C,0x00,
    // 49 '1'
    0x18,0x38,0x18,0x18,0x18,0x18,0x7E,0x00,
    // 50 '2'
    0x7C,0xC6,0x06,0x1C,0x30,0x60,0xFE,0x00,
    // 51 '3'
    0x7C,0xC6,0x06,0x3C,0x06,0xC6,0x7C,0x00,
    // 52 '4'
    0x1C,0x3C,0x6C,0xCC,0xFE,0x0C,0x0C,0x00,
    // 53 '5'
    0xFE,0xC0,0xFC,0x06,0x06,0xC6,0x7C,0x00,
    // 54 '6'
    0x3C,0x60,0xC0,0xFC,0xC6,0xC6,0x7C,0x00,
    // 55 '7'
    0xFE,0x06,0x0C,0x18,0x30,0x30,0x30,0x00,
    // 56 '8'
    0x7C,0xC6,0xC6,0x7C,0xC6,0xC6,0x7C,0x00,
    // 57 '9'
    0x7C,0xC6,0xC6,0x7E,0x06,0x0C,0x78,0x00,
    // 58 ':'
    0x00,0x18,0x18,0x00,0x00,0x18,0x18,0x00,
    // 59 ';'
    0x00,0x18,0x18,0x00,0x00,0x18,0x18,0x30,
    // 60 '<'
    0x0C,0x18,0x30,0x60,0x30,0x18,0x0C,0x00,
    // 61 '='
    0x00,0x00,0x7E,0x00,0x7E,0x00,0x00,0x00,
    // 62 '>'
    0x60,0x30,0x18,0x0C,0x18,0x30,0x60,0x00,
    // 63 '?'
    0x7C,0xC6,0x0C,0x18,0x18,0x00,0x18,0x00,
    // 64 '@'
    0x7C,0xC6,0xDE,0xDE,0xDC,0xC0,0x7C,0x00,
    // 65 'A'
    0x38,0x6C,0xC6,0xC6,0xFE,0xC6,0xC6,0x00,
    // 66 'B'
    0xFC,0xC6,0xC6,0xFC,0xC6,0xC6,0xFC,0x00,
    // 67 'C'
    0x7C,0xC6,0xC0,0xC0,0xC0,0xC6,0x7C,0x00,
    // 68 'D'
    0xF8,0xCC,0xC6,0xC6,0xC6,0xCC,0xF8,0x00,
    // 69 'E'
    0xFE,0xC0,0xC0,0xFC,0xC0,0xC0,0xFE,0x00,
    // 70 'F'
    0xFE,0xC0,0xC0,0xFC,0xC0,0xC0,0xC0,0x00,
    // 71 'G'
    0x7C,0xC6,0xC0,0xCE,0xC6,0xC6,0x7E,0x00,
    // 72 'H'
    0xC6,0xC6,0xC6,0xFE,0xC6,0xC6,0xC6,0x00,
    // 73 'I'
    0x7E,0x18,0x18,0x18,0x18,0x18,0x7E,0x00,
    // 74 'J'
    0x06,0x06,0x06,0x06,0xC6,0xC6,0x7C,0x00,
    // 75 'K'
    0xC6,0xCC,0xD8,0xF0,0xD8,0xCC,0xC6,0x00,
    // 76 'L'
    0xC0,0xC0,0xC0,0xC0,0xC0,0xC0,0xFE,0x00,
    // 77 'M'
    0xC6,0xEE,0xFE,0xD6,0xC6,0xC6,0xC6,0x00,
    // 78 'N'
    0xC6,0xE6,0xF6,0xDE,0xCE,0xC6,0xC6,0x00,
    // 79 'O'
    0x7C,0xC6,0xC6,0xC6,0xC6,0xC6,0x7C,0x00,
    // 80 'P'
    0xFC,0xC6,0xC6,0xFC,0xC0,0xC0,0xC0,0x00,
    // 81 'Q'
    0x7C,0xC6,0xC6,0xC6,0xD6,0xDE,0x7C,0x06,
    // 82 'R'
    0xFC,0xC6,0xC6,0xFC,0xD8,0xCC,0xC6,0x00,
    // 83 'S'
    0x7C,0xC6,0xC0,0x7C,0x06,0xC6,0x7C,0x00,
    // 84 'T'
    0xFE,0x18,0x18,0x18,0x18,0x18,0x18,0x00,
    // 85 'U'
    0xC6,0xC6,0xC6,0xC6,0xC6,0xC6,0x7C,0x00,
    // 86 'V'
    0xC6,0xC6,0xC6,0x6C,0x6C,0x38,0x10,0x00,
    // 87 'W'
    0xC6,0xC6,0xC6,0xD6,0xFE,0xEE,0xC6,0x00,
    // 88 'X'
    0xC6,0x6C,0x38,0x38,0x6C,0xC6,0xC6,0x00,
    // 89 'Y'
    0xC6,0xC6,0x6C,0x38,0x18,0x18,0x18,0x00,
    // 90 'Z'
    0xFE,0x0C,0x18,0x30,0x60,0xC0,0xFE,0x00,
    // 91 '['
    0x3C,0x30,0x30,0x30,0x30,0x30,0x3C,0x00,
    // 92 '\'
    0xC0,0x60,0x30,0x18,0x0C,0x06,0x00,0x00,
    // 93 ']'
    0x3C,0x0C,0x0C,0x0C,0x0C,0x0C,0x3C,0x00,
    // 94 '^'
    0x10,0x38,0x6C,0xC6,0x00,0x00,0x00,0x00,
    // 95 '_'
    0x00,0x00,0x00,0x00,0x00,0x00,0xFE,0x00,
    // 96 '`'
    0x30,0x18,0x0C,0x00,0x00,0x00,0x00,0x00,
    // 97 'a'
    0x00,0x00,0x7C,0x06,0x7E,0xC6,0x7E,0x00,
    // 98 'b'
    0xC0,0xC0,0xFC,0xC6,0xC6,0xC6,0xFC,0x00,
    // 99 'c'
    0x00,0x00,0x7C,0xC6,0xC0,0xC6,0x7C,0x00,
    // 100 'd'
    0x06,0x06,0x7E,0xC6,0xC6,0xC6,0x7E,0x00,
    // 101 'e'
    0x00,0x00,0x7C,0xC6,0xFE,0xC0,0x7C,0x00,
    // 102 'f'
    0x1C,0x36,0x30,0x7C,0x30,0x30,0x30,0x00,
    // 103 'g'
    0x00,0x00,0x7E,0xC6,0xC6,0x7E,0x06,0x7C,
    // 104 'h'
    0xC0,0xC0,0xFC,0xC6,0xC6,0xC6,0xC6,0x00,
    // 105 'i'
    0x18,0x00,0x38,0x18,0x18,0x18,0x3C,0x00,
    // 106 'j'
    0x0C,0x00,0x1C,0x0C,0x0C,0xCC,0xCC,0x78,
    // 107 'k'
    0xC0,0xC0,0xCC,0xD8,0xF0,0xD8,0xCC,0x00,
    // 108 'l'
    0x38,0x18,0x18,0x18,0x18,0x18,0x3C,0x00,
    // 109 'm'
    0x00,0x00,0xCC,0xFE,0xD6,0xC6,0xC6,0x00,
    // 110 'n'
    0x00,0x00,0xFC,0xC6,0xC6,0xC6,0xC6,0x00,
    // 111 'o'
    0x00,0x00,0x7C,0xC6,0xC6,0xC6,0x7C,0x00,
    // 112 'p'
    0x00,0x00,0xFC,0xC6,0xC6,0xFC,0xC0,0xC0,
    // 113 'q'
    0x00,0x00,0x7E,0xC6,0xC6,0x7E,0x06,0x06,
    // 114 'r'
    0x00,0x00,0xDC,0xE6,0xC0,0xC0,0xC0,0x00,
    // 115 's'
    0x00,0x00,0x7E,0xC0,0x7C,0x06,0xFC,0x00,
    // 116 't'
    0x30,0x30,0x7C,0x30,0x30,0x36,0x1C,0x00,
    // 117 'u'
    0x00,0x00,0xC6,0xC6,0xC6,0xC6,0x7E,0x00,
    // 118 'v'
    0x00,0x00,0xC6,0xC6,0x6C,0x38,0x10,0x00,
    // 119 'w'
    0x00,0x00,0xC6,0xC6,0xD6,0xFE,0x6C,0x00,
    // 120 'x'
    0x00,0x00,0xC6,0x6C,0x38,0x6C,0xC6,0x00,
    // 121 'y'
    0x00,0x00,0xC6,0xC6,0xC6,0x7E,0x06,0x7C,
    // 122 'z'
    0x00,0x00,0xFE,0x0C,0x38,0x60,0xFE,0x00,
    // 123 '{'
    0x0E,0x18,0x18,0x70,0x18,0x18,0x0E,0x00,
    // 124 '|'
    0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x00,
    // 125 '}'
    0x70,0x18,0x18,0x0E,0x18,0x18,0x70,0x00,
    // 126 '~'
    0x76,0xDC,0x00,0x00,0x00,0x00,0x00,0x00,
];

// ═══════════════════════════════════════════════════════════
// Drawing Primitives
// ═══════════════════════════════════════════════════════════

fn set_pixel(buf: &mut [u8], x: usize, y: usize, r: u8, g: u8, b: u8) {
    if x < WIDTH && y < HEIGHT {
        let off = (y * WIDTH + x) * 4;
        buf[off] = r;
        buf[off + 1] = g;
        buf[off + 2] = b;
        buf[off + 3] = 0xFF;
    }
}

fn draw_char(buf: &mut [u8], x: usize, y: usize, ch: u8, r: u8, g: u8, b: u8) {
    if ch < FONT_FIRST || ch > 126 { return; }
    let idx = (ch - FONT_FIRST) as usize;
    let glyph = &FONT[idx * 8..(idx + 1) * 8];
    for row in 0..FONT_CHAR_H {
        let py = y + row;
        if py >= HEIGHT { break; }
        let bits = glyph[row];
        for col in 0..8usize {
            if bits & (0x80 >> col) != 0 {
                let px = x + col;
                if px < WIDTH {
                    set_pixel(buf, px, py, r, g, b);
                }
            }
        }
    }
}

fn draw_text(buf: &mut [u8], x: usize, y: usize, text: &str, r: u8, g: u8, b: u8) {
    let mut cx = x;
    for &ch in text.as_bytes() {
        draw_char(buf, cx, y, ch, r, g, b);
        cx += FONT_CHAR_W;
    }
}

/// Draw a dark semi-transparent rectangle as background for text.
fn draw_rect_bg(buf: &mut [u8], x: usize, y: usize, w: usize, h: usize) {
    for dy in 0..h {
        let py = y + dy;
        if py >= HEIGHT { break; }
        for dx in 0..w {
            let px = x + dx;
            if px >= WIDTH { break; }
            let off = (py * WIDTH + px) * 4;
            // Darken to ~25% brightness
            buf[off] = buf[off] >> 2;
            buf[off + 1] = buf[off + 1] >> 2;
            buf[off + 2] = buf[off + 2] >> 2;
        }
    }
}

/// Bresenham line drawing.
fn draw_line(buf: &mut [u8], x0: i32, y0: i32, x1: i32, y1: i32, r: u8, g: u8, b: u8) {
    let mut x = x0;
    let mut y = y0;
    let dx = (x1 - x0).abs();
    let dy = -(y1 - y0).abs();
    let sx = if x0 < x1 { 1 } else { -1 };
    let sy = if y0 < y1 { 1 } else { -1 };
    let mut err = dx + dy;

    for _ in 0..10000 {
        if x >= 0 && x < WIDTH as i32 && y >= 0 && y < HEIGHT as i32 {
            set_pixel(buf, x as usize, y as usize, r, g, b);
        }
        if x == x1 && y == y1 { break; }
        let e2 = 2 * err;
        if e2 >= dy { err += dy; x += sx; }
        if e2 <= dx { err += dx; y += sy; }
    }
}

// ═══════════════════════════════════════════════════════════
// Overlay Renderers
// ═══════════════════════════════════════════════════════════

/// F1: Stats HUD
fn draw_stats_hud(buf: &mut [u8], debug: &DebugState, bus: &Interconnect) {
    let stats = &debug.prev_stats;
    let fps = {
        let avg = debug.fps_samples.iter().sum::<f64>() / 60.0;
        if avg > 0.001 { 1000.0 / avg } else { 0.0 }
    };

    let lines: [String; 8] = [
        format!("FPS: {:.1}", fps),
        format!("Tris: {}", stats.tri_count),
        format!("Vtx:  {}", stats.vtx_count),
        format!("Fill: {}  Tex: {}", stats.fill_rect_count, stats.tex_rect_count),
        format!("DL cmds: {}", stats.dl_cmd_count),
        format!("RSP: gfx={} aud={}", stats.rsp_gfx_tasks, stats.rsp_audio_tasks),
        format!("VI: {}x{} origin={:#X}", bus.vi.width, 240, bus.vi.origin),
        format!("Frame: {}", debug.frame_count),
    ];

    let max_w = lines.iter().map(|l| l.len() * FONT_CHAR_W + 8).max().unwrap_or(100);
    draw_rect_bg(buf, 2, 2, max_w, lines.len() * 9 + 4);

    for (i, line) in lines.iter().enumerate() {
        draw_text(buf, 4, 4 + i * 9, line, 0xFF, 0xFF, 0x00);
    }
}

/// F2: Wireframe overlay
fn draw_wireframe(buf: &mut [u8], debug: &DebugState) {
    for edge in &debug.wire_edges {
        draw_line(buf, edge.x0 as i32, edge.y0 as i32,
                  edge.x1 as i32, edge.y1 as i32, 0x00, 0xFF, 0x00);
    }
}

/// F3: Depth buffer visualization
fn draw_depth_viz(buf: &mut [u8], bus: &Interconnect) {
    let z_addr = bus.renderer.z_image_addr as usize;
    if z_addr == 0 { return; }

    let rdram = bus.rdram.data();
    let width = (bus.renderer.color_image_width as usize).min(WIDTH);

    for y in 0..HEIGHT {
        for x in 0..width {
            let offset = z_addr + (y * width + x) * 2;
            if offset + 1 >= rdram.len() { continue; }
            let z = u16::from_be_bytes([rdram[offset], rdram[offset + 1]]);
            let gray = (255u16).saturating_sub(z >> 7).min(255) as u8;
            set_pixel(buf, x, y, gray, gray, gray);
        }
    }
}

/// F4: Texture / tile info
fn draw_texture_info(buf: &mut [u8], bus: &Interconnect) {
    let r = &bus.renderer;
    let mut lines: Vec<String> = vec!["=== Tiles ===".to_string()];

    for i in 0..8 {
        let t = &r.tiles[i];
        if t.line == 0 && t.tmem == 0 { continue; }
        let fmt = match t.format {
            0 => "RGBA", 1 => "YUV", 2 => "CI", 3 => "IA", 4 => "I", _ => "?"
        };
        let bpp = match t.size { 0 => "4b", 1 => "8b", 2 => "16b", 3 => "32b", _ => "?" };
        lines.push(format!("T{}: {} {} tmem={} pal={} {}x{}",
            i, fmt, bpp, t.tmem, t.palette,
            (t.sh >> 2).wrapping_sub(t.sl >> 2) + 1,
            (t.th >> 2).wrapping_sub(t.tl >> 2) + 1));
    }

    let max_w = lines.iter().map(|l| l.len() * FONT_CHAR_W + 8).max().unwrap_or(100);
    let y_start = HEIGHT - lines.len() * 9 - 6;
    draw_rect_bg(buf, 2, y_start, max_w, lines.len() * 9 + 4);

    for (i, line) in lines.iter().enumerate() {
        let color = if i == 0 { (0xFF, 0xFF, 0xFF) } else { (0x80, 0xFF, 0x80) };
        draw_text(buf, 4, y_start + 2 + i * 9, line, color.0, color.1, color.2);
    }
}

/// F5: Geometry inspector
fn draw_geometry_inspector(buf: &mut [u8], bus: &Interconnect) {
    let r = &bus.renderer;
    let mut lines: Vec<String> = vec!["=== Geometry ===".to_string()];

    lines.push(format!("VP scl:({:.0},{:.0},{:.0})", r.viewport_scale[0], r.viewport_scale[1], r.viewport_scale[2]));
    lines.push(format!("VP trn:({:.0},{:.0},{:.0})", r.viewport_trans[0], r.viewport_trans[1], r.viewport_trans[2]));
    lines.push(format!("Geo: {:#010X}", r.geometry_mode));

    // Decode key geometry mode flags
    let mut flags = Vec::new();
    if r.geometry_mode & 0x0001 != 0 { flags.push("ZBUF"); }
    if r.geometry_mode & 0x0200 != 0 { flags.push("CULL_F"); }
    if r.geometry_mode & 0x0400 != 0 { flags.push("CULL_B"); }
    if r.geometry_mode & 0x10000 != 0 { flags.push("FOG"); }
    if r.geometry_mode & 0x20000 != 0 { flags.push("LIGHT"); }
    if !flags.is_empty() {
        lines.push(format!("  {}", flags.join(" ")));
    }

    lines.push(format!("OtherH:{:#010X}", r.othermode_h));
    lines.push(format!("OtherL:{:#010X}", r.othermode_l));
    lines.push(format!("Scissor:({},{})..({},{})",
        r.scissor_ulx >> 2, r.scissor_uly >> 2, r.scissor_lrx >> 2, r.scissor_lry >> 2));
    lines.push(format!("CImg:{:#X} {}px Z:{:#X}",
        r.color_image_addr, r.color_image_width, r.z_image_addr));

    // MVP row 0
    lines.push(format!("MVP[0]: {:.2} {:.2} {:.2} {:.2}",
        r.mvp[0][0], r.mvp[0][1], r.mvp[0][2], r.mvp[0][3]));

    // Show first few active vertices
    let mut shown = 0;
    for i in 0..32 {
        if shown >= 3 { break; }
        let v = &r.vertex_buffer[i];
        if v.w.abs() > 0.001 {
            lines.push(format!("V{:2}:({:.0},{:.0},{:.0}) #{:02X}{:02X}{:02X}",
                i, v.x, v.y, v.z, v.r, v.g, v.b));
            shown += 1;
        }
    }

    let max_w = lines.iter().map(|l| l.len() * FONT_CHAR_W + 8).max().unwrap_or(200);
    let y_start = 100;
    draw_rect_bg(buf, 2, y_start, max_w, lines.len() * 9 + 4);

    for (i, line) in lines.iter().enumerate() {
        let color = if i == 0 { (0xFF, 0xFF, 0xFF) } else { (0x80, 0xFF, 0x80) };
        draw_text(buf, 4, y_start + 2 + i * 9, line, color.0, color.1, color.2);
    }
}

/// F6: Display list log
fn draw_dl_log(buf: &mut [u8], debug: &DebugState) {
    let max_lines = 22;
    let x = 164;
    let y_start = 2;

    draw_rect_bg(buf, 162, 0, 158, max_lines * 9 + 4);
    draw_text(buf, x, y_start, "=== DL Log ===", 0xFF, 0xFF, 0xFF);

    let mut y = y_start + 9;
    for entry in debug.dl_log.iter_recent(max_lines - 1) {
        let line = format!("{:02X} {}", (entry.w0 >> 24) as u8, entry.opcode_name);
        draw_text(buf, x, y, &line, 0xCC, 0xCC, 0xFF);
        y += 9;
    }
}

/// F7: OS/scheduler monitor
fn draw_os_monitor(buf: &mut [u8], debug: &DebugState, bus: &Interconnect) {
    let mi = &bus.mi;
    let rsp = &bus.rsp;
    let vi = &bus.vi;

    let mut lines: Vec<String> = vec!["=== OS Monitor ===".to_string()];

    // Interrupt state
    let names = ["SP", "SI", "AI", "VI", "PI", "DP"];
    let active: Vec<&str> = (0..6u8)
        .filter(|&i| mi.intr & (1 << i) != 0)
        .map(|i| names[i as usize])
        .collect();
    let active_str = if active.is_empty() { "none".to_string() } else { active.join(" ") };
    lines.push(format!("MI: {} mask={:06b}", active_str, mi.intr_mask));

    lines.push(format!("RSP: {} tasks  st={:#06X}", rsp.start_count, rsp.status));
    lines.push(format!("VI: {}/{} origin={:#X}", vi.v_current, vi.v_sync, vi.origin));

    // Thread list
    lines.push("--- Threads ---".to_string());
    for t in &debug.cached_threads {
        let s = match t.state {
            1 => "STOP", 2 => "RUN ", 4 => "CURR", 8 => "WAIT", _ => "??? "
        };
        lines.push(format!("T{}:p{:3} {} {:#010X}", t.id, t.priority, s, t.saved_pc));
    }

    let max_w = lines.iter().map(|l| l.len() * FONT_CHAR_W + 8).max().unwrap_or(200);
    draw_rect_bg(buf, 2, 0, max_w, lines.len() * 9 + 4);

    for (i, line) in lines.iter().enumerate() {
        let color = match i {
            0 => (0xFF, 0xFF, 0xFF),
            4 => (0xCC, 0xCC, 0xCC),
            _ => (0xFF, 0xCC, 0x00),
        };
        draw_text(buf, 4, 2 + i * 9, line, color.0, color.1, color.2);
    }
}

/// Scan RDRAM for OS thread structures (same heuristic as --diag mode).
pub fn scan_os_threads(bus: &Interconnect) -> Vec<n64_core::debug::ThreadInfo> {
    let rdram = bus.rdram.data();
    let read_u32 = |off: usize| -> u32 {
        if off + 3 < rdram.len() {
            u32::from_be_bytes([rdram[off], rdram[off+1], rdram[off+2], rdram[off+3]])
        } else { 0 }
    };

    let mut best_chain: Vec<n64_core::debug::ThreadInfo> = Vec::new();

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
            let saved_pc = read_u32(p + 0x11C);
            if pri > 255 { break; }
            chain.push(n64_core::debug::ThreadInfo {
                vaddr: cur, priority: pri, state, id, saved_pc,
            });
            let next = read_u32(p + 0x0C);
            if next == 0 || next < 0x8000_0000 || next >= 0x8080_0000 || next == cur { break; }
            cur = next;
        }
        if chain.len() > best_chain.len() {
            best_chain = chain;
        }
    }
    best_chain
}

// ═══════════════════════════════════════════════════════════
// Public API
// ═══════════════════════════════════════════════════════════

/// Main entry: draw all active debug overlays onto the pixel buffer.
pub fn draw_overlays(buf: &mut [u8], debug: &mut DebugState, bus: &Interconnect) {
    let flags = &debug.flags;
    let any_active = flags.show_stats || flags.show_wireframe || flags.show_depth
        || flags.show_textures || flags.show_geometry || flags.show_dl_log
        || flags.show_os_monitor;
    if !any_active { return; }

    // Rate-limited OS thread scan
    if flags.show_os_monitor {
        if debug.os_scan_countdown == 0 {
            debug.cached_threads = scan_os_threads(bus);
            debug.os_scan_countdown = 60; // rescan every ~1 second
        } else {
            debug.os_scan_countdown -= 1;
        }
    }

    // Draw order: depth replaces FB, then wireframe, then panels on top
    if flags.show_depth { draw_depth_viz(buf, bus); }
    if flags.show_wireframe { draw_wireframe(buf, debug); }
    if flags.show_os_monitor { draw_os_monitor(buf, debug, bus); }
    if flags.show_geometry { draw_geometry_inspector(buf, bus); }
    if flags.show_textures { draw_texture_info(buf, bus); }
    if flags.show_dl_log { draw_dl_log(buf, debug); }
    if flags.show_stats { draw_stats_hud(buf, debug, bus); }
}

/// Handle an F-key press to toggle debug modes.
pub fn handle_f_key(debug: &mut DebugState, key: KeyCode) {
    match key {
        KeyCode::F1 => debug.flags.show_stats = !debug.flags.show_stats,
        KeyCode::F2 => debug.flags.show_wireframe = !debug.flags.show_wireframe,
        KeyCode::F3 => debug.flags.show_depth = !debug.flags.show_depth,
        KeyCode::F4 => debug.flags.show_textures = !debug.flags.show_textures,
        KeyCode::F5 => debug.flags.show_geometry = !debug.flags.show_geometry,
        KeyCode::F6 => debug.flags.show_dl_log = !debug.flags.show_dl_log,
        KeyCode::F7 => debug.flags.show_os_monitor = !debug.flags.show_os_monitor,
        KeyCode::F8 => debug.flags.freeze_frame = !debug.flags.freeze_frame,
        _ => {}
    }
}
