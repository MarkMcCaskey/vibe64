/// GBI — Graphics Binary Interface.
///
/// Defines display list command formats used by N64 games. Each command
/// is 64 bits (two 32-bit words). The upper byte of the first word
/// identifies the command.
///
/// Two major variants exist:
/// - **F3D/F3DEX**: Original microcode (Super Mario 64, Mario Kart 64, etc.)
/// - **F3DEX2**: Later revision with reorganized opcodes (Zelda OoT, etc.)
///
/// In HLE mode, we interpret display list commands directly.

/// Microcode variant — determines GBI opcode layout.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UcodeType {
    F3dex2,
    F3d,
}

/// Detect microcode variant from ROM game code.
pub fn detect_ucode(game_code: &[u8; 4]) -> UcodeType {
    let code = std::str::from_utf8(game_code).unwrap_or("");
    // F3D games (original microcode)
    const F3D_GAMES: &[&str] = &[
        "NSME", "NSMJ", "NSMP",  // Super Mario 64
        "NMKE", "NMKJ", "NMKP",  // Mario Kart 64
        "NSSE", "NSSJ", "NSSP",  // Star Fox 64
        "NWRE", "NWRJ", "NWRP",  // Wave Race 64
        "NPWE", "NPWJ", "NPWP",  // Pilotwings 64
    ];
    for &c in F3D_GAMES {
        if code == c { return UcodeType::F3d; }
    }
    UcodeType::F3dex2
}

// ─── RDP commands (passed through by F3DEX2 to the RDP) ───

pub const G_SETCIMG: u8      = 0xFF; // Set Color Image (framebuffer)
pub const G_SETZIMG: u8      = 0xFE; // Set Z Image (depth buffer)
pub const G_SETTIMG: u8      = 0xFD; // Set Texture Image source
pub const G_SETCOMBINE: u8   = 0xFC; // Set Color Combiner mode
pub const G_SETENVCOLOR: u8  = 0xFB; // Set Environment Color
pub const G_SETPRIMCOLOR: u8 = 0xFA; // Set Primitive Color
pub const G_SETBLENDCOLOR: u8 = 0xF9; // Set Blend Color
pub const G_SETFOGCOLOR: u8  = 0xF8; // Set Fog Color
pub const G_SETFILLCOLOR: u8 = 0xF7; // Set Fill Color
pub const G_FILLRECT: u8     = 0xF6; // Fill Rectangle
pub const G_SETTILE: u8      = 0xF5; // Set Tile Descriptor
pub const G_LOADTILE: u8     = 0xF4; // Load Tile from DRAM → TMEM
pub const G_LOADBLOCK: u8    = 0xF3; // Load Block from DRAM → TMEM
pub const G_SETTILESIZE: u8  = 0xF2; // Set Tile Size
pub const G_LOADTLUT: u8     = 0xF0; // Load Texture Lookup Table
pub const G_SETSCISSOR: u8   = 0xED; // Set Scissor Rectangle
pub const G_SETCONVERT: u8   = 0xEC; // Set Color Convert coefficients
pub const G_SETKEYR: u8      = 0xEB; // Set Chroma Key Red
pub const G_SETKEYGB: u8     = 0xEA; // Set Chroma Key Green/Blue
pub const G_RDPFULLSYNC: u8  = 0xE9; // Full Sync (wait for RDP idle)
pub const G_RDPTILESYNC: u8  = 0xE8; // Tile Sync
pub const G_RDPPIPESYNC: u8  = 0xE7; // Pipe Sync
pub const G_RDPLOADSYNC: u8  = 0xE6; // Load Sync
pub const G_TEXRECT: u8      = 0xE4; // Texture Rectangle
pub const G_TEXRECTFLIP: u8  = 0xE5; // Texture Rectangle (S/T flipped)
pub const G_SETOTHERMODE_H: u8 = 0xE3; // Set Other Modes (high half)
pub const G_SETOTHERMODE_L: u8 = 0xE2; // Set Other Modes (low half)
pub const G_RDPSETOTHERMODE: u8 = 0xEF; // Set Both Other Mode halves at once

// ─── RSP geometry commands (F3DEX2-specific opcodes) ───

pub const G_NOOP: u8         = 0x00; // No Operation
pub const G_VTX: u8          = 0x01; // Load Vertices
pub const G_MODIFYVTX: u8    = 0x02; // Modify Vertex
pub const G_TRI1: u8         = 0x05; // Draw 1 Triangle
pub const G_TRI2: u8         = 0x06; // Draw 2 Triangles
pub const G_QUAD: u8         = 0x07; // Draw Quadrilateral
pub const G_POPMTX: u8       = 0xD8; // Pop Matrix Stack
pub const G_GEOMETRYMODE: u8 = 0xD9; // Set/Clear Geometry Mode flags
pub const G_MTX: u8          = 0xDA; // Load Matrix
pub const G_MOVEWORD: u8     = 0xDB; // Move Word to RSP DMEM
pub const G_MOVEMEM: u8      = 0xDC; // Move Memory block
pub const G_TEXTURE: u8      = 0xD7; // Set Texture parameters
pub const G_DL: u8           = 0xDE; // Branch/Call Display List
pub const G_ENDDL: u8        = 0xDF; // End Display List
pub const G_SETGEOMETRYMODE: u8 = 0xD9; // alias
pub const G_SPECIAL_1: u8    = 0xD5; // Special (branch on Z)
pub const G_RDPHALF_1: u8    = 0xE1; // RDP half word 1 (used by texture rect)
pub const G_RDPHALF_2: u8    = 0xF1; // RDP half word 2 (used by texture rect)

// ─── Cycle type constants (from othermode_H bits 52-53) ───

pub const CYCLE_1: u8    = 0;
pub const CYCLE_2: u8    = 1;
pub const CYCLE_COPY: u8 = 2;
pub const CYCLE_FILL: u8 = 3;

// ─── OSTask type constants ───

pub const M_GFXTASK: u32 = 1;
pub const M_AUDTASK: u32 = 2;

// ─── OSTask field offsets within DMEM ───
// osSpTaskLoad DMAs the 64-byte OSTask header to the last 64 bytes
// of DMEM (0xFC0..0xFFF). The RSP microcode reads it from there.

pub const TASK_TYPE: u32       = 0xFC0;
pub const TASK_FLAGS: u32      = 0xFC4;
pub const TASK_UCODE_BOOT: u32 = 0xFC8;
pub const TASK_UCODE_SIZE_BOOT: u32 = 0xFCC;
pub const TASK_UCODE: u32     = 0xFD0;
pub const TASK_UCODE_SIZE: u32 = 0xFD4;
pub const TASK_UCODE_DATA: u32 = 0xFD8;
pub const TASK_UCODE_DATA_SIZE: u32 = 0xFDC;
pub const TASK_DRAM_STACK: u32 = 0xFE0;
pub const TASK_DRAM_STACK_SIZE: u32 = 0xFE4;
pub const TASK_OUTPUT_BUFF: u32 = 0xFE8;
pub const TASK_OUTPUT_BUFF_SIZE: u32 = 0xFEC;
pub const TASK_DATA_PTR: u32   = 0xFF0;
pub const TASK_DATA_SIZE: u32  = 0xFF4;
pub const TASK_YIELD_DATA_PTR: u32 = 0xFF8;
pub const TASK_YIELD_DATA_SIZE: u32 = 0xFFC;

/// Maximum display list call depth (F3DEX2 supports 10 levels).
const MAX_DL_STACK: usize = 18;

/// Maximum commands to process per display list (safety limit).
/// OoT draws 3D world geometry before overlay/text, requiring >100K commands.
const MAX_COMMANDS: usize = 1_000_000;

use super::renderer::Renderer;

/// Walk a display list starting at the given physical RDRAM address.
/// Dispatches each GBI command to the renderer.
pub fn process_display_list(renderer: &mut Renderer, rdram: &mut [u8], addr: u32) {
    let mut pc = addr & 0x00FF_FFFF; // mask to RDRAM range
    let mut stack: Vec<u32> = Vec::with_capacity(MAX_DL_STACK);
    let mut cmd_count = 0usize;
    let mut opcode_counts = [0u32; 256];

    loop {
        if cmd_count >= MAX_COMMANDS {
            log::warn!("Display list exceeded {} commands, stopping", MAX_COMMANDS);
            break;
        }
        cmd_count += 1;

        // Read 64-bit command (two big-endian 32-bit words)
        let w0 = read_u32(rdram, pc);
        let w1 = read_u32(rdram, pc.wrapping_add(4));
        pc = pc.wrapping_add(8);

        let cmd = (w0 >> 24) as u8;
        opcode_counts[cmd as usize] += 1;

        // DL command logging for debug overlay
        if renderer.debug_dl_log && renderer.dl_log_entries.len() < 4096 {
            renderer.dl_log_entries.push(crate::debug::DlLogEntry {
                pc: pc.wrapping_sub(8),
                w0,
                w1,
                opcode_name: opcode_name_f3dex2(cmd),
            });
        }

        match cmd {
            G_NOOP => {}

            G_ENDDL => {
                if let Some(return_addr) = stack.pop() {
                    pc = return_addr;
                } else {
                    break; // top-level DL ended
                }
            }

            G_DL => {
                let addr = renderer.resolve_segment(w1);
                let push = (w0 >> 16) & 0xFF;
                if push == 0 {
                    // Call (push return address)
                    stack.push(pc);
                }
                // else: branch (don't push, just jump)
                pc = addr;
            }

            // ─── RDP state commands ───
            G_SETCIMG => renderer.cmd_set_color_image(w0, w1),
            G_SETZIMG => renderer.cmd_set_z_image(w0, w1),
            G_SETTIMG => renderer.cmd_set_texture_image(w0, w1),
            G_SETFILLCOLOR => renderer.cmd_set_fill_color(w1),
            G_SETENVCOLOR => renderer.cmd_set_env_color(w1),
            G_SETPRIMCOLOR => renderer.cmd_set_prim_color(w0, w1),
            G_SETBLENDCOLOR => renderer.cmd_set_blend_color(w1),
            G_SETFOGCOLOR => renderer.cmd_set_fog_color(w1),
            G_SETCOMBINE => renderer.cmd_set_combine(w0, w1),
            G_SETOTHERMODE_H => renderer.cmd_set_other_mode_h(w0, w1),
            G_SETOTHERMODE_L => renderer.cmd_set_other_mode_l(w0, w1),
            G_RDPSETOTHERMODE => renderer.cmd_rdp_set_other_mode(w0, w1),
            G_SETSCISSOR => renderer.cmd_set_scissor(w0, w1),

            // ─── Tile commands ───
            G_SETTILE => renderer.cmd_set_tile(w0, w1),
            G_SETTILESIZE => renderer.cmd_set_tile_size(w0, w1),
            G_LOADBLOCK => renderer.cmd_load_block(w0, w1, rdram),
            G_LOADTILE => renderer.cmd_load_tile(w0, w1, rdram),
            G_LOADTLUT => renderer.cmd_load_tlut(w0, w1, rdram),

            // ─── Drawing commands ───
            G_FILLRECT => renderer.cmd_fill_rect(w0, w1, rdram),

            G_TEXRECT | G_TEXRECTFLIP => {
                // Texture rect uses 3 words total (128 bits):
                // Word 0-1: rectangle coords
                // Next command word pair: S/T coords and step
                // In F3DEX2, the extra data comes as G_RDPHALF_1 + G_RDPHALF_2
                // but some games embed it directly. Read next two words.
                let w2 = read_u32(rdram, pc);
                let w3 = read_u32(rdram, pc.wrapping_add(4));
                pc = pc.wrapping_add(8);
                // Sometimes preceded by RDPHALF commands, skip their headers
                let (extra0, extra1) = if (w2 >> 24) as u8 == G_RDPHALF_1 {
                    let h1 = w3;
                    let _w4 = read_u32(rdram, pc);
                    let w5 = read_u32(rdram, pc.wrapping_add(4));
                    pc = pc.wrapping_add(8);
                    (h1, w5)
                } else {
                    (w2, w3)
                };
                renderer.cmd_texture_rect(w0, w1, extra0, extra1, rdram, cmd == G_TEXRECTFLIP);
            }

            // ─── Geometry commands (RSP-level, need vertex buffer) ───
            G_VTX => renderer.cmd_vertex(w0, w1, rdram),
            G_TRI1 => renderer.cmd_tri1(w0, w1, rdram),
            G_TRI2 => renderer.cmd_tri2(w0, w1, rdram),
            G_MTX => renderer.cmd_load_matrix(w0, w1, rdram),
            G_POPMTX => renderer.cmd_pop_matrix(w0, w1),
            G_GEOMETRYMODE => renderer.cmd_geometry_mode(w0, w1),
            G_TEXTURE => renderer.cmd_texture(w0, w1),

            // ─── Sync commands (no-ops for software renderer) ───
            G_RDPFULLSYNC | G_RDPTILESYNC | G_RDPPIPESYNC | G_RDPLOADSYNC => {}

            // ─── RSP memory/word commands ───
            G_MOVEMEM => {
                let index = w0 & 0xFF;
                let offset = ((w0 >> 8) & 0xFF) as usize * 8;
                match index {
                    0x08 => renderer.cmd_set_viewport(w1, rdram),
                    0x0A => renderer.cmd_set_light(offset, w1, rdram),
                    _ => {}
                }
            }
            G_MOVEWORD => {
                let index = (w0 >> 16) & 0xFF;
                let offset = w0 & 0xFFFF;
                match index {
                    0x02 => { // G_MW_NUMLIGHT
                        renderer.num_dir_lights = (w1 / 24) as u8;
                    }
                    0x06 => { // G_MW_SEGMENT
                        let seg = (offset / 4) as usize;
                        if seg < 16 {
                            renderer.segment_table[seg] = w1 & 0x00FF_FFFF;
                        }
                    }
                    0x08 => { // G_MW_FOG
                        renderer.cmd_set_fog(w1);
                    }
                    0x0A => { // G_MW_LIGHTCOL: set light color via moveword
                        let light_idx = (offset as usize) / 24;
                        let sub = (offset as usize) % 24;
                        if light_idx < 8 && (sub == 0 || sub == 4) {
                            renderer.light_colors[light_idx] = [
                                (w1 >> 24) as u8,
                                (w1 >> 16) as u8,
                                (w1 >> 8) as u8,
                            ];
                        }
                    }
                    0x0E => {} // G_MW_PERSPNORM: perspective normalization (ignored)
                    _ => {}
                }
            }
            G_QUAD => renderer.cmd_tri2(w0, w1, rdram),
            G_SETCONVERT | G_SETKEYR | G_SETKEYGB
            | G_SPECIAL_1 | G_MODIFYVTX => {}

            G_RDPHALF_1 | G_RDPHALF_2 => {
                // These store half-words for the next texture rect command.
                // Already handled inline by G_TEXRECT above when they appear
                // immediately before it. If standalone, just record the value.
                renderer.rdp_half[if cmd == G_RDPHALF_1 { 0 } else { 1 }] = w1;
            }

            _ => {
                log::trace!("Unknown GBI command {:#04X} at DL addr {:#010X}", cmd, pc.wrapping_sub(8));
            }
        }
    }

}

/// Translate F3D geometry mode bits to F3DEX2 positions.
///
/// Three bits have different positions between F3D and F3DEX2:
///   G_CULL_FRONT:     F3D 0x1000  → F3DEX2 0x0200
///   G_CULL_BACK:      F3D 0x2000  → F3DEX2 0x0400
///   G_SHADING_SMOOTH: F3D 0x0200  → F3DEX2 0x200000
/// All other bits (ZBUFFER, SHADE, LIGHTING, FOG, etc.) are identical.
fn translate_f3d_geom(bits: u32) -> u32 {
    let mut out = bits & !(0x1000 | 0x2000 | 0x0200); // clear F3D-specific bits
    if bits & 0x1000 != 0 { out |= 0x0200; }      // cull front
    if bits & 0x2000 != 0 { out |= 0x0400; }      // cull back
    if bits & 0x0200 != 0 { out |= 0x0020_0000; }  // smooth shading
    out
}

/// Walk a display list using F3D (original) opcode table.
/// Used by earlier games: Super Mario 64, Mario Kart 64, etc.
pub fn process_display_list_f3d(renderer: &mut Renderer, rdram: &mut [u8], addr: u32) {
    // F3D opcode constants (differ from F3DEX2)
    const F3D_MTX: u8           = 0x01;
    const F3D_MOVEMEM: u8       = 0x03;
    const F3D_VTX: u8           = 0x04;
    const F3D_DL: u8            = 0x06;
    const F3D_RDPHALF_1: u8     = 0xB4;
    const F3D_CLEARGEOMETRYMODE: u8 = 0xB6;
    const F3D_SETGEOMETRYMODE: u8   = 0xB7;
    const F3D_ENDDL: u8         = 0xB8;
    const F3D_SETOTHERMODE_L: u8 = 0xB9;
    const F3D_SETOTHERMODE_H: u8 = 0xBA;
    const F3D_TEXTURE: u8       = 0xBB;
    const F3D_MOVEWORD: u8      = 0xBC;
    const F3D_POPMTX: u8        = 0xBD;
    const F3D_TRI1: u8          = 0xBF;

    // F3D geometry mode bits differ from F3DEX2 — see translate_f3d_geom().

    let mut pc = addr & 0x00FF_FFFF;
    let mut stack: Vec<u32> = Vec::with_capacity(MAX_DL_STACK);
    let mut cmd_count = 0usize;

    loop {
        if cmd_count >= MAX_COMMANDS {
            log::warn!("F3D display list exceeded {} commands, stopping", MAX_COMMANDS);
            break;
        }
        cmd_count += 1;

        let w0 = read_u32(rdram, pc);
        let w1 = read_u32(rdram, pc.wrapping_add(4));
        pc = pc.wrapping_add(8);

        let cmd = (w0 >> 24) as u8;

        // DL command logging for debug overlay
        if renderer.debug_dl_log && renderer.dl_log_entries.len() < 4096 {
            renderer.dl_log_entries.push(crate::debug::DlLogEntry {
                pc: pc.wrapping_sub(8),
                w0,
                w1,
                opcode_name: opcode_name_f3d(cmd),
            });
        }

        match cmd {
            0x00 => {} // G_NOOP / G_SPNOOP

            F3D_ENDDL => {
                if let Some(return_addr) = stack.pop() {
                    pc = return_addr;
                } else {
                    break;
                }
            }

            F3D_DL => {
                let addr = renderer.resolve_segment(w1);
                let push = (w0 >> 16) & 0xFF;
                if push == 0 {
                    stack.push(pc);
                }
                pc = addr;
            }

            // ─── F3D vertex load ───
            // w0: [04][par:8][length:16] where par = ((n-1)<<4)|v0
            // w1: DRAM address
            F3D_VTX => {
                let par = (w0 >> 16) & 0xFF;
                let n = ((par >> 4) & 0xF) as usize + 1;
                let v0 = (par & 0xF) as usize;

                // Build a w0 compatible with F3DEX2's cmd_vertex()
                // F3DEX2 format: [01][00][num:8][v0+num:7 << 1]
                let compat_w0 = ((n as u32) << 12) | (((v0 + n) as u32 & 0x7F) << 1);
                renderer.cmd_vertex(compat_w0, w1, rdram);
            }

            // ─── F3D triangle ───
            // w0: [BF][00][00][00]  w1: [00][v0*10:8][v1*10:8][v2*10:8]
            F3D_TRI1 => {
                let v0 = ((w1 >> 16) & 0xFF) as usize / 10;
                let v1 = ((w1 >> 8) & 0xFF) as usize / 10;
                let v2 = (w1 & 0xFF) as usize / 10;
                if v0 < 32 && v1 < 32 && v2 < 32 {
                    renderer.rasterize_triangle(v0, v1, v2, rdram);
                    renderer.tri_count += 1;
                }
            }

            // ─── F3D matrix ───
            // w0: [01][params:8][length:16]  w1: address
            // F3D params: bit0=projection, bit1=load, bit2=push(inverted)
            F3D_MTX => {
                let f3d_params = (w0 >> 16) & 0xFF;
                // Translate F3D params to F3DEX2 format:
                // F3DEX2: bit0=push(inv), bit1=load, bit2=projection
                let push_inv = (f3d_params >> 2) & 1;  // F3D bit2 → F3DEX2 bit0
                let load = (f3d_params >> 1) & 1;       // same position
                let proj = f3d_params & 1;               // F3D bit0 → F3DEX2 bit2
                let compat_params = push_inv | (load << 1) | (proj << 2);
                let compat_w0 = compat_params;
                renderer.cmd_load_matrix(compat_w0, w1, rdram);
            }

            F3D_POPMTX => {
                renderer.cmd_pop_matrix(w0, w1);
            }

            // ─── F3D geometry mode (split into two commands) ───
            // Translate F3D-specific bits to F3DEX2 positions before applying.
            F3D_CLEARGEOMETRYMODE => {
                let bits = translate_f3d_geom(w1);
                renderer.geometry_mode &= !bits;
            }
            F3D_SETGEOMETRYMODE => {
                let bits = translate_f3d_geom(w1);
                renderer.geometry_mode |= bits;
            }

            F3D_TEXTURE => renderer.cmd_texture(w0, w1),
            F3D_SETOTHERMODE_H => renderer.cmd_set_other_mode_h(w0, w1),
            F3D_SETOTHERMODE_L => renderer.cmd_set_other_mode_l(w0, w1),

            F3D_MOVEWORD => {
                // F3D encoding: w0 = [BC][offset:16][index:8]
                // (F3DEX2 puts index in bits[23:16], F3D puts it in bits[7:0])
                let index = w0 & 0xFF;
                let offset = (w0 >> 8) & 0xFFFF;
                match index {
                    0x06 => { // G_MW_SEGMENT
                        let seg = (offset / 4) as usize;
                        if seg < 16 {
                            renderer.segment_table[seg] = w1 & 0x00FF_FFFF;
                        }
                    }
                    0x08 => renderer.cmd_set_fog(w1),
                    0x02 => { // G_MW_NUMLIGHT — F3D formula: ((val - 0x80000000) / 32) - 1
                        let val = w1.wrapping_sub(0x80000000);
                        renderer.num_dir_lights = (val / 32).saturating_sub(1) as u8;
                    }
                    0x0E => {} // G_MW_PERSPNORM — not needed for HLE
                    0x0A => {} // G_MW_LIGHTCOL — handled via MOVEMEM
                    _ => {}
                }
            }

            F3D_MOVEMEM => {
                // F3D G_MOVEMEM: w0[23:16] = type/index
                let mtype = (w0 >> 16) & 0xFF;
                match mtype {
                    0x80 => renderer.cmd_set_viewport(w1, rdram), // G_MV_VIEWPORT
                    // F3D light indices: 0x82=lookaty, 0x84=lookatx,
                    // 0x86=light0, 0x88=light1, ... 0x94=light7
                    // Map to cmd_set_light slot: (mtype-0x82)/2, offset=slot*24
                    0x82..=0x94 => {
                        let slot = ((mtype as usize) - 0x82) / 2;
                        renderer.cmd_set_light(slot * 24, w1, rdram);
                    }
                    _ => {}
                }
            }

            F3D_RDPHALF_1 => {
                renderer.rdp_half[0] = w1;
            }

            // ─── RDP commands (same as F3DEX2) ───
            G_SETCIMG => renderer.cmd_set_color_image(w0, w1),
            G_SETZIMG => renderer.cmd_set_z_image(w0, w1),
            G_SETTIMG => renderer.cmd_set_texture_image(w0, w1),
            G_SETFILLCOLOR => renderer.cmd_set_fill_color(w1),
            G_SETENVCOLOR => renderer.cmd_set_env_color(w1),
            G_SETPRIMCOLOR => renderer.cmd_set_prim_color(w0, w1),
            G_SETBLENDCOLOR => renderer.cmd_set_blend_color(w1),
            G_SETFOGCOLOR => renderer.cmd_set_fog_color(w1),
            G_SETCOMBINE => renderer.cmd_set_combine(w0, w1),
            G_SETSCISSOR => renderer.cmd_set_scissor(w0, w1),
            G_SETTILE => renderer.cmd_set_tile(w0, w1),
            G_SETTILESIZE => renderer.cmd_set_tile_size(w0, w1),
            G_LOADBLOCK => renderer.cmd_load_block(w0, w1, rdram),
            G_LOADTILE => renderer.cmd_load_tile(w0, w1, rdram),
            G_LOADTLUT => renderer.cmd_load_tlut(w0, w1, rdram),
            G_FILLRECT => renderer.cmd_fill_rect(w0, w1, rdram),

            G_TEXRECT | G_TEXRECTFLIP => {
                let w2 = read_u32(rdram, pc);
                let w3 = read_u32(rdram, pc.wrapping_add(4));
                pc = pc.wrapping_add(8);
                let (extra0, extra1) = if (w2 >> 24) as u8 == F3D_RDPHALF_1 {
                    let h1 = w3;
                    let _w4 = read_u32(rdram, pc);
                    let w5 = read_u32(rdram, pc.wrapping_add(4));
                    pc = pc.wrapping_add(8);
                    (h1, w5)
                } else {
                    (w2, w3)
                };
                renderer.cmd_texture_rect(w0, w1, extra0, extra1, rdram, cmd == G_TEXRECTFLIP);
            }

            G_RDPFULLSYNC | G_RDPTILESYNC | G_RDPPIPESYNC | G_RDPLOADSYNC => {}
            G_SETCONVERT | G_SETKEYR | G_SETKEYGB => {}

            _ => {
                log::trace!("Unknown F3D command {:#04X} at {:#010X}", cmd, pc.wrapping_sub(8));
            }
        }
    }

    log::debug!("F3D display list processed: {} commands", cmd_count);
}

/// Look up a human-readable name for an F3DEX2 GBI opcode.
pub fn opcode_name_f3dex2(cmd: u8) -> &'static str {
    match cmd {
        0x00 => "G_NOOP",
        0x01 => "G_VTX",
        0x02 => "G_MODIFYVTX",
        0x05 => "G_TRI1",
        0x06 => "G_TRI2",
        0x07 => "G_QUAD",
        0xD5 => "G_SPECIAL_1",
        0xD7 => "G_TEXTURE",
        0xD8 => "G_POPMTX",
        0xD9 => "G_GEOMETRYMODE",
        0xDA => "G_MTX",
        0xDB => "G_MOVEWORD",
        0xDC => "G_MOVEMEM",
        0xDE => "G_DL",
        0xDF => "G_ENDDL",
        0xE1 => "G_RDPHALF_1",
        0xE2 => "G_SETOTHERMODE_L",
        0xE3 => "G_SETOTHERMODE_H",
        0xE4 => "G_TEXRECT",
        0xE5 => "G_TEXRECTFLIP",
        0xE6 => "G_RDPLOADSYNC",
        0xE7 => "G_RDPPIPESYNC",
        0xE8 => "G_RDPTILESYNC",
        0xE9 => "G_RDPFULLSYNC",
        0xEA => "G_SETKEYGB",
        0xEB => "G_SETKEYR",
        0xEC => "G_SETCONVERT",
        0xED => "G_SETSCISSOR",
        0xEF => "G_RDPSETOTHERMODE",
        0xF0 => "G_LOADTLUT",
        0xF1 => "G_RDPHALF_2",
        0xF2 => "G_SETTILESIZE",
        0xF3 => "G_LOADBLOCK",
        0xF4 => "G_LOADTILE",
        0xF5 => "G_SETTILE",
        0xF6 => "G_FILLRECT",
        0xF7 => "G_SETFILLCOLOR",
        0xF8 => "G_SETFOGCOLOR",
        0xF9 => "G_SETBLENDCOLOR",
        0xFA => "G_SETPRIMCOLOR",
        0xFB => "G_SETENVCOLOR",
        0xFC => "G_SETCOMBINE",
        0xFD => "G_SETTIMG",
        0xFE => "G_SETZIMG",
        0xFF => "G_SETCIMG",
        _ => "???",
    }
}

/// Look up a human-readable name for an F3D (original) GBI opcode.
pub fn opcode_name_f3d(cmd: u8) -> &'static str {
    match cmd {
        0x00 => "G_SPNOOP",
        0x01 => "G_MTX",
        0x03 => "G_MOVEMEM",
        0x04 => "G_VTX",
        0x06 => "G_DL",
        0xB4 => "G_RDPHALF_1",
        0xB6 => "G_CLEARGEOM",
        0xB7 => "G_SETGEOM",
        0xB8 => "G_ENDDL",
        0xB9 => "G_SETOTHERMODE_L",
        0xBA => "G_SETOTHERMODE_H",
        0xBB => "G_TEXTURE",
        0xBC => "G_MOVEWORD",
        0xBD => "G_POPMTX",
        0xBF => "G_TRI1",
        // RDP commands (same encoding)
        0xE4 => "G_TEXRECT",
        0xE5 => "G_TEXRECTFLIP",
        0xE6 => "G_RDPLOADSYNC",
        0xE7 => "G_RDPPIPESYNC",
        0xE8 => "G_RDPTILESYNC",
        0xE9 => "G_RDPFULLSYNC",
        0xED => "G_SETSCISSOR",
        0xF0 => "G_LOADTLUT",
        0xF2 => "G_SETTILESIZE",
        0xF3 => "G_LOADBLOCK",
        0xF4 => "G_LOADTILE",
        0xF5 => "G_SETTILE",
        0xF6 => "G_FILLRECT",
        0xF7 => "G_SETFILLCOLOR",
        0xF8 => "G_SETFOGCOLOR",
        0xF9 => "G_SETBLENDCOLOR",
        0xFA => "G_SETPRIMCOLOR",
        0xFB => "G_SETENVCOLOR",
        0xFC => "G_SETCOMBINE",
        0xFD => "G_SETTIMG",
        0xFE => "G_SETZIMG",
        0xFF => "G_SETCIMG",
        _ => "???",
    }
}

/// Read a big-endian u32 from RDRAM.
fn read_u32(rdram: &[u8], addr: u32) -> u32 {
    let off = (addr as usize) & (rdram.len() - 1);
    if off + 3 >= rdram.len() { return 0; }
    u32::from_be_bytes([rdram[off], rdram[off+1], rdram[off+2], rdram[off+3]])
}

