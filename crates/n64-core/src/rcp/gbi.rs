/// GBI — Graphics Binary Interface (F3DEX2 variant).
///
/// Defines the display list command format used by Zelda OoT and most
/// N64 games. Each command is 64 bits (two 32-bit words). The upper
/// byte of the first word identifies the command.
///
/// The F3DEX2 microcode processes these commands on the RSP, converting
/// geometry commands into RDP rasterization commands. In HLE mode, we
/// interpret them directly.

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
const MAX_COMMANDS: usize = 100_000;

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
                match index {
                    0x06 => { // G_MW_SEGMENT
                        let seg = ((w0 & 0xFFFF) / 4) as usize;
                        if seg < 16 {
                            renderer.segment_table[seg] = w1 & 0x00FF_FFFF;
                        }
                    }
                    0x08 => { // G_MW_FOG
                        renderer.cmd_set_fog(w1);
                    }
                    0x0E => { // G_MW_NUMLIGHT
                        renderer.num_dir_lights = (w1 / 24) as u8;
                    }
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

    log::debug!("Display list processed: {} commands", cmd_count);

    // Log opcode distribution for debugging
    let mut summary = String::new();
    for (op, &count) in opcode_counts.iter().enumerate() {
        if count > 0 {
            summary.push_str(&format!(" {:02X}:{}", op, count));
        }
    }
    log::debug!("  Opcodes:{}", summary);
}

/// Read a big-endian u32 from RDRAM.
fn read_u32(rdram: &[u8], addr: u32) -> u32 {
    let off = (addr as usize) & (rdram.len() - 1);
    if off + 3 >= rdram.len() { return 0; }
    u32::from_be_bytes([rdram[off], rdram[off+1], rdram[off+2], rdram[off+3]])
}

