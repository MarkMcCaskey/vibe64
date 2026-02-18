/// Software RDP — Reality Display Processor emulation.
///
/// Maintains RDP rendering state and implements rasterization for
/// fill rectangles, texture rectangles, and (eventually) triangles.
/// Output pixels are written directly to RDRAM at the configured
/// color image address.

/// Tile descriptor — one of 8 texture tile configurations.
#[derive(Clone, Copy, Default)]
pub struct TileDescriptor {
    pub format: u8,    // 0=RGBA, 1=YUV, 2=CI, 3=IA, 4=I
    pub size: u8,      // 0=4bit, 1=8bit, 2=16bit, 3=32bit
    pub line: u16,     // TMEM line stride (in 64-bit words)
    pub tmem: u16,     // TMEM offset (in 64-bit words)
    pub palette: u8,   // Palette index (for CI4)
    pub cm_s: u8,      // Clamp/Mirror/Wrap for S
    pub mask_s: u8,    // S mask (power of 2)
    pub shift_s: u8,   // S shift
    pub cm_t: u8,      // Clamp/Mirror/Wrap for T
    pub mask_t: u8,    // T mask (power of 2)
    pub shift_t: u8,   // T shift
    // Tile size (set by G_SETTILESIZE)
    pub sl: u16,       // S low (10.2 fixed-point)
    pub tl: u16,       // T low (10.2 fixed-point)
    pub sh: u16,       // S high (10.2 fixed-point)
    pub th: u16,       // T high (10.2 fixed-point)
}

/// Transformed vertex in screen space.
#[derive(Clone, Copy, Default)]
pub struct Vertex {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub w: f32,
    pub s: f32,
    pub t: f32,
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

/// The software renderer / RDP state machine.
pub struct Renderer {
    // ─── Color image (framebuffer destination) ───
    pub color_image_addr: u32,
    pub color_image_width: u32,
    pub color_image_format: u8,
    pub color_image_size: u8,

    // ─── Z image (depth buffer) ───
    pub z_image_addr: u32,

    // ─── Texture image source ───
    pub texture_image_addr: u32,
    pub texture_image_width: u32,
    pub texture_image_format: u8,
    pub texture_image_size: u8,

    // ─── Fill color ───
    pub fill_color: u32,

    // ─── Scissor (10.2 fixed-point) ───
    pub scissor_ulx: u16,
    pub scissor_uly: u16,
    pub scissor_lrx: u16,
    pub scissor_lry: u16,

    // ─── Other modes ───
    pub othermode_h: u32,
    pub othermode_l: u32,

    // ─── Colors ───
    pub env_color: [u8; 4],    // RGBA
    pub prim_color: [u8; 4],   // RGBA
    pub blend_color: [u8; 4],  // RGBA
    pub fog_color: [u8; 4],    // RGBA
    pub prim_lod_frac: u8,
    pub prim_min_level: u8,

    // ─── Color combiner ───
    pub combine_hi: u32,
    pub combine_lo: u32,

    // ─── Segment table (RSP address resolution) ───
    pub segment_table: [u32; 16],

    // ─── Fog parameters ───
    pub fog_multiplier: i16,
    pub fog_offset: i16,

    // ─── Lighting ───
    pub num_dir_lights: u8,
    pub light_colors: [[u8; 3]; 8],   // 0..num_dir_lights = directional, num_dir_lights = ambient
    pub light_dirs: [[i8; 3]; 8],

    // ─── Texture memory (4 KB, mirroring real TMEM) ───
    pub tmem: [u8; 4096],

    // ─── 8 tile descriptors ───
    pub tiles: [TileDescriptor; 8],

    // ─── Geometry state ───
    pub vertex_buffer: [Vertex; 32],
    pub geometry_mode: u32,
    pub texture_on: bool,
    pub texture_tile: u8,
    pub texture_scale_s: u16,
    pub texture_scale_t: u16,

    // ─── Matrix stack ───
    pub modelview: [[f32; 4]; 4],
    pub projection: [[f32; 4]; 4],
    pub mvp: [[f32; 4]; 4],
    pub matrix_stack: Vec<[[f32; 4]; 4]>,

    // ─── Viewport (pixel-space scale & translate) ───
    pub viewport_scale: [f32; 3],
    pub viewport_trans: [f32; 3],

    // ─── RDP half-word storage (for texture rect) ───
    pub rdp_half: [u32; 2],

    // ─── Debug counters ───
    pub fill_rect_count: u32,
    pub tex_rect_count: u32,
    pub tex_rect_skip: u32,
    pub tri_count: u32,
    pub vtx_count: u32,
}

impl Renderer {
    pub fn new() -> Self {
        Self {
            color_image_addr: 0,
            color_image_width: 320,
            color_image_format: 0,
            color_image_size: 2,
            z_image_addr: 0,
            texture_image_addr: 0,
            texture_image_width: 0,
            texture_image_format: 0,
            texture_image_size: 0,
            fill_color: 0,
            scissor_ulx: 0,
            scissor_uly: 0,
            scissor_lrx: 320 << 2,
            scissor_lry: 240 << 2,
            othermode_h: 0,
            othermode_l: 0,
            env_color: [0; 4],
            prim_color: [0; 4],
            blend_color: [0; 4],
            fog_color: [0; 4],
            prim_lod_frac: 0,
            prim_min_level: 0,
            combine_hi: 0,
            combine_lo: 0,
            segment_table: [0; 16],
            fog_multiplier: 0,
            fog_offset: 0,
            num_dir_lights: 0,
            light_colors: [[0; 3]; 8],
            light_dirs: [[0; 3]; 8],
            tmem: [0; 4096],
            tiles: [TileDescriptor::default(); 8],
            vertex_buffer: [Vertex::default(); 32],
            geometry_mode: 0,
            texture_on: false,
            texture_tile: 0,
            texture_scale_s: 0,
            texture_scale_t: 0,
            modelview: identity_matrix(),
            projection: identity_matrix(),
            mvp: identity_matrix(),
            matrix_stack: Vec::new(),
            viewport_scale: [160.0, 120.0, 511.0],
            viewport_trans: [160.0, 120.0, 511.0],
            rdp_half: [0; 2],
            fill_rect_count: 0,
            tex_rect_count: 0,
            tex_rect_skip: 0,
            tri_count: 0,
            vtx_count: 0,
        }
    }

    /// Current cycle type from othermode_H bits 52-53.
    fn cycle_type(&self) -> u8 {
        ((self.othermode_h >> 20) & 0x3) as u8
    }

    /// Resolve a segment-relative address to a physical RDRAM address.
    /// Format: addr[27:24] = segment ID, addr[23:0] = offset.
    /// Result = segment_table[seg] + offset.
    pub fn resolve_segment(&self, addr: u32) -> u32 {
        let seg = ((addr >> 24) & 0x0F) as usize;
        let offset = addr & 0x00FF_FFFF;
        self.segment_table[seg] + offset
    }

    // ═══════════════════════════════════════════════════════════════
    // RDP State Commands
    // ═══════════════════════════════════════════════════════════════

    /// G_SETCIMG: Set the framebuffer destination.
    /// w0: [FF][fmt:3][siz:2][0:1][width-1:12]  w1: [address:32]
    pub fn cmd_set_color_image(&mut self, w0: u32, w1: u32) {
        self.color_image_format = ((w0 >> 21) & 0x7) as u8;
        self.color_image_size = ((w0 >> 19) & 0x3) as u8;
        self.color_image_width = (w0 & 0xFFF) + 1;
        self.color_image_addr = self.resolve_segment(w1);
        log::trace!("SetColorImage: addr={:#X} width={} size={}",
            self.color_image_addr, self.color_image_width, self.color_image_size);
    }

    /// G_SETZIMG: Set the depth buffer destination.
    pub fn cmd_set_z_image(&mut self, _w0: u32, w1: u32) {
        self.z_image_addr = self.resolve_segment(w1);
    }

    /// G_SETTIMG: Set the texture source address.
    pub fn cmd_set_texture_image(&mut self, w0: u32, w1: u32) {
        self.texture_image_format = ((w0 >> 21) & 0x7) as u8;
        self.texture_image_size = ((w0 >> 19) & 0x3) as u8;
        self.texture_image_width = (w0 & 0xFFF) + 1;
        self.texture_image_addr = self.resolve_segment(w1);
    }

    pub fn cmd_set_fill_color(&mut self, w1: u32) {
        self.fill_color = w1;
    }

    pub fn cmd_set_env_color(&mut self, w1: u32) {
        self.env_color = w1.to_be_bytes();
    }

    pub fn cmd_set_prim_color(&mut self, w0: u32, w1: u32) {
        self.prim_min_level = ((w0 >> 8) & 0x1F) as u8;
        self.prim_lod_frac = (w0 & 0xFF) as u8;
        self.prim_color = w1.to_be_bytes();
    }

    pub fn cmd_set_blend_color(&mut self, w1: u32) {
        self.blend_color = w1.to_be_bytes();
    }

    pub fn cmd_set_fog_color(&mut self, w1: u32) {
        self.fog_color = w1.to_be_bytes();
    }

    /// G_MW_FOG: Set fog multiplier and offset.
    /// w1[31:16] = multiplier (s16), w1[15:0] = offset (s16).
    pub fn cmd_set_fog(&mut self, w1: u32) {
        self.fog_multiplier = (w1 >> 16) as i16;
        self.fog_offset = (w1 & 0xFFFF) as i16;
    }

    pub fn cmd_set_combine(&mut self, w0: u32, w1: u32) {
        self.combine_hi = w0 & 0x00FF_FFFF;
        self.combine_lo = w1;
    }

    /// G_SETOTHERMODE_H: Modify high half of other modes.
    /// w0: [E3][00][shift:8][len:8]  w1: [data:32]
    pub fn cmd_set_other_mode_h(&mut self, w0: u32, w1: u32) {
        let shift = (w0 >> 8) & 0xFF;
        let len = w0 & 0xFF;
        let len = if len == 0 { 32 } else { len };
        let mask = ((1u64 << len) - 1) as u32;
        self.othermode_h = (self.othermode_h & !(mask << shift)) | (w1 & (mask << shift));
    }

    /// G_SETOTHERMODE_L: Modify low half of other modes.
    pub fn cmd_set_other_mode_l(&mut self, w0: u32, w1: u32) {
        let shift = (w0 >> 8) & 0xFF;
        let len = w0 & 0xFF;
        let len = if len == 0 { 32 } else { len };
        let mask = ((1u64 << len) - 1) as u32;
        self.othermode_l = (self.othermode_l & !(mask << shift)) | (w1 & (mask << shift));
    }

    /// G_SETSCISSOR: Set clipping rectangle.
    /// w0: [ED][ulx:12][uly:12]  w1: [mode:4][lrx:12][lry:12]
    pub fn cmd_set_scissor(&mut self, w0: u32, w1: u32) {
        self.scissor_ulx = ((w0 >> 12) & 0xFFF) as u16;
        self.scissor_uly = (w0 & 0xFFF) as u16;
        self.scissor_lrx = ((w1 >> 12) & 0xFFF) as u16;
        self.scissor_lry = (w1 & 0xFFF) as u16;
    }

    // ═══════════════════════════════════════════════════════════════
    // Tile Commands
    // ═══════════════════════════════════════════════════════════════

    /// G_SETTILE: Configure a tile descriptor.
    pub fn cmd_set_tile(&mut self, w0: u32, w1: u32) {
        let idx = ((w1 >> 24) & 0x7) as usize;
        let tile = &mut self.tiles[idx];
        tile.format = ((w0 >> 21) & 0x7) as u8;
        tile.size = ((w0 >> 19) & 0x3) as u8;
        tile.line = ((w0 >> 9) & 0x1FF) as u16;
        tile.tmem = (w0 & 0x1FF) as u16;
        tile.palette = ((w1 >> 20) & 0xF) as u8;
        tile.cm_t = ((w1 >> 18) & 0x3) as u8;
        tile.mask_t = ((w1 >> 14) & 0xF) as u8;
        tile.shift_t = ((w1 >> 10) & 0xF) as u8;
        tile.cm_s = ((w1 >> 8) & 0x3) as u8;
        tile.mask_s = ((w1 >> 4) & 0xF) as u8;
        tile.shift_s = (w1 & 0xF) as u8;
    }

    /// G_SETTILESIZE: Set tile texture coordinates.
    pub fn cmd_set_tile_size(&mut self, w0: u32, w1: u32) {
        let idx = ((w1 >> 24) & 0x7) as usize;
        let tile = &mut self.tiles[idx];
        tile.sl = ((w0 >> 12) & 0xFFF) as u16;
        tile.tl = (w0 & 0xFFF) as u16;
        tile.sh = ((w1 >> 12) & 0xFFF) as u16;
        tile.th = (w1 & 0xFFF) as u16;
    }

    /// G_LOADBLOCK: Copy texture data from RDRAM to TMEM as a contiguous block.
    pub fn cmd_load_block(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let _sl = (w0 >> 12) & 0xFFF;
        let _tl = w0 & 0xFFF;
        let tile_idx = ((w1 >> 24) & 0x7) as usize;
        let texels = ((w1 >> 12) & 0xFFF) + 1; // number of texels
        let _dxt = w1 & 0xFFF;

        let tile = &self.tiles[tile_idx];
        let tmem_offset = (tile.tmem as usize) * 8; // TMEM offset in bytes

        // Calculate bytes per texel
        let bytes_per_texel = match tile.size {
            0 => 1, // 4-bit (we'll handle as 1 byte per 2 texels)
            1 => 1, // 8-bit
            2 => 2, // 16-bit
            3 => 4, // 32-bit
            _ => 2,
        };
        let byte_count = if tile.size == 0 {
            (texels as usize + 1) / 2
        } else {
            texels as usize * bytes_per_texel
        };

        let src = self.texture_image_addr as usize;
        let copy_len = byte_count.min(4096 - tmem_offset).min(rdram.len().saturating_sub(src));

        for i in 0..copy_len {
            let src_idx = (src + i) & (rdram.len() - 1);
            if tmem_offset + i < 4096 {
                self.tmem[tmem_offset + i] = rdram[src_idx];
            }
        }
    }

    /// G_LOADTILE: Copy a rectangular region of texture data from RDRAM to TMEM.
    pub fn cmd_load_tile(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let sl = ((w0 >> 12) & 0xFFF) >> 2;
        let tl = (w0 & 0xFFF) >> 2;
        let tile_idx = ((w1 >> 24) & 0x7) as usize;
        let sh = ((w1 >> 12) & 0xFFF) >> 2;
        let th = (w1 & 0xFFF) >> 2;

        let tile = &self.tiles[tile_idx];
        let tmem_offset = (tile.tmem as usize) * 8;
        let src_width = self.texture_image_width as usize;

        let bpp = match self.texture_image_size {
            0 => 1, // 4-bit, handled specially
            1 => 1,
            2 => 2,
            3 => 4,
            _ => 2,
        };

        let src_base = self.texture_image_addr as usize;
        let mut tmem_pos = tmem_offset;

        for t in tl..=th {
            for s in sl..=sh {
                let src_offset = if self.texture_image_size == 0 {
                    (t as usize * src_width + s as usize) / 2
                } else {
                    (t as usize * src_width + s as usize) * bpp
                };
                let src_addr = (src_base + src_offset) & (rdram.len() - 1);
                for b in 0..bpp {
                    if tmem_pos < 4096 && src_addr + b < rdram.len() {
                        self.tmem[tmem_pos] = rdram[src_addr + b];
                    }
                    tmem_pos += 1;
                }
            }
        }
    }

    /// G_LOADTLUT: Load texture lookup table (palette) into TMEM.
    pub fn cmd_load_tlut(&mut self, _w0: u32, w1: u32, rdram: &[u8]) {
        let tile_idx = ((w1 >> 24) & 0x7) as usize;
        let count = (((w1 >> 14) & 0x3FF) + 1) as usize;

        let _tile = &self.tiles[tile_idx];
        // TLUT is loaded to upper half of TMEM (offset 0x800)
        let tmem_offset = 0x800 + (_tile.tmem as usize) * 8;
        let src = self.texture_image_addr as usize;

        let byte_count = count * 2; // 16-bit entries
        for i in 0..byte_count {
            let src_idx = (src + i) & (rdram.len() - 1);
            let dst_idx = tmem_offset + i;
            if dst_idx < 4096 {
                self.tmem[dst_idx] = rdram[src_idx];
            }
        }
    }

    // ═══════════════════════════════════════════════════════════════
    // Drawing Commands
    // ═══════════════════════════════════════════════════════════════

    /// G_FILLRECT: Fill a rectangle with the current fill color.
    /// w0: [F6][lrx:12][lry:12]  w1: [00][ulx:12][uly:12]
    /// Coordinates are 10.2 fixed-point.
    pub fn cmd_fill_rect(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let lrx = ((w0 >> 12) & 0xFFF) as i32;
        let lry = (w0 & 0xFFF) as i32;
        let ulx = ((w1 >> 12) & 0xFFF) as i32;
        let uly = (w1 & 0xFFF) as i32;

        // Convert 10.2 to pixels (round down for upper-left, round down for lower-right)
        let x0 = (ulx >> 2).max(self.scissor_ulx as i32 >> 2);
        let y0 = (uly >> 2).max(self.scissor_uly as i32 >> 2);
        let x1 = ((lrx >> 2) + 1).min(self.scissor_lrx as i32 >> 2);
        let y1 = ((lry >> 2) + 1).min(self.scissor_lry as i32 >> 2);

        if x0 >= x1 || y0 >= y1 || self.color_image_addr == 0 {
            return;
        }

        let width = self.color_image_width as i32;
        let addr = self.color_image_addr as usize;

        match self.color_image_size {
            2 => {
                // 16-bit: fill_color contains two packed RGBA5551 pixels
                let color_even = (self.fill_color >> 16) as u16;
                let color_odd = (self.fill_color & 0xFFFF) as u16;
                for y in y0..y1 {
                    for x in x0..x1 {
                        let offset = addr + ((y * width + x) as usize) * 2;
                        if offset + 1 < rdram.len() {
                            let color = if x & 1 == 0 { color_even } else { color_odd };
                            let bytes = color.to_be_bytes();
                            rdram[offset] = bytes[0];
                            rdram[offset + 1] = bytes[1];
                        }
                    }
                }
            }
            3 => {
                // 32-bit: fill_color is a single RGBA8888 pixel
                let bytes = self.fill_color.to_be_bytes();
                for y in y0..y1 {
                    for x in x0..x1 {
                        let offset = addr + ((y * width + x) as usize) * 4;
                        if offset + 3 < rdram.len() {
                            rdram[offset] = bytes[0];
                            rdram[offset + 1] = bytes[1];
                            rdram[offset + 2] = bytes[2];
                            rdram[offset + 3] = bytes[3];
                        }
                    }
                }
            }
            _ => {}
        }

        self.fill_rect_count += 1;
    }

    /// G_TEXRECT / G_TEXRECTFLIP: Draw a textured rectangle.
    /// w0: [E4][lrx:12][lry:12]  w1: [tile:3][00][ulx:12][uly:12]
    /// extra0: [s:16][t:16]       extra1: [dsdx:16][dtdy:16]
    pub fn cmd_texture_rect(
        &mut self, w0: u32, w1: u32, extra0: u32, extra1: u32,
        rdram: &mut [u8], flip: bool,
    ) {
        let lrx = ((w0 >> 12) & 0xFFF) as i32;
        let lry = (w0 & 0xFFF) as i32;
        let tile_idx = ((w1 >> 24) & 0x7) as usize;
        let ulx = ((w1 >> 12) & 0xFFF) as i32;
        let uly = (w1 & 0xFFF) as i32;

        let s = (extra0 >> 16) as i16 as i32; // S16.5 fixed-point → S10.5
        let t = (extra0 & 0xFFFF) as i16 as i32;
        let dsdx = (extra1 >> 16) as i16 as i32; // S5.10 fixed-point
        let dtdy = (extra1 & 0xFFFF) as i16 as i32;

        // Convert 10.2 to pixels
        let x0 = (ulx >> 2).max(self.scissor_ulx as i32 >> 2);
        let y0 = (uly >> 2).max(self.scissor_uly as i32 >> 2);
        let x1 = (lrx >> 2).min(self.scissor_lrx as i32 >> 2);
        let y1 = (lry >> 2).min(self.scissor_lry as i32 >> 2);

        if x0 >= x1 || y0 >= y1 || self.color_image_addr == 0 {
            self.tex_rect_skip += 1;
            return;
        }

        let tile = &self.tiles[tile_idx];
        let addr = self.color_image_addr as usize;
        let width = self.color_image_width as i32;

        let cycle = self.cycle_type();

        // Log first few texture rects for debugging
        if self.tex_rect_count < 5 {
            log::debug!(
                "TEXRECT #{}: ({},{})→({},{}) tile={} fmt={} sz={} tmem={:#X} line={} cycle={}",
                self.tex_rect_count, x0, y0, x1, y1, tile_idx,
                tile.format, tile.size, tile.tmem * 8, tile.line, cycle,
            );
        }

        // Convert S/T from S10.5 to S10.10 to match dsdx/dtdy (S5.10)
        let s10 = s << 5;
        let t10 = t << 5;

        for y in y0..y1 {
            for x in x0..x1 {
                // Compute texture coordinates in S.10 fixed-point
                let dx = x - (ulx >> 2);
                let dy = y - (uly >> 2);

                let (tex_s, tex_t) = if flip {
                    (s10 + dy * dsdx, t10 + dx * dtdy)
                } else {
                    (s10 + dx * dsdx, t10 + dy * dtdy)
                };

                // Sample texture and run through color combiner
                let texel0 = self.sample_texture(tile_idx, tex_s >> 5, tex_t >> 5, cycle);
                let color = self.combine_pixel(texel0, [255, 255, 255, 255]);

                self.write_pixel(rdram, x, y, color);
            }
        }

        self.tex_rect_count += 1;
    }

    /// Sample a texel from TMEM at the given S/T coordinates.
    /// S and T are in 10.5 fixed-point.
    /// Checks othermode_h text_filt bits to select point or bilinear.
    fn sample_texture(&self, tile_idx: usize, s: i32, t: i32, _cycle: u8) -> [u8; 4] {
        // Text filter mode: othermode_h bits 13-12
        // 0 = point, 2 = bilinear, 3 = average (treat as bilinear)
        let text_filt = (self.othermode_h >> 12) & 0x3;

        if text_filt >= 2 {
            // Bilinear: sample 4 neighbors and blend by fractional position
            let frac_s = (s & 0x1F) as u16; // 5-bit fraction (0-31)
            let frac_t = (t & 0x1F) as u16;

            let s0 = s >> 5;
            let t0 = t >> 5;
            let s1 = s0 + 1;
            let t1 = t0 + 1;

            let c00 = self.sample_texel_point(tile_idx, s0, t0);
            let c10 = self.sample_texel_point(tile_idx, s1, t0);
            let c01 = self.sample_texel_point(tile_idx, s0, t1);
            let c11 = self.sample_texel_point(tile_idx, s1, t1);

            // Bilinear blend using 5-bit fractions (0-31 range)
            let inv_s = 32 - frac_s;
            let inv_t = 32 - frac_t;
            let mut result = [0u8; 4];
            for i in 0..4 {
                let top = c00[i] as u16 * inv_s + c10[i] as u16 * frac_s;
                let bot = c01[i] as u16 * inv_s + c11[i] as u16 * frac_s;
                result[i] = ((top * inv_t + bot * frac_t + 512) >> 10) as u8;
            }
            result
        } else {
            // Point sampling
            let si = s >> 5;
            let ti = t >> 5;
            self.sample_texel_point(tile_idx, si, ti)
        }
    }

    /// Point-sample a single texel at integer coordinates.
    fn sample_texel_point(&self, tile_idx: usize, si: i32, ti: i32) -> [u8; 4] {
        let tile = &self.tiles[tile_idx];

        let si = self.wrap_coord(si, tile.mask_s, tile.cm_s);
        let ti = self.wrap_coord(ti, tile.mask_t, tile.cm_t);

        let tmem_base = (tile.tmem as usize) * 8;
        let line_bytes = (tile.line as usize) * 8;

        match tile.size {
            0 => {
                // 4-bit (CI4 or I4)
                let byte_offset = tmem_base + ti as usize * line_bytes + (si as usize / 2);
                if byte_offset < 4096 {
                    let byte = self.tmem[byte_offset];
                    let nibble = if si & 1 == 0 { byte >> 4 } else { byte & 0x0F };
                    if tile.format == 2 {
                        self.lookup_tlut(tile.palette as usize * 16 + nibble as usize)
                    } else {
                        let i = nibble | (nibble << 4);
                        [i, i, i, 0xFF]
                    }
                } else {
                    [0, 0, 0, 0xFF]
                }
            }
            1 => {
                // 8-bit
                let byte_offset = tmem_base + ti as usize * line_bytes + si as usize;
                if byte_offset < 4096 {
                    let val = self.tmem[byte_offset];
                    match tile.format {
                        2 => self.lookup_tlut(val as usize), // CI8
                        3 => { // IA8: I[7:4], A[3:0]
                            let i = val & 0xF0;
                            let a = (val & 0x0F) << 4;
                            [i, i, i, a]
                        }
                        4 | _ => [val, val, val, 0xFF], // I8
                    }
                } else {
                    [0, 0, 0, 0xFF]
                }
            }
            2 => {
                // 16-bit
                let byte_offset = tmem_base + ti as usize * line_bytes + si as usize * 2;
                if byte_offset + 1 < 4096 {
                    let hi = self.tmem[byte_offset];
                    let lo = self.tmem[byte_offset + 1];
                    let val = ((hi as u16) << 8) | lo as u16;
                    match tile.format {
                        0 => unpack_rgba5551(val), // RGBA16
                        3 => { // IA16: I[15:8], A[7:0]
                            let i = hi;
                            [i, i, i, lo]
                        }
                        _ => unpack_rgba5551(val),
                    }
                } else {
                    [0, 0, 0, 0xFF]
                }
            }
            3 => {
                // 32-bit RGBA
                let byte_offset = tmem_base + ti as usize * line_bytes + si as usize * 4;
                if byte_offset + 3 < 4096 {
                    [
                        self.tmem[byte_offset],
                        self.tmem[byte_offset + 1],
                        self.tmem[byte_offset + 2],
                        self.tmem[byte_offset + 3],
                    ]
                } else {
                    [0, 0, 0, 0xFF]
                }
            }
            _ => [0, 0, 0, 0xFF],
        }
    }

    /// Wrap/clamp a texture coordinate.
    fn wrap_coord(&self, coord: i32, mask: u8, cm: u8) -> i32 {
        if mask == 0 {
            return coord.max(0);
        }
        let size = 1i32 << mask;
        let clamp = cm & 1 != 0;
        let mirror = cm & 2 != 0;

        if clamp {
            coord.clamp(0, size - 1)
        } else if mirror {
            let c = coord & (size * 2 - 1);
            if c >= size { size * 2 - 1 - c } else { c }
        } else {
            coord & (size - 1)
        }
    }

    /// Look up a color in the TLUT (texture lookup table) stored in upper TMEM.
    fn lookup_tlut(&self, index: usize) -> [u8; 4] {
        let offset = 0x800 + index * 2;
        if offset + 1 < 4096 {
            let hi = self.tmem[offset];
            let lo = self.tmem[offset + 1];
            let val = ((hi as u16) << 8) | lo as u16;
            unpack_rgba5551(val)
        } else {
            [0, 0, 0, 0xFF]
        }
    }

    // ═══════════════════════════════════════════════════════════════
    // Color Combiner
    // ═══════════════════════════════════════════════════════════════

    /// Evaluate the RDP color combiner: (A - B) * C + D per channel.
    /// `texel0` is the sampled texture color, `shade` is the interpolated vertex color.
    fn combine_pixel(&self, texel0: [u8; 4], shade: [u8; 4]) -> [u8; 4] {
        let cycle = self.cycle_type();

        // Copy mode: pass texel through directly
        if cycle == 2 { return texel0; }
        // Fill mode: shouldn't reach here (handled by fill_rect)
        if cycle == 3 { return texel0; }

        // Extract cycle 0 selectors from combine_hi / combine_lo
        let a0_rgb = ((self.combine_hi >> 20) & 0xF) as u8;
        let c0_rgb = ((self.combine_hi >> 15) & 0x1F) as u8;
        let a0_a   = ((self.combine_hi >> 12) & 0x7) as u8;
        let c0_a   = ((self.combine_hi >> 9) & 0x7) as u8;
        let b0_rgb = ((self.combine_lo >> 28) & 0xF) as u8;
        let d0_rgb = ((self.combine_lo >> 15) & 0x7) as u8;
        let b0_a   = ((self.combine_lo >> 12) & 0x7) as u8;
        let d0_a   = ((self.combine_lo >> 9) & 0x7) as u8;

        // Cycle 0: COMBINED input is zero (no prior cycle output)
        let combined = [0u8; 4];
        let result = self.cc_evaluate(
            a0_rgb, b0_rgb, c0_rgb, d0_rgb,
            a0_a, b0_a, c0_a, d0_a,
            &combined, &texel0, &shade,
        );

        // 2-cycle mode: run again with cycle 1 selectors, feeding cycle 0 result as COMBINED
        if cycle == 1 {
            let a1_rgb = ((self.combine_hi >> 5) & 0xF) as u8;
            let c1_rgb = (self.combine_hi & 0x1F) as u8;
            let a1_a   = ((self.combine_lo >> 21) & 0x7) as u8;
            let c1_a   = ((self.combine_lo >> 18) & 0x7) as u8;
            let b1_rgb = ((self.combine_lo >> 24) & 0xF) as u8;
            let d1_rgb = ((self.combine_lo >> 6) & 0x7) as u8;
            let b1_a   = ((self.combine_lo >> 3) & 0x7) as u8;
            let d1_a   = (self.combine_lo & 0x7) as u8;

            return self.cc_evaluate(
                a1_rgb, b1_rgb, c1_rgb, d1_rgb,
                a1_a, b1_a, c1_a, d1_a,
                &result, &texel0, &shade,
            );
        }

        result
    }

    /// Apply (A - B) * C + D for RGB and Alpha using the given selectors.
    fn cc_evaluate(
        &self,
        a_rgb_sel: u8, b_rgb_sel: u8, c_rgb_sel: u8, d_rgb_sel: u8,
        a_a_sel: u8, b_a_sel: u8, c_a_sel: u8, d_a_sel: u8,
        combined: &[u8; 4], texel0: &[u8; 4], shade: &[u8; 4],
    ) -> [u8; 4] {
        let a_rgb = self.cc_rgb_a(a_rgb_sel, combined, texel0, shade);
        let b_rgb = self.cc_rgb_b(b_rgb_sel, combined, texel0, shade);
        let c_rgb = self.cc_rgb_c(c_rgb_sel, combined, texel0, shade);
        let d_rgb = self.cc_rgb_d(d_rgb_sel, combined, texel0, shade);

        let a_a = self.cc_alpha_abd(a_a_sel, combined, texel0, shade);
        let b_a = self.cc_alpha_abd(b_a_sel, combined, texel0, shade);
        let c_a = self.cc_alpha_c(c_a_sel, texel0, shade);
        let d_a = self.cc_alpha_abd(d_a_sel, combined, texel0, shade);

        let mut result = [0u8; 4];
        for i in 0..3 {
            let v = (a_rgb[i] as i16 - b_rgb[i] as i16) * c_rgb[i] as i16 / 256
                + d_rgb[i] as i16;
            result[i] = v.clamp(0, 255) as u8;
        }
        let v = (a_a as i16 - b_a as i16) * c_a as i16 / 256 + d_a as i16;
        result[3] = v.clamp(0, 255) as u8;
        result
    }

    /// RGB input A (4-bit, 16 options): COMBINED, TEXEL0, TEXEL1, PRIM, SHADE, ENV, 1.0, NOISE, 0...
    fn cc_rgb_a(&self, sel: u8, combined: &[u8; 4], texel0: &[u8; 4], shade: &[u8; 4]) -> [u8; 3] {
        match sel {
            0 => [combined[0], combined[1], combined[2]],
            1 => [texel0[0], texel0[1], texel0[2]],
            2 => [texel0[0], texel0[1], texel0[2]], // TEXEL1 → TEXEL0
            3 => [self.prim_color[0], self.prim_color[1], self.prim_color[2]],
            4 => [shade[0], shade[1], shade[2]],
            5 => [self.env_color[0], self.env_color[1], self.env_color[2]],
            6 => [255, 255, 255], // 1.0
            _ => [0, 0, 0],       // NOISE / 0
        }
    }

    /// RGB input B (4-bit, 16 options): COMBINED, TEXEL0, TEXEL1, PRIM, SHADE, ENV, CENTER, K4, 0...
    fn cc_rgb_b(&self, sel: u8, combined: &[u8; 4], texel0: &[u8; 4], shade: &[u8; 4]) -> [u8; 3] {
        match sel {
            0 => [combined[0], combined[1], combined[2]],
            1 => [texel0[0], texel0[1], texel0[2]],
            2 => [texel0[0], texel0[1], texel0[2]], // TEXEL1
            3 => [self.prim_color[0], self.prim_color[1], self.prim_color[2]],
            4 => [shade[0], shade[1], shade[2]],
            5 => [self.env_color[0], self.env_color[1], self.env_color[2]],
            _ => [0, 0, 0], // CENTER, K4, 0
        }
    }

    /// RGB input C (5-bit, 32 options): includes alpha-as-RGB variants and LOD fraction.
    fn cc_rgb_c(&self, sel: u8, combined: &[u8; 4], texel0: &[u8; 4], shade: &[u8; 4]) -> [u8; 3] {
        match sel {
            0  => [combined[0], combined[1], combined[2]],
            1  => [texel0[0], texel0[1], texel0[2]],
            2  => [texel0[0], texel0[1], texel0[2]], // TEXEL1
            3  => [self.prim_color[0], self.prim_color[1], self.prim_color[2]],
            4  => [shade[0], shade[1], shade[2]],
            5  => [self.env_color[0], self.env_color[1], self.env_color[2]],
            7  => [combined[3], combined[3], combined[3]],     // COMBINED_ALPHA
            8  => [texel0[3], texel0[3], texel0[3]],           // TEXEL0_ALPHA
            9  => [texel0[3], texel0[3], texel0[3]],           // TEXEL1_ALPHA
            10 => [self.prim_color[3]; 3],                     // PRIM_ALPHA
            11 => [shade[3]; 3],                               // SHADE_ALPHA
            12 => [self.env_color[3]; 3],                      // ENV_ALPHA
            14 => [self.prim_lod_frac; 3],                     // PRIM_LOD_FRAC
            _  => [0, 0, 0], // KEY_SCALE, LOD_FRAC, K5, 0
        }
    }

    /// RGB input D (3-bit, 8 options): COMBINED, TEXEL0, TEXEL1, PRIM, SHADE, ENV, 1.0, 0.
    fn cc_rgb_d(&self, sel: u8, combined: &[u8; 4], texel0: &[u8; 4], shade: &[u8; 4]) -> [u8; 3] {
        match sel {
            0 => [combined[0], combined[1], combined[2]],
            1 => [texel0[0], texel0[1], texel0[2]],
            2 => [texel0[0], texel0[1], texel0[2]], // TEXEL1
            3 => [self.prim_color[0], self.prim_color[1], self.prim_color[2]],
            4 => [shade[0], shade[1], shade[2]],
            5 => [self.env_color[0], self.env_color[1], self.env_color[2]],
            6 => [255, 255, 255], // 1.0
            _ => [0, 0, 0],
        }
    }

    /// Alpha inputs A, B, D (3-bit, 8 options): COMBINED, TEXEL0, TEXEL1, PRIM, SHADE, ENV, 1.0, 0.
    fn cc_alpha_abd(&self, sel: u8, combined: &[u8; 4], texel0: &[u8; 4], shade: &[u8; 4]) -> u8 {
        match sel {
            0 => combined[3],
            1 => texel0[3],
            2 => texel0[3], // TEXEL1
            3 => self.prim_color[3],
            4 => shade[3],
            5 => self.env_color[3],
            6 => 255,
            _ => 0,
        }
    }

    /// Alpha input C (3-bit, DIFFERENT mapping): LOD_FRAC, TEXEL0, TEXEL1, PRIM, SHADE, ENV, PRIM_LOD, 0.
    fn cc_alpha_c(&self, sel: u8, texel0: &[u8; 4], shade: &[u8; 4]) -> u8 {
        match sel {
            1 => texel0[3],
            2 => texel0[3], // TEXEL1
            3 => self.prim_color[3],
            4 => shade[3],
            5 => self.env_color[3],
            6 => self.prim_lod_frac,
            _ => 0, // LOD_FRACTION, 0
        }
    }

    // ═══════════════════════════════════════════════════════════════
    // Geometry Commands
    // ═══════════════════════════════════════════════════════════════

    /// G_VTX: Load vertices into the vertex buffer.
    pub fn cmd_vertex(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let num = ((w0 >> 12) & 0xFF) as usize;
        let start = (((w0 >> 1) & 0x7F) as usize).saturating_sub(num);
        let addr = self.resolve_segment(w1) as usize;
        self.vtx_count += num as u32;

        log::trace!("G_VTX: num={} start={} addr={:#X}", num, start, addr);

        for i in 0..num {
            let idx = start + i;
            if idx >= 32 { break; }

            let off = addr + i * 16; // 16 bytes per vertex
            if off + 15 >= rdram.len() { break; }

            let x = read_i16(rdram, off) as f32;
            let y = read_i16(rdram, off + 2) as f32;
            let z = read_i16(rdram, off + 4) as f32;
            // off+6: padding/flag
            let s = read_i16(rdram, off + 8) as f32;
            let t = read_i16(rdram, off + 10) as f32;
            let a = rdram[(off + 15) & (rdram.len() - 1)];

            // When G_LIGHTING is on, bytes 12-14 are normals; compute shade color
            let (r, g, b) = if self.geometry_mode & 0x20000 != 0 {
                let nx = rdram[(off + 12) & (rdram.len() - 1)] as i8 as f32;
                let ny = rdram[(off + 13) & (rdram.len() - 1)] as i8 as f32;
                let nz = rdram[(off + 14) & (rdram.len() - 1)] as i8 as f32;

                // Transform normal by modelview upper-3x3 (n * M, row-vector convention)
                let wnx = nx * self.modelview[0][0] + ny * self.modelview[1][0] + nz * self.modelview[2][0];
                let wny = nx * self.modelview[0][1] + ny * self.modelview[1][1] + nz * self.modelview[2][1];
                let wnz = nx * self.modelview[0][2] + ny * self.modelview[1][2] + nz * self.modelview[2][2];
                let len = (wnx * wnx + wny * wny + wnz * wnz).sqrt();
                let (wnx, wny, wnz) = if len > 0.001 {
                    (wnx / len, wny / len, wnz / len)
                } else {
                    (0.0, 0.0, 1.0)
                };

                // Ambient light is at index num_dir_lights
                let n = self.num_dir_lights as usize;
                let amb = if n < 8 { self.light_colors[n] } else { [64, 64, 64] };
                let mut ra = amb[0] as f32;
                let mut ga = amb[1] as f32;
                let mut ba = amb[2] as f32;

                // Accumulate directional lights
                for li in 0..n.min(7) {
                    let dx = self.light_dirs[li][0] as f32 / 127.0;
                    let dy = self.light_dirs[li][1] as f32 / 127.0;
                    let dz = self.light_dirs[li][2] as f32 / 127.0;
                    let dot = (wnx * dx + wny * dy + wnz * dz).max(0.0);
                    ra += self.light_colors[li][0] as f32 * dot;
                    ga += self.light_colors[li][1] as f32 * dot;
                    ba += self.light_colors[li][2] as f32 * dot;
                }
                (ra.clamp(0.0, 255.0) as u8, ga.clamp(0.0, 255.0) as u8, ba.clamp(0.0, 255.0) as u8)
            } else {
                // No lighting — read raw vertex colors
                let r = rdram[(off + 12) & (rdram.len() - 1)];
                let g = rdram[(off + 13) & (rdram.len() - 1)];
                let b = rdram[(off + 14) & (rdram.len() - 1)];
                (r, g, b)
            };

            // Transform vertex by MVP matrix → clip space
            let [cx, cy, cz, cw] = mat4_mul_vec(&self.mvp, [x, y, z, 1.0]);

            // When G_FOG (0x10000) is enabled, compute fog factor from clip W
            // and replace vertex alpha with it. The blender then uses shade alpha
            // to lerp between pixel color and fog_color.
            let a = if self.geometry_mode & 0x10000 != 0 && cw.abs() > 0.0001 {
                let fog = (cw * self.fog_multiplier as f32 + self.fog_offset as f32)
                    .clamp(0.0, 255.0);
                fog as u8
            } else {
                a
            };

            // Perspective divide + viewport transform → screen space
            if cw.abs() > 0.0001 {
                let inv_w = 1.0 / cw;
                self.vertex_buffer[idx] = Vertex {
                    x: cx * inv_w * self.viewport_scale[0] + self.viewport_trans[0],
                    y: cy * inv_w * self.viewport_scale[1] + self.viewport_trans[1],
                    z: (cz * inv_w * self.viewport_scale[2] + self.viewport_trans[2])
                        .clamp(0.0, 0xFFFF as f32),
                    w: inv_w,
                    s, t, r, g, b, a,
                };
            } else {
                // Vertex at infinity — place off-screen so triangles get clipped
                self.vertex_buffer[idx] = Vertex {
                    x: -10000.0, y: -10000.0, z: 0.0, w: 0.0,
                    s, t, r, g, b, a,
                };
            }
        }
    }

    /// G_TRI1: Draw one triangle.
    pub fn cmd_tri1(&mut self, w0: u32, _w1: u32, rdram: &mut [u8]) {
        let v0 = ((w0 >> 16) & 0xFF) as usize / 2;
        let v1 = ((w0 >> 8) & 0xFF) as usize / 2;
        let v2 = (w0 & 0xFF) as usize / 2;
        if v0 < 32 && v1 < 32 && v2 < 32 {
            if self.tri_count < 5 || (self.tri_count >= 1000 && self.tri_count < 1005)
                || (self.tri_count >= 10000 && self.tri_count < 10005)
                || (self.tri_count >= 50000 && self.tri_count < 50005) {
                let a = self.vertex_buffer[v0];
                let b = self.vertex_buffer[v1];
                let c = self.vertex_buffer[v2];
                log::debug!("TRI #{}: ({:.1},{:.1}) ({:.1},{:.1}) ({:.1},{:.1}) w={:.4},{:.4},{:.4}",
                    self.tri_count, a.x, a.y, b.x, b.y, c.x, c.y, a.w, b.w, c.w);
            }
            self.rasterize_triangle(v0, v1, v2, rdram);
            self.tri_count += 1;
        }
    }

    /// G_TRI2: Draw two triangles.
    pub fn cmd_tri2(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let v0 = ((w0 >> 16) & 0xFF) as usize / 2;
        let v1 = ((w0 >> 8) & 0xFF) as usize / 2;
        let v2 = (w0 & 0xFF) as usize / 2;
        let v3 = ((w1 >> 16) & 0xFF) as usize / 2;
        let v4 = ((w1 >> 8) & 0xFF) as usize / 2;
        let v5 = (w1 & 0xFF) as usize / 2;
        if v0 < 32 && v1 < 32 && v2 < 32 {
            if self.tri_count < 5 || (self.tri_count >= 1000 && self.tri_count < 1005)
                || (self.tri_count >= 50000 && self.tri_count < 50005) {
                let a = self.vertex_buffer[v0];
                let b = self.vertex_buffer[v1];
                let c = self.vertex_buffer[v2];
                log::debug!("TRI #{}: ({:.1},{:.1}) ({:.1},{:.1}) ({:.1},{:.1})",
                    self.tri_count, a.x, a.y, b.x, b.y, c.x, c.y);
            }
            self.rasterize_triangle(v0, v1, v2, rdram);
            self.tri_count += 1;
        }
        if v3 < 32 && v4 < 32 && v5 < 32 {
            self.rasterize_triangle(v3, v4, v5, rdram);
            self.tri_count += 1;
        }
    }

    /// G_MTX: Load a 4x4 matrix from RDRAM.
    pub fn cmd_load_matrix(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let addr = self.resolve_segment(w1) as usize;
        let params = w0 & 0xFF;
        // F3DEX2: push bit is XORed in the macro (stored inverted)
        let push = params & 0x01 == 0;
        let load = params & 0x02 != 0;
        let projection = params & 0x04 != 0;

        let mat = load_fixed_point_matrix(rdram, addr);

        log::trace!("G_MTX: params={:#04X} push={} load={} proj={} addr={:#X}",
            params, push, load, projection, addr);

        if projection {
            if load {
                self.projection = mat;
            } else {
                self.projection = mat4_mul(&self.projection, &mat);
            }
        } else {
            if push {
                self.matrix_stack.push(self.modelview);
            }
            self.modelview = if load { mat } else { mat4_mul(&self.modelview, &mat) };
        }

        // N64 uses v * MVP, so MVP = MV * P
        self.mvp = mat4_mul(&self.modelview, &self.projection);
    }

    /// G_POPMTX: Pop the modelview matrix stack.
    pub fn cmd_pop_matrix(&mut self, _w0: u32, w1: u32) {
        let count = (w1 / 64).max(1);
        for _ in 0..count {
            if let Some(mat) = self.matrix_stack.pop() {
                self.modelview = mat;
            }
        }
        self.mvp = mat4_mul(&self.modelview, &self.projection);
    }

    /// G_GEOMETRYMODE: Set/clear geometry mode flags.
    pub fn cmd_geometry_mode(&mut self, w0: u32, w1: u32) {
        let clear_bits = !(w0 & 0x00FF_FFFF);
        self.geometry_mode = (self.geometry_mode & clear_bits) | w1;
    }

    /// G_TEXTURE: Set texture parameters.
    pub fn cmd_texture(&mut self, w0: u32, w1: u32) {
        self.texture_scale_s = (w1 >> 16) as u16;
        self.texture_scale_t = (w1 & 0xFFFF) as u16;
        self.texture_tile = ((w0 >> 8) & 0x7) as u8;
        self.texture_on = (w0 & 0xFF) != 0 || self.texture_scale_s != 0 || self.texture_scale_t != 0;
    }

    /// G_MOVEMEM/G_MV_VIEWPORT: Load viewport parameters from RDRAM.
    /// The viewport is 16 bytes: vscale[4] (S13.2) then vtrans[4] (S13.2).
    pub fn cmd_set_viewport(&mut self, w1: u32, rdram: &[u8]) {
        let addr = self.resolve_segment(w1) as usize;
        for i in 0..3 {
            self.viewport_scale[i] = read_i16(rdram, addr + i * 2) as f32 / 4.0;
            self.viewport_trans[i] = read_i16(rdram, addr + 8 + i * 2) as f32 / 4.0;
        }
        log::trace!(
            "Viewport: scale=({:.1},{:.1},{:.1}) trans=({:.1},{:.1},{:.1})",
            self.viewport_scale[0], self.viewport_scale[1], self.viewport_scale[2],
            self.viewport_trans[0], self.viewport_trans[1], self.viewport_trans[2],
        );
    }

    /// G_MOVEMEM/G_MV_LIGHT: Load light parameters from RDRAM.
    /// Light data is 16 bytes: col[3], pad, colc[3], pad, dir[3], pad, pad*4.
    pub fn cmd_set_light(&mut self, dmem_offset: usize, addr: u32, rdram: &[u8]) {
        let slot = dmem_offset / 24;
        if slot < 2 || slot > 9 { return; }
        let idx = slot - 2;
        if idx >= 8 { return; }

        let a = self.resolve_segment(addr) as usize;
        if a + 11 < rdram.len() {
            self.light_colors[idx] = [rdram[a], rdram[a + 1], rdram[a + 2]];
            self.light_dirs[idx] = [
                rdram[a + 8] as i8,
                rdram[a + 9] as i8,
                rdram[a + 10] as i8,
            ];
        }
    }

    /// Read a pixel from the framebuffer ("memory color" in RDP terms).
    fn read_pixel(&self, rdram: &[u8], x: i32, y: i32) -> [u8; 4] {
        let addr = self.color_image_addr as usize;
        let width = self.color_image_width as i32;

        match self.color_image_size {
            2 => {
                let offset = addr + ((y * width + x) as usize) * 2;
                if offset + 1 < rdram.len() {
                    let val = u16::from_be_bytes([rdram[offset], rdram[offset + 1]]);
                    unpack_rgba5551(val)
                } else {
                    [0, 0, 0, 0]
                }
            }
            3 => {
                let offset = addr + ((y * width + x) as usize) * 4;
                if offset + 3 < rdram.len() {
                    [rdram[offset], rdram[offset + 1], rdram[offset + 2], rdram[offset + 3]]
                } else {
                    [0, 0, 0, 0]
                }
            }
            _ => [0, 0, 0, 0],
        }
    }

    /// Apply the RDP blender: (P * A + M * B) / (A + B).
    ///
    /// P and M are color sources (pixel, memory, blend_color, fog).
    /// A and B are alpha factors selected by the blend mode.
    /// The blend mode is extracted from othermode_l bits 16-31.
    ///
    /// Cycle 0 layout in othermode_l:
    ///   P = bits[31:30], A = bits[27:26], M = bits[23:22], B = bits[19:18]
    fn blend_pixel(&self, pixel: [u8; 4], memory: [u8; 4]) -> [u8; 4] {
        let p_sel = (self.othermode_l >> 30) & 0x3;
        let a_sel = (self.othermode_l >> 26) & 0x3;
        let m_sel = (self.othermode_l >> 22) & 0x3;
        let b_sel = (self.othermode_l >> 18) & 0x3;

        // Select P color source
        let p = match p_sel {
            0 => pixel,
            1 => memory,
            2 => self.blend_color,
            _ => self.fog_color,
        };

        // Select A alpha factor (single value applied to all channels)
        let a: u16 = match a_sel {
            0 => pixel[3] as u16,
            1 => self.fog_color[3] as u16,
            2 => pixel[3] as u16, // shade alpha — use pixel alpha as approximation
            _ => 0,
        };

        // Select M color source
        let m = match m_sel {
            0 => pixel,
            1 => memory,
            2 => self.blend_color,
            _ => self.fog_color,
        };

        // Select B factor
        let b: u16 = match b_sel {
            0 => 255u16.saturating_sub(a), // 1 - A
            1 => memory[3] as u16,
            2 => 255,
            _ => 0,
        };

        let denom = a + b;
        if denom == 0 {
            return pixel;
        }

        let mut result = [0u8; 4];
        for i in 0..3 {
            let val = (p[i] as u16 * a + m[i] as u16 * b) / denom;
            result[i] = val.min(255) as u8;
        }
        result[3] = pixel[3]; // preserve incoming alpha
        result
    }

    /// Write a pixel to the framebuffer with alpha compare and blending.
    fn write_pixel(&self, rdram: &mut [u8], x: i32, y: i32, color: [u8; 4]) {
        // Alpha compare: othermode_l bits 0-1
        // 0 = none, 1 = threshold compare, 2/3 = dither
        let alpha_compare = self.othermode_l & 0x3;
        if alpha_compare == 1 && color[3] < self.blend_color[3] {
            return; // Fail alpha threshold test
        }

        // Blender: FORCE_BL (bit 14) forces the blender to execute
        let force_bl = self.othermode_l & (1 << 14) != 0;
        let final_color = if force_bl {
            let memory = self.read_pixel(rdram, x, y);
            self.blend_pixel(color, memory)
        } else {
            color
        };

        let addr = self.color_image_addr as usize;
        let width = self.color_image_width as i32;

        match self.color_image_size {
            2 => {
                let offset = addr + ((y * width + x) as usize) * 2;
                if offset + 1 >= rdram.len() { return; }
                let pixel = pack_rgba5551(final_color[0], final_color[1], final_color[2], final_color[3]);
                let bytes = pixel.to_be_bytes();
                rdram[offset] = bytes[0];
                rdram[offset + 1] = bytes[1];
            }
            3 => {
                let offset = addr + ((y * width + x) as usize) * 4;
                if offset + 3 >= rdram.len() { return; }
                rdram[offset] = final_color[0];
                rdram[offset + 1] = final_color[1];
                rdram[offset + 2] = final_color[2];
                rdram[offset + 3] = final_color[3];
            }
            _ => {}
        }
    }

    /// Rasterize a triangle using edge-function (half-space) method.
    /// Interpolates vertex colors and optionally samples textures.
    fn rasterize_triangle(&self, i0: usize, i1: usize, i2: usize, rdram: &mut [u8]) {
        let v0 = self.vertex_buffer[i0];
        let v1 = self.vertex_buffer[i1];
        let v2 = self.vertex_buffer[i2];

        // Bounding box clipped to scissor
        let scr_ulx = (self.scissor_ulx >> 2) as f32;
        let scr_uly = (self.scissor_uly >> 2) as f32;
        let scr_lrx = (self.scissor_lrx >> 2) as f32;
        let scr_lry = (self.scissor_lry >> 2) as f32;

        let min_x = v0.x.min(v1.x).min(v2.x).max(scr_ulx).floor() as i32;
        let min_y = v0.y.min(v1.y).min(v2.y).max(scr_uly).floor() as i32;
        let max_x = v0.x.max(v1.x).max(v2.x).min(scr_lrx).ceil() as i32;
        let max_y = v0.y.max(v1.y).max(v2.y).min(scr_lry).ceil() as i32;

        if min_x >= max_x || min_y >= max_y { return; }

        // Triangle area via edge function — also detects degenerate triangles
        let area = edge_function(v0.x, v0.y, v1.x, v1.y, v2.x, v2.y);
        if area.abs() < 0.001 { return; }

        // Face culling: check geometry_mode flags
        // G_CULL_FRONT = 0x0200, G_CULL_BACK = 0x0400
        // In Y-down screen space, the edge function sign is inverted:
        //   positive area = CW = back face, negative = CCW = front face
        let cull_front = self.geometry_mode & 0x0200 != 0;
        let cull_back = self.geometry_mode & 0x0400 != 0;
        if cull_back && area > 0.0 { return; }
        if cull_front && area < 0.0 { return; }

        let inv_area = 1.0 / area;

        let addr = self.color_image_addr as usize;
        let width = self.color_image_width as i32;
        let cycle = self.cycle_type();
        let textured = self.texture_on;
        let tile_idx = self.texture_tile as usize;

        // Z-buffer state from othermode_l
        let z_cmp = self.othermode_l & 0x10 != 0;
        let z_upd = self.othermode_l & 0x20 != 0;
        let z_enabled = self.geometry_mode & 0x1 != 0 && self.z_image_addr != 0;
        let z_addr = self.z_image_addr as usize;

        for y in min_y..max_y {
            for x in min_x..max_x {
                let px = x as f32 + 0.5;
                let py = y as f32 + 0.5;

                // Barycentric coordinates via edge functions
                let w0 = edge_function(v1.x, v1.y, v2.x, v2.y, px, py) * inv_area;
                let w1 = edge_function(v2.x, v2.y, v0.x, v0.y, px, py) * inv_area;
                let w2 = 1.0 - w0 - w1;

                // Inside test: all barycentric coords must be non-negative
                if w0 < 0.0 || w1 < 0.0 || w2 < 0.0 { continue; }

                // Z-buffer depth test
                if z_enabled {
                    let z = (w0 * v0.z + w1 * v1.z + w2 * v2.z).clamp(0.0, 0x7FFF as f32) as u16;
                    let z_offset = z_addr + ((y * width + x) as usize) * 2;
                    if z_offset + 1 < rdram.len() {
                        let old_z = u16::from_be_bytes([rdram[z_offset], rdram[z_offset + 1]]);
                        // Z compare: lower Z = closer to camera
                        if z_cmp && z > old_z {
                            continue; // Fail depth test — farther than existing pixel
                        }
                        if z_upd {
                            let z_bytes = z.to_be_bytes();
                            rdram[z_offset] = z_bytes[0];
                            rdram[z_offset + 1] = z_bytes[1];
                        }
                    }
                }

                // Interpolate vertex color
                let r = (w0 * v0.r as f32 + w1 * v1.r as f32 + w2 * v2.r as f32)
                    .clamp(0.0, 255.0) as u8;
                let g = (w0 * v0.g as f32 + w1 * v1.g as f32 + w2 * v2.g as f32)
                    .clamp(0.0, 255.0) as u8;
                let b = (w0 * v0.b as f32 + w1 * v1.b as f32 + w2 * v2.b as f32)
                    .clamp(0.0, 255.0) as u8;
                let a = (w0 * v0.a as f32 + w1 * v1.a as f32 + w2 * v2.a as f32)
                    .clamp(0.0, 255.0) as u8;

                // Sample texture if enabled, otherwise texel0 = zero
                let texel0 = if textured {
                    // Perspective-correct interpolation: interpolate S/W and T/W
                    // (which are linear in screen space), then divide by
                    // interpolated 1/W to recover correct S and T.
                    let inv_w = w0 * v0.w + w1 * v1.w + w2 * v2.w;
                    let (tex_s, tex_t) = if inv_w.abs() > 1e-10 {
                        let s_over_w = w0 * v0.s * v0.w + w1 * v1.s * v1.w + w2 * v2.s * v2.w;
                        let t_over_w = w0 * v0.t * v0.w + w1 * v1.t * v1.w + w2 * v2.t * v2.w;
                        (s_over_w / inv_w, t_over_w / inv_w)
                    } else {
                        (w0 * v0.s + w1 * v1.s + w2 * v2.s,
                         w0 * v0.t + w1 * v1.t + w2 * v2.t)
                    };
                    let scaled_s = (tex_s * self.texture_scale_s as f32 / 65536.0) as i32;
                    let scaled_t = (tex_t * self.texture_scale_t as f32 / 65536.0) as i32;
                    self.sample_texture(tile_idx, scaled_s, scaled_t, cycle)
                } else {
                    [0, 0, 0, 0]
                };
                let shade = [r, g, b, a];
                let color = self.combine_pixel(texel0, shade);

                self.write_pixel(rdram, x, y, color);
            }
        }
    }
}

// ═══════════════════════════════════════════════════════════════
// Helper functions
// ═══════════════════════════════════════════════════════════════

fn unpack_rgba5551(val: u16) -> [u8; 4] {
    let r5 = (val >> 11) & 0x1F;
    let g5 = (val >> 6) & 0x1F;
    let b5 = (val >> 1) & 0x1F;
    let a = if val & 1 != 0 { 0xFF } else { 0x00 };
    [
        ((r5 << 3) | (r5 >> 2)) as u8,
        ((g5 << 3) | (g5 >> 2)) as u8,
        ((b5 << 3) | (b5 >> 2)) as u8,
        a,
    ]
}

fn pack_rgba5551(r: u8, g: u8, b: u8, a: u8) -> u16 {
    let r5 = (r >> 3) as u16;
    let g5 = (g >> 3) as u16;
    let b5 = (b >> 3) as u16;
    let a1 = if a >= 128 { 1u16 } else { 0 };
    (r5 << 11) | (g5 << 6) | (b5 << 1) | a1
}

fn identity_matrix() -> [[f32; 4]; 4] {
    [
        [1.0, 0.0, 0.0, 0.0],
        [0.0, 1.0, 0.0, 0.0],
        [0.0, 0.0, 1.0, 0.0],
        [0.0, 0.0, 0.0, 1.0],
    ]
}

fn mat4_mul(a: &[[f32; 4]; 4], b: &[[f32; 4]; 4]) -> [[f32; 4]; 4] {
    let mut result = [[0.0f32; 4]; 4];
    for i in 0..4 {
        for j in 0..4 {
            for k in 0..4 {
                result[i][j] += a[i][k] * b[k][j];
            }
        }
    }
    result
}

/// Compute v * M (row-vector × matrix), matching N64 RSP convention.
fn mat4_mul_vec(m: &[[f32; 4]; 4], v: [f32; 4]) -> [f32; 4] {
    [
        v[0]*m[0][0] + v[1]*m[1][0] + v[2]*m[2][0] + v[3]*m[3][0],
        v[0]*m[0][1] + v[1]*m[1][1] + v[2]*m[2][1] + v[3]*m[3][1],
        v[0]*m[0][2] + v[1]*m[1][2] + v[2]*m[2][2] + v[3]*m[3][2],
        v[0]*m[0][3] + v[1]*m[1][3] + v[2]*m[2][3] + v[3]*m[3][3],
    ]
}

/// Load a 4x4 fixed-point matrix from RDRAM (N64 format: 16.16 split layout).
/// First 32 bytes: integer parts, next 32 bytes: fractional parts.
fn load_fixed_point_matrix(rdram: &[u8], addr: usize) -> [[f32; 4]; 4] {
    let mut result = [[0.0f32; 4]; 4];
    for i in 0..4 {
        for j in 0..4 {
            let int_offset = addr + (i * 4 + j) * 2;
            let frac_offset = addr + 32 + (i * 4 + j) * 2;
            if int_offset + 1 < rdram.len() && frac_offset + 1 < rdram.len() {
                let int_part = read_i16(rdram, int_offset) as i32;
                let frac_part = read_u16(rdram, frac_offset) as i32;
                result[i][j] = int_part as f32 + frac_part as f32 / 65536.0;
            }
        }
    }
    result
}

fn read_i16(rdram: &[u8], offset: usize) -> i16 {
    let off = offset & (rdram.len() - 1);
    if off + 1 < rdram.len() {
        i16::from_be_bytes([rdram[off], rdram[off + 1]])
    } else {
        0
    }
}

fn read_u16(rdram: &[u8], offset: usize) -> u16 {
    let off = offset & (rdram.len() - 1);
    if off + 1 < rdram.len() {
        u16::from_be_bytes([rdram[off], rdram[off + 1]])
    } else {
        0
    }
}

/// Edge function for triangle rasterization.
///
/// Computes the signed area of the parallelogram formed by vectors
/// (v1 - v0) and (p - v0). This is the fundamental building block
/// of the half-space rasterizer:
///   - When evaluated at all 3 edges, the signs tell us if p is inside
///   - The values give us barycentric coordinates (after dividing by total area)
///   - For a CCW triangle, positive means p is to the left of edge v0→v1
fn edge_function(v0x: f32, v0y: f32, v1x: f32, v1y: f32, px: f32, py: f32) -> f32 {
    (v1x - v0x) * (py - v0y) - (v1y - v0y) * (px - v0x)
}
