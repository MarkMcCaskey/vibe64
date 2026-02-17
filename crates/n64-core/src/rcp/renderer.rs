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

    // ─── RDP half-word storage (for texture rect) ───
    pub rdp_half: [u32; 2],

    // ─── Debug counters ───
    pub fill_rect_count: u32,
    pub tex_rect_count: u32,
    pub tex_rect_skip: u32,
    pub tri_count: u32,
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
            rdp_half: [0; 2],
            fill_rect_count: 0,
            tex_rect_count: 0,
            tex_rect_skip: 0,
            tri_count: 0,
        }
    }

    /// Current cycle type from othermode_H bits 52-53.
    fn cycle_type(&self) -> u8 {
        ((self.othermode_h >> 20) & 0x3) as u8
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
        self.color_image_addr = virt_to_phys(w1);
        log::trace!("SetColorImage: addr={:#X} width={} size={}",
            self.color_image_addr, self.color_image_width, self.color_image_size);
    }

    /// G_SETZIMG: Set the depth buffer destination.
    pub fn cmd_set_z_image(&mut self, _w0: u32, w1: u32) {
        self.z_image_addr = virt_to_phys(w1);
    }

    /// G_SETTIMG: Set the texture source address.
    pub fn cmd_set_texture_image(&mut self, w0: u32, w1: u32) {
        self.texture_image_format = ((w0 >> 21) & 0x7) as u8;
        self.texture_image_size = ((w0 >> 19) & 0x3) as u8;
        self.texture_image_width = (w0 & 0xFFF) + 1;
        self.texture_image_addr = virt_to_phys(w1);
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

        for y in y0..y1 {
            for x in x0..x1 {
                // Compute texture coordinates
                let dx = x - (ulx >> 2);
                let dy = y - (uly >> 2);

                let (tex_s, tex_t) = if flip {
                    (s + dy * dsdx, t + dx * dtdy)
                } else {
                    (s + dx * dsdx, t + dy * dtdy)
                };

                // Sample texture
                let color = self.sample_texture(tile_idx, tex_s, tex_t, cycle);

                // Write pixel
                match self.color_image_size {
                    2 => {
                        let offset = addr + ((y * width + x) as usize) * 2;
                        if offset + 1 < rdram.len() {
                            let pixel = pack_rgba5551(color[0], color[1], color[2], color[3]);
                            let bytes = pixel.to_be_bytes();
                            rdram[offset] = bytes[0];
                            rdram[offset + 1] = bytes[1];
                        }
                    }
                    3 => {
                        let offset = addr + ((y * width + x) as usize) * 4;
                        if offset + 3 < rdram.len() {
                            rdram[offset] = color[0];
                            rdram[offset + 1] = color[1];
                            rdram[offset + 2] = color[2];
                            rdram[offset + 3] = color[3];
                        }
                    }
                    _ => {}
                }
            }
        }

        self.tex_rect_count += 1;
    }

    /// Sample a texel from TMEM at the given S/T coordinates.
    /// S and T are in 10.5 fixed-point.
    fn sample_texture(&self, tile_idx: usize, s: i32, t: i32, _cycle: u8) -> [u8; 4] {
        let tile = &self.tiles[tile_idx];

        // Convert S10.5 to integer texel coordinates
        let si = (s >> 5) as i32;
        let ti = (t >> 5) as i32;

        // Apply wrapping/clamping
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
                        // CI4: look up in TLUT
                        self.lookup_tlut(tile.palette as usize * 16 + nibble as usize)
                    } else {
                        // I4: expand to 8-bit intensity
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
    // Geometry Commands (stubs for now)
    // ═══════════════════════════════════════════════════════════════

    /// G_VTX: Load vertices into the vertex buffer.
    pub fn cmd_vertex(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let num = ((w0 >> 12) & 0xFF) as usize;
        let start = (((w0 >> 1) & 0x7F) as usize).saturating_sub(num);
        let addr = virt_to_phys(w1) as usize;

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
            let r = rdram[(off + 12) & (rdram.len() - 1)];
            let g = rdram[(off + 13) & (rdram.len() - 1)];
            let b = rdram[(off + 14) & (rdram.len() - 1)];
            let a = rdram[(off + 15) & (rdram.len() - 1)];

            // Transform vertex by MVP matrix
            let [tx, ty, tz, tw] = mat4_mul_vec(&self.mvp, [x, y, z, 1.0]);

            self.vertex_buffer[idx] = Vertex {
                x: tx, y: ty, z: tz, w: tw,
                s, t, r, g, b, a,
            };
        }
    }

    /// G_TRI1: Draw one triangle.
    pub fn cmd_tri1(&mut self, w0: u32, _w1: u32) {
        let v0 = ((w0 >> 16) & 0xFF) as usize / 2;
        let v1 = ((w0 >> 8) & 0xFF) as usize / 2;
        let v2 = (w0 & 0xFF) as usize / 2;
        if v0 < 32 && v1 < 32 && v2 < 32 {
            self.tri_count += 1;
            // TODO: rasterize triangle
        }
    }

    /// G_TRI2: Draw two triangles.
    pub fn cmd_tri2(&mut self, w0: u32, w1: u32) {
        let v0 = ((w0 >> 16) & 0xFF) as usize / 2;
        let v1 = ((w0 >> 8) & 0xFF) as usize / 2;
        let v2 = (w0 & 0xFF) as usize / 2;
        let v3 = ((w1 >> 16) & 0xFF) as usize / 2;
        let v4 = ((w1 >> 8) & 0xFF) as usize / 2;
        let v5 = (w1 & 0xFF) as usize / 2;
        if v0 < 32 && v1 < 32 && v2 < 32 { self.tri_count += 1; }
        if v3 < 32 && v4 < 32 && v5 < 32 { self.tri_count += 1; }
        // TODO: rasterize triangles
    }

    /// G_MTX: Load a 4x4 matrix from RDRAM.
    pub fn cmd_load_matrix(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let addr = virt_to_phys(w1) as usize;
        let params = w0 & 0xFF;
        let push = params & 0x01 == 0;
        let load = params & 0x02 != 0; // load vs multiply
        let projection = params & 0x04 == 0;

        let mat = load_fixed_point_matrix(rdram, addr);

        if projection {
            self.projection = if load { mat } else { mat4_mul(&mat, &self.projection) };
        } else {
            if push {
                self.matrix_stack.push(self.modelview);
            }
            self.modelview = if load { mat } else { mat4_mul(&mat, &self.modelview) };
        }

        self.mvp = mat4_mul(&self.projection, &self.modelview);
    }

    /// G_POPMTX: Pop the modelview matrix stack.
    pub fn cmd_pop_matrix(&mut self, _w0: u32, w1: u32) {
        let count = (w1 / 64).max(1);
        for _ in 0..count {
            if let Some(mat) = self.matrix_stack.pop() {
                self.modelview = mat;
            }
        }
        self.mvp = mat4_mul(&self.projection, &self.modelview);
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
}

// ═══════════════════════════════════════════════════════════════
// Helper functions
// ═══════════════════════════════════════════════════════════════

fn virt_to_phys(addr: u32) -> u32 {
    match addr {
        0x8000_0000..=0x9FFF_FFFF => addr - 0x8000_0000,
        0xA000_0000..=0xBFFF_FFFF => addr - 0xA000_0000,
        _ => addr & 0x00FF_FFFF,
    }
}

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

fn mat4_mul_vec(m: &[[f32; 4]; 4], v: [f32; 4]) -> [f32; 4] {
    [
        m[0][0]*v[0] + m[0][1]*v[1] + m[0][2]*v[2] + m[0][3]*v[3],
        m[1][0]*v[0] + m[1][1]*v[1] + m[1][2]*v[2] + m[1][3]*v[3],
        m[2][0]*v[0] + m[2][1]*v[1] + m[2][2]*v[2] + m[2][3]*v[3],
        m[3][0]*v[0] + m[3][1]*v[1] + m[3][2]*v[2] + m[3][3]*v[3],
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
