/// Save State System — snapshot and restore full emulator state.
///
/// Binary format (little-endian):
///   [0..4]   Magic: "N64S"
///   [4..8]   Version: u32
///   [8..12]  ROM CRC: u32 (from cart header, to prevent loading wrong ROM's state)
///   [12..16] Data size: u32 (total bytes after header)
///   [16..]   State data (CPU, memory, peripherals, renderer)
///
/// Total size: ~8.05 MB (dominated by 8 MB RDRAM).
use std::io;
use std::path::Path;

const MAGIC: &[u8; 4] = b"N64S";
const VERSION: u32 = 1;
const HEADER_SIZE: usize = 16;

// ─── Binary Writer ───────────────────────────────────────────

struct StateWriter {
    buf: Vec<u8>,
}

impl StateWriter {
    fn new() -> Self {
        Self {
            buf: Vec::with_capacity(8 * 1024 * 1024 + 65536),
        }
    }

    fn write_u8(&mut self, v: u8) {
        self.buf.push(v);
    }
    fn write_u16(&mut self, v: u16) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_i16(&mut self, v: i16) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_u32(&mut self, v: u32) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_u64(&mut self, v: u64) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_i8(&mut self, v: i8) {
        self.buf.push(v as u8);
    }
    fn write_f32(&mut self, v: f32) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }
    fn write_bytes(&mut self, data: &[u8]) {
        self.buf.extend_from_slice(data);
    }
    fn write_bool(&mut self, v: bool) {
        self.buf.push(if v { 1 } else { 0 });
    }

    fn into_bytes(self) -> Vec<u8> {
        self.buf
    }
}

// ─── Binary Reader ───────────────────────────────────────────

struct StateReader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> StateReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    fn read_u8(&mut self) -> io::Result<u8> {
        if self.pos >= self.data.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "truncated"));
        }
        let v = self.data[self.pos];
        self.pos += 1;
        Ok(v)
    }

    fn read_u16(&mut self) -> io::Result<u16> {
        if self.pos + 2 > self.data.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "truncated"));
        }
        let v = u16::from_le_bytes([self.data[self.pos], self.data[self.pos + 1]]);
        self.pos += 2;
        Ok(v)
    }

    fn read_i16(&mut self) -> io::Result<i16> {
        Ok(self.read_u16()? as i16)
    }

    fn read_u32(&mut self) -> io::Result<u32> {
        if self.pos + 4 > self.data.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "truncated"));
        }
        let v = u32::from_le_bytes(self.data[self.pos..self.pos + 4].try_into().unwrap());
        self.pos += 4;
        Ok(v)
    }

    fn read_u64(&mut self) -> io::Result<u64> {
        if self.pos + 8 > self.data.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "truncated"));
        }
        let v = u64::from_le_bytes(self.data[self.pos..self.pos + 8].try_into().unwrap());
        self.pos += 8;
        Ok(v)
    }

    fn read_i8(&mut self) -> io::Result<i8> {
        Ok(self.read_u8()? as i8)
    }

    fn read_f32(&mut self) -> io::Result<f32> {
        Ok(f32::from_le_bytes(self.read_u32()?.to_le_bytes()))
    }

    fn read_bool(&mut self) -> io::Result<bool> {
        Ok(self.read_u8()? != 0)
    }

    fn read_bytes(&mut self, len: usize) -> io::Result<&'a [u8]> {
        if self.pos + len > self.data.len() {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "truncated"));
        }
        let slice = &self.data[self.pos..self.pos + len];
        self.pos += len;
        Ok(slice)
    }
}

// ─── Capture ─────────────────────────────────────────────────

/// Capture the full emulator state into a byte buffer.
pub fn capture(n64: &crate::N64) -> Vec<u8> {
    let mut w = StateWriter::new();

    // ── CPU ──
    for &r in &n64.cpu.gpr {
        w.write_u64(r);
    }
    w.write_u64(n64.cpu.pc);
    w.write_u64(n64.cpu.next_pc);
    w.write_u64(n64.cpu.hi);
    w.write_u64(n64.cpu.lo);
    w.write_bool(n64.cpu.in_delay_slot);
    w.write_bool(n64.cpu.ll_bit);

    // ── COP0 ──
    for &r in &n64.cpu.cop0.regs {
        w.write_u64(r);
    }

    // ── COP1 ──
    for &r in &n64.cpu.cop1.fpr {
        w.write_u64(r);
    }
    w.write_u32(n64.cpu.cop1.fcr31);

    // ── TLB ──
    for entry in &n64.cpu.tlb.entries {
        w.write_u32(entry.page_mask);
        w.write_u64(entry.entry_hi);
        w.write_u64(entry.entry_lo0);
        w.write_u64(entry.entry_lo1);
    }

    // ── N64 cycle counter ──
    w.write_u64(n64.cycles);

    // ── RDRAM (8 MB) ──
    w.write_bytes(n64.bus.rdram.data());

    // ── PIF ──
    w.write_bytes(&n64.bus.pif.ram);
    // EEPROM: write length then data
    w.write_u32(n64.bus.pif.eeprom.len() as u32);
    w.write_bytes(&n64.bus.pif.eeprom);
    w.write_bool(n64.bus.pif.eeprom_dirty);
    // Controller state
    w.write_u16(n64.bus.pif.controller.buttons);
    w.write_i8(n64.bus.pif.controller.stick_x);
    w.write_i8(n64.bus.pif.controller.stick_y);

    // ── RSP ──
    w.write_bytes(&n64.bus.rsp.dmem);
    w.write_bytes(&n64.bus.rsp.imem);
    w.write_u32(n64.bus.rsp.status);
    w.write_u32(n64.bus.rsp.pc);
    w.write_u32(n64.bus.rsp.semaphore.get());
    w.write_u32(n64.bus.rsp.dma_mem_addr);
    w.write_u32(n64.bus.rsp.dma_dram_addr);
    w.write_u32(n64.bus.rsp.dma_len);
    w.write_u32(n64.bus.rsp.dma_count);
    w.write_u32(n64.bus.rsp.dma_skip);

    // ── MI ──
    w.write_u32(n64.bus.mi.mode);
    w.write_u8(n64.bus.mi.intr);
    w.write_u8(n64.bus.mi.intr_mask);

    // ── VI ──
    w.write_u32(n64.bus.vi.ctrl);
    w.write_u32(n64.bus.vi.origin);
    w.write_u32(n64.bus.vi.width);
    w.write_u32(n64.bus.vi.v_intr);
    w.write_u32(n64.bus.vi.v_current);
    w.write_u32(n64.bus.vi.burst);
    w.write_u32(n64.bus.vi.v_sync);
    w.write_u32(n64.bus.vi.h_sync);
    w.write_u32(n64.bus.vi.h_sync_leap);
    w.write_u32(n64.bus.vi.h_video);
    w.write_u32(n64.bus.vi.v_video);
    w.write_u32(n64.bus.vi.v_burst);
    w.write_u32(n64.bus.vi.x_scale);
    w.write_u32(n64.bus.vi.y_scale);

    // ── AI ──
    w.write_u32(n64.bus.ai.dram_addr);
    w.write_u32(n64.bus.ai.len);
    w.write_u32(n64.bus.ai.control);
    w.write_u32(n64.bus.ai.dacrate);
    w.write_u32(n64.bus.ai.bitrate);
    w.write_u64(n64.bus.ai.dma_cycles);

    // ── PI ──
    w.write_u32(n64.bus.pi.dram_addr);
    w.write_u32(n64.bus.pi.cart_addr);
    w.write_u32(n64.bus.pi.pending_dma_len);
    w.write_u64(n64.bus.pi.dma_busy_cycles);
    w.write_u32(n64.bus.pi.dom1_lat);
    w.write_u32(n64.bus.pi.dom1_pwd);
    w.write_u32(n64.bus.pi.dom1_pgs);
    w.write_u32(n64.bus.pi.dom1_rls);
    w.write_u32(n64.bus.pi.dom2_lat);
    w.write_u32(n64.bus.pi.dom2_pwd);
    w.write_u32(n64.bus.pi.dom2_pgs);
    w.write_u32(n64.bus.pi.dom2_rls);

    // ── SI ──
    w.write_u32(n64.bus.si.dram_addr);
    w.write_u32(n64.bus.si.status);
    w.write_u64(n64.bus.si.dma_busy_cycles);

    // ── RI ──
    w.write_u32(n64.bus.ri.mode);
    w.write_u32(n64.bus.ri.config);
    w.write_u32(n64.bus.ri.current_load);
    w.write_u32(n64.bus.ri.select);
    w.write_u32(n64.bus.ri.refresh);
    w.write_u32(n64.bus.ri.latency);

    // ── RDP ──
    w.write_u32(n64.bus.rdp.start);
    w.write_u32(n64.bus.rdp.end);
    w.write_u32(n64.bus.rdp.current);
    w.write_u32(n64.bus.rdp.status);

    // ── Renderer ──
    write_renderer(&mut w, &n64.bus.renderer);

    // ── Audio HLE ──
    write_audio_hle(&mut w, &n64.bus.audio_hle);

    // Build final file with header
    let data = w.into_bytes();
    let rom_crc = n64.bus.cart.crc();

    let mut file = Vec::with_capacity(HEADER_SIZE + data.len());
    file.extend_from_slice(MAGIC);
    file.extend_from_slice(&VERSION.to_le_bytes());
    file.extend_from_slice(&rom_crc.to_le_bytes());
    file.extend_from_slice(&(data.len() as u32).to_le_bytes());
    file.extend_from_slice(&data);
    file
}

fn write_renderer(w: &mut StateWriter, r: &crate::rcp::renderer::Renderer) {
    w.write_u32(r.color_image_addr);
    w.write_u32(r.color_image_width);
    w.write_u8(r.color_image_format);
    w.write_u8(r.color_image_size);
    w.write_u32(r.z_image_addr);
    w.write_u32(r.texture_image_addr);
    w.write_u32(r.texture_image_width);
    w.write_u8(r.texture_image_format);
    w.write_u8(r.texture_image_size);
    w.write_u32(r.fill_color);
    w.write_u16(r.scissor_ulx);
    w.write_u16(r.scissor_uly);
    w.write_u16(r.scissor_lrx);
    w.write_u16(r.scissor_lry);
    w.write_u32(r.othermode_h);
    w.write_u32(r.othermode_l);
    w.write_bytes(&r.env_color);
    w.write_bytes(&r.prim_color);
    w.write_bytes(&r.blend_color);
    w.write_bytes(&r.fog_color);
    w.write_u8(r.prim_lod_frac);
    w.write_u8(r.prim_min_level);
    w.write_u32(r.combine_hi);
    w.write_u32(r.combine_lo);
    for &seg in &r.segment_table {
        w.write_u32(seg);
    }
    w.write_i16(r.fog_multiplier);
    w.write_i16(r.fog_offset);
    w.write_u8(r.num_dir_lights);
    for color in &r.light_colors {
        w.write_bytes(color);
    }
    for dir in &r.light_dirs {
        for &d in dir {
            w.write_i8(d);
        }
    }
    for look in &r.lookat {
        for &f in look {
            w.write_f32(f);
        }
    }
    w.write_bytes(&r.tmem);
    for tile in &r.tiles {
        w.write_u8(tile.format);
        w.write_u8(tile.size);
        w.write_u16(tile.line);
        w.write_u16(tile.tmem);
        w.write_u8(tile.palette);
        w.write_u8(tile.cm_s);
        w.write_u8(tile.mask_s);
        w.write_u8(tile.shift_s);
        w.write_u8(tile.cm_t);
        w.write_u8(tile.mask_t);
        w.write_u8(tile.shift_t);
        w.write_u16(tile.sl);
        w.write_u16(tile.tl);
        w.write_u16(tile.sh);
        w.write_u16(tile.th);
    }
    for vtx in &r.vertex_buffer {
        w.write_f32(vtx.x);
        w.write_f32(vtx.y);
        w.write_f32(vtx.z);
        w.write_f32(vtx.w);
        w.write_f32(vtx.clip_x);
        w.write_f32(vtx.clip_y);
        w.write_f32(vtx.clip_z);
        w.write_f32(vtx.clip_w);
        w.write_f32(vtx.s);
        w.write_f32(vtx.t);
        w.write_u8(vtx.r);
        w.write_u8(vtx.g);
        w.write_u8(vtx.b);
        w.write_u8(vtx.a);
    }
    w.write_u32(r.geometry_mode);
    w.write_bool(r.texture_on);
    w.write_u8(r.texture_tile);
    w.write_u16(r.texture_scale_s);
    w.write_u16(r.texture_scale_t);
    // Matrices (4x4 f32)
    for row in &r.modelview {
        for &f in row {
            w.write_f32(f);
        }
    }
    for row in &r.projection {
        for &f in row {
            w.write_f32(f);
        }
    }
    for row in &r.mvp {
        for &f in row {
            w.write_f32(f);
        }
    }
    // Matrix stack: length-prefixed
    w.write_u32(r.matrix_stack.len() as u32);
    for mat in &r.matrix_stack {
        for row in mat {
            for &f in row {
                w.write_f32(f);
            }
        }
    }
    for &f in &r.viewport_scale {
        w.write_f32(f);
    }
    for &f in &r.viewport_trans {
        w.write_f32(f);
    }
    for &h in &r.rdp_half {
        w.write_u32(h);
    }
}

fn write_audio_hle(w: &mut StateWriter, a: &crate::rcp::audio_hle::AudioHle) {
    w.write_bytes(a.dmem());
    w.write_u16(a.buf_in());
    w.write_u16(a.buf_out());
    w.write_u16(a.buf_count());
    // ADPCM table: length-prefixed
    let table = a.adpcm_table();
    w.write_u32(table.len() as u32);
    for &s in table {
        w.write_i16(s);
    }
    w.write_u32(a.adpcm_loop());
    let env = a.env_values();
    for &v in env {
        w.write_u16(v);
    }
    let steps = a.env_steps();
    for &v in steps {
        w.write_u16(v);
    }
    w.write_u16(a.filter_count());
    w.write_u32(a.filter_lut_addr());
}

// ─── Restore ─────────────────────────────────────────────────

/// Validate save state header before restoring.
/// Rejects incompatible versions and truncated files. Warns on CRC mismatch
/// but allows loading (useful for different ROM regions of the same game).
fn validate_header(
    file_version: u32,
    file_crc: u32,
    current_crc: u32,
    data_len: u32,
    actual_len: usize,
) -> Result<(), String> {
    if file_version > VERSION {
        return Err(format!(
            "Save state version {} is newer than supported ({})",
            file_version, VERSION
        ));
    }
    if (data_len as usize) > actual_len {
        return Err(format!(
            "Save state truncated: header says {} bytes, file has {}",
            data_len, actual_len
        ));
    }
    if file_crc != current_crc {
        log::warn!(
            "Save state CRC mismatch: file={:#010X}, ROM={:#010X} — loading anyway",
            file_crc,
            current_crc
        );
    }
    Ok(())
}

/// Restore emulator state from a save state byte buffer.
/// Returns Ok(()) on success, Err(message) on failure.
pub fn restore(n64: &mut crate::N64, data: &[u8]) -> Result<(), String> {
    if data.len() < HEADER_SIZE {
        return Err("Save state too small for header".to_string());
    }

    // Parse header
    if &data[0..4] != MAGIC {
        return Err("Invalid save state magic".to_string());
    }
    let file_version = u32::from_le_bytes(data[4..8].try_into().unwrap());
    let file_crc = u32::from_le_bytes(data[8..12].try_into().unwrap());
    let data_len = u32::from_le_bytes(data[12..16].try_into().unwrap());
    let current_crc = n64.bus.cart.crc();
    let actual_data = &data[HEADER_SIZE..];

    validate_header(
        file_version,
        file_crc,
        current_crc,
        data_len,
        actual_data.len(),
    )?;

    let mut r = StateReader::new(actual_data);

    // ── CPU ──
    for reg in &mut n64.cpu.gpr {
        *reg = r.read_u64().map_err(|e| e.to_string())?;
    }
    n64.cpu.pc = r.read_u64().map_err(|e| e.to_string())?;
    n64.cpu.next_pc = r.read_u64().map_err(|e| e.to_string())?;
    n64.cpu.hi = r.read_u64().map_err(|e| e.to_string())?;
    n64.cpu.lo = r.read_u64().map_err(|e| e.to_string())?;
    n64.cpu.in_delay_slot = r.read_bool().map_err(|e| e.to_string())?;
    n64.cpu.ll_bit = r.read_bool().map_err(|e| e.to_string())?;

    // ── COP0 ──
    for reg in &mut n64.cpu.cop0.regs {
        *reg = r.read_u64().map_err(|e| e.to_string())?;
    }

    // ── COP1 ──
    for reg in &mut n64.cpu.cop1.fpr {
        *reg = r.read_u64().map_err(|e| e.to_string())?;
    }
    n64.cpu.cop1.fcr31 = r.read_u32().map_err(|e| e.to_string())?;

    // ── TLB ──
    for entry in &mut n64.cpu.tlb.entries {
        entry.page_mask = r.read_u32().map_err(|e| e.to_string())?;
        entry.entry_hi = r.read_u64().map_err(|e| e.to_string())?;
        entry.entry_lo0 = r.read_u64().map_err(|e| e.to_string())?;
        entry.entry_lo1 = r.read_u64().map_err(|e| e.to_string())?;
    }

    // ── N64 cycle counter ──
    n64.cycles = r.read_u64().map_err(|e| e.to_string())?;

    // ── RDRAM ──
    let rdram_data = r
        .read_bytes(n64.bus.rdram.data().len())
        .map_err(|e| e.to_string())?;
    n64.bus.rdram.data_mut().copy_from_slice(rdram_data);

    // ── PIF ──
    let pif_ram = r.read_bytes(64).map_err(|e| e.to_string())?;
    n64.bus.pif.ram.copy_from_slice(pif_ram);
    let eeprom_len = r.read_u32().map_err(|e| e.to_string())? as usize;
    if eeprom_len > 0 {
        let eeprom_data = r.read_bytes(eeprom_len).map_err(|e| e.to_string())?;
        if eeprom_len == n64.bus.pif.eeprom.len() {
            n64.bus.pif.eeprom.copy_from_slice(eeprom_data);
        }
    }
    n64.bus.pif.eeprom_dirty = r.read_bool().map_err(|e| e.to_string())?;
    n64.bus.pif.controller.buttons = r.read_u16().map_err(|e| e.to_string())?;
    n64.bus.pif.controller.stick_x = r.read_i8().map_err(|e| e.to_string())?;
    n64.bus.pif.controller.stick_y = r.read_i8().map_err(|e| e.to_string())?;

    // ── RSP ──
    let dmem = r.read_bytes(4096).map_err(|e| e.to_string())?;
    n64.bus.rsp.dmem.copy_from_slice(dmem);
    let imem = r.read_bytes(4096).map_err(|e| e.to_string())?;
    n64.bus.rsp.imem.copy_from_slice(imem);
    n64.bus.rsp.status = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.rsp.pc = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus
        .rsp
        .semaphore
        .set(r.read_u32().map_err(|e| e.to_string())?);
    n64.bus.rsp.dma_mem_addr = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.rsp.dma_dram_addr = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.rsp.dma_len = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.rsp.dma_count = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.rsp.dma_skip = r.read_u32().map_err(|e| e.to_string())?;

    // ── MI ──
    n64.bus.mi.mode = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.mi.intr = r.read_u8().map_err(|e| e.to_string())?;
    n64.bus.mi.intr_mask = r.read_u8().map_err(|e| e.to_string())?;

    // ── VI ──
    n64.bus.vi.ctrl = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.origin = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.width = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.v_intr = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.v_current = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.burst = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.v_sync = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.h_sync = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.h_sync_leap = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.h_video = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.v_video = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.v_burst = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.x_scale = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.vi.y_scale = r.read_u32().map_err(|e| e.to_string())?;

    // ── AI ──
    n64.bus.ai.dram_addr = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ai.len = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ai.control = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ai.dacrate = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ai.bitrate = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ai.dma_cycles = r.read_u64().map_err(|e| e.to_string())?;

    // ── PI ──
    n64.bus.pi.dram_addr = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.cart_addr = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.pending_dma_len = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.dma_busy_cycles = r.read_u64().map_err(|e| e.to_string())?;
    n64.bus.pi.dom1_lat = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.dom1_pwd = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.dom1_pgs = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.dom1_rls = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.dom2_lat = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.dom2_pwd = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.dom2_pgs = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.pi.dom2_rls = r.read_u32().map_err(|e| e.to_string())?;

    // ── SI ──
    n64.bus.si.dram_addr = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.si.status = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.si.dma_busy_cycles = r.read_u64().map_err(|e| e.to_string())?;

    // ── RI ──
    n64.bus.ri.mode = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ri.config = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ri.current_load = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ri.select = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ri.refresh = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.ri.latency = r.read_u32().map_err(|e| e.to_string())?;

    // ── RDP ──
    n64.bus.rdp.start = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.rdp.end = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.rdp.current = r.read_u32().map_err(|e| e.to_string())?;
    n64.bus.rdp.status = r.read_u32().map_err(|e| e.to_string())?;

    // ── Renderer ──
    read_renderer(&mut r, &mut n64.bus.renderer).map_err(|e| e.to_string())?;

    // ── Audio HLE ──
    read_audio_hle(&mut r, &mut n64.bus.audio_hle).map_err(|e| e.to_string())?;

    // Clear transient state that shouldn't survive restore
    n64.bus.audio_sample_count = 0;
    n64.bus.audio_nonzero_sample_count = 0;
    n64.cpu.tlb_miss = None;
    n64.cpu.reserved_instr = false;

    Ok(())
}

fn read_renderer(r: &mut StateReader, ren: &mut crate::rcp::renderer::Renderer) -> io::Result<()> {
    ren.color_image_addr = r.read_u32()?;
    ren.color_image_width = r.read_u32()?;
    ren.color_image_format = r.read_u8()?;
    ren.color_image_size = r.read_u8()?;
    ren.z_image_addr = r.read_u32()?;
    ren.texture_image_addr = r.read_u32()?;
    ren.texture_image_width = r.read_u32()?;
    ren.texture_image_format = r.read_u8()?;
    ren.texture_image_size = r.read_u8()?;
    ren.fill_color = r.read_u32()?;
    ren.scissor_ulx = r.read_u16()?;
    ren.scissor_uly = r.read_u16()?;
    ren.scissor_lrx = r.read_u16()?;
    ren.scissor_lry = r.read_u16()?;
    ren.othermode_h = r.read_u32()?;
    ren.othermode_l = r.read_u32()?;
    let env = r.read_bytes(4)?;
    ren.env_color.copy_from_slice(env);
    let prim = r.read_bytes(4)?;
    ren.prim_color.copy_from_slice(prim);
    let blend = r.read_bytes(4)?;
    ren.blend_color.copy_from_slice(blend);
    let fog = r.read_bytes(4)?;
    ren.fog_color.copy_from_slice(fog);
    ren.prim_lod_frac = r.read_u8()?;
    ren.prim_min_level = r.read_u8()?;
    ren.combine_hi = r.read_u32()?;
    ren.combine_lo = r.read_u32()?;
    for seg in &mut ren.segment_table {
        *seg = r.read_u32()?;
    }
    ren.fog_multiplier = r.read_i16()?;
    ren.fog_offset = r.read_i16()?;
    ren.num_dir_lights = r.read_u8()?;
    for color in &mut ren.light_colors {
        let bytes = r.read_bytes(3)?;
        color.copy_from_slice(bytes);
    }
    for dir in &mut ren.light_dirs {
        for d in dir {
            *d = r.read_i8()?;
        }
    }
    for look in &mut ren.lookat {
        for f in look {
            *f = r.read_f32()?;
        }
    }
    let tmem = r.read_bytes(4096)?;
    ren.tmem.copy_from_slice(tmem);
    for tile in &mut ren.tiles {
        tile.format = r.read_u8()?;
        tile.size = r.read_u8()?;
        tile.line = r.read_u16()?;
        tile.tmem = r.read_u16()?;
        tile.palette = r.read_u8()?;
        tile.cm_s = r.read_u8()?;
        tile.mask_s = r.read_u8()?;
        tile.shift_s = r.read_u8()?;
        tile.cm_t = r.read_u8()?;
        tile.mask_t = r.read_u8()?;
        tile.shift_t = r.read_u8()?;
        tile.sl = r.read_u16()?;
        tile.tl = r.read_u16()?;
        tile.sh = r.read_u16()?;
        tile.th = r.read_u16()?;
    }
    for vtx in &mut ren.vertex_buffer {
        vtx.x = r.read_f32()?;
        vtx.y = r.read_f32()?;
        vtx.z = r.read_f32()?;
        vtx.w = r.read_f32()?;
        vtx.clip_x = r.read_f32()?;
        vtx.clip_y = r.read_f32()?;
        vtx.clip_z = r.read_f32()?;
        vtx.clip_w = r.read_f32()?;
        vtx.s = r.read_f32()?;
        vtx.t = r.read_f32()?;
        vtx.r = r.read_u8()?;
        vtx.g = r.read_u8()?;
        vtx.b = r.read_u8()?;
        vtx.a = r.read_u8()?;
    }
    ren.geometry_mode = r.read_u32()?;
    ren.texture_on = r.read_bool()?;
    ren.texture_tile = r.read_u8()?;
    ren.texture_scale_s = r.read_u16()?;
    ren.texture_scale_t = r.read_u16()?;
    for row in &mut ren.modelview {
        for f in row {
            *f = r.read_f32()?;
        }
    }
    for row in &mut ren.projection {
        for f in row {
            *f = r.read_f32()?;
        }
    }
    for row in &mut ren.mvp {
        for f in row {
            *f = r.read_f32()?;
        }
    }
    let stack_len = r.read_u32()? as usize;
    ren.matrix_stack.clear();
    for _ in 0..stack_len {
        let mut mat = [[0.0f32; 4]; 4];
        for row in &mut mat {
            for f in row {
                *f = r.read_f32()?;
            }
        }
        ren.matrix_stack.push(mat);
    }
    for f in &mut ren.viewport_scale {
        *f = r.read_f32()?;
    }
    for f in &mut ren.viewport_trans {
        *f = r.read_f32()?;
    }
    for h in &mut ren.rdp_half {
        *h = r.read_u32()?;
    }
    Ok(())
}

fn read_audio_hle(r: &mut StateReader, a: &mut crate::rcp::audio_hle::AudioHle) -> io::Result<()> {
    let dmem = r.read_bytes(a.dmem().len())?;
    a.set_dmem(dmem);
    a.set_buf_in(r.read_u16()?);
    a.set_buf_out(r.read_u16()?);
    a.set_buf_count(r.read_u16()?);
    let table_len = r.read_u32()? as usize;
    let mut table = vec![0i16; table_len];
    for s in &mut table {
        *s = r.read_i16()?;
    }
    a.set_adpcm_table(&table);
    a.set_adpcm_loop(r.read_u32()?);
    let mut env = [0u16; 3];
    for v in &mut env {
        *v = r.read_u16()?;
    }
    a.set_env_values(&env);
    let mut steps = [0u16; 3];
    for v in &mut steps {
        *v = r.read_u16()?;
    }
    a.set_env_steps(&steps);
    a.set_filter_count(r.read_u16()?);
    a.set_filter_lut_addr(r.read_u32()?);
    Ok(())
}

// ─── File I/O ────────────────────────────────────────────────

/// Save state to a file.
pub fn save_to_file(n64: &crate::N64, path: &Path) -> io::Result<()> {
    let data = capture(n64);
    std::fs::write(path, &data)?;
    log::info!(
        "Saved state to {:?} ({:.2} MB)",
        path,
        data.len() as f64 / (1024.0 * 1024.0)
    );
    Ok(())
}

/// Load state from a file.
pub fn load_from_file(n64: &mut crate::N64, path: &Path) -> Result<(), String> {
    let data = std::fs::read(path).map_err(|e| format!("Failed to read {:?}: {}", path, e))?;
    restore(n64, &data)?;
    log::info!("Loaded state from {:?}", path);
    Ok(())
}
