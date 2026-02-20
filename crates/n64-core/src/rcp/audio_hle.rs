/// Audio HLE — High-Level Emulation of the N64 audio RSP microcode.
///
/// OoT uses the "nead" ABI variant (n_aspMain), with this command table:
///   0x00: NOOP       0x01: ADPCM      0x02: CLEARBUFF   0x03: UNKNOWN
///   0x04: ADDMIXER   0x05: RESAMPLE   0x06: RESAMPLE_ZOH 0x07: FILTER
///   0x08: SETBUFF    0x09: DUPLICATE  0x0A: DMEMMOVE    0x0B: LOADADPCM
///   0x0C: MIXER      0x0D: INTERLEAVE 0x0E: HILOGAIN    0x0F: SETLOOP
///   0x10: NEAD_16    0x11: INTERL     0x12: ENVSETUP1   0x13: ENVMIXER
///   0x14: LOADBUFF   0x15: SAVEBUFF   0x16: ENVSETUP2   0x17: UNKNOWN

const DMEM_SIZE: usize = 0x2000; // 8KB working buffer (addresses can exceed 0x1000)

/// Persistent audio HLE state across tasks.
pub struct AudioHle {
    /// Working buffer simulating DMEM for audio processing.
    dmem: Vec<u8>,

    // SETBUFF state (set by cmd 0x08, used by ADPCM/RESAMPLE)
    buf_in: u16,
    buf_out: u16,
    buf_count: u16,

    // ADPCM state
    adpcm_table: Vec<i16>, // codebook (up to 128 entries)
    adpcm_loop: u32,       // loop point address in RDRAM

    // Envelope state (set by ENVSETUP1/ENVSETUP2, used by ENVMIXER)
    env_values: [u16; 3], // [0]=left vol, [1]=right vol, [2]=dry/wet mix
    env_steps: [u16; 3],  // ramp steps per 8-sample group

    // FILTER state
    filter_count: u16,
    filter_lut_addr: u32,
}

impl AudioHle {
    pub fn new() -> Self {
        Self {
            dmem: vec![0u8; DMEM_SIZE],
            buf_in: 0,
            buf_out: 0,
            buf_count: 0,
            adpcm_table: vec![0i16; 256],
            adpcm_loop: 0,
            env_values: [0; 3],
            env_steps: [0; 3],
            filter_count: 0,
            filter_lut_addr: 0,
        }
    }

    /// Process an audio command list from RDRAM (nead OoT ABI).
    pub fn process_audio_list(&mut self, rdram: &mut [u8], data_ptr: u32, data_size: u32) {
        let base = data_ptr as usize;
        let num_cmds = data_size as usize / 8;

        for i in 0..num_cmds {
            let off = base + i * 8;
            if off + 7 >= rdram.len() { break; }

            let w0 = u32::from_be_bytes([rdram[off], rdram[off+1], rdram[off+2], rdram[off+3]]);
            let w1 = u32::from_be_bytes([rdram[off+4], rdram[off+5], rdram[off+6], rdram[off+7]]);
            let cmd = (w0 >> 24) & 0x1F;

            match cmd {
                0x00 => {} // NOOP
                0x01 => self.cmd_adpcm(w0, w1, rdram),
                0x02 => self.cmd_clearbuff(w0, w1),
                0x04 => self.cmd_addmixer(w0, w1),
                0x05 => self.cmd_resample(w0, w1, rdram),
                0x07 => self.cmd_filter(w0, w1, rdram),
                0x08 => self.cmd_setbuff(w0, w1),
                0x09 => self.cmd_duplicate(w0, w1),
                0x0A => self.cmd_dmemmove(w0, w1),
                0x0B => self.cmd_loadadpcm(w0, w1, rdram),
                0x0C => self.cmd_mixer(w0, w1),
                0x0D => self.cmd_interleave(w0, w1),
                0x0F => self.cmd_setloop(w0, w1),
                0x12 => self.cmd_envsetup1(w0, w1),
                0x13 => self.cmd_envmixer(w0, w1),
                0x14 => self.cmd_loadbuff(w0, w1, rdram),
                0x15 => self.cmd_savebuff(w0, w1, rdram),
                0x16 => self.cmd_envsetup2(w0, w1),
                _ => {} // UNKNOWN / NOOP / HILOGAIN / RESAMPLE_ZOH / etc
            }
        }
    }

    // ─── DMEM helpers ───

    fn read_i16(&self, addr: u16) -> i16 {
        let a = (addr as usize) & (DMEM_SIZE - 2);
        i16::from_be_bytes([self.dmem[a], self.dmem[a + 1]])
    }

    fn write_i16(&mut self, addr: u16, val: i16) {
        let a = (addr as usize) & (DMEM_SIZE - 2);
        let b = val.to_be_bytes();
        self.dmem[a] = b[0];
        self.dmem[a + 1] = b[1];
    }

    fn read_u8_dmem(&self, addr: u16) -> u8 {
        self.dmem[(addr as usize) & (DMEM_SIZE - 1)]
    }

    fn resolve_addr(addr: u32) -> usize {
        let phys = match addr {
            0x8000_0000..=0x9FFF_FFFF => addr - 0x8000_0000,
            0xA000_0000..=0xBFFF_FFFF => addr - 0xA000_0000,
            _ => addr & 0x00FF_FFFF,
        };
        phys as usize
    }

    // ─── Nead OoT Command Handlers ───

    /// 0x02 CLEARBUFF: Zero a region of DMEM.
    fn cmd_clearbuff(&mut self, w0: u32, w1: u32) {
        let dmem_addr = (w0 & 0xFFF) as usize;
        let count = (w1 & 0xFFF) as usize;
        let end = (dmem_addr + count).min(DMEM_SIZE);
        for i in dmem_addr..end {
            self.dmem[i] = 0;
        }
    }

    /// 0x04 ADDMIXER: Add source buffer into destination.
    fn cmd_addmixer(&mut self, w0: u32, w1: u32) {
        let count = ((w0 >> 12) & 0xFF0) as usize;
        let src = (w1 >> 16) as u16;
        let dst = w1 as u16;
        for i in (0..count).step_by(2) {
            let s = self.read_i16(src.wrapping_add(i as u16)) as i32;
            let d = self.read_i16(dst.wrapping_add(i as u16)) as i32;
            self.write_i16(dst.wrapping_add(i as u16), (s + d).clamp(-32768, 32767) as i16);
        }
    }

    /// 0x08 SETBUFF: Set buffer addresses for ADPCM/RESAMPLE.
    /// Nead format: in = w0[15:0], out = w1[31:16], count = w1[15:0]
    fn cmd_setbuff(&mut self, w0: u32, w1: u32) {
        self.buf_in = w0 as u16;
        self.buf_out = (w1 >> 16) as u16;
        self.buf_count = w1 as u16;
    }

    /// 0x09 DUPLICATE
    fn cmd_duplicate(&mut self, w0: u32, w1: u32) {
        let count = ((w0 >> 12) & 0xFF0) as usize;
        let src = (w1 >> 16) as usize;
        let dst = (w1 & 0xFFFF) as usize;
        for i in 0..count {
            let byte = self.dmem[(src + i) % DMEM_SIZE];
            self.dmem[(dst + i) % DMEM_SIZE] = byte;
        }
    }

    /// 0x0A DMEMMOVE: Move data within DMEM.
    fn cmd_dmemmove(&mut self, w0: u32, w1: u32) {
        let src = (w0 & 0xFFFF) as usize;
        let dst = ((w1 >> 16) & 0xFFFF) as usize;
        let count = ((w1 & 0xFFFF) as usize + 3) & !3; // align to 4
        let mut tmp = vec![0u8; count];
        for i in 0..count {
            tmp[i] = self.dmem[(src + i) % DMEM_SIZE];
        }
        for i in 0..count {
            self.dmem[(dst + i) % DMEM_SIZE] = tmp[i];
        }
    }

    /// 0x0B LOADADPCM: Load ADPCM codebook from RDRAM.
    fn cmd_loadadpcm(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let count = ((w0 >> 16) & 0xFF) as usize;
        let addr = Self::resolve_addr(w1);
        for i in 0..count.min(self.adpcm_table.len()) {
            let off = addr + i * 2;
            if off + 1 < rdram.len() {
                self.adpcm_table[i] = i16::from_be_bytes([rdram[off], rdram[off + 1]]);
            }
        }
    }

    /// 0x0C MIXER: Mix source into destination with gain.
    fn cmd_mixer(&mut self, w0: u32, w1: u32) {
        let count = ((w0 >> 12) & 0xFF0) as usize;
        let gain = w0 as i16;
        let src = (w1 >> 16) as u16;
        let dst = w1 as u16;
        for i in (0..count).step_by(2) {
            let s = self.read_i16(src.wrapping_add(i as u16)) as i32;
            let d = self.read_i16(dst.wrapping_add(i as u16)) as i32;
            let r = d + ((s * gain as i32) >> 15);
            self.write_i16(dst.wrapping_add(i as u16), r.clamp(-32768, 32767) as i16);
        }
    }

    /// 0x0D INTERLEAVE: Interleave L/R mono to stereo.
    fn cmd_interleave(&mut self, w0: u32, w1: u32) {
        let count = ((w0 >> 12) & 0xFF0) as usize;
        let out = (w0 & 0xFFF) as u16;
        let left = (w1 >> 16) as u16;
        let right = w1 as u16;
        let samples = count / 2;
        let mut l_buf = vec![0i16; samples];
        let mut r_buf = vec![0i16; samples];
        for i in 0..samples {
            l_buf[i] = self.read_i16(left.wrapping_add((i * 2) as u16));
            r_buf[i] = self.read_i16(right.wrapping_add((i * 2) as u16));
        }
        for i in 0..samples {
            self.write_i16(out.wrapping_add((i * 4) as u16), l_buf[i]);
            self.write_i16(out.wrapping_add((i * 4 + 2) as u16), r_buf[i]);
        }
    }

    /// 0x0F SETLOOP: Set ADPCM loop point.
    fn cmd_setloop(&mut self, _w0: u32, w1: u32) {
        self.adpcm_loop = w1;
    }

    /// 0x07 FILTER: Conditional filter setup/execution.
    #[allow(unused_variables)]
    fn cmd_filter(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let flags = ((w0 >> 16) & 0xFF) as u8;
        let address = Self::resolve_addr(w1);

        if flags > 1 {
            // Store filter parameters for later execution
            self.filter_count = w0 as u16;
            self.filter_lut_addr = w1 & 0x00FF_FFFF;
        } else {
            // Execute filter
            let dmem = (w0 & 0xFFFF) as u16;
            let count = self.filter_count as usize;
            if count == 0 { return; }

            // Simplified filter: copy filtered data through RDRAM state
            // (Full filter implementation uses 8-tap FIR from LUT)
            let lut_addr = self.filter_lut_addr as usize;

            // Load 8 filter coefficients from RDRAM LUT
            let mut coefs = [0i16; 8];
            for j in 0..8 {
                let off = lut_addr + j * 2;
                if off + 1 < rdram.len() {
                    coefs[j] = i16::from_be_bytes([rdram[off], rdram[off + 1]]);
                }
            }

            // Apply 8-tap FIR filter
            let mut prev = [0i16; 8];
            // Load previous state from RDRAM
            for j in 0..8 {
                let off = address + j * 2;
                if off + 1 < rdram.len() {
                    prev[j] = i16::from_be_bytes([rdram[off], rdram[off + 1]]);
                }
            }

            let mut output = vec![0i16; count];
            for i in 0..count {
                let input = self.read_i16(dmem.wrapping_add((i * 2) as u16));
                // Simple passthrough for now (full 8-tap FIR would use coefs)
                output[i] = input;
            }

            // Save state to RDRAM (last 8 samples)
            let state_start = if count >= 8 { count - 8 } else { 0 };
            for j in 0..8.min(count) {
                let sample = output[state_start + j];
                let off = address + j * 2;
                if off + 1 < rdram.len() {
                    let bytes = sample.to_be_bytes();
                    rdram[off] = bytes[0];
                    rdram[off + 1] = bytes[1];
                }
            }

            // Write output back to DMEM
            for i in 0..count {
                self.write_i16(dmem.wrapping_add((i * 2) as u16), output[i]);
            }
        }
    }

    /// 0x12 ENVSETUP1: Set envelope values and steps.
    /// Nead format: env_values[2] = (w0>>8)&0xFF00, env_steps from w0 and w1.
    fn cmd_envsetup1(&mut self, w0: u32, w1: u32) {
        self.env_values[2] = ((w0 >> 8) & 0xFF00) as u16;
        self.env_steps[2] = w0 as u16;
        self.env_steps[0] = (w1 >> 16) as u16;
        self.env_steps[1] = w1 as u16;
    }

    /// 0x16 ENVSETUP2: Set L/R volume levels.
    /// Nead format: env_values[0] = w1[31:16], env_values[1] = w1[15:0].
    fn cmd_envsetup2(&mut self, _w0: u32, w1: u32) {
        self.env_values[0] = (w1 >> 16) as u16;
        self.env_values[1] = w1 as u16;
    }

    /// 0x13 ENVMIXER: Apply volume envelope and mix into 4 output buffers.
    /// Nead format: input DMEM addr and output buffer addrs encoded in w0/w1.
    fn cmd_envmixer(&mut self, w0: u32, w1: u32) {
        let dmemi = ((w0 >> 12) & 0xFF0) as u16;
        let count = ((w0 >> 8) & 0xFF) as usize;

        let _swap_wet_lr = ((w0 >> 4) & 1) != 0;
        let dmem_dl = ((w1 >> 20) & 0xFF0) as u16; // dry left
        let dmem_dr = ((w1 >> 12) & 0xFF0) as u16; // dry right
        let dmem_wl = ((w1 >> 4) & 0xFF0) as u16;  // wet left
        let dmem_wr = ((w1 << 4) & 0xFF0) as u16;  // wet right

        // XOR masks for phase inversion (from w0 low nibble)
        let xor_wl: i16 = if w0 & 0x8 != 0 { -1 } else { 0 };
        let xor_wr: i16 = if w0 & 0x4 != 0 { -1 } else { 0 };
        let xor_dl: i16 = if w0 & 0x2 != 0 { -1 } else { 0 };
        let xor_dr: i16 = if w0 & 0x1 != 0 { -1 } else { 0 };

        // Process count samples in groups of 8
        let count = (count + 7) & !7; // align to 8
        let mut in_off = 0u16;
        let mut dl_off = 0u16;
        let mut dr_off = 0u16;
        let mut wl_off = 0u16;
        let mut wr_off = 0u16;

        // Workaround: when L/R volumes are 0 but dry/wet is set, use dry/wet as
        // the main volume. This handles the common OoT case where ENVSETUP2
        // doesn't set per-channel volumes.
        let vol_l = if self.env_values[0] == 0 && self.env_values[2] != 0 {
            self.env_values[2]
        } else {
            self.env_values[0]
        };
        let vol_r = if self.env_values[1] == 0 && self.env_values[2] != 0 {
            self.env_values[2]
        } else {
            self.env_values[1]
        };

        let mut remaining = count;
        while remaining > 0 {
            for _ in 0..8 {
                let s = self.read_i16(dmemi.wrapping_add(in_off)) as i32;

                // Apply L/R volume (env_values are unsigned Q0.16)
                let l = ((s * vol_l as u32 as i32) >> 16) as i16 ^ xor_dl;
                let r = ((s * vol_r as u32 as i32) >> 16) as i16 ^ xor_dr;

                // Apply dry/wet mix
                let l2 = ((l as i32 * self.env_values[2] as u32 as i32) >> 16) as i16 ^ xor_wl;
                let r2 = ((r as i32 * self.env_values[2] as u32 as i32) >> 16) as i16 ^ xor_wr;

                // Accumulate into output buffers
                let dl_cur = self.read_i16(dmem_dl.wrapping_add(dl_off)) as i32;
                self.write_i16(dmem_dl.wrapping_add(dl_off),
                    (dl_cur + l as i32).clamp(-32768, 32767) as i16);

                let dr_cur = self.read_i16(dmem_dr.wrapping_add(dr_off)) as i32;
                self.write_i16(dmem_dr.wrapping_add(dr_off),
                    (dr_cur + r as i32).clamp(-32768, 32767) as i16);

                let wl_cur = self.read_i16(dmem_wl.wrapping_add(wl_off)) as i32;
                self.write_i16(dmem_wl.wrapping_add(wl_off),
                    (wl_cur + l2 as i32).clamp(-32768, 32767) as i16);

                let wr_cur = self.read_i16(dmem_wr.wrapping_add(wr_off)) as i32;
                self.write_i16(dmem_wr.wrapping_add(wr_off),
                    (wr_cur + r2 as i32).clamp(-32768, 32767) as i16);

                in_off += 2;
                dl_off += 2;
                dr_off += 2;
                wl_off += 2;
                wr_off += 2;
            }
            // Ramp volumes per 8-sample group
            self.env_values[0] = self.env_values[0].wrapping_add(self.env_steps[0]);
            self.env_values[1] = self.env_values[1].wrapping_add(self.env_steps[1]);
            self.env_values[2] = self.env_values[2].wrapping_add(self.env_steps[2]);
            remaining = remaining.saturating_sub(8);
        }
    }

    /// 0x14 LOADBUFF: DMA from RDRAM → DMEM.
    fn cmd_loadbuff(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let count = ((w0 >> 12) & 0xFFF) as usize;
        let dmem_addr = (w0 & 0xFFF) as usize;
        let addr = Self::resolve_addr(w1);

        // Enforce DMA alignment
        let dmem_addr = dmem_addr & !3;
        let addr = addr & !7;
        let count = (count + 7) & !7;

        let copy_len = count.min(DMEM_SIZE.saturating_sub(dmem_addr)).min(rdram.len().saturating_sub(addr));
        self.dmem[dmem_addr..dmem_addr + copy_len].copy_from_slice(&rdram[addr..addr + copy_len]);
    }

    /// 0x15 SAVEBUFF: DMA from DMEM → RDRAM.
    fn cmd_savebuff(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let count = ((w0 >> 12) & 0xFFF) as usize;
        let dmem_addr = (w0 & 0xFFF) as usize;
        let addr = Self::resolve_addr(w1);

        // Enforce DMA alignment
        let dmem_addr = dmem_addr & !3;
        let addr = addr & !7;
        let count = (count + 7) & !7;

        let copy_len = count.min(DMEM_SIZE.saturating_sub(dmem_addr)).min(rdram.len().saturating_sub(addr));
        rdram[addr..addr + copy_len].copy_from_slice(&self.dmem[dmem_addr..dmem_addr + copy_len]);
    }

    /// 0x05 RESAMPLE: Resample audio with pitch adjustment.
    /// Nead format: pitch = (w0 & 0xFFFF) << 1, uses SETBUFF state.
    fn cmd_resample(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let flags = ((w0 >> 16) & 0xFF) as u8;
        let pitch = ((w0 & 0xFFFF) as u32) << 1; // Q16.16 fixed-point
        let state_addr = Self::resolve_addr(w1);

        let in_addr = self.buf_in;
        let out_addr = self.buf_out;
        let count = ((self.buf_count as usize) + 0xF) & !0xF; // align to 16

        if count == 0 || pitch == 0 { return; }

        // Convert to sample indices (count is in bytes, 2 bytes per sample)
        let out_samples = count / 2;

        // Input position starts 4 samples before buf_in (for interpolation context)
        let ipos_base = (in_addr >> 1).wrapping_sub(4);

        // Load or reset pitch accumulator
        let mut pitch_accu: u32;
        if flags & 0x01 != 0 {
            // INIT: reset accumulator, zero the 4 context samples
            pitch_accu = 0;
            for j in 0..4 {
                self.write_i16(in_addr.wrapping_sub(8).wrapping_add((j * 2) as u16), 0);
            }
        } else if state_addr + 16 <= rdram.len() {
            // Load saved state: pitch_accu (4 bytes) + 4 previous samples (8 bytes)
            pitch_accu = u32::from_be_bytes([
                rdram[state_addr], rdram[state_addr+1],
                rdram[state_addr+2], rdram[state_addr+3],
            ]);
            // Restore 4 previous samples before in_addr
            for j in 0..4 {
                let s = i16::from_be_bytes([
                    rdram[state_addr + 4 + j*2], rdram[state_addr + 5 + j*2],
                ]);
                self.write_i16(in_addr.wrapping_sub(8).wrapping_add((j * 2) as u16), s);
            }
        } else {
            pitch_accu = 0;
        }

        let mut ipos = ipos_base;
        let mut opos = out_addr;

        for _ in 0..out_samples {
            // Linear interpolation (simplified from Mupen64Plus's 4-tap LUT)
            let frac = (pitch_accu & 0xFFFF) as i32;
            let s0 = self.read_i16((ipos.wrapping_add(2)) as u16 * 2) as i32;
            let s1 = self.read_i16((ipos.wrapping_add(3)) as u16 * 2) as i32;
            let sample = s0 + (((s1 - s0) * frac) >> 16);
            self.write_i16(opos, sample.clamp(-32768, 32767) as i16);

            opos = opos.wrapping_add(2);
            pitch_accu += pitch;
            ipos = ipos.wrapping_add((pitch_accu >> 16) as u16);
            pitch_accu &= 0xFFFF;
        }

        // Save state
        if state_addr + 16 <= rdram.len() {
            let ab = pitch_accu.to_be_bytes();
            rdram[state_addr..state_addr+4].copy_from_slice(&ab);
            // Save current position's 4 samples
            for j in 0..4 {
                let s = self.read_i16(ipos.wrapping_add(j as u16) as u16 * 2);
                let bytes = s.to_be_bytes();
                rdram[state_addr + 4 + j*2] = bytes[0];
                rdram[state_addr + 5 + j*2] = bytes[1];
            }
        }
    }

    // ─── Save state accessors ───

    pub fn dmem(&self) -> &[u8] { &self.dmem }
    pub fn set_dmem(&mut self, data: &[u8]) {
        let len = data.len().min(self.dmem.len());
        self.dmem[..len].copy_from_slice(&data[..len]);
    }
    pub fn buf_in(&self) -> u16 { self.buf_in }
    pub fn set_buf_in(&mut self, v: u16) { self.buf_in = v; }
    pub fn buf_out(&self) -> u16 { self.buf_out }
    pub fn set_buf_out(&mut self, v: u16) { self.buf_out = v; }
    pub fn buf_count(&self) -> u16 { self.buf_count }
    pub fn set_buf_count(&mut self, v: u16) { self.buf_count = v; }
    pub fn adpcm_table(&self) -> &[i16] { &self.adpcm_table }
    pub fn set_adpcm_table(&mut self, data: &[i16]) {
        self.adpcm_table.clear();
        self.adpcm_table.extend_from_slice(data);
    }
    pub fn adpcm_loop(&self) -> u32 { self.adpcm_loop }
    pub fn set_adpcm_loop(&mut self, v: u32) { self.adpcm_loop = v; }
    pub fn env_values(&self) -> &[u16; 3] { &self.env_values }
    pub fn set_env_values(&mut self, v: &[u16; 3]) { self.env_values = *v; }
    pub fn env_steps(&self) -> &[u16; 3] { &self.env_steps }
    pub fn set_env_steps(&mut self, v: &[u16; 3]) { self.env_steps = *v; }
    pub fn filter_count(&self) -> u16 { self.filter_count }
    pub fn set_filter_count(&mut self, v: u16) { self.filter_count = v; }
    pub fn filter_lut_addr(&self) -> u32 { self.filter_lut_addr }
    pub fn set_filter_lut_addr(&mut self, v: u32) { self.filter_lut_addr = v; }

    /// 0x01 ADPCM: Decode ADPCM audio to PCM.
    /// Uses SETBUFF state for in/out/count addresses.
    fn cmd_adpcm(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let flags = ((w0 >> 16) & 0xFF) as u8;
        let state_addr = Self::resolve_addr(w1);

        let dmemi = self.buf_in;
        let dmemo = self.buf_out;
        let count = ((self.buf_count as usize) + 0x1F) & !0x1F; // align to 32

        if count == 0 { return; }

        let do_init = flags & 0x01 != 0;
        let is_loop = flags & 0x02 != 0;

        // Load last frame (16 samples = 32 bytes of state)
        let mut last_frame = [0i16; 16];
        if !do_init {
            let load_addr = if is_loop {
                Self::resolve_addr(self.adpcm_loop)
            } else {
                state_addr
            };
            if load_addr + 32 <= rdram.len() {
                for j in 0..16 {
                    last_frame[j] = i16::from_be_bytes([
                        rdram[load_addr + j*2], rdram[load_addr + j*2 + 1],
                    ]);
                }
            }
        }

        // Write initial state to output (16 samples)
        let mut out_pos = dmemo;
        for j in 0..16 {
            self.write_i16(out_pos, last_frame[j]);
            out_pos = out_pos.wrapping_add(2);
        }

        // Decode ADPCM frames
        let mut in_pos = dmemi;
        let mut remaining = count;

        while remaining > 0 {
            // Read header byte: scale in high nibble, predictor in low nibble
            let code = self.read_u8_dmem(in_pos);
            in_pos = in_pos.wrapping_add(1);
            let scale = ((code & 0xF0) >> 4) as u32;
            let pred_idx = (code & 0x0F) as usize;

            // Get codebook entry (16 coefficients: book1[0..8] and book2[0..8])
            let cb_offset = pred_idx * 16;

            // Decode 16 samples from 8 data bytes (4-bit nibbles)
            let rshift = if scale < 12 { 12 - scale } else { 0 };
            let mut frame = [0i16; 16];

            for byte_i in 0..8 {
                let byte = self.read_u8_dmem(in_pos);
                in_pos = in_pos.wrapping_add(1);

                // High nibble
                let hi = ((byte & 0xF0) as i8 as i16) >> rshift as i16;
                frame[byte_i * 2] = hi;

                // Low nibble
                let lo = (((byte & 0x0F) << 4) as i8 as i16) >> rshift as i16;
                frame[byte_i * 2 + 1] = lo;
            }

            // Apply prediction using codebook (first 8 samples)
            let l1 = last_frame[14];
            let l2 = last_frame[15];
            for i in 0..8 {
                let book1 = if cb_offset + i < self.adpcm_table.len() {
                    self.adpcm_table[cb_offset + i] as i32
                } else { 0 };
                let book2 = if cb_offset + 8 + i < self.adpcm_table.len() {
                    self.adpcm_table[cb_offset + 8 + i] as i32
                } else { 0 };

                let mut accu = (frame[i] as i32) << 11;
                accu += book1 * l1 as i32 + book2 * l2 as i32;
                for j in 0..i {
                    let b2 = if cb_offset + 8 + j < self.adpcm_table.len() {
                        self.adpcm_table[cb_offset + 8 + j] as i32
                    } else { 0 };
                    accu += b2 * last_frame[i - 1 - j] as i32;
                }
                last_frame[i] = (accu >> 11).clamp(-32768, 32767) as i16;
            }

            // Second 8 samples
            let l1_2 = last_frame[6];
            let l2_2 = last_frame[7];
            for i in 0..8 {
                let book1 = if cb_offset + i < self.adpcm_table.len() {
                    self.adpcm_table[cb_offset + i] as i32
                } else { 0 };
                let book2 = if cb_offset + 8 + i < self.adpcm_table.len() {
                    self.adpcm_table[cb_offset + 8 + i] as i32
                } else { 0 };

                let mut accu = (frame[8 + i] as i32) << 11;
                accu += book1 * l1_2 as i32 + book2 * l2_2 as i32;
                for j in 0..i {
                    let b2 = if cb_offset + 8 + j < self.adpcm_table.len() {
                        self.adpcm_table[cb_offset + 8 + j] as i32
                    } else { 0 };
                    accu += b2 * last_frame[8 + i - 1 - j] as i32;
                }
                last_frame[8 + i] = (accu >> 11).clamp(-32768, 32767) as i16;
            }

            // Write decoded frame to output
            for j in 0..16 {
                self.write_i16(out_pos, last_frame[j]);
                out_pos = out_pos.wrapping_add(2);
            }

            remaining = remaining.saturating_sub(32);
        }

        // Save last frame state to RDRAM
        if state_addr + 32 <= rdram.len() {
            for j in 0..16 {
                let bytes = last_frame[j].to_be_bytes();
                rdram[state_addr + j*2] = bytes[0];
                rdram[state_addr + j*2 + 1] = bytes[1];
            }
        }
    }
}
