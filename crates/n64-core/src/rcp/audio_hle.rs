/// Audio HLE — High-Level Emulation of the N64 audio RSP microcode.
///
/// Different games use different microcode variants (ABIs) with varying
/// command tables. This emulator supports:
/// - **Nead**: Zelda OoT, Majora's Mask
/// - **Standard**: Super Mario 64, Star Fox 64, etc.
use std::fs::OpenOptions;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AudioAbi {
    Nead,
    Standard,
}

const DMEM_SIZE: usize = 0x2000; // 8KB working buffer (addresses can exceed 0x1000)

#[derive(Debug, Clone)]
pub struct AudioHleDebugSnapshot {
    pub tasks: u64,
    pub commands: u64,
    pub nead_cmd_counts: [u64; 32],
    pub standard_cmd_counts: [u64; 256],
    pub nead_loadadpcm_hi_zero: u64,
    pub nead_loadadpcm_hi_nonzero: u64,
    pub nead_envsetup1_step_mode: u64,
    pub nead_envsetup1_value_mode: u64,
    pub nead_envsetup2_mono_mode: u64,
    pub nead_envsetup2_stereo_mode: u64,
    pub nead_envmixer_out_mode: u64,
    pub nead_envmixer_inplace_mode: u64,
    pub nead_loadbuff_bytes: u64,
    pub nead_loadbuff_min_addr: u32,
    pub nead_loadbuff_max_addr: u32,
    pub nead_savebuff_bytes: u64,
    pub nead_savebuff_min_addr: u32,
    pub nead_savebuff_max_addr: u32,
    pub dma_truncate_events: u64,
    pub dma_truncate_bytes: u64,
}

impl Default for AudioHleDebugSnapshot {
    fn default() -> Self {
        Self {
            tasks: 0,
            commands: 0,
            nead_cmd_counts: [0; 32],
            standard_cmd_counts: [0; 256],
            nead_loadadpcm_hi_zero: 0,
            nead_loadadpcm_hi_nonzero: 0,
            nead_envsetup1_step_mode: 0,
            nead_envsetup1_value_mode: 0,
            nead_envsetup2_mono_mode: 0,
            nead_envsetup2_stereo_mode: 0,
            nead_envmixer_out_mode: 0,
            nead_envmixer_inplace_mode: 0,
            nead_loadbuff_bytes: 0,
            nead_loadbuff_min_addr: u32::MAX,
            nead_loadbuff_max_addr: 0,
            nead_savebuff_bytes: 0,
            nead_savebuff_min_addr: u32::MAX,
            nead_savebuff_max_addr: 0,
            dma_truncate_events: 0,
            dma_truncate_bytes: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AudioSaveRangeEvent {
    pub rsp_task_index: u64,
    pub start: u32,
    pub end: u32,
}

struct AudioHleTrace {
    writer: BufWriter<std::fs::File>,
    max_tasks: u64,
    max_cmds_per_task: usize,
    task_index: u64,
}

impl AudioHleTrace {
    fn from_env() -> Option<Self> {
        let path = std::env::var_os("N64_AUDIO_HLE_TRACE_PATH")?;
        let max_tasks = std::env::var("N64_AUDIO_HLE_TRACE_MAX_TASKS")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(64);
        let max_cmds_per_task = std::env::var("N64_AUDIO_HLE_TRACE_MAX_CMDS")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(256);
        let path = PathBuf::from(path);
        let file = match OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&path)
        {
            Ok(f) => f,
            Err(e) => {
                log::warn!("Audio HLE trace disabled ({}): {}", path.display(), e);
                return None;
            }
        };
        Some(Self {
            writer: BufWriter::new(file),
            max_tasks,
            max_cmds_per_task,
            task_index: 0,
        })
    }

    fn begin_task(
        &mut self,
        abi: AudioAbi,
        data_ptr: u32,
        data_size: u32,
        num_cmds: usize,
        buf_in: u16,
        buf_out: u16,
        buf_count: u16,
        env_values: [u16; 3],
        env_steps: [u16; 3],
    ) -> bool {
        let trace_this_task = self.task_index < self.max_tasks;
        if trace_this_task {
            let _ = writeln!(
                self.writer,
                "TASK idx={} abi={:?} ptr={:#010X} size={} cmds={} buf_in={:#06X} buf_out={:#06X} buf_count={} env={:04X}/{:04X}/{:04X} step={:04X}/{:04X}/{:04X}",
                self.task_index,
                abi,
                data_ptr,
                data_size,
                num_cmds,
                buf_in,
                buf_out,
                buf_count,
                env_values[0],
                env_values[1],
                env_values[2],
                env_steps[0],
                env_steps[1],
                env_steps[2]
            );
        }
        trace_this_task
    }

    fn cmd(&mut self, abi: AudioAbi, cmd_idx: usize, cmd: u32, w0: u32, w1: u32) {
        if cmd_idx >= self.max_cmds_per_task {
            return;
        }
        let detail = match abi {
            AudioAbi::Nead => match cmd {
                0x01 | 0x10 => format!("adpcm_flags={:#04X} state={:#010X}", (w0 >> 16) & 0xFF, w1),
                0x05 | 0x06 => format!(
                    "resample flags={:#04X} pitch={:#06X} state_phys={:#08X}",
                    (w0 >> 16) & 0xFF,
                    w0 & 0xFFFF,
                    Self::resolve_addr(w1) as u32
                ),
                0x08 => format!(
                    "setbuff in={:#06X} out={:#06X} count={}",
                    w0 as u16,
                    (w1 >> 16) as u16,
                    w1 as u16
                ),
                0x14 | 0x15 => {
                    let raw_count = ((w0 >> 12) & 0xFFF) as u32;
                    let count = (raw_count + 7) & !7;
                    let dmem = (w0 & 0xFFF) as u16;
                    let phys = Self::resolve_addr(w1) as u32;
                    let end = phys.saturating_add(count).saturating_sub(1);
                    format!(
                        "{} dmem={:#06X} raw_count={} count_aligned={} rdram={:#08X}-{:#08X}",
                        if cmd == 0x14 { "loadbuff" } else { "savebuff" },
                        dmem,
                        raw_count,
                        count,
                        phys,
                        end
                    )
                }
                0x0B => format!(
                    "loadadpcm hi_count={} low16={} table_phys={:#08X}",
                    (w0 >> 16) & 0xFF,
                    w0 & 0xFFFF,
                    Self::resolve_addr(w1) as u32
                ),
                0x12 => format!(
                    "envsetup1 mode={}",
                    if (w0 & 0x8) == 0 { "step" } else { "value" }
                ),
                0x13 => format!(
                    "envmixer in={:#06X} count={} out_mode={}",
                    ((w0 >> 12) & 0xFF0) as u16,
                    (w0 >> 8) & 0xFF,
                    if (w0 & 0x10) != 0 { 1 } else { 0 }
                ),
                0x16 => format!(
                    "envsetup2 mode={}",
                    if (w0 & 0x10) == 0 { "mono" } else { "stereo" }
                ),
                _ => String::new(),
            },
            AudioAbi::Standard => match cmd {
                0x08 => format!(
                    "setbuff flags={:#04X} in={:#06X} out={:#06X} count={}",
                    (w0 >> 16) & 0xFF,
                    w0 as u16,
                    (w1 >> 16) as u16,
                    w1 as u16
                ),
                0x09 => format!(
                    "setvol flags={:#04X} vol={} extra={:#010X}",
                    (w0 >> 16) & 0xFF,
                    w0 as i16,
                    w1
                ),
                _ => String::new(),
            },
        };
        let _ = writeln!(
            self.writer,
            "  CMD idx={} op={:#04X} w0={:#010X} w1={:#010X} {}",
            cmd_idx, cmd, w0, w1, detail
        );
    }

    fn end_task(&mut self, buf_in: u16, buf_out: u16, buf_count: u16, env_values: [u16; 3]) {
        if self.task_index < self.max_tasks {
            let _ = writeln!(
                self.writer,
                "END idx={} buf_in={:#06X} buf_out={:#06X} buf_count={} env={:04X}/{:04X}/{:04X}",
                self.task_index,
                buf_in,
                buf_out,
                buf_count,
                env_values[0],
                env_values[1],
                env_values[2]
            );
            let _ = self.writer.flush();
        }
        self.task_index = self.task_index.saturating_add(1);
    }

    fn resolve_addr(addr: u32) -> usize {
        match addr {
            0x8000_0000..=0x9FFF_FFFF => (addr - 0x8000_0000) as usize,
            0xA000_0000..=0xBFFF_FFFF => (addr - 0xA000_0000) as usize,
            _ => (addr & 0x00FF_FFFF) as usize,
        }
    }
}

/// Persistent audio HLE state across tasks.
pub struct AudioHle {
    /// Currently active ABI.
    pub abi: AudioAbi,

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

    // Standard ABI volume/pan
    vol_left: i16,
    vol_right: i16,
    std_vol_target_left: i16,
    std_vol_target_right: i16,
    std_vol_rate_left: u32,
    std_vol_rate_right: u32,
    std_env_dry: i16,
    std_env_wet: i16,
    std_aux_r: u16,
    std_aux_wet_l: u16,
    std_aux_wet_r: u16,

    // FILTER state
    filter_count: u16,
    filter_lut_addr: u32,

    // Optional diagnostics
    trace: Option<AudioHleTrace>,
    debug: AudioHleDebugSnapshot,
    nead_loadadpcm_fallback_mode: u8, // 0=none, 1=half(low16/2), 2=full(low16)
    write_event_tracking_enabled: bool,
    current_rsp_task_index: u64,
    pending_save_events: Vec<AudioSaveRangeEvent>,
}

impl AudioHle {
    pub fn new() -> Self {
        Self {
            abi: AudioAbi::Standard, // Default to Standard
            dmem: vec![0u8; DMEM_SIZE],
            buf_in: 0,
            buf_out: 0,
            buf_count: 0,
            adpcm_table: vec![0i16; 256],
            adpcm_loop: 0,
            env_values: [0; 3],
            env_steps: [0; 3],
            vol_left: 0,
            vol_right: 0,
            std_vol_target_left: 0,
            std_vol_target_right: 0,
            std_vol_rate_left: 0,
            std_vol_rate_right: 0,
            std_env_dry: 0x7FFF,
            std_env_wet: 0,
            std_aux_r: 0,
            std_aux_wet_l: 0,
            std_aux_wet_r: 0,
            filter_count: 0,
            filter_lut_addr: 0,
            trace: AudioHleTrace::from_env(),
            debug: AudioHleDebugSnapshot::default(),
            nead_loadadpcm_fallback_mode: Self::parse_nead_loadadpcm_fallback_mode(),
            write_event_tracking_enabled: Self::parse_temporal_trace_enabled(),
            current_rsp_task_index: 0,
            pending_save_events: Vec::new(),
        }
    }

    fn parse_nead_loadadpcm_fallback_mode() -> u8 {
        match std::env::var("N64_AUDIO_NEAD_LOADADPCM_FALLBACK")
            .unwrap_or_else(|_| "half".to_string())
            .to_ascii_lowercase()
            .as_str()
        {
            "none" | "0" => 0,
            "full" | "1" => 2,
            _ => 1, // half
        }
    }

    fn parse_temporal_trace_enabled() -> bool {
        matches!(
            std::env::var("N64_AUDIO_GFX_TEMPORAL_TRACE")
                .as_deref()
                .map(|s| s.to_ascii_lowercase()),
            Ok(v) if matches!(v.as_str(), "1" | "true" | "yes" | "on")
        )
    }

    /// Detect audio microcode variant from ROM game code.
    pub fn detect_abi(game_code: &[u8; 4]) -> AudioAbi {
        let code = std::str::from_utf8(game_code).unwrap_or("");
        // Zelda games use the "nead" ABI.
        const NEAD_GAMES: &[&str] = &[
            "CZLE", "CZLJ", "CZLP", // Ocarina of Time
            "NZSE", "NZSJ", "NZSP", // Majora's Mask
            "NZLE", "NZLJ", "NZLP", // Majora's Mask PAL
        ];
        for &c in NEAD_GAMES {
            if code == c {
                return AudioAbi::Nead;
            }
        }
        AudioAbi::Standard
    }

    /// Process an audio command list from RDRAM.
    pub fn process_audio_list(&mut self, rdram: &mut [u8], data_ptr: u32, data_size: u32) {
        let base = data_ptr as usize;
        let num_cmds = data_size as usize / 8;
        self.debug.tasks = self.debug.tasks.saturating_add(1);

        let (buf_in, buf_out, buf_count, env_values, env_steps) = (
            self.buf_in,
            self.buf_out,
            self.buf_count,
            self.env_values,
            self.env_steps,
        );
        let trace_this_task = if let Some(trace) = self.trace.as_mut() {
            trace.begin_task(
                self.abi, data_ptr, data_size, num_cmds, buf_in, buf_out, buf_count, env_values,
                env_steps,
            )
        } else {
            false
        };

        for i in 0..num_cmds {
            let off = base + i * 8;
            if off + 7 >= rdram.len() {
                break;
            }

            let w0 =
                u32::from_be_bytes([rdram[off], rdram[off + 1], rdram[off + 2], rdram[off + 3]]);
            let w1 = u32::from_be_bytes([
                rdram[off + 4],
                rdram[off + 5],
                rdram[off + 6],
                rdram[off + 7],
            ]);
            self.debug.commands = self.debug.commands.saturating_add(1);

            match self.abi {
                AudioAbi::Nead => {
                    let cmd = ((w0 >> 24) & 0x1F) as usize;
                    self.debug.nead_cmd_counts[cmd] =
                        self.debug.nead_cmd_counts[cmd].saturating_add(1);
                    if trace_this_task {
                        if let Some(trace) = self.trace.as_mut() {
                            trace.cmd(AudioAbi::Nead, i, cmd as u32, w0, w1);
                        }
                    }
                    self.dispatch_nead(w0, w1, rdram)
                }
                AudioAbi::Standard => {
                    let cmd = ((w0 >> 24) & 0xFF) as usize;
                    self.debug.standard_cmd_counts[cmd] =
                        self.debug.standard_cmd_counts[cmd].saturating_add(1);
                    if trace_this_task {
                        if let Some(trace) = self.trace.as_mut() {
                            trace.cmd(AudioAbi::Standard, i, cmd as u32, w0, w1);
                        }
                    }
                    self.dispatch_standard(w0, w1, rdram)
                }
            }
        }

        if let Some(trace) = self.trace.as_mut() {
            trace.end_task(self.buf_in, self.buf_out, self.buf_count, self.env_values);
        }
    }

    fn dispatch_nead(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let cmd = (w0 >> 24) & 0x1F;
        match cmd {
            0x00 => {}                                    // NOOP
            0x01 | 0x10 => self.cmd_adpcm(w0, w1, rdram), // 0x10 is also ADPCM
            0x02 => self.cmd_clearbuff(w0, w1),
            0x04 => self.cmd_addmixer(w0, w1),
            0x05 => self.cmd_resample(w0, w1, rdram),
            0x06 => self.cmd_resample_zoh(w0, w1, rdram),
            0x07 => self.cmd_filter(w0, w1, rdram),
            0x08 => self.cmd_setbuff(w0, w1),
            0x09 => self.cmd_duplicate(w0, w1),
            0x0A => self.cmd_dmemmove(w0, w1),
            0x0B => self.cmd_loadadpcm(w0, w1, rdram),
            0x0C => self.cmd_mixer(w0, w1),
            0x0D => self.cmd_interleave(w0, w1),
            0x0E => self.cmd_hilogain(w0, w1),
            0x0F => self.cmd_setloop(w0, w1),
            0x11 => self.cmd_interl(w0, w1),
            0x12 => self.cmd_envsetup1(w0, w1),
            0x13 => self.cmd_envmixer(w0, w1),
            0x14 => self.cmd_loadbuff(w0, w1, rdram),
            0x15 => self.cmd_savebuff(w0, w1, rdram),
            0x16 => self.cmd_envsetup2(w0, w1),
            _ => {} // UNKNOWN / NOOP
        }
    }

    fn dispatch_standard(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let cmd = (w0 >> 24) & 0xFF;
        match cmd {
            0x00 => {} // SPNOOP / NOOP
            0x01 => self.cmd_adpcm(w0, w1, rdram),
            0x02 => self.cmd_standard_clearbuff(w0, w1),
            0x03 => self.cmd_standard_envmixer(w0, w1),
            0x04 => self.cmd_standard_loadbuff(w0, w1, rdram),
            0x05 => self.cmd_resample(w0, w1, rdram),
            0x06 => self.cmd_standard_savebuff(w0, w1, rdram),
            0x07 => {} // ABI1 SEGMENT/unknown: not needed for physical-address lists
            0x08 => self.cmd_standard_setbuff(w0, w1),
            0x09 => self.cmd_standard_setvol(w0, w1),
            0x0A => self.cmd_dmemmove(w0, w1),
            0x0B => self.cmd_standard_loadadpcm(w0, w1, rdram),
            0x0C => self.cmd_standard_mixer(w0, w1),
            0x0D => self.cmd_interl(w0, w1),
            0x0E => self.cmd_standard_polef(w0, w1),
            0x0F => self.cmd_setloop(w0, w1),
            _ => {}
        }
    }

    // ─── Standard ABI Command Handlers ───

    fn cmd_standard_polef(&mut self, _w0: u32, _w1: u32) {
        // ABI1 opcode 0x0E is a pole filter command.
        // Keep as no-op until full POLEF semantics are implemented.
    }

    fn cmd_standard_setbuff(&mut self, w0: u32, w1: u32) {
        let flags = ((w0 >> 16) & 0xFF) as u8;
        if flags & 0x08 != 0 {
            // Auxiliary buffer form used by ABI1 ENVMIXER:
            //   w0[15:0]  = right/dry buffer
            //   w1[31:16] = wet left
            //   w1[15:0]  = wet right
            self.std_aux_r = w0 as u16;
            self.std_aux_wet_l = (w1 >> 16) as u16;
            self.std_aux_wet_r = w1 as u16;
            return;
        }
        // ABI1 layout:
        //   w0[15:0]   = in DMEM
        //   w1[31:16]  = out DMEM
        //   w1[15:0]   = count (bytes)
        self.buf_in = w0 as u16;
        self.buf_out = (w1 >> 16) as u16;
        self.buf_count = w1 as u16;
        // If aux form is absent, avoid stale/zero aux destinations.
        self.std_aux_r = self.buf_out;
        self.std_aux_wet_l = 0;
        self.std_aux_wet_r = 0;
    }

    fn cmd_standard_loadadpcm(&mut self, w0: u32, w1: u32, rdram: &[u8]) {
        let count = (w0 & 0xFFFF) as usize;
        let addr = Self::resolve_addr(w1);
        for i in 0..count.min(self.adpcm_table.len()) {
            let off = addr + i * 2;
            if off + 1 < rdram.len() {
                self.adpcm_table[i] = i16::from_be_bytes([rdram[off], rdram[off + 1]]);
            }
        }
    }

    fn cmd_standard_clearbuff(&mut self, w0: u32, w1: u32) {
        let dmem_addr = (w0 & 0xFFFF) as usize;
        let count = (w1 & 0xFFFF) as usize;
        let end = (dmem_addr + count).min(DMEM_SIZE);
        for i in dmem_addr..end {
            self.dmem[i] = 0;
        }
    }

    fn cmd_standard_loadbuff(&mut self, _w0: u32, w1: u32, rdram: &[u8]) {
        let count = ((self.buf_count as usize) + 7) & !7;
        let addr = Self::resolve_addr(w1);
        let dmem_off = (self.buf_in as usize) & !3;

        let copy_len = count
            .min(DMEM_SIZE.saturating_sub(dmem_off))
            .min(rdram.len().saturating_sub(addr));
        if copy_len < count {
            self.debug.dma_truncate_events = self.debug.dma_truncate_events.saturating_add(1);
            self.debug.dma_truncate_bytes = self
                .debug
                .dma_truncate_bytes
                .saturating_add((count - copy_len) as u64);
        }
        self.dmem[dmem_off..dmem_off + copy_len].copy_from_slice(&rdram[addr..addr + copy_len]);
    }

    fn cmd_standard_savebuff(&mut self, _w0: u32, w1: u32, rdram: &mut [u8]) {
        let count = ((self.buf_count as usize) + 7) & !7;
        let addr = Self::resolve_addr(w1);
        let dmem_off = (self.buf_out as usize) & !3;

        let copy_len = count
            .min(DMEM_SIZE.saturating_sub(dmem_off))
            .min(rdram.len().saturating_sub(addr));
        if copy_len < count {
            self.debug.dma_truncate_events = self.debug.dma_truncate_events.saturating_add(1);
            self.debug.dma_truncate_bytes = self
                .debug
                .dma_truncate_bytes
                .saturating_add((count - copy_len) as u64);
        }
        if copy_len > 0 {
            self.note_rdram_write_range(addr, copy_len);
        }
        rdram[addr..addr + copy_len].copy_from_slice(&self.dmem[dmem_off..dmem_off + copy_len]);
    }

    #[allow(dead_code)]
    fn cmd_standard_pan(&mut self, _w0: u32, w1: u32) {
        let pan = (w1 & 0x7F) as i64;
        let vol = 16384i64;
        self.vol_left = (vol * (127 - pan) / 127) as i16;
        self.vol_right = (vol * pan / 127) as i16;
    }

    fn cmd_standard_setvol(&mut self, w0: u32, w1: u32) {
        self.cmd_standard_setvol_inner(w0, w1);
    }

    fn cmd_standard_setvol_inner(&mut self, w0: u32, w1: u32) {
        let flags = ((w0 >> 16) & 0xFF) as u8;
        let vol = w0 as i16;
        if flags & 0x08 != 0 {
            // ABI1 A_AUX form: dry/wet gains (Q15).
            self.std_env_dry = vol;
            self.std_env_wet = w1 as i16;
            return;
        }

        if flags & 0x04 != 0 {
            // ABI1 A_VOL form: set current channel volume.
            if flags & 0x02 != 0 {
                self.vol_left = vol;
            } else {
                self.vol_right = vol;
            }
        } else {
            // ABI1 ramp/target form.
            if flags & 0x02 != 0 {
                self.std_vol_target_left = vol;
                self.std_vol_rate_left = w1;
            } else {
                self.std_vol_target_right = vol;
                self.std_vol_rate_right = w1;
            }
        }
    }

    fn cmd_standard_envmixer(&mut self, w0: u32, w1: u32) {
        let _flags = ((w0 >> 16) & 0xFF) as u8;
        let _state_addr = Self::resolve_addr(w1);
        let count = ((self.buf_count as usize) + 0xF) & !0xF;
        let in_addr = self.buf_in;
        let out_l = self.buf_out;
        let out_r = if self.std_aux_r == 0 {
            out_l
        } else {
            self.std_aux_r
        };
        let wet_l = self.std_aux_wet_l;
        let wet_r = self.std_aux_wet_r;

        let start_l = self.vol_left as i64;
        let start_r = self.vol_right as i64;
        let target_l = self.std_vol_target_left as i64;
        let target_r = self.std_vol_target_right as i64;
        let dry = self.std_env_dry as i64;
        let wet = self.std_env_wet as i64;
        let samples = (count / 2).max(1) as i64;
        let denom = (samples - 1).max(1);

        for (sample_idx, i) in (0..count).step_by(2).enumerate() {
            let t = sample_idx as i64;
            let gain_l = start_l + ((target_l - start_l) * t) / denom;
            let gain_r = start_r + ((target_r - start_r) * t) / denom;
            let s = self.read_i16(in_addr.wrapping_add(i as u16)) as i64;
            let l = ((s * gain_l) >> 15).clamp(-32768, 32767) as i16;
            let r = ((s * gain_r) >> 15).clamp(-32768, 32767) as i16;
            let dl_add = (((l as i64) * dry) >> 15).clamp(-32768, 32767) as i16;
            let dr_add = (((r as i64) * dry) >> 15).clamp(-32768, 32767) as i16;
            let wl = (((l as i64) * wet) >> 15).clamp(-32768, 32767) as i16;
            let wr = (((r as i64) * wet) >> 15).clamp(-32768, 32767) as i16;

            let dl_cur = self.read_i16(out_l.wrapping_add(i as u16)) as i64;
            self.write_i16(
                out_l.wrapping_add(i as u16),
                (dl_cur + dl_add as i64).clamp(-32768, 32767) as i16,
            );

            if out_r != out_l {
                let dr_cur = self.read_i16(out_r.wrapping_add(i as u16)) as i64;
                self.write_i16(
                    out_r.wrapping_add(i as u16),
                    (dr_cur + dr_add as i64).clamp(-32768, 32767) as i16,
                );
            }

            if wet_l != 0 && wet_l != out_l && wet_l != out_r {
                let wl_cur = self.read_i16(wet_l.wrapping_add(i as u16)) as i64;
                self.write_i16(
                    wet_l.wrapping_add(i as u16),
                    (wl_cur + wl as i64).clamp(-32768, 32767) as i16,
                );
            }

            if wet_r != 0 && wet_r != out_l && wet_r != out_r && wet_r != wet_l {
                let wr_cur = self.read_i16(wet_r.wrapping_add(i as u16)) as i64;
                self.write_i16(
                    wet_r.wrapping_add(i as u16),
                    (wr_cur + wr as i64).clamp(-32768, 32767) as i16,
                );
            }
        }

        self.vol_left = self.std_vol_target_left;
        self.vol_right = self.std_vol_target_right;
    }

    fn cmd_standard_mixer(&mut self, w0: u32, w1: u32) {
        // ABI1 mixer uses SETBUFF count in bytes and a signed Q15 gain in w0 low16.
        let count = ((self.buf_count as usize) + 0xF) & !0xF;
        let gain = w0 as i16;
        let src = (w1 >> 16) as u16;
        let dst = w1 as u16;
        for i in (0..count).step_by(2) {
            let s = self.read_i16(src.wrapping_add(i as u16)) as i64;
            let d = self.read_i16(dst.wrapping_add(i as u16)) as i64;
            let r = d + ((s * gain as i64) >> 15);
            self.write_i16(dst.wrapping_add(i as u16), r.clamp(-32768, 32767) as i16);
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
            let s = self.read_i16(src.wrapping_add(i as u16)) as i64;
            let d = self.read_i16(dst.wrapping_add(i as u16)) as i64;
            self.write_i16(
                dst.wrapping_add(i as u16),
                (s + d).clamp(-32768, 32767) as i16,
            );
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
        let count = if self.abi == AudioAbi::Nead {
            ((w0 >> 12) & 0xFF0) as usize
        } else {
            (w0 & 0xFFFF) as usize
        };
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
        let hi_count = ((w0 >> 16) & 0xFF) as usize;
        let count = if hi_count != 0 {
            self.debug.nead_loadadpcm_hi_nonzero =
                self.debug.nead_loadadpcm_hi_nonzero.saturating_add(1);
            hi_count
        } else {
            self.debug.nead_loadadpcm_hi_zero = self.debug.nead_loadadpcm_hi_zero.saturating_add(1);
            let low = (w0 & 0xFFFF) as usize;
            match self.nead_loadadpcm_fallback_mode {
                0 => 0,
                2 => low,
                _ => (low + 1) / 2,
            }
        };
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
            let s = self.read_i16(src.wrapping_add(i as u16)) as i64;
            let d = self.read_i16(dst.wrapping_add(i as u16)) as i64;
            let r = d + ((s * gain as i64) >> 15);
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

    /// 0x0E HILOGAIN: Apply gain scaling to a DMEM buffer.
    fn cmd_hilogain(&mut self, w0: u32, w1: u32) {
        let (count, gain, buf) = if self.abi == AudioAbi::Nead {
            (((w0 >> 12) & 0xFF0) as usize, w0 as i16, w1 as u16)
        } else {
            (
                (w0 & 0xFFFF) as usize,
                (w0 >> 16) as i16,
                (w1 & 0xFFFF) as u16,
            )
        };
        for i in (0..count).step_by(2) {
            let s = self.read_i16(buf.wrapping_add(i as u16)) as i64;
            let r = (s * gain as i64) >> 15;
            self.write_i16(buf.wrapping_add(i as u16), r.clamp(-32768, 32767) as i16);
        }
    }

    /// 0x0F SETLOOP: Set ADPCM loop point.
    fn cmd_setloop(&mut self, _w0: u32, w1: u32) {
        self.adpcm_loop = w1;
    }

    /// 0x07 FILTER: Conditional filter setup/execution.
    ///
    /// When flags > 1: store filter parameters (count and LUT address).
    /// When flags <= 1: execute an 8-tap FIR filter on DMEM samples using
    /// coefficients from RDRAM and maintaining state (last 8 samples) across
    /// calls for continuity.
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
            let count_bytes = self.filter_count as usize;
            let sample_count = count_bytes / 2;
            if sample_count == 0 {
                return;
            }

            let lut_addr = self.filter_lut_addr as usize;

            // Load 8 filter coefficients from RDRAM LUT (Q1.15 fixed point)
            let mut coefs = [0i16; 8];
            for j in 0..8 {
                let off = lut_addr + j * 2;
                if off + 1 < rdram.len() {
                    coefs[j] = i16::from_be_bytes([rdram[off], rdram[off + 1]]);
                }
            }

            // Load previous 8 samples from RDRAM state (for filter history)
            let mut hist = [0i16; 8];
            for j in 0..8 {
                let off = address + j * 2;
                if off + 1 < rdram.len() {
                    hist[j] = i16::from_be_bytes([rdram[off], rdram[off + 1]]);
                }
            }

            // Apply 8-tap FIR filter: y[n] = sum(coef[k] * x[n-k]) for k=0..7
            // History ring: hist[] holds x[n-7]..x[n-0] (oldest first).
            let mut output = vec![0i16; sample_count];
            for i in 0..sample_count {
                let input = self.read_i16(dmem.wrapping_add((i * 2) as u16));

                // Shift history: drop oldest, append new input
                for k in 0..7 {
                    hist[k] = hist[k + 1];
                }
                hist[7] = input;

                // Convolve: coefs[0] is applied to newest sample (hist[7]),
                // coefs[7] is applied to oldest sample (hist[0]).
                let mut accu: i64 = 0;
                for k in 0..8 {
                    accu += hist[7 - k] as i64 * coefs[k] as i64;
                }
                output[i] = (accu >> 15).clamp(-32768, 32767) as i16;
            }

            // Save state to RDRAM (last 8 samples of history)
            for j in 0..8 {
                let off = address + j * 2;
                if off + 1 < rdram.len() {
                    if j == 0 {
                        self.note_rdram_write_range(address, 16);
                    }
                    let bytes = hist[j].to_be_bytes();
                    rdram[off] = bytes[0];
                    rdram[off + 1] = bytes[1];
                }
            }

            // Write output back to DMEM
            for i in 0..sample_count {
                self.write_i16(dmem.wrapping_add((i * 2) as u16), output[i]);
            }
        }
    }

    /// 0x11 INTERL: Alternative interleave L/R mono to stereo.
    /// Nead format: out = (w0 & 0xFFFF), left = (w1>>16), right = w1&0xFFFF.
    /// Count is taken from SETBUFF buf_count (bytes, /2 for samples).
    fn cmd_interl(&mut self, w0: u32, w1: u32) {
        let out = (w0 & 0xFFFF) as u16;
        let left = (w1 >> 16) as u16;
        let right = w1 as u16;
        let samples = self.buf_count as usize / 2;
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

    /// 0x12 ENVSETUP1: Set envelope values and steps.
    /// Nead format used by this mixer path:
    /// env_values[2] = (w0>>8)&0xFF00, env_steps from w0/w1.
    fn cmd_envsetup1(&mut self, w0: u32, w1: u32) {
        if (w0 & 0x8) == 0 {
            self.debug.nead_envsetup1_step_mode =
                self.debug.nead_envsetup1_step_mode.saturating_add(1);
        } else {
            self.debug.nead_envsetup1_value_mode =
                self.debug.nead_envsetup1_value_mode.saturating_add(1);
        }
        self.env_values[2] = ((w0 >> 8) & 0xFF00) as u16;
        self.env_steps[2] = w0 as u16;
        self.env_steps[0] = (w1 >> 16) as u16;
        self.env_steps[1] = w1 as u16;
    }

    /// 0x16 ENVSETUP2: Set L/R volume levels.
    /// Nead format: env_values[0] = w1[31:16], env_values[1] = w1[15:0].
    fn cmd_envsetup2(&mut self, w0: u32, w1: u32) {
        if (w0 & 0x10) == 0 {
            self.debug.nead_envsetup2_mono_mode =
                self.debug.nead_envsetup2_mono_mode.saturating_add(1);
        } else {
            self.debug.nead_envsetup2_stereo_mode =
                self.debug.nead_envsetup2_stereo_mode.saturating_add(1);
        }
        self.env_values[0] = (w1 >> 16) as u16;
        self.env_values[1] = w1 as u16;
    }

    /// 0x13 ENVMIXER: Apply volume envelope and mix into 4 output buffers.
    /// Nead format: input DMEM addr and output buffer addrs encoded in w0/w1.
    fn cmd_envmixer(&mut self, w0: u32, w1: u32) {
        let dmemi = ((w0 >> 12) & 0xFF0) as u16;
        let count = ((w0 >> 8) & 0xFF) as usize;
        let out_mode = (w0 & 0x10) != 0;
        if out_mode {
            self.debug.nead_envmixer_out_mode = self.debug.nead_envmixer_out_mode.saturating_add(1);
        } else {
            self.debug.nead_envmixer_inplace_mode =
                self.debug.nead_envmixer_inplace_mode.saturating_add(1);
        }
        let _swap_wet_lr = ((w0 >> 4) & 1) != 0;
        let dmem_dl = ((w1 >> 20) & 0xFF0) as u16; // dry left
        let dmem_dr = ((w1 >> 12) & 0xFF0) as u16; // dry right
        let dmem_wl = ((w1 >> 4) & 0xFF0) as u16; // wet left
        let dmem_wr = ((w1 << 4) & 0xFF0) as u16; // wet right

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

        let mut remaining = count;
        while remaining > 0 {
            // Resolve effective L/R volumes for this group.
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
            let vol_wet = self.env_values[2];

            for _ in 0..8 {
                let s = self.read_i16(dmemi.wrapping_add(in_off)) as i64;

                // Apply L/R volume (env_values are unsigned Q0.16)
                let l = ((s * vol_l as i64) >> 16) as i16 ^ xor_dl;
                let r = ((s * vol_r as i64) >> 16) as i16 ^ xor_dr;

                // Apply dry/wet mix
                let l2 = ((l as i64 * vol_wet as i64) >> 16) as i16 ^ xor_wl;
                let r2 = ((r as i64 * vol_wet as i64) >> 16) as i16 ^ xor_wr;

                // Accumulate into output buffers
                let dl_cur = self.read_i16(dmem_dl.wrapping_add(dl_off)) as i64;
                self.write_i16(
                    dmem_dl.wrapping_add(dl_off),
                    (dl_cur + l as i64).clamp(-32768, 32767) as i16,
                );

                let dr_cur = self.read_i16(dmem_dr.wrapping_add(dr_off)) as i64;
                self.write_i16(
                    dmem_dr.wrapping_add(dr_off),
                    (dr_cur + r as i64).clamp(-32768, 32767) as i16,
                );

                let wl_cur = self.read_i16(dmem_wl.wrapping_add(wl_off)) as i64;
                self.write_i16(
                    dmem_wl.wrapping_add(wl_off),
                    (wl_cur + l2 as i64).clamp(-32768, 32767) as i16,
                );

                let wr_cur = self.read_i16(dmem_wr.wrapping_add(wr_off)) as i64;
                self.write_i16(
                    dmem_wr.wrapping_add(wr_off),
                    (wr_cur + r2 as i64).clamp(-32768, 32767) as i16,
                );

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

        let copy_len = count
            .min(DMEM_SIZE.saturating_sub(dmem_addr))
            .min(rdram.len().saturating_sub(addr));
        if copy_len < count {
            self.debug.dma_truncate_events = self.debug.dma_truncate_events.saturating_add(1);
            self.debug.dma_truncate_bytes = self
                .debug
                .dma_truncate_bytes
                .saturating_add((count - copy_len) as u64);
        }
        self.debug.nead_loadbuff_bytes = self
            .debug
            .nead_loadbuff_bytes
            .saturating_add(copy_len as u64);
        if copy_len > 0 {
            let start = addr as u32;
            let end = start.saturating_add(copy_len as u32).saturating_sub(1);
            self.debug.nead_loadbuff_min_addr = self.debug.nead_loadbuff_min_addr.min(start);
            self.debug.nead_loadbuff_max_addr = self.debug.nead_loadbuff_max_addr.max(end);
        }
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

        let copy_len = count
            .min(DMEM_SIZE.saturating_sub(dmem_addr))
            .min(rdram.len().saturating_sub(addr));
        if copy_len < count {
            self.debug.dma_truncate_events = self.debug.dma_truncate_events.saturating_add(1);
            self.debug.dma_truncate_bytes = self
                .debug
                .dma_truncate_bytes
                .saturating_add((count - copy_len) as u64);
        }
        self.debug.nead_savebuff_bytes = self
            .debug
            .nead_savebuff_bytes
            .saturating_add(copy_len as u64);
        if copy_len > 0 {
            let start = addr as u32;
            let end = start.saturating_add(copy_len as u32).saturating_sub(1);
            self.debug.nead_savebuff_min_addr = self.debug.nead_savebuff_min_addr.min(start);
            self.debug.nead_savebuff_max_addr = self.debug.nead_savebuff_max_addr.max(end);
            self.note_rdram_write_range(addr, copy_len);
        }
        rdram[addr..addr + copy_len].copy_from_slice(&self.dmem[dmem_addr..dmem_addr + copy_len]);
    }

    /// 0x05 RESAMPLE: Resample audio with pitch adjustment.
    fn cmd_resample(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let flags = ((w0 >> 16) & 0xFF) as u8;
        let pitch = ((w0 & 0xFFFF) as u32) << 1; // Standard pitch scaling
        let state_addr = Self::resolve_addr(w1);

        let in_addr = self.buf_in;
        let out_addr = self.buf_out;
        let count = if self.abi == AudioAbi::Nead {
            ((self.buf_count as usize) + 0xF) & !0xF
        } else {
            self.buf_count as usize
        };

        if count == 0 || pitch == 0 {
            return;
        }

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
                rdram[state_addr],
                rdram[state_addr + 1],
                rdram[state_addr + 2],
                rdram[state_addr + 3],
            ]);
            // Restore 4 previous samples before in_addr
            for j in 0..4 {
                let s = i16::from_be_bytes([
                    rdram[state_addr + 4 + j * 2],
                    rdram[state_addr + 5 + j * 2],
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
            let (s0_off, s1_off) = if self.abi == AudioAbi::Nead {
                (2, 3)
            } else {
                (3, 4)
            };
            let s0 = self.read_i16((ipos.wrapping_add(s0_off)) as u16 * 2) as i32;
            let s1 = self.read_i16((ipos.wrapping_add(s1_off)) as u16 * 2) as i32;
            let sample = s0 + (((s1 - s0) * frac) >> 16);
            self.write_i16(opos, sample.clamp(-32768, 32767) as i16);

            opos = opos.wrapping_add(2);
            pitch_accu += pitch;
            ipos = ipos.wrapping_add((pitch_accu >> 16) as u16);
            pitch_accu &= 0xFFFF;
        }

        // Save state
        if state_addr + 16 <= rdram.len() {
            self.note_rdram_write_range(state_addr, 16);
            let ab = pitch_accu.to_be_bytes();
            rdram[state_addr..state_addr + 4].copy_from_slice(&ab);
            // Save current position's 4 samples
            for j in 0..4 {
                let s = self.read_i16(ipos.wrapping_add(j as u16) as u16 * 2);
                let bytes = s.to_be_bytes();
                rdram[state_addr + 4 + j * 2] = bytes[0];
                rdram[state_addr + 5 + j * 2] = bytes[1];
            }
        }
    }

    /// 0x06 RESAMPLE_ZOH: Zero-order-hold resampling (nearest-neighbor).
    /// Same encoding as RESAMPLE (0x05) but holds each input sample until
    /// the next one is reached, producing a stepped waveform instead of
    /// linear interpolation. Used by some games for lo-fi audio effects.
    fn cmd_resample_zoh(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let flags = ((w0 >> 16) & 0xFF) as u8;
        let pitch = ((w0 & 0xFFFF) as u32) << 1; // Q16.16 fixed-point
        let state_addr = Self::resolve_addr(w1);

        let in_addr = self.buf_in;
        let out_addr = self.buf_out;
        let count = if self.abi == AudioAbi::Nead {
            ((self.buf_count as usize) + 0xF) & !0xF
        } else {
            self.buf_count as usize
        };

        if count == 0 || pitch == 0 {
            return;
        }

        let out_samples = count / 2;
        let ipos_base = (in_addr >> 1).wrapping_sub(4);

        let mut pitch_accu: u32;
        if flags & 0x01 != 0 {
            pitch_accu = 0;
            for j in 0..4 {
                self.write_i16(in_addr.wrapping_sub(8).wrapping_add((j * 2) as u16), 0);
            }
        } else if state_addr + 16 <= rdram.len() {
            pitch_accu = u32::from_be_bytes([
                rdram[state_addr],
                rdram[state_addr + 1],
                rdram[state_addr + 2],
                rdram[state_addr + 3],
            ]);
            for j in 0..4 {
                let s = i16::from_be_bytes([
                    rdram[state_addr + 4 + j * 2],
                    rdram[state_addr + 5 + j * 2],
                ]);
                self.write_i16(in_addr.wrapping_sub(8).wrapping_add((j * 2) as u16), s);
            }
        } else {
            pitch_accu = 0;
        }

        let mut ipos = ipos_base;
        let mut opos = out_addr;

        for _ in 0..out_samples {
            // Zero-order hold: take the nearest sample without interpolation
            let s_off = if self.abi == AudioAbi::Nead { 3 } else { 4 };
            let sample = self.read_i16((ipos.wrapping_add(s_off)) as u16 * 2);
            self.write_i16(opos, sample);

            opos = opos.wrapping_add(2);
            pitch_accu += pitch;
            ipos = ipos.wrapping_add((pitch_accu >> 16) as u16);
            pitch_accu &= 0xFFFF;
        }

        // Save state
        if state_addr + 16 <= rdram.len() {
            self.note_rdram_write_range(state_addr, 16);
            let ab = pitch_accu.to_be_bytes();
            rdram[state_addr..state_addr + 4].copy_from_slice(&ab);
            for j in 0..4 {
                let s = self.read_i16(ipos.wrapping_add(j as u16) as u16 * 2);
                let bytes = s.to_be_bytes();
                rdram[state_addr + 4 + j * 2] = bytes[0];
                rdram[state_addr + 5 + j * 2] = bytes[1];
            }
        }
    }

    // ─── Save state accessors ───

    pub fn dmem(&self) -> &[u8] {
        &self.dmem
    }
    pub fn set_dmem(&mut self, data: &[u8]) {
        let len = data.len().min(self.dmem.len());
        self.dmem[..len].copy_from_slice(&data[..len]);
    }
    pub fn buf_in(&self) -> u16 {
        self.buf_in
    }
    pub fn set_buf_in(&mut self, v: u16) {
        self.buf_in = v;
    }
    pub fn buf_out(&self) -> u16 {
        self.buf_out
    }
    pub fn set_buf_out(&mut self, v: u16) {
        self.buf_out = v;
    }
    pub fn buf_count(&self) -> u16 {
        self.buf_count
    }
    pub fn set_buf_count(&mut self, v: u16) {
        self.buf_count = v;
    }
    pub fn adpcm_table(&self) -> &[i16] {
        &self.adpcm_table
    }
    pub fn set_adpcm_table(&mut self, data: &[i16]) {
        self.adpcm_table.clear();
        self.adpcm_table.extend_from_slice(data);
    }
    pub fn adpcm_loop(&self) -> u32 {
        self.adpcm_loop
    }
    pub fn set_adpcm_loop(&mut self, v: u32) {
        self.adpcm_loop = v;
    }
    pub fn env_values(&self) -> &[u16; 3] {
        &self.env_values
    }
    pub fn set_env_values(&mut self, v: &[u16; 3]) {
        self.env_values = *v;
    }
    pub fn env_steps(&self) -> &[u16; 3] {
        &self.env_steps
    }
    pub fn set_env_steps(&mut self, v: &[u16; 3]) {
        self.env_steps = *v;
    }
    pub fn filter_count(&self) -> u16 {
        self.filter_count
    }
    pub fn set_filter_count(&mut self, v: u16) {
        self.filter_count = v;
    }
    pub fn filter_lut_addr(&self) -> u32 {
        self.filter_lut_addr
    }
    pub fn set_filter_lut_addr(&mut self, v: u32) {
        self.filter_lut_addr = v;
    }

    pub fn debug_snapshot(&self) -> AudioHleDebugSnapshot {
        self.debug.clone()
    }

    pub fn debug_counts(&self) -> (u64, u64) {
        (self.debug.tasks, self.debug.commands)
    }

    pub fn set_current_rsp_task_index(&mut self, rsp_task_index: u64) {
        self.current_rsp_task_index = rsp_task_index;
    }

    pub fn take_save_range_events(&mut self) -> Vec<AudioSaveRangeEvent> {
        std::mem::take(&mut self.pending_save_events)
    }

    pub fn reset_debug_stats(&mut self) {
        self.debug = AudioHleDebugSnapshot::default();
    }

    fn note_rdram_write_range(&mut self, start: usize, len: usize) {
        if !self.write_event_tracking_enabled {
            return;
        }
        if len == 0 {
            return;
        }
        let start_u32 = start as u32;
        let end_u32 = start_u32.saturating_add(len as u32).saturating_sub(1);
        self.pending_save_events.push(AudioSaveRangeEvent {
            rsp_task_index: self.current_rsp_task_index,
            start: start_u32,
            end: end_u32,
        });
    }

    fn summarize_top_counts<const N: usize>(counts: &[u64; N], limit: usize) -> String {
        let mut entries: Vec<(usize, u64)> = counts
            .iter()
            .enumerate()
            .filter_map(|(idx, &v)| (v != 0).then_some((idx, v)))
            .collect();
        entries.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
        entries.truncate(limit);
        if entries.is_empty() {
            return "none".to_string();
        }
        entries
            .into_iter()
            .map(|(idx, v)| format!("{:02X}:{}", idx, v))
            .collect::<Vec<_>>()
            .join(",")
    }

    fn summarize_addr_range(min_addr: u32, max_addr: u32) -> String {
        if min_addr == u32::MAX {
            "none".to_string()
        } else {
            format!("{:#08X}-{:#08X}", min_addr, max_addr)
        }
    }

    fn nead_loadadpcm_mode_label(&self) -> &'static str {
        match self.nead_loadadpcm_fallback_mode {
            0 => "none",
            2 => "full",
            _ => "half",
        }
    }

    pub fn debug_stats_line(&self) -> String {
        format!(
            "tasks={} cmds={} nead_loadadpcm_mode={} nead_loadadpcm_hi_zero={} nead_loadadpcm_hi_nonzero={} nead_envsetup1_step={} nead_envsetup1_value={} nead_envsetup2_mono={} nead_envsetup2_stereo={} nead_envmixer_out={} nead_envmixer_inplace={} nead_loadbuff_bytes={} nead_loadbuff_range={} nead_savebuff_bytes={} nead_savebuff_range={} dma_truncate_events={} dma_truncate_bytes={} nead_top={} std_top={}",
            self.debug.tasks,
            self.debug.commands,
            self.nead_loadadpcm_mode_label(),
            self.debug.nead_loadadpcm_hi_zero,
            self.debug.nead_loadadpcm_hi_nonzero,
            self.debug.nead_envsetup1_step_mode,
            self.debug.nead_envsetup1_value_mode,
            self.debug.nead_envsetup2_mono_mode,
            self.debug.nead_envsetup2_stereo_mode,
            self.debug.nead_envmixer_out_mode,
            self.debug.nead_envmixer_inplace_mode,
            self.debug.nead_loadbuff_bytes,
            Self::summarize_addr_range(
                self.debug.nead_loadbuff_min_addr,
                self.debug.nead_loadbuff_max_addr
            ),
            self.debug.nead_savebuff_bytes,
            Self::summarize_addr_range(
                self.debug.nead_savebuff_min_addr,
                self.debug.nead_savebuff_max_addr
            ),
            self.debug.dma_truncate_events,
            self.debug.dma_truncate_bytes,
            Self::summarize_top_counts(&self.debug.nead_cmd_counts, 8),
            Self::summarize_top_counts(&self.debug.standard_cmd_counts, 8),
        )
    }

    /// 0x01 ADPCM: Decode ADPCM audio to PCM.
    /// Uses SETBUFF state for in/out/count addresses.
    fn cmd_adpcm(&mut self, w0: u32, w1: u32, rdram: &mut [u8]) {
        let flags = ((w0 >> 16) & 0xFF) as u8;
        let state_addr = Self::resolve_addr(w1);

        let dmemi = self.buf_in;
        let dmemo = self.buf_out;
        let count = ((self.buf_count as usize) + 0x1F) & !0x1F; // align to 32

        if count == 0 {
            return;
        }

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
                        rdram[load_addr + j * 2],
                        rdram[load_addr + j * 2 + 1],
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

            // Decode 16 samples from 8 data bytes (4-bit nibbles).
            // Each nibble is sign-extended and left-shifted by `scale`.
            let mut frame = [0i16; 16];

            for byte_i in 0..8 {
                let byte = self.read_u8_dmem(in_pos);
                in_pos = in_pos.wrapping_add(1);

                // High nibble: arithmetic right shift of i8 sign-extends
                let hi = ((byte as i8) >> 4) as i32;
                frame[byte_i * 2] = (hi << scale).clamp(-32768, 32767) as i16;

                // Low nibble: shift into sign position, then arithmetic shift
                let lo = (((byte << 4) as i8) >> 4) as i32;
                frame[byte_i * 2 + 1] = (lo << scale).clamp(-32768, 32767) as i16;
            }

            // Apply prediction using codebook (first 8 samples)
            let l1 = last_frame[14];
            let l2 = last_frame[15];
            for i in 0..8 {
                let book1 = if cb_offset + i < self.adpcm_table.len() {
                    self.adpcm_table[cb_offset + i] as i32
                } else {
                    0
                };
                let book2 = if cb_offset + 8 + i < self.adpcm_table.len() {
                    self.adpcm_table[cb_offset + 8 + i] as i32
                } else {
                    0
                };

                let mut accu: i64 = (frame[i] as i64) << 11;
                accu += (book1 as i64) * (l1 as i64);
                accu += (book2 as i64) * (l2 as i64);
                for j in 0..i {
                    let b2 = if cb_offset + 8 + j < self.adpcm_table.len() {
                        self.adpcm_table[cb_offset + 8 + j] as i64
                    } else {
                        0
                    };
                    accu += b2 * (last_frame[i - 1 - j] as i64);
                }
                last_frame[i] = ((accu + 0x400) >> 11).clamp(-32768, 32767) as i16;
            }

            // Second 8 samples
            let l1_2 = last_frame[6];
            let l2_2 = last_frame[7];
            for i in 0..8 {
                let book1 = if cb_offset + i < self.adpcm_table.len() {
                    self.adpcm_table[cb_offset + i] as i32
                } else {
                    0
                };
                let book2 = if cb_offset + 8 + i < self.adpcm_table.len() {
                    self.adpcm_table[cb_offset + 8 + i] as i32
                } else {
                    0
                };

                let mut accu: i64 = (frame[8 + i] as i64) << 11;
                accu += (book1 as i64) * (l1_2 as i64);
                accu += (book2 as i64) * (l2_2 as i64);
                for j in 0..i {
                    let b2 = if cb_offset + 8 + j < self.adpcm_table.len() {
                        self.adpcm_table[cb_offset + 8 + j] as i64
                    } else {
                        0
                    };
                    accu += b2 * (last_frame[8 + i - 1 - j] as i64);
                }
                last_frame[8 + i] = ((accu + 0x400) >> 11).clamp(-32768, 32767) as i16;
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
            self.note_rdram_write_range(state_addr, 32);
            for j in 0..16 {
                let bytes = last_frame[j].to_be_bytes();
                rdram[state_addr + j * 2] = bytes[0];
                rdram[state_addr + j * 2 + 1] = bytes[1];
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{AudioAbi, AudioHle};

    fn write_u32_be(buf: &mut [u8], off: usize, value: u32) {
        let b = value.to_be_bytes();
        buf[off..off + 4].copy_from_slice(&b);
    }

    #[test]
    fn detect_abi_matches_known_game_codes() {
        assert_eq!(AudioHle::detect_abi(b"CZLE"), AudioAbi::Nead);
        assert_eq!(AudioHle::detect_abi(b"CZLJ"), AudioAbi::Nead);
        assert_eq!(AudioHle::detect_abi(b"NZSE"), AudioAbi::Nead);
        assert_eq!(AudioHle::detect_abi(b"NSME"), AudioAbi::Standard);
    }

    #[test]
    fn nead_cmd_10_alias_executes_adpcm() {
        let mut hle = AudioHle::new();
        hle.abi = AudioAbi::Nead;

        let mut rdram = vec![0xAAu8; 0x2000];
        let list_base = 0x100usize;
        let state_addr = 0x180usize;

        // 0x08 SETBUFF: in=0x0200 out=0x0400 count=0x20
        write_u32_be(&mut rdram, list_base, (0x08 << 24) | 0x0200);
        write_u32_be(&mut rdram, list_base + 4, (0x0400 << 16) | 0x0020);

        // 0x10 is NEAD_16 in docs, but behaves as ADPCM in OoT.
        // flags=INIT so decode initializes from zero state.
        write_u32_be(&mut rdram, list_base + 8, (0x10 << 24) | (0x01 << 16));
        write_u32_be(&mut rdram, list_base + 12, state_addr as u32);

        // If 0x10 is ignored, this region stays 0xAA.
        assert!(rdram[state_addr..state_addr + 32]
            .iter()
            .all(|&b| b == 0xAA));

        hle.process_audio_list(&mut rdram, list_base as u32, 16);

        // ADPCM path writes the decoded state back (all zeros for this setup).
        assert!(rdram[state_addr..state_addr + 32]
            .iter()
            .all(|&b| b == 0x00));
    }

    #[test]
    fn standard_setbuff_load_save_follow_abi1_layout() {
        let mut hle = AudioHle::new();
        hle.abi = AudioAbi::Standard;
        let mut rdram = vec![0u8; 0x400];

        let src = 0x80usize;
        let dst = 0x100usize;
        for i in 0..16usize {
            rdram[src + i] = (0xA0 + i as u8) as u8;
        }

        let list = 0x180usize;
        // A_SETBUFF: in=0x0100, out=0x0100, count=0x0010
        write_u32_be(&mut rdram, list, (0x08u32 << 24) | 0x0100);
        write_u32_be(&mut rdram, list + 4, (0x0100u32 << 16) | 0x0010);
        // A_LOADBUFF: src RDRAM -> buf_in
        write_u32_be(&mut rdram, list + 8, 0x04u32 << 24);
        write_u32_be(&mut rdram, list + 12, src as u32);
        // A_SAVEBUFF: buf_out -> dst RDRAM
        write_u32_be(&mut rdram, list + 16, 0x06u32 << 24);
        write_u32_be(&mut rdram, list + 20, dst as u32);

        hle.process_audio_list(&mut rdram, list as u32, 24);
        assert_eq!(&rdram[dst..dst + 16], &rdram[src..src + 16]);
    }

    #[test]
    fn standard_dmemmove_opcode_0a_uses_source_and_length_fields() {
        let mut hle = AudioHle::new();
        hle.abi = AudioAbi::Standard;
        let mut dmem = vec![0u8; hle.dmem().len()];
        let src = 0x0740usize;
        let dst = 0x04C0usize;
        for i in 0..8usize {
            dmem[src + i] = (0x10 + i as u8) as u8;
        }
        hle.set_dmem(&dmem);

        let mut rdram = vec![0u8; 0x100];
        let list = 0x00usize;
        write_u32_be(&mut rdram, list, (0x0Au32 << 24) | src as u32);
        write_u32_be(&mut rdram, list + 4, ((dst as u32) << 16) | 0x0008);
        hle.process_audio_list(&mut rdram, list as u32, 8);

        let out = hle.dmem();
        assert_eq!(&out[dst..dst + 8], &dmem[src..src + 8]);
        assert!(out[dst + 8..dst + 16].iter().all(|&b| b == 0));
    }

    #[test]
    fn standard_setvol_and_envmixer_scale_buf_in_samples() {
        let mut hle = AudioHle::new();
        hle.abi = AudioAbi::Standard;
        let mut dmem = vec![0u8; hle.dmem().len()];
        let in_addr = 0x0100usize;
        dmem[in_addr..in_addr + 2].copy_from_slice(&0x2000i16.to_be_bytes());
        hle.set_dmem(&dmem);

        let mut rdram = vec![0u8; 0x200];
        let list = 0x00usize;
        // A_SETBUFF: in=0x0100 out=0x0000 count=0x0002
        write_u32_be(&mut rdram, list, (0x08u32 << 24) | in_addr as u32);
        write_u32_be(&mut rdram, list + 4, 0x0000_0002);
        // A_SETVOL: flags=0x06, vol=0x4000
        write_u32_be(
            &mut rdram,
            list + 8,
            (0x09u32 << 24) | (0x06u32 << 16) | 0x4000,
        );
        write_u32_be(&mut rdram, list + 12, 0);
        // A_ENVMIXER: flags in high byte, state ptr in w1
        write_u32_be(&mut rdram, list + 16, (0x03u32 << 24) | (0x09u32 << 16));
        write_u32_be(&mut rdram, list + 20, 0x100);
        hle.process_audio_list(&mut rdram, list as u32, 24);

        let out = hle.dmem();
        let sample = i16::from_be_bytes([out[0], out[1]]);
        assert!(sample > 0);
        assert!(sample <= 0x2000);
    }

    #[test]
    fn nead_filter_uses_byte_count_from_setup() {
        let mut hle = AudioHle::new();
        hle.abi = AudioAbi::Nead;
        let mut dmem = vec![0u8; hle.dmem().len()];
        let base = 0x0200usize;

        // Four samples in DMEM; command count below is 4 bytes = 2 samples.
        dmem[base..base + 2].copy_from_slice(&0x2000i16.to_be_bytes());
        dmem[base + 2..base + 4].copy_from_slice(&0x1000i16.to_be_bytes());
        dmem[base + 4..base + 6].copy_from_slice(&0x6000i16.to_be_bytes());
        dmem[base + 6..base + 8].copy_from_slice(&(-0x5000i16).to_be_bytes());
        hle.set_dmem(&dmem);

        let mut rdram = vec![0u8; 0x400];
        let lut = 0x0100usize;
        let state = 0x0180usize;
        // FIR coefficient[0] = 0.5 (Q15), others zero.
        rdram[lut..lut + 2].copy_from_slice(&0x4000i16.to_be_bytes());

        let list = 0x0000usize;
        // 0x07 FILTER setup: flags=2, count_bytes=4, lut addr in w1.
        write_u32_be(&mut rdram, list, (0x07u32 << 24) | (0x02u32 << 16) | 0x0004);
        write_u32_be(&mut rdram, list + 4, lut as u32);
        // 0x07 FILTER execute: process DMEM at base using saved setup/state.
        write_u32_be(&mut rdram, list + 8, (0x07u32 << 24) | base as u32);
        write_u32_be(&mut rdram, list + 12, state as u32);

        hle.process_audio_list(&mut rdram, list as u32, 16);

        let out = hle.dmem();
        let s0 = i16::from_be_bytes([out[base], out[base + 1]]);
        let s1 = i16::from_be_bytes([out[base + 2], out[base + 3]]);
        let s2 = i16::from_be_bytes([out[base + 4], out[base + 5]]);
        let s3 = i16::from_be_bytes([out[base + 6], out[base + 7]]);
        assert_eq!(s0, 0x1000);
        assert_eq!(s1, 0x0800);
        // Must remain untouched when count_bytes=4 (2 samples).
        assert_eq!(s2, 0x6000);
        assert_eq!(s3, -0x5000);
    }

    #[test]
    fn nead_envsetup2_sets_lr_from_w1() {
        let mut hle = AudioHle::new();
        hle.abi = AudioAbi::Nead;

        let mut rdram = vec![0u8; 0x200];
        let list = 0x0000usize;
        // ENVSETUP2 with bit4 clear.
        write_u32_be(&mut rdram, list, (0x16u32 << 24) | 0x0000_0040);
        write_u32_be(&mut rdram, list + 4, 0x1111_2222);
        // ENVSETUP2 with bit4 set.
        write_u32_be(&mut rdram, list + 8, (0x16u32 << 24) | 0x0000_0010);
        write_u32_be(&mut rdram, list + 12, 0x3333_4444);

        hle.process_audio_list(&mut rdram, list as u32, 16);

        // Current mixer path always takes L/R directly from w1.
        assert_eq!(hle.env_values()[0], 0x3333);
        assert_eq!(hle.env_values()[1], 0x4444);
    }

    #[test]
    fn nead_envmixer_uses_legacy_packed_aux_addresses() {
        let mut hle = AudioHle::new();
        hle.abi = AudioAbi::Nead;

        let mut dmem = vec![0u8; hle.dmem().len()];
        // 8 samples of input for one ENVMIXER group.
        for i in 0..8usize {
            dmem[0x0100 + i * 2..0x0102 + i * 2].copy_from_slice(&0x4000i16.to_be_bytes());
        }
        hle.set_dmem(&dmem);
        hle.set_env_values(&[0x7FFF, 0x7FFF, 0x4000]);
        hle.set_env_steps(&[0, 0, 0]);

        let mut rdram = vec![0u8; 0x100];
        let list = 0x0000usize;
        // ENVMIXER input at 0x100, count=8 samples.
        write_u32_be(&mut rdram, list, 0x1310_0800);
        // Legacy nibble-packed layout:
        // dry L=0x300, dry R=0x0A0, wet L=0x1B0, wet R=0xB20.
        write_u32_be(&mut rdram, list + 4, 0x3040_A1B2);

        hle.process_audio_list(&mut rdram, list as u32, 8);

        let out = hle.dmem();
        let aux1 = i16::from_be_bytes([out[0x0300], out[0x0301]]);
        let wet_l = i16::from_be_bytes([out[0x0A10], out[0x0A11]]);
        // ABI2 packed decode would have targeted 0x510 for this command.
        let abi2_addr = i16::from_be_bytes([out[0x0510], out[0x0511]]);

        assert_eq!(aux1, 0x1FFF);
        assert_eq!(wet_l, 0x07FF);
        assert_eq!(abi2_addr, 0);
    }

    #[test]
    fn nead_debug_stats_track_mode_bits() {
        let mut hle = AudioHle::new();
        hle.abi = AudioAbi::Nead;
        let mut dmem = vec![0u8; hle.dmem().len()];
        dmem[0x0200..0x0202].copy_from_slice(&0x4000i16.to_be_bytes());
        hle.set_dmem(&dmem);

        let mut rdram = vec![0u8; 0x400];
        rdram[0x0100..0x0104].copy_from_slice(&[0x12, 0x34, 0x56, 0x78]);
        rdram[0x0120..0x0124].copy_from_slice(&[0x11, 0x22, 0x33, 0x44]);
        let list = 0x0000usize;
        // LOADADPCM (hi_count = 0 => low16 bytes fallback path)
        write_u32_be(&mut rdram, list, 0x0B00_0004);
        write_u32_be(&mut rdram, list + 4, 0x0100);
        // LOADADPCM (hi_count = 2)
        write_u32_be(&mut rdram, list + 8, 0x0B02_0000);
        write_u32_be(&mut rdram, list + 12, 0x0120);
        // ENVSETUP1 step mode / value mode
        write_u32_be(&mut rdram, list + 16, 0x1200_0000);
        write_u32_be(&mut rdram, list + 20, 0x0001_0002);
        write_u32_be(&mut rdram, list + 24, 0x1200_0008);
        write_u32_be(&mut rdram, list + 28, 0x1000_2000);
        // ENVSETUP2 mono mode / stereo mode
        write_u32_be(&mut rdram, list + 32, 0x1600_0040);
        write_u32_be(&mut rdram, list + 36, 0x2222_3333);
        write_u32_be(&mut rdram, list + 40, 0x1600_0010);
        write_u32_be(&mut rdram, list + 44, 0x4444_5555);
        // ENVMIXER in-place mode / out mode
        write_u32_be(&mut rdram, list + 48, 0x1320_0100);
        write_u32_be(&mut rdram, list + 52, 0x0000_0000);
        write_u32_be(&mut rdram, list + 56, 0x1320_0110);
        write_u32_be(&mut rdram, list + 60, 0x3010_0500);

        hle.process_audio_list(&mut rdram, list as u32, 64);

        let stats = hle.debug_snapshot();
        assert_eq!(stats.tasks, 1);
        assert_eq!(stats.commands, 8);
        assert_eq!(stats.nead_cmd_counts[0x0B], 2);
        assert_eq!(stats.nead_cmd_counts[0x12], 2);
        assert_eq!(stats.nead_cmd_counts[0x16], 2);
        assert_eq!(stats.nead_cmd_counts[0x13], 2);
        assert_eq!(stats.nead_loadadpcm_hi_zero, 1);
        assert_eq!(stats.nead_loadadpcm_hi_nonzero, 1);
        assert_eq!(stats.nead_envsetup1_step_mode, 1);
        assert_eq!(stats.nead_envsetup1_value_mode, 1);
        assert_eq!(stats.nead_envsetup2_mono_mode, 1);
        assert_eq!(stats.nead_envsetup2_stereo_mode, 1);
        assert_eq!(stats.nead_envmixer_inplace_mode, 1);
        assert_eq!(stats.nead_envmixer_out_mode, 1);
    }
}
