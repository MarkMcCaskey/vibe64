use std::path::{Path, PathBuf};

use crate::bus::map::Interconnect;
use crate::bus::Bus;
use crate::cart::{self, Cartridge};
use crate::cpu::Vr4300;
use crate::jit::Engine;

#[derive(Debug, thiserror::Error)]
pub enum N64Error {
    #[error("ROM error: {0}")]
    Rom(#[from] cart::rom::RomError),
}

/// The top-level N64 system.
///
/// Owns the CPU, bus (with all hardware components), and execution engine.
pub struct N64 {
    pub cpu: Vr4300,
    pub bus: Interconnect,
    engine: Engine,
    pub cycles: u64,
    pub debug: crate::debug::DebugState,
    /// Path to the loaded ROM (used for deriving save file paths).
    pub rom_path: PathBuf,
}

impl N64 {
    /// Create a new N64 system by loading a ROM.
    pub fn new(rom_path: &Path) -> Result<Self, N64Error> {
        let (header, rom_data) = cart::rom::load_rom(rom_path)?;
        log::info!(
            "Loaded ROM: \"{}\" (format: {:?}, entry: {:#010X})",
            header.name,
            header.format,
            header.entry_point
        );

        // Detect CIC variant from boot code
        let cic = cart::cic::detect(&rom_data);
        log::info!("Detected CIC: {}", cic);

        // Detect cartridge save backend and EEPROM type from game code
        let uses_flashram = is_flashram_game(&header.game_code);
        let eeprom_type = detect_eeprom(&header.game_code);
        log::info!(
            "Cart save backend: {}",
            if uses_flashram { "FlashRAM" } else { "SRAM" }
        );
        log::info!(
            "EEPROM: {:?} (game code: {:?})",
            match eeprom_type {
                crate::memory::pif::EepromType::None => "None",
                crate::memory::pif::EepromType::Eeprom4K => "4Kbit (512 bytes)",
                crate::memory::pif::EepromType::Eeprom16K => "16Kbit (2048 bytes)",
            },
            std::str::from_utf8(&header.game_code).unwrap_or("????")
        );

        // Detect GBI microcode variant from game code (before header is moved)
        let ucode = crate::rcp::gbi::detect_ucode(&header.game_code);
        log::info!("Microcode: {:?}", ucode);

        // Detect audio HLE ABI variant
        let audio_abi = crate::rcp::audio_hle::AudioHle::detect_abi(&header.game_code);
        log::info!("Audio ABI: {:?}", audio_abi);

        let entry_point = header.entry_point;
        let game_code = header.game_code;
        let cart = Cartridge::new(rom_data, header);
        let mut bus = Interconnect::new(cart);
        bus.use_flashram = uses_flashram;
        bus.pif.set_cic(cic);
        bus.pif.set_eeprom(eeprom_type);
        init_eeprom_for_game(&game_code, &mut bus.pif);
        bus.ucode = ucode;
        bus.renderer.ucode = ucode;
        bus.audio_hle.abi = audio_abi;

        // HLE boot: instead of running the IPL3 bootloader (which is
        // encrypted for CIC-6105), do what it would have done:
        //
        // 1. Copy first 4KB of ROM to RSP DMEM (some games reference this)
        // 2. Copy 1MB of game code from ROM[0x1000] to RDRAM[0x0000]
        // 3. Set PC to the ROM header's entry point
        //
        // The IPL3 normally does step 2 via PI DMA, then jumps to entry.
        // We skip the crypto and just do the memcpy directly.

        // Load ROM header + boot code into DMEM (some games read back from it)
        let boot_len = 0x1000.min(bus.cart.data.len());
        let boot_code: Vec<u8> = bus.cart.data[..boot_len].to_vec();
        bus.rsp.load_to_dmem(0, &boot_code);

        // Copy game code: ROM[0x1000..] → RDRAM[entry_point physical]
        // The IPL3 copies game code to the entry point's physical address.
        // entry_point is virtual (e.g., 0x80000400 → physical 0x00000400)
        let game_start = 0x1000usize;
        let rdram_dest = (entry_point & 0x1FFF_FFFF) as usize;
        let copy_len = (bus.cart.data.len() - game_start).min(0x10_0000); // up to 1MB
        for i in 0..copy_len {
            let byte = bus.cart.data[game_start + i];
            bus.rdram.write_u8((rdram_dest + i) as u32, byte);
        }
        log::info!(
            "HLE boot: copied {:#X} bytes from ROM[{:#X}] to RDRAM[{:#X}]",
            copy_len,
            game_start,
            rdram_dest
        );

        // OS boot parameters — the IPL3 writes these to low RDRAM.
        // The N64 OS reads them during initialization (__osInitialize).
        bus.rdram.write_u32(0x300, 1); // osTvType: 1=NTSC
        bus.rdram.write_u32(0x304, 0); // osRomType: 0=cartridge
        bus.rdram.write_u32(0x308, 0xB000_0000); // osRomBase: cart in kseg1
        bus.rdram.write_u32(0x30C, 0); // osResetType: 0=cold boot
        bus.rdram.write_u32(
            0x310,
            match cic {
                // osCicId
                cart::cic::CicVariant::Cic6101 => 1,
                cart::cic::CicVariant::Cic6102 => 2,
                cart::cic::CicVariant::Cic6103 => 3,
                cart::cic::CicVariant::Cic6105 => 5,
                cart::cic::CicVariant::Cic6106 => 6,
                cart::cic::CicVariant::Unknown => 2, // default to 6102
            },
        );
        bus.rdram.write_u32(0x314, 0); // osVersion
        bus.rdram.write_u32(0x318, 0x0080_0000); // osMemSize: 8MB (expansion pak)

        let mut cpu = Vr4300::new();
        // Entry point from ROM header (typically 0x80000400)
        cpu.pc = entry_point as i32 as i64 as u64; // sign-extend to 64-bit
        cpu.next_pc = cpu.pc.wrapping_add(4);
        cart::cic::apply_initial_regs(cic, &mut cpu.gpr);

        // COP0 state that the IPL3 would have left
        cpu.cop0.regs[crate::cpu::cop0::Cop0::STATUS] = 0x3400_0000; // CU0+CU1 enabled

        let engine = Engine::from_env();
        log::info!("CPU execution engine: {}", engine.name());

        let mut n64 = Self {
            cpu,
            bus,
            engine,
            cycles: 0,
            debug: crate::debug::DebugState::new(),
            rom_path: rom_path.to_path_buf(),
        };

        // Load save data from disk if it exists
        n64.load_eeprom();
        n64.load_flash();
        n64.load_sram();

        Ok(n64)
    }

    /// Video info for the frontend: pixel format, framebuffer dimensions.
    pub fn vi_pixel_format(&self) -> u32 {
        self.bus.vi.ctrl & 0x3
    }
    pub fn vi_origin(&self) -> u32 {
        self.bus.vi.origin
    }
    pub fn vi_width(&self) -> u32 {
        self.bus.vi.width
    }
    pub fn rdram_data(&self) -> &[u8] {
        self.bus.rdram.data()
    }

    /// Active CPU execution engine name.
    pub fn engine_name(&self) -> &'static str {
        self.engine.name()
    }

    /// Set a callback that receives audio samples immediately when AI DMA fires.
    /// The callback receives (&[i16] stereo PCM, u32 N64 sample rate Hz).
    /// This decouples audio output from the video frame loop, matching real
    /// N64 hardware where the AI operates asynchronously from the VI.
    pub fn set_audio_callback(&mut self, cb: Box<dyn FnMut(&[i16], u32)>) {
        self.bus.audio_callback = Some(cb);
    }

    /// Path for EEPROM save file (same directory as ROM, .eeprom extension).
    pub fn eeprom_save_path(&self) -> PathBuf {
        self.rom_path.with_extension("eeprom")
    }

    /// Path for FlashRAM save file (same directory as ROM, .flash extension).
    pub fn flash_save_path(&self) -> PathBuf {
        self.rom_path.with_extension("flash")
    }

    /// Path for SRAM save file (same directory as ROM, .sram extension).
    pub fn sram_save_path(&self) -> PathBuf {
        self.rom_path.with_extension("sram")
    }

    /// Load EEPROM data from disk if a save file exists.
    fn load_eeprom(&mut self) {
        if self.bus.pif.eeprom_type == crate::memory::pif::EepromType::None {
            return;
        }
        let path = self.eeprom_save_path();
        match std::fs::read(&path) {
            Ok(data) => {
                let expected = self.bus.pif.eeprom.len();
                if data.len() == expected {
                    self.bus.pif.load_eeprom_data(&data);
                    log::info!("Loaded EEPROM save from {:?} ({} bytes)", path, data.len());
                } else {
                    log::warn!(
                        "EEPROM save {:?} has wrong size ({} bytes, expected {}), ignoring",
                        path,
                        data.len(),
                        expected
                    );
                }
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                log::info!("No EEPROM save file at {:?}, starting fresh", path);
            }
            Err(e) => {
                log::warn!("Failed to read EEPROM save {:?}: {}", path, e);
            }
        }

        if repair_eeprom_for_game(&self.bus.cart.header.game_code, &mut self.bus.pif) {
            // Persist repaired data on next save flush.
            self.bus.pif.eeprom_dirty = true;
            log::warn!(
                "Repaired EEPROM header for game {:?} to avoid known boot deadlock",
                std::str::from_utf8(&self.bus.cart.header.game_code).unwrap_or("????")
            );
        }
    }

    /// Load FlashRAM data from disk if a save file exists.
    fn load_flash(&mut self) {
        if !self.bus.use_flashram {
            return;
        }
        let path = self.flash_save_path();
        match std::fs::read(&path) {
            Ok(data) => {
                self.bus.flashram.load_data(&data);
                log::info!(
                    "Loaded FlashRAM save from {:?} ({} bytes)",
                    path,
                    data.len()
                );
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                log::info!("No FlashRAM save file at {:?}, starting fresh", path);
            }
            Err(e) => {
                log::warn!("Failed to read FlashRAM save {:?}: {}", path, e);
            }
        }
    }

    /// Load SRAM data from disk if a save file exists.
    fn load_sram(&mut self) {
        if self.bus.use_flashram {
            return;
        }
        let path = self.sram_save_path();
        match std::fs::read(&path) {
            Ok(data) => {
                let len = data.len().min(self.bus.sram.len());
                self.bus.sram[..len].copy_from_slice(&data[..len]);
                self.bus.sram_dirty = false;
                log::info!("Loaded SRAM save from {:?} ({} bytes)", path, data.len());
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                log::info!("No SRAM save file at {:?}, starting fresh", path);
            }
            Err(e) => {
                log::warn!("Failed to read SRAM save {:?}: {}", path, e);
            }
        }
    }

    /// Flush FlashRAM data to disk if modified.
    pub fn save_flash(&mut self) {
        if !self.bus.use_flashram {
            return;
        }
        if !self.bus.flashram.dirty {
            return;
        }
        let path = self.flash_save_path();
        match std::fs::write(&path, self.bus.flashram.save_data()) {
            Ok(()) => {
                self.bus.flashram.dirty = false;
                log::info!("Saved FlashRAM to {:?}", path);
            }
            Err(e) => {
                log::error!("Failed to save FlashRAM to {:?}: {}", path, e);
            }
        }
    }

    /// Flush SRAM data to disk if modified.
    pub fn save_sram(&mut self) {
        if self.bus.use_flashram || !self.bus.sram_dirty {
            return;
        }
        let path = self.sram_save_path();
        match std::fs::write(&path, &self.bus.sram) {
            Ok(()) => {
                self.bus.sram_dirty = false;
                log::info!("Saved SRAM to {:?}", path);
            }
            Err(e) => {
                log::error!("Failed to save SRAM to {:?}: {}", path, e);
            }
        }
    }

    /// Flush EEPROM data to disk if it has been modified.
    pub fn save_eeprom(&mut self) {
        if self.bus.pif.eeprom_type == crate::memory::pif::EepromType::None {
            return;
        }
        if !self.bus.pif.eeprom_dirty {
            return;
        }
        let path = self.eeprom_save_path();
        match std::fs::write(&path, self.bus.pif.eeprom_data()) {
            Ok(()) => {
                self.bus.pif.eeprom_dirty = false;
                log::info!(
                    "Saved EEPROM to {:?} ({} bytes)",
                    path,
                    self.bus.pif.eeprom.len()
                );
            }
            Err(e) => {
                log::error!("Failed to save EEPROM to {:?}: {}", path, e);
            }
        }
    }

    /// Path for save state files: `<rom>.state<slot>`
    pub fn save_state_path(&self, slot: u8) -> PathBuf {
        self.rom_path.with_extension(format!("state{}", slot))
    }

    /// Save the emulator state to a numbered slot (0-9).
    pub fn save_state(&self, slot: u8) -> Result<(), String> {
        let path = self.save_state_path(slot);
        crate::savestate::save_to_file(self, &path).map_err(|e| format!("Save state failed: {}", e))
    }

    /// Load emulator state from a numbered slot (0-9).
    pub fn load_state(&mut self, slot: u8) -> Result<(), String> {
        let path = self.save_state_path(slot);
        crate::savestate::load_from_file(self, &path)
    }

    /// Audio: N64 sample rate from AI dacrate register.
    /// sample_rate = VI_NTSC_clock / (dacrate + 1)
    pub fn audio_sample_rate(&self) -> u32 {
        let dacrate = self.bus.ai.dacrate;
        if dacrate == 0 {
            return 32000;
        } // sensible default
        48_681_812 / (dacrate + 1)
    }

    /// Execute one engine step and tick peripherals.
    /// Returns guest cycles consumed (1 for interpreter, >=1 for dynarec blocks).
    pub fn step_one(&mut self) -> u64 {
        let elapsed = self.engine.execute(&mut self.cpu, &mut self.bus);
        self.cycles += elapsed;
        self.tick_peripherals_and_invalidate(elapsed);
        elapsed
    }

    /// Run one frame (~93.75 MHz / 60 fps ≈ 1,562,500 cycles).
    pub fn run_frame(&mut self) {
        const CYCLES_PER_FRAME: u64 = 93_750_000 / 60;

        // Sync debug flags to renderer before frame
        self.debug.begin_frame();
        self.bus.renderer.debug_wireframe = self.debug.flags.show_wireframe;
        self.bus.renderer.debug_dl_log = self.debug.flags.show_dl_log;

        let target = self.cycles + CYCLES_PER_FRAME;
        while self.cycles < target {
            let elapsed = self.engine.execute(&mut self.cpu, &mut self.bus);
            self.cycles += elapsed;
            self.tick_peripherals_and_invalidate(elapsed);
        }

        // Harvest debug data from renderer
        if self.debug.flags.show_wireframe {
            std::mem::swap(
                &mut self.debug.wire_edges,
                &mut self.bus.renderer.wire_edges,
            );
            self.bus.renderer.wire_edges.clear();
        }
        if self.debug.flags.show_dl_log {
            for entry in self.bus.renderer.dl_log_entries.drain(..) {
                self.debug.dl_log.push(entry);
            }
        }
        // Copy frame stats from renderer counters
        self.debug.stats.tri_count = self.bus.renderer.tri_count;
        self.debug.stats.vtx_count = self.bus.renderer.vtx_count;
        self.debug.stats.fill_rect_count = self.bus.renderer.fill_rect_count;
        self.debug.stats.tex_rect_count = self.bus.renderer.tex_rect_count;
    }

    /// Run one frame with instruction tracing after a trigger condition.
    /// When PI DMA count reaches `trigger_dma`, start capturing instructions.
    /// Returns captured trace (PC, opcode pairs), limited to `max_trace` entries.
    pub fn run_frame_trace(&mut self, trigger_dma: u32, max_trace: usize) -> Vec<(u64, u32)> {
        const CYCLES_PER_FRAME: u64 = 93_750_000 / 60;
        let mut trace = Vec::new();
        let mut tracing = self.bus.pi.dma_count >= trigger_dma;

        let target = self.cycles + CYCLES_PER_FRAME;
        while self.cycles < target {
            if tracing && trace.len() < max_trace {
                let phys = self.cpu.translate_address(self.cpu.pc);
                let opcode = self.bus.read_u32(phys);
                trace.push((self.cpu.pc, opcode));
            }

            let elapsed = self.engine.execute(&mut self.cpu, &mut self.bus);
            self.cycles += elapsed;
            self.tick_peripherals_and_invalidate(elapsed);

            if !tracing && self.bus.pi.dma_count >= trigger_dma {
                tracing = true;
            }
        }
        trace
    }

    /// Run one frame, stopping if PC (masked to 32 bits) enters the given range.
    /// Returns Some(pc) if triggered, None if frame completed normally.
    pub fn run_frame_trap(&mut self, trap_start: u32, trap_end: u32) -> Option<u64> {
        const CYCLES_PER_FRAME: u64 = 93_750_000 / 60;

        let target = self.cycles + CYCLES_PER_FRAME;
        while self.cycles < target {
            let pc32 = self.cpu.pc as u32;
            if pc32 >= trap_start && pc32 < trap_end {
                return Some(self.cpu.pc);
            }
            let elapsed = self.engine.execute(&mut self.cpu, &mut self.bus);
            self.cycles += elapsed;
            self.tick_peripherals_and_invalidate(elapsed);
        }
        None
    }

    /// Run until r30 becomes non-zero or max_cycles is reached.
    /// Returns r30 value (0 = timeout, -1 = pass, >0 = failed test number).
    pub fn run_until_r30(&mut self, max_cycles: u64) -> u64 {
        let end = self.cycles + max_cycles;
        while self.cycles < end {
            let elapsed = self.engine.execute(&mut self.cpu, &mut self.bus);
            self.cycles += elapsed;
            self.tick_peripherals_and_invalidate(elapsed);

            if self.cpu.gpr[30] != 0 {
                // Dump CPU state on failure for debugging
                if self.cpu.gpr[30] != 0xFFFF_FFFF_FFFF_FFFF {
                    eprintln!("  r3(actual)  = {:#018X}", self.cpu.gpr[3]);
                    eprintln!("  r4(expected)= {:#018X}", self.cpu.gpr[4]);
                    eprintln!("  r8(sa)      = {:#018X}", self.cpu.gpr[8]);
                    // r6 = regargpointer, dereference to get input
                    let ptr = self.cpu.translate_address(self.cpu.gpr[6]);
                    let input_hi = self.bus.read_u32(ptr) as u64;
                    let input_lo = self.bus.read_u32(ptr + 4) as u64;
                    eprintln!("  input       = {:#018X}", (input_hi << 32) | input_lo);
                }
                return self.cpu.gpr[30];
            }
        }
        0 // timeout
    }

    fn tick_peripherals_and_invalidate(&mut self, elapsed: u64) {
        self.bus.vi.tick(elapsed, &mut self.bus.mi);
        for _ in 0..elapsed {
            self.bus.tick_pi_dma();
            self.bus.tick_si_dma();
        }
        self.bus.tick_ai_dma(elapsed);

        for (start, len) in self.bus.take_code_invalidations() {
            self.engine.invalidate_range(start, len);
        }
    }
}

/// Detect EEPROM type from the 4-byte game code in the ROM header.
/// N64 ROMs don't store save type explicitly, so we use a database approach.
fn detect_eeprom(game_code: &[u8; 4]) -> crate::memory::pif::EepromType {
    use crate::memory::pif::EepromType;
    let code = std::str::from_utf8(game_code).unwrap_or("");

    if is_flashram_game(game_code) {
        return EepromType::None;
    }

    // SRAM games also don't use EEPROM.
    const SRAM_GAMES: &[&str] = &[
        "CZLE", "CZLJ", "CZLP", // Zelda OoT
    ];
    for &c in SRAM_GAMES {
        if code == c {
            return EepromType::None;
        }
    }

    // 16Kbit EEPROM games (common ones)
    const EEPROM_16K: &[&str] = &[
        "NCLB", "NCLE", "NCLJ", // Cruis'n World
        "NYBE", // Yoshi's Story
    ];

    // 4Kbit EEPROM games (most games that use EEPROM)
    const EEPROM_4K: &[&str] = &[
        "NSME", "NSMJ", // Super Mario 64
        "NMKE", "NMKJ", // Mario Kart 64
        "NSSE", // Star Fox 64
        "NFZE", "NFZJ", // F-Zero X
        "NWRE", // Wave Race 64
    ];

    for &c in EEPROM_16K {
        if code == c {
            return EepromType::Eeprom16K;
        }
    }
    for &c in EEPROM_4K {
        if code == c {
            return EepromType::Eeprom4K;
        }
    }

    // Default: try 4K EEPROM for unrecognized games (most common save type)
    EepromType::Eeprom4K
}

fn is_flashram_game(game_code: &[u8; 4]) -> bool {
    let code = std::str::from_utf8(game_code).unwrap_or("");
    const FLASHRAM_GAMES: &[&str] = &[
        "NZSE", "NZSJ", "NZSP", // Majora's Mask (US/JP/PAL)
        "NZLE", "NZLJ", "NZLP", // Majora's Mask (PAL alt codes)
        "NPFE", "NPFJ", "NPFP", // Pokemon Stadium 2
        "NPOE", "NPOJ", "NPOP", // Pokemon Stadium
        "NPME", // Paper Mario
    ];
    FLASHRAM_GAMES.contains(&code)
}

/// Initialize EEPROM with game-specific header data for known games.
///
/// SM64 has a bug in its save data reader: the custom osEepromLongRead wrapper
/// at 0x80329150 acquires __osContAccessQueue but only releases it on the
/// success path. If EEPROM block 0 doesn't start with the magic value 0x8000,
/// the error path returns without releasing the queue, deadlocking the game
/// thread on the next acquire. Pre-writing the magic header lets the wrapper
/// proceed normally; the game detects invalid checksums in the remaining data
/// and initializes fresh save slots.
fn init_eeprom_for_game(game_code: &[u8; 4], pif: &mut crate::memory::pif::Pif) {
    let _ = repair_eeprom_for_game(game_code, pif);
}

/// Repair malformed EEPROM data for games with strict boot-time checks.
///
/// Returns true when data was modified.
fn repair_eeprom_for_game(game_code: &[u8; 4], pif: &mut crate::memory::pif::Pif) -> bool {
    let code = std::str::from_utf8(game_code).unwrap_or("");

    match code {
        // Super Mario 64: block 0 must start with 0x8000 or a libultra wrapper
        // returns early without releasing __osContAccessQueue.
        "NSME" | "NSMJ" => {
            if pif.eeprom.len() >= 2 && (pif.eeprom[0] != 0x80 || pif.eeprom[1] != 0x00) {
                pif.eeprom[0] = 0x80;
                pif.eeprom[1] = 0x00;
                return true;
            }
        }
        _ => {}
    }
    false
}
