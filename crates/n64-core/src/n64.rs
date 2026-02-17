use std::path::Path;

use crate::bus::map::Interconnect;
use crate::cart::{self, Cartridge};
use crate::cpu::Vr4300;
use crate::jit::{ExecutionEngine, Interpreter};

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
    engine: Interpreter,
    cycles: u64,
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

        let cart = Cartridge::new(rom_data, header);
        let mut bus = Interconnect::new(cart);

        // Simulate PIF boot: copy first 4KB of ROM into RSP DMEM.
        // The IPL3 bootloader starts at DMEM offset 0x40.
        let boot_code_len = 0x1000.min(bus.cart.data.len());
        let boot_code: Vec<u8> = bus.cart.data[..boot_code_len].to_vec();
        bus.rsp.load_to_dmem(0, &boot_code);

        let mut cpu = Vr4300::new();
        cpu.setup_post_pif_boot();

        Ok(Self {
            cpu,
            bus,
            engine: Interpreter,
            cycles: 0,
        })
    }

    /// Run one frame (~93.75 MHz / 60 fps ≈ 1,562,500 cycles).
    pub fn run_frame(&mut self) {
        const CYCLES_PER_FRAME: u64 = 93_750_000 / 60;

        let target = self.cycles + CYCLES_PER_FRAME;
        while self.cycles < target {
            let elapsed = self.engine.execute(&mut self.cpu, &mut self.bus);
            self.cycles += elapsed;

            // Tick the VI (scanline timing → VI interrupt)
            self.bus.vi.tick(elapsed, &mut self.bus.mi);
        }
    }
}
