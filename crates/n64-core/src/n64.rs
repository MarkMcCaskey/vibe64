use std::path::Path;

use crate::bus::map::Interconnect;
use crate::bus::Bus;
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

        // Detect CIC variant from boot code
        let cic = cart::cic::detect(&rom_data);
        log::info!("Detected CIC: {}", cic);

        let entry_point = header.entry_point;
        let cart = Cartridge::new(rom_data, header);
        let mut bus = Interconnect::new(cart);

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
            copy_len, game_start, rdram_dest
        );

        let mut cpu = Vr4300::new();
        // Entry point from ROM header (typically 0x80000400)
        cpu.pc = entry_point as i32 as i64 as u64; // sign-extend to 64-bit
        cpu.next_pc = cpu.pc.wrapping_add(4);
        cart::cic::apply_initial_regs(cic, &mut cpu.gpr);

        // COP0 state that the IPL3 would have left
        cpu.cop0.regs[crate::cpu::cop0::Cop0::STATUS] = 0x3400_0000; // CU0+CU1 enabled

        Ok(Self {
            cpu,
            bus,
            engine: Interpreter,
            cycles: 0,
        })
    }

    /// Video info for the frontend: pixel format, framebuffer dimensions.
    pub fn vi_pixel_format(&self) -> u32 { self.bus.vi.ctrl & 0x3 }
    pub fn vi_origin(&self) -> u32 { self.bus.vi.origin }
    pub fn vi_width(&self) -> u32 { self.bus.vi.width }
    pub fn rdram_data(&self) -> &[u8] { self.bus.rdram.data() }

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

    /// Run until r30 becomes non-zero or max_cycles is reached.
    /// Returns r30 value (0 = timeout, -1 = pass, >0 = failed test number).
    pub fn run_until_r30(&mut self, max_cycles: u64) -> u64 {
        let end = self.cycles + max_cycles;
        while self.cycles < end {
            let elapsed = self.engine.execute(&mut self.cpu, &mut self.bus);
            self.cycles += elapsed;
            self.bus.vi.tick(elapsed, &mut self.bus.mi);

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
}
