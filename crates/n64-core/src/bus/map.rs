use crate::bus::Bus;
use crate::cart::Cartridge;
use crate::memory::pif::Pif;
use crate::memory::rdram::Rdram;
use crate::rcp::ai::Ai;
use crate::rcp::mi::Mi;
use crate::rcp::pi::Pi;
use crate::rcp::rdp::Rdp;
use crate::rcp::ri::Ri;
use crate::rcp::rsp::Rsp;
use crate::rcp::si::Si;
use crate::rcp::vi::Vi;

/// The concrete bus that wires all hardware components together.
/// Physical address dispatch happens here.
pub struct Interconnect {
    pub rdram: Rdram,
    pub cart: Cartridge,
    pub pi: Pi,
    pub mi: Mi,
    pub vi: Vi,
    pub ai: Ai,
    pub ri: Ri,
    pub si: Si,
    pub rsp: Rsp,
    pub rdp: Rdp,
    pub pif: Pif,
}

impl Interconnect {
    pub fn new(cart: Cartridge) -> Self {
        Self {
            rdram: Rdram::new(),
            cart,
            pi: Pi::new(),
            mi: Mi::new(),
            vi: Vi::new(),
            ai: Ai::new(),
            ri: Ri::new(),
            si: Si::new(),
            rsp: Rsp::new(),
            rdp: Rdp::new(),
            pif: Pif::new(),
        }
    }

    /// Perform PI DMA: copy data from cartridge ROM to RDRAM.
    /// Called when a game writes to PI_WR_LEN.
    pub fn pi_dma_to_rdram(&mut self) {
        let cart_addr = self.pi.cart_addr & 0x0FFF_FFFF;
        let dram_addr = self.pi.dram_addr & 0x00FF_FFFF;
        let len = self.pi.pending_dma_len;

        for i in 0..len {
            let byte = self.cart.read_u8(cart_addr.wrapping_add(i));
            self.rdram.write_u8(dram_addr.wrapping_add(i), byte);
        }

        self.mi.set_interrupt(crate::rcp::mi::MiInterrupt::PI);
        self.notify_dma_write(dram_addr, len);
        self.pi.pending_dma_len = 0;
    }
}

impl Bus for Interconnect {
    fn read_u8(&self, addr: u32) -> u8 {
        let word = self.read_u32(addr & !3);
        let shift = (3 - (addr & 3)) * 8;
        (word >> shift) as u8
    }

    fn read_u16(&self, addr: u32) -> u16 {
        let word = self.read_u32(addr & !3);
        let shift = (2 - (addr & 2)) * 8;
        (word >> shift) as u16
    }

    fn read_u32(&self, addr: u32) -> u32 {
        match addr {
            0x0000_0000..=0x03EF_FFFF => self.rdram.read_u32(addr),
            0x03F0_0000..=0x03FF_FFFF => 0, // RDRAM registers (stubbed)
            0x0400_0000..=0x0400_0FFF => self.rsp.read_dmem_u32(addr & 0xFFF),
            0x0400_1000..=0x0400_1FFF => self.rsp.read_imem_u32(addr & 0xFFF),
            0x0404_0000..=0x040F_FFFF => self.rsp.read_reg_u32(addr),
            0x0410_0000..=0x041F_FFFF => self.rdp.read_reg_u32(addr),
            0x0430_0000..=0x043F_FFFF => self.mi.read_u32(addr),
            0x0440_0000..=0x044F_FFFF => self.vi.read_u32(addr),
            0x0450_0000..=0x045F_FFFF => self.ai.read_u32(addr),
            0x0460_0000..=0x046F_FFFF => self.pi.read_u32(addr),
            0x0470_0000..=0x047F_FFFF => self.ri.read_u32(addr),
            0x0480_0000..=0x048F_FFFF => self.si.read_u32(addr),
            0x1000_0000..=0x1FBF_FFFF => self.cart.read_u32(addr),
            0x1FC0_0000..=0x1FC0_07BF => self.pif.read_boot_rom_u32(addr),
            0x1FC0_07C0..=0x1FC0_07FF => self.pif.read_ram_u32(addr),
            _ => {
                log::warn!("Unhandled bus read32: {:#010X}", addr);
                0
            }
        }
    }

    fn read_u64(&self, addr: u32) -> u64 {
        let hi = self.read_u32(addr) as u64;
        let lo = self.read_u32(addr.wrapping_add(4)) as u64;
        (hi << 32) | lo
    }

    fn write_u8(&mut self, addr: u32, val: u8) {
        let aligned = addr & !3;
        let shift = (3 - (addr & 3)) * 8;
        let mask = !(0xFFu32 << shift);
        let current = self.read_u32(aligned);
        self.write_u32(aligned, (current & mask) | ((val as u32) << shift));
    }

    fn write_u16(&mut self, addr: u32, val: u16) {
        let aligned = addr & !3;
        let shift = (2 - (addr & 2)) * 8;
        let mask = !(0xFFFFu32 << shift);
        let current = self.read_u32(aligned);
        self.write_u32(aligned, (current & mask) | ((val as u32) << shift));
    }

    fn write_u32(&mut self, addr: u32, val: u32) {
        match addr {
            0x0000_0000..=0x03EF_FFFF => self.rdram.write_u32(addr, val),
            0x03F0_0000..=0x03FF_FFFF => {} // RDRAM registers (stubbed)
            0x0400_0000..=0x0400_0FFF => self.rsp.write_dmem_u32(addr & 0xFFF, val),
            0x0400_1000..=0x0400_1FFF => self.rsp.write_imem_u32(addr & 0xFFF, val),
            0x0404_0000..=0x040F_FFFF => self.rsp.write_reg_u32(addr, val),
            0x0410_0000..=0x041F_FFFF => self.rdp.write_reg_u32(addr, val),
            0x0430_0000..=0x043F_FFFF => self.mi.write_u32(addr, val),
            0x0440_0000..=0x044F_FFFF => self.vi.write_u32(addr, val, &mut self.mi),
            0x0450_0000..=0x045F_FFFF => self.ai.write_u32(addr, val),
            0x0460_0000..=0x046F_FFFF => {
                let trigger_dma = self.pi.write_u32(addr, val, &mut self.mi);
                if trigger_dma {
                    self.pi_dma_to_rdram();
                }
            }
            0x0470_0000..=0x047F_FFFF => self.ri.write_u32(addr, val),
            0x0480_0000..=0x048F_FFFF => self.si.write_u32(addr, val),
            0x1FC0_07C0..=0x1FC0_07FF => self.pif.write_ram_u32(addr, val),
            _ => {
                log::warn!("Unhandled bus write32: {:#010X} = {:#010X}", addr, val);
            }
        }
    }

    fn write_u64(&mut self, addr: u32, val: u64) {
        self.write_u32(addr, (val >> 32) as u32);
        self.write_u32(addr.wrapping_add(4), val as u32);
    }

    fn notify_dma_write(&mut self, _start: u32, _len: u32) {
        // Future JIT: invalidate compiled blocks in this range
    }

    fn pending_interrupts(&self) -> bool {
        self.mi.interrupt_pending()
    }
}
