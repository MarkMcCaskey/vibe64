use crate::bus::Bus;
use crate::cart::Cartridge;
use crate::memory::pif::Pif;
use crate::memory::rdram::Rdram;
use crate::rcp::ai::Ai;
use crate::rcp::mi::Mi;
use crate::rcp::pi::Pi;
use crate::rcp::rdp::Rdp;
use crate::rcp::renderer::Renderer;
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
    pub renderer: Renderer,
    /// ISViewer debug output buffer (512 bytes at physical 0x13FF0020)
    is_viewer_buf: [u8; 512],
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
            renderer: Renderer::new(),
            is_viewer_buf: [0u8; 512],
        }
    }

    /// Perform PI DMA: copy data from cartridge ROM to RDRAM.
    /// Called when a game writes to PI_WR_LEN.
    ///
    /// Data is copied immediately (so CPU reads see it right away),
    /// but the completion interrupt is delayed to model real hardware
    /// timing. The N64 OS DMA handler needs several instructions
    /// between writing PI_WR_LEN and receiving the completion interrupt.
    pub fn pi_dma_to_rdram(&mut self) {
        let cart_addr = self.pi.cart_addr & 0x0FFF_FFFF;
        let dram_addr = self.pi.dram_addr & 0x00FF_FFFF;
        let len = self.pi.pending_dma_len;

        for i in 0..len {
            let byte = self.cart.read_u8(cart_addr.wrapping_add(i));
            self.rdram.write_u8(dram_addr.wrapping_add(i), byte);
        }

        log::debug!(
            "PI DMA #{}: ROM[{:#010X}] → RDRAM[{:#010X}], len={:#X}",
            self.pi.dma_count + 1, cart_addr, dram_addr, len
        );

        // Delay the completion interrupt. Real PI DMA runs at ~5MB/s
        // (~50 cycles per 32-bit word). We use a minimum of 100 cycles
        // so the OS DMA handler has time to update its queue state
        // before the interrupt fires.
        let delay = (len as u64 / 2).max(100);
        self.pi.dma_busy_cycles = delay;

        self.notify_dma_write(dram_addr, len);
        self.pi.dma_log.push((dram_addr, cart_addr, len));
        self.pi.pending_dma_len = 0;
        self.pi.dma_count += 1;
    }

    /// PI DMA from RDRAM → Cart (save writes). We don't store the data,
    /// but must fire the completion interrupt so the game doesn't hang.
    pub fn pi_dma_from_rdram(&mut self) {
        let len = self.pi.pending_dma_len;
        log::debug!(
            "PI DMA (save): RDRAM[{:#010X}] → Cart[{:#010X}], len={:#X}",
            self.pi.dram_addr, self.pi.cart_addr, len
        );
        // Same timing as cart→RDRAM DMA
        let delay = (len as u64 * 93_750_000) / (5 * 1024 * 1024);
        self.pi.dma_busy_cycles = delay.max(20);
        self.pi.pending_dma_len = 0;
        self.pi.dma_count += 1;
    }

    /// Tick PI DMA timer. Call once per CPU cycle.
    /// Raises PI interrupt in MI when DMA completes.
    pub fn tick_pi_dma(&mut self) {
        if self.pi.tick_dma() {
            self.mi.set_interrupt(crate::rcp::mi::MiInterrupt::PI);
        }
    }

    pub fn tick_ai_dma(&mut self, elapsed: u64) {
        if self.ai.tick(elapsed) {
            self.mi.set_interrupt(crate::rcp::mi::MiInterrupt::AI);
        }
    }

    /// Process an RSP task: read OSTask from DMEM, and for graphics
    /// tasks, walk the display list through the HLE renderer.
    fn process_rsp_task(&mut self) {
        use crate::rcp::gbi;

        // Read OSTask fields from DMEM
        let task_type = self.rsp.read_dmem_u32(gbi::TASK_TYPE);

        if task_type == gbi::M_GFXTASK {
            let data_ptr = self.rsp.read_dmem_u32(gbi::TASK_DATA_PTR);

            // Convert virtual address to physical
            let phys_addr = match data_ptr {
                0x8000_0000..=0x9FFF_FFFF => data_ptr - 0x8000_0000,
                0xA000_0000..=0xBFFF_FFFF => data_ptr - 0xA000_0000,
                _ => data_ptr & 0x00FF_FFFF,
            };

            log::debug!(
                "RSP GFX task #{}: display list at {:#010X} (phys {:#010X})",
                self.rsp.start_count, data_ptr, phys_addr
            );

            // Walk display list, rendering into RDRAM
            let rdram = self.rdram.data_mut();
            let tris_before = self.renderer.tri_count;
            gbi::process_display_list(&mut self.renderer, rdram, phys_addr);
            let tris_this_dl = self.renderer.tri_count - tris_before;

            log::debug!(
                "GFX #{}: {} tris, ci={:#X}",
                self.rsp.start_count, tris_this_dl,
                self.renderer.color_image_addr,
            );
        }
        // Audio tasks (M_AUDTASK) are silently ignored for now
    }

    /// SP DMA Read: RDRAM → SP MEM (load microcode/data into DMEM or IMEM).
    fn sp_dma_read(&mut self) {
        let is_imem = self.rsp.dma_mem_addr & 0x1000 != 0;
        let mut mem_off = (self.rsp.dma_mem_addr & 0xFFF) as usize;
        let mut dram_addr = self.rsp.dma_dram_addr & 0x00FF_FFFF;
        let line_len = self.rsp.dma_len as usize;

        let mem = if is_imem { &mut self.rsp.imem } else { &mut self.rsp.dmem };

        for _ in 0..self.rsp.dma_count {
            for i in 0..line_len {
                let src = (dram_addr as usize + i) & 0x7F_FFFF;
                mem[(mem_off + i) & 0xFFF] = self.rdram.data()[src];
            }
            mem_off = (mem_off + line_len) & 0xFFF;
            dram_addr = dram_addr.wrapping_add(line_len as u32 + self.rsp.dma_skip);
        }

        log::trace!(
            "SP DMA read: RDRAM[{:#010X}] → {}[{:#05X}], len={:#X}, lines={}",
            self.rsp.dma_dram_addr, if is_imem { "IMEM" } else { "DMEM" },
            self.rsp.dma_mem_addr & 0xFFF, line_len, self.rsp.dma_count,
        );
    }

    /// SP DMA Write: SP MEM → RDRAM (save microcode output to RDRAM).
    fn sp_dma_write(&mut self) {
        let is_imem = self.rsp.dma_mem_addr & 0x1000 != 0;
        let mut mem_off = (self.rsp.dma_mem_addr & 0xFFF) as usize;
        let mut dram_addr = self.rsp.dma_dram_addr & 0x00FF_FFFF;
        let line_len = self.rsp.dma_len as usize;

        let mem = if is_imem { &self.rsp.imem } else { &self.rsp.dmem };

        for _ in 0..self.rsp.dma_count {
            for i in 0..line_len {
                let byte = mem[(mem_off + i) & 0xFFF];
                let dst = (dram_addr as usize + i) & 0x7F_FFFF;
                self.rdram.data_mut()[dst] = byte;
            }
            mem_off = (mem_off + line_len) & 0xFFF;
            dram_addr = dram_addr.wrapping_add(line_len as u32 + self.rsp.dma_skip);
        }

        log::trace!(
            "SP DMA write: {}[{:#05X}] → RDRAM[{:#010X}], len={:#X}, lines={}",
            if is_imem { "IMEM" } else { "DMEM" }, self.rsp.dma_mem_addr & 0xFFF,
            self.rsp.dma_dram_addr, line_len, self.rsp.dma_count,
        );
    }

    /// SI DMA Read: PIF RAM → RDRAM (game reads controller state).
    /// Processes PIF commands first, then copies 64 bytes.
    fn si_dma_read(&mut self) {
        self.pif.process_commands();
        let dram_addr = self.si.dram_addr & 0x00FF_FFFF;
        for i in 0..64u32 {
            let byte = self.pif.ram[i as usize];
            self.rdram.write_u8(dram_addr + i, byte);
        }
        self.mi.set_interrupt(crate::rcp::mi::MiInterrupt::SI);
        self.si.status |= 1 << 12; // Interrupt pending
        self.si.dma_count += 1;
    }

    /// SI DMA Write: RDRAM → PIF RAM (game sends commands to PIF).
    /// Copies 64 bytes then processes PIF commands.
    fn si_dma_write(&mut self) {
        let dram_addr = self.si.dram_addr & 0x00FF_FFFF;
        for i in 0..64u32 {
            self.pif.ram[i as usize] = self.rdram.read_u8(dram_addr + i);
        }
        self.pif.process_commands();
        self.mi.set_interrupt(crate::rcp::mi::MiInterrupt::SI);
        self.si.status |= 1 << 12; // Interrupt pending
        self.si.dma_count += 1;
    }

    /// ISViewer read: return buffer contents or zero.
    fn is_viewer_read(&self, addr: u32) -> u32 {
        match addr {
            0x13FF_0004 => 0x4953_5664, // Magic "ISVd" — indicates ISViewer present
            _ => 0,
        }
    }

    /// ISViewer write: buffer stores or length-trigger flush.
    fn is_viewer_write(&mut self, addr: u32, val: u32) {
        match addr {
            // Write to buffer region (byte-by-byte via write_u8 → write_u32)
            0x13FF_0020..=0x13FF_021F => {
                let offset = (addr - 0x13FF_0020) as usize;
                let bytes = val.to_be_bytes();
                for (i, &b) in bytes.iter().enumerate() {
                    if offset + i < 512 {
                        self.is_viewer_buf[offset + i] = b;
                    }
                }
            }
            // Length trigger: flush buffer contents to stdout
            0x13FF_0014 => {
                let len = (val as usize).min(512);
                if len > 0 {
                    let text = &self.is_viewer_buf[..len];
                    if let Ok(s) = std::str::from_utf8(text) {
                        print!("{}", s);
                    }
                    self.is_viewer_buf[..len].fill(0);
                }
            }
            _ => {}
        }
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
            // ISViewer debug port (must be checked before cart range)
            0x13FF_0000..=0x13FF_0FFF => self.is_viewer_read(addr),
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
            0x0000_0000..=0x03EF_FFFF => {
                self.rdram.write_u32(addr, val);
            }
            0x03F0_0000..=0x03FF_FFFF => {} // RDRAM registers (stubbed)
            0x0400_0000..=0x0400_0FFF => self.rsp.write_dmem_u32(addr & 0xFFF, val),
            0x0400_1000..=0x0400_1FFF => self.rsp.write_imem_u32(addr & 0xFFF, val),
            0x0404_0000..=0x040F_FFFF => {
                use crate::rcp::rsp::SpRegWrite;
                match self.rsp.write_reg_u32(addr, val, &mut self.mi) {
                    SpRegWrite::TaskStarted => self.process_rsp_task(),
                    SpRegWrite::DmaRead => self.sp_dma_read(),
                    SpRegWrite::DmaWrite => self.sp_dma_write(),
                    SpRegWrite::None => {}
                }
            }
            0x0410_0000..=0x041F_FFFF => self.rdp.write_reg_u32(addr, val),
            0x0430_0000..=0x043F_FFFF => self.mi.write_u32(addr, val),
            0x0440_0000..=0x044F_FFFF => self.vi.write_u32(addr, val, &mut self.mi),
            0x0450_0000..=0x045F_FFFF => {
                use crate::rcp::ai::AiRegWrite;
                match self.ai.write_u32(addr, val) {
                    AiRegWrite::ClearInterrupt => {
                        self.mi.clear_interrupt(crate::rcp::mi::MiInterrupt::AI);
                    }
                    AiRegWrite::None => {}
                }
            }
            0x0460_0000..=0x046F_FFFF => {
                use crate::rcp::pi::PiDmaRequest;
                match self.pi.write_u32(addr, val, &mut self.mi) {
                    PiDmaRequest::Write => self.pi_dma_to_rdram(),
                    PiDmaRequest::Read => self.pi_dma_from_rdram(),
                    PiDmaRequest::None => {}
                }
            }
            0x0470_0000..=0x047F_FFFF => self.ri.write_u32(addr, val),
            0x0480_0000..=0x048F_FFFF => {
                use crate::rcp::si::SiDmaRequest;
                let req = self.si.write_u32(addr, val);
                match req {
                    SiDmaRequest::Read => self.si_dma_read(),
                    SiDmaRequest::Write => self.si_dma_write(),
                    SiDmaRequest::None => {
                        // SI_STATUS write — clear SI interrupt
                        if addr & 0x0F_FFFF == 0x18 {
                            self.mi.clear_interrupt(crate::rcp::mi::MiInterrupt::SI);
                            self.si.status &= !(1 << 12); // Clear interrupt pending
                        }
                    }
                }
            }
            // ISViewer debug port (must be checked before cart range)
            0x13FF_0000..=0x13FF_0FFF => self.is_viewer_write(addr, val),
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
