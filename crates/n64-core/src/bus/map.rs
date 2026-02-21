use crate::bus::Bus;
use crate::cart::Cartridge;
use crate::memory::pif::Pif;
use crate::memory::rdram::Rdram;
use crate::rcp::ai::Ai;
use crate::rcp::audio_hle::AudioHle;
use crate::rcp::flashram::FlashRam;
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
    pub audio_hle: AudioHle,
    pub flashram: FlashRam,
    /// SRAM save memory (32KB) used by games like Zelda OoT.
    pub sram: Vec<u8>,
    pub sram_dirty: bool,
    /// True for games that use FlashRAM at 0x0800_0000.
    pub use_flashram: bool,
    pub ucode: crate::rcp::gbi::UcodeType,
    /// ISViewer debug output buffer (512 bytes at physical 0x13FF0020)
    is_viewer_buf: [u8; 512],
    /// Audio sample buffer: 16-bit signed PCM, stereo interleaved (L,R,L,R...).
    /// Populated from RDRAM when AI DMA starts. Frontend drains each frame.
    pub audio_samples: Vec<i16>,
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
            audio_hle: AudioHle::new(),
            flashram: FlashRam::new(),
            sram: vec![0u8; 0x8000],
            sram_dirty: false,
            use_flashram: false,
            ucode: crate::rcp::gbi::UcodeType::F3dex2,
            is_viewer_buf: [0u8; 512],
            audio_samples: Vec::with_capacity(16384),
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

        // Save memory DMA space (0x0800_0000..0x0801_FFFF):
        // either FlashRAM or SRAM depending on game.
        if cart_addr >= 0x0800_0000 && cart_addr < 0x0802_0000 {
            if self.use_flashram {
                let flash_offset = cart_addr - 0x0800_0000;
                let rdram = self.rdram.data_mut();
                let dest_start = dram_addr as usize;
                let dest_end = (dest_start + len as usize).min(rdram.len());
                self.flashram.dma_read(flash_offset, &mut rdram[dest_start..dest_end]);

                log::debug!(
                    "PI DMA (FlashRAM→RDRAM): Flash[{:#010X}] → RDRAM[{:#010X}], len={:#X}",
                    flash_offset, dram_addr, len
                );
            } else {
                let sram_base = (cart_addr - 0x0800_0000) as usize;
                for i in 0..len as usize {
                    let byte = self.sram[(sram_base + i) & 0x7FFF];
                    self.rdram.write_u8(dram_addr.wrapping_add(i as u32), byte);
                }
                log::debug!(
                    "PI DMA (SRAM→RDRAM): SRAM[{:#06X}] → RDRAM[{:#010X}], len={:#X}",
                    sram_base, dram_addr, len
                );
            }

            let delay = (len as u64 * 19).max(200);
            self.pi.dma_busy_cycles = delay;
            self.pi.pending_dma_len = 0;
            self.pi.dma_count += 1;
            return;
        }

        for i in 0..len {
            let byte = self.cart.read_u8(cart_addr.wrapping_add(i));
            self.rdram.write_u8(dram_addr.wrapping_add(i), byte);
        }

        log::debug!(
            "PI DMA #{}: ROM[{:#010X}] → RDRAM[{:#010X}], len={:#X}",
            self.pi.dma_count + 1, cart_addr, dram_addr, len
        );

        // Delay the completion interrupt to match real PI DMA speed (~5MB/s).
        // Real hardware: ~19 CPU cycles per byte (93.75MHz / 5MB/s).
        // Realistic timing ensures the N64 OS thread scheduler has time to
        // properly dispatch threads that are waiting for DMA completion.
        let delay = (len as u64 * 19).max(200);
        self.pi.dma_busy_cycles = delay;

        self.notify_dma_write(dram_addr, len);
        self.pi.dma_log.push((dram_addr, cart_addr, len));
        self.pi.pending_dma_len = 0;
        self.pi.dma_count += 1;
    }

    /// PI DMA from RDRAM → Cart (save writes). We don't store the data,
    /// but must fire the completion interrupt so the game doesn't hang.
    pub fn pi_dma_from_rdram(&mut self) {
        let cart_addr = self.pi.cart_addr & 0x0FFF_FFFF;
        let dram_addr = self.pi.dram_addr & 0x00FF_FFFF;
        let len = self.pi.pending_dma_len;

        // Save memory DMA space (0x0800_0000..0x0801_FFFF):
        // either FlashRAM or SRAM depending on game.
        if cart_addr >= 0x0800_0000 && cart_addr < 0x0802_0000 {
            if self.use_flashram {
                let rdram = self.rdram.data();
                let src_start = dram_addr as usize;
                let src_end = (src_start + len as usize).min(rdram.len());
                self.flashram.dma_write(&rdram[src_start..src_end]);

                log::debug!(
                    "PI DMA (RDRAM→FlashRAM): RDRAM[{:#010X}] → Flash page buffer, len={:#X}",
                    dram_addr, len
                );
            } else {
                let sram_base = (cart_addr - 0x0800_0000) as usize;
                for i in 0..len as usize {
                    let byte = self.rdram.read_u8(dram_addr.wrapping_add(i as u32));
                    self.sram[(sram_base + i) & 0x7FFF] = byte;
                }
                self.sram_dirty = true;
                log::debug!(
                    "PI DMA (RDRAM→SRAM): RDRAM[{:#010X}] → SRAM[{:#06X}], len={:#X}",
                    dram_addr, sram_base, len
                );
            }
        } else {
            log::debug!(
                "PI DMA (save): RDRAM[{:#010X}] → Cart[{:#010X}], len={:#X}",
                dram_addr, cart_addr, len
            );
        }

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
        let data_ptr_raw = self.rsp.read_dmem_u32(gbi::TASK_DATA_PTR);
        // Keep task logging quiet by default; use debug for diagnostics.
        if task_type != gbi::M_AUDTASK || self.rsp.start_count <= 5 || self.rsp.start_count % 200 == 0 {
            log::debug!("RSP task #{}: type={} data_ptr={:#010X} flags={:#X}",
                self.rsp.start_count, task_type, data_ptr_raw,
                self.rsp.read_dmem_u32(gbi::TASK_FLAGS));
        }

        if task_type == gbi::M_AUDTASK {
            let data_ptr = self.rsp.read_dmem_u32(gbi::TASK_DATA_PTR);
            let data_size = self.rsp.read_dmem_u32(gbi::TASK_DATA_SIZE);

            let phys = match data_ptr {
                0x8000_0000..=0x9FFF_FFFF => data_ptr - 0x8000_0000,
                0xA000_0000..=0xBFFF_FFFF => data_ptr - 0xA000_0000,
                _ => data_ptr & 0x00FF_FFFF,
            };

            log::debug!("RSP audio task: data={:#X} size={}", phys, data_size);
            let rdram = self.rdram.data_mut();
            self.audio_hle.process_audio_list(rdram, phys, data_size);
        }

        if task_type == gbi::M_GFXTASK {
            let data_ptr = self.rsp.read_dmem_u32(gbi::TASK_DATA_PTR);

            // Convert virtual address to physical
            let phys_addr = match data_ptr {
                0x8000_0000..=0x9FFF_FFFF => data_ptr - 0x8000_0000,
                0xA000_0000..=0xBFFF_FFFF => data_ptr - 0xA000_0000,
                _ => data_ptr & 0x00FF_FFFF,
            };

            // Optional display-list dump for targeted debugging.
            if std::env::var_os("N64_DUMP_GFX_DL").is_some() {
                let rdram = self.rdram.data();
                log::debug!("GFX DL at phys {:#010X}, ucode={:?}, first 60 cmds:", phys_addr, self.ucode);
                for i in 0..60 {
                    let off = phys_addr as usize + i * 8;
                    if off + 8 <= rdram.len() {
                        let w0 = u32::from_be_bytes([rdram[off], rdram[off+1], rdram[off+2], rdram[off+3]]);
                        let w1 = u32::from_be_bytes([rdram[off+4], rdram[off+5], rdram[off+6], rdram[off+7]]);
                        let cmd = w0 >> 24;
                        if cmd == 0xB8 { // G_ENDDL
                            log::debug!("  DL[{:2}]: {:#010X} {:#010X} (G_ENDDL)", i, w0, w1);
                            break;
                        }
                        log::debug!("  DL[{:2}]: {:#010X} {:#010X} (cmd={:#04X})", i, w0, w1, cmd);
                    }
                }
            }

            log::debug!(
                "RSP GFX task #{}: display list at {:#010X} (phys {:#010X})",
                self.rsp.start_count, data_ptr, phys_addr
            );

            // Walk display list, rendering into RDRAM
            let rdram = self.rdram.data_mut();
            let tris_before = self.renderer.tri_count;
            self.renderer.task_nonblack_writes = 0;
            self.renderer.task_total_writes = 0;
            let cmd_count = match self.ucode {
                gbi::UcodeType::F3dex2 => {
                    gbi::process_display_list(&mut self.renderer, rdram, phys_addr)
                }
                gbi::UcodeType::F3d => {
                    gbi::process_display_list_f3d(&mut self.renderer, rdram, phys_addr);
                    0 // F3D doesn't return count yet
                }
            };
            let tris_this_dl = self.renderer.tri_count - tris_before;

            if cmd_count >= gbi::MAX_COMMANDS {
                log::warn!("GFX #{}: HIT COMMAND LIMIT ({} commands)!", self.rsp.start_count, cmd_count);
            }

            // Log per-task info for diagnostics
            log::debug!(
                "GFX #{}: {} cmds, {} tris, ci={:#X} w={} scissor=({},{})..({},{})",
                self.rsp.start_count, cmd_count, tris_this_dl,
                self.renderer.color_image_addr,
                self.renderer.color_image_width,
                self.renderer.scissor_ulx >> 2, self.renderer.scissor_uly >> 2,
                self.renderer.scissor_lrx >> 2, self.renderer.scissor_lry >> 2,
            );

            log::debug!(
                "GFX #{}: {} tris, ci={:#X}",
                self.rsp.start_count, tris_this_dl,
                self.renderer.color_image_addr,
            );

            // Save the best frame snapshot (most non-black pixels) for diagnostics.
            {
                let ci = self.renderer.color_image_addr as usize;
                let w = self.renderer.color_image_width as usize;
                let fb_bytes = w * 240 * 2;
                let rdram = self.rdram.data();
                let nonblack = if ci > 0 && ci + fb_bytes <= rdram.len() {
                    let mut nb = 0u32;
                    for i in (0..fb_bytes).step_by(2) {
                        let px = u16::from_be_bytes([rdram[ci + i], rdram[ci + i + 1]]);
                        if px >> 1 != 0 { nb += 1; }
                    }
                    if nb > self.renderer.best_frame_nonblack {
                        self.renderer.best_frame_snapshot = rdram[ci..ci + fb_bytes].to_vec();
                        self.renderer.best_frame_nonblack = nb;
                    }
                    nb
                } else { 0 };
                // Record CI history (last 30 GFX tasks)
                if self.renderer.ci_history.len() >= 30 {
                    self.renderer.ci_history.remove(0);
                }
                let tw = self.renderer.task_total_writes;
                let tnb = self.renderer.task_nonblack_writes;
                self.renderer.ci_history.push((ci as u32, tris_this_dl, nonblack, tw, tnb));
            }

            // DP interrupt: RDP finished rendering this display list.
            // Only raised for GFX tasks (audio tasks don't use the RDP).
            self.mi.set_interrupt(crate::rcp::mi::MiInterrupt::DP);
        }
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
    /// Copies 64 bytes immediately but defers the SI interrupt by ~6000
    /// CPU cycles to match real PIF bus timing (~64 µs at 1 MHz bus).
    fn si_dma_read(&mut self) {
        // On hardware, Joybus commands in PIF RAM are executed as part of the
        // SI transfer that reads PIF RAM back to RDRAM.
        self.pif.process_commands();
        let dram_addr = self.si.dram_addr & 0x00FF_FFFF;
        for i in 0..64u32 {
            let byte = self.pif.ram[i as usize];
            self.rdram.write_u8(dram_addr + i, byte);
        }
        // Defer SI interrupt — tick_si_dma will fire it after the delay.
        self.si.dma_busy_cycles = 6000;
        self.si.dma_count += 1;
        if self.si.dma_log.len() < 200 {
            self.si.dma_log.push((dram_addr, 0)); // 0 = read (PIF→RDRAM)
        }
    }

    /// SI DMA Write: RDRAM → PIF RAM (game sends commands to PIF).
    /// Copies 64 bytes and processes PIF commands immediately, but defers
    /// the SI interrupt to match real PIF bus timing.
    fn si_dma_write(&mut self) {
        let dram_addr = self.si.dram_addr & 0x00FF_FFFF;
        for i in 0..64u32 {
            self.pif.ram[i as usize] = self.rdram.read_u8(dram_addr + i);
        }
        // Defer SI interrupt — tick_si_dma will fire it after the delay.
        self.si.dma_busy_cycles = 6000;
        self.si.dma_count += 1;
        if self.si.dma_log.len() < 200 {
            self.si.dma_log.push((dram_addr, 1)); // 1 = write (RDRAM→PIF)
        }
    }

    /// Tick SI DMA timer. Raises SI interrupt when DMA completes.
    pub fn tick_si_dma(&mut self) {
        if self.si.tick_dma() {
            self.mi.set_interrupt(crate::rcp::mi::MiInterrupt::SI);
            self.si.status |= 1 << 12; // Interrupt pending
        }
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
            // Save memory space at 0x0800_0000:
            // FlashRAM (status/data) or SRAM depending on game.
            0x0800_0000..=0x0801_FFFF => {
                if self.use_flashram {
                    self.flashram.read_status()
                } else {
                    let off = ((addr - 0x0800_0000) as usize) & 0x7FFF;
                    u32::from_be_bytes([
                        self.sram[off],
                        self.sram[(off + 1) & 0x7FFF],
                        self.sram[(off + 2) & 0x7FFF],
                        self.sram[(off + 3) & 0x7FFF],
                    ])
                }
            }
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
                    AiRegWrite::DmaStarted { dram_addr, len } => {
                        // Capture audio samples from RDRAM for frontend playback.
                        // N64 audio: 16-bit signed PCM, big-endian, stereo interleaved.
                        let base = dram_addr as usize;
                        let sample_count = len as usize / 2; // 2 bytes per i16
                        for i in 0..sample_count {
                            let off = base + i * 2;
                            if off + 1 < self.rdram.data().len() {
                                let sample = i16::from_be_bytes([
                                    self.rdram.data()[off],
                                    self.rdram.data()[off + 1],
                                ]);
                                self.audio_samples.push(sample);
                            }
                        }
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
            // Save memory command/data space at 0x0800_0000.
            // SRAM is handled via PI DMA; direct CPU writes are ignored.
            0x0800_0000..=0x0800_FFFF => {}
            0x0801_0000..=0x0801_FFFF => {
                if self.use_flashram {
                    self.flashram.command(val);
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
