/// RSP — Reality Signal Processor.
///
/// Contains 4KB DMEM (data memory) and 4KB IMEM (instruction memory).
/// The RSP runs microcode for audio/graphics processing.
/// Registers at physical 0x0404_0000, DMEM at 0x0400_0000, IMEM at 0x0400_1000.
///
/// For initial emulation, we just provide the memory and register stubs.

/// Result of writing to an RSP register — tells the bus what action to take.
pub enum SpRegWrite {
    /// No special action needed.
    None,
    /// RSP task was started (caller should process display list).
    TaskStarted,
    /// DMA: RDRAM → SP MEM (read into DMEM/IMEM).
    DmaRead,
    /// DMA: SP MEM → RDRAM (write from DMEM/IMEM).
    DmaWrite,
}

pub struct Rsp {
    /// 4 KB Data Memory
    pub dmem: [u8; 4096],
    /// 4 KB Instruction Memory
    pub imem: [u8; 4096],
    /// SP_STATUS register
    pub status: u32,
    /// SP_DMA_FULL
    pub dma_full: u32,
    /// SP_DMA_BUSY
    pub dma_busy: u32,
    /// SP_SEMAPHORE (atomic test-and-set on read, clear on write)
    pub semaphore: std::cell::Cell<u32>,
    /// SP_PC (12-bit, IMEM offset)
    pub pc: u32,
    /// SP_MEM_ADDR — bit 12 selects IMEM vs DMEM, bits 0-11 offset
    pub dma_mem_addr: u32,
    /// SP_DRAM_ADDR — RDRAM address for DMA
    pub dma_dram_addr: u32,
    /// Pending DMA length info (from SP_RD_LEN or SP_WR_LEN write)
    pub dma_len: u32,
    pub dma_count: u32,
    pub dma_skip: u32,
    /// Debug: count of RSP task starts (auto-completed)
    pub start_count: u32,
    /// Debug: log of SP_STATUS writes (val, resulting status, auto_complete)
    pub status_log: Vec<(u32, u32, bool)>,
    /// Debug: count of SP_STATUS reads
    pub status_read_count: std::cell::Cell<u32>,
}

impl Rsp {
    pub fn new() -> Self {
        Self {
            dmem: [0u8; 4096],
            imem: [0u8; 4096],
            status: 0x01, // Halted by default
            dma_full: 0,
            dma_busy: 0,
            semaphore: std::cell::Cell::new(0),
            pc: 0,
            dma_mem_addr: 0,
            dma_dram_addr: 0,
            dma_len: 0,
            dma_count: 0,
            dma_skip: 0,
            start_count: 0,
            status_log: Vec::new(),
            status_read_count: std::cell::Cell::new(0),
        }
    }

    /// Load data into DMEM at the given offset.
    pub fn load_to_dmem(&mut self, offset: usize, data: &[u8]) {
        let end = (offset + data.len()).min(4096);
        let len = end - offset;
        self.dmem[offset..end].copy_from_slice(&data[..len]);
    }

    pub fn read_dmem_u32(&self, offset: u32) -> u32 {
        let off = (offset as usize) & 0xFFF;
        u32::from_be_bytes([
            self.dmem[off],
            self.dmem[off + 1],
            self.dmem[off + 2],
            self.dmem[off + 3],
        ])
    }

    pub fn write_dmem_u32(&mut self, offset: u32, val: u32) {
        let off = (offset as usize) & 0xFFF;
        let bytes = val.to_be_bytes();
        self.dmem[off..off + 4].copy_from_slice(&bytes);
    }

    pub fn read_imem_u32(&self, offset: u32) -> u32 {
        let off = (offset as usize) & 0xFFF;
        u32::from_be_bytes([
            self.imem[off],
            self.imem[off + 1],
            self.imem[off + 2],
            self.imem[off + 3],
        ])
    }

    pub fn write_imem_u32(&mut self, offset: u32, val: u32) {
        let off = (offset as usize) & 0xFFF;
        let bytes = val.to_be_bytes();
        self.imem[off..off + 4].copy_from_slice(&bytes);
    }

    pub fn read_reg_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x4_0000 => self.dma_mem_addr,
            0x4_0004 => self.dma_dram_addr,
            0x4_0008 | 0x4_000C => 0, // SP_RD_LEN / SP_WR_LEN (write-only, reads return 0)
            0x4_0010 => {
                self.status_read_count.set(self.status_read_count.get() + 1);
                self.status
            }
            0x4_0014 => self.dma_full,
            0x4_0018 => self.dma_busy,
            0x4_001C => {
                // SP_SEMAPHORE read: atomic test-and-set
                // Returns current value, then sets to 1
                let val = self.semaphore.get();
                self.semaphore.set(1);
                val
            }
            0x8_0000 => self.pc & 0xFFC, // SP_PC (12-bit aligned)
            _ => 0,
        }
    }

    /// Write to RSP registers. Returns action for the bus to perform.
    pub fn write_reg_u32(&mut self, addr: u32, val: u32, mi: &mut super::mi::Mi) -> SpRegWrite {
        match addr & 0x0F_FFFF {
            0x4_0000 => {
                self.dma_mem_addr = val & 0x1FFF;
                SpRegWrite::None
            }
            0x4_0004 => {
                self.dma_dram_addr = val & 0x00FF_FFFF;
                SpRegWrite::None
            }
            0x4_0008 => {
                // SP_RD_LEN write: trigger DMA RDRAM → SP MEM
                self.dma_len = (val & 0xFFF) + 1;
                self.dma_count = ((val >> 12) & 0xFF) + 1;
                self.dma_skip = (val >> 20) & 0xFFF;
                SpRegWrite::DmaRead
            }
            0x4_000C => {
                // SP_WR_LEN write: trigger DMA SP MEM → RDRAM
                self.dma_len = (val & 0xFFF) + 1;
                self.dma_count = ((val >> 12) & 0xFF) + 1;
                self.dma_skip = (val >> 20) & 0xFFF;
                SpRegWrite::DmaWrite
            }
            0x4_0010 => {
                if self.write_status(val, mi) {
                    SpRegWrite::TaskStarted
                } else {
                    SpRegWrite::None
                }
            }
            0x4_001C => {
                self.semaphore.set(0);
                SpRegWrite::None
            }
            0x8_0000 => {
                self.pc = val & 0xFFC;
                SpRegWrite::None
            }
            _ => SpRegWrite::None,
        }
    }

    /// SP_STATUS write: set/clear flag pairs.
    ///
    /// When the game clears the HALT bit (starts the RSP), we auto-complete:
    /// immediately re-halt and raise the SP + DP interrupts so the game thinks
    /// the RSP finished its task. Returns true if a task was started.
    fn write_status(&mut self, val: u32, mi: &mut super::mi::Mi) -> bool {
        let was_halted = self.status & 0x01 != 0;

        // Process set/clear pairs
        if val & 0x01 != 0 {
            self.status &= !0x01;
        } // Clear halt
        if val & 0x02 != 0 {
            self.status |= 0x01;
        } // Set halt
        if val & 0x04 != 0 {
            self.status &= !0x02;
        } // Clear broke
        if val & 0x08 != 0 {
            mi.clear_interrupt(super::mi::MiInterrupt::SP);
        }
        if val & 0x10 != 0 {
            mi.set_interrupt(super::mi::MiInterrupt::SP);
        }
        // Bits 5/6: clear/set single step (status bit 5)
        if val & (1 << 5) != 0 {
            self.status &= !(1 << 5);
        }
        if val & (1 << 6) != 0 {
            self.status |= 1 << 5;
        }
        // Bits 7/8: clear/set interrupt on break (status bit 6)
        if val & (1 << 7) != 0 {
            self.status &= !(1 << 6);
        }
        if val & (1 << 8) != 0 {
            self.status |= 1 << 6;
        }
        // Bits 9-24: clear/set signal 0-7 (status bits 7-14)
        for i in 0..8u32 {
            let clear_bit = 9 + i * 2;
            let set_bit = 10 + i * 2;
            if clear_bit < 25 {
                if val & (1 << clear_bit) != 0 {
                    self.status &= !(1 << (7 + i));
                }
            }
            if set_bit < 25 {
                if val & (1 << set_bit) != 0 {
                    self.status |= 1 << (7 + i);
                }
            }
        }

        // Auto-complete: if the game just un-halted the RSP, pretend
        // the task finished instantly. Set halt + broke (RSP stopped),
        // signal 0 (task done) + signal 1 (used by some schedulers),
        // and raise the SP interrupt. DP interrupt is raised separately
        // by the bus only for GFX tasks (after display list processing),
        // matching real hardware where DP fires when the RDP finishes.
        if was_halted && (self.status & 0x01 == 0) {
            log::debug!(
                "RSP auto-complete #{}: write={:#010X} → status={:#010X}",
                self.start_count + 1,
                val,
                self.status | 0x01 | 0x02 | (1 << 7) | (1 << 8)
            );
            self.status |= 0x01 | 0x02 | (1 << 7) | (1 << 8); // halt + broke + sig0 + sig1
            mi.set_interrupt(super::mi::MiInterrupt::SP);
            self.start_count += 1;
            if self.status_log.len() < 100 {
                self.status_log.push((val, self.status, true));
            }
            return true;
        } else if val & 0x01 != 0 {
            // Tried to clear halt but didn't trigger auto-complete
            log::debug!(
                "RSP clear-halt NO-OP: was_halted={} status={:#010X} write={:#010X}",
                was_halted,
                self.status,
                val
            );
        }
        if self.status_log.len() < 100 {
            self.status_log.push((val, self.status, false));
        }
        false
    }
}
