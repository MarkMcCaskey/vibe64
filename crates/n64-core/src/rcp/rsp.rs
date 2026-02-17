/// RSP — Reality Signal Processor.
///
/// Contains 4KB DMEM (data memory) and 4KB IMEM (instruction memory).
/// The RSP runs microcode for audio/graphics processing.
/// Registers at physical 0x0404_0000, DMEM at 0x0400_0000, IMEM at 0x0400_1000.
///
/// For initial emulation, we just provide the memory and register stubs.

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
    /// SP_SEMAPHORE
    pub semaphore: u32,
    /// SP_PC (12-bit, IMEM offset)
    pub pc: u32,
}

impl Rsp {
    pub fn new() -> Self {
        Self {
            dmem: [0u8; 4096],
            imem: [0u8; 4096],
            status: 0x01, // Halted by default
            dma_full: 0,
            dma_busy: 0,
            semaphore: 0,
            pc: 0,
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
            0x4_0010 => self.status,
            0x4_0014 => self.dma_full,
            0x4_0018 => self.dma_busy,
            0x4_001C => {
                // SP_SEMAPHORE read: returns current value, then sets to 1
                // (we can't mutate here since &self, handled at bus level)
                self.semaphore
            }
            0x8_0000 => self.pc & 0xFFC, // SP_PC (12-bit aligned)
            _ => 0,
        }
    }

    pub fn write_reg_u32(&mut self, addr: u32, val: u32) {
        match addr & 0x0F_FFFF {
            0x4_0010 => {
                // SP_STATUS write: set/clear pairs
                if val & 0x01 != 0 { self.status &= !0x01; } // Clear halt
                if val & 0x02 != 0 { self.status |= 0x01; }  // Set halt
                if val & 0x04 != 0 { self.status &= !0x02; } // Clear broke
                // Bits 3-24: clear/set various flags
                if val & 0x08 != 0 {
                    // Clear SP interrupt
                }
                if val & 0x10 != 0 {
                    // Set SP interrupt
                }
            }
            0x4_001C => self.semaphore = 0, // Writing clears semaphore
            0x8_0000 => self.pc = val & 0xFFC,
            _ => {}
        }
    }
}
