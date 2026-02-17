/// SI — Serial Interface.
///
/// Handles DMA between PIF RAM (64 bytes) and RDRAM for controller I/O.
/// Writing to SI_PIF_ADDR_RD64B (0x04) triggers PIF→RDRAM.
/// Writing to SI_PIF_ADDR_WR64B (0x10) triggers RDRAM→PIF.
/// Both raise an SI interrupt on completion.
/// Registers at physical 0x0480_0000.

/// SI DMA direction requested by the game.
pub enum SiDmaRequest {
    None,
    /// PIF RAM → RDRAM (game reads controller state)
    Read,
    /// RDRAM → PIF RAM (game sends commands to PIF)
    Write,
}

pub struct Si {
    pub dram_addr: u32,
    pub status: u32,
}

impl Si {
    pub fn new() -> Self {
        Self {
            dram_addr: 0,
            status: 0,
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x00 => self.dram_addr,
            0x18 => self.status,
            _ => 0,
        }
    }

    /// Write to SI registers. Returns a DMA request if triggered.
    pub fn write_u32(&mut self, addr: u32, val: u32) -> SiDmaRequest {
        match addr & 0x0F_FFFF {
            0x00 => { self.dram_addr = val & 0x00FF_FFFF; SiDmaRequest::None }
            0x04 => {
                // SI_PIF_ADDR_RD64B: triggers PIF RAM → RDRAM DMA
                self.dram_addr = val & 0x00FF_FFFF;
                SiDmaRequest::Read
            }
            0x10 => {
                // SI_PIF_ADDR_WR64B: triggers RDRAM → PIF RAM DMA
                self.dram_addr = val & 0x00FF_FFFF;
                SiDmaRequest::Write
            }
            0x18 => {
                // Writing to SI_STATUS clears interrupt (handled by caller)
                SiDmaRequest::None
            }
            _ => SiDmaRequest::None,
        }
    }
}
