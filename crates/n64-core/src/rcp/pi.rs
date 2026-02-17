/// PI — Peripheral Interface.
///
/// Handles DMA between cartridge ROM and RDRAM.
/// Writing to PI_WR_LEN triggers a DMA transfer.
/// Registers at physical 0x0460_0000.
///
/// This is the critical path for loading game data.
/// Also the natural JIT invalidation intercept point.

use super::mi::{Mi, MiInterrupt};

pub struct Pi {
    pub dram_addr: u32,
    pub cart_addr: u32,
    pub status: u32,
    /// Pending DMA length (set when PI_WR_LEN is written, consumed by Interconnect)
    pub pending_dma_len: u32,
    // Bus timing registers (rarely used by games)
    pub dom1_lat: u32,
    pub dom1_pwd: u32,
    pub dom1_pgs: u32,
    pub dom1_rls: u32,
    pub dom2_lat: u32,
    pub dom2_pwd: u32,
    pub dom2_pgs: u32,
    pub dom2_rls: u32,
}

impl Pi {
    pub fn new() -> Self {
        Self {
            dram_addr: 0,
            cart_addr: 0,
            status: 0,
            pending_dma_len: 0,
            dom1_lat: 0,
            dom1_pwd: 0,
            dom1_pgs: 0,
            dom1_rls: 0,
            dom2_lat: 0,
            dom2_pwd: 0,
            dom2_pgs: 0,
            dom2_rls: 0,
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x00 => self.dram_addr,
            0x04 => self.cart_addr,
            0x10 => self.status,
            0x14 => self.dom1_lat,
            0x18 => self.dom1_pwd,
            0x1C => self.dom1_pgs,
            0x20 => self.dom1_rls,
            0x24 => self.dom2_lat,
            0x28 => self.dom2_pwd,
            0x2C => self.dom2_pgs,
            0x30 => self.dom2_rls,
            _ => 0,
        }
    }

    /// Write to PI registers. Returns true if a DMA should be triggered.
    pub fn write_u32(&mut self, addr: u32, val: u32, mi: &mut Mi) -> bool {
        match addr & 0x0F_FFFF {
            0x00 => {
                self.dram_addr = val & 0x00FF_FFFF;
                false
            }
            0x04 => {
                self.cart_addr = val;
                false
            }
            0x0C => {
                // PI_WR_LEN: writing triggers Cart → RDRAM DMA
                self.pending_dma_len = (val & 0x00FF_FFFF) + 1;
                true // Signal caller to perform DMA
            }
            0x10 => {
                // PI_STATUS write: bit 1 clears PI interrupt
                if val & 0x02 != 0 {
                    mi.clear_interrupt(MiInterrupt::PI);
                }
                false
            }
            0x14 => { self.dom1_lat = val & 0xFF; false }
            0x18 => { self.dom1_pwd = val & 0xFF; false }
            0x1C => { self.dom1_pgs = val & 0x0F; false }
            0x20 => { self.dom1_rls = val & 0x03; false }
            0x24 => { self.dom2_lat = val & 0xFF; false }
            0x28 => { self.dom2_pwd = val & 0xFF; false }
            0x2C => { self.dom2_pgs = val & 0x0F; false }
            0x30 => { self.dom2_rls = val & 0x03; false }
            _ => false,
        }
    }
}
