/// PI — Peripheral Interface.
///
/// Handles DMA between cartridge ROM and RDRAM.
/// Writing to PI_WR_LEN triggers a DMA transfer.
/// Registers at physical 0x0460_0000.
///
/// DMA data is copied immediately (so CPU reads see the data),
/// but the completion interrupt is delayed to model real hardware
/// timing. The N64 OS relies on having several instructions
/// between PI_WR_LEN write and the PI completion interrupt.
use super::mi::{Mi, MiInterrupt};

/// PI DMA direction.
pub enum PiDmaRequest {
    None,
    /// Cart → RDRAM (loading from ROM/SRAM)
    Write,
    /// RDRAM → Cart (saving to SRAM/Flash)
    Read,
}

pub struct Pi {
    pub dram_addr: u32,
    pub cart_addr: u32,
    pub status: u32,
    /// Pending DMA length (set when PI_WR_LEN is written, consumed by Interconnect)
    pub pending_dma_len: u32,
    /// Cycles remaining until DMA completion interrupt fires.
    /// While > 0, PI_STATUS bit 0 (DMA busy) is set.
    pub dma_busy_cycles: u64,
    /// Debug: count of completed DMA transfers
    pub dma_count: u32,
    /// Debug: log of recent DMA destinations [(dram_addr, cart_addr, len)]
    pub dma_log: Vec<(u32, u32, u32)>,
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
            dma_busy_cycles: 0,
            dma_count: 0,
            dma_log: Vec::new(),
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
            0x10 => {
                // PI_STATUS: bit 0 = DMA busy, bit 1 = I/O busy
                if self.dma_busy_cycles > 0 {
                    0x01
                } else {
                    0x00
                }
            }
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

    /// Write to PI registers. Returns a DMA request if triggered.
    pub fn write_u32(&mut self, addr: u32, val: u32, mi: &mut Mi) -> PiDmaRequest {
        log::debug!("PI write: reg={:#04X} val={:#010X}", addr & 0x0F_FFFF, val);
        match addr & 0x0F_FFFF {
            0x00 => {
                self.dram_addr = val & 0x00FF_FFFF;
                PiDmaRequest::None
            }
            0x04 => {
                self.cart_addr = val;
                PiDmaRequest::None
            }
            0x08 => {
                // PI_RD_LEN: RDRAM → Cart DMA (for SRAM/Flash saves)
                self.pending_dma_len = (val & 0x00FF_FFFF) + 1;
                log::debug!(
                    "PI RD_LEN (RDRAM→Cart): RDRAM[{:#010X}] → Cart[{:#010X}], len={:#X}",
                    self.dram_addr,
                    self.cart_addr,
                    self.pending_dma_len
                );
                PiDmaRequest::Read
            }
            0x0C => {
                // PI_WR_LEN: Cart → RDRAM DMA
                self.pending_dma_len = (val & 0x00FF_FFFF) + 1;
                PiDmaRequest::Write
            }
            0x10 => {
                // PI_STATUS write: bit 1 clears PI interrupt
                if val & 0x02 != 0 {
                    mi.clear_interrupt(MiInterrupt::PI);
                }
                PiDmaRequest::None
            }
            0x14 => {
                self.dom1_lat = val & 0xFF;
                PiDmaRequest::None
            }
            0x18 => {
                self.dom1_pwd = val & 0xFF;
                PiDmaRequest::None
            }
            0x1C => {
                self.dom1_pgs = val & 0x0F;
                PiDmaRequest::None
            }
            0x20 => {
                self.dom1_rls = val & 0x03;
                PiDmaRequest::None
            }
            0x24 => {
                self.dom2_lat = val & 0xFF;
                PiDmaRequest::None
            }
            0x28 => {
                self.dom2_pwd = val & 0xFF;
                PiDmaRequest::None
            }
            0x2C => {
                self.dom2_pgs = val & 0x0F;
                PiDmaRequest::None
            }
            0x30 => {
                self.dom2_rls = val & 0x03;
                PiDmaRequest::None
            }
            _ => PiDmaRequest::None,
        }
    }

    /// Tick the PI DMA timer. Call once per CPU cycle.
    /// Returns true when the DMA completes (caller should raise PI interrupt).
    pub fn tick_dma(&mut self) -> bool {
        if self.dma_busy_cycles > 0 {
            self.dma_busy_cycles -= 1;
            if self.dma_busy_cycles == 0 {
                return true; // DMA complete — raise interrupt
            }
        }
        false
    }

    /// Tick PI DMA timer by `elapsed` CPU cycles.
    /// Returns true when completion is crossed in this interval.
    pub fn tick_dma_batch(&mut self, elapsed: u64) -> bool {
        if elapsed == 0 || self.dma_busy_cycles == 0 {
            return false;
        }
        if elapsed >= self.dma_busy_cycles {
            self.dma_busy_cycles = 0;
            return true;
        }
        self.dma_busy_cycles -= elapsed;
        false
    }

    /// Cycles until PI would raise its completion interrupt (if any DMA active).
    pub fn cycles_until_interrupt(&self) -> Option<u64> {
        (self.dma_busy_cycles > 0).then_some(self.dma_busy_cycles)
    }
}
