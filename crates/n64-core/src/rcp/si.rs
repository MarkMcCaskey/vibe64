/// SI — Serial Interface.
///
/// Handles DMA between PIF RAM (64 bytes) and RDRAM for controller I/O.
/// Writing to SI_PIF_ADDR_RD64B (0x04) triggers PIF→RDRAM.
/// Writing to SI_PIF_ADDR_WR64B (0x10) triggers RDRAM→PIF.
/// Both raise an SI interrupt on completion.
/// Registers at physical 0x0480_0000.
///
/// On real hardware, SI DMA transfers 64 bytes over the PIF bus at ~1 MHz,
/// taking ~64 microseconds (~6000 CPU cycles at 93.75 MHz). Data is copied
/// immediately for functional correctness, but the completion interrupt is
/// delayed to prevent message queue overflow in the N64 OS.

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
    pub dma_count: u32,
    /// Cycles remaining until SI DMA completion interrupt fires.
    /// While > 0, SI_STATUS bit 0 (DMA busy) is set.
    pub dma_busy_cycles: u64,
    /// Debug: log of SI DMA events (dram_addr, direction: 0=read, 1=write)
    pub dma_log: Vec<(u32, u8)>,
}

impl Si {
    pub fn new() -> Self {
        Self {
            dram_addr: 0,
            status: 0,
            dma_count: 0,
            dma_busy_cycles: 0,
            dma_log: Vec::new(),
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x00 => self.dram_addr,
            0x18 => {
                // SI_STATUS: bit 0 = DMA busy, bit 12 = interrupt pending
                let mut status = self.status;
                if self.dma_busy_cycles > 0 {
                    status |= 0x01; // DMA busy
                }
                status
            }
            _ => 0,
        }
    }

    /// Write to SI registers. Returns a DMA request if triggered.
    pub fn write_u32(&mut self, addr: u32, val: u32) -> SiDmaRequest {
        match addr & 0x0F_FFFF {
            0x00 => {
                self.dram_addr = val & 0x00FF_FFFF;
                SiDmaRequest::None
            }
            0x04 => {
                // SI_PIF_ADDR_RD64B: triggers PIF RAM → RDRAM DMA.
                // The value written is the PIF address (always 0x1FC007C0),
                // NOT the RDRAM address. The RDRAM address was set earlier
                // via SI_DRAM_ADDR (offset 0x00). Do NOT overwrite dram_addr.
                SiDmaRequest::Read
            }
            0x10 => {
                // SI_PIF_ADDR_WR64B: triggers RDRAM → PIF RAM DMA.
                // Same as above: the value is the PIF address, not RDRAM address.
                SiDmaRequest::Write
            }
            0x18 => {
                // Writing to SI_STATUS clears interrupt (handled by caller)
                SiDmaRequest::None
            }
            _ => SiDmaRequest::None,
        }
    }

    /// Tick the SI DMA timer. Returns true when DMA completes.
    pub fn tick_dma(&mut self) -> bool {
        if self.dma_busy_cycles > 0 {
            self.dma_busy_cycles -= 1;
            if self.dma_busy_cycles == 0 {
                return true;
            }
        }
        false
    }

    /// Tick SI DMA timer by `elapsed` CPU cycles.
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

    /// Cycles until SI would raise its completion interrupt (if any DMA active).
    pub fn cycles_until_interrupt(&self) -> Option<u64> {
        (self.dma_busy_cycles > 0).then_some(self.dma_busy_cycles)
    }
}
