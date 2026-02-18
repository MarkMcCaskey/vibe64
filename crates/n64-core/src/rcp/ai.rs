/// AI — Audio Interface.
///
/// DMA-based audio output. The AI has a 2-deep FIFO for audio buffers.
/// Writing AI_LEN starts a DMA transfer from RDRAM to the audio DAC.
/// When a buffer finishes playing, the AI fires an interrupt via MI.
/// Registers at physical 0x0450_0000.

pub struct Ai {
    pub dram_addr: u32,
    pub len: u32,
    pub control: u32,
    pub dacrate: u32,
    pub bitrate: u32,
    /// Countdown in CPU cycles until current DMA buffer finishes.
    pub dma_cycles: u64,
    /// Whether a DMA is currently active (playing audio).
    pub dma_active: bool,
    /// Debug counter.
    pub dma_count: u32,
}

/// Return value from AI register writes.
pub enum AiRegWrite {
    None,
    /// AI_STATUS written — caller should clear MI AI interrupt.
    ClearInterrupt,
}

impl Ai {
    pub fn new() -> Self {
        Self {
            dram_addr: 0,
            len: 0,
            control: 0,
            dacrate: 0,
            bitrate: 0,
            dma_cycles: 0,
            dma_active: false,
            dma_count: 0,
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x04 => self.len,
            0x0C => {
                // AI_STATUS: bit 30 = DMA busy, bit 0 = FIFO full
                if self.dma_active { 1 << 30 } else { 0 }
            }
            _ => 0,
        }
    }

    pub fn write_u32(&mut self, addr: u32, val: u32) -> AiRegWrite {
        match addr & 0x0F_FFFF {
            0x00 => { self.dram_addr = val & 0x00FF_FFFF; AiRegWrite::None }
            0x04 => {
                // AI_LEN: start DMA playback
                self.len = val & 0x0003_FFF8;
                if self.len > 0 && !self.dma_active {
                    self.dma_active = true;
                    // Calculate how many CPU cycles this buffer takes to play.
                    // samples = len / 4 (16-bit stereo)
                    // sample_rate = vi_clock / (dacrate + 1)
                    // cycles = samples * cpu_freq / sample_rate
                    //        = (len/4) * cpu_freq * (dacrate+1) / vi_clock
                    let dacrate = self.dacrate.max(1) as u64;
                    let samples = self.len as u64 / 4;
                    // 93_750_000 * (dacrate+1) / 48_681_812 ≈ (dacrate+1) * 1.926
                    self.dma_cycles = samples * (dacrate + 1) * 93_750_000 / 48_681_812;
                    // Minimum 1000 cycles to avoid tight loops
                    if self.dma_cycles < 1000 { self.dma_cycles = 1000; }
                    self.dma_count += 1;
                }
                AiRegWrite::None
            }
            0x08 => { self.control = val & 1; AiRegWrite::None }
            0x0C => {
                // Writing to AI_STATUS clears AI interrupt
                AiRegWrite::ClearInterrupt
            }
            0x10 => { self.dacrate = val & 0x3FFF; AiRegWrite::None }
            0x14 => { self.bitrate = val & 0xF; AiRegWrite::None }
            _ => AiRegWrite::None,
        }
    }

    /// Tick the AI DMA timer. Returns true if the buffer just finished
    /// (caller should fire MI AI interrupt).
    pub fn tick(&mut self, elapsed: u64) -> bool {
        if !self.dma_active {
            return false;
        }
        if elapsed >= self.dma_cycles {
            self.dma_active = false;
            self.dma_cycles = 0;
            return true; // buffer done — fire interrupt
        }
        self.dma_cycles -= elapsed;
        false
    }
}
