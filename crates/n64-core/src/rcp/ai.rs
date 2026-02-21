/// AI — Audio Interface.
///
/// DMA-based audio output. The AI has a 2-deep FIFO for audio buffers.
/// Writing AI_LEN starts a DMA transfer from RDRAM to the audio DAC.
/// When a buffer finishes playing, the AI fires an interrupt via MI.
/// Registers at physical 0x0450_0000.

/// An entry in the AI's 2-deep DMA FIFO.
#[derive(Clone, Copy, Default)]
struct AiFifoEntry {
    len: u32,
    cycles: u64,
}

pub struct Ai {
    pub dram_addr: u32,
    pub len: u32,
    pub control: u32,
    pub dacrate: u32,
    pub bitrate: u32,
    /// 2-deep FIFO: [0] = currently playing, [1] = queued.
    fifo: [AiFifoEntry; 2],
    fifo_count: u8, // 0, 1, or 2
    /// Countdown in CPU cycles until current buffer finishes.
    pub dma_cycles: u64,
    /// Debug counter.
    pub dma_count: u32,
}

/// Return value from AI register writes.
pub enum AiRegWrite {
    None,
    /// AI_STATUS written — caller should clear MI AI interrupt.
    ClearInterrupt,
    /// AI_LEN written — a DMA was started/queued. Caller should capture audio
    /// samples from RDRAM at (dram_addr, len) for audio output.
    DmaStarted {
        dram_addr: u32,
        len: u32,
    },
}

impl Ai {
    pub fn new() -> Self {
        Self {
            dram_addr: 0,
            len: 0,
            control: 0,
            dacrate: 0,
            bitrate: 0,
            fifo: [AiFifoEntry::default(); 2],
            fifo_count: 0,
            dma_cycles: 0,
            dma_count: 0,
        }
    }

    /// Calculate DMA duration in CPU cycles for a given buffer length.
    fn calc_dma_cycles(&self, len: u32) -> u64 {
        let dacrate = self.dacrate.max(1) as u64;
        let samples = len as u64 / 4; // 16-bit stereo = 4 bytes per sample pair
        // cycles = samples * cpu_freq * (dacrate+1) / vi_clock
        let cycles = samples * (dacrate + 1) * 93_750_000 / 48_681_812;
        cycles.max(1000) // minimum to avoid tight loops
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x04 => {
                // AI_LEN readback reports remaining length of the current DMA.
                if self.fifo_count == 0 {
                    0
                } else {
                    let cur = self.fifo[0];
                    if cur.cycles == 0 {
                        0
                    } else {
                        let remaining =
                            ((cur.len as u64 * self.dma_cycles) + cur.cycles - 1) / cur.cycles;
                        (remaining as u32) & 0x0003_FFF8
                    }
                }
            }
            0x0C => {
                // AI_STATUS: bit 30 = DMA busy, bit 31 = FIFO full
                let mut status = 0u32;
                if self.fifo_count >= 1 {
                    status |= 1 << 30;
                } // DMA busy
                if self.fifo_count >= 2 {
                    status |= 1 << 31;
                } // FIFO full
                status
            }
            _ => 0,
        }
    }

    pub fn write_u32(&mut self, addr: u32, val: u32) -> AiRegWrite {
        match addr & 0x0F_FFFF {
            0x00 => {
                self.dram_addr = val & 0x00FF_FFFF;
                AiRegWrite::None
            }
            0x04 => {
                // AI_LEN: queue DMA playback
                self.len = val & 0x0003_FFF8;
                if self.len > 0 && self.fifo_count < 2 {
                    // CONTROL bit 0 gates playback.
                    // Games typically enable CONTROL once during init.
                    if self.control & 1 == 0 {
                        return AiRegWrite::None;
                    }

                    let cycles = self.calc_dma_cycles(self.len);
                    let entry = AiFifoEntry {
                        len: self.len,
                        cycles,
                    };

                    let idx = self.fifo_count as usize;
                    self.fifo[idx] = entry;
                    self.fifo_count += 1;

                    // If this is the first entry, start playing immediately
                    if self.fifo_count == 1 {
                        self.dma_cycles = cycles;
                    }

                    self.dma_count += 1;
                    return AiRegWrite::DmaStarted {
                        dram_addr: self.dram_addr,
                        len: self.len,
                    };
                }
                AiRegWrite::None
            }
            0x08 => {
                self.control = val & 1;
                AiRegWrite::None
            }
            0x0C => {
                // Writing to AI_STATUS clears AI interrupt
                AiRegWrite::ClearInterrupt
            }
            0x10 => {
                self.dacrate = val & 0x3FFF;
                AiRegWrite::None
            }
            0x14 => {
                self.bitrate = val & 0xF;
                AiRegWrite::None
            }
            _ => AiRegWrite::None,
        }
    }

    /// Tick the AI DMA timer. Returns true if a buffer just finished
    /// (caller should fire MI AI interrupt).
    pub fn tick(&mut self, elapsed: u64) -> bool {
        if self.control & 1 == 0 || self.fifo_count == 0 {
            return false;
        }
        if elapsed >= self.dma_cycles {
            // Current buffer finished
            // Shift FIFO: move queued entry to playing position
            self.fifo[0] = self.fifo[1];
            self.fifo[1] = AiFifoEntry::default();
            self.fifo_count -= 1;

            // Auto-start next buffer if queued
            if self.fifo_count > 0 {
                self.dma_cycles = self.fifo[0].cycles;
            } else {
                self.dma_cycles = 0;
            }
            return true; // fire AI interrupt
        }
        self.dma_cycles -= elapsed;
        false
    }

    /// Check if DMA is currently active (for backwards compatibility).
    pub fn dma_active(&self) -> bool {
        self.fifo_count > 0
    }
}

#[cfg(test)]
mod tests {
    use super::{Ai, AiRegWrite};

    #[test]
    fn status_bits_use_busy_and_full_flags() {
        let mut ai = Ai::new();
        ai.write_u32(0x08, 1); // AI_CONTROL enable
        ai.write_u32(0x10, 0x3F); // AI_DACRATE

        ai.write_u32(0x00, 0x0000_1000);
        assert!(matches!(
            ai.write_u32(0x04, 0x0000_0200),
            AiRegWrite::DmaStarted { .. }
        ));
        let status_one = ai.read_u32(0x0C);
        assert_ne!(status_one & (1 << 30), 0, "busy should be set");
        assert_eq!(status_one & (1 << 31), 0, "full should be clear");

        ai.write_u32(0x00, 0x0000_2000);
        assert!(matches!(
            ai.write_u32(0x04, 0x0000_0200),
            AiRegWrite::DmaStarted { .. }
        ));
        let status_two = ai.read_u32(0x0C);
        assert_ne!(status_two & (1 << 30), 0, "busy should stay set");
        assert_ne!(status_two & (1 << 31), 0, "full should be set");
    }

    #[test]
    fn len_readback_tracks_remaining_dma() {
        let mut ai = Ai::new();
        ai.write_u32(0x08, 1); // AI_CONTROL enable
        ai.write_u32(0x10, 0x80); // AI_DACRATE
        ai.write_u32(0x00, 0x0000_3000);
        ai.write_u32(0x04, 0x0000_1000);

        let len_start = ai.read_u32(0x04);
        assert!(len_start > 0);

        let half = ai.dma_cycles / 2;
        ai.tick(half.max(1));
        let len_mid = ai.read_u32(0x04);
        assert!(len_mid > 0 && len_mid < len_start);

        ai.tick(ai.dma_cycles + 1);
        assert_eq!(ai.read_u32(0x04), 0);
    }

    #[test]
    fn control_bit_gates_dma_start() {
        let mut ai = Ai::new();
        ai.write_u32(0x10, 0x80); // AI_DACRATE
        ai.write_u32(0x00, 0x0000_4000);

        assert!(matches!(ai.write_u32(0x04, 0x0000_0200), AiRegWrite::None));
        assert_eq!(ai.read_u32(0x0C), 0);
    }
}
