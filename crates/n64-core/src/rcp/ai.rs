/// AI — Audio Interface.
///
/// DMA-based audio output. Double-buffered.
/// Registers at physical 0x0450_0000.
/// Stub for now.

pub struct Ai {
    pub dram_addr: u32,
    pub len: u32,
    pub control: u32,
    pub status: u32,
    pub dacrate: u32,
    pub bitrate: u32,
}

impl Ai {
    pub fn new() -> Self {
        Self {
            dram_addr: 0,
            len: 0,
            control: 0,
            status: 0,
            dacrate: 0,
            bitrate: 0,
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x04 => self.len,
            0x0C => self.status,
            _ => 0,
        }
    }

    pub fn write_u32(&mut self, addr: u32, val: u32) {
        match addr & 0x0F_FFFF {
            0x00 => self.dram_addr = val & 0x00FF_FFFF,
            0x04 => self.len = val & 0x0003_FFF8,
            0x08 => self.control = val & 1,
            0x0C => {
                // Writing to status clears AI interrupt
                // (handled by caller via MI)
            }
            0x10 => self.dacrate = val & 0x3FFF,
            0x14 => self.bitrate = val & 0xF,
            _ => {}
        }
    }
}
