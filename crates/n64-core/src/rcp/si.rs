/// SI — Serial Interface.
///
/// Handles communication with controllers and EEPROM through PIF RAM.
/// Registers at physical 0x0480_0000. Stubbed for now.

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

    pub fn write_u32(&mut self, addr: u32, val: u32) {
        match addr & 0x0F_FFFF {
            0x00 => self.dram_addr = val & 0x00FF_FFFF,
            0x18 => {
                // Writing to SI_STATUS clears SI interrupt
                // (handled by caller via MI)
            }
            _ => {}
        }
    }
}
