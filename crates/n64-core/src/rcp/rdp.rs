/// RDP — Reality Display Processor.
///
/// The hardware rasterizer. Receives commands from the RSP.
/// Registers at physical 0x0410_0000.
/// Fully stubbed for now.

pub struct Rdp {
    pub start: u32,
    pub end: u32,
    pub current: u32,
    pub status: u32,
}

impl Rdp {
    pub fn new() -> Self {
        Self {
            start: 0,
            end: 0,
            current: 0,
            status: 0,
        }
    }

    pub fn read_reg_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x00 => self.start,
            0x04 => self.end,
            0x08 => self.current,
            0x0C => self.status,
            _ => 0,
        }
    }

    pub fn write_reg_u32(&mut self, addr: u32, val: u32) {
        match addr & 0x0F_FFFF {
            0x00 => self.start = val & 0x00FF_FFF8,
            0x04 => self.end = val & 0x00FF_FFF8,
            0x0C => {
                // DPC_STATUS write: set/clear pairs
                if val & 0x01 != 0 { self.status &= !0x01; } // Clear XBUS
                if val & 0x02 != 0 { self.status |= 0x01; }  // Set XBUS
            }
            _ => {}
        }
    }
}
