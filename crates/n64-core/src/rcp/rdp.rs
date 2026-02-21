/// RDP — Reality Display Processor.
///
/// The hardware rasterizer. Receives commands from the RSP.
/// Registers at physical 0x0410_0000.
///
/// In HLE mode the display list is processed by the software renderer,
/// so the RDP is never actually busy. We still model DPC status control
/// bits because libultra reads/writes DPC_STATUS for freeze/flush logic.

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
            // cbuf_ready (bit 7) set in HLE mode
            status: 0x80,
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
                if val & 0x04 != 0 { self.status &= !0x02; } // Clear FREEZE
                if val & 0x08 != 0 { self.status |= 0x02; }  // Set FREEZE
                if val & 0x10 != 0 { self.status &= !0x04; } // Clear FLUSH
                if val & 0x20 != 0 { self.status |= 0x04; }  // Set FLUSH
                // Bits 6-9 clear counters on hardware; no-op in HLE.
            }
            _ => {}
        }
    }
}
