pub mod rom;

/// Cartridge — holds the ROM data.
///
/// The ROM is loaded once and served through the PI interface,
/// exactly like real hardware. ROM data is immutable.
pub struct Cartridge {
    pub data: Vec<u8>,
    pub header: rom::RomHeader,
}

impl Cartridge {
    pub fn new(data: Vec<u8>, header: rom::RomHeader) -> Self {
        Self { data, header }
    }

    pub fn read_u8(&self, addr: u32) -> u8 {
        let offset = (addr & 0x0FFF_FFFF) as usize;
        if offset < self.data.len() {
            self.data[offset]
        } else {
            0
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        let offset = (addr & 0x0FFF_FFFF) as usize;
        if offset + 3 < self.data.len() {
            u32::from_be_bytes([
                self.data[offset],
                self.data[offset + 1],
                self.data[offset + 2],
                self.data[offset + 3],
            ])
        } else {
            0
        }
    }
}
