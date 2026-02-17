/// RDRAM — the N64's main system memory.
///
/// 4 MB on base N64, 8 MB with Expansion Pak.
/// Mapped at physical address 0x0000_0000.
pub struct Rdram {
    data: Vec<u8>,
}

const RDRAM_SIZE: usize = 8 * 1024 * 1024; // 8 MB (Expansion Pak)

impl Rdram {
    pub fn new() -> Self {
        Self {
            data: vec![0u8; RDRAM_SIZE],
        }
    }

    pub fn read_u8(&self, addr: u32) -> u8 {
        let index = (addr as usize) & (RDRAM_SIZE - 1);
        self.data[index]
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        let index = (addr as usize) & (RDRAM_SIZE - 1);
        u32::from_be_bytes([
            self.data[index],
            self.data[index + 1],
            self.data[index + 2],
            self.data[index + 3],
        ])
    }

    pub fn write_u8(&mut self, addr: u32, val: u8) {
        let index = (addr as usize) & (RDRAM_SIZE - 1);
        self.data[index] = val;
    }

    pub fn write_u32(&mut self, addr: u32, val: u32) {
        let index = (addr as usize) & (RDRAM_SIZE - 1);
        let bytes = val.to_be_bytes();
        self.data[index..index + 4].copy_from_slice(&bytes);
    }
}
