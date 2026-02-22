use memmap2::MmapMut;

/// RDRAM — the N64's main system memory.
///
/// 4 MB on base N64, 8 MB with Expansion Pak.
/// Mapped at physical address 0x0000_0000.
pub struct Rdram {
    data: MmapMut,
    /// Debug: track highest address written above 0xA0000
    pub debug_high_write: u32,
}

const RDRAM_SIZE: usize = 8 * 1024 * 1024; // 8 MB (Expansion Pak)

impl Rdram {
    pub fn new() -> Self {
        let mut data = MmapMut::map_anon(RDRAM_SIZE).expect("allocate RDRAM mmap");
        data.fill(0);
        Self {
            data,
            debug_high_write: 0,
        }
    }

    #[inline(always)]
    pub fn read_u8(&self, addr: u32) -> u8 {
        let index = (addr as usize) & (RDRAM_SIZE - 1);
        self.data[index]
    }

    #[inline(always)]
    pub fn read_u32(&self, addr: u32) -> u32 {
        let index = (addr as usize) & (RDRAM_SIZE - 1);
        u32::from_be_bytes([
            self.data[index],
            self.data[index + 1],
            self.data[index + 2],
            self.data[index + 3],
        ])
    }

    #[inline(always)]
    pub fn write_u8(&mut self, addr: u32, val: u8) {
        let index = (addr as usize) & (RDRAM_SIZE - 1);
        self.data[index] = val;
        if addr > 0xA0000 && addr > self.debug_high_write {
            self.debug_high_write = addr;
        }
    }

    /// Raw access to RDRAM data (for framebuffer reading, DMA, etc.)
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    /// Mutable access to RDRAM data (for renderer writing to framebuffer).
    pub fn data_mut(&mut self) -> &mut [u8] {
        &mut self.data
    }

    /// Base pointer for dynarec fastmem accesses.
    pub fn fastmem_base(&mut self) -> *mut u8 {
        self.data.as_mut_ptr()
    }

    /// Address mask for mirrored 8MB RDRAM accesses.
    pub fn fastmem_mask(&self) -> u64 {
        (RDRAM_SIZE - 1) as u64
    }

    #[inline(always)]
    pub fn write_u32(&mut self, addr: u32, val: u32) {
        let index = (addr as usize) & (RDRAM_SIZE - 1);
        let bytes = val.to_be_bytes();
        self.data[index..index + 4].copy_from_slice(&bytes);
        if addr > 0xA0000 && addr > self.debug_high_write {
            self.debug_high_write = addr;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rdram_read_write_roundtrip() {
        let mut rdram = Rdram::new();

        rdram.write_u32(0x0000_0000, 0xDEAD_BEEF);
        assert_eq!(rdram.read_u32(0x0000_0000), 0xDEAD_BEEF);

        rdram.write_u32(0x0000_0100, 0x1234_5678);
        assert_eq!(rdram.read_u32(0x0000_0100), 0x1234_5678);

        rdram.write_u32(0x007F_FFFC, 0xCAFE_BABE);
        assert_eq!(rdram.read_u32(0x007F_FFFC), 0xCAFE_BABE);

        rdram.write_u32(0x0080_0000, 0xAAAA_BBBB);
        assert_eq!(rdram.read_u32(0x0000_0000), 0xAAAA_BBBB);

        rdram.write_u8(0x0000_0200, 0x42);
        assert_eq!(rdram.read_u8(0x0000_0200), 0x42);

        rdram.write_u32(0x0000_0300, 0x01_02_03_04);
        assert_eq!(rdram.read_u8(0x0000_0300), 0x01);
        assert_eq!(rdram.read_u8(0x0000_0301), 0x02);
        assert_eq!(rdram.read_u8(0x0000_0302), 0x03);
        assert_eq!(rdram.read_u8(0x0000_0303), 0x04);
    }
}
