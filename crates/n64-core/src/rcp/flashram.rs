/// FlashRAM — 128KB Flash memory for cartridge saves.
///
/// Used by games like Majora's Mask, Pokemon Stadium, Paper Mario.
/// Mapped at physical 0x0800_0000 (data) and 0x0801_0000 (command).
///
/// The flash chip has a state machine driven by commands written to
/// the command register. Reads from 0x0800_0000 return different data
/// depending on the current mode (status, silicon ID, or array data).
///
/// Data transfer happens via PI DMA when cart_addr is in the 0x0800_0000 range.

const FLASH_SIZE: usize = 0x20000; // 128 KB
const PAGE_SIZE: usize = 128; // 128 bytes per page
const SECTOR_SIZE: usize = 128 * PAGE_SIZE; // 16 KB per sector

#[derive(Debug, Clone, Copy, PartialEq)]
enum FlashMode {
    ReadArray,
    Status,
    ReadSiliconId,
    PageProgram,
    SectorErase,
    ChipErase,
}

pub struct FlashRam {
    data: Vec<u8>,
    page_buf: [u8; PAGE_SIZE],
    mode: FlashMode,
    status: u32,
    erase_sector: u16,
    /// Silicon ID returned during identification (MX29L1100)
    silicon_id: [u32; 2],
    /// Dirty flag for persistence
    pub dirty: bool,
}

impl FlashRam {
    pub fn new() -> Self {
        Self {
            data: vec![0xFF; FLASH_SIZE],
            page_buf: [0xFF; PAGE_SIZE],
            mode: FlashMode::ReadArray,
            status: 0,
            erase_sector: 0,
            silicon_id: [0x1111_8001, 0x00C2_001E], // MX29L1100
            dirty: false,
        }
    }

    /// Read the status/ID register at 0x0800_0000.
    pub fn read_status(&self) -> u32 {
        match self.mode {
            FlashMode::Status => self.status,
            FlashMode::ReadSiliconId => self.silicon_id[0],
            _ => 0,
        }
    }

    /// Process a command written to 0x0801_0000.
    pub fn command(&mut self, val: u32) {
        let cmd = (val >> 24) & 0xFF;
        match cmd {
            0xD2 => {
                // Status mode
                self.mode = FlashMode::Status;
                self.status = 0;
            }
            0xE1 => {
                // Read Silicon ID
                self.mode = FlashMode::ReadSiliconId;
                self.status |= 0x01; // ID read in progress
            }
            0xF0 => {
                // Read Array
                self.mode = FlashMode::ReadArray;
            }
            0x4B => {
                // Set sector erase — sector number in bits 15:9 of val
                self.mode = FlashMode::SectorErase;
                // Sector number from address bits: sector = (val >> 9) & 0x7F
                // But many games send sector as (val & 0xFFFF) with sector in upper bits
                self.erase_sector = ((val >> 9) & 0x7F) as u16;
            }
            0x3C => {
                // Chip erase mode
                self.mode = FlashMode::ChipErase;
            }
            0x78 => {
                // Execute erase
                match self.mode {
                    FlashMode::SectorErase => {
                        let offset = self.erase_sector as usize * SECTOR_SIZE;
                        let end = (offset + SECTOR_SIZE).min(FLASH_SIZE);
                        if offset < FLASH_SIZE {
                            self.data[offset..end].fill(0xFF);
                            self.dirty = true;
                            log::debug!(
                                "FlashRAM: erased sector {} (offset {:#X})",
                                self.erase_sector,
                                offset
                            );
                        }
                        self.status |= 0x08; // Erase success
                    }
                    FlashMode::ChipErase => {
                        self.data.fill(0xFF);
                        self.dirty = true;
                        log::debug!("FlashRAM: chip erase");
                        self.status |= 0x08;
                    }
                    _ => {}
                }
                self.mode = FlashMode::Status;
            }
            0xB4 => {
                // Page program mode
                self.mode = FlashMode::PageProgram;
            }
            0xA5 => {
                // Execute page program — page offset in bits 15:9 of val
                if self.mode == FlashMode::PageProgram {
                    let page = ((val >> 9) & 0x7F) as usize;
                    let offset = page * PAGE_SIZE;
                    if offset + PAGE_SIZE <= FLASH_SIZE {
                        // Flash programming: AND existing data with page buffer
                        // (flash can only clear bits, not set them — requires erase first)
                        for i in 0..PAGE_SIZE {
                            self.data[offset + i] &= self.page_buf[i];
                        }
                        self.dirty = true;
                        log::debug!("FlashRAM: programmed page {} (offset {:#X})", page, offset);
                    }
                    self.status |= 0x04; // Program success
                }
                self.mode = FlashMode::Status;
            }
            _ => {
                log::warn!("FlashRAM: unknown command {:#010X}", val);
            }
        }
    }

    /// DMA read: FlashRAM → RDRAM (game reads save data).
    /// Called during PI DMA when cart_addr is in 0x0800_0000 range.
    pub fn dma_read(&self, cart_offset: u32, dest: &mut [u8]) {
        match self.mode {
            FlashMode::ReadSiliconId => {
                // Return silicon ID (8 bytes: id[0] then id[1])
                let id_bytes_0 = self.silicon_id[0].to_be_bytes();
                let id_bytes_1 = self.silicon_id[1].to_be_bytes();
                for (i, byte) in dest.iter_mut().enumerate() {
                    *byte = if i < 4 {
                        id_bytes_0[i]
                    } else if i < 8 {
                        id_bytes_1[i - 4]
                    } else {
                        0
                    };
                }
            }
            FlashMode::Status => {
                // Return status as repeated 32-bit words
                let status_bytes = self.status.to_be_bytes();
                for (i, byte) in dest.iter_mut().enumerate() {
                    *byte = status_bytes[i % 4];
                }
            }
            FlashMode::ReadArray | _ => {
                // Read flash data
                let offset = (cart_offset as usize) & (FLASH_SIZE - 1);
                for (i, byte) in dest.iter_mut().enumerate() {
                    let addr = (offset + i) & (FLASH_SIZE - 1);
                    *byte = self.data[addr];
                }
            }
        }
    }

    /// DMA write: RDRAM → FlashRAM (game writes save data).
    /// In PageProgram mode, this loads the page buffer.
    pub fn dma_write(&mut self, src: &[u8]) {
        if self.mode == FlashMode::PageProgram {
            let len = src.len().min(PAGE_SIZE);
            self.page_buf[..len].copy_from_slice(&src[..len]);
            log::debug!("FlashRAM: loaded {} bytes into page buffer", len);
        }
    }

    /// Load flash data from a byte slice (e.g., from a save file).
    pub fn load_data(&mut self, data: &[u8]) {
        let len = data.len().min(FLASH_SIZE);
        self.data[..len].copy_from_slice(&data[..len]);
        self.dirty = false;
    }

    /// Get flash data for saving to disk.
    pub fn save_data(&self) -> &[u8] {
        &self.data
    }
}
