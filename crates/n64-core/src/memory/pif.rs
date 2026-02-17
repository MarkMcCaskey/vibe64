/// PIF (Peripheral Interface) — handles boot ROM and controller communication.
///
/// Boot ROM: 1984 bytes at physical 0x1FC0_0000 (read-only)
/// PIF RAM:  64 bytes at physical 0x1FC0_07C0 (controller/EEPROM commands)
pub struct Pif {
    /// PIF Boot ROM (not included — emulators skip PIF boot)
    boot_rom: [u8; 1984],
    /// PIF RAM: 64 bytes for controller communication
    pub ram: [u8; 64],
}

impl Pif {
    pub fn new() -> Self {
        Self {
            boot_rom: [0u8; 1984],
            ram: [0u8; 64],
        }
    }

    pub fn read_boot_rom_u32(&self, addr: u32) -> u32 {
        let offset = (addr as usize) & 0x7FF;
        if offset + 3 < self.boot_rom.len() {
            u32::from_be_bytes([
                self.boot_rom[offset],
                self.boot_rom[offset + 1],
                self.boot_rom[offset + 2],
                self.boot_rom[offset + 3],
            ])
        } else {
            0
        }
    }

    pub fn read_ram_u32(&self, addr: u32) -> u32 {
        let offset = ((addr - 0x1FC0_07C0) as usize) & 0x3F;
        if offset + 3 < 64 {
            u32::from_be_bytes([
                self.ram[offset],
                self.ram[offset + 1],
                self.ram[offset + 2],
                self.ram[offset + 3],
            ])
        } else {
            0
        }
    }

    pub fn write_ram_u32(&mut self, addr: u32, val: u32) {
        let offset = ((addr - 0x1FC0_07C0) as usize) & 0x3F;
        if offset + 3 < 64 {
            let bytes = val.to_be_bytes();
            self.ram[offset..offset + 4].copy_from_slice(&bytes);
        }
    }
}
