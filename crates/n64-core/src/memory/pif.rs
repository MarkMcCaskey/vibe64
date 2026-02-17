/// PIF (Peripheral Interface) — handles boot ROM and controller communication.
///
/// Boot ROM: 1984 bytes at physical 0x1FC0_0000 (read-only)
/// PIF RAM:  64 bytes at physical 0x1FC0_07C0 (controller/EEPROM commands)
///
/// PIF RAM format for controller I/O:
///   Sequence of command blocks, each: [tx_len, rx_len, tx_data..., rx_data...]
///   Byte 63 (0x3F) is the PIF control byte.
///   tx_len = 0xFE means end of commands, 0xFD means skip this channel.
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

    /// Process PIF commands after SI DMA writes PIF RAM.
    ///
    /// Walks the command buffer and responds to controller queries.
    /// For now, all 4 controller ports report "no device connected."
    pub fn process_commands(&mut self) {
        let mut i = 0usize;
        let mut channel = 0u8;

        while i < 63 {
            let tx_len = self.ram[i];

            // Special markers
            if tx_len == 0xFE { break; }          // End of commands
            if tx_len == 0x00 { i += 1; channel += 1; continue; } // Skip channel
            if tx_len == 0xFD { i += 1; channel += 1; continue; } // Skip channel

            let tx = (tx_len & 0x3F) as usize;
            if i + 1 >= 63 { break; }
            let rx_len = self.ram[i + 1];
            let rx = (rx_len & 0x3F) as usize;

            let cmd_start = i + 2;
            let rx_start = cmd_start + tx;

            if rx_start + rx > 63 { break; }
            if tx == 0 { i = rx_start + rx; channel += 1; continue; }

            let cmd = self.ram[cmd_start];

            match cmd {
                0x00 | 0xFF => {
                    // Controller info / reset: respond with "no pak" for
                    // standard N64 controller on port 0, nothing on others
                    if rx >= 3 {
                        if channel == 0 {
                            // Type: 0x0005 = standard controller, pak status: 0x01
                            self.ram[rx_start] = 0x05;
                            self.ram[rx_start + 1] = 0x00;
                            self.ram[rx_start + 2] = 0x01;
                        } else {
                            // No controller: set error flag (bit 7 of rx_len)
                            self.ram[i + 1] = rx_len | 0x80;
                        }
                    }
                }
                0x01 => {
                    // Read controller buttons: 4 bytes of button/stick state
                    if rx >= 4 {
                        if channel == 0 {
                            // All buttons released, stick centered
                            self.ram[rx_start] = 0x00;
                            self.ram[rx_start + 1] = 0x00;
                            self.ram[rx_start + 2] = 0x00; // stick X
                            self.ram[rx_start + 3] = 0x00; // stick Y
                        } else {
                            self.ram[i + 1] = rx_len | 0x80;
                        }
                    }
                }
                _ => {
                    // Unknown command — flag as error
                    self.ram[i + 1] = rx_len | 0x80;
                }
            }

            i = rx_start + rx;
            channel += 1;
        }

        // Clear PIF control byte
        self.ram[63] = 0;
    }
}
