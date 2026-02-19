/// PIF (Peripheral Interface) — handles boot ROM and controller communication.
///
/// Boot ROM: 1984 bytes at physical 0x1FC0_0000 (read-only)
/// PIF RAM:  64 bytes at physical 0x1FC0_07C0 (controller/EEPROM commands)
///
/// PIF RAM format for controller I/O:
///   Sequence of command blocks, each: [tx_len, rx_len, tx_data..., rx_data...]
///   Byte 63 (0x3F) is the PIF control byte.
///   tx_len = 0xFE means end of commands, 0xFD means skip this channel.
///
/// PIF control byte (byte 63) values:
///   0x01 = process joybus commands
///   0x02 = CIC challenge-response (CIC-6105 only)
///   0x08 = terminate boot process

use crate::cart::cic::CicVariant;

/// N64 controller button bit flags (big-endian button word).
pub mod buttons {
    pub const A: u16       = 0x8000;
    pub const B: u16       = 0x4000;
    pub const Z: u16       = 0x2000;
    pub const START: u16   = 0x1000;
    pub const D_UP: u16    = 0x0800;
    pub const D_DOWN: u16  = 0x0400;
    pub const D_LEFT: u16  = 0x0200;
    pub const D_RIGHT: u16 = 0x0100;
    pub const L: u16       = 0x0020;
    pub const R: u16       = 0x0010;
    pub const C_UP: u16    = 0x0008;
    pub const C_DOWN: u16  = 0x0004;
    pub const C_LEFT: u16  = 0x0002;
    pub const C_RIGHT: u16 = 0x0001;
}

/// Live controller state, updated by the frontend from keyboard input.
#[derive(Default)]
pub struct ControllerState {
    pub buttons: u16,
    pub stick_x: i8,
    pub stick_y: i8,
}

/// EEPROM type for save data.
#[derive(Clone, Copy, PartialEq)]
pub enum EepromType {
    None,
    Eeprom4K,   // 512 bytes (64 blocks × 8 bytes)
    Eeprom16K,  // 2048 bytes (256 blocks × 8 bytes)
}

pub struct Pif {
    /// PIF Boot ROM (not included — emulators skip PIF boot)
    boot_rom: [u8; 1984],
    /// PIF RAM: 64 bytes for controller communication
    pub ram: [u8; 64],
    /// CIC variant (needed for CIC-6105 challenge-response)
    cic: CicVariant,
    /// Controller 1 state (set by frontend)
    pub controller: ControllerState,
    /// EEPROM save data (accessed via joybus channel 4)
    pub eeprom: Vec<u8>,
    pub eeprom_type: EepromType,
    /// Debug: count of unhandled joybus commands
    pub unhandled_cmds: std::collections::HashMap<u8, u32>,
}

impl Pif {
    pub fn new() -> Self {
        Self {
            boot_rom: [0u8; 1984],
            ram: [0u8; 64],
            cic: CicVariant::Unknown,
            controller: ControllerState::default(),
            eeprom: Vec::new(),
            eeprom_type: EepromType::None,
            unhandled_cmds: std::collections::HashMap::new(),
        }
    }

    /// Configure EEPROM type and allocate storage.
    pub fn set_eeprom(&mut self, etype: EepromType) {
        self.eeprom_type = etype;
        let size = match etype {
            EepromType::None => 0,
            EepromType::Eeprom4K => 512,
            EepromType::Eeprom16K => 2048,
        };
        self.eeprom = vec![0xFFu8; size];
    }

    pub fn set_cic(&mut self, cic: CicVariant) {
        self.cic = cic;
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
    /// Checks the control byte (byte 63) to determine which operation to perform:
    /// - Bit 0 (0x01): process joybus commands (controller/EEPROM)
    /// - Bit 1 (0x02): CIC challenge-response (CIC-6105 anti-piracy)
    pub fn process_commands(&mut self) {
        let control = self.ram[63];

        // Process joybus commands (controller I/O)
        if control & 0x01 != 0 || control == 0 {
            self.process_joybus();
        }

        // CIC-6105 challenge-response
        if control & 0x02 != 0 && self.cic == CicVariant::Cic6105 {
            self.cic_6105_challenge();
        }

        // Clear PIF control byte
        self.ram[63] = 0;
    }

    /// Walk joybus command buffer and respond to controller queries.
    fn process_joybus(&mut self) {
        let mut i = 0usize;
        let mut channel = 0u8;


        while i < 63 {
            let tx_len = self.ram[i];

            // Special markers
            if tx_len == 0xFE { break; }          // End of commands
            if tx_len == 0x00 { i += 1; channel += 1; continue; } // Skip channel
            if tx_len == 0xFD { i += 1; channel += 1; continue; } // Skip channel
            // Bit 7 set = channel already processed / skip (e.g. 0xFF = no device)
            if tx_len & 0x80 != 0 { i += 1; channel += 1; continue; }

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
                    // Device info / reset
                    if rx >= 3 {
                        if channel == 0 {
                            // Type: 0x0500 = standard controller (big-endian)
                            // Status: 0x00 = no controller pak
                            self.ram[rx_start] = 0x05;
                            self.ram[rx_start + 1] = 0x00;
                            self.ram[rx_start + 2] = 0x00;
                        } else if channel == 4 && self.eeprom_type != EepromType::None {
                            // EEPROM device info
                            let type_id: u16 = match self.eeprom_type {
                                EepromType::Eeprom4K => 0x0080,
                                EepromType::Eeprom16K => 0x00C0,
                                EepromType::None => unreachable!(),
                            };
                            self.ram[rx_start] = (type_id >> 8) as u8;
                            self.ram[rx_start + 1] = (type_id & 0xFF) as u8;
                            self.ram[rx_start + 2] = 0x00;
                        } else {
                            // No device: set error flag (bit 7 of rx_len)
                            self.ram[i + 1] = rx_len | 0x80;
                        }
                    }
                }
                0x01 => {
                    // Read controller buttons: 4 bytes of button/stick state
                    if rx >= 4 {
                        if channel == 0 {
                            self.ram[rx_start] = (self.controller.buttons >> 8) as u8;
                            self.ram[rx_start + 1] = (self.controller.buttons & 0xFF) as u8;
                            self.ram[rx_start + 2] = self.controller.stick_x as u8;
                            self.ram[rx_start + 3] = self.controller.stick_y as u8;
                        } else {
                            self.ram[i + 1] = rx_len | 0x80;
                        }
                    }
                }
                0x04 => {
                    // EEPROM read block (tx: [cmd, block#], rx: 8 bytes)
                    if channel == 4 && self.eeprom_type != EepromType::None && tx >= 2 && rx >= 8 {
                        let block = self.ram[cmd_start + 1] as usize;
                        let offset = block * 8;
                        if offset + 8 <= self.eeprom.len() {
                            for b in 0..8 {
                                self.ram[rx_start + b] = self.eeprom[offset + b];
                            }
                        } else {
                            // Out of range — return zeros
                            for b in 0..8 {
                                self.ram[rx_start + b] = 0;
                            }
                        }
                    } else {
                        self.ram[i + 1] = rx_len | 0x80;
                    }
                }
                0x05 => {
                    // EEPROM write block (tx: [cmd, block#, 8 data bytes], rx: 1 status)
                    if channel == 4 && self.eeprom_type != EepromType::None && tx >= 10 && rx >= 1 {
                        let block = self.ram[cmd_start + 1] as usize;
                        let offset = block * 8;
                        if offset + 8 <= self.eeprom.len() {
                            for b in 0..8 {
                                self.eeprom[offset + b] = self.ram[cmd_start + 2 + b];
                            }
                        }
                        self.ram[rx_start] = 0x00; // Success
                    } else {
                        self.ram[i + 1] = rx_len | 0x80;
                    }
                }
                _ => {
                    // Unknown command — flag as error
                    *self.unhandled_cmds.entry(cmd).or_default() += 1;
                    self.ram[i + 1] = rx_len | 0x80;
                }
            }

            i = rx_start + rx;
            channel += 1;
        }
    }

    /// CIC-NUS-6105 challenge-response algorithm.
    ///
    /// The game sends a 30-nibble challenge in PIF RAM bytes 0x30-0x3E
    /// and expects a valid 30-nibble response in the same location.
    /// If the response is wrong, the game deliberately freezes.
    ///
    /// Algorithm from: https://github.com/pj64team/Project64-1.6-Plus/blob/main/n64_cic_nus_6105.c
    fn cic_6105_challenge(&mut self) {
        const LUT0: [u8; 16] = [
            0x4, 0x7, 0xA, 0x7, 0xE, 0x5, 0xE, 0x1,
            0xC, 0xF, 0x8, 0xF, 0x6, 0x3, 0x6, 0x9,
        ];
        const LUT1: [u8; 16] = [
            0x4, 0x1, 0xA, 0x7, 0xE, 0x5, 0xE, 0x1,
            0xC, 0x9, 0x8, 0x5, 0x6, 0x3, 0xC, 0x9,
        ];

        // Extract 30 nibbles from PIF RAM bytes 0x30-0x3E
        let mut challenge = [0u8; 30];
        for i in 0..15 {
            challenge[i * 2] = (self.ram[0x30 + i] >> 4) & 0x0F;
            challenge[i * 2 + 1] = self.ram[0x30 + i] & 0x0F;
        }

        // Compute response
        let mut response = [0u8; 30];
        let mut key: u8 = 0x0B;
        let mut use_lut1 = false;

        for i in 0..30 {
            let lut = if use_lut1 { &LUT1 } else { &LUT0 };
            response[i] = (key.wrapping_add(5u8.wrapping_mul(challenge[i]))) & 0x0F;
            key = lut[response[i] as usize];
            let sgn = (response[i] >> 3) & 1;
            let mag = if sgn == 1 { (!response[i]) & 0x7 } else { response[i] & 0x7 };
            let mut mod_val = if mag % 3 == 1 { sgn } else { 1 - sgn };
            if use_lut1 && (response[i] == 0x1 || response[i] == 0x9) {
                mod_val = 1;
            }
            if use_lut1 && (response[i] == 0xB || response[i] == 0xE) {
                mod_val = 0;
            }
            use_lut1 = mod_val == 1;
        }

        // Write 30 nibbles back to PIF RAM bytes 0x30-0x3E
        for i in 0..15 {
            self.ram[0x30 + i] = (response[i * 2] << 4) | response[i * 2 + 1];
        }

        log::trace!("CIC-6105 challenge processed");
    }
}
