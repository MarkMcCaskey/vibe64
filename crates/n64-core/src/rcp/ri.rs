/// RI — RDRAM Interface.
///
/// Configures RDRAM timing. Rarely accessed by games.
/// Registers at physical 0x0470_0000. Stubbed with safe defaults.

pub struct Ri {
    pub mode: u32,
    pub config: u32,
    pub current_load: u32,
    pub select: u32,
    pub refresh: u32,
    pub latency: u32,
}

impl Ri {
    pub fn new() -> Self {
        Self {
            mode: 0x0E,
            config: 0x40,
            current_load: 0,
            select: 0x14,
            refresh: 0x0006_3634,
            latency: 0x10,
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x00 => self.mode,
            0x04 => self.config,
            0x08 => self.current_load,
            0x0C => self.select,
            0x10 => self.refresh,
            0x14 => self.latency,
            _ => 0,
        }
    }

    pub fn write_u32(&mut self, addr: u32, val: u32) {
        match addr & 0x0F_FFFF {
            0x00 => self.mode = val,
            0x04 => self.config = val,
            0x08 => self.current_load = val,
            0x0C => self.select = val,
            0x10 => self.refresh = val,
            0x14 => self.latency = val,
            _ => {}
        }
    }
}
