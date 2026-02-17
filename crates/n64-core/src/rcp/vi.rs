/// VI — Video Interface.
///
/// Controls video output: framebuffer address, resolution, timing.
/// Generates an interrupt once per frame at the configured scanline.
/// Registers at physical 0x0440_0000.

use super::mi::{Mi, MiInterrupt};

pub struct Vi {
    pub ctrl: u32,
    pub origin: u32,
    pub width: u32,
    pub v_intr: u32,    // Scanline that triggers VI interrupt
    pub v_current: u32, // Current scanline
    pub burst: u32,
    pub v_sync: u32,    // Total scanlines per frame (525 NTSC, 625 PAL)
    pub h_sync: u32,
    pub h_sync_leap: u32,
    pub h_video: u32,
    pub v_video: u32,
    pub v_burst: u32,
    pub x_scale: u32,
    pub y_scale: u32,
    /// Internal cycle counter for scanline timing
    cycles: u64,
}

impl Vi {
    pub fn new() -> Self {
        Self {
            ctrl: 0,
            origin: 0,
            width: 320,
            v_intr: 0x200, // Default interrupt line
            v_current: 0,
            burst: 0,
            v_sync: 525, // NTSC
            h_sync: 0,
            h_sync_leap: 0,
            h_video: 0,
            v_video: 0,
            v_burst: 0,
            x_scale: 0,
            y_scale: 0,
            cycles: 0,
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x00 => self.ctrl,
            0x04 => self.origin,
            0x08 => self.width,
            0x0C => self.v_intr,
            0x10 => self.v_current,
            0x14 => self.burst,
            0x18 => self.v_sync,
            0x1C => self.h_sync,
            0x20 => self.h_sync_leap,
            0x24 => self.h_video,
            0x28 => self.v_video,
            0x2C => self.v_burst,
            0x30 => self.x_scale,
            0x34 => self.y_scale,
            _ => 0,
        }
    }

    pub fn write_u32(&mut self, addr: u32, val: u32, mi: &mut Mi) {
        match addr & 0x0F_FFFF {
            0x00 => self.ctrl = val,
            0x04 => self.origin = val & 0x00FF_FFFF,
            0x08 => self.width = val & 0xFFF,
            0x0C => self.v_intr = val & 0x3FF,
            0x10 => {
                // Writing to V_CURRENT clears the VI interrupt
                mi.clear_interrupt(MiInterrupt::VI);
            }
            0x14 => self.burst = val,
            0x18 => self.v_sync = val & 0x3FF,
            0x1C => self.h_sync = val,
            0x20 => self.h_sync_leap = val,
            0x24 => self.h_video = val,
            0x28 => self.v_video = val,
            0x2C => self.v_burst = val,
            0x30 => self.x_scale = val,
            0x34 => self.y_scale = val,
            _ => {}
        }
    }

    /// Advance VI by `cycles` CPU cycles. Fires VI interrupt on the
    /// configured scanline.
    ///
    /// ~93.75 MHz CPU / 525 lines / 60 fps ≈ ~2970 cycles per scanline (NTSC)
    pub fn tick(&mut self, cycles: u64, mi: &mut Mi) {
        const CYCLES_PER_SCANLINE: u64 = 2970;

        self.cycles += cycles;
        while self.cycles >= CYCLES_PER_SCANLINE {
            self.cycles -= CYCLES_PER_SCANLINE;
            self.v_current = (self.v_current + 1) % self.v_sync.max(1);

            if self.v_current == self.v_intr {
                mi.set_interrupt(MiInterrupt::VI);
            }
        }
    }
}
