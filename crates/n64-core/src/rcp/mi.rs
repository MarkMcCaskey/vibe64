/// MI — MIPS Interface (Interrupt Controller).
///
/// Aggregates interrupts from all RCP components (SP, SI, AI, VI, PI, DP)
/// into a single interrupt line to the CPU (COP0 Cause IP2).
///
/// Registers at physical 0x0430_0000.

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum MiInterrupt {
    SP = 0,
    SI = 1,
    AI = 2,
    VI = 3,
    PI = 4,
    DP = 5,
}

pub struct Mi {
    pub mode: u32,
    pub version: u32,
    /// MI_INTR: which interrupts are currently raised (6 bits)
    pub intr: u8,
    /// MI_MASK: which interrupts are enabled (6 bits)
    pub intr_mask: u8,
}

impl Mi {
    pub fn new() -> Self {
        Self {
            mode: 0,
            version: 0x0202_0102, // Retail N64
            intr: 0,
            intr_mask: 0,
        }
    }

    pub fn read_u32(&self, addr: u32) -> u32 {
        match addr & 0x0F_FFFF {
            0x00 => self.mode,
            0x04 => self.version,
            0x08 => self.intr as u32,
            0x0C => self.intr_mask as u32,
            _ => 0,
        }
    }

    pub fn write_u32(&mut self, addr: u32, val: u32) {
        match addr & 0x0F_FFFF {
            0x00 => self.write_mode(val),
            0x0C => self.write_mask(val),
            _ => {}
        }
    }

    /// MI_MODE write: set/clear pairs + DP interrupt clear.
    ///
    /// Bit 7: clear init mode, Bit 8: set init mode
    /// Bit 9: clear ebus test, Bit 10: set ebus test
    /// Bit 11: clear DP interrupt
    /// Bit 12: clear RDRAM reg, Bit 13: set RDRAM reg
    fn write_mode(&mut self, val: u32) {
        // Init length (bits 6:0)
        self.mode = (self.mode & !0x7F) | (val & 0x7F);

        if val & (1 << 7) != 0 { self.mode &= !(1 << 7); }  // Clear init mode
        if val & (1 << 8) != 0 { self.mode |= 1 << 7; }      // Set init mode
        if val & (1 << 9) != 0 { self.mode &= !(1 << 8); }   // Clear ebus test
        if val & (1 << 10) != 0 { self.mode |= 1 << 8; }     // Set ebus test

        // Bit 11: Clear DP interrupt
        if val & (1 << 11) != 0 {
            self.clear_interrupt(MiInterrupt::DP);
        }

        if val & (1 << 12) != 0 { self.mode &= !(1 << 9); }  // Clear RDRAM reg
        if val & (1 << 13) != 0 { self.mode |= 1 << 9; }     // Set RDRAM reg
    }

    pub fn set_interrupt(&mut self, irq: MiInterrupt) {
        self.intr |= 1 << (irq as u8);
    }

    pub fn clear_interrupt(&mut self, irq: MiInterrupt) {
        self.intr &= !(1 << (irq as u8));
    }

    /// Returns true if any unmasked interrupt is pending.
    pub fn interrupt_pending(&self) -> bool {
        (self.intr & self.intr_mask) != 0
    }

    /// MI_MASK write: set/clear pairs.
    /// Bit 0 clears SP mask, bit 1 sets SP mask, etc.
    fn write_mask(&mut self, val: u32) {
        for i in 0..6u8 {
            let bit_pair = (val >> (i * 2)) & 0x03;
            match bit_pair {
                0b01 => self.intr_mask &= !(1 << i), // Clear
                0b10 => self.intr_mask |= 1 << i,    // Set
                _ => {}
            }
        }
    }
}
