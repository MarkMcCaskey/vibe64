/// COP0 — System Control Coprocessor.
///
/// Manages TLB, exceptions, interrupts, and the Count/Compare timer.
/// See doc/n64_architecture.md for register details.
pub struct Cop0 {
    /// 32 COP0 registers (64-bit each)
    pub regs: [u64; 32],
}

// COP0 register indices
impl Cop0 {
    pub const INDEX: usize = 0;
    pub const RANDOM: usize = 1;
    pub const ENTRY_LO0: usize = 2;
    pub const ENTRY_LO1: usize = 3;
    pub const CONTEXT: usize = 4;
    pub const PAGE_MASK: usize = 5;
    pub const WIRED: usize = 6;
    pub const BAD_VADDR: usize = 8;
    pub const COUNT: usize = 9;
    pub const ENTRY_HI: usize = 10;
    pub const COMPARE: usize = 11;
    pub const STATUS: usize = 12;
    pub const CAUSE: usize = 13;
    pub const EPC: usize = 14;
    pub const PRID: usize = 15;
    pub const CONFIG: usize = 16;
    pub const LL_ADDR: usize = 17;
    pub const WATCH_LO: usize = 18;
    pub const WATCH_HI: usize = 19;
    pub const X_CONTEXT: usize = 20;
    pub const TAG_LO: usize = 28;
    pub const TAG_HI: usize = 29;
    pub const ERROR_EPC: usize = 30;
}

impl Cop0 {
    pub fn new() -> Self {
        let mut regs = [0u64; 32];
        // Config register defaults: Big-endian, data cache writeback
        regs[Self::CONFIG] = 0x0006_E463;
        // Random starts at 31 (decrements to Wired)
        regs[Self::RANDOM] = 31;
        Self { regs }
    }

    /// Read a COP0 register (MFC0)
    pub fn read_reg(&self, index: usize) -> u64 {
        self.regs[index & 0x1F]
    }

    /// Write a COP0 register (MTC0) with side effects
    pub fn write_reg(&mut self, index: usize, val: u64) {
        let index = index & 0x1F;
        match index {
            Self::COUNT => self.regs[Self::COUNT] = val,
            Self::COMPARE => {
                self.regs[Self::COMPARE] = val;
                // Writing Compare clears the timer interrupt (IP7)
                self.regs[Self::CAUSE] &= !(1 << 15);
            }
            Self::STATUS => {
                // Preserve read-only bits, write writable ones
                self.regs[Self::STATUS] = val;
            }
            Self::CAUSE => {
                // Only software interrupt bits (IP0, IP1) are writable
                let writable = val & 0x0000_0300;
                let preserved = self.regs[Self::CAUSE] & !0x0000_0300;
                self.regs[Self::CAUSE] = preserved | writable;
            }
            Self::WIRED => {
                log::debug!("MTC0 Wired = {}", val & 0x1F);
                self.regs[index] = val;
            }
            _ => self.regs[index] = val,
        }
    }

    /// Increment Count by 1 (Count increments every other PCycle)
    pub fn increment_count(&mut self) {
        self.regs[Self::COUNT] = self.regs[Self::COUNT].wrapping_add(1);
    }

    /// Check if Count == Compare → set IP7 in Cause
    pub fn check_timer_interrupt(&mut self) {
        if (self.regs[Self::COUNT] as u32) == (self.regs[Self::COMPARE] as u32) {
            self.regs[Self::CAUSE] |= 1 << 15; // IP7
        }
    }

    /// Advance Count/Random/timer state by multiple retired instructions.
    ///
    /// This is equivalent to calling `increment_count`, `decrement_random`,
    /// and `check_timer_interrupt` once per instruction.
    pub fn advance_by_instructions(&mut self, instructions: u32) {
        if instructions == 0 {
            return;
        }

        let old_count = self.regs[Self::COUNT] as u32;
        let compare = self.regs[Self::COMPARE] as u32;
        self.regs[Self::COUNT] = self.regs[Self::COUNT].wrapping_add(u64::from(instructions));

        // Timer interrupt asserts when Count matches Compare on any retired step.
        let delta = compare.wrapping_sub(old_count);
        if delta != 0 && delta <= instructions {
            self.regs[Self::CAUSE] |= 1 << 15; // IP7
        }

        let wired = (self.regs[Self::WIRED] & 0x1F) as u32;
        let mut random = (self.regs[Self::RANDOM] & 0x1F) as u32;
        for _ in 0..instructions {
            random = if random <= wired { 31 } else { random - 1 };
        }
        self.regs[Self::RANDOM] = u64::from(random);
    }

    /// Decrement Random register (wraps from Wired back to 31)
    pub fn decrement_random(&mut self) {
        let wired = self.regs[Self::WIRED] & 0x1F;
        let random = self.regs[Self::RANDOM] & 0x1F;
        self.regs[Self::RANDOM] = if random <= wired { 31 } else { random - 1 };
    }

    /// Set IP2 in Cause (external RCP interrupt from MI)
    pub fn set_ip2(&mut self) {
        self.regs[Self::CAUSE] |= 1 << 10;
    }

    /// Clear IP2 in Cause
    pub fn clear_ip2(&mut self) {
        self.regs[Self::CAUSE] &= !(1 << 10);
    }

    /// Check if an interrupt should be taken.
    /// Conditions: IE=1, EXL=0, ERL=0, and (Cause.IP & Status.IM) != 0
    pub fn interrupt_pending(&self) -> bool {
        let status = self.regs[Self::STATUS] as u32;
        let cause = self.regs[Self::CAUSE] as u32;

        let ie = status & 1; // Interrupt Enable
        let exl = (status >> 1) & 1; // Exception Level
        let erl = (status >> 2) & 1; // Error Level

        if ie == 0 || exl != 0 || erl != 0 {
            return false;
        }

        // IP bits are Cause[15:8], IM bits are Status[15:8]
        let ip = (cause >> 8) & 0xFF;
        let im = (status >> 8) & 0xFF;

        (ip & im) != 0
    }
}
