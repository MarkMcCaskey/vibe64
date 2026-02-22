/// COP1 — Floating Point Unit.
///
/// 32 x 64-bit FPU registers and two control registers (FCR0, FCR31).
///
/// The FPU condition flag lives in FCR31 bit 23. FP compare instructions
/// (C.cond) set/clear this flag, and BC1T/BC1F branch on it. This is
/// fundamentally different from integer comparisons (SLT → register),
/// because the FPU was designed as a separate coprocessor with its own
/// state — the condition flag is its way of signaling to the CPU.
pub struct Cop1 {
    /// 32 floating-point registers (64-bit each).
    /// In 32-bit mode (FR=0 in COP0 Status), only even registers hold
    /// doubles; odd registers are the upper halves. Most N64 games use
    /// FR=0. We store all 32 as 64-bit and handle access width in the ops.
    pub fpr: [u64; 32],
    /// FCR0: FPU Implementation/Revision register (read-only)
    pub fcr0: u32,
    /// FCR31: FPU Control/Status register
    ///   Bits [1:0]: Rounding mode (0=nearest, 1=toward zero, 2=+inf, 3=-inf)
    ///   Bit 23: Condition flag (set by C.cond, read by BC1T/BC1F)
    pub fcr31: u32,
}

impl Cop1 {
    /// FCR31 condition flag bit position
    pub const CONDITION_BIT: u32 = 1 << 23;

    pub fn new() -> Self {
        Self {
            fpr: [0u64; 32],
            fcr0: 0x0000_0A00, // VR4300 FPU revision
            fcr31: 0,
        }
    }

    /// Get the FPU condition flag (FCR31 bit 23)
    pub fn condition(&self) -> bool {
        self.fcr31 & Self::CONDITION_BIT != 0
    }

    /// Set the FPU condition flag
    pub fn set_condition(&mut self, cond: bool) {
        if cond {
            self.fcr31 |= Self::CONDITION_BIT;
        } else {
            self.fcr31 &= !Self::CONDITION_BIT;
        }
    }

    // ─── Register access helpers ────────────────────────────────

    /// Read a single-precision float from FPR[n]
    pub fn read_f32(&self, n: usize) -> f32 {
        f32::from_bits(self.fpr[n] as u32)
    }

    /// Write a single-precision float to FPR[n]
    pub fn write_f32(&mut self, n: usize, val: f32) {
        self.fpr[n] = val.to_bits() as u64;
    }

    /// Read a double-precision float from FPR[n] (FR=0 mode: even/odd pair).
    ///
    /// N64 uses FR=0 mode where doubles span two 32-bit registers:
    /// FPR[n] holds the low 32 bits, FPR[n+1] holds the high 32 bits.
    pub fn read_f64(&self, n: usize) -> f64 {
        let low = self.fpr[n] as u32 as u64;
        let high = self.fpr[n | 1] as u32 as u64;
        f64::from_bits((high << 32) | low)
    }

    /// Write a double-precision float to FPR[n] (FR=0 mode: even/odd pair).
    pub fn write_f64(&mut self, n: usize, val: f64) {
        let bits = val.to_bits();
        self.fpr[n] = bits & 0xFFFF_FFFF; // low 32 bits → even register
        self.fpr[n | 1] = (bits >> 32) & 0xFFFF_FFFF; // high 32 bits → odd register
    }
}
