/// COP1 — Floating Point Unit.
///
/// 32 x 64-bit FPU registers and two control registers (FCR0, FCR31).
/// Implementation deferred to Phase 8.
pub struct Cop1 {
    /// 32 floating-point registers (64-bit each)
    pub fpr: [u64; 32],
    /// FCR0: FPU Implementation/Revision register (read-only)
    pub fcr0: u32,
    /// FCR31: FPU Control/Status register (rounding mode, condition flag, exceptions)
    pub fcr31: u32,
}

impl Cop1 {
    pub fn new() -> Self {
        Self {
            fpr: [0u64; 32],
            fcr0: 0x0000_0A00, // VR4300 FPU revision
            fcr31: 0,
        }
    }
}
