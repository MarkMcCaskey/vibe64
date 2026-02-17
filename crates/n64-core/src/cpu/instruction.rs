/// A decoded MIPS III instruction.
///
/// This is a thin wrapper around the raw 32-bit opcode with named
/// accessor methods so that execution code reads cleanly.
///
/// MIPS instruction formats (all 32-bit):
///
/// R-type: [opcode:6][rs:5][rt:5][rd:5][sa:5][funct:6]
/// I-type: [opcode:6][rs:5][rt:5][imm:16]
/// J-type: [opcode:6][target:26]
///
/// See doc/mips_iii_instructions.md for the full opcode tables.
#[derive(Clone, Copy, Debug)]
pub struct Instruction(pub u32);

impl Instruction {
    pub fn decode(raw: u32) -> Self {
        Self(raw)
    }

    // TODO(human): Implement bit-field accessor methods
    //
    // You need these accessors:
    //   opcode() -> u32     : bits [31:26] — primary opcode
    //   rs()     -> usize   : bits [25:21] — source register
    //   rt()     -> usize   : bits [20:16] — target register
    //   rd()     -> usize   : bits [15:11] — destination register
    //   sa()     -> u32     : bits [10:6]  — shift amount
    //   funct()  -> u32     : bits [5:0]   — function code (R-type)
    //   imm()    -> u16     : bits [15:0]  — immediate value (I-type)
    //   imm_sign_ext() -> u64 : sign-extended immediate
    //   target() -> u32     : bits [25:0]  — jump target (J-type)
    //
    // Hint: use >> (right shift) and & (mask) to extract each field.
    // Example: opcode is the top 6 bits, so (self.0 >> 26) & 0x3F

    /// Raw opcode value (for debugging)
    pub fn raw(self) -> u32 {
        self.0
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#010X}", self.0)
    }
}
