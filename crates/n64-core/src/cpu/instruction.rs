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

    pub fn opcode(self) -> u32 {
        (self.0 >> 26) & 0x3F
    }
    pub fn rs(self) -> usize {
        ((self.0 >> 21) & 0x1F) as usize
    }
    pub fn rt(self) -> usize {
        ((self.0 >> 16) & 0x1F) as usize
    }
    pub fn rd(self) -> usize {
        ((self.0 >> 11) & 0x1F) as usize
    }
    pub fn sa(self) -> usize {
        ((self.0 >> 6) & 0x1F) as usize
    }
    pub fn funct(self) -> u32 {
        self.0 & 0x3F
    }
    pub fn imm(self) -> u16 {
        self.0 as u16
    }
    pub fn imm_sign_ext(self) -> u64 {
        self.imm() as i16 as i64 as u64
    }
    pub fn target(self) -> u32 {
        self.0 & 0x03FF_FFFF
    }
    pub fn raw(self) -> u32 {
        self.0
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#010X}", self.0)
    }
}
