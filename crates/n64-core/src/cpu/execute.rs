use crate::bus::Bus;
use crate::cpu::instruction::Instruction;
use crate::cpu::vr4300::Vr4300;

impl Vr4300 {
    /// Execute a decoded instruction.
    ///
    /// `current_pc` is the PC of this instruction (before advancement),
    /// needed for branch target calculations and exception handling.
    pub fn execute(&mut self, _instr: Instruction, _bus: &mut impl Bus, _current_pc: u64) {
        // This will be filled in by the user as part of instruction
        // execution (Phase 4). For now, just log and continue.
        log::trace!("execute: PC={:#018X} instr={}", _current_pc, _instr);
    }
}
