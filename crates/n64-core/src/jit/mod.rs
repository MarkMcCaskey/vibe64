/// JIT infrastructure — execution engine abstraction.
///
/// The ExecutionEngine trait allows swapping between the interpreter
/// and a future JIT recompiler without changing the rest of the emulator.
///
/// For the interpreter phase, this is a trivial pass-through.
/// When we add JIT, it implements the same trait with:
///   - A block cache keyed by physical RDRAM address
///   - A page map for fast range invalidation
///   - Compiled native code blocks

use crate::bus::Bus;
use crate::cpu::Vr4300;

pub trait ExecutionEngine {
    /// Execute one unit of work.
    /// Interpreter: one instruction. JIT: one compiled block.
    fn execute(&mut self, cpu: &mut Vr4300, bus: &mut impl Bus) -> u64;

    /// Invalidate any compiled code covering [start, start+len) in
    /// physical RDRAM. Called when DMA writes to RDRAM.
    fn invalidate_range(&mut self, start: u32, len: u32);
}

/// The interpreter — our initial ExecutionEngine.
pub struct Interpreter;

impl ExecutionEngine for Interpreter {
    fn execute(&mut self, cpu: &mut Vr4300, bus: &mut impl Bus) -> u64 {
        cpu.step(bus)
    }

    fn invalidate_range(&mut self, _start: u32, _len: u32) {
        // Interpreter doesn't cache compiled code — nothing to invalidate.
    }
}
