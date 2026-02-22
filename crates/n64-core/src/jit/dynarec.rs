use crate::bus::Bus;
use crate::cpu::cop0::Cop0;
use crate::cpu::Vr4300;
use crate::jit::{ExecutionEngine, Interpreter};

use n64_dynarec::{
    CompiledBlock, CraneliftCompiler, EnsureResult, InstructionSource, Recompiler,
    RecompilerConfig,
};

struct BusSource<'a, B: Bus> {
    bus: &'a mut B,
}

impl<B: Bus> InstructionSource for BusSource<'_, B> {
    fn read_u32(&mut self, phys_addr: u32) -> Result<u32, n64_dynarec::CompileError> {
        Ok(self.bus.read_u32(phys_addr))
    }
}

/// Dynarec engine with interpreter fallback.
///
/// This first stage compiles block metadata/caching through the dynarec
/// pipeline and executes instructions through the interpreter path.
pub struct DynarecEngine {
    fallback: Interpreter,
    recompiler: Recompiler,
}

impl DynarecEngine {
    pub fn new_cranelift() -> Self {
        let compiler = Box::<CraneliftCompiler>::default();
        let recompiler = Recompiler::new(compiler, RecompilerConfig::default());
        Self {
            fallback: Interpreter,
            recompiler,
        }
    }

    fn timer_interrupt_would_fire(cpu: &Vr4300, instructions: u32) -> bool {
        if instructions == 0 {
            return false;
        }
        let count = cpu.cop0.regs[Cop0::COUNT] as u32;
        let compare = cpu.cop0.regs[Cop0::COMPARE] as u32;
        let delta = compare.wrapping_sub(count);
        delta != 0 && delta <= instructions
    }

    fn can_run_native_block(
        &self,
        cpu: &Vr4300,
        bus: &impl Bus,
        block: &CompiledBlock,
        start_phys: u32,
    ) -> bool {
        if block.instruction_count == 0 {
            return false;
        }

        // Delay-slot and non-linear PC paths need exact per-instruction handling.
        if cpu.in_delay_slot || cpu.next_pc != cpu.pc.wrapping_add(4) {
            return false;
        }

        // Restrict to kseg0/kseg1 direct mappings for now.
        let pc32 = cpu.pc as u32;
        if !(0x8000_0000..=0xBFFF_FFFF).contains(&pc32) {
            return false;
        }

        if start_phys != (pc32 & 0x1FFF_FFFF) {
            return false;
        }

        // If an interrupt could be taken, we must stay instruction-granular.
        if bus.pending_interrupts() || cpu.cop0.interrupt_pending() {
            return false;
        }

        if Self::timer_interrupt_would_fire(cpu, block.instruction_count) {
            return false;
        }

        true
    }

    fn run_native_block(&mut self, cpu: &mut Vr4300, bus: &impl Bus, block: &CompiledBlock) -> u64 {
        let count = block.instruction_count;
        let start_pc = cpu.pc;

        block.execute(&mut cpu.gpr);

        for i in 0..count {
            let pc = start_pc.wrapping_add((i as u64) * 4);
            cpu.pc_history[cpu.pc_history_idx] = pc as u32;
            cpu.pc_history_idx = (cpu.pc_history_idx + 1) & 63;
        }
        cpu.step_count = cpu.step_count.wrapping_add(count as u64);

        cpu.pc = cpu.pc.wrapping_add((count as u64) * 4);
        cpu.next_pc = cpu.next_pc.wrapping_add((count as u64) * 4);
        cpu.gpr[0] = 0;

        for _ in 0..count {
            cpu.cop0.increment_count();
            cpu.cop0.decrement_random();
            cpu.cop0.check_timer_interrupt();
        }

        if bus.pending_interrupts() {
            cpu.cop0.set_ip2();
        } else {
            cpu.cop0.clear_ip2();
        }

        count as u64
    }
}

impl ExecutionEngine for DynarecEngine {
    fn execute(&mut self, cpu: &mut Vr4300, bus: &mut impl Bus) -> u64 {
        let pc32 = cpu.pc as u32;
        if cpu.in_delay_slot
            || cpu.next_pc != cpu.pc.wrapping_add(4)
            || !(0x8000_0000..=0xBFFF_FFFF).contains(&pc32)
        {
            return self.fallback.execute(cpu, bus);
        }

        let start_phys = pc32 & 0x1FFF_FFFF;
        {
            let mut source = BusSource { bus };
            if let EnsureResult::CompileFailed =
                self.recompiler.ensure_compiled(start_phys, &mut source)
            {
                if let Some(err) = self.recompiler.last_error() {
                    log::debug!(
                        "Dynarec compile failed at {:#010X} (backend={}): {:?}",
                        start_phys,
                        self.recompiler.backend_name(),
                        err
                    );
                }
            }
        }

        if let Some(block) = self.recompiler.lookup(start_phys).copied() {
            if self.can_run_native_block(cpu, bus, &block, start_phys) {
                return self.run_native_block(cpu, bus, &block);
            }
        }

        self.fallback.execute(cpu, bus)
    }

    fn invalidate_range(&mut self, start: u32, len: u32) {
        self.recompiler.invalidate_range(start, len);
    }

    fn name(&self) -> &'static str {
        self.recompiler.backend_name()
    }
}
