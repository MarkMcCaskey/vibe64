use crate::bus::Bus;
use crate::cpu::Vr4300;

#[cfg(feature = "dynarec")]
mod dynarec;

/// Execution engine abstraction.
///
/// The emulator always supports the interpreter engine.
/// With the `dynarec` feature enabled, `Engine` can also use the dynamic
/// recompilation pipeline (with interpreter fallback for execution safety).
pub trait ExecutionEngine {
    /// Execute one unit of work.
    /// Interpreter: one instruction. JIT: one compiled block.
    fn execute(&mut self, cpu: &mut Vr4300, bus: &mut impl Bus) -> u64;

    /// Invalidate any compiled code covering [start, start+len) in
    /// physical RDRAM. Called when memory writes may have modified code.
    fn invalidate_range(&mut self, start: u32, len: u32);

    fn name(&self) -> &'static str;
}

/// The interpreter — our initial ExecutionEngine.
#[derive(Default)]
pub struct Interpreter;

impl ExecutionEngine for Interpreter {
    fn execute(&mut self, cpu: &mut Vr4300, bus: &mut impl Bus) -> u64 {
        cpu.step(bus)
    }

    fn invalidate_range(&mut self, _start: u32, _len: u32) {
        // Interpreter doesn't cache compiled code — nothing to invalidate.
    }

    fn name(&self) -> &'static str {
        "interpreter"
    }
}

/// Runtime-selected execution engine.
pub enum Engine {
    Interpreter(Interpreter),
    #[cfg(feature = "dynarec")]
    Dynarec(dynarec::DynarecEngine),
}

impl Engine {
    pub fn interpreter() -> Self {
        Self::Interpreter(Interpreter)
    }

    /// Build engine from environment.
    ///
    /// `N64_DYNAREC=1|on|true|cranelift` enables dynarec when compiled with
    /// the `dynarec` feature.
    pub fn from_env() -> Self {
        #[cfg(feature = "dynarec")]
        {
            let Some(raw) = std::env::var("N64_DYNAREC").ok() else {
                return Self::interpreter();
            };
            let normalized = raw.trim().to_ascii_lowercase();
            if matches!(normalized.as_str(), "" | "0" | "off" | "false" | "no") {
                return Self::interpreter();
            }
            if matches!(
                normalized.as_str(),
                "1" | "on" | "true" | "yes" | "cranelift"
            ) {
                return Self::Dynarec(dynarec::DynarecEngine::new_cranelift());
            }
            log::warn!(
                "Unknown N64_DYNAREC value {:?}; falling back to interpreter",
                raw
            );
            return Self::interpreter();
        }

        #[cfg(not(feature = "dynarec"))]
        {
            Self::interpreter()
        }
    }

    pub fn execute(&mut self, cpu: &mut Vr4300, bus: &mut impl Bus) -> u64 {
        match self {
            Engine::Interpreter(engine) => engine.execute(cpu, bus),
            #[cfg(feature = "dynarec")]
            Engine::Dynarec(engine) => engine.execute(cpu, bus),
        }
    }

    pub fn invalidate_range(&mut self, start: u32, len: u32) {
        match self {
            Engine::Interpreter(engine) => engine.invalidate_range(start, len),
            #[cfg(feature = "dynarec")]
            Engine::Dynarec(engine) => engine.invalidate_range(start, len),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Engine::Interpreter(engine) => engine.name(),
            #[cfg(feature = "dynarec")]
            Engine::Dynarec(engine) => engine.name(),
        }
    }

    pub fn dynarec_stats_line(&self) -> Option<String> {
        match self {
            Engine::Interpreter(_) => None,
            #[cfg(feature = "dynarec")]
            Engine::Dynarec(engine) => Some(engine.stats_line()),
        }
    }

    pub fn reset_stats(&mut self) {
        match self {
            Engine::Interpreter(_) => {}
            #[cfg(feature = "dynarec")]
            Engine::Dynarec(engine) => engine.reset_stats(),
        }
    }

    #[cfg(all(test, feature = "dynarec"))]
    pub(crate) fn dynarec_for_tests() -> Self {
        Self::Dynarec(dynarec::DynarecEngine::new_cranelift_for_tests())
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "dynarec")]
    use super::Engine;
    use super::{ExecutionEngine, Interpreter};
    use crate::bus::Bus;
    use crate::cpu::Vr4300;

    struct TestBus {
        mem: Vec<u8>,
        invalidations: Vec<(u32, u32)>,
    }

    impl TestBus {
        fn new(size: usize) -> Self {
            Self {
                mem: vec![0; size],
                invalidations: Vec::new(),
            }
        }

        fn load_program(&mut self, start_phys: u32, words: &[u32]) {
            for (i, word) in words.iter().enumerate() {
                let addr = start_phys + (i as u32) * 4;
                let idx = addr as usize;
                let bytes = word.to_be_bytes();
                self.mem[idx..idx + 4].copy_from_slice(&bytes);
            }
        }

        #[cfg(feature = "dynarec")]
        fn take_invalidations(&mut self) -> Vec<(u32, u32)> {
            std::mem::take(&mut self.invalidations)
        }
    }

    impl Bus for TestBus {
        fn read_u8(&self, addr: u32) -> u8 {
            self.mem[addr as usize]
        }

        fn read_u16(&self, addr: u32) -> u16 {
            let i = addr as usize;
            u16::from_be_bytes([self.mem[i], self.mem[i + 1]])
        }

        fn read_u32(&self, addr: u32) -> u32 {
            let i = addr as usize;
            u32::from_be_bytes([
                self.mem[i],
                self.mem[i + 1],
                self.mem[i + 2],
                self.mem[i + 3],
            ])
        }

        fn read_u64(&self, addr: u32) -> u64 {
            let hi = self.read_u32(addr) as u64;
            let lo = self.read_u32(addr + 4) as u64;
            (hi << 32) | lo
        }

        fn write_u8(&mut self, addr: u32, val: u8) {
            self.mem[addr as usize] = val;
            self.notify_dma_write(addr, 1);
        }

        fn write_u16(&mut self, addr: u32, val: u16) {
            let i = addr as usize;
            let bytes = val.to_be_bytes();
            self.mem[i] = bytes[0];
            self.mem[i + 1] = bytes[1];
            self.notify_dma_write(addr, 2);
        }

        fn write_u32(&mut self, addr: u32, val: u32) {
            let i = addr as usize;
            let bytes = val.to_be_bytes();
            self.mem[i..i + 4].copy_from_slice(&bytes);
            self.notify_dma_write(addr, 4);
        }

        fn write_u64(&mut self, addr: u32, val: u64) {
            self.write_u32(addr, (val >> 32) as u32);
            self.write_u32(addr + 4, val as u32);
        }

        fn notify_dma_write(&mut self, start: u32, len: u32) {
            self.invalidations.push((start, len));
        }

        fn pending_interrupts(&self) -> bool {
            false
        }
    }

    fn init_cpu() -> Vr4300 {
        let mut cpu = Vr4300::new();
        cpu.pc = 0xFFFF_FFFF_8000_0000;
        cpu.next_pc = cpu.pc + 4;
        cpu
    }

    #[cfg(feature = "dynarec")]
    fn run_until_pc(
        engine: &mut Engine,
        cpu: &mut Vr4300,
        bus: &mut TestBus,
        end_pc: u64,
        max_calls: u32,
    ) {
        let mut calls = 0u32;
        while cpu.pc != end_pc && calls < max_calls {
            let retired = engine.execute(cpu, bus);
            assert!(retired > 0, "engine retired zero instructions");
            for (start, len) in bus.take_invalidations() {
                engine.invalidate_range(start, len);
            }
            calls += 1;
        }
        assert_eq!(cpu.pc, end_pc, "engine did not reach target pc");
    }

    #[cfg(feature = "dynarec")]
    fn assert_cpu_equal(a: &Vr4300, b: &Vr4300) {
        assert_eq!(a.gpr, b.gpr);
        assert_eq!(a.pc, b.pc);
        assert_eq!(a.next_pc, b.next_pc);
        assert_eq!(a.hi, b.hi);
        assert_eq!(a.lo, b.lo);
        assert_eq!(a.cop0.regs, b.cop0.regs);
        assert_eq!(a.cop1.fpr, b.cop1.fpr);
        assert_eq!(a.cop1.fcr31, b.cop1.fcr31);
        assert_eq!(a.in_delay_slot, b.in_delay_slot);
        assert_eq!(a.ll_bit, b.ll_bit);
    }

    #[test]
    fn interpreter_executes_simple_program() {
        let mut bus = TestBus::new(0x2000);
        let program = [
            0x3C0C_8000, // lui t4, 0x8000
            0x2408_0005, // addiu t0, r0, 5
            0x2409_0007, // addiu t1, r0, 7
            0x0109_5021, // addu t2, t0, t1
            0xAD8A_0100, // sw t2, 0x100(t4)
            0x8D8B_0100, // lw t3, 0x100(t4)
            0x3562_1234, // ori v0, t3, 0x1234
        ];
        bus.load_program(0, &program);

        let mut cpu = init_cpu();
        let mut interpreter = Interpreter;
        for _ in 0..program.len() {
            let _ = interpreter.execute(&mut cpu, &mut bus);
        }

        assert_eq!(cpu.gpr[10], 12); // t2
        assert_eq!(cpu.gpr[11], 12); // t3
        assert_eq!(cpu.gpr[2], 0x123C); // v0
    }

    #[cfg(feature = "dynarec")]
    #[test]
    fn dynarec_matches_interpreter_on_arith_and_load_store() {
        let program = [
            0x3C0C_8000, // lui t4, 0x8000
            0x2408_0005, // addiu t0, r0, 5
            0x2409_0007, // addiu t1, r0, 7
            0x0109_5021, // addu t2, t0, t1
            0xAD8A_0100, // sw t2, 0x100(t4)
            0x8D8B_0100, // lw t3, 0x100(t4)
            0x3562_1234, // ori v0, t3, 0x1234
            0x4200_0018, // eret (sentinel: unsupported by dynarec)
        ];

        let mut bus_interp = TestBus::new(0x2000);
        bus_interp.load_program(0, &program);
        let mut bus_dynarec = TestBus::new(0x2000);
        bus_dynarec.load_program(0, &program);

        let mut cpu_interp = init_cpu();
        let mut cpu_dynarec = init_cpu();
        let mut interpreter_engine = Engine::Interpreter(Interpreter);
        let mut dynarec_engine = Engine::dynarec_for_tests();

        let end_pc = init_cpu().pc + 7 * 4;
        run_until_pc(
            &mut interpreter_engine,
            &mut cpu_interp,
            &mut bus_interp,
            end_pc,
            32,
        );
        run_until_pc(
            &mut dynarec_engine,
            &mut cpu_dynarec,
            &mut bus_dynarec,
            end_pc,
            32,
        );

        assert_cpu_equal(&cpu_interp, &cpu_dynarec);
    }

    #[cfg(feature = "dynarec")]
    #[test]
    fn dynarec_matches_interpreter_on_branch_delay_slot() {
        let program = [
            0x2408_0001, // addiu t0, r0, 1
            0x1108_0002, // beq t0, t0, +2
            0x2409_0007, // addiu t1, r0, 7 (delay slot)
            0x2409_0063, // addiu t1, r0, 99 (skipped)
            0x3522_0000, // ori v0, t1, 0
            0x4200_0018, // eret (sentinel: unsupported by dynarec)
        ];

        let mut bus_interp = TestBus::new(0x2000);
        bus_interp.load_program(0, &program);
        let mut bus_dynarec = TestBus::new(0x2000);
        bus_dynarec.load_program(0, &program);

        let mut cpu_interp = init_cpu();
        let mut cpu_dynarec = init_cpu();
        let mut interpreter_engine = Engine::Interpreter(Interpreter);
        let mut dynarec_engine = Engine::dynarec_for_tests();

        let end_pc = init_cpu().pc + 5 * 4;
        run_until_pc(
            &mut interpreter_engine,
            &mut cpu_interp,
            &mut bus_interp,
            end_pc,
            32,
        );
        run_until_pc(
            &mut dynarec_engine,
            &mut cpu_dynarec,
            &mut bus_dynarec,
            end_pc,
            32,
        );

        assert_cpu_equal(&cpu_interp, &cpu_dynarec);
        assert_eq!(cpu_dynarec.gpr[2], 7); // v0 should see delay-slot value path
    }

    #[cfg(feature = "dynarec")]
    #[test]
    fn dynarec_matches_interpreter_on_backward_branch_loop() {
        let program = [
            0x3C0C_8000, // lui t4, 0x8000
            0x2408_0000, // addiu t0, r0, 0
            0x2409_0008, // addiu t1, r0, 8
            0x240A_0000, // addiu t2, r0, 0
            0x2508_0001, // addiu t0, t0, 1
            0x0148_5021, // addu t2, t2, t0
            0x1509_FFFD, // bne t0, t1, -3
            0xAD88_0100, // sw t0, 0x100(t4) (delay slot)
            0xAD8A_0104, // sw t2, 0x104(t4)
            0x8D8B_0100, // lw t3, 0x100(t4)
            0x8D8D_0104, // lw t5, 0x104(t4)
            0x3562_0000, // ori v0, t3, 0
            0x35A3_0000, // ori v1, t5, 0
            0x4200_0018, // eret (sentinel: unsupported by dynarec)
        ];

        let mut bus_interp = TestBus::new(0x3000);
        bus_interp.load_program(0, &program);
        let mut bus_dynarec = TestBus::new(0x3000);
        bus_dynarec.load_program(0, &program);

        let mut cpu_interp = init_cpu();
        let mut cpu_dynarec = init_cpu();
        let mut interpreter_engine = Engine::Interpreter(Interpreter);
        let mut dynarec_engine = Engine::dynarec_for_tests();

        let end_pc = init_cpu().pc + 13 * 4;
        run_until_pc(
            &mut interpreter_engine,
            &mut cpu_interp,
            &mut bus_interp,
            end_pc,
            256,
        );
        run_until_pc(
            &mut dynarec_engine,
            &mut cpu_dynarec,
            &mut bus_dynarec,
            end_pc,
            256,
        );

        assert_cpu_equal(&cpu_interp, &cpu_dynarec);
        assert_eq!(cpu_dynarec.gpr[2], 8); // v0: final loop counter
        assert_eq!(cpu_dynarec.gpr[3], 36); // v1: sum 1..8
        assert_eq!(bus_dynarec.read_u32(0x100), 8);
        assert_eq!(bus_dynarec.read_u32(0x104), 36);
    }
}
