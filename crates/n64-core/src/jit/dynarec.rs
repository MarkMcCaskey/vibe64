use crate::bus::Bus;
use crate::cpu::cop0::Cop0;
use crate::cpu::Vr4300;
use crate::jit::{ExecutionEngine, Interpreter};
use std::collections::HashMap;

use n64_dynarec::{
    CompiledBlock, CraneliftCompiler, EnsureResult, InstructionSource, Recompiler,
    RecompilerConfig, RecompilerStats,
};

#[derive(Debug, Clone, Copy, Default)]
pub struct DynarecRuntimeStats {
    pub native_blocks_executed: u64,
    pub native_instructions_executed: u64,
    pub fallback_instructions_executed: u64,
    pub fallback_early_guard: u64,
    pub fallback_guard_after_lookup: u64,
    pub fallback_no_block: u64,
    pub fallback_failed_cache: u64,
    pub fallback_cold: u64,
    pub ensure_compiled_calls: u64,
    pub ensure_compiled_compiled: u64,
    pub ensure_compiled_compile_failed: u64,
    pub ensure_compiled_cache_hit: u64,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DynarecStats {
    pub runtime: DynarecRuntimeStats,
    pub recompiler: RecompilerStats,
    pub block_cache_len: usize,
    pub failed_cache_len: usize,
    pub hot_entries: usize,
    pub hot_threshold: u16,
    pub min_native_instructions: u32,
}

#[derive(Clone, Copy)]
enum FallbackReason {
    EarlyGuard,
    GuardAfterLookup,
    NoBlock,
    FailedCache,
    Cold,
}

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
    runtime: DynarecRuntimeStats,
    hot_counts: HashMap<u32, u16>,
    hot_threshold: u16,
    min_native_instructions: u32,
}

impl DynarecEngine {
    fn parse_env_u16(name: &str, default: u16) -> u16 {
        std::env::var(name)
            .ok()
            .and_then(|s| s.parse::<u16>().ok())
            .filter(|v| *v > 0)
            .unwrap_or(default)
    }

    fn parse_env_u32(name: &str, default: u32) -> u32 {
        std::env::var(name)
            .ok()
            .and_then(|s| s.parse::<u32>().ok())
            .filter(|v| *v > 0)
            .unwrap_or(default)
    }

    pub fn new_cranelift() -> Self {
        let hot_threshold = Self::parse_env_u16("N64_DYNAREC_HOT_THRESHOLD", 4096);
        let min_native_instructions = Self::parse_env_u32("N64_DYNAREC_MIN_BLOCK_INSNS", 1);
        let compiler = Box::<CraneliftCompiler>::default();
        let recompiler = Recompiler::new(compiler, RecompilerConfig::default());
        Self {
            fallback: Interpreter,
            recompiler,
            runtime: DynarecRuntimeStats::default(),
            hot_counts: HashMap::new(),
            hot_threshold,
            min_native_instructions,
        }
    }

    pub fn stats(&self) -> DynarecStats {
        DynarecStats {
            runtime: self.runtime,
            recompiler: self.recompiler.stats(),
            block_cache_len: self.recompiler.cache_len(),
            failed_cache_len: self.recompiler.failed_cache_len(),
            hot_entries: self.hot_counts.len(),
            hot_threshold: self.hot_threshold,
            min_native_instructions: self.min_native_instructions,
        }
    }

    pub fn stats_line(&self) -> String {
        let stats = self.stats();
        format!(
            "native_blocks={} native_instr={} fallback_instr={} fallback_early_guard={} fallback_guard_after_lookup={} fallback_no_block={} fallback_failed_cache={} fallback_cold={} ensure_calls={} ensure_compiled={} ensure_compile_failed={} ensure_cache_hit={} recompiler_cache_hits={} recompiler_failed_cache_hits={} recompiler_blocks_compiled={} recompiler_compile_failures={} recompiler_invalidated_blocks={} block_cache_len={} failed_cache_len={} hot_entries={} hot_threshold={} min_block_insns={}",
            stats.runtime.native_blocks_executed,
            stats.runtime.native_instructions_executed,
            stats.runtime.fallback_instructions_executed,
            stats.runtime.fallback_early_guard,
            stats.runtime.fallback_guard_after_lookup,
            stats.runtime.fallback_no_block,
            stats.runtime.fallback_failed_cache,
            stats.runtime.fallback_cold,
            stats.runtime.ensure_compiled_calls,
            stats.runtime.ensure_compiled_compiled,
            stats.runtime.ensure_compiled_compile_failed,
            stats.runtime.ensure_compiled_cache_hit,
            stats.recompiler.cache_hits,
            stats.recompiler.failed_cache_hits,
            stats.recompiler.blocks_compiled,
            stats.recompiler.compile_failures,
            stats.recompiler.invalidated_blocks,
            stats.block_cache_len,
            stats.failed_cache_len,
            stats.hot_entries,
            stats.hot_threshold,
            stats.min_native_instructions
        )
    }

    pub fn reset_stats(&mut self) {
        self.runtime = DynarecRuntimeStats::default();
        self.recompiler.reset_stats();
    }

    fn run_fallback(
        &mut self,
        cpu: &mut Vr4300,
        bus: &mut impl Bus,
        reason: FallbackReason,
    ) -> u64 {
        let retired = self.fallback.execute(cpu, bus);
        self.runtime.fallback_instructions_executed = self
            .runtime
            .fallback_instructions_executed
            .wrapping_add(retired);
        match reason {
            FallbackReason::EarlyGuard => {
                self.runtime.fallback_early_guard += 1;
            }
            FallbackReason::GuardAfterLookup => {
                self.runtime.fallback_guard_after_lookup += 1;
            }
            FallbackReason::NoBlock => {
                self.runtime.fallback_no_block += 1;
            }
            FallbackReason::FailedCache => {
                self.runtime.fallback_failed_cache += 1;
            }
            FallbackReason::Cold => {
                self.runtime.fallback_cold += 1;
            }
        }
        retired
    }

    fn should_attempt_compile(&mut self, start_phys: u32) -> bool {
        let entry = self.hot_counts.entry(start_phys).or_insert(0);
        *entry = entry.saturating_add(1);
        *entry >= self.hot_threshold
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
        if block.instruction_count < self.min_native_instructions {
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

        let next_pc = block.execute(&mut cpu.gpr, start_pc);

        for i in 0..count {
            let pc = start_pc.wrapping_add((i as u64) * 4);
            cpu.pc_history[cpu.pc_history_idx] = pc as u32;
            cpu.pc_history_idx = (cpu.pc_history_idx + 1) & 63;
        }
        cpu.step_count = cpu.step_count.wrapping_add(count as u64);

        cpu.pc = next_pc;
        cpu.next_pc = next_pc.wrapping_add(4);
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

        self.runtime.native_blocks_executed += 1;
        self.runtime.native_instructions_executed = self
            .runtime
            .native_instructions_executed
            .wrapping_add(count as u64);

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
            return self.run_fallback(cpu, bus, FallbackReason::EarlyGuard);
        }

        let start_phys = pc32 & 0x1FFF_FFFF;
        if let Some(block) = self.recompiler.lookup(start_phys).copied() {
            if self.can_run_native_block(cpu, bus, &block, start_phys) {
                return self.run_native_block(cpu, bus, &block);
            }
            return self.run_fallback(cpu, bus, FallbackReason::GuardAfterLookup);
        }

        if self.recompiler.is_failed_cached(start_phys) {
            return self.run_fallback(cpu, bus, FallbackReason::FailedCache);
        }

        if !self.should_attempt_compile(start_phys) {
            return self.run_fallback(cpu, bus, FallbackReason::Cold);
        }

        self.runtime.ensure_compiled_calls += 1;
        let ensure_result = {
            let mut source = BusSource { bus };
            self.recompiler.ensure_compiled(start_phys, &mut source)
        };

        match ensure_result {
            EnsureResult::CacheHit => {
                self.runtime.ensure_compiled_cache_hit += 1;
            }
            EnsureResult::Compiled => {
                self.runtime.ensure_compiled_compiled += 1;
                self.hot_counts.remove(&start_phys);
            }
            EnsureResult::CompileFailed => {
                self.runtime.ensure_compiled_compile_failed += 1;
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
            return self.run_fallback(cpu, bus, FallbackReason::GuardAfterLookup);
        }

        self.run_fallback(cpu, bus, FallbackReason::NoBlock)
    }

    fn invalidate_range(&mut self, start: u32, len: u32) {
        self.recompiler.invalidate_range(start, len);
    }

    fn name(&self) -> &'static str {
        self.recompiler.backend_name()
    }
}
