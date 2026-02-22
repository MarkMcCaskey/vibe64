use crate::bus::Bus;
use crate::cpu::cop0::Cop0;
use crate::cpu::instruction::Instruction;
use crate::cpu::Vr4300;
use crate::jit::{ExecutionEngine, Interpreter};
use std::collections::HashMap;
use std::time::Instant;

use n64_dynarec::{
    CompiledBlock, CraneliftCompiler, EnsureResult, InstructionSource, Recompiler,
    RecompilerConfig, RecompilerStats, RuntimeCallbacks,
};

#[derive(Debug, Clone, Copy, Default)]
pub struct DynarecRuntimeStats {
    pub native_blocks_executed: u64,
    pub native_instructions_executed: u64,
    pub native_interp_delegated_instructions: u64,
    pub native_gas_exits: u64,
    pub fallback_instructions_executed: u64,
    pub fallback_early_guard: u64,
    pub fallback_guard_after_lookup: u64,
    pub fallback_no_block: u64,
    pub fallback_failed_cache: u64,
    pub fallback_cold: u64,
    pub fallback_compile_budget: u64,
    pub ensure_compiled_calls: u64,
    pub ensure_compiled_compiled: u64,
    pub ensure_compiled_compile_failed: u64,
    pub ensure_compiled_cache_hit: u64,
    pub ensure_compiled_time_us: u64,
    pub ensure_compiled_time_max_us: u64,
    pub invalidate_calls: u64,
    pub invalidate_bytes: u64,
    pub invalidate_time_us: u64,
    pub invalidate_time_max_us: u64,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DynarecStats {
    pub runtime: DynarecRuntimeStats,
    pub recompiler: RecompilerStats,
    pub block_cache_len: usize,
    pub failed_cache_len: usize,
    pub hot_entries: usize,
    pub hot_threshold: u16,
    pub max_block_instructions: u32,
    pub min_native_instructions: u32,
    pub native_gas_limit: u32,
    /// 0 means unlimited chaining (bounded by `native_gas_limit`).
    pub chain_limit: u32,
    /// Compile budget refill rate in microseconds per millisecond of host time.
    pub compile_budget_us_per_ms: u32,
    /// Maximum host-time burst window for compile budget accumulation.
    pub compile_budget_burst_ms: u32,
    /// Current compile budget credit in microseconds.
    pub compile_budget_credit_us: i64,
    /// Max compile budget credit in microseconds.
    pub compile_budget_cap_us: i64,
}

#[derive(Clone, Copy)]
enum FallbackReason {
    EarlyGuard,
    GuardAfterLookup,
    NoBlock,
    FailedCache,
    Cold,
    CompileBudget,
}

struct BusSource<'a, B: Bus> {
    bus: &'a mut B,
}

impl<B: Bus> InstructionSource for BusSource<'_, B> {
    fn read_u32(&mut self, phys_addr: u32) -> Result<u32, n64_dynarec::CompileError> {
        Ok(self.bus.read_u32(phys_addr))
    }
}

struct CallbackContext<B: Bus> {
    cpu: *mut Vr4300,
    bus: *mut B,
}

unsafe extern "C" fn cb_load_u8<B: Bus>(user: *mut u8, vaddr: u64) -> u64 {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    let phys = cpu.translate_address(vaddr);
    u64::from(bus.read_u8(phys))
}

unsafe extern "C" fn cb_load_u16<B: Bus>(user: *mut u8, vaddr: u64) -> u64 {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    let phys = cpu.translate_address(vaddr);
    u64::from(bus.read_u16(phys))
}

unsafe extern "C" fn cb_load_u32<B: Bus>(user: *mut u8, vaddr: u64) -> u64 {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    let phys = cpu.translate_address(vaddr);
    u64::from(bus.read_u32(phys))
}

unsafe extern "C" fn cb_load_u64<B: Bus>(user: *mut u8, vaddr: u64) -> u64 {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    let phys = cpu.translate_address(vaddr);
    bus.read_u64(phys)
}

unsafe extern "C" fn cb_store_u8<B: Bus>(user: *mut u8, vaddr: u64, value: u64) {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    let phys = cpu.translate_address(vaddr);
    bus.write_u8(phys, value as u8);
}

unsafe extern "C" fn cb_store_u16<B: Bus>(user: *mut u8, vaddr: u64, value: u64) {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    let phys = cpu.translate_address(vaddr);
    bus.write_u16(phys, value as u16);
}

unsafe extern "C" fn cb_store_u32<B: Bus>(user: *mut u8, vaddr: u64, value: u64) {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    let phys = cpu.translate_address(vaddr);
    bus.write_u32(phys, value as u32);
}

unsafe extern "C" fn cb_store_u64<B: Bus>(user: *mut u8, vaddr: u64, value: u64) {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    let phys = cpu.translate_address(vaddr);
    bus.write_u64(phys, value);
}

unsafe extern "C" fn cb_cop0_read<B: Bus>(user: *mut u8, reg: u64) -> u64 {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    cpu.cop0.read_reg((reg as usize) & 0x1F)
}

unsafe extern "C" fn cb_cop0_write<B: Bus>(user: *mut u8, reg: u64, value: u64) {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    cpu.cop0.write_reg((reg as usize) & 0x1F, value);
}

unsafe extern "C" fn cb_cop1_condition<B: Bus>(user: *mut u8) -> u64 {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    u64::from(cpu.cop1.condition())
}

unsafe extern "C" fn cb_interp_exec<B: Bus>(user: *mut u8, raw: u64, current_pc: u64) {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let bus = unsafe { &mut *ctx.bus };
    cpu.execute(Instruction::decode(raw as u32), bus, current_pc);
}

unsafe extern "C" fn cb_hi_read<B: Bus>(user: *mut u8) -> u64 {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    cpu.hi
}

unsafe extern "C" fn cb_hi_write<B: Bus>(user: *mut u8, value: u64) {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    cpu.hi = value;
}

unsafe extern "C" fn cb_lo_read<B: Bus>(user: *mut u8) -> u64 {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    cpu.lo
}

unsafe extern "C" fn cb_lo_write<B: Bus>(user: *mut u8, value: u64) {
    // SAFETY: user pointer is created from `CallbackContext<B>` in `run_native_block`.
    let ctx = unsafe { &mut *(user as *mut CallbackContext<B>) };
    // SAFETY: pointers come from live mutable references held by `run_native_block`.
    let cpu = unsafe { &mut *ctx.cpu };
    cpu.lo = value;
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
    max_block_instructions: u32,
    min_native_instructions: u32,
    native_gas_limit: u32,
    /// 0 means unlimited chaining (bounded by `native_gas_limit`).
    chain_limit: u32,
    compile_budget_us_per_ms: u32,
    compile_budget_burst_ms: u32,
    compile_budget_credit_us: i64,
    compile_budget_cap_us: i64,
    compile_budget_last_refill: Instant,
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

    fn parse_env_u32_allow_zero(name: &str, default: u32) -> u32 {
        std::env::var(name)
            .ok()
            .and_then(|s| s.parse::<u32>().ok())
            .unwrap_or(default)
    }

    pub fn new_cranelift() -> Self {
        let hot_threshold = Self::parse_env_u16("N64_DYNAREC_HOT_THRESHOLD", 8192);
        let max_block_instructions = Self::parse_env_u32("N64_DYNAREC_MAX_BLOCK_INSNS", 256);
        let min_native_instructions = Self::parse_env_u32("N64_DYNAREC_MIN_BLOCK_INSNS", 2);
        let native_gas_limit = Self::parse_env_u32("N64_DYNAREC_NATIVE_GAS", 512);
        let chain_limit = Self::parse_env_u32_allow_zero("N64_DYNAREC_CHAIN_LIMIT", 2);
        let compile_budget_us_per_ms =
            Self::parse_env_u32_allow_zero("N64_DYNAREC_COMPILE_BUDGET_US_PER_MS", 0);
        let compile_budget_burst_ms =
            Self::parse_env_u32("N64_DYNAREC_COMPILE_BUDGET_BURST_MS", 30);
        let compile_budget_cap_us =
            i64::from(compile_budget_us_per_ms.saturating_mul(compile_budget_burst_ms.max(1)));
        let compiler = Box::<CraneliftCompiler>::default();
        let recompiler = Recompiler::new(
            compiler,
            RecompilerConfig {
                max_block_instructions,
            },
        );
        Self {
            fallback: Interpreter,
            recompiler,
            runtime: DynarecRuntimeStats::default(),
            hot_counts: HashMap::new(),
            hot_threshold,
            max_block_instructions,
            min_native_instructions,
            native_gas_limit,
            chain_limit,
            compile_budget_us_per_ms,
            compile_budget_burst_ms,
            compile_budget_credit_us: compile_budget_cap_us,
            compile_budget_cap_us,
            compile_budget_last_refill: Instant::now(),
        }
    }

    #[cfg(test)]
    pub fn new_cranelift_for_tests() -> Self {
        let max_block_instructions = 256;
        let compiler = Box::<CraneliftCompiler>::default();
        let recompiler = Recompiler::new(
            compiler,
            RecompilerConfig {
                max_block_instructions,
            },
        );
        Self {
            fallback: Interpreter,
            recompiler,
            runtime: DynarecRuntimeStats::default(),
            hot_counts: HashMap::new(),
            hot_threshold: 1,
            max_block_instructions,
            min_native_instructions: 1,
            native_gas_limit: 1024,
            chain_limit: 2,
            compile_budget_us_per_ms: 0,
            compile_budget_burst_ms: 1,
            compile_budget_credit_us: 0,
            compile_budget_cap_us: 0,
            compile_budget_last_refill: Instant::now(),
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
            max_block_instructions: self.max_block_instructions,
            min_native_instructions: self.min_native_instructions,
            native_gas_limit: self.native_gas_limit,
            chain_limit: self.chain_limit,
            compile_budget_us_per_ms: self.compile_budget_us_per_ms,
            compile_budget_burst_ms: self.compile_budget_burst_ms,
            compile_budget_credit_us: self.compile_budget_credit_us,
            compile_budget_cap_us: self.compile_budget_cap_us,
        }
    }

    pub fn stats_line(&self) -> String {
        let stats = self.stats();
        format!(
            "native_blocks={} native_instr={} native_interp_instr={} native_gas_exits={} fallback_instr={} fallback_early_guard={} fallback_guard_after_lookup={} fallback_no_block={} fallback_failed_cache={} fallback_cold={} fallback_compile_budget={} ensure_calls={} ensure_compiled={} ensure_compile_failed={} ensure_cache_hit={} ensure_time_us={} ensure_time_max_us={} recompiler_cache_hits={} recompiler_failed_cache_hits={} recompiler_blocks_compiled={} recompiler_compile_failures={} recompiler_invalidated_blocks={} invalidate_calls={} invalidate_bytes={} invalidate_time_us={} invalidate_time_max_us={} block_cache_len={} failed_cache_len={} hot_entries={} hot_threshold={} max_block_insns={} min_block_insns={} native_gas={} chain_limit={} compile_budget_us_per_ms={} compile_budget_burst_ms={} compile_budget_cap_us={} compile_budget_credit_us={}",
            stats.runtime.native_blocks_executed,
            stats.runtime.native_instructions_executed,
            stats.runtime.native_interp_delegated_instructions,
            stats.runtime.native_gas_exits,
            stats.runtime.fallback_instructions_executed,
            stats.runtime.fallback_early_guard,
            stats.runtime.fallback_guard_after_lookup,
            stats.runtime.fallback_no_block,
            stats.runtime.fallback_failed_cache,
            stats.runtime.fallback_cold,
            stats.runtime.fallback_compile_budget,
            stats.runtime.ensure_compiled_calls,
            stats.runtime.ensure_compiled_compiled,
            stats.runtime.ensure_compiled_compile_failed,
            stats.runtime.ensure_compiled_cache_hit,
            stats.runtime.ensure_compiled_time_us,
            stats.runtime.ensure_compiled_time_max_us,
            stats.recompiler.cache_hits,
            stats.recompiler.failed_cache_hits,
            stats.recompiler.blocks_compiled,
            stats.recompiler.compile_failures,
            stats.recompiler.invalidated_blocks,
            stats.runtime.invalidate_calls,
            stats.runtime.invalidate_bytes,
            stats.runtime.invalidate_time_us,
            stats.runtime.invalidate_time_max_us,
            stats.block_cache_len,
            stats.failed_cache_len,
            stats.hot_entries,
            stats.hot_threshold,
            stats.max_block_instructions,
            stats.min_native_instructions,
            stats.native_gas_limit,
            stats.chain_limit,
            stats.compile_budget_us_per_ms,
            stats.compile_budget_burst_ms,
            stats.compile_budget_cap_us,
            stats.compile_budget_credit_us
        )
    }

    pub fn reset_stats(&mut self) {
        self.runtime = DynarecRuntimeStats::default();
        self.recompiler.reset_stats();
        self.compile_budget_credit_us = self.compile_budget_cap_us;
        self.compile_budget_last_refill = Instant::now();
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
            FallbackReason::CompileBudget => {
                self.runtime.fallback_compile_budget += 1;
            }
        }
        retired
    }

    fn should_attempt_compile(&mut self, start_phys: u32) -> bool {
        let entry = self.hot_counts.entry(start_phys).or_insert(0);
        *entry = entry.saturating_add(1);
        *entry >= self.hot_threshold
    }

    fn refill_compile_budget(&mut self) {
        if self.compile_budget_us_per_ms == 0 {
            return;
        }
        let now = Instant::now();
        let elapsed_ms = now
            .saturating_duration_since(self.compile_budget_last_refill)
            .as_millis();
        if elapsed_ms == 0 {
            return;
        }
        self.compile_budget_last_refill = now;
        let added = elapsed_ms
            .saturating_mul(u128::from(self.compile_budget_us_per_ms))
            .min(i64::MAX as u128) as i64;
        self.compile_budget_credit_us =
            (self.compile_budget_credit_us.saturating_add(added)).min(self.compile_budget_cap_us);
    }

    fn compile_budget_allows_attempt(&self) -> bool {
        self.compile_budget_us_per_ms == 0 || self.compile_budget_credit_us > 0
    }

    fn charge_compile_budget(&mut self, spent_us: u64) {
        if self.compile_budget_us_per_ms == 0 {
            return;
        }
        let spent = spent_us.min(i64::MAX as u64) as i64;
        self.compile_budget_credit_us = self.compile_budget_credit_us.saturating_sub(spent);
        if self.compile_budget_credit_us < -self.compile_budget_cap_us {
            self.compile_budget_credit_us = -self.compile_budget_cap_us;
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
        if block.instruction_count < self.min_native_instructions {
            return false;
        }

        // Blocks dominated by interpreter-delegated ops are usually slower than
        // the plain interpreter path.
        if block.interp_op_count != 0 && block.interp_op_count >= block.instruction_count {
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

        if Self::timer_interrupt_would_fire(cpu, block.max_retired_instructions) {
            return false;
        }

        true
    }

    fn run_native_block<B: Bus>(
        &mut self,
        cpu: &mut Vr4300,
        bus: &mut B,
        block: &CompiledBlock,
    ) -> u64 {
        let start_pc = cpu.pc;
        let fastmem = bus.dynarec_fastmem().unwrap_or_default();
        let mut callback_ctx = CallbackContext {
            cpu: (cpu as *mut Vr4300),
            bus: (bus as *mut B),
        };
        let mut callbacks = RuntimeCallbacks {
            user: (&mut callback_ctx as *mut CallbackContext<B>).cast::<u8>(),
            load_u8: cb_load_u8::<B>,
            load_u16: cb_load_u16::<B>,
            load_u32: cb_load_u32::<B>,
            load_u64: cb_load_u64::<B>,
            store_u8: cb_store_u8::<B>,
            store_u16: cb_store_u16::<B>,
            store_u32: cb_store_u32::<B>,
            store_u64: cb_store_u64::<B>,
            cop0_read: cb_cop0_read::<B>,
            cop0_write: cb_cop0_write::<B>,
            cop1_condition: cb_cop1_condition::<B>,
            interp_exec: cb_interp_exec::<B>,
            hi_read: cb_hi_read::<B>,
            hi_write: cb_hi_write::<B>,
            lo_read: cb_lo_read::<B>,
            lo_write: cb_lo_write::<B>,
            fastmem_base: fastmem.rdram_base,
            fastmem_phys_limit: fastmem.rdram_phys_limit,
            fastmem_phys_mask: fastmem.rdram_phys_mask,
        };
        let execution = block.execute(&mut cpu.gpr, &mut cpu.cop1.fpr, start_pc, &mut callbacks);
        let count = execution.retired_instructions;
        let next_pc = execution.next_pc;

        // PC history ring stores only the most recent 64 entries; when native
        // chunks are larger, older entries would be overwritten anyway.
        let history_count = count.min(64);
        let history_start =
            start_pc.wrapping_add(u64::from(count.saturating_sub(history_count)) * 4);
        for i in 0..u64::from(history_count) {
            let pc = history_start.wrapping_add(i * 4);
            cpu.pc_history[cpu.pc_history_idx] = pc as u32;
            cpu.pc_history_idx = (cpu.pc_history_idx + 1) & 63;
        }
        cpu.step_count = cpu.step_count.wrapping_add(count as u64);

        cpu.pc = next_pc;
        cpu.next_pc = next_pc.wrapping_add(4);
        cpu.gpr[0] = 0;

        cpu.cop0.advance_by_instructions(count);

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
        self.runtime.native_interp_delegated_instructions = self
            .runtime
            .native_interp_delegated_instructions
            .wrapping_add(block.interp_op_count as u64);

        u64::from(count)
    }

    fn run_native_chain<B: Bus>(
        &mut self,
        cpu: &mut Vr4300,
        bus: &mut B,
        first_block: CompiledBlock,
    ) -> u64 {
        let mut total_retired = 0u64;
        let gas_limit = u64::from(self.native_gas_limit.max(1));
        let mut blocks_left = if self.chain_limit == 0 {
            None
        } else {
            Some(self.chain_limit)
        };
        let mut block = first_block;

        loop {
            let retired = self.run_native_block(cpu, bus, &block);
            if retired == 0 {
                break;
            }
            total_retired = total_retired.wrapping_add(retired);
            if total_retired >= gas_limit {
                self.runtime.native_gas_exits = self.runtime.native_gas_exits.wrapping_add(1);
                break;
            }

            if let Some(left) = blocks_left.as_mut() {
                *left -= 1;
                if *left == 0 {
                    break;
                }
            }

            if cpu.in_delay_slot
                || cpu.next_pc != cpu.pc.wrapping_add(4)
                || !(0x8000_0000..=0xBFFF_FFFF).contains(&(cpu.pc as u32))
            {
                break;
            }

            let pc32 = cpu.pc as u32;
            let next_phys = pc32 & 0x1FFF_FFFF;
            let Some(next_block) = self.recompiler.lookup(next_phys).copied() else {
                break;
            };
            if !self.can_run_native_block(cpu, bus, &next_block, next_phys) {
                break;
            }
            block = next_block;
        }

        total_retired
    }
}

impl ExecutionEngine for DynarecEngine {
    fn execute(&mut self, cpu: &mut Vr4300, bus: &mut impl Bus) -> u64 {
        self.refill_compile_budget();

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
                return self.run_native_chain(cpu, bus, block);
            }
            return self.run_fallback(cpu, bus, FallbackReason::GuardAfterLookup);
        }

        if self.recompiler.is_failed_cached(start_phys) {
            return self.run_fallback(cpu, bus, FallbackReason::FailedCache);
        }

        if !self.should_attempt_compile(start_phys) {
            return self.run_fallback(cpu, bus, FallbackReason::Cold);
        }
        if !self.compile_budget_allows_attempt() {
            return self.run_fallback(cpu, bus, FallbackReason::CompileBudget);
        }

        self.runtime.ensure_compiled_calls += 1;
        let compile_start = Instant::now();
        let ensure_result = {
            let mut source = BusSource { bus };
            self.recompiler.ensure_compiled(start_phys, &mut source)
        };
        let compile_elapsed_us = compile_start
            .elapsed()
            .as_micros()
            .min(u128::from(u64::MAX)) as u64;
        self.runtime.ensure_compiled_time_us = self
            .runtime
            .ensure_compiled_time_us
            .wrapping_add(compile_elapsed_us);
        self.runtime.ensure_compiled_time_max_us = self
            .runtime
            .ensure_compiled_time_max_us
            .max(compile_elapsed_us);
        self.charge_compile_budget(compile_elapsed_us);

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
                return self.run_native_chain(cpu, bus, block);
            }
            return self.run_fallback(cpu, bus, FallbackReason::GuardAfterLookup);
        }

        self.run_fallback(cpu, bus, FallbackReason::NoBlock)
    }

    fn invalidate_range(&mut self, start: u32, len: u32) {
        if len == 0 {
            return;
        }
        let invalidate_start = Instant::now();
        self.recompiler.invalidate_range(start, len);
        let elapsed_us = invalidate_start
            .elapsed()
            .as_micros()
            .min(u128::from(u64::MAX)) as u64;
        self.runtime.invalidate_calls = self.runtime.invalidate_calls.wrapping_add(1);
        self.runtime.invalidate_bytes = self.runtime.invalidate_bytes.wrapping_add(u64::from(len));
        self.runtime.invalidate_time_us = self.runtime.invalidate_time_us.wrapping_add(elapsed_us);
        self.runtime.invalidate_time_max_us = self.runtime.invalidate_time_max_us.max(elapsed_us);
    }

    fn name(&self) -> &'static str {
        self.recompiler.backend_name()
    }
}
