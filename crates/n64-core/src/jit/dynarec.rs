use crate::bus::Bus;
use crate::cpu::cop0::Cop0;
use crate::cpu::instruction::Instruction;
use crate::cpu::Vr4300;
use crate::jit::{ExecutionEngine, Interpreter};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread::{self, JoinHandle};
use std::time::Instant;

use n64_dynarec::{
    BlockCompiler, CompileError, CompileRequest, CompiledBlock, CraneliftCompiler,
    CraneliftOptLevel, EnsureResult, InstructionSource, Recompiler, RecompilerConfig,
    RecompilerStats, RuntimeCallbacks,
};

const TRACKING_PAGE_SHIFT: u32 = 12;

fn page_of(addr: u32) -> u32 {
    addr >> TRACKING_PAGE_SHIFT
}

fn page_span_from_start_len(start: u32, len: u32) -> (u32, u32) {
    let end_inclusive = start.saturating_add(len.saturating_sub(1));
    (page_of(start), page_of(end_inclusive))
}

fn page_span_for_block(block: &CompiledBlock) -> (u32, u32) {
    if block.end_phys > block.start_phys {
        let end_inclusive = block.end_phys - 1;
        (page_of(block.start_phys), page_of(end_inclusive))
    } else {
        let page = page_of(block.start_phys);
        (page, page)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct DynarecRuntimeStats {
    pub native_blocks_executed: u64,
    pub native_instructions_executed: u64,
    pub native_interp_delegated_instructions: u64,
    pub native_gas_exits: u64,
    pub native_link_hits: u64,
    pub native_link_misses: u64,
    pub native_link_inserts: u64,
    pub promote_calls: u64,
    pub promote_compiled: u64,
    pub promote_compile_failed: u64,
    pub promote_cache_hit: u64,
    pub promote_skipped_budget: u64,
    pub promote_time_us: u64,
    pub promote_time_max_us: u64,
    pub promote_async_enqueued: u64,
    pub promote_async_completed: u64,
    pub promote_async_dropped_stale: u64,
    pub promote_async_queue_full: u64,
    pub promote_async_snapshot_miss: u64,
    pub promote_async_worker_down: u64,
    pub fallback_instructions_executed: u64,
    pub fallback_early_guard: u64,
    pub fallback_guard_after_lookup: u64,
    pub fallback_no_block: u64,
    pub fallback_failed_cache: u64,
    pub fallback_cold: u64,
    pub fallback_compile_budget: u64,
    pub guard_reject_zero_insn: u64,
    pub guard_reject_min_block: u64,
    pub guard_reject_interp_dominated: u64,
    pub guard_reject_delay_or_nonlinear: u64,
    pub guard_reject_non_kseg: u64,
    pub guard_reject_start_phys_mismatch: u64,
    pub guard_reject_pending_interrupt: u64,
    pub guard_reject_timer_interrupt: u64,
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
    pub native_link_cache_len: usize,
    pub native_link_fanout: u32,
    pub failed_cache_len: usize,
    pub hot_entries: usize,
    pub hot_threshold: u16,
    pub max_block_instructions: u32,
    pub tier1_max_block_instructions: u32,
    pub promote_hot_threshold: u16,
    pub async_promote_enabled: bool,
    pub async_snapshot_instructions: u32,
    pub async_queue_limit: u32,
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

#[derive(Clone, Copy)]
enum NativeGuardRejectReason {
    ZeroInsn,
    MinBlock,
    InterpDominated,
    DelayOrNonLinearPc,
    NonKsegAddress,
    StartPhysMismatch,
    PendingInterrupt,
    TimerInterrupt,
}

#[derive(Clone, Copy)]
struct NativeEdgeLink {
    target_start_phys: u32,
    target_block: CompiledBlock,
    target_first_page: u32,
    target_first_generation: u64,
    target_last_page: u32,
    target_last_generation: u64,
}

struct BusSource<'a, B: Bus> {
    bus: &'a mut B,
}

impl<B: Bus> InstructionSource for BusSource<'_, B> {
    fn read_u32(&mut self, phys_addr: u32) -> Result<u32, n64_dynarec::CompileError> {
        Ok(self.bus.read_u32(phys_addr))
    }
}

struct SnapshotSource {
    start_phys: u32,
    words: Vec<u32>,
}

impl InstructionSource for SnapshotSource {
    fn read_u32(&mut self, phys_addr: u32) -> Result<u32, CompileError> {
        if phys_addr < self.start_phys || (phys_addr - self.start_phys) & 3 != 0 {
            return Err(CompileError::MemoryRead { phys_addr });
        }
        let idx = ((phys_addr - self.start_phys) >> 2) as usize;
        self.words
            .get(idx)
            .copied()
            .ok_or(CompileError::MemoryRead { phys_addr })
    }
}

#[derive(Debug, Clone, Copy)]
enum AsyncCompileKind {
    Promote,
}

#[derive(Debug)]
struct AsyncCompileJob {
    kind: AsyncCompileKind,
    start_phys: u32,
    max_block_instructions: u32,
    snapshot_start_phys: u32,
    snapshot_words: Vec<u32>,
    page_generations: Vec<(u32, u64)>,
}

#[derive(Debug)]
struct AsyncCompileResult {
    kind: AsyncCompileKind,
    start_phys: u32,
    compile_time_us: u64,
    page_generations: Vec<(u32, u64)>,
    result: Result<CompiledBlock, CompileError>,
}

enum AsyncCompileMessage {
    Compile(AsyncCompileJob),
    Shutdown,
}

fn async_compile_worker_loop(
    rx: Receiver<AsyncCompileMessage>,
    tx: Sender<AsyncCompileResult>,
    opt_level: CraneliftOptLevel,
) {
    let mut compiler = CraneliftCompiler::with_opt_level(opt_level);
    while let Ok(message) = rx.recv() {
        match message {
            AsyncCompileMessage::Shutdown => break,
            AsyncCompileMessage::Compile(job) => {
                let mut source = SnapshotSource {
                    start_phys: job.snapshot_start_phys,
                    words: job.snapshot_words,
                };
                let started = Instant::now();
                let result = compiler.compile(
                    &CompileRequest {
                        start_phys: job.start_phys,
                        max_instructions: job.max_block_instructions,
                    },
                    &mut source,
                );
                let compile_time_us =
                    started.elapsed().as_micros().min(u128::from(u64::MAX)) as u64;
                let response = AsyncCompileResult {
                    kind: job.kind,
                    start_phys: job.start_phys,
                    compile_time_us,
                    page_generations: job.page_generations,
                    result,
                };
                if tx.send(response).is_err() {
                    break;
                }
            }
        }
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
    promote_compiler: Box<CraneliftCompiler>,
    runtime: DynarecRuntimeStats,
    hot_counts: HashMap<u32, u16>,
    promote_counts: HashMap<u32, u16>,
    promote_attempted: HashSet<u32>,
    promote_spans: HashMap<u32, (u32, u32)>,
    promote_pages: HashMap<u32, HashSet<u32>>,
    tier_floor_insns: HashMap<u32, u32>,
    native_links: HashMap<u32, Vec<NativeEdgeLink>>,
    native_link_fanout: usize,
    hot_threshold: u16,
    max_block_instructions: u32,
    tier1_max_block_instructions: u32,
    promote_hot_threshold: u16,
    async_promote_enabled: bool,
    async_snapshot_instructions: u32,
    async_queue_limit: usize,
    min_native_instructions: u32,
    native_gas_limit: u32,
    /// 0 means unlimited chaining (bounded by `native_gas_limit`).
    chain_limit: u32,
    compile_budget_us_per_ms: u32,
    compile_budget_burst_ms: u32,
    compile_budget_credit_us: i64,
    compile_budget_cap_us: i64,
    compile_budget_last_refill: Instant,
    page_generations: HashMap<u32, u64>,
    pending_promotions: HashSet<u32>,
    pending_spans: HashMap<u32, (u32, u32)>,
    pending_pages: HashMap<u32, HashSet<u32>>,
    async_tx: Option<Sender<AsyncCompileMessage>>,
    async_rx: Option<Receiver<AsyncCompileResult>>,
    async_thread: Option<JoinHandle<()>>,
}

impl DynarecEngine {
    fn parse_env_bool(name: &str, default: bool) -> bool {
        match std::env::var(name) {
            Ok(raw) => match raw.trim().to_ascii_lowercase().as_str() {
                "1" | "on" | "true" | "yes" => true,
                "0" | "off" | "false" | "no" => false,
                _ => default,
            },
            Err(_) => default,
        }
    }

    fn parse_env_u16(name: &str, default: u16) -> u16 {
        std::env::var(name)
            .ok()
            .and_then(|s| s.parse::<u16>().ok())
            .filter(|v| *v > 0)
            .unwrap_or(default)
    }

    fn parse_env_u16_allow_zero(name: &str, default: u16) -> u16 {
        std::env::var(name)
            .ok()
            .and_then(|s| s.parse::<u16>().ok())
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

    fn parse_env_opt_level(name: &str, default: CraneliftOptLevel) -> CraneliftOptLevel {
        match std::env::var(name) {
            Ok(raw) => match CraneliftOptLevel::parse(&raw) {
                Some(level) => level,
                None => {
                    log::warn!("Unknown {} value {:?}; using {:?}", name, raw, default);
                    default
                }
            },
            Err(_) => default,
        }
    }

    pub fn new_cranelift() -> Self {
        let hot_threshold = Self::parse_env_u16("N64_DYNAREC_HOT_THRESHOLD", 24);
        let max_block_instructions = Self::parse_env_u32("N64_DYNAREC_MAX_BLOCK_INSNS", 512);
        let tier1_default = max_block_instructions.min(64).max(1);
        let tier1_max_block_instructions =
            Self::parse_env_u32("N64_DYNAREC_TIER1_MAX_BLOCK_INSNS", tier1_default)
                .min(max_block_instructions)
                .max(1);
        let promote_hot_threshold =
            Self::parse_env_u16_allow_zero("N64_DYNAREC_PROMOTE_THRESHOLD", 8);
        let async_promote_enabled =
            Self::parse_env_bool("N64_DYNAREC_ASYNC_PROMOTE", true) && promote_hot_threshold > 0;
        let async_snapshot_instructions =
            Self::parse_env_u32("N64_DYNAREC_ASYNC_SNAPSHOT_INSNS", 256);
        let async_queue_limit =
            Self::parse_env_u32("N64_DYNAREC_ASYNC_QUEUE_LIMIT", 256).max(1) as usize;
        let min_native_instructions = Self::parse_env_u32("N64_DYNAREC_MIN_BLOCK_INSNS", 2);
        let native_gas_limit = Self::parse_env_u32("N64_DYNAREC_NATIVE_GAS", 8192);
        let chain_limit = Self::parse_env_u32_allow_zero("N64_DYNAREC_CHAIN_LIMIT", 0);
        let native_link_fanout = Self::parse_env_u32("N64_DYNAREC_LINK_FANOUT", 8) as usize;
        let compile_budget_us_per_ms =
            Self::parse_env_u32_allow_zero("N64_DYNAREC_COMPILE_BUDGET_US_PER_MS", 0);
        let compile_budget_burst_ms =
            Self::parse_env_u32("N64_DYNAREC_COMPILE_BUDGET_BURST_MS", 30);
        let compile_budget_cap_us =
            i64::from(compile_budget_us_per_ms.saturating_mul(compile_budget_burst_ms.max(1)));
        let global_opt_set = std::env::var("N64_CRANELIFT_OPT_LEVEL").is_ok();
        let global_opt_level =
            Self::parse_env_opt_level("N64_CRANELIFT_OPT_LEVEL", CraneliftOptLevel::None);
        let tier1_opt_level =
            Self::parse_env_opt_level("N64_DYNAREC_TIER1_OPT_LEVEL", global_opt_level);
        let tier2_default_opt = if global_opt_set {
            global_opt_level
        } else {
            CraneliftOptLevel::Speed
        };
        let tier2_opt_level =
            Self::parse_env_opt_level("N64_DYNAREC_TIER2_OPT_LEVEL", tier2_default_opt);

        let compiler = Box::new(CraneliftCompiler::with_opt_level(tier1_opt_level));
        let promote_compiler = Box::new(CraneliftCompiler::with_opt_level(tier2_opt_level));
        let recompiler = Recompiler::new(
            compiler,
            RecompilerConfig {
                max_block_instructions,
            },
        );
        let (async_tx, async_rx, async_thread) = if async_promote_enabled {
            let (job_tx, job_rx) = mpsc::channel::<AsyncCompileMessage>();
            let (result_tx, result_rx) = mpsc::channel::<AsyncCompileResult>();
            match thread::Builder::new()
                .name("n64-jit-promote".to_string())
                .spawn(move || async_compile_worker_loop(job_rx, result_tx, tier2_opt_level))
            {
                Ok(handle) => (Some(job_tx), Some(result_rx), Some(handle)),
                Err(err) => {
                    log::warn!("Failed to start async dynarec worker: {}", err);
                    (None, None, None)
                }
            }
        } else {
            (None, None, None)
        };
        Self {
            fallback: Interpreter,
            recompiler,
            promote_compiler,
            runtime: DynarecRuntimeStats::default(),
            hot_counts: HashMap::default(),
            promote_counts: HashMap::default(),
            promote_attempted: HashSet::default(),
            promote_spans: HashMap::default(),
            promote_pages: HashMap::default(),
            tier_floor_insns: HashMap::default(),
            native_links: HashMap::default(),
            native_link_fanout,
            hot_threshold,
            max_block_instructions,
            tier1_max_block_instructions,
            promote_hot_threshold,
            async_promote_enabled: async_tx.is_some(),
            async_snapshot_instructions,
            async_queue_limit,
            min_native_instructions,
            native_gas_limit,
            chain_limit,
            compile_budget_us_per_ms,
            compile_budget_burst_ms,
            compile_budget_credit_us: compile_budget_cap_us,
            compile_budget_cap_us,
            compile_budget_last_refill: Instant::now(),
            page_generations: HashMap::default(),
            pending_promotions: HashSet::default(),
            pending_spans: HashMap::default(),
            pending_pages: HashMap::default(),
            async_tx,
            async_rx,
            async_thread,
        }
    }

    #[cfg(test)]
    pub fn new_cranelift_for_tests() -> Self {
        let max_block_instructions = 256;
        let compiler = Box::new(CraneliftCompiler::with_opt_level(CraneliftOptLevel::None));
        let promote_compiler =
            Box::new(CraneliftCompiler::with_opt_level(CraneliftOptLevel::Speed));
        let recompiler = Recompiler::new(
            compiler,
            RecompilerConfig {
                max_block_instructions,
            },
        );
        Self {
            fallback: Interpreter,
            recompiler,
            promote_compiler,
            runtime: DynarecRuntimeStats::default(),
            hot_counts: HashMap::default(),
            promote_counts: HashMap::default(),
            promote_attempted: HashSet::default(),
            promote_spans: HashMap::default(),
            promote_pages: HashMap::default(),
            tier_floor_insns: HashMap::default(),
            native_links: HashMap::default(),
            native_link_fanout: 8,
            hot_threshold: 1,
            max_block_instructions,
            tier1_max_block_instructions: 2,
            promote_hot_threshold: 2,
            async_promote_enabled: false,
            async_snapshot_instructions: 256,
            async_queue_limit: 8,
            min_native_instructions: 1,
            native_gas_limit: 1024,
            chain_limit: 0,
            compile_budget_us_per_ms: 0,
            compile_budget_burst_ms: 1,
            compile_budget_credit_us: 0,
            compile_budget_cap_us: 0,
            compile_budget_last_refill: Instant::now(),
            page_generations: HashMap::default(),
            pending_promotions: HashSet::default(),
            pending_spans: HashMap::default(),
            pending_pages: HashMap::default(),
            async_tx: None,
            async_rx: None,
            async_thread: None,
        }
    }

    pub fn stats(&self) -> DynarecStats {
        let native_link_cache_len = self.native_links.values().map(Vec::len).sum::<usize>();
        DynarecStats {
            runtime: self.runtime,
            recompiler: self.recompiler.stats(),
            block_cache_len: self.recompiler.cache_len(),
            native_link_cache_len,
            native_link_fanout: self.native_link_fanout as u32,
            failed_cache_len: self.recompiler.failed_cache_len(),
            hot_entries: self.hot_counts.len(),
            hot_threshold: self.hot_threshold,
            max_block_instructions: self.max_block_instructions,
            tier1_max_block_instructions: self.tier1_max_block_instructions,
            promote_hot_threshold: self.promote_hot_threshold,
            async_promote_enabled: self.async_promote_enabled,
            async_snapshot_instructions: self.async_snapshot_instructions,
            async_queue_limit: self.async_queue_limit as u32,
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
            "native_blocks={} native_instr={} native_interp_instr={} native_gas_exits={} native_link_hits={} native_link_misses={} native_link_inserts={} promote_calls={} promote_compiled={} promote_compile_failed={} promote_cache_hit={} promote_skipped_budget={} promote_time_us={} promote_time_max_us={} promote_async_enqueued={} promote_async_completed={} promote_async_dropped_stale={} promote_async_queue_full={} promote_async_snapshot_miss={} promote_async_worker_down={} fallback_instr={} fallback_early_guard={} fallback_guard_after_lookup={} fallback_no_block={} fallback_failed_cache={} fallback_cold={} fallback_compile_budget={} guard_reject_zero_insn={} guard_reject_min_block={} guard_reject_interp_dominated={} guard_reject_delay_or_nonlinear={} guard_reject_non_kseg={} guard_reject_start_phys_mismatch={} guard_reject_pending_interrupt={} guard_reject_timer_interrupt={} ensure_calls={} ensure_compiled={} ensure_compile_failed={} ensure_cache_hit={} ensure_time_us={} ensure_time_max_us={} recompiler_cache_hits={} recompiler_failed_cache_hits={} recompiler_blocks_compiled={} recompiler_compile_failures={} recompiler_invalidated_blocks={} invalidate_calls={} invalidate_bytes={} invalidate_time_us={} invalidate_time_max_us={} block_cache_len={} native_link_cache_len={} native_link_fanout={} failed_cache_len={} hot_entries={} hot_threshold={} max_block_insns={} tier1_max_block_insns={} promote_hot_threshold={} async_promote_enabled={} async_snapshot_insns={} async_queue_limit={} min_block_insns={} native_gas={} chain_limit={} compile_budget_us_per_ms={} compile_budget_burst_ms={} compile_budget_cap_us={} compile_budget_credit_us={}",
            stats.runtime.native_blocks_executed,
            stats.runtime.native_instructions_executed,
            stats.runtime.native_interp_delegated_instructions,
            stats.runtime.native_gas_exits,
            stats.runtime.native_link_hits,
            stats.runtime.native_link_misses,
            stats.runtime.native_link_inserts,
            stats.runtime.promote_calls,
            stats.runtime.promote_compiled,
            stats.runtime.promote_compile_failed,
            stats.runtime.promote_cache_hit,
            stats.runtime.promote_skipped_budget,
            stats.runtime.promote_time_us,
            stats.runtime.promote_time_max_us,
            stats.runtime.promote_async_enqueued,
            stats.runtime.promote_async_completed,
            stats.runtime.promote_async_dropped_stale,
            stats.runtime.promote_async_queue_full,
            stats.runtime.promote_async_snapshot_miss,
            stats.runtime.promote_async_worker_down,
            stats.runtime.fallback_instructions_executed,
            stats.runtime.fallback_early_guard,
            stats.runtime.fallback_guard_after_lookup,
            stats.runtime.fallback_no_block,
            stats.runtime.fallback_failed_cache,
            stats.runtime.fallback_cold,
            stats.runtime.fallback_compile_budget,
            stats.runtime.guard_reject_zero_insn,
            stats.runtime.guard_reject_min_block,
            stats.runtime.guard_reject_interp_dominated,
            stats.runtime.guard_reject_delay_or_nonlinear,
            stats.runtime.guard_reject_non_kseg,
            stats.runtime.guard_reject_start_phys_mismatch,
            stats.runtime.guard_reject_pending_interrupt,
            stats.runtime.guard_reject_timer_interrupt,
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
            stats.native_link_cache_len,
            stats.native_link_fanout,
            stats.failed_cache_len,
            stats.hot_entries,
            stats.hot_threshold,
            stats.max_block_instructions,
            stats.tier1_max_block_instructions,
            stats.promote_hot_threshold,
            u32::from(stats.async_promote_enabled),
            stats.async_snapshot_instructions,
            stats.async_queue_limit,
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
        self.promote_counts.clear();
        self.promote_attempted.clear();
        self.promote_spans.clear();
        self.promote_pages.clear();
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

    fn record_guard_reject_reason(&mut self, reason: NativeGuardRejectReason) {
        match reason {
            NativeGuardRejectReason::ZeroInsn => self.runtime.guard_reject_zero_insn += 1,
            NativeGuardRejectReason::MinBlock => self.runtime.guard_reject_min_block += 1,
            NativeGuardRejectReason::InterpDominated => {
                self.runtime.guard_reject_interp_dominated += 1
            }
            NativeGuardRejectReason::DelayOrNonLinearPc => {
                self.runtime.guard_reject_delay_or_nonlinear += 1
            }
            NativeGuardRejectReason::NonKsegAddress => self.runtime.guard_reject_non_kseg += 1,
            NativeGuardRejectReason::StartPhysMismatch => {
                self.runtime.guard_reject_start_phys_mismatch += 1
            }
            NativeGuardRejectReason::PendingInterrupt => {
                self.runtime.guard_reject_pending_interrupt += 1
            }
            NativeGuardRejectReason::TimerInterrupt => {
                self.runtime.guard_reject_timer_interrupt += 1
            }
        }
    }

    fn should_attempt_compile(&mut self, start_phys: u32) -> bool {
        let entry = self.hot_counts.entry(start_phys).or_insert(0);
        *entry = entry.saturating_add(1);
        *entry >= self.hot_threshold
    }

    fn desired_compile_max(&self, start_phys: u32) -> u32 {
        let floor = self
            .tier_floor_insns
            .get(&start_phys)
            .copied()
            .unwrap_or(self.tier1_max_block_instructions);
        floor
            .max(self.tier1_max_block_instructions)
            .min(self.max_block_instructions)
            .max(1)
    }

    fn raise_tier_floor(&mut self, start_phys: u32, compiled_max: u32) {
        let floor = compiled_max
            .max(self.tier1_max_block_instructions)
            .min(self.max_block_instructions)
            .max(1);
        self.tier_floor_insns
            .entry(start_phys)
            .and_modify(|v| *v = (*v).max(floor))
            .or_insert(floor);
    }

    fn reindex_promote_span(&mut self, start_phys: u32, block: &CompiledBlock) {
        let span = page_span_for_block(block);
        if self.promote_spans.get(&start_phys).copied() == Some(span) {
            return;
        }
        self.remove_promote_span(start_phys);
        self.promote_spans.insert(start_phys, span);
        for page in span.0..=span.1 {
            self.promote_pages
                .entry(page)
                .or_default()
                .insert(start_phys);
        }
    }

    fn remove_promote_span(&mut self, start_phys: u32) {
        let Some((first, last)) = self.promote_spans.remove(&start_phys) else {
            return;
        };
        for page in first..=last {
            let mut remove_page = false;
            if let Some(keys) = self.promote_pages.get_mut(&page) {
                keys.remove(&start_phys);
                remove_page = keys.is_empty();
            }
            if remove_page {
                self.promote_pages.remove(&page);
            }
        }
    }

    fn remove_promote_tracking(&mut self, start_phys: u32) {
        self.promote_counts.remove(&start_phys);
        self.remove_promote_span(start_phys);
    }

    fn index_pending_promotion(&mut self, start_phys: u32, span: (u32, u32)) {
        self.pending_promotions.insert(start_phys);
        self.pending_spans.insert(start_phys, span);
        for page in span.0..=span.1 {
            self.pending_pages
                .entry(page)
                .or_default()
                .insert(start_phys);
        }
    }

    fn remove_pending_promotion(&mut self, start_phys: u32) {
        self.pending_promotions.remove(&start_phys);
        let Some((first, last)) = self.pending_spans.remove(&start_phys) else {
            return;
        };
        for page in first..=last {
            let mut remove_page = false;
            if let Some(keys) = self.pending_pages.get_mut(&page) {
                keys.remove(&start_phys);
                remove_page = keys.is_empty();
            }
            if remove_page {
                self.pending_pages.remove(&page);
            }
        }
    }

    fn collect_indexed_keys(
        index: &HashMap<u32, HashSet<u32>>,
        first_page: u32,
        last_page: u32,
    ) -> HashSet<u32> {
        let mut keys = HashSet::default();
        for page in first_page..=last_page {
            if let Some(page_keys) = index.get(&page) {
                keys.extend(page_keys.iter().copied());
            }
        }
        keys
    }

    fn snapshot_page_generations(&self, first_page: u32, last_page: u32) -> Vec<(u32, u64)> {
        let mut pages = Vec::with_capacity((last_page - first_page + 1) as usize);
        for page in first_page..=last_page {
            let generation = self.page_generations.get(&page).copied().unwrap_or(0);
            pages.push((page, generation));
        }
        pages
    }

    fn page_generations_match(&self, pages: &[(u32, u64)]) -> bool {
        pages.iter().all(|(page, generation)| {
            self.page_generations.get(page).copied().unwrap_or(0) == *generation
        })
    }

    fn bump_page_generations(&mut self, first_page: u32, last_page: u32) {
        for page in first_page..=last_page {
            let entry = self.page_generations.entry(page).or_insert(0);
            *entry = entry.wrapping_add(1);
        }
    }

    fn record_native_execution(&mut self, block: &CompiledBlock) {
        if self.promote_hot_threshold == 0
            || self.max_block_instructions <= self.tier1_max_block_instructions
        {
            return;
        }
        if self.promote_attempted.contains(&block.start_phys) {
            return;
        }
        self.reindex_promote_span(block.start_phys, block);
        let entry = self.promote_counts.entry(block.start_phys).or_insert(0);
        *entry = entry.saturating_add(1);
    }

    fn clear_native_links(&mut self) {
        self.native_links.clear();
    }

    fn native_link_target_generations_match(&self, link: &NativeEdgeLink) -> bool {
        let first_generation = self
            .page_generations
            .get(&link.target_first_page)
            .copied()
            .unwrap_or(0);
        if first_generation != link.target_first_generation {
            return false;
        }
        let last_generation = self
            .page_generations
            .get(&link.target_last_page)
            .copied()
            .unwrap_or(0);
        last_generation == link.target_last_generation
    }

    fn remove_native_link(&mut self, source_start_phys: u32, next_phys: u32) {
        let mut remove_source = false;
        if let Some(links) = self.native_links.get_mut(&source_start_phys) {
            if let Some(idx) = links
                .iter()
                .position(|link| link.target_start_phys == next_phys)
            {
                links.remove(idx);
            }
            remove_source = links.is_empty();
        }
        if remove_source {
            self.native_links.remove(&source_start_phys);
        }
    }

    fn lookup_native_link<B: Bus>(
        &mut self,
        source_start_phys: u32,
        next_phys: u32,
        cpu: &Vr4300,
        bus: &B,
    ) -> Option<CompiledBlock> {
        let Some(link) = self.native_links.get(&source_start_phys).and_then(|links| {
            links
                .iter()
                .find(|link| link.target_start_phys == next_phys)
                .copied()
        }) else {
            self.runtime.native_link_misses = self.runtime.native_link_misses.wrapping_add(1);
            return None;
        };
        if !self.native_link_target_generations_match(&link) {
            self.remove_native_link(source_start_phys, next_phys);
            self.runtime.native_link_misses = self.runtime.native_link_misses.wrapping_add(1);
            return None;
        }
        if !self.can_run_native_block(cpu, bus, &link.target_block, next_phys) {
            self.runtime.native_link_misses = self.runtime.native_link_misses.wrapping_add(1);
            return None;
        }
        self.runtime.native_link_hits = self.runtime.native_link_hits.wrapping_add(1);
        Some(link.target_block)
    }

    fn remember_native_link(
        &mut self,
        source_start_phys: u32,
        next_phys: u32,
        target_block: CompiledBlock,
    ) {
        let (target_first_page, target_last_page) = page_span_for_block(&target_block);
        let target_first_generation = self
            .page_generations
            .get(&target_first_page)
            .copied()
            .unwrap_or(0);
        let target_last_generation = self
            .page_generations
            .get(&target_last_page)
            .copied()
            .unwrap_or(0);
        let link = NativeEdgeLink {
            target_start_phys: next_phys,
            target_block,
            target_first_page,
            target_first_generation,
            target_last_page,
            target_last_generation,
        };
        let fanout = self.native_link_fanout.max(1);
        let links = self.native_links.entry(source_start_phys).or_default();
        if let Some(existing) = links
            .iter_mut()
            .find(|existing| existing.target_start_phys == next_phys)
        {
            let needs_update = existing.target_first_generation != target_first_generation
                || existing.target_last_generation != target_last_generation
                || existing.target_block.start_phys != target_block.start_phys
                || existing.target_block.end_phys != target_block.end_phys
                || existing.target_block.max_retired_instructions
                    != target_block.max_retired_instructions;
            if !needs_update {
                return;
            }
            self.runtime.native_link_inserts = self.runtime.native_link_inserts.wrapping_add(1);
            *existing = link;
            return;
        }
        self.runtime.native_link_inserts = self.runtime.native_link_inserts.wrapping_add(1);
        if links.len() >= fanout {
            links.remove(0);
        }
        links.push(link);
    }

    fn should_attempt_promotion(&mut self, start_phys: u32) -> bool {
        if self.promote_hot_threshold == 0
            || self.max_block_instructions <= self.tier1_max_block_instructions
        {
            return false;
        }
        let entry = self.promote_counts.entry(start_phys).or_insert(0);
        if *entry < self.promote_hot_threshold {
            return false;
        }
        *entry = 0;
        true
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

    fn capture_snapshot_words<B: Bus>(
        &mut self,
        bus: &mut B,
        start_phys: u32,
        max_block_instructions: u32,
    ) -> Option<(u32, Vec<u32>, (u32, u32), Vec<(u32, u64)>)> {
        let window_insns = self
            .async_snapshot_instructions
            .max(max_block_instructions)
            .max(1);

        let fastmem = bus.dynarec_fastmem().unwrap_or_default();
        if fastmem.rdram_phys_limit == 0 {
            return None;
        }
        let start_phys_u64 = u64::from(start_phys);
        if start_phys_u64 >= fastmem.rdram_phys_limit {
            return None;
        }
        let max_window_by_limit = ((fastmem.rdram_phys_limit - start_phys_u64) / 4) as u32;
        if max_window_by_limit == 0 {
            return None;
        }
        let insn_count = window_insns
            .min(max_window_by_limit)
            .max(max_block_instructions);
        if insn_count < max_block_instructions {
            return None;
        }

        let mut words = Vec::with_capacity(insn_count as usize);
        let mut phys = start_phys;
        for _ in 0..insn_count {
            words.push(bus.read_u32(phys));
            phys = phys.wrapping_add(4);
        }

        let len_bytes = insn_count.saturating_mul(4).max(1);
        let span = page_span_from_start_len(start_phys, len_bytes);
        let page_generations = self.snapshot_page_generations(span.0, span.1);
        Some((start_phys, words, span, page_generations))
    }

    fn enqueue_async_promotion<B: Bus>(
        &mut self,
        start_phys: u32,
        max_block_instructions: u32,
        bus: &mut B,
    ) -> bool {
        if !self.async_promote_enabled {
            return false;
        }
        if self.pending_promotions.contains(&start_phys) {
            return true;
        }
        if self.pending_promotions.len() >= self.async_queue_limit {
            self.runtime.promote_async_queue_full += 1;
            return false;
        }

        let Some(tx) = self.async_tx.clone() else {
            self.runtime.promote_async_worker_down += 1;
            self.async_promote_enabled = false;
            return false;
        };

        let Some((snapshot_start_phys, snapshot_words, span, page_generations)) =
            self.capture_snapshot_words(bus, start_phys, max_block_instructions)
        else {
            self.runtime.promote_async_snapshot_miss += 1;
            return false;
        };

        let job = AsyncCompileJob {
            kind: AsyncCompileKind::Promote,
            start_phys,
            max_block_instructions,
            snapshot_start_phys,
            snapshot_words,
            page_generations,
        };
        if tx.send(AsyncCompileMessage::Compile(job)).is_err() {
            self.runtime.promote_async_worker_down += 1;
            self.async_promote_enabled = false;
            return false;
        }

        self.promote_attempted.insert(start_phys);
        self.index_pending_promotion(start_phys, span);
        self.runtime.promote_calls += 1;
        self.runtime.promote_async_enqueued += 1;
        true
    }

    fn handle_async_compile_result(&mut self, result: AsyncCompileResult) {
        self.runtime.promote_async_completed += 1;
        self.runtime.promote_time_us = self
            .runtime
            .promote_time_us
            .wrapping_add(result.compile_time_us);
        self.runtime.promote_time_max_us =
            self.runtime.promote_time_max_us.max(result.compile_time_us);

        if matches!(result.kind, AsyncCompileKind::Promote) {
            self.remove_pending_promotion(result.start_phys);
        }

        if !self.page_generations_match(&result.page_generations) {
            self.runtime.promote_async_dropped_stale += 1;
            self.promote_attempted.remove(&result.start_phys);
            return;
        }

        match result.result {
            Ok(block) => {
                let existing_max = self
                    .recompiler
                    .lookup(result.start_phys)
                    .map(|b| b.max_retired_instructions)
                    .unwrap_or(0);
                if existing_max >= block.max_retired_instructions {
                    self.runtime.promote_cache_hit += 1;
                    return;
                }
                self.clear_native_links();
                self.recompiler
                    .install_compiled_block(result.start_phys, block);
                self.runtime.promote_compiled += 1;
                self.raise_tier_floor(result.start_phys, block.max_retired_instructions);
                self.remove_promote_tracking(result.start_phys);
            }
            Err(err) => {
                self.runtime.promote_compile_failed += 1;
                log::debug!(
                    "Dynarec async promote failed at {:#010X} (backend={}): {:?}",
                    result.start_phys,
                    self.recompiler.backend_name(),
                    err
                );
            }
        }
    }

    fn drain_async_compile_results(&mut self) {
        loop {
            let next = {
                let Some(rx) = self.async_rx.as_ref() else {
                    return;
                };
                rx.try_recv()
            };
            match next {
                Ok(result) => self.handle_async_compile_result(result),
                Err(TryRecvError::Empty) => break,
                Err(TryRecvError::Disconnected) => {
                    self.async_promote_enabled = false;
                    self.async_rx = None;
                    self.async_tx = None;
                    break;
                }
            }
        }
    }

    fn maybe_promote_block<B: Bus>(&mut self, start_phys: u32, block: CompiledBlock, bus: &mut B) {
        if block.max_retired_instructions >= self.max_block_instructions {
            self.raise_tier_floor(start_phys, block.max_retired_instructions);
            self.promote_attempted.insert(start_phys);
            self.remove_pending_promotion(start_phys);
            self.remove_promote_tracking(start_phys);
            return;
        }
        if self.promote_attempted.contains(&start_phys) {
            return;
        }
        if !self.should_attempt_promotion(start_phys) {
            return;
        }
        if self.enqueue_async_promotion(start_phys, self.max_block_instructions, bus) {
            return;
        }
        if !self.compile_budget_allows_attempt() {
            self.runtime.promote_skipped_budget += 1;
            return;
        }

        self.promote_attempted.insert(start_phys);
        self.runtime.promote_calls += 1;
        let compile_start = Instant::now();
        let compile_result = {
            let request = CompileRequest {
                start_phys,
                max_instructions: self.max_block_instructions.max(1),
            };
            let mut source = BusSource { bus };
            self.promote_compiler.compile(&request, &mut source)
        };
        let compile_elapsed_us = compile_start
            .elapsed()
            .as_micros()
            .min(u128::from(u64::MAX)) as u64;
        self.runtime.promote_time_us = self
            .runtime
            .promote_time_us
            .wrapping_add(compile_elapsed_us);
        self.runtime.promote_time_max_us = self.runtime.promote_time_max_us.max(compile_elapsed_us);
        self.charge_compile_budget(compile_elapsed_us);

        match compile_result {
            Ok(promoted) => {
                let existing_max = self
                    .recompiler
                    .lookup(start_phys)
                    .map(|cached| cached.max_retired_instructions)
                    .unwrap_or(0);
                if existing_max >= promoted.max_retired_instructions {
                    self.runtime.promote_cache_hit += 1;
                    self.remove_promote_tracking(start_phys);
                    return;
                }
                self.clear_native_links();
                self.recompiler.install_compiled_block(start_phys, promoted);
                self.runtime.promote_compiled += 1;
                self.raise_tier_floor(start_phys, promoted.max_retired_instructions);
                self.remove_promote_tracking(start_phys);
            }
            Err(err) => {
                self.runtime.promote_compile_failed += 1;
                log::debug!(
                    "Dynarec promote failed at {:#010X} (backend={}): {:?}",
                    start_phys,
                    self.recompiler.backend_name(),
                    err
                );
            }
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

    fn external_interrupt_would_fire(cpu: &Vr4300, bus: &impl Bus) -> bool {
        if !bus.pending_interrupts() {
            return false;
        }

        let status = cpu.cop0.regs[Cop0::STATUS] as u32;
        let ie = (status & 1) != 0;
        let exl = ((status >> 1) & 1) != 0;
        let erl = ((status >> 2) & 1) != 0;
        let im2 = ((status >> 10) & 1) != 0;
        ie && !exl && !erl && im2
    }

    fn block_timer_guard_instructions(block: &CompiledBlock) -> u32 {
        if block.has_backedge {
            block.max_retired_instructions
        } else {
            block.instruction_count.max(1)
        }
    }

    fn native_guard_reject_reason(
        &self,
        cpu: &Vr4300,
        bus: &impl Bus,
        block: &CompiledBlock,
        start_phys: u32,
    ) -> Option<NativeGuardRejectReason> {
        if block.instruction_count == 0 {
            return Some(NativeGuardRejectReason::ZeroInsn);
        }
        if block.instruction_count < self.min_native_instructions {
            return Some(NativeGuardRejectReason::MinBlock);
        }

        // Blocks dominated by interpreter-delegated ops are usually slower than
        // the plain interpreter path.
        if block.interp_op_count != 0 && block.interp_op_count >= block.instruction_count {
            return Some(NativeGuardRejectReason::InterpDominated);
        }

        // Delay-slot and non-linear PC paths need exact per-instruction handling.
        if cpu.in_delay_slot || cpu.next_pc != cpu.pc.wrapping_add(4) {
            return Some(NativeGuardRejectReason::DelayOrNonLinearPc);
        }

        // Restrict to kseg0/kseg1 direct mappings for now.
        let pc32 = cpu.pc as u32;
        if !(0x8000_0000..=0xBFFF_FFFF).contains(&pc32) {
            return Some(NativeGuardRejectReason::NonKsegAddress);
        }

        if start_phys != (pc32 & 0x1FFF_FFFF) {
            return Some(NativeGuardRejectReason::StartPhysMismatch);
        }

        // If an interrupt could be taken, we must stay instruction-granular.
        if cpu.cop0.interrupt_pending()
            || Self::external_interrupt_would_fire(cpu, bus)
            || (bus.pending_interrupts() && block.may_write_interrupt_state)
        {
            return Some(NativeGuardRejectReason::PendingInterrupt);
        }

        let timer_guard_insns = Self::block_timer_guard_instructions(block);
        if Self::timer_interrupt_would_fire(cpu, timer_guard_insns) {
            return Some(NativeGuardRejectReason::TimerInterrupt);
        }

        None
    }

    fn can_run_native_block(
        &self,
        cpu: &Vr4300,
        bus: &impl Bus,
        block: &CompiledBlock,
        start_phys: u32,
    ) -> bool {
        self.native_guard_reject_reason(cpu, bus, block, start_phys)
            .is_none()
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
            hi_ptr: (&mut cpu.hi as *mut u64),
            lo_ptr: (&mut cpu.lo as *mut u64),
            cop1_fcr31_ptr: (&mut cpu.cop1.fcr31 as *mut u32),
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
        self.record_native_execution(block);

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
            let source_start_phys = block.start_phys;
            let retired = self.run_native_block(cpu, bus, &block);
            if retired == 0 {
                break;
            }
            total_retired = total_retired.wrapping_add(retired);
            if total_retired >= gas_limit {
                self.runtime.native_gas_exits = self.runtime.native_gas_exits.wrapping_add(1);
                break;
            }

            self.maybe_promote_block(source_start_phys, block, bus);

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
            if let Some(linked_block) =
                self.lookup_native_link(source_start_phys, next_phys, cpu, &*bus)
            {
                block = linked_block;
                continue;
            }
            let Some(next_block) = self.recompiler.lookup(next_phys).copied() else {
                break;
            };
            if !self.can_run_native_block(cpu, bus, &next_block, next_phys) {
                break;
            }
            self.remember_native_link(source_start_phys, next_phys, next_block);
            block = next_block;
        }

        total_retired
    }
}

impl ExecutionEngine for DynarecEngine {
    fn execute(&mut self, cpu: &mut Vr4300, bus: &mut impl Bus) -> u64 {
        self.drain_async_compile_results();
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
            self.raise_tier_floor(start_phys, block.max_retired_instructions);
            if let Some(reason) = self.native_guard_reject_reason(cpu, bus, &block, start_phys) {
                self.record_guard_reject_reason(reason);
                return self.run_fallback(cpu, bus, FallbackReason::GuardAfterLookup);
            }
            return self.run_native_chain(cpu, bus, block);
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
        let compile_max = self.desired_compile_max(start_phys);
        let compile_start = Instant::now();
        let ensure_result = {
            let mut source = BusSource { bus };
            self.recompiler
                .ensure_compiled_with_max(start_phys, compile_max, &mut source)
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
                self.remove_promote_tracking(start_phys);
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
            self.raise_tier_floor(start_phys, block.max_retired_instructions);
            if let Some(reason) = self.native_guard_reject_reason(cpu, bus, &block, start_phys) {
                self.record_guard_reject_reason(reason);
                return self.run_fallback(cpu, bus, FallbackReason::GuardAfterLookup);
            }
            return self.run_native_chain(cpu, bus, block);
        }

        self.run_fallback(cpu, bus, FallbackReason::NoBlock)
    }

    fn invalidate_range(&mut self, start: u32, len: u32) {
        if len == 0 {
            return;
        }
        let invalidate_start = Instant::now();
        let (first_page, last_page) = page_span_from_start_len(start, len);
        self.bump_page_generations(first_page, last_page);
        self.recompiler.invalidate_range(start, len);

        let promote_drop = Self::collect_indexed_keys(&self.promote_pages, first_page, last_page);
        for key in promote_drop {
            self.remove_promote_tracking(key);
            self.promote_attempted.remove(&key);
        }

        let pending_drop = Self::collect_indexed_keys(&self.pending_pages, first_page, last_page);
        for key in pending_drop {
            self.remove_pending_promotion(key);
            self.promote_attempted.remove(&key);
        }

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

impl Drop for DynarecEngine {
    fn drop(&mut self) {
        if let Some(tx) = self.async_tx.take() {
            let _ = tx.send(AsyncCompileMessage::Shutdown);
        }
        if let Some(handle) = self.async_thread.take() {
            let _ = handle.join();
        }
    }
}
