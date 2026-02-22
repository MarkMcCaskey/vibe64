//! Dynarec infrastructure shared by emulator cores.
//!
//! This crate keeps backend/compiler concerns separate from core emulation logic.
//! The first backend is Cranelift.

use std::collections::{HashMap, HashSet};

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, AbiParam, FuncRef, InstBuilder, MemFlags, Type, Value};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};

/// Input needed to compile a guest basic block.
#[derive(Debug, Clone, Copy)]
pub struct CompileRequest {
    /// Start physical address (guest space) of the block.
    pub start_phys: u32,
    /// Maximum number of instructions to include in a block.
    pub max_instructions: u32,
}

/// Guest instruction source used by the compiler.
pub trait InstructionSource {
    fn read_u32(&mut self, phys_addr: u32) -> Result<u32, CompileError>;
}

/// Runtime callbacks used by generated code for memory/COP0 operations.
#[repr(C)]
pub struct RuntimeCallbacks {
    pub user: *mut u8,
    pub load_u8: unsafe extern "C" fn(*mut u8, u64) -> u64,
    pub load_u16: unsafe extern "C" fn(*mut u8, u64) -> u64,
    pub load_u32: unsafe extern "C" fn(*mut u8, u64) -> u64,
    pub load_u64: unsafe extern "C" fn(*mut u8, u64) -> u64,
    pub store_u8: unsafe extern "C" fn(*mut u8, u64, u64),
    pub store_u16: unsafe extern "C" fn(*mut u8, u64, u64),
    pub store_u32: unsafe extern "C" fn(*mut u8, u64, u64),
    pub store_u64: unsafe extern "C" fn(*mut u8, u64, u64),
    /// Notify runtime that a fastmem store touched `phys..phys+len`.
    pub store_invalidate: unsafe extern "C" fn(*mut u8, u64, u64),
    pub cop0_read: unsafe extern "C" fn(*mut u8, u64) -> u64,
    pub cop0_write: unsafe extern "C" fn(*mut u8, u64, u64),
    pub cop1_condition: unsafe extern "C" fn(*mut u8) -> u64,
    pub interp_exec: unsafe extern "C" fn(*mut u8, u64, u64),
    pub hi_read: unsafe extern "C" fn(*mut u8) -> u64,
    pub hi_write: unsafe extern "C" fn(*mut u8, u64),
    pub lo_read: unsafe extern "C" fn(*mut u8) -> u64,
    pub lo_write: unsafe extern "C" fn(*mut u8, u64),
    /// Optional direct pointer to CPU HI register for fast native access.
    pub hi_ptr: *mut u64,
    /// Optional direct pointer to CPU LO register for fast native access.
    pub lo_ptr: *mut u64,
    /// Optional direct pointer to COP1 FCR31 for BC1* condition checks.
    pub cop1_fcr31_ptr: *mut u32,
    /// Optional fastmem base pointer (RDRAM backing). Null disables fastmem.
    pub fastmem_base: *mut u8,
    /// Exclusive physical range limit for fastmem loads/stores.
    pub fastmem_phys_limit: u64,
    /// Physical address mirror mask for fastmem (e.g. RDRAM size mask).
    pub fastmem_phys_mask: u64,
}

unsafe extern "C" fn n64_jit_load_u8(ctx: *mut u8, vaddr: u64) -> u64 {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.load_u8)(cbs.user, vaddr) }
}

unsafe extern "C" fn n64_jit_load_u16(ctx: *mut u8, vaddr: u64) -> u64 {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.load_u16)(cbs.user, vaddr) }
}

unsafe extern "C" fn n64_jit_load_u32(ctx: *mut u8, vaddr: u64) -> u64 {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.load_u32)(cbs.user, vaddr) }
}

unsafe extern "C" fn n64_jit_load_u64(ctx: *mut u8, vaddr: u64) -> u64 {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.load_u64)(cbs.user, vaddr) }
}

unsafe extern "C" fn n64_jit_store_u8(ctx: *mut u8, vaddr: u64, value: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.store_u8)(cbs.user, vaddr, value) }
}

unsafe extern "C" fn n64_jit_store_u16(ctx: *mut u8, vaddr: u64, value: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.store_u16)(cbs.user, vaddr, value) }
}

unsafe extern "C" fn n64_jit_store_u32(ctx: *mut u8, vaddr: u64, value: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.store_u32)(cbs.user, vaddr, value) }
}

unsafe extern "C" fn n64_jit_store_u64(ctx: *mut u8, vaddr: u64, value: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.store_u64)(cbs.user, vaddr, value) }
}

unsafe extern "C" fn n64_jit_store_invalidate(ctx: *mut u8, phys: u64, len: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.store_invalidate)(cbs.user, phys, len) }
}

unsafe extern "C" fn n64_jit_cop0_read(ctx: *mut u8, reg: u64) -> u64 {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.cop0_read)(cbs.user, reg) }
}

unsafe extern "C" fn n64_jit_cop0_write(ctx: *mut u8, reg: u64, value: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.cop0_write)(cbs.user, reg, value) }
}

unsafe extern "C" fn n64_jit_cop1_condition(ctx: *mut u8) -> u64 {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.cop1_condition)(cbs.user) }
}

unsafe extern "C" fn n64_jit_interp_exec(ctx: *mut u8, raw: u64, current_pc: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.interp_exec)(cbs.user, raw, current_pc) }
}

unsafe extern "C" fn n64_jit_hi_read(ctx: *mut u8) -> u64 {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.hi_read)(cbs.user) }
}

unsafe extern "C" fn n64_jit_hi_write(ctx: *mut u8, value: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.hi_write)(cbs.user, value) }
}

unsafe extern "C" fn n64_jit_lo_read(ctx: *mut u8) -> u64 {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.lo_read)(cbs.user) }
}

unsafe extern "C" fn n64_jit_lo_write(ctx: *mut u8, value: u64) {
    // SAFETY: `ctx` is provided by the caller as a valid RuntimeCallbacks pointer.
    let cbs = unsafe { &mut *(ctx as *mut RuntimeCallbacks) };
    // SAFETY: callback function pointer is provided by caller.
    unsafe { (cbs.lo_write)(cbs.user, value) }
}

type JitBlockFn = unsafe extern "C" fn(*mut u64, *mut u64, u64, *mut u8, *mut u32, u32) -> u64;

/// Pointer to compiled native entry point.
#[derive(Clone, Copy)]
pub struct BlockEntry(*const u8);

impl BlockEntry {
    fn as_fn(self) -> JitBlockFn {
        // SAFETY: entry pointers are returned by Cranelift JIT with this exact
        // signature when blocks are compiled.
        unsafe { std::mem::transmute(self.0) }
    }
}

// SAFETY: `BlockEntry` is an immutable pointer to finalized executable memory
// owned by the JIT module. Sharing or moving the pointer between threads does
// not mutate underlying state; execution synchronization is managed by callers.
unsafe impl Send for BlockEntry {}
// SAFETY: same rationale as `Send`; the pointer is immutable metadata.
unsafe impl Sync for BlockEntry {}

impl std::fmt::Debug for BlockEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BlockEntry({:p})", self.0)
    }
}

/// Metadata and executable for one compiled block.
#[derive(Debug, Clone, Copy)]
pub struct CompiledBlock {
    /// Inclusive block start address in guest physical space.
    pub start_phys: u32,
    /// Exclusive block end address in guest physical space.
    pub end_phys: u32,
    /// Number of guest instructions included in this block.
    pub instruction_count: u32,
    /// Number of instructions in this block delegated via `Op::Interp`.
    pub interp_op_count: u32,
    /// Maximum number of guest instructions one native entry may retire.
    pub max_retired_instructions: u32,
    /// True when this block may enable or signal interrupts via COP0 writes or
    /// interpreter-delegated instructions.
    pub may_write_interrupt_state: bool,
    /// True when this trace contains a control-flow backedge.
    pub has_backedge: bool,
    /// True if the block ended on a control-transfer boundary.
    pub has_control_flow: bool,
    /// True when compilation stopped at an unsupported non-branch opcode.
    pub ended_on_unsupported: bool,
    entry: BlockEntry,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockExecution {
    pub next_pc: u64,
    pub retired_instructions: u32,
}

impl CompiledBlock {
    /// Execute this compiled block against the GPR/FPR file with an explicit
    /// upper bound on retired instructions for this invocation.
    pub fn execute_with_limit(
        &self,
        gpr: &mut [u64; 32],
        fpr: &mut [u64; 32],
        start_pc: u64,
        callbacks: *mut RuntimeCallbacks,
        retire_limit: u32,
    ) -> BlockExecution {
        let mut retired = 0u32;
        // SAFETY: compiled blocks are generated with signature
        // `extern "C" fn(*mut u64, *mut u64, u64, *mut u8, *mut u32, u32) -> u64`,
        // `gpr`/`fpr` point to 32 contiguous u64s, and callback pointer comes
        // from the caller.
        let next_pc = unsafe {
            (self.entry.as_fn())(
                gpr.as_mut_ptr(),
                fpr.as_mut_ptr(),
                start_pc,
                callbacks.cast::<u8>(),
                (&mut retired as *mut u32).cast::<u32>(),
                retire_limit.max(1),
            )
        };
        BlockExecution {
            next_pc,
            retired_instructions: retired,
        }
    }

    /// Execute this compiled block against the GPR file.
    pub fn execute(
        &self,
        gpr: &mut [u64; 32],
        fpr: &mut [u64; 32],
        start_pc: u64,
        callbacks: *mut RuntimeCallbacks,
    ) -> BlockExecution {
        self.execute_with_limit(gpr, fpr, start_pc, callbacks, self.max_retired_instructions)
    }
}

/// Compilation failure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileError {
    /// Source memory could not be read.
    MemoryRead { phys_addr: u32 },
    /// Instruction is not currently supported by the backend.
    UnsupportedOpcode { phys_addr: u32, opcode: u32 },
    /// Generic backend failure.
    Backend { message: String },
}

/// Backend trait for block compilers.
///
/// Backends should treat `start_phys` as the cache key. Invalidation is always
/// performed in guest physical addresses.
pub trait BlockCompiler {
    fn name(&self) -> &'static str;
    fn compile(
        &mut self,
        request: &CompileRequest,
        source: &mut dyn InstructionSource,
    ) -> Result<CompiledBlock, CompileError>;
    fn invalidate_range(&mut self, _start_phys: u32, _len: u32) {}
}

/// Compiler configuration.
#[derive(Debug, Clone, Copy)]
pub struct RecompilerConfig {
    pub max_block_instructions: u32,
}

impl Default for RecompilerConfig {
    fn default() -> Self {
        Self {
            max_block_instructions: 64,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EnsureResult {
    CacheHit,
    Compiled,
    CompileFailed,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct RecompilerStats {
    pub cache_hits: u64,
    pub failed_cache_hits: u64,
    pub blocks_compiled: u64,
    pub compile_failures: u64,
    pub invalidated_blocks: u64,
}

const INVALIDATION_PAGE_SHIFT: u32 = 12;

fn page_of(addr: u32) -> u32 {
    addr >> INVALIDATION_PAGE_SHIFT
}

fn page_span_from_start_len(start: u32, len: u32) -> (u32, u32) {
    let end_inclusive = start.saturating_add(len.saturating_sub(1));
    (page_of(start), page_of(end_inclusive))
}

fn page_span_for_block(block: &CompiledBlock) -> (u32, u32) {
    // end_phys is exclusive. If malformed/wrapped, conservatively index only
    // the start page.
    if block.end_phys > block.start_phys {
        let end_inclusive = block.end_phys - 1;
        (page_of(block.start_phys), page_of(end_inclusive))
    } else {
        let page = page_of(block.start_phys);
        (page, page)
    }
}

/// Core recompiler pipeline: cache + backend compiler.
pub struct Recompiler {
    compiler: Box<dyn BlockCompiler>,
    config: RecompilerConfig,
    cache: HashMap<u32, CompiledBlock>,
    cache_pages: HashMap<u32, HashSet<u32>>,
    failed_cache: HashSet<u32>,
    failed_pages: HashMap<u32, HashSet<u32>>,
    stats: RecompilerStats,
    last_error: Option<CompileError>,
}

impl Recompiler {
    pub fn new(compiler: Box<dyn BlockCompiler>, config: RecompilerConfig) -> Self {
        Self {
            compiler,
            config,
            cache: HashMap::new(),
            cache_pages: HashMap::new(),
            failed_cache: HashSet::new(),
            failed_pages: HashMap::new(),
            stats: RecompilerStats::default(),
            last_error: None,
        }
    }

    pub fn backend_name(&self) -> &'static str {
        self.compiler.name()
    }

    pub fn cache_len(&self) -> usize {
        self.cache.len()
    }

    pub fn failed_cache_len(&self) -> usize {
        self.failed_cache.len()
    }

    pub fn stats(&self) -> RecompilerStats {
        self.stats
    }

    pub fn reset_stats(&mut self) {
        self.stats = RecompilerStats::default();
        self.last_error = None;
    }

    pub fn last_error(&self) -> Option<&CompileError> {
        self.last_error.as_ref()
    }

    pub fn lookup(&self, start_phys: u32) -> Option<&CompiledBlock> {
        self.cache.get(&start_phys)
    }

    /// Returns true when any compiled block currently overlaps `page`.
    pub fn has_cached_page(&self, page: u32) -> bool {
        self.cache_pages.contains_key(&page)
    }

    /// Returns true when any compiled block overlaps `start_phys..start_phys+len`.
    pub fn has_cached_overlap(&self, start_phys: u32, len: u32) -> bool {
        if len == 0 {
            return false;
        }
        let (first_page, last_page) = page_span_from_start_len(start_phys, len);
        let end_phys = start_phys.saturating_add(len);
        for page in first_page..=last_page {
            let Some(keys) = self.cache_pages.get(&page) else {
                continue;
            };
            for key in keys {
                let Some(block) = self.cache.get(key) else {
                    continue;
                };
                if block.start_phys < end_phys && start_phys < block.end_phys {
                    return true;
                }
            }
        }
        false
    }

    pub fn is_failed_cached(&self, start_phys: u32) -> bool {
        self.failed_cache.contains(&start_phys)
    }

    /// Mark a start address as permanently non-native until invalidation.
    ///
    /// This removes any cached block at `start_phys` and inserts a failed-cache
    /// marker so hot paths stop recompiling or probing guards repeatedly.
    pub fn blacklist_start(&mut self, start_phys: u32) -> bool {
        let removed_cached = self.remove_cached(start_phys);
        let was_failed = self.failed_cache.contains(&start_phys);
        self.insert_failed(start_phys);
        removed_cached || !was_failed
    }

    /// Install an externally compiled block into the cache.
    ///
    /// This is used by async compilation pipelines where machine code is
    /// produced off-thread and published to the main recompiler cache.
    pub fn install_compiled_block(&mut self, start_phys: u32, block: CompiledBlock) {
        self.stats.blocks_compiled += 1;
        self.remove_failed(start_phys);
        self.insert_cached(start_phys, block);
    }

    pub fn ensure_compiled(
        &mut self,
        start_phys: u32,
        source: &mut dyn InstructionSource,
    ) -> EnsureResult {
        self.ensure_compiled_with_max(start_phys, self.config.max_block_instructions, source)
    }

    pub fn ensure_compiled_with_max(
        &mut self,
        start_phys: u32,
        max_block_instructions: u32,
        source: &mut dyn InstructionSource,
    ) -> EnsureResult {
        if self.cache.contains_key(&start_phys) {
            self.stats.cache_hits += 1;
            return EnsureResult::CacheHit;
        }
        if self.failed_cache.contains(&start_phys) {
            self.stats.cache_hits += 1;
            self.stats.failed_cache_hits += 1;
            return EnsureResult::CacheHit;
        }

        let request = CompileRequest {
            start_phys,
            max_instructions: max_block_instructions.max(1),
        };
        match self.compiler.compile(&request, source) {
            Ok(block) => {
                self.stats.blocks_compiled += 1;
                self.remove_failed(start_phys);
                self.insert_cached(start_phys, block);
                EnsureResult::Compiled
            }
            Err(err) => {
                self.stats.compile_failures += 1;
                self.insert_failed(start_phys);
                self.last_error = Some(err);
                EnsureResult::CompileFailed
            }
        }
    }

    /// Recompile a cached block with a new instruction budget.
    ///
    /// If no block is cached yet, this falls back to `ensure_compiled_with_max`.
    /// If compilation fails for an existing cached block, the old block is kept.
    pub fn recompile_with_max(
        &mut self,
        start_phys: u32,
        max_block_instructions: u32,
        source: &mut dyn InstructionSource,
    ) -> EnsureResult {
        let Some(existing) = self.cache.get(&start_phys).copied() else {
            return self.ensure_compiled_with_max(start_phys, max_block_instructions, source);
        };

        let requested_max = max_block_instructions.max(1);
        if existing.max_retired_instructions >= requested_max {
            self.stats.cache_hits += 1;
            return EnsureResult::CacheHit;
        }

        let request = CompileRequest {
            start_phys,
            max_instructions: requested_max,
        };
        match self.compiler.compile(&request, source) {
            Ok(block) => {
                self.stats.blocks_compiled += 1;
                self.insert_cached(start_phys, block);
                EnsureResult::Compiled
            }
            Err(err) => {
                self.stats.compile_failures += 1;
                self.last_error = Some(err);
                EnsureResult::CompileFailed
            }
        }
    }

    pub fn invalidate_range(&mut self, start_phys: u32, len: u32) {
        if len == 0 {
            return;
        }
        let (first_page, last_page) = page_span_from_start_len(start_phys, len);
        let end_phys = start_phys.saturating_add(len);

        let mut cached_to_drop = HashSet::new();
        for page in first_page..=last_page {
            if let Some(keys) = self.cache_pages.get(&page) {
                cached_to_drop.extend(keys.iter().copied());
            }
        }
        for key in cached_to_drop {
            let should_drop = self
                .cache
                .get(&key)
                .map(|block| block.start_phys < end_phys && start_phys < block.end_phys)
                .unwrap_or(false);
            if should_drop && self.remove_cached(key) {
                self.stats.invalidated_blocks += 1;
            }
        }

        let mut failed_to_drop = HashSet::new();
        for page in first_page..=last_page {
            if let Some(keys) = self.failed_pages.get(&page) {
                failed_to_drop.extend(keys.iter().copied());
            }
        }
        for key in failed_to_drop {
            if key >= start_phys && key < end_phys && self.remove_failed(key) {
                self.stats.invalidated_blocks += 1;
            }
        }

        self.compiler.invalidate_range(start_phys, len);
    }

    fn insert_cached(&mut self, start_phys: u32, block: CompiledBlock) {
        if self.cache.contains_key(&start_phys) {
            // Keep the page index coherent if we ever overwrite an entry.
            self.remove_cached(start_phys);
        }
        self.cache.insert(start_phys, block);
        let (first_page, last_page) = page_span_for_block(&block);
        for page in first_page..=last_page {
            self.cache_pages.entry(page).or_default().insert(start_phys);
        }
    }

    fn remove_cached(&mut self, start_phys: u32) -> bool {
        let Some(block) = self.cache.remove(&start_phys) else {
            return false;
        };
        let (first_page, last_page) = page_span_for_block(&block);
        for page in first_page..=last_page {
            let mut remove_page = false;
            if let Some(keys) = self.cache_pages.get_mut(&page) {
                keys.remove(&start_phys);
                remove_page = keys.is_empty();
            }
            if remove_page {
                self.cache_pages.remove(&page);
            }
        }
        true
    }

    fn insert_failed(&mut self, start_phys: u32) {
        if self.failed_cache.insert(start_phys) {
            let page = page_of(start_phys);
            self.failed_pages
                .entry(page)
                .or_default()
                .insert(start_phys);
        }
    }

    fn remove_failed(&mut self, start_phys: u32) -> bool {
        if !self.failed_cache.remove(&start_phys) {
            return false;
        }
        let page = page_of(start_phys);
        let mut remove_page = false;
        if let Some(keys) = self.failed_pages.get_mut(&page) {
            keys.remove(&start_phys);
            remove_page = keys.is_empty();
        }
        if remove_page {
            self.failed_pages.remove(&page);
        }
        true
    }
}

#[derive(Debug, Clone, Copy)]
enum Op {
    Addi { rs: u8, rt: u8, imm: i16 },
    Addiu { rs: u8, rt: u8, imm: i16 },
    Daddiu { rs: u8, rt: u8, imm: i16 },
    Slti { rs: u8, rt: u8, imm: i16 },
    Sltiu { rs: u8, rt: u8, imm: i16 },
    Andi { rs: u8, rt: u8, imm: u16 },
    Ori { rs: u8, rt: u8, imm: u16 },
    Xori { rs: u8, rt: u8, imm: u16 },
    Lui { rt: u8, imm: i16 },
    Addu { rs: u8, rt: u8, rd: u8 },
    Subu { rs: u8, rt: u8, rd: u8 },
    And { rs: u8, rt: u8, rd: u8 },
    Or { rs: u8, rt: u8, rd: u8 },
    Xor { rs: u8, rt: u8, rd: u8 },
    Nor { rs: u8, rt: u8, rd: u8 },
    Slt { rs: u8, rt: u8, rd: u8 },
    Sltu { rs: u8, rt: u8, rd: u8 },
    Daddu { rs: u8, rt: u8, rd: u8 },
    Dsubu { rs: u8, rt: u8, rd: u8 },
    Sll { rt: u8, rd: u8, sa: u8 },
    Srl { rt: u8, rd: u8, sa: u8 },
    Sra { rt: u8, rd: u8, sa: u8 },
    Sllv { rs: u8, rt: u8, rd: u8 },
    Srlv { rs: u8, rt: u8, rd: u8 },
    Srav { rs: u8, rt: u8, rd: u8 },
    Mfhi { rd: u8 },
    Mthi { rs: u8 },
    Mflo { rd: u8 },
    Mtlo { rs: u8 },
    Mult { rs: u8, rt: u8 },
    Multu { rs: u8, rt: u8 },
    Div { rs: u8, rt: u8 },
    Divu { rs: u8, rt: u8 },
    Dmult { rs: u8, rt: u8 },
    Dmultu { rs: u8, rt: u8 },
    Ddiv { rs: u8, rt: u8 },
    Ddivu { rs: u8, rt: u8 },
    Lb { base: u8, rt: u8, imm: i16 },
    Lh { base: u8, rt: u8, imm: i16 },
    Lhu { base: u8, rt: u8, imm: i16 },
    Lw { base: u8, rt: u8, imm: i16 },
    Lwu { base: u8, rt: u8, imm: i16 },
    Ld { base: u8, rt: u8, imm: i16 },
    Lbu { base: u8, rt: u8, imm: i16 },
    Sw { base: u8, rt: u8, imm: i16 },
    Sb { base: u8, rt: u8, imm: i16 },
    Sh { base: u8, rt: u8, imm: i16 },
    Sd { base: u8, rt: u8, imm: i16 },
    Lwc1 { base: u8, ft: u8, imm: i16 },
    Swc1 { base: u8, ft: u8, imm: i16 },
    Ldc1 { base: u8, ft: u8, imm: i16 },
    Sdc1 { base: u8, ft: u8, imm: i16 },
    Mfc0 { rt: u8, rd: u8 },
    Dmfc0 { rt: u8, rd: u8 },
    Mtc0 { rt: u8, rd: u8 },
    Dmtc0 { rt: u8, rd: u8 },
    Mfc1 { rt: u8, fs: u8 },
    Mtc1 { rt: u8, fs: u8 },
    AddS { fd: u8, fs: u8, ft: u8 },
    SubS { fd: u8, fs: u8, ft: u8 },
    MulS { fd: u8, fs: u8, ft: u8 },
    DivS { fd: u8, fs: u8, ft: u8 },
    AddD { fd: u8, fs: u8, ft: u8 },
    SubD { fd: u8, fs: u8, ft: u8 },
    MulD { fd: u8, fs: u8, ft: u8 },
    DivD { fd: u8, fs: u8, ft: u8 },
    Interp { raw: u32 },
    Sync,
    Cache,
}

#[derive(Debug, Clone, Copy)]
enum BranchTerminator {
    Beq { rs: u8, rt: u8, offset: i16 },
    Bne { rs: u8, rt: u8, offset: i16 },
    Beql { rs: u8, rt: u8, offset: i16 },
    Bnel { rs: u8, rt: u8, offset: i16 },
    Blez { rs: u8, offset: i16 },
    Bgtz { rs: u8, offset: i16 },
    Blezl { rs: u8, offset: i16 },
    Bgtzl { rs: u8, offset: i16 },
    Bltz { rs: u8, offset: i16 },
    Bgez { rs: u8, offset: i16 },
    Bltzl { rs: u8, offset: i16 },
    Bgezl { rs: u8, offset: i16 },
    Bc1f { offset: i16 },
    Bc1t { offset: i16 },
    Bc1fl { offset: i16 },
    Bc1tl { offset: i16 },
    J { target: u32 },
    Jal { target: u32 },
    Jalr { rs: u8, rd: u8 },
    Jr { rs: u8 },
}

#[derive(Debug, Clone, Copy)]
enum TraceStep {
    Op {
        phys: u32,
        op: Op,
    },
    Branch {
        phys: u32,
        branch: BranchTerminator,
        delay_op: Op,
        continue_fallthrough: bool,
    },
}

impl TraceStep {
    fn phys(self) -> u32 {
        match self {
            TraceStep::Op { phys, .. } | TraceStep::Branch { phys, .. } => phys,
        }
    }
}

fn decode_branch(raw: u32) -> Option<BranchTerminator> {
    let opcode = (raw >> 26) as u8;
    let rs = ((raw >> 21) & 0x1F) as u8;
    let rt = ((raw >> 16) & 0x1F) as u8;
    let funct = (raw & 0x3F) as u8;
    let offset = raw as u16 as i16;
    let target = raw & 0x03FF_FFFF;
    match opcode {
        0x02 => Some(BranchTerminator::J { target }),
        0x03 => Some(BranchTerminator::Jal { target }),
        0x04 => Some(BranchTerminator::Beq { rs, rt, offset }),
        0x05 => Some(BranchTerminator::Bne { rs, rt, offset }),
        0x06 => Some(BranchTerminator::Blez { rs, offset }),
        0x07 => Some(BranchTerminator::Bgtz { rs, offset }),
        0x14 => Some(BranchTerminator::Beql { rs, rt, offset }),
        0x15 => Some(BranchTerminator::Bnel { rs, rt, offset }),
        0x16 => Some(BranchTerminator::Blezl { rs, offset }),
        0x17 => Some(BranchTerminator::Bgtzl { rs, offset }),
        0x01 => match rt {
            0x00 => Some(BranchTerminator::Bltz { rs, offset }),
            0x01 => Some(BranchTerminator::Bgez { rs, offset }),
            0x02 => Some(BranchTerminator::Bltzl { rs, offset }),
            0x03 => Some(BranchTerminator::Bgezl { rs, offset }),
            _ => None,
        },
        0x11 if rs == 0x08 => match rt {
            0x00 => Some(BranchTerminator::Bc1f { offset }),
            0x01 => Some(BranchTerminator::Bc1t { offset }),
            0x02 => Some(BranchTerminator::Bc1fl { offset }),
            0x03 => Some(BranchTerminator::Bc1tl { offset }),
            _ => None,
        },
        0x00 if funct == 0x08 => Some(BranchTerminator::Jr { rs }),
        0x00 if funct == 0x09 => Some(BranchTerminator::Jalr {
            rs,
            rd: ((raw >> 11) & 0x1F) as u8,
        }),
        _ => None,
    }
}

fn jump_target_phys(from_phys: u32, target: u32) -> u32 {
    from_phys.wrapping_add(4) & 0xF000_0000 | (target << 2)
}

fn branch_target_phys(phys: u32, branch: BranchTerminator) -> Option<u32> {
    match branch {
        BranchTerminator::Beq { offset, .. }
        | BranchTerminator::Bne { offset, .. }
        | BranchTerminator::Beql { offset, .. }
        | BranchTerminator::Bnel { offset, .. }
        | BranchTerminator::Blez { offset, .. }
        | BranchTerminator::Bgtz { offset, .. }
        | BranchTerminator::Blezl { offset, .. }
        | BranchTerminator::Bgtzl { offset, .. }
        | BranchTerminator::Bltz { offset, .. }
        | BranchTerminator::Bgez { offset, .. }
        | BranchTerminator::Bltzl { offset, .. }
        | BranchTerminator::Bgezl { offset, .. }
        | BranchTerminator::Bc1f { offset }
        | BranchTerminator::Bc1t { offset }
        | BranchTerminator::Bc1fl { offset }
        | BranchTerminator::Bc1tl { offset } => Some(
            phys.wrapping_add(4)
                .wrapping_add(((offset as i32) << 2) as u32),
        ),
        BranchTerminator::J { target } | BranchTerminator::Jal { target } => {
            Some(jump_target_phys(phys, target))
        }
        BranchTerminator::Jr { .. } | BranchTerminator::Jalr { .. } => None,
    }
}

fn decode_supported_non_branch(raw: u32) -> Option<Op> {
    let opcode = (raw >> 26) as u8;
    let rs = ((raw >> 21) & 0x1F) as u8;
    let rt = ((raw >> 16) & 0x1F) as u8;
    let rd = ((raw >> 11) & 0x1F) as u8;
    let sa = ((raw >> 6) & 0x1F) as u8;
    let funct = (raw & 0x3F) as u8;
    let imm_u16 = raw as u16;
    let imm_i16 = imm_u16 as i16;

    match opcode {
        0x08 => Some(Op::Addi {
            rs,
            rt,
            imm: imm_i16,
        }),
        0x09 => Some(Op::Addiu {
            rs,
            rt,
            imm: imm_i16,
        }),
        0x0A => Some(Op::Slti {
            rs,
            rt,
            imm: imm_i16,
        }),
        0x0B => Some(Op::Sltiu {
            rs,
            rt,
            imm: imm_i16,
        }),
        0x0C => Some(Op::Andi {
            rs,
            rt,
            imm: imm_u16,
        }),
        0x0D => Some(Op::Ori {
            rs,
            rt,
            imm: imm_u16,
        }),
        0x0E => Some(Op::Xori {
            rs,
            rt,
            imm: imm_u16,
        }),
        0x0F => Some(Op::Lui { rt, imm: imm_i16 }),
        0x18 | 0x19 => Some(Op::Daddiu {
            rs,
            rt,
            imm: imm_i16,
        }),
        0x20 => Some(Op::Lb {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x21 => Some(Op::Lh {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x23 => Some(Op::Lw {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x24 => Some(Op::Lbu {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x25 => Some(Op::Lhu {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x27 => Some(Op::Lwu {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x28 => Some(Op::Sb {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x29 => Some(Op::Sh {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x2B => Some(Op::Sw {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x37 => Some(Op::Ld {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x3F => Some(Op::Sd {
            base: rs,
            rt,
            imm: imm_i16,
        }),
        0x10 => match rs {
            0x00 => Some(Op::Mfc0 { rt, rd }),
            0x01 => Some(Op::Dmfc0 { rt, rd }),
            0x04 => Some(Op::Mtc0 { rt, rd }),
            0x05 => Some(Op::Dmtc0 { rt, rd }),
            _ => None,
        },
        0x00 => match funct {
            0x00 => Some(Op::Sll { rt, rd, sa }),
            0x02 => Some(Op::Srl { rt, rd, sa }),
            0x03 => Some(Op::Sra { rt, rd, sa }),
            0x04 => Some(Op::Sllv { rs, rt, rd }),
            0x06 => Some(Op::Srlv { rs, rt, rd }),
            0x07 => Some(Op::Srav { rs, rt, rd }),
            0x0F => Some(Op::Sync),
            0x10 => Some(Op::Mfhi { rd }),
            0x11 => Some(Op::Mthi { rs }),
            0x12 => Some(Op::Mflo { rd }),
            0x13 => Some(Op::Mtlo { rs }),
            0x18 => Some(Op::Mult { rs, rt }),
            0x19 => Some(Op::Multu { rs, rt }),
            0x1A => Some(Op::Div { rs, rt }),
            0x1B => Some(Op::Divu { rs, rt }),
            0x1C => Some(Op::Dmult { rs, rt }),
            0x1D => Some(Op::Dmultu { rs, rt }),
            0x1E => Some(Op::Ddiv { rs, rt }),
            0x1F => Some(Op::Ddivu { rs, rt }),
            0x21 => Some(Op::Addu { rs, rt, rd }),
            0x23 => Some(Op::Subu { rs, rt, rd }),
            0x24 => Some(Op::And { rs, rt, rd }),
            0x25 => Some(Op::Or { rs, rt, rd }),
            0x26 => Some(Op::Xor { rs, rt, rd }),
            0x27 => Some(Op::Nor { rs, rt, rd }),
            0x2A => Some(Op::Slt { rs, rt, rd }),
            0x2B => Some(Op::Sltu { rs, rt, rd }),
            0x2D => Some(Op::Daddu { rs, rt, rd }),
            0x2F => Some(Op::Dsubu { rs, rt, rd }),
            _ => None,
        },
        0x11 => match rs {
            0x00 => Some(Op::Mfc1 { rt, fs: rd }),
            0x04 => Some(Op::Mtc1 { rt, fs: rd }),
            0x10 => match funct {
                0x00 => Some(Op::AddS {
                    fd: sa,
                    fs: rd,
                    ft: rt,
                }),
                0x01 => Some(Op::SubS {
                    fd: sa,
                    fs: rd,
                    ft: rt,
                }),
                0x02 => Some(Op::MulS {
                    fd: sa,
                    fs: rd,
                    ft: rt,
                }),
                0x03 => Some(Op::DivS {
                    fd: sa,
                    fs: rd,
                    ft: rt,
                }),
                _ => Some(Op::Interp { raw }),
            },
            0x11 => match funct {
                0x00 => Some(Op::AddD {
                    fd: sa,
                    fs: rd,
                    ft: rt,
                }),
                0x01 => Some(Op::SubD {
                    fd: sa,
                    fs: rd,
                    ft: rt,
                }),
                0x02 => Some(Op::MulD {
                    fd: sa,
                    fs: rd,
                    ft: rt,
                }),
                0x03 => Some(Op::DivD {
                    fd: sa,
                    fs: rd,
                    ft: rt,
                }),
                _ => Some(Op::Interp { raw }),
            },
            0x08 => None,
            _ => Some(Op::Interp { raw }),
        },
        0x31 => Some(Op::Lwc1 {
            base: rs,
            ft: rt,
            imm: imm_i16,
        }),
        0x35 => Some(Op::Ldc1 {
            base: rs,
            ft: rt,
            imm: imm_i16,
        }),
        0x39 => Some(Op::Swc1 {
            base: rs,
            ft: rt,
            imm: imm_i16,
        }),
        0x3D => Some(Op::Sdc1 {
            base: rs,
            ft: rt,
            imm: imm_i16,
        }),
        0x2F => Some(Op::Cache),
        0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 | 0x14 | 0x15 | 0x16 | 0x17 => None,
        _ => None,
    }
}

fn can_inline_interp_non_branch(raw: u32) -> bool {
    let opcode = (raw >> 26) as u8;
    let rs = ((raw >> 21) & 0x1F) as u8;
    let funct = (raw & 0x3F) as u8;
    match opcode {
        // All branch/jump forms must be explicit terminators.
        0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 | 0x14 | 0x15 | 0x16 | 0x17 => false,
        // COP0 branch-like/system returns must terminate.
        0x10 if rs == 0x10 && funct == 0x18 => false, // eret
        // COP1 branch format must terminate.
        0x11 if rs == 0x08 => false,
        // Special control-flow and explicit exception traps should terminate.
        0x00 if matches!(funct, 0x08 | 0x09 | 0x0C | 0x0D | 0x30..=0x37) => false,
        _ => true,
    }
}

fn op_may_write_interrupt_state(op: Op) -> bool {
    match op {
        Op::Mtc0 { rd, .. } | Op::Dmtc0 { rd, .. } => matches!(rd, 12 | 13),
        Op::Interp { .. } => true,
        _ => false,
    }
}

fn iconst_u64(builder: &mut FunctionBuilder<'_>, value: u64) -> Value {
    builder
        .ins()
        .iconst(types::I64, i64::from_ne_bytes(value.to_ne_bytes()))
}

const GPR_COUNT: usize = 32;

fn gpr_var(reg: u8) -> Variable {
    Variable::from_u32(u32::from(reg))
}

fn load_gpr(builder: &mut FunctionBuilder<'_>, reg: u8) -> Value {
    builder.use_var(gpr_var(reg))
}

fn store_gpr(builder: &mut FunctionBuilder<'_>, reg: u8, value: Value) {
    if reg != 0 {
        builder.def_var(gpr_var(reg), value);
    }
}

fn init_gpr_vars(builder: &mut FunctionBuilder<'_>, gpr_ptr: Value, flags: MemFlags) {
    for _ in 0..GPR_COUNT {
        let _ = builder.declare_var(types::I64);
    }
    let zero = builder.ins().iconst(types::I64, 0);
    builder.def_var(gpr_var(0), zero);
    for reg in 1..GPR_COUNT {
        let value = builder
            .ins()
            .load(types::I64, flags, gpr_ptr, (reg as i32) * 8);
        builder.def_var(gpr_var(reg as u8), value);
    }
}

fn spill_gpr_vars(builder: &mut FunctionBuilder<'_>, gpr_ptr: Value, flags: MemFlags) {
    for reg in 1..GPR_COUNT {
        let value = builder.use_var(gpr_var(reg as u8));
        builder.ins().store(flags, value, gpr_ptr, (reg as i32) * 8);
    }
}

fn reload_gpr_vars(builder: &mut FunctionBuilder<'_>, gpr_ptr: Value, flags: MemFlags) {
    let zero = builder.ins().iconst(types::I64, 0);
    builder.def_var(gpr_var(0), zero);
    for reg in 1..GPR_COUNT {
        let value = builder
            .ins()
            .load(types::I64, flags, gpr_ptr, (reg as i32) * 8);
        builder.def_var(gpr_var(reg as u8), value);
    }
}

fn load_fpr_word(
    builder: &mut FunctionBuilder<'_>,
    fpr_ptr: Value,
    reg: u8,
    flags: MemFlags,
) -> Value {
    let raw64 = builder
        .ins()
        .load(types::I64, flags, fpr_ptr, i32::from(reg) * 8);
    builder.ins().ireduce(types::I32, raw64)
}

fn store_fpr_word(
    builder: &mut FunctionBuilder<'_>,
    fpr_ptr: Value,
    reg: u8,
    value32: Value,
    flags: MemFlags,
) {
    let value64 = builder.ins().uextend(types::I64, value32);
    builder
        .ins()
        .store(flags, value64, fpr_ptr, i32::from(reg) * 8);
}

fn load_fpr_double_bits(
    builder: &mut FunctionBuilder<'_>,
    fpr_ptr: Value,
    reg: u8,
    flags: MemFlags,
) -> Value {
    let low32 = load_fpr_word(builder, fpr_ptr, reg, flags);
    let high32 = load_fpr_word(builder, fpr_ptr, reg | 1, flags);
    let low64 = builder.ins().uextend(types::I64, low32);
    let high64 = builder.ins().uextend(types::I64, high32);
    let sh = builder.ins().iconst(types::I64, 32);
    let high64 = builder.ins().ishl(high64, sh);
    builder.ins().bor(high64, low64)
}

fn store_fpr_double_bits(
    builder: &mut FunctionBuilder<'_>,
    fpr_ptr: Value,
    reg: u8,
    bits64: Value,
    flags: MemFlags,
) {
    let low32 = builder.ins().ireduce(types::I32, bits64);
    let sh = builder.ins().iconst(types::I64, 32);
    let high64 = builder.ins().ushr(bits64, sh);
    let high32 = builder.ins().ireduce(types::I32, high64);
    store_fpr_word(builder, fpr_ptr, reg, low32, flags);
    store_fpr_word(builder, fpr_ptr, reg | 1, high32, flags);
}

#[derive(Clone, Copy)]
struct ImportedFuncRefs {
    load_u8: FuncRef,
    load_u16: FuncRef,
    load_u32: FuncRef,
    load_u64: FuncRef,
    store_u8: FuncRef,
    store_u16: FuncRef,
    store_u32: FuncRef,
    store_u64: FuncRef,
    store_invalidate: FuncRef,
    cop0_read: FuncRef,
    cop0_write: FuncRef,
    cop1_condition: FuncRef,
    interp_exec: FuncRef,
    hi_read: FuncRef,
    hi_write: FuncRef,
    lo_read: FuncRef,
    lo_write: FuncRef,
}

#[derive(Clone, Copy)]
struct FastmemValues {
    ptr_ty: Type,
    base: Value,
    phys_limit: Value,
    phys_mask: Value,
}

#[derive(Clone, Copy)]
struct RuntimeStatePtrs {
    ptr_ty: Type,
    hi_ptr: Value,
    lo_ptr: Value,
    cop1_fcr31_ptr: Value,
}

fn i64_to_ptr_sized(builder: &mut FunctionBuilder<'_>, ptr_ty: Type, value: Value) -> Value {
    if ptr_ty == types::I64 {
        value
    } else {
        builder.ins().ireduce(ptr_ty, value)
    }
}

fn fastmem_guard_and_addr(
    builder: &mut FunctionBuilder<'_>,
    fastmem: FastmemValues,
    vaddr: Value,
) -> (Value, Value, Value) {
    let vaddr32 = builder.ins().ireduce(types::I32, vaddr);
    let seg_mask = builder
        .ins()
        .iconst(types::I32, i64::from(0xC000_0000u32 as i32));
    let seg = builder.ins().band(vaddr32, seg_mask);
    let direct = builder
        .ins()
        .iconst(types::I32, i64::from(0x8000_0000u32 as i32));
    let is_direct = builder.ins().icmp(IntCC::Equal, seg, direct);

    let phys_mask_const = iconst_u64(builder, 0x1FFF_FFFF);
    let phys = builder.ins().band(vaddr, phys_mask_const);
    let in_range = builder
        .ins()
        .icmp(IntCC::UnsignedLessThan, phys, fastmem.phys_limit);
    let mut fast_cond = is_direct;
    fast_cond = builder.ins().band(fast_cond, in_range);

    let masked_phys = builder.ins().band(phys, fastmem.phys_mask);
    let offset = i64_to_ptr_sized(builder, fastmem.ptr_ty, masked_phys);
    let addr = builder.ins().iadd(fastmem.base, offset);
    (fast_cond, addr, masked_phys)
}

fn load_be_from_fastmem(
    builder: &mut FunctionBuilder<'_>,
    addr: Value,
    width: u8,
    flags: MemFlags,
) -> Value {
    match width {
        1 => {
            let raw = builder.ins().load(types::I8, flags, addr, 0);
            builder.ins().uextend(types::I64, raw)
        }
        2 => {
            let raw = builder.ins().load(types::I16, flags, addr, 0);
            let be = builder.ins().bswap(raw);
            builder.ins().uextend(types::I64, be)
        }
        4 => {
            let raw = builder.ins().load(types::I32, flags, addr, 0);
            let be = builder.ins().bswap(raw);
            builder.ins().uextend(types::I64, be)
        }
        8 => {
            let raw = builder.ins().load(types::I64, flags, addr, 0);
            builder.ins().bswap(raw)
        }
        _ => unreachable!("unsupported fastmem load width"),
    }
}

fn store_be_to_fastmem(
    builder: &mut FunctionBuilder<'_>,
    addr: Value,
    value64: Value,
    width: u8,
    flags: MemFlags,
) {
    match width {
        1 => {
            let v = builder.ins().ireduce(types::I8, value64);
            builder.ins().store(flags, v, addr, 0);
        }
        2 => {
            let v = builder.ins().ireduce(types::I16, value64);
            let be = builder.ins().bswap(v);
            builder.ins().store(flags, be, addr, 0);
        }
        4 => {
            let v = builder.ins().ireduce(types::I32, value64);
            let be = builder.ins().bswap(v);
            builder.ins().store(flags, be, addr, 0);
        }
        8 => {
            let be = builder.ins().bswap(value64);
            builder.ins().store(flags, be, addr, 0);
        }
        _ => unreachable!("unsupported fastmem store width"),
    }
}

fn emit_load_via_fastmem(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helper: FuncRef,
    fastmem: FastmemValues,
    vaddr: Value,
    width: u8,
) -> Value {
    let (fast_cond, fast_addr, _fast_phys) = fastmem_guard_and_addr(builder, fastmem, vaddr);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(done_block, types::I64);
    builder
        .ins()
        .brif(fast_cond, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let mut fast_flags = MemFlags::new();
    fast_flags.set_notrap();
    let fast_val = load_be_from_fastmem(builder, fast_addr, width, fast_flags);
    let args = [fast_val.into()];
    builder.ins().jump(done_block, &args);

    builder.switch_to_block(slow_block);
    let call = builder.ins().call(helper, &[callbacks_ptr, vaddr]);
    let slow_val = builder.inst_results(call)[0];
    let args = [slow_val.into()];
    builder.ins().jump(done_block, &args);

    builder.switch_to_block(done_block);
    builder.block_params(done_block)[0]
}

fn emit_store_via_fastmem(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helper: FuncRef,
    invalidate_helper: FuncRef,
    fastmem: FastmemValues,
    vaddr: Value,
    value64: Value,
    width: u8,
) {
    let (fast_cond, fast_addr, fast_phys) = fastmem_guard_and_addr(builder, fastmem, vaddr);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let done_block = builder.create_block();
    builder
        .ins()
        .brif(fast_cond, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let mut fast_flags = MemFlags::new();
    fast_flags.set_notrap();
    store_be_to_fastmem(builder, fast_addr, value64, width, fast_flags);
    let width_u64 = iconst_u64(builder, width as u64);
    builder
        .ins()
        .call(invalidate_helper, &[callbacks_ptr, fast_phys, width_u64]);
    builder.ins().jump(done_block, &[]);

    builder.switch_to_block(slow_block);
    builder.ins().call(helper, &[callbacks_ptr, vaddr, value64]);
    builder.ins().jump(done_block, &[]);

    builder.switch_to_block(done_block);
}

fn read_u64_ptr_or_call(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    ptr_ty: Type,
    ptr: Value,
    helper: FuncRef,
) -> Value {
    let zero = builder.ins().iconst(ptr_ty, 0);
    let has_ptr = builder.ins().icmp(IntCC::NotEqual, ptr, zero);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(done_block, types::I64);
    builder
        .ins()
        .brif(has_ptr, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let mut flags = MemFlags::new();
    flags.set_notrap();
    flags.set_aligned();
    let loaded = builder.ins().load(types::I64, flags, ptr, 0);
    let args = [loaded.into()];
    builder.ins().jump(done_block, &args);

    builder.switch_to_block(slow_block);
    let call = builder.ins().call(helper, &[callbacks_ptr]);
    let slow = builder.inst_results(call)[0];
    let args = [slow.into()];
    builder.ins().jump(done_block, &args);

    builder.switch_to_block(done_block);
    builder.block_params(done_block)[0]
}

fn write_u64_ptr_or_call(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    ptr_ty: Type,
    ptr: Value,
    helper: FuncRef,
    value: Value,
) {
    let zero = builder.ins().iconst(ptr_ty, 0);
    let has_ptr = builder.ins().icmp(IntCC::NotEqual, ptr, zero);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let done_block = builder.create_block();
    builder
        .ins()
        .brif(has_ptr, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let mut flags = MemFlags::new();
    flags.set_notrap();
    flags.set_aligned();
    builder.ins().store(flags, value, ptr, 0);
    builder.ins().jump(done_block, &[]);

    builder.switch_to_block(slow_block);
    builder.ins().call(helper, &[callbacks_ptr, value]);
    builder.ins().jump(done_block, &[]);

    builder.switch_to_block(done_block);
}

fn read_hi(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    runtime_ptrs: RuntimeStatePtrs,
) -> Value {
    read_u64_ptr_or_call(
        builder,
        callbacks_ptr,
        runtime_ptrs.ptr_ty,
        runtime_ptrs.hi_ptr,
        helpers.hi_read,
    )
}

fn read_lo(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    runtime_ptrs: RuntimeStatePtrs,
) -> Value {
    read_u64_ptr_or_call(
        builder,
        callbacks_ptr,
        runtime_ptrs.ptr_ty,
        runtime_ptrs.lo_ptr,
        helpers.lo_read,
    )
}

fn write_hi(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    runtime_ptrs: RuntimeStatePtrs,
    value: Value,
) {
    write_u64_ptr_or_call(
        builder,
        callbacks_ptr,
        runtime_ptrs.ptr_ty,
        runtime_ptrs.hi_ptr,
        helpers.hi_write,
        value,
    );
}

fn write_lo(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    runtime_ptrs: RuntimeStatePtrs,
    value: Value,
) {
    write_u64_ptr_or_call(
        builder,
        callbacks_ptr,
        runtime_ptrs.ptr_ty,
        runtime_ptrs.lo_ptr,
        helpers.lo_write,
        value,
    );
}

fn read_cop1_condition_flag(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    runtime_ptrs: RuntimeStatePtrs,
) -> Value {
    let zero_ptr = builder.ins().iconst(runtime_ptrs.ptr_ty, 0);
    let has_ptr = builder
        .ins()
        .icmp(IntCC::NotEqual, runtime_ptrs.cop1_fcr31_ptr, zero_ptr);

    let fast_block = builder.create_block();
    let slow_block = builder.create_block();
    let done_block = builder.create_block();
    builder.append_block_param(done_block, types::I64);
    builder
        .ins()
        .brif(has_ptr, fast_block, &[], slow_block, &[]);

    builder.switch_to_block(fast_block);
    let mut flags = MemFlags::new();
    flags.set_notrap();
    flags.set_aligned();
    let fcr31 = builder
        .ins()
        .load(types::I32, flags, runtime_ptrs.cop1_fcr31_ptr, 0);
    let cond_mask = builder.ins().iconst(types::I32, i64::from(1 << 23));
    let cond_bits = builder.ins().band(fcr31, cond_mask);
    let zero_i32 = builder.ins().iconst(types::I32, 0);
    let cond = builder.ins().icmp(IntCC::NotEqual, cond_bits, zero_i32);
    let cond64 = builder.ins().uextend(types::I64, cond);
    let args = [cond64.into()];
    builder.ins().jump(done_block, &args);

    builder.switch_to_block(slow_block);
    let call = builder.ins().call(helpers.cop1_condition, &[callbacks_ptr]);
    let slow = builder.inst_results(call)[0];
    let args = [slow.into()];
    builder.ins().jump(done_block, &args);

    builder.switch_to_block(done_block);
    builder.block_params(done_block)[0]
}

fn emit_op(
    builder: &mut FunctionBuilder<'_>,
    gpr_ptr: Value,
    fpr_ptr: Value,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    runtime_ptrs: RuntimeStatePtrs,
    fastmem: FastmemValues,
    flags: MemFlags,
    current_pc: Value,
    op: Op,
) {
    match op {
        Op::Addi { rs, rt, imm } | Op::Addiu { rs, rt, imm } => {
            let rs64 = load_gpr(builder, rs);
            let rs32 = builder.ins().ireduce(types::I32, rs64);
            let imm32 = builder.ins().iconst(types::I32, i64::from(imm));
            let sum32 = builder.ins().iadd(rs32, imm32);
            let sum64 = builder.ins().sextend(types::I64, sum32);
            store_gpr(builder, rt, sum64);
        }
        Op::Daddiu { rs, rt, imm } => {
            let rs64 = load_gpr(builder, rs);
            let imm64 = iconst_u64(builder, imm as i64 as u64);
            let sum64 = builder.ins().iadd(rs64, imm64);
            store_gpr(builder, rt, sum64);
        }
        Op::Slti { rs, rt, imm } => {
            let rs64 = load_gpr(builder, rs);
            let imm64 = builder.ins().iconst(types::I64, i64::from(imm));
            let cmp = builder.ins().icmp(IntCC::SignedLessThan, rs64, imm64);
            let result = builder.ins().uextend(types::I64, cmp);
            store_gpr(builder, rt, result);
        }
        Op::Sltiu { rs, rt, imm } => {
            let rs64 = load_gpr(builder, rs);
            let imm64 = iconst_u64(builder, imm as i64 as u64);
            let cmp = builder.ins().icmp(IntCC::UnsignedLessThan, rs64, imm64);
            let result = builder.ins().uextend(types::I64, cmp);
            store_gpr(builder, rt, result);
        }
        Op::Andi { rs, rt, imm } => {
            let rs64 = load_gpr(builder, rs);
            let imm64 = iconst_u64(builder, u64::from(imm));
            let result = builder.ins().band(rs64, imm64);
            store_gpr(builder, rt, result);
        }
        Op::Ori { rs, rt, imm } => {
            let rs64 = load_gpr(builder, rs);
            let imm64 = iconst_u64(builder, u64::from(imm));
            let result = builder.ins().bor(rs64, imm64);
            store_gpr(builder, rt, result);
        }
        Op::Xori { rs, rt, imm } => {
            let rs64 = load_gpr(builder, rs);
            let imm64 = iconst_u64(builder, u64::from(imm));
            let result = builder.ins().bxor(rs64, imm64);
            store_gpr(builder, rt, result);
        }
        Op::Lui { rt, imm } => {
            let val = ((imm as i32) as i64) << 16;
            let value = builder.ins().iconst(types::I64, val);
            store_gpr(builder, rt, value);
        }
        Op::Addu { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let lhs32 = builder.ins().ireduce(types::I32, lhs);
            let rhs32 = builder.ins().ireduce(types::I32, rhs);
            let sum32 = builder.ins().iadd(lhs32, rhs32);
            let result = builder.ins().sextend(types::I64, sum32);
            store_gpr(builder, rd, result);
        }
        Op::Subu { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let lhs32 = builder.ins().ireduce(types::I32, lhs);
            let rhs32 = builder.ins().ireduce(types::I32, rhs);
            let diff32 = builder.ins().isub(lhs32, rhs32);
            let result = builder.ins().sextend(types::I64, diff32);
            store_gpr(builder, rd, result);
        }
        Op::And { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let result = builder.ins().band(lhs, rhs);
            store_gpr(builder, rd, result);
        }
        Op::Or { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let result = builder.ins().bor(lhs, rhs);
            store_gpr(builder, rd, result);
        }
        Op::Xor { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let result = builder.ins().bxor(lhs, rhs);
            store_gpr(builder, rd, result);
        }
        Op::Nor { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let or_val = builder.ins().bor(lhs, rhs);
            let result = builder.ins().bnot(or_val);
            store_gpr(builder, rd, result);
        }
        Op::Slt { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let cmp = builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs);
            let result = builder.ins().uextend(types::I64, cmp);
            store_gpr(builder, rd, result);
        }
        Op::Sltu { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let cmp = builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs);
            let result = builder.ins().uextend(types::I64, cmp);
            store_gpr(builder, rd, result);
        }
        Op::Daddu { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let result = builder.ins().iadd(lhs, rhs);
            store_gpr(builder, rd, result);
        }
        Op::Dsubu { rs, rt, rd } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let result = builder.ins().isub(lhs, rhs);
            store_gpr(builder, rd, result);
        }
        Op::Sll { rt, rd, sa } => {
            let rt64 = load_gpr(builder, rt);
            let rt32 = builder.ins().ireduce(types::I32, rt64);
            let sh = builder.ins().iconst(types::I32, i64::from(sa));
            let result32 = builder.ins().ishl(rt32, sh);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, rd, result64);
        }
        Op::Srl { rt, rd, sa } => {
            let rt64 = load_gpr(builder, rt);
            let rt32 = builder.ins().ireduce(types::I32, rt64);
            let sh = builder.ins().iconst(types::I32, i64::from(sa));
            let result32 = builder.ins().ushr(rt32, sh);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, rd, result64);
        }
        Op::Sra { rt, rd, sa } => {
            let rt64 = load_gpr(builder, rt);
            let sh = builder.ins().iconst(types::I64, i64::from(sa));
            let shifted64 = builder.ins().ushr(rt64, sh);
            let result32 = builder.ins().ireduce(types::I32, shifted64);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, rd, result64);
        }
        Op::Sllv { rs, rt, rd } => {
            let rs = load_gpr(builder, rs);
            let sh_mask = iconst_u64(builder, 0x1F);
            let sh_masked = builder.ins().band(rs, sh_mask);
            let sh = builder.ins().ireduce(types::I32, sh_masked);
            let rt64 = load_gpr(builder, rt);
            let rt32 = builder.ins().ireduce(types::I32, rt64);
            let result32 = builder.ins().ishl(rt32, sh);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, rd, result64);
        }
        Op::Srlv { rs, rt, rd } => {
            let rs = load_gpr(builder, rs);
            let sh_mask = iconst_u64(builder, 0x1F);
            let sh_masked = builder.ins().band(rs, sh_mask);
            let sh = builder.ins().ireduce(types::I32, sh_masked);
            let rt64 = load_gpr(builder, rt);
            let rt32 = builder.ins().ireduce(types::I32, rt64);
            let result32 = builder.ins().ushr(rt32, sh);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, rd, result64);
        }
        Op::Srav { rs, rt, rd } => {
            let rs = load_gpr(builder, rs);
            let sh_mask = iconst_u64(builder, 0x1F);
            let sh = builder.ins().band(rs, sh_mask);
            let rt64 = load_gpr(builder, rt);
            let shifted64 = builder.ins().ushr(rt64, sh);
            let result32 = builder.ins().ireduce(types::I32, shifted64);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, rd, result64);
        }
        Op::Mfhi { rd } => {
            let value = read_hi(builder, callbacks_ptr, helpers, runtime_ptrs);
            store_gpr(builder, rd, value);
        }
        Op::Mthi { rs } => {
            let value = load_gpr(builder, rs);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, value);
        }
        Op::Mflo { rd } => {
            let value = read_lo(builder, callbacks_ptr, helpers, runtime_ptrs);
            store_gpr(builder, rd, value);
        }
        Op::Mtlo { rs } => {
            let value = load_gpr(builder, rs);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, value);
        }
        Op::Mult { rs, rt } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let lhs32 = builder.ins().ireduce(types::I32, lhs);
            let rhs32 = builder.ins().ireduce(types::I32, rhs);
            let lhs64 = builder.ins().sextend(types::I64, lhs32);
            let rhs64 = builder.ins().sextend(types::I64, rhs32);
            let product = builder.ins().imul(lhs64, rhs64);
            let lo32 = builder.ins().ireduce(types::I32, product);
            let lo64 = builder.ins().sextend(types::I64, lo32);
            let sh = builder.ins().iconst(types::I64, 32);
            let hi_shifted = builder.ins().sshr(product, sh);
            let hi32 = builder.ins().ireduce(types::I32, hi_shifted);
            let hi64 = builder.ins().sextend(types::I64, hi32);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, lo64);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, hi64);
        }
        Op::Multu { rs, rt } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let lhs32 = builder.ins().ireduce(types::I32, lhs);
            let rhs32 = builder.ins().ireduce(types::I32, rhs);
            let lhs64 = builder.ins().uextend(types::I64, lhs32);
            let rhs64 = builder.ins().uextend(types::I64, rhs32);
            let product = builder.ins().imul(lhs64, rhs64);
            let lo32 = builder.ins().ireduce(types::I32, product);
            let lo64 = builder.ins().sextend(types::I64, lo32);
            let sh = builder.ins().iconst(types::I64, 32);
            let hi_shifted = builder.ins().ushr(product, sh);
            let hi32 = builder.ins().ireduce(types::I32, hi_shifted);
            let hi64 = builder.ins().sextend(types::I64, hi32);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, lo64);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, hi64);
        }
        Op::Div { rs, rt } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let lhs32 = builder.ins().ireduce(types::I32, lhs);
            let rhs32 = builder.ins().ireduce(types::I32, rhs);
            let zero = builder.ins().iconst(types::I32, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, rhs32, zero);
            let min_i32 = builder.ins().iconst(types::I32, i64::from(i32::MIN));
            let neg_one = builder.ins().iconst(types::I32, -1);
            let is_min = builder.ins().icmp(IntCC::Equal, lhs32, min_i32);
            let is_neg_one = builder.ins().icmp(IntCC::Equal, rhs32, neg_one);
            let is_overflow = builder.ins().band(is_min, is_neg_one);
            let skip = builder.ins().bor(is_zero, is_overflow);
            let one = builder.ins().iconst(types::I32, 1);
            let safe_rhs32 = builder.ins().select(skip, one, rhs32);
            let quot32 = builder.ins().sdiv(lhs32, safe_rhs32);
            let rem32 = builder.ins().srem(lhs32, safe_rhs32);
            let quot64 = builder.ins().sextend(types::I64, quot32);
            let rem64 = builder.ins().sextend(types::I64, rem32);
            let old_lo = read_lo(builder, callbacks_ptr, helpers, runtime_ptrs);
            let old_hi = read_hi(builder, callbacks_ptr, helpers, runtime_ptrs);
            let lo = builder.ins().select(skip, old_lo, quot64);
            let hi = builder.ins().select(skip, old_hi, rem64);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, lo);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, hi);
        }
        Op::Divu { rs, rt } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let lhs32 = builder.ins().ireduce(types::I32, lhs);
            let rhs32 = builder.ins().ireduce(types::I32, rhs);
            let zero = builder.ins().iconst(types::I32, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, rhs32, zero);
            let one = builder.ins().iconst(types::I32, 1);
            let safe_rhs32 = builder.ins().select(is_zero, one, rhs32);
            let quot32 = builder.ins().udiv(lhs32, safe_rhs32);
            let rem32 = builder.ins().urem(lhs32, safe_rhs32);
            let quot64 = builder.ins().sextend(types::I64, quot32);
            let rem64 = builder.ins().sextend(types::I64, rem32);
            let old_lo = read_lo(builder, callbacks_ptr, helpers, runtime_ptrs);
            let old_hi = read_hi(builder, callbacks_ptr, helpers, runtime_ptrs);
            let lo = builder.ins().select(is_zero, old_lo, quot64);
            let hi = builder.ins().select(is_zero, old_hi, rem64);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, lo);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, hi);
        }
        Op::Dmult { rs, rt } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let lo = builder.ins().imul(lhs, rhs);
            let hi = builder.ins().smulhi(lhs, rhs);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, lo);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, hi);
        }
        Op::Dmultu { rs, rt } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let lo = builder.ins().imul(lhs, rhs);
            let hi = builder.ins().umulhi(lhs, rhs);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, lo);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, hi);
        }
        Op::Ddiv { rs, rt } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let zero = iconst_u64(builder, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, rhs, zero);
            let min_i64 = builder.ins().iconst(types::I64, i64::MIN);
            let neg_one = builder.ins().iconst(types::I64, -1);
            let is_min = builder.ins().icmp(IntCC::Equal, lhs, min_i64);
            let is_neg_one = builder.ins().icmp(IntCC::Equal, rhs, neg_one);
            let is_overflow = builder.ins().band(is_min, is_neg_one);
            let skip = builder.ins().bor(is_zero, is_overflow);
            let one = iconst_u64(builder, 1);
            let safe_rhs = builder.ins().select(skip, one, rhs);
            let quot = builder.ins().sdiv(lhs, safe_rhs);
            let rem = builder.ins().srem(lhs, safe_rhs);
            let old_lo = read_lo(builder, callbacks_ptr, helpers, runtime_ptrs);
            let old_hi = read_hi(builder, callbacks_ptr, helpers, runtime_ptrs);
            let lo = builder.ins().select(skip, old_lo, quot);
            let hi = builder.ins().select(skip, old_hi, rem);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, lo);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, hi);
        }
        Op::Ddivu { rs, rt } => {
            let lhs = load_gpr(builder, rs);
            let rhs = load_gpr(builder, rt);
            let zero = iconst_u64(builder, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, rhs, zero);
            let one = iconst_u64(builder, 1);
            let safe_rhs = builder.ins().select(is_zero, one, rhs);
            let quot = builder.ins().udiv(lhs, safe_rhs);
            let rem = builder.ins().urem(lhs, safe_rhs);
            let old_lo = read_lo(builder, callbacks_ptr, helpers, runtime_ptrs);
            let old_hi = read_hi(builder, callbacks_ptr, helpers, runtime_ptrs);
            let lo = builder.ins().select(is_zero, old_lo, quot);
            let hi = builder.ins().select(is_zero, old_hi, rem);
            write_lo(builder, callbacks_ptr, helpers, runtime_ptrs, lo);
            write_hi(builder, callbacks_ptr, helpers, runtime_ptrs, hi);
        }
        Op::Lb { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u8, fastmem, vaddr, 1);
            let loaded8 = builder.ins().ireduce(types::I8, loaded);
            let loaded64 = builder.ins().sextend(types::I64, loaded8);
            store_gpr(builder, rt, loaded64);
        }
        Op::Lh { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u16, fastmem, vaddr, 2);
            let loaded16 = builder.ins().ireduce(types::I16, loaded);
            let loaded64 = builder.ins().sextend(types::I64, loaded16);
            store_gpr(builder, rt, loaded64);
        }
        Op::Lhu { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u16, fastmem, vaddr, 2);
            let loaded16 = builder.ins().ireduce(types::I16, loaded);
            let loaded64 = builder.ins().uextend(types::I64, loaded16);
            store_gpr(builder, rt, loaded64);
        }
        Op::Lw { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u32, fastmem, vaddr, 4);
            let loaded32 = builder.ins().ireduce(types::I32, loaded);
            let loaded64 = builder.ins().sextend(types::I64, loaded32);
            store_gpr(builder, rt, loaded64);
        }
        Op::Lwu { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u32, fastmem, vaddr, 4);
            let loaded32 = builder.ins().ireduce(types::I32, loaded);
            let loaded64 = builder.ins().uextend(types::I64, loaded32);
            store_gpr(builder, rt, loaded64);
        }
        Op::Ld { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded64 =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u64, fastmem, vaddr, 8);
            store_gpr(builder, rt, loaded64);
        }
        Op::Lbu { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u8, fastmem, vaddr, 1);
            let loaded8 = builder.ins().ireduce(types::I8, loaded);
            let loaded64 = builder.ins().uextend(types::I64, loaded8);
            store_gpr(builder, rt, loaded64);
        }
        Op::Sw { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_gpr(builder, rt);
            let value32 = builder.ins().ireduce(types::I32, value);
            let value64 = builder.ins().uextend(types::I64, value32);
            emit_store_via_fastmem(
                builder,
                callbacks_ptr,
                helpers.store_u32,
                helpers.store_invalidate,
                fastmem,
                vaddr,
                value64,
                4,
            );
        }
        Op::Sb { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_gpr(builder, rt);
            let value8 = builder.ins().ireduce(types::I8, value);
            let value64 = builder.ins().uextend(types::I64, value8);
            emit_store_via_fastmem(
                builder,
                callbacks_ptr,
                helpers.store_u8,
                helpers.store_invalidate,
                fastmem,
                vaddr,
                value64,
                1,
            );
        }
        Op::Sh { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_gpr(builder, rt);
            let value16 = builder.ins().ireduce(types::I16, value);
            let value64 = builder.ins().uextend(types::I64, value16);
            emit_store_via_fastmem(
                builder,
                callbacks_ptr,
                helpers.store_u16,
                helpers.store_invalidate,
                fastmem,
                vaddr,
                value64,
                2,
            );
        }
        Op::Sd { base, rt, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_gpr(builder, rt);
            emit_store_via_fastmem(
                builder,
                callbacks_ptr,
                helpers.store_u64,
                helpers.store_invalidate,
                fastmem,
                vaddr,
                value,
                8,
            );
        }
        Op::Lwc1 { base, ft, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u32, fastmem, vaddr, 4);
            let bits32 = builder.ins().ireduce(types::I32, loaded);
            store_fpr_word(builder, fpr_ptr, ft, bits32, flags);
        }
        Op::Swc1 { base, ft, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let bits32 = load_fpr_word(builder, fpr_ptr, ft, flags);
            let value64 = builder.ins().uextend(types::I64, bits32);
            emit_store_via_fastmem(
                builder,
                callbacks_ptr,
                helpers.store_u32,
                helpers.store_invalidate,
                fastmem,
                vaddr,
                value64,
                4,
            );
        }
        Op::Ldc1 { base, ft, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let loaded =
                emit_load_via_fastmem(builder, callbacks_ptr, helpers.load_u64, fastmem, vaddr, 8);
            store_fpr_double_bits(builder, fpr_ptr, ft, loaded, flags);
        }
        Op::Sdc1 { base, ft, imm } => {
            let base_addr = load_gpr(builder, base);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_fpr_double_bits(builder, fpr_ptr, ft, flags);
            emit_store_via_fastmem(
                builder,
                callbacks_ptr,
                helpers.store_u64,
                helpers.store_invalidate,
                fastmem,
                vaddr,
                value,
                8,
            );
        }
        Op::Mfc1 { rt, fs } => {
            let bits32 = load_fpr_word(builder, fpr_ptr, fs, flags);
            let value64 = builder.ins().sextend(types::I64, bits32);
            store_gpr(builder, rt, value64);
        }
        Op::Mtc1 { rt, fs } => {
            let value64 = load_gpr(builder, rt);
            let bits32 = builder.ins().ireduce(types::I32, value64);
            store_fpr_word(builder, fpr_ptr, fs, bits32, flags);
        }
        Op::AddS { fd, fs, ft } => {
            let fs_bits = load_fpr_word(builder, fpr_ptr, fs, flags);
            let ft_bits = load_fpr_word(builder, fpr_ptr, ft, flags);
            let fs_val = builder.ins().bitcast(types::F32, MemFlags::new(), fs_bits);
            let ft_val = builder.ins().bitcast(types::F32, MemFlags::new(), ft_bits);
            let result = builder.ins().fadd(fs_val, ft_val);
            let result_bits = builder.ins().bitcast(types::I32, MemFlags::new(), result);
            store_fpr_word(builder, fpr_ptr, fd, result_bits, flags);
        }
        Op::SubS { fd, fs, ft } => {
            let fs_bits = load_fpr_word(builder, fpr_ptr, fs, flags);
            let ft_bits = load_fpr_word(builder, fpr_ptr, ft, flags);
            let fs_val = builder.ins().bitcast(types::F32, MemFlags::new(), fs_bits);
            let ft_val = builder.ins().bitcast(types::F32, MemFlags::new(), ft_bits);
            let result = builder.ins().fsub(fs_val, ft_val);
            let result_bits = builder.ins().bitcast(types::I32, MemFlags::new(), result);
            store_fpr_word(builder, fpr_ptr, fd, result_bits, flags);
        }
        Op::MulS { fd, fs, ft } => {
            let fs_bits = load_fpr_word(builder, fpr_ptr, fs, flags);
            let ft_bits = load_fpr_word(builder, fpr_ptr, ft, flags);
            let fs_val = builder.ins().bitcast(types::F32, MemFlags::new(), fs_bits);
            let ft_val = builder.ins().bitcast(types::F32, MemFlags::new(), ft_bits);
            let result = builder.ins().fmul(fs_val, ft_val);
            let result_bits = builder.ins().bitcast(types::I32, MemFlags::new(), result);
            store_fpr_word(builder, fpr_ptr, fd, result_bits, flags);
        }
        Op::DivS { fd, fs, ft } => {
            let fs_bits = load_fpr_word(builder, fpr_ptr, fs, flags);
            let ft_bits = load_fpr_word(builder, fpr_ptr, ft, flags);
            let fs_val = builder.ins().bitcast(types::F32, MemFlags::new(), fs_bits);
            let ft_val = builder.ins().bitcast(types::F32, MemFlags::new(), ft_bits);
            let result = builder.ins().fdiv(fs_val, ft_val);
            let result_bits = builder.ins().bitcast(types::I32, MemFlags::new(), result);
            store_fpr_word(builder, fpr_ptr, fd, result_bits, flags);
        }
        Op::AddD { fd, fs, ft } => {
            let fs_bits = load_fpr_double_bits(builder, fpr_ptr, fs, flags);
            let ft_bits = load_fpr_double_bits(builder, fpr_ptr, ft, flags);
            let fs_val = builder.ins().bitcast(types::F64, MemFlags::new(), fs_bits);
            let ft_val = builder.ins().bitcast(types::F64, MemFlags::new(), ft_bits);
            let result = builder.ins().fadd(fs_val, ft_val);
            let result_bits = builder.ins().bitcast(types::I64, MemFlags::new(), result);
            store_fpr_double_bits(builder, fpr_ptr, fd, result_bits, flags);
        }
        Op::SubD { fd, fs, ft } => {
            let fs_bits = load_fpr_double_bits(builder, fpr_ptr, fs, flags);
            let ft_bits = load_fpr_double_bits(builder, fpr_ptr, ft, flags);
            let fs_val = builder.ins().bitcast(types::F64, MemFlags::new(), fs_bits);
            let ft_val = builder.ins().bitcast(types::F64, MemFlags::new(), ft_bits);
            let result = builder.ins().fsub(fs_val, ft_val);
            let result_bits = builder.ins().bitcast(types::I64, MemFlags::new(), result);
            store_fpr_double_bits(builder, fpr_ptr, fd, result_bits, flags);
        }
        Op::MulD { fd, fs, ft } => {
            let fs_bits = load_fpr_double_bits(builder, fpr_ptr, fs, flags);
            let ft_bits = load_fpr_double_bits(builder, fpr_ptr, ft, flags);
            let fs_val = builder.ins().bitcast(types::F64, MemFlags::new(), fs_bits);
            let ft_val = builder.ins().bitcast(types::F64, MemFlags::new(), ft_bits);
            let result = builder.ins().fmul(fs_val, ft_val);
            let result_bits = builder.ins().bitcast(types::I64, MemFlags::new(), result);
            store_fpr_double_bits(builder, fpr_ptr, fd, result_bits, flags);
        }
        Op::DivD { fd, fs, ft } => {
            let fs_bits = load_fpr_double_bits(builder, fpr_ptr, fs, flags);
            let ft_bits = load_fpr_double_bits(builder, fpr_ptr, ft, flags);
            let fs_val = builder.ins().bitcast(types::F64, MemFlags::new(), fs_bits);
            let ft_val = builder.ins().bitcast(types::F64, MemFlags::new(), ft_bits);
            let result = builder.ins().fdiv(fs_val, ft_val);
            let result_bits = builder.ins().bitcast(types::I64, MemFlags::new(), result);
            store_fpr_double_bits(builder, fpr_ptr, fd, result_bits, flags);
        }
        Op::Mfc0 { rt, rd } => {
            let reg = iconst_u64(builder, u64::from(rd));
            let call = builder.ins().call(helpers.cop0_read, &[callbacks_ptr, reg]);
            let value = builder.inst_results(call)[0];
            let value32 = builder.ins().ireduce(types::I32, value);
            let value64 = builder.ins().sextend(types::I64, value32);
            store_gpr(builder, rt, value64);
        }
        Op::Dmfc0 { rt, rd } => {
            let reg = iconst_u64(builder, u64::from(rd));
            let call = builder.ins().call(helpers.cop0_read, &[callbacks_ptr, reg]);
            let value = builder.inst_results(call)[0];
            store_gpr(builder, rt, value);
        }
        Op::Mtc0 { rt, rd } | Op::Dmtc0 { rt, rd } => {
            let reg = iconst_u64(builder, u64::from(rd));
            let value = load_gpr(builder, rt);
            builder
                .ins()
                .call(helpers.cop0_write, &[callbacks_ptr, reg, value]);
        }
        Op::Interp { raw } => {
            let raw = iconst_u64(builder, u64::from(raw));
            spill_gpr_vars(builder, gpr_ptr, flags);
            builder
                .ins()
                .call(helpers.interp_exec, &[callbacks_ptr, raw, current_pc]);
            reload_gpr_vars(builder, gpr_ptr, flags);
        }
        Op::Sync => {
            // SYNC is a no-op for this emulator core.
        }
        Op::Cache => {
            // CACHE is treated as a no-op by the interpreter.
        }
    }
}

/// Cranelift backend compiler.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CraneliftOptLevel {
    None,
    Speed,
    SpeedAndSize,
}

impl CraneliftOptLevel {
    pub fn parse(raw: &str) -> Option<Self> {
        match raw.trim().to_ascii_lowercase().as_str() {
            "none" => Some(Self::None),
            "speed" => Some(Self::Speed),
            "speed_and_size" | "speed-size" | "speedsize" => Some(Self::SpeedAndSize),
            _ => None,
        }
    }

    fn as_setting(self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Speed => "speed",
            Self::SpeedAndSize => "speed_and_size",
        }
    }
}

pub struct CraneliftCompiler {
    module: JITModule,
    context: cranelift_codegen::Context,
    builder_context: FunctionBuilderContext,
    load_u8_id: FuncId,
    load_u16_id: FuncId,
    load_u32_id: FuncId,
    load_u64_id: FuncId,
    store_u8_id: FuncId,
    store_u16_id: FuncId,
    store_u32_id: FuncId,
    store_u64_id: FuncId,
    store_invalidate_id: FuncId,
    cop0_read_id: FuncId,
    cop0_write_id: FuncId,
    cop1_condition_id: FuncId,
    interp_exec_id: FuncId,
    hi_read_id: FuncId,
    hi_write_id: FuncId,
    lo_read_id: FuncId,
    lo_write_id: FuncId,
    next_symbol_id: u64,
}

impl CraneliftCompiler {
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

    pub fn with_opt_level(opt_level: CraneliftOptLevel) -> Self {
        let verify_ir = Self::parse_env_bool("N64_CRANELIFT_VERIFY", false);
        Self::with_opt_level_and_verify(opt_level, verify_ir)
    }

    pub fn with_opt_level_and_verify(opt_level: CraneliftOptLevel, verify_ir: bool) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder
            .set("opt_level", opt_level.as_setting())
            .expect("set cranelift opt_level");
        // Useful for debugging backend issues; disabled by default for JIT throughput.
        flag_builder
            .set("enable_verifier", if verify_ir { "true" } else { "false" })
            .expect("set cranelift verifier");
        let flags = settings::Flags::new(flag_builder);

        let isa_builder = cranelift_native::builder().expect("create host ISA builder");
        let isa = isa_builder.finish(flags).expect("finish host ISA");
        let mut jit_builder = JITBuilder::with_isa(isa, default_libcall_names());
        jit_builder.symbol("n64_jit_load_u8", n64_jit_load_u8 as *const u8);
        jit_builder.symbol("n64_jit_load_u16", n64_jit_load_u16 as *const u8);
        jit_builder.symbol("n64_jit_load_u32", n64_jit_load_u32 as *const u8);
        jit_builder.symbol("n64_jit_load_u64", n64_jit_load_u64 as *const u8);
        jit_builder.symbol("n64_jit_store_u8", n64_jit_store_u8 as *const u8);
        jit_builder.symbol("n64_jit_store_u16", n64_jit_store_u16 as *const u8);
        jit_builder.symbol("n64_jit_store_u32", n64_jit_store_u32 as *const u8);
        jit_builder.symbol("n64_jit_store_u64", n64_jit_store_u64 as *const u8);
        jit_builder.symbol(
            "n64_jit_store_invalidate",
            n64_jit_store_invalidate as *const u8,
        );
        jit_builder.symbol("n64_jit_cop0_read", n64_jit_cop0_read as *const u8);
        jit_builder.symbol("n64_jit_cop0_write", n64_jit_cop0_write as *const u8);
        jit_builder.symbol(
            "n64_jit_cop1_condition",
            n64_jit_cop1_condition as *const u8,
        );
        jit_builder.symbol("n64_jit_interp_exec", n64_jit_interp_exec as *const u8);
        jit_builder.symbol("n64_jit_hi_read", n64_jit_hi_read as *const u8);
        jit_builder.symbol("n64_jit_hi_write", n64_jit_hi_write as *const u8);
        jit_builder.symbol("n64_jit_lo_read", n64_jit_lo_read as *const u8);
        jit_builder.symbol("n64_jit_lo_write", n64_jit_lo_write as *const u8);
        let mut module = JITModule::new(jit_builder);
        let ptr_ty = module.target_config().pointer_type();

        let mut unary_sig = module.make_signature();
        unary_sig.params.push(AbiParam::new(ptr_ty));
        unary_sig.params.push(AbiParam::new(types::I64));
        unary_sig.returns.push(AbiParam::new(types::I64));

        let mut ternary_sig = module.make_signature();
        ternary_sig.params.push(AbiParam::new(ptr_ty));
        ternary_sig.params.push(AbiParam::new(types::I64));
        ternary_sig.params.push(AbiParam::new(types::I64));

        let mut ctx_unary_sig = module.make_signature();
        ctx_unary_sig.params.push(AbiParam::new(ptr_ty));
        ctx_unary_sig.returns.push(AbiParam::new(types::I64));

        let mut ctx_binary_sig = module.make_signature();
        ctx_binary_sig.params.push(AbiParam::new(ptr_ty));
        ctx_binary_sig.params.push(AbiParam::new(types::I64));

        let load_u8_id = module
            .declare_function("n64_jit_load_u8", Linkage::Import, &unary_sig)
            .expect("declare n64_jit_load_u8");
        let load_u16_id = module
            .declare_function("n64_jit_load_u16", Linkage::Import, &unary_sig)
            .expect("declare n64_jit_load_u16");
        let load_u32_id = module
            .declare_function("n64_jit_load_u32", Linkage::Import, &unary_sig)
            .expect("declare n64_jit_load_u32");
        let load_u64_id = module
            .declare_function("n64_jit_load_u64", Linkage::Import, &unary_sig)
            .expect("declare n64_jit_load_u64");
        let store_u8_id = module
            .declare_function("n64_jit_store_u8", Linkage::Import, &ternary_sig)
            .expect("declare n64_jit_store_u8");
        let store_u16_id = module
            .declare_function("n64_jit_store_u16", Linkage::Import, &ternary_sig)
            .expect("declare n64_jit_store_u16");
        let store_u32_id = module
            .declare_function("n64_jit_store_u32", Linkage::Import, &ternary_sig)
            .expect("declare n64_jit_store_u32");
        let store_u64_id = module
            .declare_function("n64_jit_store_u64", Linkage::Import, &ternary_sig)
            .expect("declare n64_jit_store_u64");
        let store_invalidate_id = module
            .declare_function("n64_jit_store_invalidate", Linkage::Import, &ternary_sig)
            .expect("declare n64_jit_store_invalidate");
        let cop0_read_id = module
            .declare_function("n64_jit_cop0_read", Linkage::Import, &unary_sig)
            .expect("declare n64_jit_cop0_read");
        let cop0_write_id = module
            .declare_function("n64_jit_cop0_write", Linkage::Import, &ternary_sig)
            .expect("declare n64_jit_cop0_write");
        let cop1_condition_id = module
            .declare_function("n64_jit_cop1_condition", Linkage::Import, &ctx_unary_sig)
            .expect("declare n64_jit_cop1_condition");
        let interp_exec_id = module
            .declare_function("n64_jit_interp_exec", Linkage::Import, &ternary_sig)
            .expect("declare n64_jit_interp_exec");
        let hi_read_id = module
            .declare_function("n64_jit_hi_read", Linkage::Import, &ctx_unary_sig)
            .expect("declare n64_jit_hi_read");
        let hi_write_id = module
            .declare_function("n64_jit_hi_write", Linkage::Import, &ctx_binary_sig)
            .expect("declare n64_jit_hi_write");
        let lo_read_id = module
            .declare_function("n64_jit_lo_read", Linkage::Import, &ctx_unary_sig)
            .expect("declare n64_jit_lo_read");
        let lo_write_id = module
            .declare_function("n64_jit_lo_write", Linkage::Import, &ctx_binary_sig)
            .expect("declare n64_jit_lo_write");

        let context = module.make_context();

        Self {
            module,
            context,
            builder_context: FunctionBuilderContext::new(),
            load_u8_id,
            load_u16_id,
            load_u32_id,
            load_u64_id,
            store_u8_id,
            store_u16_id,
            store_u32_id,
            store_u64_id,
            store_invalidate_id,
            cop0_read_id,
            cop0_write_id,
            cop1_condition_id,
            interp_exec_id,
            hi_read_id,
            hi_write_id,
            lo_read_id,
            lo_write_id,
            next_symbol_id: 0,
        }
    }
}

impl Default for CraneliftCompiler {
    fn default() -> Self {
        let env_level =
            std::env::var("N64_CRANELIFT_OPT_LEVEL").unwrap_or_else(|_| "none".to_string());
        let opt_level = match CraneliftOptLevel::parse(&env_level) {
            Some(level) => level,
            None => {
                log::warn!(
                    "Unknown N64_CRANELIFT_OPT_LEVEL={:?}; using \"none\"",
                    env_level
                );
                CraneliftOptLevel::None
            }
        };
        Self::with_opt_level(opt_level)
    }
}

impl BlockCompiler for CraneliftCompiler {
    fn name(&self) -> &'static str {
        "cranelift"
    }

    fn compile(
        &mut self,
        request: &CompileRequest,
        source: &mut dyn InstructionSource,
    ) -> Result<CompiledBlock, CompileError> {
        let mut steps = Vec::new();
        let mut phys = request.start_phys;
        let max_instructions = request.max_instructions.max(1);
        let mut decoded_count = 0u32;
        let mut interp_op_count = 0u32;
        let mut has_control_flow = false;
        let mut ended_on_unsupported = false;
        let mut may_write_interrupt_state = false;

        while decoded_count < max_instructions {
            let opcode = match source.read_u32(phys) {
                Ok(opcode) => opcode,
                Err(err) => {
                    if steps.is_empty() {
                        return Err(err);
                    }
                    break;
                }
            };

            if let Some(branch) = decode_branch(opcode) {
                if decoded_count + 2 > max_instructions {
                    if steps.is_empty() {
                        return Err(CompileError::UnsupportedOpcode {
                            phys_addr: phys,
                            opcode,
                        });
                    }
                    break;
                }
                let delay_phys = phys.wrapping_add(4);
                let delay_raw = match source.read_u32(delay_phys) {
                    Ok(raw) => raw,
                    Err(err) => {
                        if steps.is_empty() {
                            return Err(err);
                        }
                        break;
                    }
                };
                let Some(delay_op) = decode_supported_non_branch(delay_raw) else {
                    if steps.is_empty() {
                        return Err(CompileError::UnsupportedOpcode {
                            phys_addr: phys,
                            opcode,
                        });
                    }
                    break;
                };
                if matches!(delay_op, Op::Interp { .. }) {
                    interp_op_count = interp_op_count.saturating_add(1);
                }
                if op_may_write_interrupt_state(delay_op) {
                    may_write_interrupt_state = true;
                }
                let continue_fallthrough = matches!(
                    branch,
                    BranchTerminator::Beq { .. }
                        | BranchTerminator::Bne { .. }
                        | BranchTerminator::Blez { .. }
                        | BranchTerminator::Bgtz { .. }
                        | BranchTerminator::Beql { .. }
                        | BranchTerminator::Bnel { .. }
                        | BranchTerminator::Blezl { .. }
                        | BranchTerminator::Bgtzl { .. }
                        | BranchTerminator::Bltz { .. }
                        | BranchTerminator::Bgez { .. }
                        | BranchTerminator::Bltzl { .. }
                        | BranchTerminator::Bgezl { .. }
                        | BranchTerminator::Bc1f { .. }
                        | BranchTerminator::Bc1t { .. }
                        | BranchTerminator::Bc1fl { .. }
                        | BranchTerminator::Bc1tl { .. }
                );
                let follow_unconditional_target = match branch {
                    BranchTerminator::J { target } | BranchTerminator::Jal { target } => {
                        let target_phys = jump_target_phys(phys, target);
                        (target_phys != phys).then_some(target_phys)
                    }
                    _ => None,
                };
                steps.push(TraceStep::Branch {
                    phys,
                    branch,
                    delay_op,
                    continue_fallthrough,
                });
                has_control_flow = true;
                decoded_count += 2;
                if !continue_fallthrough {
                    if let Some(target_phys) = follow_unconditional_target {
                        let already_seen =
                            steps.iter().copied().any(|step| step.phys() == target_phys);
                        if !already_seen {
                            phys = target_phys;
                            continue;
                        }
                    }
                    break;
                }
                phys = phys.wrapping_add(8);
                continue;
            }

            match decode_supported_non_branch(opcode) {
                Some(op) => {
                    let writes_interrupt_state = op_may_write_interrupt_state(op);
                    let at_block_start = steps.is_empty();
                    if writes_interrupt_state && !at_block_start {
                        break;
                    }
                    if matches!(op, Op::Interp { .. }) {
                        interp_op_count = interp_op_count.saturating_add(1);
                    }
                    if writes_interrupt_state {
                        may_write_interrupt_state = true;
                    }
                    steps.push(TraceStep::Op { phys, op });
                    decoded_count += 1;
                    phys = phys.wrapping_add(4);
                }
                None => {
                    if can_inline_interp_non_branch(opcode) {
                        if !steps.is_empty() {
                            break;
                        }
                        may_write_interrupt_state = true;
                        steps.push(TraceStep::Op {
                            phys,
                            op: Op::Interp { raw: opcode },
                        });
                        interp_op_count = interp_op_count.saturating_add(1);
                        decoded_count += 1;
                        break;
                    }
                    if steps.is_empty() {
                        return Err(CompileError::UnsupportedOpcode {
                            phys_addr: phys,
                            opcode,
                        });
                    }
                    ended_on_unsupported = true;
                    break;
                }
            }
        }

        self.context.clear();
        self.context.func.signature.params.clear();
        self.context.func.signature.returns.clear();
        let ptr_ty = self.module.target_config().pointer_type();
        self.context
            .func
            .signature
            .params
            .push(AbiParam::new(ptr_ty));
        self.context
            .func
            .signature
            .params
            .push(AbiParam::new(ptr_ty));
        self.context
            .func
            .signature
            .params
            .push(AbiParam::new(types::I64));
        self.context
            .func
            .signature
            .params
            .push(AbiParam::new(ptr_ty));
        self.context
            .func
            .signature
            .params
            .push(AbiParam::new(ptr_ty));
        self.context
            .func
            .signature
            .params
            .push(AbiParam::new(types::I32));
        self.context
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I64));

        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let gpr_ptr = builder.block_params(entry_block)[0];
        let fpr_ptr = builder.block_params(entry_block)[1];
        let start_pc = builder.block_params(entry_block)[2];
        let callbacks_ptr = builder.block_params(entry_block)[3];
        let retired_out_ptr = builder.block_params(entry_block)[4];
        let runtime_retire_limit = builder.block_params(entry_block)[5];
        let mut callback_flags = MemFlags::new();
        callback_flags.set_notrap();
        callback_flags.set_aligned();
        let fastmem_base = builder.ins().load(
            ptr_ty,
            callback_flags,
            callbacks_ptr,
            std::mem::offset_of!(RuntimeCallbacks, fastmem_base) as i32,
        );
        let fastmem_phys_limit = builder.ins().load(
            types::I64,
            callback_flags,
            callbacks_ptr,
            std::mem::offset_of!(RuntimeCallbacks, fastmem_phys_limit) as i32,
        );
        let fastmem_phys_mask = builder.ins().load(
            types::I64,
            callback_flags,
            callbacks_ptr,
            std::mem::offset_of!(RuntimeCallbacks, fastmem_phys_mask) as i32,
        );
        let hi_ptr = builder.ins().load(
            ptr_ty,
            callback_flags,
            callbacks_ptr,
            std::mem::offset_of!(RuntimeCallbacks, hi_ptr) as i32,
        );
        let lo_ptr = builder.ins().load(
            ptr_ty,
            callback_flags,
            callbacks_ptr,
            std::mem::offset_of!(RuntimeCallbacks, lo_ptr) as i32,
        );
        let cop1_fcr31_ptr = builder.ins().load(
            ptr_ty,
            callback_flags,
            callbacks_ptr,
            std::mem::offset_of!(RuntimeCallbacks, cop1_fcr31_ptr) as i32,
        );
        let fastmem = FastmemValues {
            ptr_ty,
            base: fastmem_base,
            phys_limit: fastmem_phys_limit,
            phys_mask: fastmem_phys_mask,
        };
        let runtime_ptrs = RuntimeStatePtrs {
            ptr_ty,
            hi_ptr,
            lo_ptr,
            cop1_fcr31_ptr,
        };
        let helpers = ImportedFuncRefs {
            load_u8: self
                .module
                .declare_func_in_func(self.load_u8_id, builder.func),
            load_u16: self
                .module
                .declare_func_in_func(self.load_u16_id, builder.func),
            load_u32: self
                .module
                .declare_func_in_func(self.load_u32_id, builder.func),
            load_u64: self
                .module
                .declare_func_in_func(self.load_u64_id, builder.func),
            store_u8: self
                .module
                .declare_func_in_func(self.store_u8_id, builder.func),
            store_u16: self
                .module
                .declare_func_in_func(self.store_u16_id, builder.func),
            store_u32: self
                .module
                .declare_func_in_func(self.store_u32_id, builder.func),
            store_u64: self
                .module
                .declare_func_in_func(self.store_u64_id, builder.func),
            store_invalidate: self
                .module
                .declare_func_in_func(self.store_invalidate_id, builder.func),
            cop0_read: self
                .module
                .declare_func_in_func(self.cop0_read_id, builder.func),
            cop0_write: self
                .module
                .declare_func_in_func(self.cop0_write_id, builder.func),
            cop1_condition: self
                .module
                .declare_func_in_func(self.cop1_condition_id, builder.func),
            interp_exec: self
                .module
                .declare_func_in_func(self.interp_exec_id, builder.func),
            hi_read: self
                .module
                .declare_func_in_func(self.hi_read_id, builder.func),
            hi_write: self
                .module
                .declare_func_in_func(self.hi_write_id, builder.func),
            lo_read: self
                .module
                .declare_func_in_func(self.lo_read_id, builder.func),
            lo_write: self
                .module
                .declare_func_in_func(self.lo_write_id, builder.func),
        };
        let mut flags = MemFlags::new();
        flags.set_notrap();
        flags.set_aligned();
        init_gpr_vars(&mut builder, gpr_ptr, flags);

        let mut phys_to_index = HashMap::with_capacity(steps.len());
        for (idx, step) in steps.iter().copied().enumerate() {
            phys_to_index.insert(step.phys(), idx);
        }

        let has_backedge = steps.iter().copied().enumerate().any(|(step_idx, step)| {
            let TraceStep::Branch { phys, branch, .. } = step else {
                return false;
            };
            let Some(target_phys) = branch_target_phys(phys, branch) else {
                return false;
            };
            phys_to_index
                .get(&target_phys)
                .copied()
                .map(|target_idx| target_idx <= step_idx)
                .unwrap_or(false)
        });

        let mut block_starts = vec![0usize];
        for (step_idx, step) in steps.iter().copied().enumerate() {
            let TraceStep::Branch {
                phys,
                branch,
                continue_fallthrough,
                ..
            } = step
            else {
                continue;
            };
            if continue_fallthrough && step_idx + 1 < steps.len() {
                block_starts.push(step_idx + 1);
            }
            if let Some(target_phys) = branch_target_phys(phys, branch) {
                if let Some(target_idx) = phys_to_index.get(&target_phys).copied() {
                    block_starts.push(target_idx);
                }
            }
        }
        block_starts.sort_unstable();
        block_starts.dedup();

        let mut block_for_step = vec![0usize; steps.len()];
        for (block_idx, &start_idx) in block_starts.iter().enumerate() {
            let end_idx = block_starts
                .get(block_idx + 1)
                .copied()
                .unwrap_or(steps.len());
            for step_idx in start_idx..end_idx {
                block_for_step[step_idx] = block_idx;
            }
        }

        let exit_block = builder.create_block();
        builder.append_block_param(exit_block, types::I64);
        builder.append_block_param(exit_block, types::I32);
        let step_blocks: Vec<_> = block_starts
            .iter()
            .map(|_| {
                let b = builder.create_block();
                builder.append_block_param(b, types::I32);
                b
            })
            .collect();
        let compile_limit_value = request.max_instructions.max(1).min(i32::MAX as u32);
        let compile_limit = builder
            .ins()
            .iconst(types::I32, i64::from(compile_limit_value));
        let zero = builder.ins().iconst(types::I32, 0);
        let runtime_is_zero = builder.ins().icmp(IntCC::Equal, runtime_retire_limit, zero);
        let runtime_limit =
            builder
                .ins()
                .select(runtime_is_zero, compile_limit, runtime_retire_limit);
        let retire_limit = builder.ins().umin(runtime_limit, compile_limit);

        let zero_retired = builder.ins().iconst(types::I32, 0);
        let args = [zero_retired.into()];
        builder.ins().jump(step_blocks[0], &args);

        let mut active_block = None;
        let mut active_block_idx = 0usize;
        let mut retired_count = zero_retired;
        for (step_idx, step) in steps.iter().copied().enumerate() {
            let block_idx = block_for_step[step_idx];
            let step_block = step_blocks[block_idx];
            if active_block != Some(step_block) {
                builder.switch_to_block(step_block);
                retired_count = builder.block_params(step_block)[0];
                active_block = Some(step_block);
                active_block_idx = block_idx;
            }
            let step_phys = step.phys();
            let step_delta = i64::from(step_phys.wrapping_sub(request.start_phys));
            let current_pc = builder.ins().iadd_imm(start_pc, step_delta);
            let reached_budget = builder.ins().icmp(
                IntCC::UnsignedGreaterThanOrEqual,
                retired_count,
                retire_limit,
            );
            let execute_step_block = builder.create_block();
            let exit_args = [current_pc.into(), retired_count.into()];
            builder.ins().brif(
                reached_budget,
                exit_block,
                &exit_args,
                execute_step_block,
                &[],
            );
            builder.switch_to_block(execute_step_block);

            match step {
                TraceStep::Op { op, .. } => {
                    emit_op(
                        &mut builder,
                        gpr_ptr,
                        fpr_ptr,
                        callbacks_ptr,
                        helpers,
                        runtime_ptrs,
                        fastmem,
                        flags,
                        current_pc,
                        op,
                    );
                    retired_count = builder.ins().iadd_imm(retired_count, 1);
                    let next_block_idx = if step_idx + 1 < steps.len() {
                        Some(block_for_step[step_idx + 1])
                    } else {
                        None
                    };
                    match next_block_idx {
                        Some(next_idx) if next_idx == active_block_idx => {}
                        Some(next_idx) => {
                            let args = [retired_count.into()];
                            builder.ins().jump(step_blocks[next_idx], &args);
                            active_block = None;
                        }
                        None => {
                            let ret_pc = builder.ins().iadd_imm(current_pc, 4);
                            let args = [ret_pc.into(), retired_count.into()];
                            builder.ins().jump(exit_block, &args);
                            active_block = None;
                        }
                    }
                }
                TraceStep::Branch {
                    phys,
                    branch,
                    delay_op,
                    continue_fallthrough,
                } => {
                    let next_of_branch = builder.ins().iadd_imm(current_pc, 4);
                    let fallthrough_pc = builder.ins().iadd_imm(current_pc, 8);
                    let fallthrough_block = if continue_fallthrough && step_idx + 1 < steps.len() {
                        Some(step_blocks[block_for_step[step_idx + 1]])
                    } else {
                        None
                    };

                    match branch {
                        BranchTerminator::J { target } => {
                            let target_phys = jump_target_phys(phys, target);
                            let target_block = phys_to_index
                                .get(&target_phys)
                                .copied()
                                .map(|idx| step_blocks[block_for_step[idx]]);
                            emit_op(
                                &mut builder,
                                gpr_ptr,
                                fpr_ptr,
                                callbacks_ptr,
                                helpers,
                                runtime_ptrs,
                                fastmem,
                                flags,
                                next_of_branch,
                                delay_op,
                            );
                            let retired_after = builder.ins().iadd_imm(retired_count, 2);
                            if let Some(target) = target_block {
                                let args = [retired_after.into()];
                                builder.ins().jump(target, &args);
                            } else {
                                let upper = iconst_u64(&mut builder, 0xFFFF_FFFF_F000_0000);
                                let upper_pc = builder.ins().band(current_pc, upper);
                                let low = iconst_u64(&mut builder, u64::from(target) << 2);
                                let target_pc = builder.ins().bor(upper_pc, low);
                                let args = [target_pc.into(), retired_after.into()];
                                builder.ins().jump(exit_block, &args);
                            }
                        }
                        BranchTerminator::Jal { target } => {
                            let target_phys = jump_target_phys(phys, target);
                            let target_block = phys_to_index
                                .get(&target_phys)
                                .copied()
                                .map(|idx| step_blocks[block_for_step[idx]]);
                            let link = builder.ins().iadd_imm(current_pc, 8);
                            store_gpr(&mut builder, 31, link);
                            emit_op(
                                &mut builder,
                                gpr_ptr,
                                fpr_ptr,
                                callbacks_ptr,
                                helpers,
                                runtime_ptrs,
                                fastmem,
                                flags,
                                next_of_branch,
                                delay_op,
                            );
                            let retired_after = builder.ins().iadd_imm(retired_count, 2);
                            if let Some(target) = target_block {
                                let args = [retired_after.into()];
                                builder.ins().jump(target, &args);
                            } else {
                                let upper = iconst_u64(&mut builder, 0xFFFF_FFFF_F000_0000);
                                let upper_pc = builder.ins().band(current_pc, upper);
                                let low = iconst_u64(&mut builder, u64::from(target) << 2);
                                let target_pc = builder.ins().bor(upper_pc, low);
                                let args = [target_pc.into(), retired_after.into()];
                                builder.ins().jump(exit_block, &args);
                            }
                        }
                        BranchTerminator::Jr { rs } => {
                            emit_op(
                                &mut builder,
                                gpr_ptr,
                                fpr_ptr,
                                callbacks_ptr,
                                helpers,
                                runtime_ptrs,
                                fastmem,
                                flags,
                                next_of_branch,
                                delay_op,
                            );
                            let target_pc = load_gpr(&mut builder, rs);
                            let retired_after = builder.ins().iadd_imm(retired_count, 2);
                            let args = [target_pc.into(), retired_after.into()];
                            builder.ins().jump(exit_block, &args);
                        }
                        BranchTerminator::Jalr { rs, rd } => {
                            let link = builder.ins().iadd_imm(current_pc, 8);
                            store_gpr(&mut builder, rd, link);
                            emit_op(
                                &mut builder,
                                gpr_ptr,
                                fpr_ptr,
                                callbacks_ptr,
                                helpers,
                                runtime_ptrs,
                                fastmem,
                                flags,
                                next_of_branch,
                                delay_op,
                            );
                            let target_pc = load_gpr(&mut builder, rs);
                            let retired_after = builder.ins().iadd_imm(retired_count, 2);
                            let args = [target_pc.into(), retired_after.into()];
                            builder.ins().jump(exit_block, &args);
                        }
                        BranchTerminator::Beq { rs, rt, offset }
                        | BranchTerminator::Bne { rs, rt, offset }
                        | BranchTerminator::Beql { rs, rt, offset }
                        | BranchTerminator::Bnel { rs, rt, offset } => {
                            let lhs = load_gpr(&mut builder, rs);
                            let rhs = load_gpr(&mut builder, rt);
                            let cond_taken = match branch {
                                BranchTerminator::Beq { .. } | BranchTerminator::Beql { .. } => {
                                    builder.ins().icmp(IntCC::Equal, lhs, rhs)
                                }
                                _ => builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
                            };
                            let offset_val = builder
                                .ins()
                                .iconst(types::I64, i64::from((offset as i32) << 2));
                            let taken_pc = builder.ins().iadd(next_of_branch, offset_val);
                            let target_phys = phys
                                .wrapping_add(4)
                                .wrapping_add(((offset as i32) << 2) as u32);
                            let taken_block = phys_to_index
                                .get(&target_phys)
                                .copied()
                                .map(|idx| step_blocks[block_for_step[idx]]);
                            let is_likely = matches!(
                                branch,
                                BranchTerminator::Beql { .. } | BranchTerminator::Bnel { .. }
                            );

                            if is_likely {
                                let taken_exec_block = builder.create_block();
                                let not_taken_retired = builder.ins().iadd_imm(retired_count, 1);
                                match fallthrough_block {
                                    Some(fallthrough) => {
                                        let args = [not_taken_retired.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken_exec_block,
                                            &[],
                                            fallthrough,
                                            &args,
                                        );
                                    }
                                    None => {
                                        let args =
                                            [fallthrough_pc.into(), not_taken_retired.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken_exec_block,
                                            &[],
                                            exit_block,
                                            &args,
                                        );
                                    }
                                }

                                builder.switch_to_block(taken_exec_block);
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    fpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    runtime_ptrs,
                                    fastmem,
                                    flags,
                                    next_of_branch,
                                    delay_op,
                                );
                                let taken_retired = builder.ins().iadd_imm(retired_count, 2);
                                match taken_block {
                                    Some(target) => {
                                        let args = [taken_retired.into()];
                                        builder.ins().jump(target, &args);
                                    }
                                    None => {
                                        let args = [taken_pc.into(), taken_retired.into()];
                                        builder.ins().jump(exit_block, &args);
                                    }
                                }
                            } else {
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    fpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    runtime_ptrs,
                                    fastmem,
                                    flags,
                                    next_of_branch,
                                    delay_op,
                                );
                                let retired_after = builder.ins().iadd_imm(retired_count, 2);
                                match (taken_block, fallthrough_block) {
                                    (Some(taken), Some(fallthrough)) => {
                                        let args = [retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken,
                                            &args,
                                            fallthrough,
                                            &args,
                                        );
                                    }
                                    (Some(taken), None) => {
                                        let targs = [retired_after.into()];
                                        let fargs = [fallthrough_pc.into(), retired_after.into()];
                                        builder
                                            .ins()
                                            .brif(cond_taken, taken, &targs, exit_block, &fargs);
                                    }
                                    (None, Some(fallthrough)) => {
                                        let targs = [taken_pc.into(), retired_after.into()];
                                        let fargs = [retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            exit_block,
                                            &targs,
                                            fallthrough,
                                            &fargs,
                                        );
                                    }
                                    (None, None) => {
                                        let targs = [taken_pc.into(), retired_after.into()];
                                        let fargs = [fallthrough_pc.into(), retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken, exit_block, &targs, exit_block, &fargs,
                                        );
                                    }
                                }
                            }
                        }
                        BranchTerminator::Bc1f { offset }
                        | BranchTerminator::Bc1t { offset }
                        | BranchTerminator::Bc1fl { offset }
                        | BranchTerminator::Bc1tl { offset } => {
                            let condition = read_cop1_condition_flag(
                                &mut builder,
                                callbacks_ptr,
                                helpers,
                                runtime_ptrs,
                            );
                            let zero = iconst_u64(&mut builder, 0);
                            let cond_taken = match branch {
                                BranchTerminator::Bc1f { .. } | BranchTerminator::Bc1fl { .. } => {
                                    builder.ins().icmp(IntCC::Equal, condition, zero)
                                }
                                _ => builder.ins().icmp(IntCC::NotEqual, condition, zero),
                            };
                            let offset_val = builder
                                .ins()
                                .iconst(types::I64, i64::from((offset as i32) << 2));
                            let taken_pc = builder.ins().iadd(next_of_branch, offset_val);
                            let target_phys = phys
                                .wrapping_add(4)
                                .wrapping_add(((offset as i32) << 2) as u32);
                            let taken_block = phys_to_index
                                .get(&target_phys)
                                .copied()
                                .map(|idx| step_blocks[block_for_step[idx]]);
                            let is_likely = matches!(
                                branch,
                                BranchTerminator::Bc1fl { .. } | BranchTerminator::Bc1tl { .. }
                            );

                            if is_likely {
                                let taken_exec_block = builder.create_block();
                                let not_taken_retired = builder.ins().iadd_imm(retired_count, 1);
                                match fallthrough_block {
                                    Some(fallthrough) => {
                                        let args = [not_taken_retired.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken_exec_block,
                                            &[],
                                            fallthrough,
                                            &args,
                                        );
                                    }
                                    None => {
                                        let args =
                                            [fallthrough_pc.into(), not_taken_retired.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken_exec_block,
                                            &[],
                                            exit_block,
                                            &args,
                                        );
                                    }
                                }

                                builder.switch_to_block(taken_exec_block);
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    fpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    runtime_ptrs,
                                    fastmem,
                                    flags,
                                    next_of_branch,
                                    delay_op,
                                );
                                let taken_retired = builder.ins().iadd_imm(retired_count, 2);
                                match taken_block {
                                    Some(target) => {
                                        let args = [taken_retired.into()];
                                        builder.ins().jump(target, &args);
                                    }
                                    None => {
                                        let args = [taken_pc.into(), taken_retired.into()];
                                        builder.ins().jump(exit_block, &args);
                                    }
                                }
                            } else {
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    fpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    runtime_ptrs,
                                    fastmem,
                                    flags,
                                    next_of_branch,
                                    delay_op,
                                );
                                let retired_after = builder.ins().iadd_imm(retired_count, 2);
                                match (taken_block, fallthrough_block) {
                                    (Some(taken), Some(fallthrough)) => {
                                        let args = [retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken,
                                            &args,
                                            fallthrough,
                                            &args,
                                        );
                                    }
                                    (Some(taken), None) => {
                                        let targs = [retired_after.into()];
                                        let fargs = [fallthrough_pc.into(), retired_after.into()];
                                        builder
                                            .ins()
                                            .brif(cond_taken, taken, &targs, exit_block, &fargs);
                                    }
                                    (None, Some(fallthrough)) => {
                                        let targs = [taken_pc.into(), retired_after.into()];
                                        let fargs = [retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            exit_block,
                                            &targs,
                                            fallthrough,
                                            &fargs,
                                        );
                                    }
                                    (None, None) => {
                                        let targs = [taken_pc.into(), retired_after.into()];
                                        let fargs = [fallthrough_pc.into(), retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken, exit_block, &targs, exit_block, &fargs,
                                        );
                                    }
                                }
                            }
                        }
                        BranchTerminator::Blez { rs, offset }
                        | BranchTerminator::Bgtz { rs, offset }
                        | BranchTerminator::Blezl { rs, offset }
                        | BranchTerminator::Bgtzl { rs, offset }
                        | BranchTerminator::Bltz { rs, offset }
                        | BranchTerminator::Bgez { rs, offset }
                        | BranchTerminator::Bltzl { rs, offset }
                        | BranchTerminator::Bgezl { rs, offset } => {
                            let lhs = load_gpr(&mut builder, rs);
                            let zero = iconst_u64(&mut builder, 0);
                            let cond_taken = match branch {
                                BranchTerminator::Blez { .. } | BranchTerminator::Blezl { .. } => {
                                    builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, zero)
                                }
                                BranchTerminator::Bgtz { .. } | BranchTerminator::Bgtzl { .. } => {
                                    builder.ins().icmp(IntCC::SignedGreaterThan, lhs, zero)
                                }
                                BranchTerminator::Bltz { .. } | BranchTerminator::Bltzl { .. } => {
                                    builder.ins().icmp(IntCC::SignedLessThan, lhs, zero)
                                }
                                _ => builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThanOrEqual, lhs, zero),
                            };
                            let offset_val = builder
                                .ins()
                                .iconst(types::I64, i64::from((offset as i32) << 2));
                            let taken_pc = builder.ins().iadd(next_of_branch, offset_val);
                            let target_phys = phys
                                .wrapping_add(4)
                                .wrapping_add(((offset as i32) << 2) as u32);
                            let taken_block = phys_to_index
                                .get(&target_phys)
                                .copied()
                                .map(|idx| step_blocks[block_for_step[idx]]);
                            let is_likely = matches!(
                                branch,
                                BranchTerminator::Blezl { .. }
                                    | BranchTerminator::Bgtzl { .. }
                                    | BranchTerminator::Bltzl { .. }
                                    | BranchTerminator::Bgezl { .. }
                            );

                            if is_likely {
                                let taken_exec_block = builder.create_block();
                                let not_taken_retired = builder.ins().iadd_imm(retired_count, 1);
                                match fallthrough_block {
                                    Some(fallthrough) => {
                                        let args = [not_taken_retired.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken_exec_block,
                                            &[],
                                            fallthrough,
                                            &args,
                                        );
                                    }
                                    None => {
                                        let args =
                                            [fallthrough_pc.into(), not_taken_retired.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken_exec_block,
                                            &[],
                                            exit_block,
                                            &args,
                                        );
                                    }
                                }

                                builder.switch_to_block(taken_exec_block);
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    fpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    runtime_ptrs,
                                    fastmem,
                                    flags,
                                    next_of_branch,
                                    delay_op,
                                );
                                let taken_retired = builder.ins().iadd_imm(retired_count, 2);
                                match taken_block {
                                    Some(target) => {
                                        let args = [taken_retired.into()];
                                        builder.ins().jump(target, &args);
                                    }
                                    None => {
                                        let args = [taken_pc.into(), taken_retired.into()];
                                        builder.ins().jump(exit_block, &args);
                                    }
                                }
                            } else {
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    fpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    runtime_ptrs,
                                    fastmem,
                                    flags,
                                    next_of_branch,
                                    delay_op,
                                );
                                let retired_after = builder.ins().iadd_imm(retired_count, 2);
                                match (taken_block, fallthrough_block) {
                                    (Some(taken), Some(fallthrough)) => {
                                        let args = [retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            taken,
                                            &args,
                                            fallthrough,
                                            &args,
                                        );
                                    }
                                    (Some(taken), None) => {
                                        let targs = [retired_after.into()];
                                        let fargs = [fallthrough_pc.into(), retired_after.into()];
                                        builder
                                            .ins()
                                            .brif(cond_taken, taken, &targs, exit_block, &fargs);
                                    }
                                    (None, Some(fallthrough)) => {
                                        let targs = [taken_pc.into(), retired_after.into()];
                                        let fargs = [retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken,
                                            exit_block,
                                            &targs,
                                            fallthrough,
                                            &fargs,
                                        );
                                    }
                                    (None, None) => {
                                        let targs = [taken_pc.into(), retired_after.into()];
                                        let fargs = [fallthrough_pc.into(), retired_after.into()];
                                        builder.ins().brif(
                                            cond_taken, exit_block, &targs, exit_block, &fargs,
                                        );
                                    }
                                }
                            }
                        }
                    }
                    active_block = None;
                }
            }
        }

        builder.seal_all_blocks();
        builder.switch_to_block(exit_block);
        let exit_params = builder.block_params(exit_block);
        let ret_pc = exit_params[0];
        let retired_count = exit_params[1];
        spill_gpr_vars(&mut builder, gpr_ptr, flags);

        builder
            .ins()
            .store(flags, retired_count, retired_out_ptr, 0);
        builder.ins().return_(&[ret_pc]);
        builder.finalize();

        let symbol = format!("n64_jit_block_{}", self.next_symbol_id);
        self.next_symbol_id += 1;

        let func_id = self
            .module
            .declare_function(&symbol, Linkage::Local, &self.context.func.signature)
            .map_err(|e| CompileError::Backend {
                message: format!("declare_function: {e}"),
            })?;

        self.module
            .define_function(func_id, &mut self.context)
            .map_err(|e| CompileError::Backend {
                message: format!("define_function: {e}"),
            })?;

        self.module.clear_context(&mut self.context);
        self.module
            .finalize_definitions()
            .map_err(|e| CompileError::Backend {
                message: format!("finalize_definitions: {e}"),
            })?;

        let entry = BlockEntry(self.module.get_finalized_function(func_id));

        let instruction_count = decoded_count;

        Ok(CompiledBlock {
            start_phys: request.start_phys,
            end_phys: request
                .start_phys
                .wrapping_add(instruction_count.saturating_mul(4)),
            instruction_count,
            interp_op_count,
            max_retired_instructions: request.max_instructions.max(1),
            may_write_interrupt_state,
            has_backedge,
            has_control_flow,
            ended_on_unsupported,
            entry,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct TestSource {
        words: HashMap<u32, u32>,
    }

    impl TestSource {
        fn with_words(pairs: &[(u32, u32)]) -> Self {
            let mut words = HashMap::new();
            for (addr, word) in pairs {
                words.insert(*addr, *word);
            }
            Self { words }
        }
    }

    impl InstructionSource for TestSource {
        fn read_u32(&mut self, phys_addr: u32) -> Result<u32, CompileError> {
            self.words
                .get(&phys_addr)
                .copied()
                .ok_or(CompileError::MemoryRead { phys_addr })
        }
    }

    unsafe extern "C" fn test_load_u8(_user: *mut u8, _vaddr: u64) -> u64 {
        0
    }

    unsafe extern "C" fn test_load_u16(_user: *mut u8, _vaddr: u64) -> u64 {
        0
    }

    unsafe extern "C" fn test_load_u32(_user: *mut u8, _vaddr: u64) -> u64 {
        0
    }

    unsafe extern "C" fn test_load_u64(_user: *mut u8, _vaddr: u64) -> u64 {
        0
    }

    unsafe extern "C" fn test_store_u8(_user: *mut u8, _vaddr: u64, _value: u64) {}

    unsafe extern "C" fn test_store_u16(_user: *mut u8, _vaddr: u64, _value: u64) {}

    unsafe extern "C" fn test_store_u32(_user: *mut u8, _vaddr: u64, _value: u64) {}

    unsafe extern "C" fn test_store_u64(_user: *mut u8, _vaddr: u64, _value: u64) {}

    unsafe extern "C" fn test_store_invalidate(_user: *mut u8, _phys: u64, _len: u64) {}

    unsafe extern "C" fn test_cop0_read(_user: *mut u8, _reg: u64) -> u64 {
        0
    }

    unsafe extern "C" fn test_cop0_write(_user: *mut u8, _reg: u64, _value: u64) {}

    unsafe extern "C" fn test_cop1_condition(_user: *mut u8) -> u64 {
        0
    }

    unsafe extern "C" fn test_interp_exec(_user: *mut u8, _raw: u64, _current_pc: u64) {}

    unsafe extern "C" fn test_hi_read(_user: *mut u8) -> u64 {
        0
    }

    unsafe extern "C" fn test_hi_write(_user: *mut u8, _value: u64) {}

    unsafe extern "C" fn test_lo_read(_user: *mut u8) -> u64 {
        0
    }

    unsafe extern "C" fn test_lo_write(_user: *mut u8, _value: u64) {}

    fn default_callbacks() -> RuntimeCallbacks {
        RuntimeCallbacks {
            user: std::ptr::null_mut(),
            load_u8: test_load_u8,
            load_u16: test_load_u16,
            load_u32: test_load_u32,
            load_u64: test_load_u64,
            store_u8: test_store_u8,
            store_u16: test_store_u16,
            store_u32: test_store_u32,
            store_u64: test_store_u64,
            store_invalidate: test_store_invalidate,
            cop0_read: test_cop0_read,
            cop0_write: test_cop0_write,
            cop1_condition: test_cop1_condition,
            interp_exec: test_interp_exec,
            hi_read: test_hi_read,
            hi_write: test_hi_write,
            lo_read: test_lo_read,
            lo_write: test_lo_write,
            hi_ptr: std::ptr::null_mut(),
            lo_ptr: std::ptr::null_mut(),
            cop1_fcr31_ptr: std::ptr::null_mut(),
            fastmem_base: std::ptr::null_mut(),
            fastmem_phys_limit: 0,
            fastmem_phys_mask: 0,
        }
    }

    #[derive(Default)]
    struct CallbackState {
        mem: HashMap<u64, u8>,
        cop0: [u64; 32],
        cop1_condition: u64,
        hi: u64,
        lo: u64,
        store_invalidate_calls: u64,
    }

    unsafe extern "C" fn state_load_u8(user: *mut u8, vaddr: u64) -> u64 {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        u64::from(*state.mem.get(&vaddr).unwrap_or(&0))
    }

    unsafe extern "C" fn state_load_u32(user: *mut u8, vaddr: u64) -> u64 {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        let b0 = *state.mem.get(&vaddr).unwrap_or(&0);
        let b1 = *state.mem.get(&vaddr.wrapping_add(1)).unwrap_or(&0);
        let b2 = *state.mem.get(&vaddr.wrapping_add(2)).unwrap_or(&0);
        let b3 = *state.mem.get(&vaddr.wrapping_add(3)).unwrap_or(&0);
        u64::from(u32::from_be_bytes([b0, b1, b2, b3]))
    }

    unsafe extern "C" fn state_load_u16(user: *mut u8, vaddr: u64) -> u64 {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        let b0 = *state.mem.get(&vaddr).unwrap_or(&0);
        let b1 = *state.mem.get(&vaddr.wrapping_add(1)).unwrap_or(&0);
        u64::from(u16::from_be_bytes([b0, b1]))
    }

    unsafe extern "C" fn state_load_u64(user: *mut u8, vaddr: u64) -> u64 {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        let b0 = *state.mem.get(&vaddr).unwrap_or(&0);
        let b1 = *state.mem.get(&vaddr.wrapping_add(1)).unwrap_or(&0);
        let b2 = *state.mem.get(&vaddr.wrapping_add(2)).unwrap_or(&0);
        let b3 = *state.mem.get(&vaddr.wrapping_add(3)).unwrap_or(&0);
        let b4 = *state.mem.get(&vaddr.wrapping_add(4)).unwrap_or(&0);
        let b5 = *state.mem.get(&vaddr.wrapping_add(5)).unwrap_or(&0);
        let b6 = *state.mem.get(&vaddr.wrapping_add(6)).unwrap_or(&0);
        let b7 = *state.mem.get(&vaddr.wrapping_add(7)).unwrap_or(&0);
        u64::from_be_bytes([b0, b1, b2, b3, b4, b5, b6, b7])
    }

    unsafe extern "C" fn state_store_u8(user: *mut u8, vaddr: u64, value: u64) {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.mem.insert(vaddr, value as u8);
    }

    unsafe extern "C" fn state_store_u32(user: *mut u8, vaddr: u64, value: u64) {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        let bytes = (value as u32).to_be_bytes();
        state.mem.insert(vaddr, bytes[0]);
        state.mem.insert(vaddr.wrapping_add(1), bytes[1]);
        state.mem.insert(vaddr.wrapping_add(2), bytes[2]);
        state.mem.insert(vaddr.wrapping_add(3), bytes[3]);
    }

    unsafe extern "C" fn state_store_u16(user: *mut u8, vaddr: u64, value: u64) {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        let bytes = (value as u16).to_be_bytes();
        state.mem.insert(vaddr, bytes[0]);
        state.mem.insert(vaddr.wrapping_add(1), bytes[1]);
    }

    unsafe extern "C" fn state_store_u64(user: *mut u8, vaddr: u64, value: u64) {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        let bytes = value.to_be_bytes();
        state.mem.insert(vaddr, bytes[0]);
        state.mem.insert(vaddr.wrapping_add(1), bytes[1]);
        state.mem.insert(vaddr.wrapping_add(2), bytes[2]);
        state.mem.insert(vaddr.wrapping_add(3), bytes[3]);
        state.mem.insert(vaddr.wrapping_add(4), bytes[4]);
        state.mem.insert(vaddr.wrapping_add(5), bytes[5]);
        state.mem.insert(vaddr.wrapping_add(6), bytes[6]);
        state.mem.insert(vaddr.wrapping_add(7), bytes[7]);
    }

    unsafe extern "C" fn state_store_invalidate(user: *mut u8, _phys: u64, _len: u64) {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.store_invalidate_calls = state.store_invalidate_calls.wrapping_add(1);
    }

    unsafe extern "C" fn state_cop0_read(user: *mut u8, reg: u64) -> u64 {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.cop0[(reg as usize) & 0x1F]
    }

    unsafe extern "C" fn state_cop0_write(user: *mut u8, reg: u64, value: u64) {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.cop0[(reg as usize) & 0x1F] = value;
    }

    unsafe extern "C" fn state_cop1_condition(user: *mut u8) -> u64 {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.cop1_condition
    }

    unsafe extern "C" fn state_interp_exec(_user: *mut u8, _raw: u64, _current_pc: u64) {
        // Test programs avoid interpreter-delegated opcodes.
    }

    unsafe extern "C" fn state_hi_read(user: *mut u8) -> u64 {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.hi
    }

    unsafe extern "C" fn state_hi_write(user: *mut u8, value: u64) {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.hi = value;
    }

    unsafe extern "C" fn state_lo_read(user: *mut u8) -> u64 {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.lo
    }

    unsafe extern "C" fn state_lo_write(user: *mut u8, value: u64) {
        // SAFETY: callback user points to `CallbackState` for this test.
        let state = unsafe { &mut *(user as *mut CallbackState) };
        state.lo = value;
    }

    fn state_callbacks(state: &mut CallbackState) -> RuntimeCallbacks {
        RuntimeCallbacks {
            user: (state as *mut CallbackState).cast::<u8>(),
            load_u8: state_load_u8,
            load_u16: state_load_u16,
            load_u32: state_load_u32,
            load_u64: state_load_u64,
            store_u8: state_store_u8,
            store_u16: state_store_u16,
            store_u32: state_store_u32,
            store_u64: state_store_u64,
            store_invalidate: state_store_invalidate,
            cop0_read: state_cop0_read,
            cop0_write: state_cop0_write,
            cop1_condition: state_cop1_condition,
            interp_exec: state_interp_exec,
            hi_read: state_hi_read,
            hi_write: state_hi_write,
            lo_read: state_lo_read,
            lo_write: state_lo_write,
            hi_ptr: (&mut state.hi as *mut u64),
            lo_ptr: (&mut state.lo as *mut u64),
            cop1_fcr31_ptr: std::ptr::null_mut(),
            fastmem_base: std::ptr::null_mut(),
            fastmem_phys_limit: 0,
            fastmem_phys_mask: 0,
        }
    }

    #[test]
    fn cranelift_opt_level_parsing_accepts_aliases() {
        assert_eq!(
            CraneliftOptLevel::parse("none"),
            Some(CraneliftOptLevel::None)
        );
        assert_eq!(
            CraneliftOptLevel::parse("speed"),
            Some(CraneliftOptLevel::Speed)
        );
        assert_eq!(
            CraneliftOptLevel::parse("speed_and_size"),
            Some(CraneliftOptLevel::SpeedAndSize)
        );
        assert_eq!(
            CraneliftOptLevel::parse("speed-size"),
            Some(CraneliftOptLevel::SpeedAndSize)
        );
        assert_eq!(
            CraneliftOptLevel::parse("SPEEDSIZE"),
            Some(CraneliftOptLevel::SpeedAndSize)
        );
        assert_eq!(CraneliftOptLevel::parse("unknown"), None);
    }

    #[test]
    fn compiler_stops_before_unsupported_opcode() {
        let start = 0x1000u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),     // addiu t0, r0, 1
            (start + 4, 0x2409_0002), // addiu t1, r0, 2
            (start + 8, 0x4200_0018), // eret (unsupported)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 5,
                },
                &mut src,
            )
            .expect("compile");

        assert_eq!(block.instruction_count, 2);
        assert_eq!(block.end_phys, start + 8);
        assert!(!block.has_control_flow);
    }

    #[test]
    fn compiled_block_executes_alu_sequence() {
        let start = 0x2000u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0005),      // addiu t0, r0, 5
            (start + 4, 0x2409_0007),  // addiu t1, r0, 7
            (start + 8, 0x0109_5021),  // addu t2, t0, t1
            (start + 12, 0x3542_1234), // ori v0, t2, 0x1234
            (start + 16, 0x2400_0001), // addiu r0, r0, 1 (ignored write)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 5,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut callbacks = default_callbacks();
        let exec = block.execute(&mut gpr, &mut fpr, 0xFFFF_FFFF_8000_2000, &mut callbacks);

        assert_eq!(block.instruction_count, 5);
        assert!(!block.may_write_interrupt_state);
        assert_eq!(gpr[8], 5);
        assert_eq!(gpr[9], 7);
        assert_eq!(gpr[10], 12);
        assert_eq!(gpr[2], 0x123C);
        assert_eq!(gpr[0], 0);
        assert_eq!(exec.next_pc, 0xFFFF_FFFF_8000_2000 + 20);
        assert_eq!(exec.retired_instructions, 5);
    }

    #[test]
    fn compiled_block_executes_beq_with_delay_slot() {
        let start = 0x2200u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),      // addiu t0, r0, 1
            (start + 4, 0x2409_0001),  // addiu t1, r0, 1
            (start + 8, 0x1109_0002),  // beq t0, t1, +2
            (start + 12, 0x240A_0005), // addiu t2, r0, 5 (delay slot)
            (start + 16, 0x240A_0063), // addiu t2, r0, 99 (branch skips this)
            (start + 20, 0x4200_0018), // eret (sentinel: unsupported)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 8,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut callbacks = default_callbacks();
        let start_pc = 0xFFFF_FFFF_8000_2200u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 5);
        assert_eq!(gpr[10], 5);
        assert_eq!(exec.next_pc, start_pc + 20);
        assert_eq!(exec.retired_instructions, 4);
    }

    #[test]
    fn compiled_block_marks_backedge_loop_metadata() {
        let start = 0x22C0u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0003),      // addiu t0, r0, 3
            (start + 4, 0x2508_FFFF),  // addiu t0, t0, -1
            (start + 8, 0x1500_FFFE),  // bne t0, r0, -2 (backedge to start+4)
            (start + 12, 0x0000_0000), // nop (delay slot)
            (start + 16, 0x4200_0018), // eret (sentinel: unsupported)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 16,
                },
                &mut src,
            )
            .expect("compile");

        assert!(block.has_control_flow);
        assert!(block.has_backedge);
        assert!(!block.may_write_interrupt_state);
    }

    #[test]
    fn compiled_block_executes_beql_not_taken_skips_delay_slot() {
        let start = 0x2300u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),      // addiu t0, r0, 1
            (start + 4, 0x2409_0002),  // addiu t1, r0, 2
            (start + 8, 0x5109_0001),  // beql t0, t1, +1 (not taken)
            (start + 12, 0x240A_0005), // addiu t2, r0, 5 (delay slot, skipped)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 8,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut callbacks = default_callbacks();
        let start_pc = 0xFFFF_FFFF_8000_2300u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 4);
        assert_eq!(gpr[10], 0);
        assert_eq!(exec.next_pc, start_pc + 16);
        assert_eq!(exec.retired_instructions, 3);
    }

    #[test]
    fn compiled_block_executes_bc1t_with_delay_slot() {
        let start = 0x2340u32;
        let mut src = TestSource::with_words(&[
            (start, 0x4501_0002),      // bc1t +2
            (start + 4, 0x2408_0005),  // addiu t0, r0, 5 (delay slot)
            (start + 8, 0x2408_0063),  // addiu t0, r0, 99 (skipped)
            (start + 12, 0x2409_0007), // addiu t1, r0, 7 (target)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 8,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut state = CallbackState::default();
        state.cop1_condition = 1;
        let mut callbacks = state_callbacks(&mut state);
        let start_pc = 0xFFFF_FFFF_8000_2340u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(gpr[8], 5);
        assert_eq!(gpr[9], 7);
        assert_eq!(exec.next_pc, start_pc + 16);
        assert_eq!(exec.retired_instructions, 3);
    }

    #[test]
    fn compiled_block_executes_bc1fl_not_taken_skips_delay_slot() {
        let start = 0x2380u32;
        let mut src = TestSource::with_words(&[
            (start, 0x4502_0001),     // bc1fl +1 (not taken when condition=true)
            (start + 4, 0x2408_0005), // addiu t0, r0, 5 (delay slot, skipped)
            (start + 8, 0x2409_0007), // addiu t1, r0, 7
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 8,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut state = CallbackState::default();
        state.cop1_condition = 1;
        let mut callbacks = state_callbacks(&mut state);
        let start_pc = 0xFFFF_FFFF_8000_2380u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(gpr[8], 0);
        assert_eq!(gpr[9], 7);
        assert_eq!(exec.next_pc, start_pc + 12);
        assert_eq!(exec.retired_instructions, 2);
    }

    #[test]
    fn compiled_block_executes_jr_with_delay_slot() {
        let start = 0x2400u32;
        let mut src = TestSource::with_words(&[
            (start, 0x0160_0008),     // jr t3
            (start + 4, 0x2409_0007), // addiu t1, r0, 7 (delay slot)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 8,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut callbacks = default_callbacks();
        gpr[11] = 0xFFFF_FFFF_8000_2600;
        let start_pc = 0xFFFF_FFFF_8000_2400u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 2);
        assert_eq!(gpr[9], 7);
        assert_eq!(exec.next_pc, gpr[11]);
        assert_eq!(exec.retired_instructions, 2);
    }

    #[test]
    fn compiled_block_executes_jalr_with_delay_slot() {
        let start = 0x2480u32;
        let mut src = TestSource::with_words(&[
            (start, 0x0320_F809),     // jalr ra, t9
            (start + 4, 0x2408_0005), // addiu t0, r0, 5 (delay slot)
            (start + 8, 0x2408_0063), // addiu t0, r0, 99 (must not execute)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 8,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut callbacks = default_callbacks();
        let start_pc = 0xFFFF_FFFF_8000_2480u64;
        gpr[25] = start_pc + 0x40;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 2);
        assert_eq!(gpr[31], start_pc + 8);
        assert_eq!(gpr[8], 5);
        assert_eq!(exec.next_pc, start_pc + 0x40);
        assert_eq!(exec.retired_instructions, 2);
    }

    #[test]
    fn compiled_block_traces_through_direct_jump_target() {
        let start = 0x2800u32;
        let mut src = TestSource::with_words(&[
            (start, 0x0800_0A04),      // j 0x2810
            (start + 4, 0x2408_0005),  // addiu t0, r0, 5 (delay slot)
            (start + 16, 0x2409_0007), // addiu t1, r0, 7 (jump target)
            (start + 20, 0x4200_0018), // eret (sentinel: unsupported)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 8,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut callbacks = default_callbacks();
        let start_pc = 0xFFFF_FFFF_8000_2800u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 3);
        assert_eq!(gpr[8], 5);
        assert_eq!(gpr[9], 7);
        assert_eq!(exec.next_pc, start_pc + 20);
        assert_eq!(exec.retired_instructions, 3);
    }

    #[test]
    fn compiled_block_traces_through_jal_target_and_return() {
        let start = 0x2840u32;
        let mut src = TestSource::with_words(&[
            (start, 0x0C00_0A14),      // jal 0x2850
            (start + 4, 0x2408_0005),  // addiu t0, r0, 5 (delay slot)
            (start + 16, 0x2409_0007), // addiu t1, r0, 7 (jal target)
            (start + 20, 0x03E0_0008), // jr ra
            (start + 24, 0x240A_0009), // addiu t2, r0, 9 (delay slot)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 16,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut callbacks = default_callbacks();
        let start_pc = 0xFFFF_FFFF_8000_2840u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 5);
        assert_eq!(gpr[31], start_pc + 8);
        assert_eq!(gpr[8], 5);
        assert_eq!(gpr[9], 7);
        assert_eq!(gpr[10], 9);
        assert_eq!(exec.next_pc, start_pc + 8);
        assert_eq!(exec.retired_instructions, 5);
    }

    #[test]
    fn compiled_block_executes_hilo_mul_div_sequence() {
        let start = 0x24C0u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0006),      // addiu t0, r0, 6
            (start + 4, 0x2409_0007),  // addiu t1, r0, 7
            (start + 8, 0x0109_0019),  // multu t0, t1
            (start + 12, 0x0000_8012), // mflo s0
            (start + 16, 0x0000_8810), // mfhi s1
            (start + 20, 0x0100_0013), // mtlo t0
            (start + 24, 0x0120_0011), // mthi t1
            (start + 28, 0x0000_9012), // mflo s2
            (start + 32, 0x0000_9810), // mfhi s3
            (start + 36, 0x0128_001B), // divu t1, t0
            (start + 40, 0x0000_A012), // mflo s4
            (start + 44, 0x0000_A810), // mfhi s5
            (start + 48, 0x4200_0018), // eret (sentinel: unsupported)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 20,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut state = CallbackState::default();
        let mut callbacks = state_callbacks(&mut state);
        let start_pc = 0xFFFF_FFFF_8000_24C0u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert_eq!(exec.next_pc, start_pc + 48);
        assert_eq!(exec.retired_instructions, 12);
        assert_eq!(gpr[16], 42);
        assert_eq!(gpr[17], 0);
        assert_eq!(gpr[18], 6);
        assert_eq!(gpr[19], 7);
        assert_eq!(gpr[20], 1);
        assert_eq!(gpr[21], 1);
        assert_eq!(state.lo, 1);
        assert_eq!(state.hi, 1);
    }

    #[test]
    fn compiled_block_executes_memory_and_cop0_ops_via_callbacks() {
        let start = 0x2500u32;
        let mut src = TestSource::with_words(&[
            (start, 0x3C0C_8000),      // lui t4, 0x8000
            (start + 4, 0x2408_0012),  // addiu t0, r0, 0x12
            (start + 8, 0xA188_0103),  // sb t0, 0x103(t4)
            (start + 12, 0x9189_0103), // lbu t1, 0x103(t4)
            (start + 16, 0x240A_0034), // addiu t2, r0, 0x34
            (start + 20, 0xAD8A_0100), // sw t2, 0x100(t4)
            (start + 24, 0x8D8B_0100), // lw t3, 0x100(t4)
            (start + 28, 0x408B_6000), // mtc0 t3, $12
            (start + 32, 0x4002_6000), // mfc0 v0, $12
            (start + 36, 0x4200_0018), // eret (sentinel: unsupported)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let mem_block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 16,
                },
                &mut src,
            )
            .expect("compile");
        let cop0_block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start + 28,
                    max_instructions: 16,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut state = CallbackState::default();
        let mut callbacks = state_callbacks(&mut state);
        let start_pc = 0xFFFF_FFFF_8000_2500u64;
        let mem_exec = mem_block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);
        let cop0_exec = cop0_block.execute(&mut gpr, &mut fpr, start_pc + 28, &mut callbacks);

        assert_eq!(mem_exec.next_pc, start_pc + 28);
        assert_eq!(mem_exec.retired_instructions, 7);
        assert!(!mem_block.may_write_interrupt_state);
        assert_eq!(cop0_exec.next_pc, start_pc + 36);
        assert_eq!(cop0_exec.retired_instructions, 2);
        assert!(cop0_block.may_write_interrupt_state);
        assert_eq!(gpr[9], 0x12);
        assert_eq!(gpr[11], 0x34);
        assert_eq!(gpr[2], 0x34);
        assert_eq!(state.cop0[12], 0x34);
    }

    #[test]
    fn compiled_block_uses_fastmem_for_integer_load_store() {
        let start = 0x2580u32;
        let mut src = TestSource::with_words(&[
            (start, 0x3C0C_8000),      // lui t4, 0x8000
            (start + 4, 0x8D88_0000),  // lw t0, 0(t4)
            (start + 8, 0xA188_0007),  // sb t0, 7(t4)
            (start + 12, 0x9189_0007), // lbu t1, 7(t4)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 4,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut state = CallbackState::default();
        let mut callbacks = state_callbacks(&mut state);
        let mut fastmem = vec![0u8; 8 * 1024 * 1024];
        fastmem[0..4].copy_from_slice(&0x1122_3344u32.to_be_bytes());
        callbacks.fastmem_base = fastmem.as_mut_ptr();
        callbacks.fastmem_phys_limit = 0x03F0_0000;
        callbacks.fastmem_phys_mask = 0x007F_FFFF;

        let start_pc = 0xFFFF_FFFF_8000_2580u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert_eq!(exec.next_pc, start_pc + 16);
        assert_eq!(exec.retired_instructions, 4);
        assert_eq!(gpr[8], 0x1122_3344);
        assert_eq!(gpr[9], 0x44);
        assert_eq!(fastmem[7], 0x44);
        assert_eq!(state.store_invalidate_calls, 1);
        assert!(state.mem.is_empty(), "slow callback path should not run");
    }

    #[test]
    fn compiled_block_executes_cop1_memory_ops_via_fastmem() {
        let start = 0x25C0u32;
        let mut src = TestSource::with_words(&[
            (start, 0x3C0C_8000),      // lui t4, 0x8000
            (start + 4, 0xC582_0000),  // lwc1 f2, 0(t4)
            (start + 8, 0xC584_0004),  // lwc1 f4, 4(t4)
            (start + 12, 0x4604_1180), // add.s f6, f2, f4
            (start + 16, 0xE586_0008), // swc1 f6, 8(t4)
            (start + 20, 0xD588_0010), // ldc1 f8, 16(t4)
            (start + 24, 0xF588_0018), // sdc1 f8, 24(t4)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 7,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut state = CallbackState::default();
        let mut callbacks = state_callbacks(&mut state);
        let mut fastmem = vec![0u8; 8 * 1024 * 1024];
        fastmem[0..4].copy_from_slice(&0x3FC0_0000u32.to_be_bytes()); // 1.5f32
        fastmem[4..8].copy_from_slice(&0x4010_0000u32.to_be_bytes()); // 2.25f32
        fastmem[16..24].copy_from_slice(&0x1122_3344_5566_7788u64.to_be_bytes());
        callbacks.fastmem_base = fastmem.as_mut_ptr();
        callbacks.fastmem_phys_limit = 0x03F0_0000;
        callbacks.fastmem_phys_mask = 0x007F_FFFF;

        let start_pc = 0xFFFF_FFFF_8000_25C0u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert_eq!(exec.next_pc, start_pc + 28);
        assert_eq!(exec.retired_instructions, 7);
        assert_eq!(
            u32::from_be_bytes([fastmem[8], fastmem[9], fastmem[10], fastmem[11]]),
            0x4070_0000
        );
        assert_eq!(
            u64::from_be_bytes([
                fastmem[24],
                fastmem[25],
                fastmem[26],
                fastmem[27],
                fastmem[28],
                fastmem[29],
                fastmem[30],
                fastmem[31]
            ]),
            0x1122_3344_5566_7788
        );
        assert_eq!(fpr[8], 0x5566_7788);
        assert_eq!(fpr[9], 0x1122_3344);
        assert_eq!(state.store_invalidate_calls, 2);
        assert!(state.mem.is_empty(), "slow callback path should not run");
    }

    #[test]
    fn compiled_block_mfc0_sign_extends_low_32_bits() {
        let start = 0x2600u32;
        let mut src = TestSource::with_words(&[
            (start, 0x4008_6800),     // mfc0 t0, $13
            (start + 4, 0x4200_0018), // eret (sentinel: unsupported)
        ]);

        let mut compiler = CraneliftCompiler::default();
        let block = compiler
            .compile(
                &CompileRequest {
                    start_phys: start,
                    max_instructions: 4,
                },
                &mut src,
            )
            .expect("compile");

        let mut gpr = [0u64; 32];
        let mut fpr = [0u64; 32];
        let mut state = CallbackState::default();
        state.cop0[13] = 0x0000_0000_8000_0001;
        let mut callbacks = state_callbacks(&mut state);
        let start_pc = 0xFFFF_FFFF_8000_2600u64;
        let exec = block.execute(&mut gpr, &mut fpr, start_pc, &mut callbacks);

        assert_eq!(exec.next_pc, start_pc + 4);
        assert_eq!(exec.retired_instructions, 1);
        assert_eq!(gpr[8], 0xFFFF_FFFF_8000_0001);
    }

    #[test]
    fn recompiler_caches_and_invalidates() {
        let start = 0x3000u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),
            (start + 4, 0x2409_0002),
            (start + 8, 0x0000_0000),
        ]);
        let mut rc = Recompiler::new(
            Box::<CraneliftCompiler>::default(),
            RecompilerConfig {
                max_block_instructions: 3,
            },
        );

        assert_eq!(rc.ensure_compiled(start, &mut src), EnsureResult::Compiled);
        assert_eq!(rc.ensure_compiled(start, &mut src), EnsureResult::CacheHit);
        assert_eq!(rc.cache_len(), 1);

        rc.invalidate_range(start + 4, 4);
        assert_eq!(rc.cache_len(), 0);
        assert_eq!(rc.stats().invalidated_blocks, 1);
    }

    #[test]
    fn invalidate_range_drops_block_spanning_pages() {
        let start = 0x1FFCu32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),      // addiu t0, r0, 1
            (start + 4, 0x2508_0001),  // addiu t0, t0, 1
            (start + 8, 0x2508_0001),  // addiu t0, t0, 1
            (start + 12, 0x2508_0001), // addiu t0, t0, 1
        ]);
        let mut rc = Recompiler::new(
            Box::<CraneliftCompiler>::default(),
            RecompilerConfig {
                max_block_instructions: 4,
            },
        );

        assert_eq!(rc.ensure_compiled(start, &mut src), EnsureResult::Compiled);
        assert_eq!(rc.cache_len(), 1);

        // Invalidate one word on the second page touched by this block.
        rc.invalidate_range(0x2000, 4);
        assert_eq!(rc.cache_len(), 0);
        assert_eq!(rc.stats().invalidated_blocks, 1);
    }

    #[test]
    fn recompile_with_max_promotes_cached_block() {
        let start = 0x3400u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),      // addiu t0, r0, 1
            (start + 4, 0x2508_0001),  // addiu t0, t0, 1
            (start + 8, 0x2508_0001),  // addiu t0, t0, 1
            (start + 12, 0x2508_0001), // addiu t0, t0, 1
            (start + 16, 0x4200_0018), // eret (sentinel: unsupported)
        ]);
        let mut rc = Recompiler::new(
            Box::<CraneliftCompiler>::default(),
            RecompilerConfig {
                max_block_instructions: 8,
            },
        );

        assert_eq!(
            rc.ensure_compiled_with_max(start, 1, &mut src),
            EnsureResult::Compiled
        );
        let tier1 = rc.lookup(start).expect("tier1 block");
        assert_eq!(tier1.max_retired_instructions, 1);

        assert_eq!(
            rc.recompile_with_max(start, 4, &mut src),
            EnsureResult::Compiled
        );
        let promoted = rc.lookup(start).expect("promoted block");
        assert!(promoted.max_retired_instructions >= 4);

        assert_eq!(
            rc.recompile_with_max(start, 2, &mut src),
            EnsureResult::CacheHit
        );
        assert_eq!(rc.stats().blocks_compiled, 2);
    }

    #[test]
    fn recompile_with_max_keeps_existing_block_on_failure() {
        let start = 0x3500u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),     // addiu t0, r0, 1
            (start + 4, 0x2508_0001), // addiu t0, t0, 1
        ]);
        let mut rc = Recompiler::new(
            Box::<CraneliftCompiler>::default(),
            RecompilerConfig {
                max_block_instructions: 8,
            },
        );

        assert_eq!(
            rc.ensure_compiled_with_max(start, 2, &mut src),
            EnsureResult::Compiled
        );
        let before = rc.lookup(start).expect("baseline block");
        assert_eq!(before.max_retired_instructions, 2);

        // Simulate code changing to an unsupported opcode before promotion.
        src.words.insert(start, 0x4200_0018);
        assert_eq!(
            rc.recompile_with_max(start, 8, &mut src),
            EnsureResult::CompileFailed
        );
        let after = rc
            .lookup(start)
            .expect("cached block after failed recompile");
        assert_eq!(after.max_retired_instructions, 2);
        assert_eq!(rc.stats().blocks_compiled, 1);
        assert_eq!(rc.stats().compile_failures, 1);
    }

    #[test]
    fn compile_failure_is_recorded_and_cached() {
        let start = 0x4000u32;
        let mut src = TestSource::with_words(&[(start, 0x4200_0018)]); // eret (unsupported)
        let mut rc = Recompiler::new(
            Box::<CraneliftCompiler>::default(),
            RecompilerConfig::default(),
        );

        let res = rc.ensure_compiled(start, &mut src);
        assert_eq!(res, EnsureResult::CompileFailed);
        assert_eq!(rc.stats().compile_failures, 1);
        assert_eq!(rc.failed_cache_len(), 1);
        assert!(matches!(
            rc.last_error(),
            Some(CompileError::UnsupportedOpcode {
                phys_addr,
                opcode: 0x4200_0018
            }) if *phys_addr == start
        ));

        let res = rc.ensure_compiled(start, &mut src);
        assert_eq!(res, EnsureResult::CacheHit);
        assert_eq!(rc.stats().compile_failures, 1);
        assert_eq!(rc.stats().failed_cache_hits, 1);

        rc.invalidate_range(start, 4);
        assert_eq!(rc.failed_cache_len(), 0);
    }

    #[test]
    fn blacklist_start_moves_cached_block_to_failed_cache() {
        let start = 0x4100u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),     // addiu t0, r0, 1
            (start + 4, 0x2508_0001), // addiu t0, t0, 1
        ]);
        let mut rc = Recompiler::new(
            Box::<CraneliftCompiler>::default(),
            RecompilerConfig {
                max_block_instructions: 4,
            },
        );

        assert_eq!(rc.ensure_compiled(start, &mut src), EnsureResult::Compiled);
        assert_eq!(rc.cache_len(), 1);
        assert_eq!(rc.failed_cache_len(), 0);

        assert!(rc.blacklist_start(start));
        assert_eq!(rc.cache_len(), 0);
        assert_eq!(rc.failed_cache_len(), 1);
        assert!(rc.is_failed_cached(start));

        assert_eq!(rc.ensure_compiled(start, &mut src), EnsureResult::CacheHit);
        assert_eq!(rc.stats().failed_cache_hits, 1);
    }
}
