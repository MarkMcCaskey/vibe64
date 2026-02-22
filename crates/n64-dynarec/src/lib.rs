//! Dynarec infrastructure shared by emulator cores.
//!
//! This crate keeps backend/compiler concerns separate from core emulation logic.
//! The first backend is Cranelift.

use std::collections::{HashMap, HashSet};

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, AbiParam, FuncRef, InstBuilder, MemFlags, Value};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
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
    pub cop0_read: unsafe extern "C" fn(*mut u8, u64) -> u64,
    pub cop0_write: unsafe extern "C" fn(*mut u8, u64, u64),
    pub hi_read: unsafe extern "C" fn(*mut u8) -> u64,
    pub hi_write: unsafe extern "C" fn(*mut u8, u64),
    pub lo_read: unsafe extern "C" fn(*mut u8) -> u64,
    pub lo_write: unsafe extern "C" fn(*mut u8, u64),
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

type JitBlockFn = unsafe extern "C" fn(*mut u64, u64, *mut u8, *mut u32) -> u64;

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
    /// True if the block ended on a control-transfer boundary.
    pub has_control_flow: bool,
    entry: BlockEntry,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BlockExecution {
    pub next_pc: u64,
    pub retired_instructions: u32,
}

impl CompiledBlock {
    /// Execute this compiled block against the GPR file.
    pub fn execute(
        &self,
        gpr: &mut [u64; 32],
        start_pc: u64,
        callbacks: *mut RuntimeCallbacks,
    ) -> BlockExecution {
        let mut retired = 0u32;
        // SAFETY: compiled blocks are generated with signature
        // `extern "C" fn(*mut u64, u64, *mut u8, *mut u32) -> u64`,
        // `gpr` points to 32 contiguous u64s, and callback pointer comes from
        // the caller.
        let next_pc = unsafe {
            (self.entry.as_fn())(
                gpr.as_mut_ptr(),
                start_pc,
                callbacks.cast::<u8>(),
                (&mut retired as *mut u32).cast::<u32>(),
            )
        };
        BlockExecution {
            next_pc,
            retired_instructions: retired,
        }
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

/// Core recompiler pipeline: cache + backend compiler.
pub struct Recompiler {
    compiler: Box<dyn BlockCompiler>,
    config: RecompilerConfig,
    cache: HashMap<u32, CompiledBlock>,
    failed_cache: HashSet<u32>,
    stats: RecompilerStats,
    last_error: Option<CompileError>,
}

impl Recompiler {
    pub fn new(compiler: Box<dyn BlockCompiler>, config: RecompilerConfig) -> Self {
        Self {
            compiler,
            config,
            cache: HashMap::new(),
            failed_cache: HashSet::new(),
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

    pub fn is_failed_cached(&self, start_phys: u32) -> bool {
        self.failed_cache.contains(&start_phys)
    }

    pub fn ensure_compiled(
        &mut self,
        start_phys: u32,
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
            max_instructions: self.config.max_block_instructions.max(1),
        };
        match self.compiler.compile(&request, source) {
            Ok(block) => {
                self.stats.blocks_compiled += 1;
                self.failed_cache.remove(&start_phys);
                self.cache.insert(start_phys, block);
                EnsureResult::Compiled
            }
            Err(err) => {
                self.stats.compile_failures += 1;
                self.failed_cache.insert(start_phys);
                self.last_error = Some(err);
                EnsureResult::CompileFailed
            }
        }
    }

    pub fn invalidate_range(&mut self, start_phys: u32, len: u32) {
        if len == 0 {
            return;
        }
        let end_phys = start_phys.saturating_add(len);
        let keys_to_drop: Vec<u32> = self
            .cache
            .iter()
            .filter_map(|(key, block)| {
                let overlap = block.start_phys < end_phys && start_phys < block.end_phys;
                if overlap {
                    Some(*key)
                } else {
                    None
                }
            })
            .collect();

        for key in keys_to_drop {
            if self.cache.remove(&key).is_some() {
                self.stats.invalidated_blocks += 1;
            }
        }

        let failed_to_drop: Vec<u32> = self
            .failed_cache
            .iter()
            .copied()
            .filter(|addr| *addr >= start_phys && *addr < end_phys)
            .collect();
        for key in failed_to_drop {
            if self.failed_cache.remove(&key) {
                self.stats.invalidated_blocks += 1;
            }
        }

        self.compiler.invalidate_range(start_phys, len);
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
    Mfc0 { rt: u8, rd: u8 },
    Dmfc0 { rt: u8, rd: u8 },
    Mtc0 { rt: u8, rd: u8 },
    Dmtc0 { rt: u8, rd: u8 },
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
    J { target: u32 },
    Jal { target: u32 },
    Jalr { rs: u8, rd: u8 },
    Jr { rs: u8 },
}

#[derive(Debug, Clone, Copy)]
enum TraceStep {
    Op(Op),
    Branch {
        branch: BranchTerminator,
        delay_op: Op,
        continue_fallthrough: bool,
    },
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
        0x00 if funct == 0x08 => Some(BranchTerminator::Jr { rs }),
        0x00 if funct == 0x09 => Some(BranchTerminator::Jalr {
            rs,
            rd: ((raw >> 11) & 0x1F) as u8,
        }),
        _ => None,
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
        0x2F => Some(Op::Cache),
        0x01 | 0x02 | 0x03 | 0x04 | 0x05 | 0x06 | 0x07 | 0x14 | 0x15 | 0x16 | 0x17 => None,
        _ => None,
    }
}

fn iconst_u64(builder: &mut FunctionBuilder<'_>, value: u64) -> Value {
    builder
        .ins()
        .iconst(types::I64, i64::from_ne_bytes(value.to_ne_bytes()))
}

fn load_gpr(builder: &mut FunctionBuilder<'_>, gpr_ptr: Value, reg: u8, flags: MemFlags) -> Value {
    if reg == 0 {
        builder.ins().iconst(types::I64, 0)
    } else {
        builder
            .ins()
            .load(types::I64, flags, gpr_ptr, i32::from(reg) * 8)
    }
}

fn store_gpr(
    builder: &mut FunctionBuilder<'_>,
    gpr_ptr: Value,
    reg: u8,
    value: Value,
    flags: MemFlags,
) {
    if reg != 0 {
        builder
            .ins()
            .store(flags, value, gpr_ptr, i32::from(reg) * 8);
    }
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
    cop0_read: FuncRef,
    cop0_write: FuncRef,
    hi_read: FuncRef,
    hi_write: FuncRef,
    lo_read: FuncRef,
    lo_write: FuncRef,
}

fn read_hi(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
) -> Value {
    let call = builder.ins().call(helpers.hi_read, &[callbacks_ptr]);
    builder.inst_results(call)[0]
}

fn read_lo(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
) -> Value {
    let call = builder.ins().call(helpers.lo_read, &[callbacks_ptr]);
    builder.inst_results(call)[0]
}

fn write_hi(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    value: Value,
) {
    builder
        .ins()
        .call(helpers.hi_write, &[callbacks_ptr, value]);
}

fn write_lo(
    builder: &mut FunctionBuilder<'_>,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    value: Value,
) {
    builder
        .ins()
        .call(helpers.lo_write, &[callbacks_ptr, value]);
}

fn emit_op(
    builder: &mut FunctionBuilder<'_>,
    gpr_ptr: Value,
    callbacks_ptr: Value,
    helpers: ImportedFuncRefs,
    flags: MemFlags,
    op: Op,
) {
    match op {
        Op::Addi { rs, rt, imm } | Op::Addiu { rs, rt, imm } => {
            let rs64 = load_gpr(builder, gpr_ptr, rs, flags);
            let rs32 = builder.ins().ireduce(types::I32, rs64);
            let imm32 = builder.ins().iconst(types::I32, i64::from(imm));
            let sum32 = builder.ins().iadd(rs32, imm32);
            let sum64 = builder.ins().sextend(types::I64, sum32);
            store_gpr(builder, gpr_ptr, rt, sum64, flags);
        }
        Op::Daddiu { rs, rt, imm } => {
            let rs64 = load_gpr(builder, gpr_ptr, rs, flags);
            let imm64 = iconst_u64(builder, imm as i64 as u64);
            let sum64 = builder.ins().iadd(rs64, imm64);
            store_gpr(builder, gpr_ptr, rt, sum64, flags);
        }
        Op::Slti { rs, rt, imm } => {
            let rs64 = load_gpr(builder, gpr_ptr, rs, flags);
            let imm64 = builder.ins().iconst(types::I64, i64::from(imm));
            let cmp = builder.ins().icmp(IntCC::SignedLessThan, rs64, imm64);
            let result = builder.ins().uextend(types::I64, cmp);
            store_gpr(builder, gpr_ptr, rt, result, flags);
        }
        Op::Sltiu { rs, rt, imm } => {
            let rs64 = load_gpr(builder, gpr_ptr, rs, flags);
            let imm64 = iconst_u64(builder, imm as i64 as u64);
            let cmp = builder.ins().icmp(IntCC::UnsignedLessThan, rs64, imm64);
            let result = builder.ins().uextend(types::I64, cmp);
            store_gpr(builder, gpr_ptr, rt, result, flags);
        }
        Op::Andi { rs, rt, imm } => {
            let rs64 = load_gpr(builder, gpr_ptr, rs, flags);
            let imm64 = iconst_u64(builder, u64::from(imm));
            let result = builder.ins().band(rs64, imm64);
            store_gpr(builder, gpr_ptr, rt, result, flags);
        }
        Op::Ori { rs, rt, imm } => {
            let rs64 = load_gpr(builder, gpr_ptr, rs, flags);
            let imm64 = iconst_u64(builder, u64::from(imm));
            let result = builder.ins().bor(rs64, imm64);
            store_gpr(builder, gpr_ptr, rt, result, flags);
        }
        Op::Xori { rs, rt, imm } => {
            let rs64 = load_gpr(builder, gpr_ptr, rs, flags);
            let imm64 = iconst_u64(builder, u64::from(imm));
            let result = builder.ins().bxor(rs64, imm64);
            store_gpr(builder, gpr_ptr, rt, result, flags);
        }
        Op::Lui { rt, imm } => {
            let val = ((imm as i32) as i64) << 16;
            let value = builder.ins().iconst(types::I64, val);
            store_gpr(builder, gpr_ptr, rt, value, flags);
        }
        Op::Addu { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let lhs32 = builder.ins().ireduce(types::I32, lhs);
            let rhs32 = builder.ins().ireduce(types::I32, rhs);
            let sum32 = builder.ins().iadd(lhs32, rhs32);
            let result = builder.ins().sextend(types::I64, sum32);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Subu { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let lhs32 = builder.ins().ireduce(types::I32, lhs);
            let rhs32 = builder.ins().ireduce(types::I32, rhs);
            let diff32 = builder.ins().isub(lhs32, rhs32);
            let result = builder.ins().sextend(types::I64, diff32);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::And { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let result = builder.ins().band(lhs, rhs);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Or { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let result = builder.ins().bor(lhs, rhs);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Xor { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let result = builder.ins().bxor(lhs, rhs);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Nor { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let or_val = builder.ins().bor(lhs, rhs);
            let result = builder.ins().bnot(or_val);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Slt { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let cmp = builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs);
            let result = builder.ins().uextend(types::I64, cmp);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Sltu { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let cmp = builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs);
            let result = builder.ins().uextend(types::I64, cmp);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Daddu { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let result = builder.ins().iadd(lhs, rhs);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Dsubu { rs, rt, rd } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let result = builder.ins().isub(lhs, rhs);
            store_gpr(builder, gpr_ptr, rd, result, flags);
        }
        Op::Sll { rt, rd, sa } => {
            let rt64 = load_gpr(builder, gpr_ptr, rt, flags);
            let rt32 = builder.ins().ireduce(types::I32, rt64);
            let sh = builder.ins().iconst(types::I32, i64::from(sa));
            let result32 = builder.ins().ishl(rt32, sh);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, gpr_ptr, rd, result64, flags);
        }
        Op::Srl { rt, rd, sa } => {
            let rt64 = load_gpr(builder, gpr_ptr, rt, flags);
            let rt32 = builder.ins().ireduce(types::I32, rt64);
            let sh = builder.ins().iconst(types::I32, i64::from(sa));
            let result32 = builder.ins().ushr(rt32, sh);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, gpr_ptr, rd, result64, flags);
        }
        Op::Sra { rt, rd, sa } => {
            let rt64 = load_gpr(builder, gpr_ptr, rt, flags);
            let sh = builder.ins().iconst(types::I64, i64::from(sa));
            let shifted64 = builder.ins().ushr(rt64, sh);
            let result32 = builder.ins().ireduce(types::I32, shifted64);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, gpr_ptr, rd, result64, flags);
        }
        Op::Sllv { rs, rt, rd } => {
            let rs = load_gpr(builder, gpr_ptr, rs, flags);
            let sh_mask = iconst_u64(builder, 0x1F);
            let sh_masked = builder.ins().band(rs, sh_mask);
            let sh = builder.ins().ireduce(types::I32, sh_masked);
            let rt64 = load_gpr(builder, gpr_ptr, rt, flags);
            let rt32 = builder.ins().ireduce(types::I32, rt64);
            let result32 = builder.ins().ishl(rt32, sh);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, gpr_ptr, rd, result64, flags);
        }
        Op::Srlv { rs, rt, rd } => {
            let rs = load_gpr(builder, gpr_ptr, rs, flags);
            let sh_mask = iconst_u64(builder, 0x1F);
            let sh_masked = builder.ins().band(rs, sh_mask);
            let sh = builder.ins().ireduce(types::I32, sh_masked);
            let rt64 = load_gpr(builder, gpr_ptr, rt, flags);
            let rt32 = builder.ins().ireduce(types::I32, rt64);
            let result32 = builder.ins().ushr(rt32, sh);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, gpr_ptr, rd, result64, flags);
        }
        Op::Srav { rs, rt, rd } => {
            let rs = load_gpr(builder, gpr_ptr, rs, flags);
            let sh_mask = iconst_u64(builder, 0x1F);
            let sh = builder.ins().band(rs, sh_mask);
            let rt64 = load_gpr(builder, gpr_ptr, rt, flags);
            let shifted64 = builder.ins().ushr(rt64, sh);
            let result32 = builder.ins().ireduce(types::I32, shifted64);
            let result64 = builder.ins().sextend(types::I64, result32);
            store_gpr(builder, gpr_ptr, rd, result64, flags);
        }
        Op::Mfhi { rd } => {
            let value = read_hi(builder, callbacks_ptr, helpers);
            store_gpr(builder, gpr_ptr, rd, value, flags);
        }
        Op::Mthi { rs } => {
            let value = load_gpr(builder, gpr_ptr, rs, flags);
            write_hi(builder, callbacks_ptr, helpers, value);
        }
        Op::Mflo { rd } => {
            let value = read_lo(builder, callbacks_ptr, helpers);
            store_gpr(builder, gpr_ptr, rd, value, flags);
        }
        Op::Mtlo { rs } => {
            let value = load_gpr(builder, gpr_ptr, rs, flags);
            write_lo(builder, callbacks_ptr, helpers, value);
        }
        Op::Mult { rs, rt } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
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
            write_lo(builder, callbacks_ptr, helpers, lo64);
            write_hi(builder, callbacks_ptr, helpers, hi64);
        }
        Op::Multu { rs, rt } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
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
            write_lo(builder, callbacks_ptr, helpers, lo64);
            write_hi(builder, callbacks_ptr, helpers, hi64);
        }
        Op::Div { rs, rt } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
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
            let old_lo = read_lo(builder, callbacks_ptr, helpers);
            let old_hi = read_hi(builder, callbacks_ptr, helpers);
            let lo = builder.ins().select(skip, old_lo, quot64);
            let hi = builder.ins().select(skip, old_hi, rem64);
            write_lo(builder, callbacks_ptr, helpers, lo);
            write_hi(builder, callbacks_ptr, helpers, hi);
        }
        Op::Divu { rs, rt } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
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
            let old_lo = read_lo(builder, callbacks_ptr, helpers);
            let old_hi = read_hi(builder, callbacks_ptr, helpers);
            let lo = builder.ins().select(is_zero, old_lo, quot64);
            let hi = builder.ins().select(is_zero, old_hi, rem64);
            write_lo(builder, callbacks_ptr, helpers, lo);
            write_hi(builder, callbacks_ptr, helpers, hi);
        }
        Op::Dmult { rs, rt } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let lo = builder.ins().imul(lhs, rhs);
            let hi = builder.ins().smulhi(lhs, rhs);
            write_lo(builder, callbacks_ptr, helpers, lo);
            write_hi(builder, callbacks_ptr, helpers, hi);
        }
        Op::Dmultu { rs, rt } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let lo = builder.ins().imul(lhs, rhs);
            let hi = builder.ins().umulhi(lhs, rhs);
            write_lo(builder, callbacks_ptr, helpers, lo);
            write_hi(builder, callbacks_ptr, helpers, hi);
        }
        Op::Ddiv { rs, rt } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
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
            let old_lo = read_lo(builder, callbacks_ptr, helpers);
            let old_hi = read_hi(builder, callbacks_ptr, helpers);
            let lo = builder.ins().select(skip, old_lo, quot);
            let hi = builder.ins().select(skip, old_hi, rem);
            write_lo(builder, callbacks_ptr, helpers, lo);
            write_hi(builder, callbacks_ptr, helpers, hi);
        }
        Op::Ddivu { rs, rt } => {
            let lhs = load_gpr(builder, gpr_ptr, rs, flags);
            let rhs = load_gpr(builder, gpr_ptr, rt, flags);
            let zero = iconst_u64(builder, 0);
            let is_zero = builder.ins().icmp(IntCC::Equal, rhs, zero);
            let one = iconst_u64(builder, 1);
            let safe_rhs = builder.ins().select(is_zero, one, rhs);
            let quot = builder.ins().udiv(lhs, safe_rhs);
            let rem = builder.ins().urem(lhs, safe_rhs);
            let old_lo = read_lo(builder, callbacks_ptr, helpers);
            let old_hi = read_hi(builder, callbacks_ptr, helpers);
            let lo = builder.ins().select(is_zero, old_lo, quot);
            let hi = builder.ins().select(is_zero, old_hi, rem);
            write_lo(builder, callbacks_ptr, helpers, lo);
            write_hi(builder, callbacks_ptr, helpers, hi);
        }
        Op::Lb { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let call = builder.ins().call(helpers.load_u8, &[callbacks_ptr, vaddr]);
            let loaded = builder.inst_results(call)[0];
            let loaded8 = builder.ins().ireduce(types::I8, loaded);
            let loaded64 = builder.ins().sextend(types::I64, loaded8);
            store_gpr(builder, gpr_ptr, rt, loaded64, flags);
        }
        Op::Lh { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let call = builder
                .ins()
                .call(helpers.load_u16, &[callbacks_ptr, vaddr]);
            let loaded = builder.inst_results(call)[0];
            let loaded16 = builder.ins().ireduce(types::I16, loaded);
            let loaded64 = builder.ins().sextend(types::I64, loaded16);
            store_gpr(builder, gpr_ptr, rt, loaded64, flags);
        }
        Op::Lhu { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let call = builder
                .ins()
                .call(helpers.load_u16, &[callbacks_ptr, vaddr]);
            let loaded = builder.inst_results(call)[0];
            let loaded16 = builder.ins().ireduce(types::I16, loaded);
            let loaded64 = builder.ins().uextend(types::I64, loaded16);
            store_gpr(builder, gpr_ptr, rt, loaded64, flags);
        }
        Op::Lw { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let call = builder
                .ins()
                .call(helpers.load_u32, &[callbacks_ptr, vaddr]);
            let loaded = builder.inst_results(call)[0];
            let loaded32 = builder.ins().ireduce(types::I32, loaded);
            let loaded64 = builder.ins().sextend(types::I64, loaded32);
            store_gpr(builder, gpr_ptr, rt, loaded64, flags);
        }
        Op::Lwu { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let call = builder
                .ins()
                .call(helpers.load_u32, &[callbacks_ptr, vaddr]);
            let loaded = builder.inst_results(call)[0];
            let loaded32 = builder.ins().ireduce(types::I32, loaded);
            let loaded64 = builder.ins().uextend(types::I64, loaded32);
            store_gpr(builder, gpr_ptr, rt, loaded64, flags);
        }
        Op::Ld { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let call = builder
                .ins()
                .call(helpers.load_u64, &[callbacks_ptr, vaddr]);
            let loaded64 = builder.inst_results(call)[0];
            store_gpr(builder, gpr_ptr, rt, loaded64, flags);
        }
        Op::Lbu { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let call = builder.ins().call(helpers.load_u8, &[callbacks_ptr, vaddr]);
            let loaded = builder.inst_results(call)[0];
            let loaded8 = builder.ins().ireduce(types::I8, loaded);
            let loaded64 = builder.ins().uextend(types::I64, loaded8);
            store_gpr(builder, gpr_ptr, rt, loaded64, flags);
        }
        Op::Sw { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_gpr(builder, gpr_ptr, rt, flags);
            let value32 = builder.ins().ireduce(types::I32, value);
            let value64 = builder.ins().uextend(types::I64, value32);
            builder
                .ins()
                .call(helpers.store_u32, &[callbacks_ptr, vaddr, value64]);
        }
        Op::Sb { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_gpr(builder, gpr_ptr, rt, flags);
            let value8 = builder.ins().ireduce(types::I8, value);
            let value64 = builder.ins().uextend(types::I64, value8);
            builder
                .ins()
                .call(helpers.store_u8, &[callbacks_ptr, vaddr, value64]);
        }
        Op::Sh { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_gpr(builder, gpr_ptr, rt, flags);
            let value16 = builder.ins().ireduce(types::I16, value);
            let value64 = builder.ins().uextend(types::I64, value16);
            builder
                .ins()
                .call(helpers.store_u16, &[callbacks_ptr, vaddr, value64]);
        }
        Op::Sd { base, rt, imm } => {
            let base_addr = load_gpr(builder, gpr_ptr, base, flags);
            let vaddr = builder.ins().iadd_imm(base_addr, i64::from(imm));
            let value = load_gpr(builder, gpr_ptr, rt, flags);
            builder
                .ins()
                .call(helpers.store_u64, &[callbacks_ptr, vaddr, value]);
        }
        Op::Mfc0 { rt, rd } => {
            let reg = iconst_u64(builder, u64::from(rd));
            let call = builder.ins().call(helpers.cop0_read, &[callbacks_ptr, reg]);
            let value = builder.inst_results(call)[0];
            let value32 = builder.ins().ireduce(types::I32, value);
            let value64 = builder.ins().sextend(types::I64, value32);
            store_gpr(builder, gpr_ptr, rt, value64, flags);
        }
        Op::Dmfc0 { rt, rd } => {
            let reg = iconst_u64(builder, u64::from(rd));
            let call = builder.ins().call(helpers.cop0_read, &[callbacks_ptr, reg]);
            let value = builder.inst_results(call)[0];
            store_gpr(builder, gpr_ptr, rt, value, flags);
        }
        Op::Mtc0 { rt, rd } | Op::Dmtc0 { rt, rd } => {
            let reg = iconst_u64(builder, u64::from(rd));
            let value = load_gpr(builder, gpr_ptr, rt, flags);
            builder
                .ins()
                .call(helpers.cop0_write, &[callbacks_ptr, reg, value]);
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
    cop0_read_id: FuncId,
    cop0_write_id: FuncId,
    hi_read_id: FuncId,
    hi_write_id: FuncId,
    lo_read_id: FuncId,
    lo_write_id: FuncId,
    next_symbol_id: u64,
}

impl Default for CraneliftCompiler {
    fn default() -> Self {
        let env_level = std::env::var("N64_CRANELIFT_OPT_LEVEL")
            .unwrap_or_else(|_| "speed".to_string())
            .to_ascii_lowercase();
        let opt_level = match env_level.as_str() {
            "none" => "none",
            "speed_and_size" | "speed-size" | "speedsize" => "speed_and_size",
            "speed" => "speed",
            other => {
                log::warn!(
                    "Unknown N64_CRANELIFT_OPT_LEVEL={:?}; using \"speed\"",
                    other
                );
                "speed"
            }
        };

        let mut flag_builder = settings::builder();
        // Tweakable via N64_CRANELIFT_OPT_LEVEL for benchmarking and tuning.
        flag_builder
            .set("opt_level", opt_level)
            .expect("set cranelift opt_level");
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
        jit_builder.symbol("n64_jit_cop0_read", n64_jit_cop0_read as *const u8);
        jit_builder.symbol("n64_jit_cop0_write", n64_jit_cop0_write as *const u8);
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
        let cop0_read_id = module
            .declare_function("n64_jit_cop0_read", Linkage::Import, &unary_sig)
            .expect("declare n64_jit_cop0_read");
        let cop0_write_id = module
            .declare_function("n64_jit_cop0_write", Linkage::Import, &ternary_sig)
            .expect("declare n64_jit_cop0_write");
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
            cop0_read_id,
            cop0_write_id,
            hi_read_id,
            hi_write_id,
            lo_read_id,
            lo_write_id,
            next_symbol_id: 0,
        }
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
        let mut has_control_flow = false;

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
                );
                steps.push(TraceStep::Branch {
                    branch,
                    delay_op,
                    continue_fallthrough,
                });
                has_control_flow = true;
                decoded_count += 2;
                phys = phys.wrapping_add(8);
                if !continue_fallthrough {
                    break;
                }
                continue;
            }

            match decode_supported_non_branch(opcode) {
                Some(op) => {
                    steps.push(TraceStep::Op(op));
                    decoded_count += 1;
                    phys = phys.wrapping_add(4);
                }
                None => {
                    if steps.is_empty() {
                        return Err(CompileError::UnsupportedOpcode {
                            phys_addr: phys,
                            opcode,
                        });
                    }
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
            .returns
            .push(AbiParam::new(types::I64));

        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let gpr_ptr = builder.block_params(entry_block)[0];
        let start_pc = builder.block_params(entry_block)[1];
        let callbacks_ptr = builder.block_params(entry_block)[2];
        let retired_out_ptr = builder.block_params(entry_block)[3];
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
            cop0_read: self
                .module
                .declare_func_in_func(self.cop0_read_id, builder.func),
            cop0_write: self
                .module
                .declare_func_in_func(self.cop0_write_id, builder.func),
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

        let exit_block = builder.create_block();
        builder.append_block_param(exit_block, types::I64);
        builder.append_block_param(exit_block, types::I32);

        let mut current_pc = start_pc;
        let mut retired_count = builder.ins().iconst(types::I32, 0);
        let mut terminated = false;

        for step in steps.iter().copied() {
            if terminated {
                break;
            }
            match step {
                TraceStep::Op(op) => {
                    emit_op(&mut builder, gpr_ptr, callbacks_ptr, helpers, flags, op);
                    retired_count = builder.ins().iadd_imm(retired_count, 1);
                    current_pc = builder.ins().iadd_imm(current_pc, 4);
                }
                TraceStep::Branch {
                    branch,
                    delay_op,
                    continue_fallthrough,
                } => {
                    let next_of_branch = builder.ins().iadd_imm(current_pc, 4);
                    let fallthrough_pc = builder.ins().iadd_imm(current_pc, 8);
                    match branch {
                        BranchTerminator::J { target } => {
                            let upper = iconst_u64(&mut builder, 0xFFFF_FFFF_F000_0000);
                            let upper_pc = builder.ins().band(current_pc, upper);
                            let low = iconst_u64(&mut builder, u64::from(target) << 2);
                            emit_op(
                                &mut builder,
                                gpr_ptr,
                                callbacks_ptr,
                                helpers,
                                flags,
                                delay_op,
                            );
                            let target_pc = builder.ins().bor(upper_pc, low);
                            let retired_after = builder.ins().iadd_imm(retired_count, 2);
                            let args = [target_pc.into(), retired_after.into()];
                            builder.ins().jump(exit_block, &args);
                            terminated = true;
                        }
                        BranchTerminator::Jal { target } => {
                            let upper = iconst_u64(&mut builder, 0xFFFF_FFFF_F000_0000);
                            let upper_pc = builder.ins().band(current_pc, upper);
                            let low = iconst_u64(&mut builder, u64::from(target) << 2);
                            let link = builder.ins().iadd_imm(current_pc, 8);
                            store_gpr(&mut builder, gpr_ptr, 31, link, flags);
                            emit_op(
                                &mut builder,
                                gpr_ptr,
                                callbacks_ptr,
                                helpers,
                                flags,
                                delay_op,
                            );
                            let target_pc = builder.ins().bor(upper_pc, low);
                            let retired_after = builder.ins().iadd_imm(retired_count, 2);
                            let args = [target_pc.into(), retired_after.into()];
                            builder.ins().jump(exit_block, &args);
                            terminated = true;
                        }
                        BranchTerminator::Jr { rs } => {
                            emit_op(
                                &mut builder,
                                gpr_ptr,
                                callbacks_ptr,
                                helpers,
                                flags,
                                delay_op,
                            );
                            let target_pc = load_gpr(&mut builder, gpr_ptr, rs, flags);
                            let retired_after = builder.ins().iadd_imm(retired_count, 2);
                            let args = [target_pc.into(), retired_after.into()];
                            builder.ins().jump(exit_block, &args);
                            terminated = true;
                        }
                        BranchTerminator::Jalr { rs, rd } => {
                            let link = builder.ins().iadd_imm(current_pc, 8);
                            store_gpr(&mut builder, gpr_ptr, rd, link, flags);
                            emit_op(
                                &mut builder,
                                gpr_ptr,
                                callbacks_ptr,
                                helpers,
                                flags,
                                delay_op,
                            );
                            // Match interpreter ordering: target is read after link write.
                            let target_pc = load_gpr(&mut builder, gpr_ptr, rs, flags);
                            let retired_after = builder.ins().iadd_imm(retired_count, 2);
                            let args = [target_pc.into(), retired_after.into()];
                            builder.ins().jump(exit_block, &args);
                            terminated = true;
                        }
                        BranchTerminator::Beq { rs, rt, offset }
                        | BranchTerminator::Bne { rs, rt, offset }
                        | BranchTerminator::Beql { rs, rt, offset }
                        | BranchTerminator::Bnel { rs, rt, offset } => {
                            let lhs = load_gpr(&mut builder, gpr_ptr, rs, flags);
                            let rhs = load_gpr(&mut builder, gpr_ptr, rt, flags);
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
                            let is_likely = matches!(
                                branch,
                                BranchTerminator::Beql { .. } | BranchTerminator::Bnel { .. }
                            );
                            if is_likely && continue_fallthrough {
                                let taken_block = builder.create_block();
                                let cont_block = builder.create_block();
                                builder
                                    .ins()
                                    .brif(cond_taken, taken_block, &[], cont_block, &[]);
                                builder.seal_block(taken_block);
                                builder.seal_block(cont_block);

                                builder.switch_to_block(taken_block);
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    flags,
                                    delay_op,
                                );
                                let taken_retired = builder.ins().iadd_imm(retired_count, 2);
                                let args = [taken_pc.into(), taken_retired.into()];
                                builder.ins().jump(exit_block, &args);

                                builder.switch_to_block(cont_block);
                                retired_count = builder.ins().iadd_imm(retired_count, 1);
                                current_pc = fallthrough_pc;
                            } else {
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    flags,
                                    delay_op,
                                );
                                let retired_after = builder.ins().iadd_imm(retired_count, 2);
                                if continue_fallthrough {
                                    let cont_block = builder.create_block();
                                    let exit_args = [taken_pc.into(), retired_after.into()];
                                    builder.ins().brif(
                                        cond_taken,
                                        exit_block,
                                        &exit_args,
                                        cont_block,
                                        &[],
                                    );
                                    builder.seal_block(cont_block);
                                    builder.switch_to_block(cont_block);
                                    retired_count = retired_after;
                                    current_pc = fallthrough_pc;
                                } else {
                                    let ret_pc =
                                        builder.ins().select(cond_taken, taken_pc, fallthrough_pc);
                                    let args = [ret_pc.into(), retired_after.into()];
                                    builder.ins().jump(exit_block, &args);
                                    terminated = true;
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
                            let lhs = load_gpr(&mut builder, gpr_ptr, rs, flags);
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
                            let is_likely = matches!(
                                branch,
                                BranchTerminator::Blezl { .. }
                                    | BranchTerminator::Bgtzl { .. }
                                    | BranchTerminator::Bltzl { .. }
                                    | BranchTerminator::Bgezl { .. }
                            );
                            if is_likely && continue_fallthrough {
                                let taken_block = builder.create_block();
                                let cont_block = builder.create_block();
                                builder
                                    .ins()
                                    .brif(cond_taken, taken_block, &[], cont_block, &[]);
                                builder.seal_block(taken_block);
                                builder.seal_block(cont_block);

                                builder.switch_to_block(taken_block);
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    flags,
                                    delay_op,
                                );
                                let taken_retired = builder.ins().iadd_imm(retired_count, 2);
                                let args = [taken_pc.into(), taken_retired.into()];
                                builder.ins().jump(exit_block, &args);

                                builder.switch_to_block(cont_block);
                                retired_count = builder.ins().iadd_imm(retired_count, 1);
                                current_pc = fallthrough_pc;
                            } else {
                                emit_op(
                                    &mut builder,
                                    gpr_ptr,
                                    callbacks_ptr,
                                    helpers,
                                    flags,
                                    delay_op,
                                );
                                let retired_after = builder.ins().iadd_imm(retired_count, 2);
                                if continue_fallthrough {
                                    let cont_block = builder.create_block();
                                    let exit_args = [taken_pc.into(), retired_after.into()];
                                    builder.ins().brif(
                                        cond_taken,
                                        exit_block,
                                        &exit_args,
                                        cont_block,
                                        &[],
                                    );
                                    builder.seal_block(cont_block);
                                    builder.switch_to_block(cont_block);
                                    retired_count = retired_after;
                                    current_pc = fallthrough_pc;
                                } else {
                                    let ret_pc =
                                        builder.ins().select(cond_taken, taken_pc, fallthrough_pc);
                                    let args = [ret_pc.into(), retired_after.into()];
                                    builder.ins().jump(exit_block, &args);
                                    terminated = true;
                                }
                            }
                        }
                    }
                }
            }
        }

        if !terminated {
            let args = [current_pc.into(), retired_count.into()];
            builder.ins().jump(exit_block, &args);
        }

        builder.switch_to_block(exit_block);
        builder.seal_block(exit_block);
        let exit_params = builder.block_params(exit_block);
        let ret_pc = exit_params[0];
        let retired_count = exit_params[1];

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
            has_control_flow,
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

    unsafe extern "C" fn test_cop0_read(_user: *mut u8, _reg: u64) -> u64 {
        0
    }

    unsafe extern "C" fn test_cop0_write(_user: *mut u8, _reg: u64, _value: u64) {}

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
            cop0_read: test_cop0_read,
            cop0_write: test_cop0_write,
            hi_read: test_hi_read,
            hi_write: test_hi_write,
            lo_read: test_lo_read,
            lo_write: test_lo_write,
        }
    }

    #[derive(Default)]
    struct CallbackState {
        mem: HashMap<u64, u8>,
        cop0: [u64; 32],
        hi: u64,
        lo: u64,
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
            cop0_read: state_cop0_read,
            cop0_write: state_cop0_write,
            hi_read: state_hi_read,
            hi_write: state_hi_write,
            lo_read: state_lo_read,
            lo_write: state_lo_write,
        }
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
        let mut callbacks = default_callbacks();
        let exec = block.execute(&mut gpr, 0xFFFF_FFFF_8000_2000, &mut callbacks);

        assert_eq!(block.instruction_count, 5);
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
        let mut callbacks = default_callbacks();
        let start_pc = 0xFFFF_FFFF_8000_2200u64;
        let exec = block.execute(&mut gpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 5);
        assert_eq!(gpr[10], 5);
        assert_eq!(exec.next_pc, start_pc + 20);
        assert_eq!(exec.retired_instructions, 4);
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
        let mut callbacks = default_callbacks();
        let start_pc = 0xFFFF_FFFF_8000_2300u64;
        let exec = block.execute(&mut gpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 4);
        assert_eq!(gpr[10], 0);
        assert_eq!(exec.next_pc, start_pc + 16);
        assert_eq!(exec.retired_instructions, 3);
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
        let mut callbacks = default_callbacks();
        gpr[11] = 0xFFFF_FFFF_8000_2600;
        let start_pc = 0xFFFF_FFFF_8000_2400u64;
        let exec = block.execute(&mut gpr, start_pc, &mut callbacks);

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
        let mut callbacks = default_callbacks();
        let start_pc = 0xFFFF_FFFF_8000_2480u64;
        gpr[25] = start_pc + 0x40;
        let exec = block.execute(&mut gpr, start_pc, &mut callbacks);

        assert!(block.has_control_flow);
        assert_eq!(block.instruction_count, 2);
        assert_eq!(gpr[31], start_pc + 8);
        assert_eq!(gpr[8], 5);
        assert_eq!(exec.next_pc, start_pc + 0x40);
        assert_eq!(exec.retired_instructions, 2);
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
        let mut state = CallbackState::default();
        let mut callbacks = state_callbacks(&mut state);
        let start_pc = 0xFFFF_FFFF_8000_24C0u64;
        let exec = block.execute(&mut gpr, start_pc, &mut callbacks);

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
        let mut state = CallbackState::default();
        let mut callbacks = state_callbacks(&mut state);
        let start_pc = 0xFFFF_FFFF_8000_2500u64;
        let exec = block.execute(&mut gpr, start_pc, &mut callbacks);

        assert_eq!(exec.next_pc, start_pc + 36);
        assert_eq!(exec.retired_instructions, 9);
        assert_eq!(gpr[9], 0x12);
        assert_eq!(gpr[11], 0x34);
        assert_eq!(gpr[2], 0x34);
        assert_eq!(state.cop0[12], 0x34);
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
        let mut state = CallbackState::default();
        state.cop0[13] = 0x0000_0000_8000_0001;
        let mut callbacks = state_callbacks(&mut state);
        let start_pc = 0xFFFF_FFFF_8000_2600u64;
        let exec = block.execute(&mut gpr, start_pc, &mut callbacks);

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
}
