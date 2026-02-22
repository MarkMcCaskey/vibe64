//! Dynarec infrastructure shared by emulator cores.
//!
//! This crate keeps backend/compiler concerns separate from core emulation logic.
//! The first backend is Cranelift.

use std::collections::{BTreeMap, BTreeSet};

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, AbiParam, InstBuilder, MemFlags, Value};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, Linkage, Module};

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

type JitBlockFn = unsafe extern "C" fn(*mut u64);

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

impl CompiledBlock {
    /// Execute this compiled block against the GPR file.
    pub fn execute(&self, gpr: &mut [u64; 32]) {
        // SAFETY: compiled blocks are generated with signature
        // `extern "C" fn(*mut u64)` and `gpr` points to 32 contiguous u64s.
        unsafe {
            (self.entry.as_fn())(gpr.as_mut_ptr());
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
    cache: BTreeMap<u32, CompiledBlock>,
    failed_cache: BTreeSet<u32>,
    stats: RecompilerStats,
    last_error: Option<CompileError>,
}

impl Recompiler {
    pub fn new(compiler: Box<dyn BlockCompiler>, config: RecompilerConfig) -> Self {
        Self {
            compiler,
            config,
            cache: BTreeMap::new(),
            failed_cache: BTreeSet::new(),
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

    pub fn last_error(&self) -> Option<&CompileError> {
        self.last_error.as_ref()
    }

    pub fn lookup(&self, start_phys: u32) -> Option<&CompiledBlock> {
        self.cache.get(&start_phys)
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
            .range(start_phys..end_phys)
            .copied()
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
}

fn decode_supported(raw: u32) -> Option<Op> {
    let opcode = (raw >> 26) as u8;
    let rs = ((raw >> 21) & 0x1F) as u8;
    let rt = ((raw >> 16) & 0x1F) as u8;
    let rd = ((raw >> 11) & 0x1F) as u8;
    let sa = ((raw >> 6) & 0x1F) as u8;
    let funct = (raw & 0x3F) as u8;
    let imm_u16 = raw as u16;
    let imm_i16 = imm_u16 as i16;

    match opcode {
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
        0x00 => match funct {
            0x00 => Some(Op::Sll { rt, rd, sa }),
            0x02 => Some(Op::Srl { rt, rd, sa }),
            0x03 => Some(Op::Sra { rt, rd, sa }),
            0x04 => Some(Op::Sllv { rs, rt, rd }),
            0x06 => Some(Op::Srlv { rs, rt, rd }),
            0x07 => Some(Op::Srav { rs, rt, rd }),
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

fn emit_op(builder: &mut FunctionBuilder<'_>, gpr_ptr: Value, flags: MemFlags, op: Op) {
    match op {
        Op::Addiu { rs, rt, imm } => {
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
    }
}

/// Cranelift backend compiler.
pub struct CraneliftCompiler {
    module: JITModule,
    context: cranelift_codegen::Context,
    builder_context: FunctionBuilderContext,
    next_symbol_id: u64,
}

impl Default for CraneliftCompiler {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        // Favor runtime speed for hot instruction paths.
        flag_builder
            .set("opt_level", "speed")
            .expect("set cranelift opt_level");
        let flags = settings::Flags::new(flag_builder);

        let isa_builder = cranelift_native::builder().expect("create host ISA builder");
        let isa = isa_builder.finish(flags).expect("finish host ISA");
        let jit_builder = JITBuilder::with_isa(isa, default_libcall_names());
        let module = JITModule::new(jit_builder);
        let context = module.make_context();

        Self {
            module,
            context,
            builder_context: FunctionBuilderContext::new(),
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
        let mut ops = Vec::new();
        let mut phys = request.start_phys;

        for _ in 0..request.max_instructions.max(1) {
            let opcode = source.read_u32(phys)?;
            match decode_supported(opcode) {
                Some(op) => {
                    ops.push(op);
                    phys = phys.wrapping_add(4);
                }
                None => {
                    if ops.is_empty() {
                        return Err(CompileError::UnsupportedOpcode { phys_addr: phys, opcode });
                    }
                    break;
                }
            }
        }

        self.context.clear();
        self.context.func.signature.params.clear();
        self.context.func.signature.returns.clear();
        self.context
            .func
            .signature
            .params
            .push(AbiParam::new(self.module.target_config().pointer_type()));

        let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.builder_context);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let gpr_ptr = builder.block_params(entry_block)[0];
        let mut flags = MemFlags::new();
        flags.set_notrap();
        flags.set_aligned();

        for op in ops.iter().copied() {
            emit_op(&mut builder, gpr_ptr, flags, op);
        }

        builder.ins().return_(&[]);
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

        Ok(CompiledBlock {
            start_phys: request.start_phys,
            end_phys: request
                .start_phys
                .wrapping_add((ops.len() as u32).saturating_mul(4)),
            instruction_count: ops.len() as u32,
            has_control_flow: false,
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

    #[test]
    fn compiler_stops_before_unsupported_opcode() {
        let start = 0x1000u32;
        let mut src = TestSource::with_words(&[
            (start, 0x2408_0001),     // addiu t0, r0, 1
            (start + 4, 0x2409_0002), // addiu t1, r0, 2
            (start + 8, 0x1109_0001), // beq t0, t1, +1 (unsupported)
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
        block.execute(&mut gpr);

        assert_eq!(block.instruction_count, 5);
        assert_eq!(gpr[8], 5);
        assert_eq!(gpr[9], 7);
        assert_eq!(gpr[10], 12);
        assert_eq!(gpr[2], 0x123C);
        assert_eq!(gpr[0], 0);
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
        let mut src = TestSource::with_words(&[(start, 0x1109_0001)]); // beq (unsupported)
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
                opcode: 0x1109_0001
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
