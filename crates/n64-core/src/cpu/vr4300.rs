use std::collections::HashMap;

use crate::bus::Bus;
use crate::cpu::cop0::Cop0;
use crate::cpu::cop1::Cop1;
use crate::cpu::exceptions::ExceptionCode;
use crate::cpu::instruction::Instruction;
use crate::cpu::tlb::Tlb;

/// NEC VR4300 CPU — the N64's main processor.
///
/// A MIPS III 64-bit processor running at 93.75 MHz.
/// 32 general-purpose 64-bit registers (r0 hardwired to 0),
/// HI/LO for multiply/divide, and two coprocessors.
pub struct Vr4300 {
    /// 32 general-purpose 64-bit registers. gpr[0] is always 0.
    pub gpr: [u64; 32],

    /// Program counter (64-bit virtual address)
    pub pc: u64,

    /// Next PC — used for branch delay slot handling.
    /// After each instruction: pc = next_pc; next_pc += 4.
    /// Branch instructions modify next_pc instead of pc.
    pub next_pc: u64,

    /// HI register — upper half of multiply, remainder of divide
    pub hi: u64,

    /// LO register — lower half of multiply, quotient of divide
    pub lo: u64,

    /// COP0: System Control Coprocessor (TLB, exceptions, interrupts)
    pub cop0: Cop0,

    /// COP1: Floating Point Unit
    pub cop1: Cop1,

    /// 32-entry TLB for virtual→physical address translation
    pub tlb: Tlb,

    /// Is the CPU currently in a branch delay slot?
    pub in_delay_slot: bool,

    /// LLBit for Load Linked / Store Conditional (atomic operations)
    pub ll_bit: bool,

    /// Pending TLB miss from a load/store during instruction execution.
    /// Checked by step() after execute() to take TLB exception.
    pub tlb_miss: Option<(u64, ExceptionCode)>,

    /// Set by execute() when an undefined opcode is encountered.
    /// step() checks this to take a Reserved Instruction exception.
    pub reserved_instr: bool,

    /// Track unimplemented opcode hits: key = "CATEGORY:0xNN", value = hit count.
    /// Logs each unique opcode once on first encounter.
    pub unimpl_opcodes: HashMap<String, u64>,

    /// When true, log every FPU operation with input/output values.
    /// Set by the diagnostic loop to trace specific functions.
    pub fpu_trace: bool,

    /// Ring buffer of recent PCs for crash debugging
    pub pc_history: [u32; 64],
    pub pc_history_idx: usize,
    pub step_count: u64,
    /// Count of TLB miss exceptions taken
    pub tlb_miss_count: u64,
}

impl Vr4300 {
    pub fn new() -> Self {
        let mut cpu = Self {
            gpr: [0u64; 32],
            pc: 0xFFFF_FFFF_BFC0_0000, // Reset vector (kseg1 → PIF ROM)
            next_pc: 0xFFFF_FFFF_BFC0_0004,
            hi: 0,
            lo: 0,
            cop0: Cop0::new(),
            cop1: Cop1::new(),
            tlb: Tlb::new(),
            in_delay_slot: false,
            ll_bit: false,
            tlb_miss: None,
            reserved_instr: false,
            unimpl_opcodes: HashMap::new(),
            fpu_trace: false,
            pc_history: [0u32; 64],
            pc_history_idx: 0,
            step_count: 0,
            tlb_miss_count: 0,
        };

        // COP0 initial state after cold reset
        cpu.cop0.regs[Cop0::STATUS] = 0x3400_0000; // CU0=1, CU1=1, ERL=0
        cpu.cop0.regs[Cop0::PRID] = 0x0000_0B22; // NEC VR4300

        cpu
    }

    /// Set up initial state as if PIF boot ROM has already run.
    /// This skips the PIF boot sequence and starts at the IPL3
    /// bootloader in DMEM.
    pub fn setup_post_pif_boot(&mut self) {
        self.pc = 0xA400_0040;
        self.next_pc = self.pc + 4;

        // CIC-6102 initial register values (most common)
        self.gpr[20] = 0x0000_0001; // s4
        self.gpr[22] = 0x0000_003F; // s6
        self.gpr[29] = 0xA400_1FF0; // sp: top of DMEM
    }

    /// Record an unimplemented opcode and trigger Reserved Instruction exception.
    /// On real hardware, undefined opcodes cause this exception. The N64 OS
    /// exception handler decides how to handle it (usually fatal).
    pub fn unimpl(&mut self, key: String, pc: u64, raw: u32) {
        let count = self.unimpl_opcodes.entry(key.clone()).or_insert(0);
        if *count == 0 {
            log::error!(
                "UNIMPLEMENTED {} at PC={:#018X} (raw={:#010X})",
                key,
                pc,
                raw
            );
        }
        *count += 1;
        self.reserved_instr = true;
    }

    /// Dump summary of all unimplemented opcodes encountered.
    pub fn dump_unimpl_summary(&self) {
        if self.unimpl_opcodes.is_empty() {
            return;
        }
        let mut entries: Vec<_> = self.unimpl_opcodes.iter().collect();
        entries.sort_by(|a, b| b.1.cmp(a.1));
        eprintln!("=== Unimplemented opcode summary ===");
        for (key, count) in &entries {
            eprintln!("  {:30} hit {} time(s)", key, count);
        }
    }

    #[inline(always)]
    fn kseg_direct_phys(addr32: u32) -> Option<u32> {
        match addr32 & 0xE000_0000 {
            0x8000_0000 | 0xA000_0000 => Some(addr32 & 0x1FFF_FFFF),
            _ => None,
        }
    }

    #[cold]
    fn handle_fetch_tlb_miss(&mut self, faulting_pc: u64, was_delay_slot: bool) -> u64 {
        let epc = if was_delay_slot {
            faulting_pc.wrapping_sub(4) // branch instruction
        } else {
            faulting_pc
        };
        self.take_tlb_exception(ExceptionCode::TlbLoad, faulting_pc, epc);
        if was_delay_slot {
            self.cop0.regs[Cop0::CAUSE] |= 1u64 << 31; // BD bit
        }
        self.cop0.increment_count();
        self.cop0.decrement_random();
        self.cop0.check_timer_interrupt();
        1
    }

    #[cold]
    fn handle_data_tlb_miss(
        &mut self,
        code: ExceptionCode,
        bad_vaddr: u64,
        current_pc: u64,
        was_delay_slot: bool,
    ) -> u64 {
        self.tlb_miss_count += 1;
        if self.tlb_miss_count <= 5 {
            log::warn!(
                "TLB {:?} #{} at PC={:#010X} bad_vaddr={:#018X} step={}",
                code,
                self.tlb_miss_count,
                current_pc as u32,
                bad_vaddr,
                self.step_count
            );
        }
        let epc = if was_delay_slot {
            current_pc.wrapping_sub(4) // branch instruction
        } else {
            current_pc
        };
        self.take_tlb_exception(code, bad_vaddr, epc);
        if was_delay_slot {
            self.cop0.regs[Cop0::CAUSE] |= 1u64 << 31; // BD bit
        }
        self.gpr[0] = 0;
        self.cop0.increment_count();
        self.cop0.decrement_random();
        self.cop0.check_timer_interrupt();
        1
    }

    #[cold]
    fn handle_reserved_instruction(&mut self, current_pc: u64, was_delay_slot: bool) -> u64 {
        self.reserved_instr = false;
        let epc = if was_delay_slot {
            current_pc.wrapping_sub(4)
        } else {
            current_pc
        };
        self.cop0.regs[Cop0::EPC] = epc;
        let cause = self.cop0.regs[Cop0::CAUSE] as u32;
        let cause = (cause & !0x7C) | ((ExceptionCode::ReservedInstruction as u32) << 2);
        self.cop0.regs[Cop0::CAUSE] = cause as i32 as i64 as u64;
        if was_delay_slot {
            self.cop0.regs[Cop0::CAUSE] |= 1u64 << 31;
        }
        self.cop0.regs[Cop0::STATUS] |= 0x02; // Set EXL
        let bev = (self.cop0.regs[Cop0::STATUS] >> 22) & 1;
        let vector = if bev != 0 {
            0xFFFF_FFFF_BFC0_0200u64
        } else {
            0xFFFF_FFFF_8000_0180u64
        };
        self.pc = vector;
        self.next_pc = vector.wrapping_add(4);
        self.gpr[0] = 0;
        self.cop0.increment_count();
        self.cop0.decrement_random();
        self.cop0.check_timer_interrupt();
        1
    }

    /// Execute one instruction. Returns number of cycles consumed.
    pub fn step(&mut self, bus: &mut impl Bus) -> u64 {
        // Record PC in ring buffer for crash debugging
        self.pc_history[self.pc_history_idx] = self.pc as u32;
        self.pc_history_idx = (self.pc_history_idx + 1) & 63;
        self.step_count += 1;
        let current_pc = self.pc;

        // Save and clear delay slot flag. execute() will set it for branches.
        let was_delay_slot = self.in_delay_slot;
        self.in_delay_slot = false;

        // 1. Translate virtual PC to physical address
        let phys_addr = match Self::kseg_direct_phys(current_pc as u32) {
            Some(addr) => addr,
            None => match self.try_translate(current_pc) {
                Some(addr) => addr,
                None => return self.handle_fetch_tlb_miss(current_pc, was_delay_slot),
            },
        };

        // 2. Fetch instruction
        let opcode = bus.read_u32(phys_addr);
        let instr = Instruction::decode(opcode);

        // 3. Advance PC before execution (branch delay slot handling)
        self.pc = self.next_pc;
        self.next_pc = self.next_pc.wrapping_add(4);

        // 4. Execute
        self.execute(instr, bus, current_pc);

        // 5. Check for TLB miss during data access (set by load/store ops)
        if let Some((bad_vaddr, code)) = self.tlb_miss.take() {
            return self.handle_data_tlb_miss(code, bad_vaddr, current_pc, was_delay_slot);
        }

        // 5b. Check for Reserved Instruction exception (undefined opcode)
        if self.reserved_instr {
            return self.handle_reserved_instruction(current_pc, was_delay_slot);
        }

        // 6. r0 is hardwired to 0 — enforce after every instruction
        self.gpr[0] = 0;

        // 7. Increment COP0 Count register (1 per 2 PCycles) and decrement Random
        self.cop0.increment_count();
        self.cop0.decrement_random();

        // 8. Check for timer interrupt (Count == Compare)
        self.cop0.check_timer_interrupt();

        // 9. Latch external interrupt line: IP2 reflects MI state each step
        if bus.pending_interrupts() {
            self.cop0.set_ip2();
        } else {
            self.cop0.clear_ip2();
        }

        // 10. Take interrupt if conditions are met.
        //     Skip if a branch was just executed — the delay slot must
        //     execute first (branch + delay slot are atomic on MIPS).
        if !self.in_delay_slot && self.cop0.interrupt_pending() {
            self.take_exception(ExceptionCode::Interrupt);
        }

        1 // simplified cycle count for interpreter
    }

    /// Take an exception: save state and jump to the exception handler.
    ///
    /// EPC = PC of the instruction that was about to execute (self.pc at this point,
    /// since step() has already advanced past the current instruction).
    /// Sets EXL in Status (disabling further interrupts), writes exception code
    /// to Cause, and vectors to the appropriate handler address.
    pub fn take_exception(&mut self, code: ExceptionCode) {
        // EPC = the next instruction that *would* have executed
        self.cop0.regs[Cop0::EPC] = self.pc;

        // Set exception code in Cause[6:2], preserving other bits (IP, BD, etc.)
        let cause = self.cop0.regs[Cop0::CAUSE] as u32;
        let cause = (cause & !0x7C) | ((code as u32) << 2);
        self.cop0.regs[Cop0::CAUSE] = cause as i32 as i64 as u64;

        // Set EXL (bit 1) — exception level, disables interrupts
        self.cop0.regs[Cop0::STATUS] |= 0x02;

        // Exception vector depends on BEV bit (Status[22])
        let bev = (self.cop0.regs[Cop0::STATUS] >> 22) & 1;
        let vector = if bev != 0 {
            0xFFFF_FFFF_BFC0_0200u64 // Boot exception vectors (in PIF ROM)
        } else {
            0xFFFF_FFFF_8000_0180u64 // Normal exception vector (in RDRAM)
        };

        self.pc = vector;
        self.next_pc = vector.wrapping_add(4);
    }

    /// Try to translate a virtual address. Returns None on TLB miss.
    /// Used by step() and load/store ops to detect TLB exceptions.
    #[inline(always)]
    pub fn try_translate(&self, vaddr: u64) -> Option<u32> {
        let addr32 = vaddr as u32;
        if let Some(phys) = Self::kseg_direct_phys(addr32) {
            return Some(phys);
        }

        let asid = self.cop0.regs[Cop0::ENTRY_HI] as u8;
        self.tlb.lookup(vaddr, asid)
    }

    /// Translate a virtual address with fallback for debug/diagnostic use.
    /// On TLB miss, returns addr & 0x1FFFFFFF instead of None.
    #[inline(always)]
    pub fn translate_address(&self, vaddr: u64) -> u32 {
        let addr32 = vaddr as u32;
        if let Some(phys) = Self::kseg_direct_phys(addr32) {
            return phys;
        }

        let asid = self.cop0.regs[Cop0::ENTRY_HI] as u8;
        self.tlb.lookup(vaddr, asid).unwrap_or(addr32 & 0x1FFF_FFFF)
    }

    /// Take a TLB exception (Refill or Invalid).
    ///
    /// Sets BadVAddr, Context, EntryHi, Cause, EPC, and vectors to
    /// the appropriate handler. TLB Refill uses vector 0x80000000
    /// when EXL=0 (most common case), otherwise falls back to 0x80000180.
    fn take_tlb_exception(&mut self, code: ExceptionCode, bad_vaddr: u64, victim_pc: u64) {
        // Set TLB-specific COP0 registers
        self.cop0.regs[Cop0::BAD_VADDR] = bad_vaddr;

        // Context: preserve PTEBase[63:23], set VPN2[22:4], zero [3:0]
        let vpn2 = (bad_vaddr >> 13) & 0x7_FFFF;
        let pte_base = self.cop0.regs[Cop0::CONTEXT] & 0xFFFF_FFFF_FF80_0000;
        self.cop0.regs[Cop0::CONTEXT] = pte_base | (vpn2 << 4);

        // EntryHi: VPN2 from bad_vaddr, preserve current ASID
        let asid = self.cop0.regs[Cop0::ENTRY_HI] & 0xFF;
        self.cop0.regs[Cop0::ENTRY_HI] = (bad_vaddr & 0xFFFF_FFFF_FFFF_E000) | asid;

        // Check EXL before setting it — determines exception vector
        let exl_was_set = (self.cop0.regs[Cop0::STATUS] & 0x02) != 0;

        // Set exception code in Cause[6:2]
        let cause = self.cop0.regs[Cop0::CAUSE] as u32;
        let cause = (cause & !0x7C) | ((code as u32) << 2);
        self.cop0.regs[Cop0::CAUSE] = cause as i32 as i64 as u64;

        // EPC = faulting instruction (only if EXL was clear)
        if !exl_was_set {
            self.cop0.regs[Cop0::EPC] = victim_pc;
        }

        // Set EXL (bit 1) — disables further exceptions
        self.cop0.regs[Cop0::STATUS] |= 0x02;

        // Vector: TLB Refill (0x80000000) when EXL was clear, else general (0x80000180)
        let bev = (self.cop0.regs[Cop0::STATUS] >> 22) & 1;
        let vector = if exl_was_set {
            if bev != 0 {
                0xFFFF_FFFF_BFC0_0380u64
            } else {
                0xFFFF_FFFF_8000_0180u64
            }
        } else {
            if bev != 0 {
                0xFFFF_FFFF_BFC0_0000u64
            } else {
                0xFFFF_FFFF_8000_0000u64
            }
        };

        self.pc = vector;
        self.next_pc = vector.wrapping_add(4);
    }
}
