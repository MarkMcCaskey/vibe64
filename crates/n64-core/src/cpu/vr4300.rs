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

    /// Execute one instruction. Returns number of cycles consumed.
    pub fn step(&mut self, bus: &mut impl Bus) -> u64 {
        // 1. Translate virtual PC to physical address
        let phys_addr = self.translate_address(self.pc);

        // 2. Fetch instruction
        let opcode = bus.read_u32(phys_addr);
        let instr = Instruction::decode(opcode);

        // 3. Advance PC before execution (branch delay slot handling)
        let current_pc = self.pc;
        self.pc = self.next_pc;
        self.next_pc = self.next_pc.wrapping_add(4);

        // 4. Execute
        self.execute(instr, bus, current_pc);

        // 5. r0 is hardwired to 0 — enforce after every instruction
        self.gpr[0] = 0;

        // 6. Increment COP0 Count register (1 per 2 PCycles) and decrement Random
        self.cop0.increment_count();
        self.cop0.decrement_random();

        // 7. Check for timer interrupt (Count == Compare)
        self.cop0.check_timer_interrupt();

        // 8. Latch external interrupt line: IP2 reflects MI state each step
        if bus.pending_interrupts() {
            self.cop0.set_ip2();
        } else {
            self.cop0.clear_ip2();
        }

        // 9. Take interrupt if conditions are met (IE=1, EXL=0, ERL=0, unmasked IP)
        if self.cop0.interrupt_pending() {
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

    /// Translate a 64-bit virtual address to a 32-bit physical address.
    ///
    /// MIPS segments:
    ///   kseg0 (0x8000_0000..0x9FFF_FFFF): direct map, cached
    ///   kseg1 (0xA000_0000..0xBFFF_FFFF): direct map, uncached
    ///   kuseg (0x0000_0000..0x7FFF_FFFF): TLB mapped
    ///   ksseg/kseg2 (0xC000_0000+):       TLB mapped
    pub fn translate_address(&self, vaddr: u64) -> u32 {
        let addr32 = vaddr as u32;
        match addr32 {
            0x8000_0000..=0x9FFF_FFFF => addr32 - 0x8000_0000, // kseg0: direct, cached
            0xA000_0000..=0xBFFF_FFFF => addr32 - 0xA000_0000, // kseg1: direct, uncached
            _ => {
                // kuseg, ksseg, kseg3 — all TLB mapped
                let asid = self.cop0.regs[Cop0::ENTRY_HI] as u8;
                match self.tlb.lookup(vaddr, asid) {
                    Some(paddr) => paddr,
                    None => {
                        // TLB miss — should raise exception, for now fallback
                        log::trace!("TLB miss for {:#018X}", vaddr);
                        addr32 & 0x1FFF_FFFF
                    }
                }
            }
        }
    }
}
