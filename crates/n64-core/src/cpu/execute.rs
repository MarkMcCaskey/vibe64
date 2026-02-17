use crate::bus::Bus;
use crate::cpu::cop0::Cop0;
use crate::cpu::instruction::Instruction;
use crate::cpu::vr4300::Vr4300;

impl Vr4300 {
    /// Execute a decoded instruction.
    ///
    /// `current_pc` is the PC of this instruction (before the pc/next_pc
    /// advancement), needed for branch target calculations.
    pub fn execute(&mut self, instr: Instruction, bus: &mut impl Bus, current_pc: u64) {
        match instr.opcode() {
            0x00 => self.execute_special(instr, bus),
            0x01 => self.execute_regimm(instr, current_pc),
            0x02 => self.op_j(instr, current_pc),
            0x03 => self.op_jal(instr, current_pc),
            0x04 => self.op_beq(instr, current_pc),
            0x05 => self.op_bne(instr, current_pc),
            0x06 => self.op_blez(instr, current_pc),
            0x07 => self.op_bgtz(instr, current_pc),
            0x08 => self.op_addi(instr),
            0x09 => self.op_addiu(instr),
            0x0A => self.op_slti(instr),
            0x0B => self.op_sltiu(instr),
            0x0C => self.op_andi(instr),
            0x0D => self.op_ori(instr),
            0x0E => self.op_xori(instr),
            0x0F => self.op_lui(instr),
            0x10 => self.execute_cop0(instr, bus),
            0x11 => self.execute_cop1(instr, bus, current_pc),
            0x14 => self.op_beql(instr, current_pc),
            0x15 => self.op_bnel(instr, current_pc),
            0x16 => self.op_blezl(instr, current_pc),
            0x17 => self.op_bgtzl(instr, current_pc),
            0x19 => self.op_daddiu(instr),
            0x20 => self.op_lb(instr, bus),
            0x21 => self.op_lh(instr, bus),
            0x23 => self.op_lw(instr, bus),
            0x24 => self.op_lbu(instr, bus),
            0x25 => self.op_lhu(instr, bus),
            0x27 => self.op_lwu(instr, bus),
            0x28 => self.op_sb(instr, bus),
            0x29 => self.op_sh(instr, bus),
            0x2B => self.op_sw(instr, bus),
            0x2F => self.op_cache(instr),
            0x30 => self.op_ll(instr, bus),
            0x31 => self.op_lwc1(instr, bus),
            0x35 => self.op_ldc1(instr, bus),
            0x37 => self.op_ld(instr, bus),
            0x38 => self.op_sc(instr, bus),
            0x39 => self.op_swc1(instr, bus),
            0x3D => self.op_sdc1(instr, bus),
            0x3F => self.op_sd(instr, bus),
            _ => log::warn!(
                "Unimplemented opcode {:#04X} at PC={:#018X} (raw={:#010X})",
                instr.opcode(), current_pc, instr.raw()
            ),
        }
    }

    fn execute_special(&mut self, instr: Instruction, _bus: &mut impl Bus) {
        match instr.funct() {
            0x00 => self.op_sll(instr),
            0x02 => self.op_srl(instr),
            0x03 => self.op_sra(instr),
            0x04 => self.op_sllv(instr),
            0x06 => self.op_srlv(instr),
            0x07 => self.op_srav(instr),
            0x08 => self.op_jr(instr),
            0x09 => self.op_jalr(instr),
            0x0D => {} // BREAK — stub
            0x0F => {} // SYNC — pipeline barrier, no-op for interpreter
            0x10 => self.op_mfhi(instr),
            0x11 => self.op_mthi(instr),
            0x12 => self.op_mflo(instr),
            0x13 => self.op_mtlo(instr),
            0x14 => self.op_dsllv(instr),
            0x16 => self.op_dsrlv(instr),
            0x17 => self.op_dsrav(instr),
            0x18 => self.op_mult(instr),
            0x19 => self.op_multu(instr),
            0x1A => self.op_div(instr),
            0x1B => self.op_divu(instr),
            0x1C => self.op_dmult(instr),
            0x1D => self.op_dmultu(instr),
            0x1E => self.op_ddiv(instr),
            0x1F => self.op_ddivu(instr),
            0x20 => self.op_add(instr),
            0x21 => self.op_addu(instr),
            0x22 => self.op_sub(instr),
            0x23 => self.op_subu(instr),
            0x24 => self.op_and(instr),
            0x25 => self.op_or(instr),
            0x26 => self.op_xor(instr),
            0x27 => self.op_nor(instr),
            0x2A => self.op_slt(instr),
            0x2B => self.op_sltu(instr),
            0x2C => self.op_dadd(instr),
            0x2D => self.op_daddu(instr),
            0x2F => self.op_dsubu(instr),
            0x38 => self.op_dsll(instr),
            0x3A => self.op_dsrl(instr),
            0x3B => self.op_dsra(instr),
            0x3C => self.op_dsll32(instr),
            0x3E => self.op_dsrl32(instr),
            0x3F => self.op_dsra32(instr),
            _ => log::warn!("Unimplemented SPECIAL funct={:#04X}", instr.funct()),
        }
    }

    fn execute_regimm(&mut self, instr: Instruction, current_pc: u64) {
        match instr.rt() {
            0x00 => self.op_bltz(instr, current_pc),
            0x01 => self.op_bgez(instr, current_pc),
            0x02 => self.op_bltzl(instr, current_pc),
            0x03 => self.op_bgezl(instr, current_pc),
            0x10 => self.op_bltzal(instr, current_pc),
            0x11 => self.op_bgezal(instr, current_pc),
            _ => log::warn!("Unimplemented REGIMM rt={:#04X}", instr.rt()),
        }
    }

    fn execute_cop0(&mut self, instr: Instruction, _bus: &mut impl Bus) {
        match instr.rs() {
            0x00 => { // MFC0: rt = cop0[rd]
                let val = self.cop0.read_reg(instr.rd()) as i32 as u64;
                self.gpr[instr.rt()] = val;
            }
            0x04 => { // MTC0: cop0[rd] = rt
                let val = self.gpr[instr.rt()];
                self.cop0.write_reg(instr.rd(), val);
            }
            0x10..=0x1F => {
                // CO (Coprocessor Operation)
                match instr.funct() {
                    0x01 => {} // TLBR — stub
                    0x02 => {} // TLBWI — stub
                    0x06 => {} // TLBWR — stub
                    0x08 => {} // TLBP — stub
                    0x18 => self.op_eret(),
                    _ => log::warn!("Unimplemented COP0 CO funct={:#04X}", instr.funct()),
                }
            }
            _ => log::warn!("Unimplemented COP0 rs={:#04X}", instr.rs()),
        }
    }

    // ─── Branch helpers ─────────────────────────────────────────

    /// Compute branch target: PC + 4 + (sign_extend(offset) << 2).
    /// Note: current_pc is the branch instruction's PC. The offset is
    /// relative to current_pc + 4 (the delay slot).
    fn branch_target(&self, instr: Instruction, current_pc: u64) -> u64 {
        let offset = (instr.imm() as i16 as i64) << 2;
        (current_pc.wrapping_add(4)).wrapping_add(offset as u64)
    }

    fn branch(&mut self, condition: bool, instr: Instruction, current_pc: u64) {
        if condition {
            self.next_pc = self.branch_target(instr, current_pc);
        }
    }

    /// "Likely" branch: if NOT taken, nullify the delay slot.
    fn branch_likely(&mut self, condition: bool, instr: Instruction, current_pc: u64) {
        if condition {
            self.next_pc = self.branch_target(instr, current_pc);
        } else {
            // Skip the delay slot by advancing PC past it
            self.pc = self.next_pc;
            self.next_pc = self.next_pc.wrapping_add(4);
        }
    }

    // ─── ALU: Immediate ─────────────────────────────────────────

    fn op_addi(&mut self, instr: Instruction) {
        // TODO: trap on overflow (for now, same as ADDIU)
        self.op_addiu(instr);
    }

    fn op_addiu(&mut self, instr: Instruction) {
        let result = (self.gpr[instr.rs()] as i32).wrapping_add(instr.imm() as i16 as i32);
        self.gpr[instr.rt()] = result as i64 as u64; // sign-extend 32→64
    }

    fn op_slti(&mut self, instr: Instruction) {
        let cond = (self.gpr[instr.rs()] as i64) < (instr.imm_sign_ext() as i64);
        self.gpr[instr.rt()] = cond as u64;
    }

    fn op_sltiu(&mut self, instr: Instruction) {
        let cond = self.gpr[instr.rs()] < instr.imm_sign_ext();
        self.gpr[instr.rt()] = cond as u64;
    }

    fn op_andi(&mut self, instr: Instruction) {
        self.gpr[instr.rt()] = self.gpr[instr.rs()] & (instr.imm() as u64);
    }

    fn op_ori(&mut self, instr: Instruction) {
        self.gpr[instr.rt()] = self.gpr[instr.rs()] | (instr.imm() as u64);
    }

    fn op_xori(&mut self, instr: Instruction) {
        self.gpr[instr.rt()] = self.gpr[instr.rs()] ^ (instr.imm() as u64);
    }

    fn op_lui(&mut self, instr: Instruction) {
        let val = (instr.imm() as i16 as i32 as i64) << 16;
        self.gpr[instr.rt()] = val as u64;
    }

    fn op_daddiu(&mut self, instr: Instruction) {
        self.gpr[instr.rt()] = self.gpr[instr.rs()].wrapping_add(instr.imm_sign_ext());
    }

    // ─── ALU: Register (SPECIAL) ────────────────────────────────

    fn op_add(&mut self, instr: Instruction) {
        // TODO: trap on overflow
        self.op_addu(instr);
    }

    fn op_addu(&mut self, instr: Instruction) {
        let result = (self.gpr[instr.rs()] as i32).wrapping_add(self.gpr[instr.rt()] as i32);
        self.gpr[instr.rd()] = result as i64 as u64;
    }

    fn op_sub(&mut self, instr: Instruction) {
        // TODO: trap on overflow
        self.op_subu(instr);
    }

    fn op_subu(&mut self, instr: Instruction) {
        let result = (self.gpr[instr.rs()] as i32).wrapping_sub(self.gpr[instr.rt()] as i32);
        self.gpr[instr.rd()] = result as i64 as u64;
    }

    fn op_and(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rs()] & self.gpr[instr.rt()];
    }

    fn op_or(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rs()] | self.gpr[instr.rt()];
    }

    fn op_xor(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rs()] ^ self.gpr[instr.rt()];
    }

    fn op_nor(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = !(self.gpr[instr.rs()] | self.gpr[instr.rt()]);
    }

    fn op_slt(&mut self, instr: Instruction) {
        let cond = (self.gpr[instr.rs()] as i64) < (self.gpr[instr.rt()] as i64);
        self.gpr[instr.rd()] = cond as u64;
    }

    fn op_sltu(&mut self, instr: Instruction) {
        let cond = self.gpr[instr.rs()] < self.gpr[instr.rt()];
        self.gpr[instr.rd()] = cond as u64;
    }

    // ─── 64-bit ALU ─────────────────────────────────────────────

    fn op_dadd(&mut self, instr: Instruction) {
        // TODO: trap on overflow
        self.op_daddu(instr);
    }

    fn op_daddu(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rs()].wrapping_add(self.gpr[instr.rt()]);
    }

    fn op_dsubu(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rs()].wrapping_sub(self.gpr[instr.rt()]);
    }

    // ─── Shifts (32-bit) ────────────────────────────────────────

    fn op_sll(&mut self, instr: Instruction) {
        let result = (self.gpr[instr.rt()] as i32) << instr.sa();
        self.gpr[instr.rd()] = result as i64 as u64;
    }

    fn op_srl(&mut self, instr: Instruction) {
        let result = (self.gpr[instr.rt()] as u32) >> instr.sa();
        self.gpr[instr.rd()] = result as i32 as i64 as u64;
    }

    fn op_sra(&mut self, instr: Instruction) {
        let result = (self.gpr[instr.rt()] as i32) >> instr.sa();
        self.gpr[instr.rd()] = result as i64 as u64;
    }

    fn op_sllv(&mut self, instr: Instruction) {
        let shift = self.gpr[instr.rs()] & 0x1F;
        let result = (self.gpr[instr.rt()] as i32) << shift;
        self.gpr[instr.rd()] = result as i64 as u64;
    }

    fn op_srlv(&mut self, instr: Instruction) {
        let shift = self.gpr[instr.rs()] & 0x1F;
        let result = (self.gpr[instr.rt()] as u32) >> shift;
        self.gpr[instr.rd()] = result as i32 as i64 as u64;
    }

    fn op_srav(&mut self, instr: Instruction) {
        let shift = self.gpr[instr.rs()] & 0x1F;
        let result = (self.gpr[instr.rt()] as i32) >> shift;
        self.gpr[instr.rd()] = result as i64 as u64;
    }

    // ─── Shifts (64-bit) ────────────────────────────────────────

    fn op_dsll(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rt()] << instr.sa();
    }

    fn op_dsrl(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rt()] >> instr.sa();
    }

    fn op_dsra(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = (self.gpr[instr.rt()] as i64 >> instr.sa()) as u64;
    }

    fn op_dsll32(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rt()] << (instr.sa() + 32);
    }

    fn op_dsrl32(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.gpr[instr.rt()] >> (instr.sa() + 32);
    }

    fn op_dsra32(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = (self.gpr[instr.rt()] as i64 >> (instr.sa() + 32)) as u64;
    }

    fn op_dsllv(&mut self, instr: Instruction) {
        let shift = self.gpr[instr.rs()] & 0x3F;
        self.gpr[instr.rd()] = self.gpr[instr.rt()] << shift;
    }

    fn op_dsrlv(&mut self, instr: Instruction) {
        let shift = self.gpr[instr.rs()] & 0x3F;
        self.gpr[instr.rd()] = self.gpr[instr.rt()] >> shift;
    }

    fn op_dsrav(&mut self, instr: Instruction) {
        let shift = self.gpr[instr.rs()] & 0x3F;
        self.gpr[instr.rd()] = (self.gpr[instr.rt()] as i64 >> shift) as u64;
    }

    // ─── Multiply / Divide ──────────────────────────────────────

    fn op_mult(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()] as i32 as i64;
        let b = self.gpr[instr.rt()] as i32 as i64;
        let result = a.wrapping_mul(b);
        self.lo = result as i32 as i64 as u64; // lower 32, sign-extended
        self.hi = (result >> 32) as i32 as i64 as u64;
    }

    fn op_multu(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()] as u32 as u64;
        let b = self.gpr[instr.rt()] as u32 as u64;
        let result = a.wrapping_mul(b);
        self.lo = result as i32 as i64 as u64;
        self.hi = (result >> 32) as i32 as i64 as u64;
    }

    fn op_div(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()] as i32;
        let b = self.gpr[instr.rt()] as i32;
        if b == 0 { return; }
        if a == i32::MIN && b == -1 { return; }
        self.lo = (a / b) as i64 as u64;
        self.hi = (a % b) as i64 as u64;
    }

    fn op_divu(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()] as u32;
        let b = self.gpr[instr.rt()] as u32;
        if b == 0 { return; }
        self.lo = (a / b) as i32 as i64 as u64;
        self.hi = (a % b) as i32 as i64 as u64;
    }

    fn op_dmult(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()] as i64 as i128;
        let b = self.gpr[instr.rt()] as i64 as i128;
        let result = a.wrapping_mul(b);
        self.lo = result as u64;
        self.hi = (result >> 64) as u64;
    }

    fn op_dmultu(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()] as u128;
        let b = self.gpr[instr.rt()] as u128;
        let result = a.wrapping_mul(b);
        self.lo = result as u64;
        self.hi = (result >> 64) as u64;
    }

    fn op_ddiv(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()] as i64;
        let b = self.gpr[instr.rt()] as i64;
        if b == 0 { return; }
        if a == i64::MIN && b == -1 { return; }
        self.lo = (a / b) as u64;
        self.hi = (a % b) as u64;
    }

    fn op_ddivu(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()];
        let b = self.gpr[instr.rt()];
        if b == 0 { return; }
        self.lo = a / b;
        self.hi = a % b;
    }

    fn op_mfhi(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.hi;
    }

    fn op_mthi(&mut self, instr: Instruction) {
        self.hi = self.gpr[instr.rs()];
    }

    fn op_mflo(&mut self, instr: Instruction) {
        self.gpr[instr.rd()] = self.lo;
    }

    fn op_mtlo(&mut self, instr: Instruction) {
        self.lo = self.gpr[instr.rs()];
    }

    // ─── Branches ───────────────────────────────────────────────

    fn op_beq(&mut self, instr: Instruction, pc: u64) {
        self.branch(self.gpr[instr.rs()] == self.gpr[instr.rt()], instr, pc);
    }

    fn op_bne(&mut self, instr: Instruction, pc: u64) {
        self.branch(self.gpr[instr.rs()] != self.gpr[instr.rt()], instr, pc);
    }

    fn op_blez(&mut self, instr: Instruction, pc: u64) {
        self.branch((self.gpr[instr.rs()] as i64) <= 0, instr, pc);
    }

    fn op_bgtz(&mut self, instr: Instruction, pc: u64) {
        self.branch((self.gpr[instr.rs()] as i64) > 0, instr, pc);
    }

    fn op_beql(&mut self, instr: Instruction, pc: u64) {
        self.branch_likely(self.gpr[instr.rs()] == self.gpr[instr.rt()], instr, pc);
    }

    fn op_bnel(&mut self, instr: Instruction, pc: u64) {
        self.branch_likely(self.gpr[instr.rs()] != self.gpr[instr.rt()], instr, pc);
    }

    fn op_blezl(&mut self, instr: Instruction, pc: u64) {
        self.branch_likely((self.gpr[instr.rs()] as i64) <= 0, instr, pc);
    }

    fn op_bgtzl(&mut self, instr: Instruction, pc: u64) {
        self.branch_likely((self.gpr[instr.rs()] as i64) > 0, instr, pc);
    }

    // REGIMM branches
    fn op_bltz(&mut self, instr: Instruction, pc: u64) {
        self.branch((self.gpr[instr.rs()] as i64) < 0, instr, pc);
    }

    fn op_bgez(&mut self, instr: Instruction, pc: u64) {
        self.branch((self.gpr[instr.rs()] as i64) >= 0, instr, pc);
    }

    fn op_bltzl(&mut self, instr: Instruction, pc: u64) {
        self.branch_likely((self.gpr[instr.rs()] as i64) < 0, instr, pc);
    }

    fn op_bgezl(&mut self, instr: Instruction, pc: u64) {
        self.branch_likely((self.gpr[instr.rs()] as i64) >= 0, instr, pc);
    }

    fn op_bltzal(&mut self, instr: Instruction, pc: u64) {
        self.gpr[31] = pc.wrapping_add(8); // link (skip delay slot)
        self.branch((self.gpr[instr.rs()] as i64) < 0, instr, pc);
    }

    fn op_bgezal(&mut self, instr: Instruction, pc: u64) {
        self.gpr[31] = pc.wrapping_add(8);
        self.branch((self.gpr[instr.rs()] as i64) >= 0, instr, pc);
    }

    // ─── Jumps ──────────────────────────────────────────────────

    fn op_j(&mut self, instr: Instruction, pc: u64) {
        self.next_pc = (pc & 0xFFFF_FFFF_F000_0000) | ((instr.target() as u64) << 2);
    }

    fn op_jal(&mut self, instr: Instruction, pc: u64) {
        self.gpr[31] = pc.wrapping_add(8); // return address (skip delay slot)
        self.next_pc = (pc & 0xFFFF_FFFF_F000_0000) | ((instr.target() as u64) << 2);
    }

    fn op_jr(&mut self, instr: Instruction) {
        self.next_pc = self.gpr[instr.rs()];
    }

    fn op_jalr(&mut self, instr: Instruction) {
        let return_addr = self.pc; // already advanced past delay slot
        self.next_pc = self.gpr[instr.rs()];
        self.gpr[instr.rd()] = return_addr;
    }

    // ─── Loads ──────────────────────────────────────────────────

    fn load_addr(&self, instr: Instruction) -> u32 {
        let vaddr = self.gpr[instr.rs()].wrapping_add(instr.imm_sign_ext());
        self.translate_address(vaddr)
    }

    fn op_lb(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.gpr[instr.rt()] = bus.read_u8(addr) as i8 as i64 as u64;
    }

    fn op_lbu(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.gpr[instr.rt()] = bus.read_u8(addr) as u64;
    }

    fn op_lh(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.gpr[instr.rt()] = bus.read_u16(addr) as i16 as i64 as u64;
    }

    fn op_lhu(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.gpr[instr.rt()] = bus.read_u16(addr) as u64;
    }

    fn op_lw(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.gpr[instr.rt()] = bus.read_u32(addr) as i32 as i64 as u64;
    }

    fn op_lwu(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.gpr[instr.rt()] = bus.read_u32(addr) as u64;
    }

    fn op_ld(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.gpr[instr.rt()] = bus.read_u64(addr);
    }

    // ─── Stores ─────────────────────────────────────────────────

    fn op_sb(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let addr = self.load_addr(instr);
        bus.write_u8(addr, self.gpr[instr.rt()] as u8);
    }

    fn op_sh(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let addr = self.load_addr(instr);
        bus.write_u16(addr, self.gpr[instr.rt()] as u16);
    }

    fn op_sw(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let addr = self.load_addr(instr);
        bus.write_u32(addr, self.gpr[instr.rt()] as u32);
    }

    fn op_sd(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let addr = self.load_addr(instr);
        bus.write_u64(addr, self.gpr[instr.rt()]);
    }

    // ─── Special ────────────────────────────────────────────────

    fn op_cache(&mut self, _instr: Instruction) {
        // Cache operations are no-ops for the interpreter.
    }

    fn op_eret(&mut self) {
        let status = self.cop0.regs[Cop0::STATUS];
        if status & 0x04 != 0 {
            self.pc = self.cop0.regs[Cop0::ERROR_EPC];
            self.cop0.regs[Cop0::STATUS] &= !0x04;
        } else {
            self.pc = self.cop0.regs[Cop0::EPC];
            self.cop0.regs[Cop0::STATUS] &= !0x02;
        }
        self.next_pc = self.pc.wrapping_add(4);
        self.ll_bit = false;
    }

    // ─── Load Linked / Store Conditional ────────────────────────

    fn op_ll(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.gpr[instr.rt()] = bus.read_u32(addr) as i32 as i64 as u64;
        self.ll_bit = true;
    }

    fn op_sc(&mut self, instr: Instruction, bus: &mut impl Bus) {
        if self.ll_bit {
            let addr = self.load_addr(instr);
            bus.write_u32(addr, self.gpr[instr.rt()] as u32);
            self.gpr[instr.rt()] = 1; // success
        } else {
            self.gpr[instr.rt()] = 0; // failed
        }
        self.ll_bit = false;
    }

    // ─── COP1 (FPU) Load/Store ──────────────────────────────────

    fn op_lwc1(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        let val = bus.read_u32(addr);
        self.cop1.fpr[instr.rt()] = val as u64;
    }

    fn op_swc1(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let addr = self.load_addr(instr);
        bus.write_u32(addr, self.cop1.fpr[instr.rt()] as u32);
    }

    fn op_ldc1(&mut self, instr: Instruction, bus: &impl Bus) {
        let addr = self.load_addr(instr);
        self.cop1.fpr[instr.rt()] = bus.read_u64(addr);
    }

    fn op_sdc1(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let addr = self.load_addr(instr);
        bus.write_u64(addr, self.cop1.fpr[instr.rt()]);
    }

    // ─── COP1 (FPU) Execution ───────────────────────────────────

    fn execute_cop1(&mut self, instr: Instruction, _bus: &mut impl Bus, current_pc: u64) {
        let fmt = instr.rs();
        match fmt {
            0x00 => { // MFC1: GPR[rt] = FPR[rd] (low 32 bits)
                self.gpr[instr.rt()] = self.cop1.fpr[instr.rd()] as i32 as i64 as u64;
            }
            0x01 => { // DMFC1: GPR[rt] = FPR[rd] (full 64 bits)
                self.gpr[instr.rt()] = self.cop1.fpr[instr.rd()];
            }
            0x02 => { // CFC1: GPR[rt] = FCR[rd]
                let val = match instr.rd() {
                    0 => self.cop1.fcr0,
                    31 => self.cop1.fcr31,
                    _ => 0,
                };
                self.gpr[instr.rt()] = val as i32 as i64 as u64;
            }
            0x04 => { // MTC1: FPR[rd] = GPR[rt] (low 32 bits)
                self.cop1.fpr[instr.rd()] = self.gpr[instr.rt()] as u32 as u64;
            }
            0x05 => { // DMTC1: FPR[rd] = GPR[rt] (full 64 bits)
                self.cop1.fpr[instr.rd()] = self.gpr[instr.rt()];
            }
            0x06 => { // CTC1: FCR[rd] = GPR[rt]
                match instr.rd() {
                    31 => self.cop1.fcr31 = self.gpr[instr.rt()] as u32,
                    _ => {}
                }
            }
            0x08 => { // BC1: branch on FPU condition
                let cond = self.cop1.condition();
                match instr.rt() {
                    0x00 => self.branch(!cond, instr, current_pc),        // BC1F
                    0x01 => self.branch(cond, instr, current_pc),         // BC1T
                    0x02 => self.branch_likely(!cond, instr, current_pc), // BC1FL
                    0x03 => self.branch_likely(cond, instr, current_pc),  // BC1TL
                    _ => {}
                }
            }
            0x10 => self.cop1_single(instr, current_pc),
            0x11 => self.cop1_double(instr, current_pc),
            0x14 => self.cop1_w(instr, current_pc),
            0x15 => self.cop1_l(instr, current_pc),
            _ => log::warn!("Unimplemented COP1 rs/fmt={:#04X}", fmt),
        }
    }

    // ─── COP1 Single-Precision (fmt=0x10) ────────────────────────

    fn cop1_single(&mut self, instr: Instruction, _current_pc: u64) {
        let fs = instr.rd();
        let ft = instr.rt();
        let fd = instr.sa();

        match instr.funct() {
            0x00 => { // ADD.S
                let result = self.cop1.read_f32(fs) + self.cop1.read_f32(ft);
                self.cop1.write_f32(fd, result);
            }
            0x01 => { // SUB.S
                let result = self.cop1.read_f32(fs) - self.cop1.read_f32(ft);
                self.cop1.write_f32(fd, result);
            }
            0x02 => { // MUL.S
                let result = self.cop1.read_f32(fs) * self.cop1.read_f32(ft);
                self.cop1.write_f32(fd, result);
            }
            0x03 => { // DIV.S
                let result = self.cop1.read_f32(fs) / self.cop1.read_f32(ft);
                self.cop1.write_f32(fd, result);
            }
            0x04 => { // SQRT.S
                self.cop1.write_f32(fd, self.cop1.read_f32(fs).sqrt());
            }
            0x05 => { // ABS.S
                self.cop1.write_f32(fd, self.cop1.read_f32(fs).abs());
            }
            0x06 => { // MOV.S — copy raw bits, avoid float canonicalization
                self.cop1.fpr[fd] = self.cop1.fpr[fs] & 0xFFFF_FFFF;
            }
            0x07 => { // NEG.S
                self.cop1.write_f32(fd, -self.cop1.read_f32(fs));
            }
            0x08 => { // ROUND.L.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).round_ties_even() as i64) as u64;
            }
            0x09 => { // TRUNC.L.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).trunc() as i64) as u64;
            }
            0x0A => { // CEIL.L.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).ceil() as i64) as u64;
            }
            0x0B => { // FLOOR.L.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).floor() as i64) as u64;
            }
            0x0C => { // ROUND.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).round_ties_even() as i32 as u32) as u64;
            }
            0x0D => { // TRUNC.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).trunc() as i32 as u32) as u64;
            }
            0x0E => { // CEIL.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).ceil() as i32 as u32) as u64;
            }
            0x0F => { // FLOOR.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).floor() as i32 as u32) as u64;
            }
            0x21 => { // CVT.D.S
                self.cop1.write_f64(fd, self.cop1.read_f32(fs) as f64);
            }
            0x24 => { // CVT.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).round_ties_even() as i32 as u32) as u64;
            }
            0x25 => { // CVT.L.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).round_ties_even() as i64) as u64;
            }
            0x30..=0x3F => self.cop1_compare_s(instr),
            _ => log::warn!("Unimplemented COP1.S funct={:#04X}", instr.funct()),
        }
    }

    // ─── COP1 Double-Precision (fmt=0x11) ────────────────────────

    fn cop1_double(&mut self, instr: Instruction, _current_pc: u64) {
        let fs = instr.rd();
        let ft = instr.rt();
        let fd = instr.sa();

        match instr.funct() {
            0x00 => { // ADD.D
                let result = self.cop1.read_f64(fs) + self.cop1.read_f64(ft);
                self.cop1.write_f64(fd, result);
            }
            0x01 => { // SUB.D
                let result = self.cop1.read_f64(fs) - self.cop1.read_f64(ft);
                self.cop1.write_f64(fd, result);
            }
            0x02 => { // MUL.D
                let result = self.cop1.read_f64(fs) * self.cop1.read_f64(ft);
                self.cop1.write_f64(fd, result);
            }
            0x03 => { // DIV.D
                let result = self.cop1.read_f64(fs) / self.cop1.read_f64(ft);
                self.cop1.write_f64(fd, result);
            }
            0x04 => { // SQRT.D
                self.cop1.write_f64(fd, self.cop1.read_f64(fs).sqrt());
            }
            0x05 => { // ABS.D
                self.cop1.write_f64(fd, self.cop1.read_f64(fs).abs());
            }
            0x06 => { // MOV.D — copy raw 64-bit bits
                self.cop1.fpr[fd] = self.cop1.fpr[fs];
            }
            0x07 => { // NEG.D
                self.cop1.write_f64(fd, -self.cop1.read_f64(fs));
            }
            0x08 => { // ROUND.L.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).round_ties_even() as i64) as u64;
            }
            0x09 => { // TRUNC.L.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).trunc() as i64) as u64;
            }
            0x0A => { // CEIL.L.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).ceil() as i64) as u64;
            }
            0x0B => { // FLOOR.L.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).floor() as i64) as u64;
            }
            0x0C => { // ROUND.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).round_ties_even() as i32 as u32) as u64;
            }
            0x0D => { // TRUNC.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).trunc() as i32 as u32) as u64;
            }
            0x0E => { // CEIL.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).ceil() as i32 as u32) as u64;
            }
            0x0F => { // FLOOR.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).floor() as i32 as u32) as u64;
            }
            0x20 => { // CVT.S.D
                self.cop1.write_f32(fd, self.cop1.read_f64(fs) as f32);
            }
            0x24 => { // CVT.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).round_ties_even() as i32 as u32) as u64;
            }
            0x25 => { // CVT.L.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).round_ties_even() as i64) as u64;
            }
            0x30..=0x3F => self.cop1_compare_d(instr),
            _ => log::warn!("Unimplemented COP1.D funct={:#04X}", instr.funct()),
        }
    }

    // ─── COP1 Word Integer (fmt=0x14) ───────────────────────────

    fn cop1_w(&mut self, instr: Instruction, _current_pc: u64) {
        let fs = instr.rd();
        let fd = instr.sa();
        let int_val = self.cop1.fpr[fs] as i32;

        match instr.funct() {
            0x20 => { // CVT.S.W
                self.cop1.write_f32(fd, int_val as f32);
            }
            0x21 => { // CVT.D.W
                self.cop1.write_f64(fd, int_val as f64);
            }
            _ => log::warn!("Unimplemented COP1.W funct={:#04X}", instr.funct()),
        }
    }

    // ─── COP1 Long Integer (fmt=0x15) ───────────────────────────

    fn cop1_l(&mut self, instr: Instruction, _current_pc: u64) {
        let fs = instr.rd();
        let fd = instr.sa();
        let int_val = self.cop1.fpr[fs] as i64;

        match instr.funct() {
            0x20 => { // CVT.S.L
                self.cop1.write_f32(fd, int_val as f32);
            }
            0x21 => { // CVT.D.L
                self.cop1.write_f64(fd, int_val as f64);
            }
            _ => log::warn!("Unimplemented COP1.L funct={:#04X}", instr.funct()),
        }
    }

    // ─── COP1 Compare Helpers ───────────────────────────────────

    fn cop1_compare_s(&mut self, instr: Instruction) {
        let a = self.cop1.read_f32(instr.rd());
        let b = self.cop1.read_f32(instr.rt());
        let unordered = a.is_nan() || b.is_nan();
        let cond = instr.funct() & 0x0F;
        let result = ((cond & 0x01) != 0 && unordered)
                  || ((cond & 0x02) != 0 && !unordered && a == b)
                  || ((cond & 0x04) != 0 && !unordered && a < b);
        self.cop1.set_condition(result);
    }

    fn cop1_compare_d(&mut self, instr: Instruction) {
        let a = self.cop1.read_f64(instr.rd());
        let b = self.cop1.read_f64(instr.rt());
        let unordered = a.is_nan() || b.is_nan();
        let cond = instr.funct() & 0x0F;
        let result = ((cond & 0x01) != 0 && unordered)
                  || ((cond & 0x02) != 0 && !unordered && a == b)
                  || ((cond & 0x04) != 0 && !unordered && a < b);
        self.cop1.set_condition(result);
    }
}
