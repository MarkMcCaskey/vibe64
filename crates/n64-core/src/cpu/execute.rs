use crate::bus::Bus;
use crate::cpu::cop0::Cop0;
use crate::cpu::exceptions::ExceptionCode;
use crate::cpu::instruction::Instruction;
use crate::cpu::vr4300::Vr4300;

impl Vr4300 {
    /// Execute a decoded instruction.
    ///
    /// `current_pc` is the PC of this instruction (before the pc/next_pc
    /// advancement), needed for branch target calculations.
    pub fn execute(&mut self, instr: Instruction, bus: &mut impl Bus, current_pc: u64) {
        match instr.opcode() {
            0x00 => self.execute_special(instr, bus, current_pc),
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
            0x10 => self.execute_cop0(instr, bus, current_pc),
            0x11 => self.execute_cop1(instr, bus, current_pc),
            0x14 => self.op_beql(instr, current_pc),
            0x15 => self.op_bnel(instr, current_pc),
            0x16 => self.op_blezl(instr, current_pc),
            0x17 => self.op_bgtzl(instr, current_pc),
            0x18 => self.op_daddiu(instr), // DADDI (TODO: trap on overflow)
            0x19 => self.op_daddiu(instr),
            0x1A => self.op_ldl(instr, bus),
            0x1B => self.op_ldr(instr, bus),
            0x20 => self.op_lb(instr, bus),
            0x21 => self.op_lh(instr, bus),
            0x22 => self.op_lwl(instr, bus),
            0x23 => self.op_lw(instr, bus),
            0x24 => self.op_lbu(instr, bus),
            0x25 => self.op_lhu(instr, bus),
            0x26 => self.op_lwr(instr, bus),
            0x27 => self.op_lwu(instr, bus),
            0x28 => self.op_sb(instr, bus),
            0x29 => self.op_sh(instr, bus),
            0x2A => self.op_swl(instr, bus),
            0x2B => self.op_sw(instr, bus),
            0x2C => self.op_sdl(instr, bus),
            0x2D => self.op_sdr(instr, bus),
            0x2E => self.op_swr(instr, bus),
            0x2F => self.op_cache(instr),
            0x30 => self.op_ll(instr, bus),
            0x31 => self.op_lwc1(instr, bus),
            0x35 => self.op_ldc1(instr, bus),
            0x37 => self.op_ld(instr, bus),
            0x38 => self.op_sc(instr, bus),
            0x39 => self.op_swc1(instr, bus),
            0x3D => self.op_sdc1(instr, bus),
            0x3F => self.op_sd(instr, bus),
            _ => self.unimpl(
                format!("opcode={:#04X}", instr.opcode()),
                current_pc,
                instr.raw(),
            ),
        }
    }

    fn execute_special(&mut self, instr: Instruction, _bus: &mut impl Bus, current_pc: u64) {
        match instr.funct() {
            0x00 => self.op_sll(instr),
            0x02 => self.op_srl(instr),
            0x03 => self.op_sra(instr),
            0x04 => self.op_sllv(instr),
            0x06 => self.op_srlv(instr),
            0x07 => self.op_srav(instr),
            0x08 => self.op_jr(instr, _bus),
            0x09 => self.op_jalr(instr, current_pc),
            0x0C => {
                // SYSCALL: EPC must point to the SYSCALL instruction itself
                self.pc = current_pc;
                self.next_pc = current_pc.wrapping_add(4);
                self.take_exception(crate::cpu::exceptions::ExceptionCode::Syscall);
            }
            0x0D => {
                // BREAK: EPC must point to the BREAK instruction itself
                self.pc = current_pc;
                self.next_pc = current_pc.wrapping_add(4);
                self.take_exception(crate::cpu::exceptions::ExceptionCode::Breakpoint);
            }
            0x0A => {
                // MOVZ: if rt == 0, rd = rs
                if self.gpr[instr.rt()] == 0 {
                    self.gpr[instr.rd()] = self.gpr[instr.rs()];
                }
            }
            0x0B => {
                // MOVN: if rt != 0, rd = rs
                if self.gpr[instr.rt()] != 0 {
                    self.gpr[instr.rd()] = self.gpr[instr.rs()];
                }
            }
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
            0x2E => {
                // DSUB (TODO: trap on overflow)
                self.gpr[instr.rd()] = self.gpr[instr.rs()].wrapping_sub(self.gpr[instr.rt()]);
            }
            0x2F => self.op_dsubu(instr),
            0x30 => {
                // TGE: trap if rs >= rt (signed)
                if (self.gpr[instr.rs()] as i64) >= (self.gpr[instr.rt()] as i64) {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x31 => {
                // TGEU: trap if rs >= rt (unsigned)
                if self.gpr[instr.rs()] >= self.gpr[instr.rt()] {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x32 => {
                // TLT: trap if rs < rt (signed)
                if (self.gpr[instr.rs()] as i64) < (self.gpr[instr.rt()] as i64) {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x33 => {
                // TLTU: trap if rs < rt (unsigned)
                if self.gpr[instr.rs()] < self.gpr[instr.rt()] {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x34 => {
                // TEQ: trap if rs == rt
                if self.gpr[instr.rs()] == self.gpr[instr.rt()] {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x36 => {
                // TNE: trap if rs != rt
                if self.gpr[instr.rs()] != self.gpr[instr.rt()] {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x38 => self.op_dsll(instr),
            0x3A => self.op_dsrl(instr),
            0x3B => self.op_dsra(instr),
            0x3C => self.op_dsll32(instr),
            0x3E => self.op_dsrl32(instr),
            0x3F => self.op_dsra32(instr),
            _ => self.unimpl(
                format!("SPECIAL funct={:#04X}", instr.funct()),
                current_pc,
                instr.raw(),
            ),
        }
    }

    fn execute_regimm(&mut self, instr: Instruction, current_pc: u64) {
        match instr.rt() {
            0x00 => self.op_bltz(instr, current_pc),
            0x01 => self.op_bgez(instr, current_pc),
            0x02 => self.op_bltzl(instr, current_pc),
            0x03 => self.op_bgezl(instr, current_pc),
            0x08 => {
                // TGEI: trap if rs >= sign_ext(imm)
                if (self.gpr[instr.rs()] as i64) >= (instr.imm() as i16 as i64) {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x09 => {
                // TGEIU
                if self.gpr[instr.rs()] >= instr.imm_sign_ext() {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x0A => {
                // TLTI
                if (self.gpr[instr.rs()] as i64) < (instr.imm() as i16 as i64) {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x0B => {
                // TLTIU
                if self.gpr[instr.rs()] < instr.imm_sign_ext() {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x0C => {
                // TEQI
                if self.gpr[instr.rs()] == instr.imm_sign_ext() {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x0E => {
                // TNEI
                if self.gpr[instr.rs()] != instr.imm_sign_ext() {
                    self.pc = current_pc;
                    self.next_pc = current_pc.wrapping_add(4);
                    self.take_exception(crate::cpu::exceptions::ExceptionCode::Trap);
                }
            }
            0x10 => self.op_bltzal(instr, current_pc),
            0x11 => self.op_bgezal(instr, current_pc),
            _ => self.unimpl(
                format!("REGIMM rt={:#04X}", instr.rt()),
                current_pc,
                instr.raw(),
            ),
        }
    }

    fn execute_cop0(&mut self, instr: Instruction, _bus: &mut impl Bus, current_pc: u64) {
        match instr.rs() {
            0x00 => {
                // MFC0: rt = cop0[rd] (sign-extended 32-bit)
                let val = self.cop0.read_reg(instr.rd()) as i32 as u64;
                self.gpr[instr.rt()] = val;
            }
            0x01 => {
                // DMFC0: rt = cop0[rd] (full 64-bit)
                self.gpr[instr.rt()] = self.cop0.read_reg(instr.rd());
            }
            0x04 => {
                // MTC0: cop0[rd] = rt
                let val = self.gpr[instr.rt()];
                self.cop0.write_reg(instr.rd(), val);
            }
            0x05 => {
                // DMTC0: cop0[rd] = rt (full 64-bit)
                let val = self.gpr[instr.rt()];
                self.cop0.write_reg(instr.rd(), val);
            }
            0x10..=0x1F => {
                // CO (Coprocessor Operation)
                match instr.funct() {
                    0x01 => self.op_tlbr(),
                    0x02 => self.op_tlbwi(),
                    0x06 => self.op_tlbwr(),
                    0x08 => self.op_tlbp(),
                    0x18 => self.op_eret(),
                    _ => self.unimpl(
                        format!("COP0 CO funct={:#04X}", instr.funct()),
                        current_pc,
                        instr.raw(),
                    ),
                }
            }
            _ => self.unimpl(
                format!("COP0 rs={:#04X}", instr.rs()),
                current_pc,
                instr.raw(),
            ),
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
        self.in_delay_slot = true;
    }

    /// "Likely" branch: if NOT taken, nullify the delay slot.
    fn branch_likely(&mut self, condition: bool, instr: Instruction, current_pc: u64) {
        if condition {
            self.next_pc = self.branch_target(instr, current_pc);
            self.in_delay_slot = true;
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
        // Shift the full 64-bit value, then truncate to 32 bits.
        // On real hardware, bits [31+sa : sa] of the 64-bit register
        // become the 32-bit result (bit 32 can "fall into" the window).
        let shifted = self.gpr[instr.rt()] >> instr.sa();
        let result = shifted as i32;
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
        let shifted = self.gpr[instr.rt()] >> shift;
        let result = shifted as i32;
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
        if b == 0 {
            return;
        }
        if a == i32::MIN && b == -1 {
            return;
        }
        self.lo = (a / b) as i64 as u64;
        self.hi = (a % b) as i64 as u64;
    }

    fn op_divu(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()] as u32;
        let b = self.gpr[instr.rt()] as u32;
        if b == 0 {
            return;
        }
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
        if b == 0 {
            return;
        }
        if a == i64::MIN && b == -1 {
            return;
        }
        self.lo = (a / b) as u64;
        self.hi = (a % b) as u64;
    }

    fn op_ddivu(&mut self, instr: Instruction) {
        let a = self.gpr[instr.rs()];
        let b = self.gpr[instr.rt()];
        if b == 0 {
            return;
        }
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
        self.in_delay_slot = true;
    }

    fn op_jal(&mut self, instr: Instruction, pc: u64) {
        // No extra tracing
        self.gpr[31] = pc.wrapping_add(8); // return address (skip delay slot)
        self.next_pc = (pc & 0xFFFF_FFFF_F000_0000) | ((instr.target() as u64) << 2);
        self.in_delay_slot = true;
    }

    fn op_jr(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let target = self.gpr[instr.rs()];
        // Crash detector: catch jump-to-zero
        if (target as u32) < 0x1000 && target != 0xFFFF_FFFF_8000_0000 {
            static LOGGED: std::sync::atomic::AtomicBool =
                std::sync::atomic::AtomicBool::new(false);
            if !LOGGED.swap(true, std::sync::atomic::Ordering::Relaxed) {
                let crash_pc = self.pc.wrapping_sub(4);
                log::error!(
                    "JR to zero at step {}! rs={} target={:#018X} PC={:#010X}",
                    self.step_count,
                    instr.rs(),
                    target,
                    crash_pc as u32
                );
                // Dump PC history (last 64 PCs)
                log::error!("  PC history (last 64 instructions):");
                let start = self.pc_history_idx;
                for i in 0..64 {
                    let idx = (start + i) & 63;
                    let pc = self.pc_history[idx];
                    if pc != 0 {
                        let phys = self.translate_address(pc as u64);
                        let opcode = bus.read_u32(phys);
                        log::error!("    [{:2}] {:#010X}: {:#010X}", i, pc, opcode);
                    }
                }
            }
        }
        self.next_pc = target;
        self.in_delay_slot = true;
    }

    fn op_jalr(&mut self, instr: Instruction, pc: u64) {
        self.gpr[instr.rd()] = pc.wrapping_add(8); // return address (skip delay slot)
        self.next_pc = self.gpr[instr.rs()];
        self.in_delay_slot = true;
    }

    // ─── Memory address translation for loads/stores ───────────

    /// Compute and translate a load address. Returns None on TLB miss
    /// (sets tlb_miss flag so step() can take the exception).
    fn load_addr(&mut self, instr: Instruction) -> Option<u32> {
        let vaddr = self.gpr[instr.rs()].wrapping_add(instr.imm_sign_ext());
        match self.try_translate(vaddr) {
            Some(paddr) => Some(paddr),
            None => {
                if self.tlb_miss.is_none() {
                    self.tlb_miss = Some((vaddr, ExceptionCode::TlbLoad));
                }
                None
            }
        }
    }

    /// Compute and translate a store address. Returns None on TLB miss.
    fn store_addr(&mut self, instr: Instruction) -> Option<u32> {
        let vaddr = self.gpr[instr.rs()].wrapping_add(instr.imm_sign_ext());
        match self.try_translate(vaddr) {
            Some(paddr) => Some(paddr),
            None => {
                if self.tlb_miss.is_none() {
                    self.tlb_miss = Some((vaddr, ExceptionCode::TlbStore));
                }
                None
            }
        }
    }

    // ─── Loads ──────────────────────────────────────────────────

    fn op_lb(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        self.gpr[instr.rt()] = bus.read_u8(addr) as i8 as i64 as u64;
    }

    fn op_lbu(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        self.gpr[instr.rt()] = bus.read_u8(addr) as u64;
    }

    fn op_lh(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        self.gpr[instr.rt()] = bus.read_u16(addr) as i16 as i64 as u64;
    }

    fn op_lhu(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        self.gpr[instr.rt()] = bus.read_u16(addr) as u64;
    }

    fn op_lw(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        self.gpr[instr.rt()] = bus.read_u32(addr) as i32 as i64 as u64;
    }

    fn op_lwu(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        self.gpr[instr.rt()] = bus.read_u32(addr) as u64;
    }

    fn op_ld(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        self.gpr[instr.rt()] = bus.read_u64(addr);
    }

    // ─── Stores ─────────────────────────────────────────────────

    fn op_sb(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        bus.write_u8(addr, self.gpr[instr.rt()] as u8);
    }

    fn op_sh(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        bus.write_u16(addr, self.gpr[instr.rt()] as u16);
    }

    fn op_sw(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        bus.write_u32(addr, self.gpr[instr.rt()] as u32);
    }

    fn op_sd(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        bus.write_u64(addr, self.gpr[instr.rt()]);
    }

    // ─── Unaligned loads/stores (big-endian) ────────────────────

    fn op_lwl(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        let word = bus.read_u32(addr & !3);
        let old = self.gpr[instr.rt()] as u32;
        let result = match addr & 3 {
            0 => word,
            1 => (word << 8) | (old & 0x0000_00FF),
            2 => (word << 16) | (old & 0x0000_FFFF),
            3 => (word << 24) | (old & 0x00FF_FFFF),
            _ => unreachable!(),
        };
        self.gpr[instr.rt()] = result as i32 as i64 as u64;
    }

    fn op_lwr(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        let word = bus.read_u32(addr & !3);
        let old = self.gpr[instr.rt()] as u32;
        let result = match addr & 3 {
            0 => (old & 0xFFFF_FF00) | (word >> 24),
            1 => (old & 0xFFFF_0000) | (word >> 16),
            2 => (old & 0xFF00_0000) | (word >> 8),
            3 => word,
            _ => unreachable!(),
        };
        self.gpr[instr.rt()] = result as i32 as i64 as u64;
    }

    fn op_swl(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        let aligned = addr & !3;
        let old = bus.read_u32(aligned);
        let rt = self.gpr[instr.rt()] as u32;
        let new_word = match addr & 3 {
            0 => rt,
            1 => (old & 0xFF00_0000) | (rt >> 8),
            2 => (old & 0xFFFF_0000) | (rt >> 16),
            3 => (old & 0xFFFF_FF00) | (rt >> 24),
            _ => unreachable!(),
        };
        bus.write_u32(aligned, new_word);
    }

    fn op_swr(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        let aligned = addr & !3;
        let old = bus.read_u32(aligned);
        let rt = self.gpr[instr.rt()] as u32;
        let new_word = match addr & 3 {
            0 => (rt << 24) | (old & 0x00FF_FFFF),
            1 => (rt << 16) | (old & 0x0000_FFFF),
            2 => (rt << 8) | (old & 0x0000_00FF),
            3 => rt,
            _ => unreachable!(),
        };
        bus.write_u32(aligned, new_word);
    }

    fn op_ldl(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        let dword = bus.read_u64(addr & !7);
        let old = self.gpr[instr.rt()];
        let shift = (addr & 7) * 8;
        let result = if shift == 0 {
            dword
        } else {
            (dword << shift) | (old & ((1u64 << shift) - 1))
        };
        self.gpr[instr.rt()] = result;
    }

    fn op_ldr(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        let dword = bus.read_u64(addr & !7);
        let old = self.gpr[instr.rt()];
        let shift = (7 - (addr & 7)) * 8;
        let result = if shift == 0 {
            dword
        } else {
            (dword >> shift) | (old & !((1u64 << (64 - shift)) - 1))
        };
        self.gpr[instr.rt()] = result;
    }

    fn op_sdl(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        let aligned = addr & !7;
        let old = bus.read_u64(aligned);
        let rt = self.gpr[instr.rt()];
        let shift = (addr & 7) * 8;
        let result = if shift == 0 {
            rt
        } else {
            (rt >> shift) | (old & !((1u64 << (64 - shift)) - 1))
        };
        bus.write_u64(aligned, result);
    }

    fn op_sdr(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        let aligned = addr & !7;
        let old = bus.read_u64(aligned);
        let rt = self.gpr[instr.rt()];
        let shift = (7 - (addr & 7)) * 8;
        let result = if shift == 0 {
            rt
        } else {
            (rt << shift) | (old & ((1u64 << shift) - 1))
        };
        bus.write_u64(aligned, result);
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

    // ─── TLB Instructions ─────────────────────────────────────────

    /// TLBR: Read TLB entry at Index into COP0 registers
    fn op_tlbr(&mut self) {
        let index = (self.cop0.regs[Cop0::INDEX] & 0x1F) as usize;
        let entry = &self.tlb.entries[index];
        self.cop0.regs[Cop0::PAGE_MASK] = entry.page_mask as u64;
        self.cop0.regs[Cop0::ENTRY_HI] = entry.entry_hi;
        self.cop0.regs[Cop0::ENTRY_LO0] = entry.entry_lo0;
        self.cop0.regs[Cop0::ENTRY_LO1] = entry.entry_lo1;
    }

    /// TLBWI: Write COP0 registers into TLB entry at Index
    fn op_tlbwi(&mut self) {
        let index = (self.cop0.regs[Cop0::INDEX] & 0x1F) as usize;
        self.tlb.entries[index].page_mask = self.cop0.regs[Cop0::PAGE_MASK] as u32;
        self.tlb.entries[index].entry_hi = self.cop0.regs[Cop0::ENTRY_HI];
        self.tlb.entries[index].entry_lo0 = self.cop0.regs[Cop0::ENTRY_LO0];
        self.tlb.entries[index].entry_lo1 = self.cop0.regs[Cop0::ENTRY_LO1];
        log::debug!(
            "TLBWI [{}]: hi={:#018X} lo0={:#018X} lo1={:#018X} mask={:#010X}",
            index,
            self.cop0.regs[Cop0::ENTRY_HI],
            self.cop0.regs[Cop0::ENTRY_LO0],
            self.cop0.regs[Cop0::ENTRY_LO1],
            self.cop0.regs[Cop0::PAGE_MASK]
        );
    }

    /// TLBWR: Write COP0 registers into TLB entry at Random
    fn op_tlbwr(&mut self) {
        let wired = self.cop0.regs[Cop0::WIRED] as usize & 0x1F;
        let random = self.cop0.regs[Cop0::RANDOM] as usize & 0x1F;
        let index = if random > wired { random } else { wired };
        self.tlb.entries[index].page_mask = self.cop0.regs[Cop0::PAGE_MASK] as u32;
        self.tlb.entries[index].entry_hi = self.cop0.regs[Cop0::ENTRY_HI];
        self.tlb.entries[index].entry_lo0 = self.cop0.regs[Cop0::ENTRY_LO0];
        self.tlb.entries[index].entry_lo1 = self.cop0.regs[Cop0::ENTRY_LO1];
        log::debug!(
            "TLBWR [{}]: hi={:#018X} lo0={:#018X} lo1={:#018X} mask={:#010X}",
            index,
            self.cop0.regs[Cop0::ENTRY_HI],
            self.cop0.regs[Cop0::ENTRY_LO0],
            self.cop0.regs[Cop0::ENTRY_LO1],
            self.cop0.regs[Cop0::PAGE_MASK]
        );
    }

    /// TLBP: Probe TLB for matching entry, write index to Index register
    fn op_tlbp(&mut self) {
        let entry_hi = self.cop0.regs[Cop0::ENTRY_HI];
        let asid = entry_hi as u8;

        // Search all 32 entries for a VPN match
        self.cop0.regs[Cop0::INDEX] = 0x8000_0000; // Set P bit (not found)
        for i in 0..32 {
            let entry = &self.tlb.entries[i];
            let mask = (entry.page_mask as u64) | 0x1FFF;
            let vpn_mask = !mask;

            if (entry_hi & vpn_mask) != (entry.entry_hi & vpn_mask) {
                continue;
            }
            let global = (entry.entry_lo0 & 1) != 0 && (entry.entry_lo1 & 1) != 0;
            if !global && (entry.entry_hi as u8) != asid {
                continue;
            }
            self.cop0.regs[Cop0::INDEX] = i as u64; // Clear P bit, set index
            break;
        }
    }

    // ─── Load Linked / Store Conditional ────────────────────────

    fn op_ll(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        self.gpr[instr.rt()] = bus.read_u32(addr) as i32 as i64 as u64;
        self.ll_bit = true;
    }

    fn op_sc(&mut self, instr: Instruction, bus: &mut impl Bus) {
        if self.ll_bit {
            let Some(addr) = self.store_addr(instr) else {
                return;
            };
            bus.write_u32(addr, self.gpr[instr.rt()] as u32);
            self.gpr[instr.rt()] = 1; // success
        } else {
            self.gpr[instr.rt()] = 0; // failed
        }
        self.ll_bit = false;
    }

    // ─── COP1 (FPU) Load/Store ──────────────────────────────────

    fn op_lwc1(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        let val = bus.read_u32(addr);
        if self.fpu_trace {
            eprintln!(
                "  [{:#010X}] LWC1 f{} = [{:#010X}] = {:#010X} ({})",
                self.pc.wrapping_sub(4),
                instr.rt(),
                addr,
                val,
                f32::from_bits(val)
            );
        }
        self.cop1.fpr[instr.rt()] = val as u64;
    }

    fn op_swc1(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        let val = self.cop1.fpr[instr.rt()] as u32;
        if self.fpu_trace {
            eprintln!(
                "  [{:#010X}] SWC1 [{:#010X}] = f{}({:#010X} = {})",
                self.pc.wrapping_sub(4),
                addr,
                instr.rt(),
                val,
                f32::from_bits(val)
            );
        }
        bus.write_u32(addr, val);
    }

    fn op_ldc1(&mut self, instr: Instruction, bus: &impl Bus) {
        let Some(addr) = self.load_addr(instr) else {
            return;
        };
        let val = bus.read_u64(addr);
        // FR=0: split 64-bit value into even (low) and odd (high) registers
        let rt = instr.rt();
        self.cop1.fpr[rt] = val & 0xFFFF_FFFF;
        self.cop1.fpr[rt | 1] = (val >> 32) & 0xFFFF_FFFF;
    }

    fn op_sdc1(&mut self, instr: Instruction, bus: &mut impl Bus) {
        let Some(addr) = self.store_addr(instr) else {
            return;
        };
        // FR=0: combine even (low) and odd (high) registers
        let rt = instr.rt();
        let low = self.cop1.fpr[rt] as u32 as u64;
        let high = self.cop1.fpr[rt | 1] as u32 as u64;
        bus.write_u64(addr, (high << 32) | low);
    }

    // ─── COP1 (FPU) Execution ───────────────────────────────────

    fn execute_cop1(&mut self, instr: Instruction, _bus: &mut impl Bus, current_pc: u64) {
        let fmt = instr.rs();
        match fmt {
            0x00 => {
                // MFC1: GPR[rt] = FPR[rd] (low 32 bits)
                let val = self.cop1.fpr[instr.rd()] as i32 as i64 as u64;
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] MFC1 r{} = f{}(bits={:#010X} = {})",
                        current_pc,
                        instr.rt(),
                        instr.rd(),
                        self.cop1.fpr[instr.rd()] as u32,
                        self.cop1.read_f32(instr.rd())
                    );
                }
                self.gpr[instr.rt()] = val;
            }
            0x01 => {
                // DMFC1: GPR[rt] = FPR[rd] (FR=0: combine even/odd pair)
                let rd = instr.rd();
                let low = self.cop1.fpr[rd] as u32 as u64;
                let high = self.cop1.fpr[rd | 1] as u32 as u64;
                self.gpr[instr.rt()] = (high << 32) | low;
            }
            0x02 => {
                // CFC1: GPR[rt] = FCR[rd]
                let val = match instr.rd() {
                    0 => self.cop1.fcr0,
                    31 => self.cop1.fcr31,
                    _ => 0,
                };
                self.gpr[instr.rt()] = val as i32 as i64 as u64;
            }
            0x04 => {
                // MTC1: FPR[rd] = GPR[rt] (low 32 bits)
                let bits = self.gpr[instr.rt()] as u32;
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] MTC1 f{} = r{}(bits={:#010X} = {})",
                        current_pc,
                        instr.rd(),
                        instr.rt(),
                        bits,
                        f32::from_bits(bits)
                    );
                }
                self.cop1.fpr[instr.rd()] = bits as u64;
            }
            0x05 => {
                // DMTC1: FPR[rd] = GPR[rt] (FR=0: split into even/odd pair)
                let val = self.gpr[instr.rt()];
                let rd = instr.rd();
                self.cop1.fpr[rd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[rd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x06 => {
                // CTC1: FCR[rd] = GPR[rt]
                match instr.rd() {
                    31 => self.cop1.fcr31 = self.gpr[instr.rt()] as u32,
                    _ => {}
                }
            }
            0x08 => {
                // BC1: branch on FPU condition
                let cond = self.cop1.condition();
                match instr.rt() {
                    0x00 => self.branch(!cond, instr, current_pc), // BC1F
                    0x01 => self.branch(cond, instr, current_pc),  // BC1T
                    0x02 => self.branch_likely(!cond, instr, current_pc), // BC1FL
                    0x03 => self.branch_likely(cond, instr, current_pc), // BC1TL
                    _ => {}
                }
            }
            0x10 => self.cop1_single(instr, current_pc),
            0x11 => self.cop1_double(instr, current_pc),
            0x14 => self.cop1_w(instr, current_pc),
            0x15 => self.cop1_l(instr, current_pc),
            _ => self.unimpl(format!("COP1 rs/fmt={:#04X}", fmt), current_pc, instr.raw()),
        }
    }

    // ─── COP1 Single-Precision (fmt=0x10) ────────────────────────

    fn cop1_single(&mut self, instr: Instruction, current_pc: u64) {
        let fs = instr.rd();
        let ft = instr.rt();
        let fd = instr.sa();

        match instr.funct() {
            0x00 => {
                // ADD.S
                let a = self.cop1.read_f32(fs);
                let b = self.cop1.read_f32(ft);
                let result = a + b;
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] ADD.S f{} = f{}({}) + f{}({}) = {}",
                        current_pc, fd, fs, a, ft, b, result
                    );
                }
                self.cop1.write_f32(fd, result);
            }
            0x01 => {
                // SUB.S
                let a = self.cop1.read_f32(fs);
                let b = self.cop1.read_f32(ft);
                let result = a - b;
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] SUB.S f{} = f{}({}) - f{}({}) = {}",
                        current_pc, fd, fs, a, ft, b, result
                    );
                }
                self.cop1.write_f32(fd, result);
            }
            0x02 => {
                // MUL.S
                let a = self.cop1.read_f32(fs);
                let b = self.cop1.read_f32(ft);
                let result = a * b;
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] MUL.S f{} = f{}({}) * f{}({}) = {}",
                        current_pc, fd, fs, a, ft, b, result
                    );
                }
                self.cop1.write_f32(fd, result);
            }
            0x03 => {
                // DIV.S
                let a = self.cop1.read_f32(fs);
                let b = self.cop1.read_f32(ft);
                let result = a / b;
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] DIV.S f{} = f{}({}) / f{}({}) = {}",
                        current_pc, fd, fs, a, ft, b, result
                    );
                }
                self.cop1.write_f32(fd, result);
            }
            0x04 => {
                // SQRT.S
                let a = self.cop1.read_f32(fs);
                let result = a.sqrt();
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] SQRT.S f{} = sqrt(f{}({})) = {}",
                        current_pc, fd, fs, a, result
                    );
                }
                self.cop1.write_f32(fd, result);
            }
            0x05 => {
                // ABS.S
                let a = self.cop1.read_f32(fs);
                let result = a.abs();
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] ABS.S f{} = abs(f{}({})) = {}",
                        current_pc, fd, fs, a, result
                    );
                }
                self.cop1.write_f32(fd, result);
            }
            0x06 => {
                // MOV.S — copy raw bits, avoid float canonicalization
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] MOV.S f{} = f{}(bits={:#010X} = {})",
                        current_pc,
                        fd,
                        fs,
                        self.cop1.fpr[fs] as u32,
                        self.cop1.read_f32(fs)
                    );
                }
                self.cop1.fpr[fd] = self.cop1.fpr[fs] & 0xFFFF_FFFF;
            }
            0x07 => {
                // NEG.S
                let a = self.cop1.read_f32(fs);
                let result = -a;
                if self.fpu_trace {
                    eprintln!(
                        "  [{:#010X}] NEG.S f{} = -f{}({}) = {}",
                        current_pc, fd, fs, a, result
                    );
                }
                self.cop1.write_f32(fd, result);
            }
            0x08 => {
                // ROUND.L.S (FR=0: 64-bit result split into even/odd pair)
                let val = (self.cop1.read_f32(fs).round_ties_even() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x09 => {
                // TRUNC.L.S
                let val = (self.cop1.read_f32(fs).trunc() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x0A => {
                // CEIL.L.S
                let val = (self.cop1.read_f32(fs).ceil() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x0B => {
                // FLOOR.L.S
                let val = (self.cop1.read_f32(fs).floor() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x0C => {
                // ROUND.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).round_ties_even() as i32 as u32) as u64;
            }
            0x0D => {
                // TRUNC.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).trunc() as i32 as u32) as u64;
            }
            0x0E => {
                // CEIL.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).ceil() as i32 as u32) as u64;
            }
            0x0F => {
                // FLOOR.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).floor() as i32 as u32) as u64;
            }
            0x21 => {
                // CVT.D.S
                self.cop1.write_f64(fd, self.cop1.read_f32(fs) as f64);
            }
            0x24 => {
                // CVT.W.S
                self.cop1.fpr[fd] = (self.cop1.read_f32(fs).round_ties_even() as i32 as u32) as u64;
            }
            0x25 => {
                // CVT.L.S (FR=0: 64-bit result split into even/odd pair)
                let val = (self.cop1.read_f32(fs).round_ties_even() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x30..=0x3F => self.cop1_compare_s(instr),
            _ => self.unimpl(
                format!("COP1.S funct={:#04X}", instr.funct()),
                current_pc,
                instr.raw(),
            ),
        }
    }

    // ─── COP1 Double-Precision (fmt=0x11) ────────────────────────

    fn cop1_double(&mut self, instr: Instruction, current_pc: u64) {
        let fs = instr.rd();
        let ft = instr.rt();
        let fd = instr.sa();

        match instr.funct() {
            0x00 => {
                // ADD.D
                let result = self.cop1.read_f64(fs) + self.cop1.read_f64(ft);
                self.cop1.write_f64(fd, result);
            }
            0x01 => {
                // SUB.D
                let result = self.cop1.read_f64(fs) - self.cop1.read_f64(ft);
                self.cop1.write_f64(fd, result);
            }
            0x02 => {
                // MUL.D
                let result = self.cop1.read_f64(fs) * self.cop1.read_f64(ft);
                self.cop1.write_f64(fd, result);
            }
            0x03 => {
                // DIV.D
                let result = self.cop1.read_f64(fs) / self.cop1.read_f64(ft);
                self.cop1.write_f64(fd, result);
            }
            0x04 => {
                // SQRT.D
                self.cop1.write_f64(fd, self.cop1.read_f64(fs).sqrt());
            }
            0x05 => {
                // ABS.D
                self.cop1.write_f64(fd, self.cop1.read_f64(fs).abs());
            }
            0x06 => {
                // MOV.D — copy raw 64-bit bits (FR=0: copy even+odd pair)
                self.cop1.fpr[fd] = self.cop1.fpr[fs];
                self.cop1.fpr[fd | 1] = self.cop1.fpr[fs | 1];
            }
            0x07 => {
                // NEG.D
                self.cop1.write_f64(fd, -self.cop1.read_f64(fs));
            }
            0x08 => {
                // ROUND.L.D (FR=0: 64-bit result split into even/odd pair)
                let val = (self.cop1.read_f64(fs).round_ties_even() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x09 => {
                // TRUNC.L.D
                let val = (self.cop1.read_f64(fs).trunc() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x0A => {
                // CEIL.L.D
                let val = (self.cop1.read_f64(fs).ceil() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x0B => {
                // FLOOR.L.D
                let val = (self.cop1.read_f64(fs).floor() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x0C => {
                // ROUND.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).round_ties_even() as i32 as u32) as u64;
            }
            0x0D => {
                // TRUNC.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).trunc() as i32 as u32) as u64;
            }
            0x0E => {
                // CEIL.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).ceil() as i32 as u32) as u64;
            }
            0x0F => {
                // FLOOR.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).floor() as i32 as u32) as u64;
            }
            0x20 => {
                // CVT.S.D
                self.cop1.write_f32(fd, self.cop1.read_f64(fs) as f32);
            }
            0x24 => {
                // CVT.W.D
                self.cop1.fpr[fd] = (self.cop1.read_f64(fs).round_ties_even() as i32 as u32) as u64;
            }
            0x25 => {
                // CVT.L.D (FR=0: 64-bit result split into even/odd pair)
                let val = (self.cop1.read_f64(fs).round_ties_even() as i64) as u64;
                self.cop1.fpr[fd] = val & 0xFFFF_FFFF;
                self.cop1.fpr[fd | 1] = (val >> 32) & 0xFFFF_FFFF;
            }
            0x30..=0x3F => self.cop1_compare_d(instr),
            _ => self.unimpl(
                format!("COP1.D funct={:#04X}", instr.funct()),
                current_pc,
                instr.raw(),
            ),
        }
    }

    // ─── COP1 Word Integer (fmt=0x14) ───────────────────────────

    fn cop1_w(&mut self, instr: Instruction, current_pc: u64) {
        let fs = instr.rd();
        let fd = instr.sa();
        let int_val = self.cop1.fpr[fs] as i32;

        match instr.funct() {
            0x20 => {
                // CVT.S.W
                self.cop1.write_f32(fd, int_val as f32);
            }
            0x21 => {
                // CVT.D.W
                self.cop1.write_f64(fd, int_val as f64);
            }
            _ => self.unimpl(
                format!("COP1.W funct={:#04X}", instr.funct()),
                current_pc,
                instr.raw(),
            ),
        }
    }

    // ─── COP1 Long Integer (fmt=0x15) ───────────────────────────

    fn cop1_l(&mut self, instr: Instruction, current_pc: u64) {
        let fs = instr.rd();
        let fd = instr.sa();
        // FR=0: combine even/odd pair for 64-bit integer value
        let low = self.cop1.fpr[fs] as u32 as u64;
        let high = self.cop1.fpr[fs | 1] as u32 as u64;
        let int_val = ((high << 32) | low) as i64;

        match instr.funct() {
            0x20 => {
                // CVT.S.L
                self.cop1.write_f32(fd, int_val as f32);
            }
            0x21 => {
                // CVT.D.L
                self.cop1.write_f64(fd, int_val as f64);
            }
            _ => self.unimpl(
                format!("COP1.L funct={:#04X}", instr.funct()),
                current_pc,
                instr.raw(),
            ),
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
        if self.fpu_trace {
            eprintln!(
                "  [{:#010X}] C.{}.S f{}({}) f{}({}) = {}",
                self.pc.wrapping_sub(8),
                cond,
                instr.rd(),
                a,
                instr.rt(),
                b,
                result
            );
        }
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
