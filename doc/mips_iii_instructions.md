# MIPS III Instruction Set Reference

For the NEC VR4300 (N64 CPU). This covers the instructions most commonly encountered in N64 games.

References:
- [MIPS III Instructions — N64brew](https://n64brew.dev/wiki/MIPS_III_instructions)
- [MIPS R4000 User's Manual (IDT)](https://www.cs.cmu.edu/afs/cs/academic/class/15740-f97/public/doc/mips-isa.pdf)
- [VR4300 Datasheet — NEC](https://www.nec.co.jp/products/device/featured/r4300i/)

---

## Instruction Formats

All MIPS instructions are **32 bits** wide. Three formats:

### R-Type (Register)
```
31      26 25    21 20    16 15    11 10     6 5      0
┌─────────┬────────┬────────┬────────┬────────┬────────┐
│  opcode │   rs   │   rt   │   rd   │   sa   │ funct  │
│  (6)    │  (5)   │  (5)   │  (5)   │  (5)   │  (6)   │
└─────────┴────────┴────────┴────────┴────────┴────────┘
```
Used for: register-to-register ALU ops (ADD, SUB, AND, OR, SLT, shifts, etc.)

### I-Type (Immediate)
```
31      26 25    21 20    16 15                        0
┌─────────┬────────┬────────┬──────────────────────────┐
│  opcode │   rs   │   rt   │       immediate          │
│  (6)    │  (5)   │  (5)   │         (16)             │
└─────────┴────────┴────────┴──────────────────────────┘
```
Used for: immediate ALU ops, loads, stores, branches

### J-Type (Jump)
```
31      26 25                                          0
┌─────────┬────────────────────────────────────────────┐
│  opcode │               target                       │
│  (6)    │                (26)                        │
└─────────┴────────────────────────────────────────────┘
```
Used for: J, JAL

---

## Primary Opcode Table (bits [31:26])

| Opcode | Hex | Mnemonic | Format | Description |
|--------|-----|----------|--------|-------------|
| 000000 | 0x00 | SPECIAL | R | Secondary decode on `funct` field |
| 000001 | 0x01 | REGIMM | I | Secondary decode on `rt` field |
| 000010 | 0x02 | J | J | Jump |
| 000011 | 0x03 | JAL | J | Jump and Link |
| 000100 | 0x04 | BEQ | I | Branch if Equal |
| 000101 | 0x05 | BNE | I | Branch if Not Equal |
| 000110 | 0x06 | BLEZ | I | Branch if <= 0 |
| 000111 | 0x07 | BGTZ | I | Branch if > 0 |
| 001000 | 0x08 | ADDI | I | Add Immediate (trap on overflow) |
| 001001 | 0x09 | ADDIU | I | Add Immediate Unsigned (no trap) |
| 001010 | 0x0A | SLTI | I | Set on Less Than Immediate |
| 001011 | 0x0B | SLTIU | I | Set on Less Than Immediate Unsigned |
| 001100 | 0x0C | ANDI | I | AND Immediate |
| 001101 | 0x0D | ORI | I | OR Immediate |
| 001110 | 0x0E | XORI | I | XOR Immediate |
| 001111 | 0x0F | LUI | I | Load Upper Immediate |
| 010000 | 0x10 | COP0 | — | Coprocessor 0 operations |
| 010001 | 0x11 | COP1 | — | Coprocessor 1 (FPU) operations |
| 010100 | 0x14 | BEQL | I | Branch if Equal Likely* |
| 010101 | 0x15 | BNEL | I | Branch if Not Equal Likely* |
| 010110 | 0x16 | BLEZL | I | Branch if <= 0 Likely* |
| 010111 | 0x17 | BGTZL | I | Branch if > 0 Likely* |
| 011000 | 0x18 | DADDI | I | Doubleword Add Immediate |
| 011001 | 0x19 | DADDIU | I | Doubleword Add Immediate Unsigned |
| 011010 | 0x1A | LDL | I | Load Doubleword Left |
| 011011 | 0x1B | LDR | I | Load Doubleword Right |
| 100000 | 0x20 | LB | I | Load Byte (sign-extend) |
| 100001 | 0x21 | LH | I | Load Halfword (sign-extend) |
| 100010 | 0x22 | LWL | I | Load Word Left (unaligned) |
| 100011 | 0x23 | LW | I | Load Word (sign-extend to 64) |
| 100100 | 0x24 | LBU | I | Load Byte Unsigned |
| 100101 | 0x25 | LHU | I | Load Halfword Unsigned |
| 100110 | 0x26 | LWR | I | Load Word Right (unaligned) |
| 100111 | 0x27 | LWU | I | Load Word Unsigned (zero-extend) |
| 101000 | 0x28 | SB | I | Store Byte |
| 101001 | 0x29 | SH | I | Store Halfword |
| 101010 | 0x2A | SWL | I | Store Word Left (unaligned) |
| 101011 | 0x2B | SW | I | Store Word |
| 101110 | 0x2E | SWR | I | Store Word Right (unaligned) |
| 101111 | 0x2F | CACHE | I | Cache operation |
| 110000 | 0x30 | LL | I | Load Linked (for atomics) |
| 110001 | 0x31 | LWC1 | I | Load Word to COP1 (FPU) |
| 110101 | 0x35 | LDC1 | I | Load Doubleword to COP1 |
| 110111 | 0x37 | LD | I | Load Doubleword |
| 111000 | 0x38 | SC | I | Store Conditional (for atomics) |
| 111001 | 0x39 | SWC1 | I | Store Word from COP1 |
| 111101 | 0x3D | SDC1 | I | Store Doubleword from COP1 |
| 111111 | 0x3F | SD | I | Store Doubleword |

*"Likely" branches: if branch is NOT taken, the delay slot is nullified (not executed). This is a MIPS II+ optimization.

---

## SPECIAL Function Table (opcode=0x00, decode on `funct` bits [5:0])

| Funct | Hex | Mnemonic | Description |
|-------|-----|----------|-------------|
| 000000 | 0x00 | SLL | Shift Left Logical (also NOP when rd=rt=sa=0) |
| 000010 | 0x02 | SRL | Shift Right Logical |
| 000011 | 0x03 | SRA | Shift Right Arithmetic |
| 000100 | 0x04 | SLLV | Shift Left Logical Variable |
| 000110 | 0x06 | SRLV | Shift Right Logical Variable |
| 000111 | 0x07 | SRAV | Shift Right Arithmetic Variable |
| 001000 | 0x08 | JR | Jump Register |
| 001001 | 0x09 | JALR | Jump and Link Register |
| 001100 | 0x0C | SYSCALL | System Call |
| 001101 | 0x0D | BREAK | Breakpoint |
| 001111 | 0x0F | SYNC | Synchronize (pipeline barrier) |
| 010000 | 0x10 | MFHI | Move From HI |
| 010001 | 0x11 | MTHI | Move To HI |
| 010010 | 0x12 | MFLO | Move From LO |
| 010011 | 0x13 | MTLO | Move To LO |
| 010100 | 0x14 | DSLLV | Doubleword Shift Left Logical Variable |
| 010110 | 0x16 | DSRLV | Doubleword Shift Right Logical Variable |
| 010111 | 0x17 | DSRAV | Doubleword Shift Right Arithmetic Variable |
| 011000 | 0x18 | MULT | Multiply (signed, result in HI:LO) |
| 011001 | 0x19 | MULTU | Multiply Unsigned |
| 011010 | 0x1A | DIV | Divide (signed, LO=quotient, HI=remainder) |
| 011011 | 0x1B | DIVU | Divide Unsigned |
| 011100 | 0x1C | DMULT | Doubleword Multiply |
| 011101 | 0x1D | DMULTU | Doubleword Multiply Unsigned |
| 011110 | 0x1E | DDIV | Doubleword Divide |
| 011111 | 0x1F | DDIVU | Doubleword Divide Unsigned |
| 100000 | 0x20 | ADD | Add (trap on overflow) |
| 100001 | 0x21 | ADDU | Add Unsigned (no trap) |
| 100010 | 0x22 | SUB | Subtract (trap on overflow) |
| 100011 | 0x23 | SUBU | Subtract Unsigned |
| 100100 | 0x24 | AND | Bitwise AND |
| 100101 | 0x25 | OR | Bitwise OR |
| 100110 | 0x26 | XOR | Bitwise XOR |
| 100111 | 0x27 | NOR | Bitwise NOR |
| 101010 | 0x2A | SLT | Set on Less Than (signed) |
| 101011 | 0x2B | SLTU | Set on Less Than Unsigned |
| 101100 | 0x2C | DADD | Doubleword Add (trap) |
| 101101 | 0x2D | DADDU | Doubleword Add Unsigned |
| 101110 | 0x2E | DSUB | Doubleword Subtract (trap) |
| 101111 | 0x2F | DSUBU | Doubleword Subtract Unsigned |
| 110000 | 0x30 | TGE | Trap if Greater or Equal |
| 110100 | 0x34 | TEQ | Trap if Equal |
| 110110 | 0x36 | TNE | Trap if Not Equal |
| 111000 | 0x38 | DSLL | Doubleword Shift Left Logical |
| 111010 | 0x3A | DSRL | Doubleword Shift Right Logical |
| 111011 | 0x3B | DSRA | Doubleword Shift Right Arithmetic |
| 111100 | 0x3C | DSLL32 | Doubleword Shift Left Logical + 32 |
| 111110 | 0x3E | DSRL32 | Doubleword Shift Right Logical + 32 |
| 111111 | 0x3F | DSRA32 | Doubleword Shift Right Arithmetic + 32 |

---

## REGIMM Table (opcode=0x01, decode on `rt` bits [20:16])

| rt | Hex | Mnemonic | Description |
|----|-----|----------|-------------|
| 00000 | 0x00 | BLTZ | Branch if Less Than Zero |
| 00001 | 0x01 | BGEZ | Branch if Greater or Equal to Zero |
| 00010 | 0x02 | BLTZL | Branch if < 0 Likely |
| 00011 | 0x03 | BGEZL | Branch if >= 0 Likely |
| 10000 | 0x10 | BLTZAL | Branch if < 0 and Link |
| 10001 | 0x11 | BGEZAL | Branch if >= 0 and Link |
| 10010 | 0x12 | BLTZALL | Branch if < 0 and Link Likely |
| 10011 | 0x13 | BGEZALL | Branch if >= 0 and Link Likely |

---

## COP0 Instructions (opcode=0x10)

Decoded by `rs` field (bits [25:21]):

| rs | Mnemonic | Description |
|----|----------|-------------|
| 0x00 | MFC0 rd, rt | Move From COP0 register rt to GPR rd |
| 0x01 | DMFC0 rd, rt | Doubleword Move From COP0 |
| 0x04 | MTC0 rt, rd | Move To COP0 register rd from GPR rt |
| 0x05 | DMTC0 rt, rd | Doubleword Move To COP0 |

When `rs` bit 4 is set (rs >= 0x10), decode on `funct`:

| Funct | Mnemonic | Description |
|-------|----------|-------------|
| 0x01 | TLBR | Read TLB entry at Index |
| 0x02 | TLBWI | Write TLB entry at Index |
| 0x06 | TLBWR | Write TLB entry at Random |
| 0x08 | TLBP | Probe TLB for matching entry |
| 0x18 | ERET | Exception Return (PC = EPC, clear EXL) |

---

## COP1 (FPU) Instructions (opcode=0x11)

Decoded by `rs` field (called `fmt` for FPU ops):

| rs/fmt | Mnemonic | Description |
|--------|----------|-------------|
| 0x00 | MFC1 | Move From FPU register |
| 0x01 | DMFC1 | Doubleword Move From FPU |
| 0x02 | CFC1 | Move From FPU Control register |
| 0x04 | MTC1 | Move To FPU register |
| 0x05 | DMTC1 | Doubleword Move To FPU |
| 0x06 | CTC1 | Move To FPU Control register |
| 0x08 | BC1 | Branch on FPU condition (decode rt: 0=BC1F, 1=BC1T) |
| 0x10 | S | Single-precision operations (decode funct) |
| 0x11 | D | Double-precision operations (decode funct) |
| 0x14 | W | Word (integer) conversion ops |
| 0x15 | L | Long (64-bit integer) conversion ops |

### FPU funct table (for fmt=S or fmt=D):

| Funct | Mnemonic | Description |
|-------|----------|-------------|
| 0x00 | ADD | FP Add |
| 0x01 | SUB | FP Subtract |
| 0x02 | MUL | FP Multiply |
| 0x03 | DIV | FP Divide |
| 0x04 | SQRT | FP Square Root |
| 0x05 | ABS | FP Absolute Value |
| 0x06 | MOV | FP Move |
| 0x07 | NEG | FP Negate |
| 0x09 | TRUNC.L | Truncate to Long |
| 0x0D | TRUNC.W | Truncate to Word |
| 0x0A | CEIL.L | Ceiling to Long |
| 0x0B | FLOOR.L | Floor to Long |
| 0x0E | CEIL.W | Ceiling to Word |
| 0x0F | FLOOR.W | Floor to Word |
| 0x08 | ROUND.L | Round to Long |
| 0x0C | ROUND.W | Round to Word |
| 0x21 | CVT.D | Convert to Double |
| 0x20 | CVT.S | Convert to Single |
| 0x24 | CVT.W | Convert to Word |
| 0x25 | CVT.L | Convert to Long |
| 0x30-0x3F | C.cond | FP Compare (sets FPU condition flag) |

---

## Instruction Implementation Notes

### Sign Extension Pitfalls

```
ADDIU $t0, $zero, 0xFFFF
```
Despite "Unsigned", the immediate IS sign-extended: `0xFFFF` → `0xFFFF_FFFF_FFFF_FFFF`.
"Unsigned" means no overflow exception, NOT unsigned immediate extension.

The only truly zero-extending immediate ops are: **ANDI, ORI, XORI**.

### Load/Store Addressing

All loads/stores compute the effective address as: `GPR[rs] + sign_extend(offset)`

The address must be naturally aligned:
- LW/SW: 4-byte aligned
- LH/SH: 2-byte aligned
- LD/SD: 8-byte aligned
- LB/SB: no alignment required

Misaligned access causes an Address Error exception. The LWL/LWR/SWL/SWR instructions exist specifically for unaligned access.

### Multiply/Divide Results

MULT/MULTU: 64-bit result goes to HI (upper 32) and LO (lower 32).
DIV/DIVU: LO = quotient, HI = remainder.

DMULT/DMULTU: 128-bit result, HI = upper 64, LO = lower 64.

Results are read back with MFHI/MFLO.

### Branch Target Calculation

For I-type branches (BEQ, BNE, etc.):
```
target = PC + 4 + (sign_extend(offset) << 2)
```
Note: offset is relative to the instruction AFTER the branch (PC+4), not the branch itself.

For J-type jumps (J, JAL):
```
target = (PC & 0xF000_0000) | (target_field << 2)
```
The upper 4 bits come from the current PC. This means J/JAL can only jump within the current 256MB region.

### Priority of Instructions to Implement

For booting N64 games, implement in roughly this order:
1. **LUI, ORI, ADDIU** — register loading (used immediately by boot code)
2. **SW, LW** — memory access
3. **BNE, BEQ, J** — control flow
4. **SLL, ADDU, AND, OR** — basic ALU
5. **JR, JAL, JALR** — function calls
6. **SLT, SLTU, SLTI** — comparisons
7. **SRL, SRA, SUBU, NOR, XOR** — remaining ALU
8. **LB, LBU, LH, LHU, SB, SH** — byte/halfword access
9. **MULT, MULTU, DIV, DIVU, MFHI, MFLO** — arithmetic
10. **MFC0, MTC0** — COP0 access (needed for interrupts)
11. **Branches**: BGEZ, BLTZ, BGTZ, BLEZ, and "likely" variants
12. **64-bit ops**: LD, SD, DADDU, DSLL, DSLL32, etc.
13. **FPU**: MFC1, MTC1, ADD.S, MUL.S, etc. (needed for 3D games)
