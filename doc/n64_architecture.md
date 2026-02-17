# N64 Hardware Architecture Reference

## Overview

The Nintendo 64 (1996) is built around two main chips:
- **NEC VR4300** — The main CPU (a cost-reduced MIPS R4300i)
- **RCP (Reality Co-Processor)** — Contains the RSP and RDP for audio/graphics

References:
- [N64brew Wiki — VR4300](https://n64brew.dev/wiki/VR4300)
- [N64brew Wiki — Reality Co-Processor](https://n64brew.dev/wiki/Reality_Co-Processor)
- [N64brew Wiki — Memory map](https://n64brew.dev/wiki/Memory_map)
- [MIPS R4300i Datasheet](https://www.nec.co.jp/products/device/featured/r4300i/)

---

## CPU: NEC VR4300 (MIPS III)

The VR4300 is a 64-bit MIPS III processor running at **93.75 MHz**.

### Registers

| Register Set | Count | Width | Notes |
|---|---|---|---|
| GPR (General Purpose) | 32 | 64-bit | `$zero` (r0) is hardwired to 0 |
| PC (Program Counter) | 1 | 64-bit | Virtual address |
| HI / LO | 2 | 64-bit | Multiply/divide results |
| COP0 (System Control) | 32 | 64-bit | TLB, exceptions, interrupts, timer |
| COP1 (FPU) | 32 | 64-bit | Floating point; also FCR0, FCR31 |

### Pipeline

5-stage in-order pipeline: **IF → ID → EX → MEM → WB**

For an interpreter, we treat instructions as single-step. The pipeline matters for:
- **Branch delay slots**: The instruction after a branch ALWAYS executes before the branch takes effect
- **Load delay slots**: On real hardware there's a 1-cycle delay after loads, but most N64 games (and the compiler) account for this. Many emulators ignore load delays.

Reference: [VR4300 Pipeline — N64brew](https://n64brew.dev/wiki/VR4300#Pipeline)

### Branch Delay Slots

This is the single most important MIPS quirk for emulator developers:

```
BNE $t0, $zero, target   ; branch instruction
ADDIU $t1, $zero, 1      ; delay slot — ALWAYS executes, even if branch taken
```

Implementation strategy: maintain `pc` and `next_pc`. Branch instructions modify `next_pc`. After every instruction: `pc = next_pc; next_pc += 4`.

### 32-bit Compatibility Mode

The VR4300 is 64-bit, but nearly all N64 games run in 32-bit mode (Status register KSU=0, UX/SX/KX=0). The critical rule:

**Any 32-bit operation must sign-extend its result to 64 bits in the register.**

Example: `ADDU` computes a 32-bit sum, then sign-extends bits [31:0] to fill the full 64-bit register. Forgetting this causes address translation bugs because a 32-bit address like `0x8000_0400` must become `0xFFFF_FFFF_8000_0400` in the 64-bit register.

Reference: [MIPS III ISA — Sign Extension](https://n64brew.dev/wiki/MIPS_III_instructions#Sign_Extension)

### COP0 — System Control Coprocessor

COP0 manages TLB, exceptions, interrupts, and system configuration.

Key registers:

| Index | Name | Purpose |
|---|---|---|
| 0 | Index | TLB index for TLBWI/TLBR |
| 1 | Random | Decrements each cycle; TLBWR writes here |
| 2 | EntryLo0 | TLB even page entry (PFN + flags) |
| 3 | EntryLo1 | TLB odd page entry |
| 5 | PageMask | TLB page size mask |
| 8 | BadVAddr | Faulting virtual address |
| 9 | Count | Timer (increments every other CPU cycle) |
| 10 | EntryHi | TLB virtual page + ASID |
| 11 | Compare | Timer interrupt fires when Count == Compare |
| 12 | Status | Operating mode, interrupt enables, ERL/EXL bits |
| 13 | Cause | Exception code, pending interrupt bits |
| 14 | EPC | Exception return address |
| 15 | PRId | Processor ID (0x0B22 for VR4300) |
| 16 | Config | Cache/endianness config |
| 30 | ErrorEPC | Error exception return address |

Reference: [VR4300 COP0 — N64brew](https://n64brew.dev/wiki/VR4300/COP0)

### Virtual Address Translation

The MIPS architecture divides the 32-bit virtual address space into segments:

```
0x0000_0000 — 0x7FFF_FFFF  kuseg   (2 GB, TLB mapped, user mode)
0x8000_0000 — 0x9FFF_FFFF  kseg0   (512 MB, direct map, CACHED)
0xA000_0000 — 0xBFFF_FFFF  kseg1   (512 MB, direct map, UNCACHED)
0xC000_0000 — 0xDFFF_FFFF  ksseg   (512 MB, TLB mapped, supervisor)
0xE000_0000 — 0xFFFF_FFFF  kseg2   (512 MB, TLB mapped, kernel)
```

**kseg0** and **kseg1** are the workhorses. Both map to physical `0x0000_0000 — 0x1FFF_FFFF` by simply stripping the top 3 bits. The only difference is caching behavior (which we don't model in an interpreter).

Most N64 code runs in kseg0/kseg1, so TLB is rarely exercised. But some games (e.g., GoldenEye, Perfect Dark) do use it.

Reference: [VR4300 TLB — N64brew](https://n64brew.dev/wiki/VR4300/TLB)

### TLB (Translation Lookaside Buffer)

- **32 entries**, fully associative
- Variable page sizes: 4KB, 16KB, 64KB, 256KB, 1MB, 4MB, 16MB
- Each entry maps a pair of pages (even/odd) via EntryLo0/EntryLo1
- ASID (Address Space Identifier) support for process isolation (rarely used on N64)

TLB instructions:
- `TLBR` — Read entry at Index
- `TLBWI` — Write entry at Index
- `TLBWR` — Write entry at Random
- `TLBP` — Probe for matching entry

### Interrupts

External interrupts arrive via COP0 Cause register bits IP[7:2]:
- IP2 (bit 10): RCP interrupt (from MI — aggregates SP, SI, AI, VI, PI, DP)
- IP7 (bit 15): Timer interrupt (Count == Compare)

Interrupt handling: if `Status.IE=1` and `Status.EXL=0` and `Status.ERL=0` and `(Cause.IP & Status.IM) != 0`, take exception at vector `0x8000_0180`.

---

## RCP: Reality Co-Processor

The RCP contains two sub-processors and multiple hardware interfaces.

### RSP (Reality Signal Processor)

A custom MIPS-like processor with:
- **Scalar unit**: Subset of MIPS (32-bit, no multiply/divide, no floating point)
- **Vector unit**: 32 x 128-bit vector registers, SIMD operations
- **4 KB DMEM** (data memory) at physical `0x0400_0000`
- **4 KB IMEM** (instruction memory) at physical `0x0400_1000`
- Runs **microcode** loaded by the game for audio/graphics processing

The RSP processes display lists and audio, then DMAs results to RDRAM. Games ship microcode in the ROM (part of the N64 SDK's "libultra" library).

For initial emulation, the RSP can be stubbed. HLE (High-Level Emulation) replaces RSP microcode with native implementations — this is what most N64 emulators do.

Reference: [RSP — N64brew](https://n64brew.dev/wiki/Reality_Signal_Processor)

### RDP (Reality Display Processor)

The hardware rasterizer. It receives commands from the RSP via:
- **XBUS**: Direct RSP→RDP path (RSP writes commands to RDP registers)
- **RDRAM**: RDP reads command lists from RDRAM

The RDP handles: triangle rasterization, texture mapping, Z-buffering, anti-aliasing, color combining, blending.

For initial emulation, RDP can be stubbed (no rendering). Later, options are:
- Software renderer (accurate but slow)
- Hardware-accelerated (translate RDP commands to OpenGL/Vulkan)
- Use an existing plugin like paraLLEl-RDP

Reference: [RDP — N64brew](https://n64brew.dev/wiki/Reality_Display_Processor)

---

## Boot Sequence

1. **PIF ROM** (at physical `0x1FC0_0000`, 1984 bytes) initializes hardware
2. PIF copies **first 4096 bytes of cartridge ROM** to RSP DMEM (`0x0400_0000`)
3. PIF sets initial CPU register values (varies by CIC security chip)
4. CPU begins execution at `0xA4000040` (kseg1 → physical `0x04000040` = DMEM + 0x40)
5. The **IPL3 bootloader** (in the first 4KB of ROM, after the 64-byte header) runs:
   - Copies game code from ROM to RDRAM via PI DMA
   - Jumps to the ROM header's entry point (typically `0x80000400`)

### CIC (Copy Protection)

The CIC chip in the cartridge handshakes with the PIF. Different CIC variants (6101, 6102, 6103, 6105, 6106) set different initial register values. For emulation, we can detect the CIC from the ROM's boot code CRC and set registers accordingly.

Common CIC-6102 initial values:
- `$s4` (r20) = `0x0000_0001`
- `$s6` (r22) = `0x0000_003F`
- `$sp` (r29) = `0xA400_1FF0`

Reference: [PIF — N64brew](https://n64brew.dev/wiki/PIF-NUS)

---

## Cartridge ROM

### Format Detection

Three byte orderings exist. Detected by the first 4 bytes:

| Format | Extension | Magic Bytes | Description |
|---|---|---|---|
| Big-endian (native) | .z64 | `80 37 12 40` | No conversion needed |
| Byte-swapped | .v64 | `37 80 40 12` | Every 2 bytes swapped |
| Little-endian | .n64 | `40 12 37 80` | Every 4 bytes reversed |

### ROM Header (first 64 bytes, after normalization to big-endian)

| Offset | Size | Field |
|---|---|---|
| 0x00 | 4 | PI BSD DOM1 config (also contains magic) |
| 0x04 | 4 | Clock rate override (0 = default 62.5 MHz) |
| 0x08 | 4 | Entry point (virtual address) |
| 0x0C | 4 | Release/version |
| 0x10 | 4 | CRC1 |
| 0x14 | 4 | CRC2 |
| 0x18 | 8 | Reserved |
| 0x20 | 20 | Game name (ASCII, space-padded) |
| 0x34 | 4 | Reserved |
| 0x38 | 4 | Game code (media + cart ID + country) |
| 0x3C | 1 | ROM version |
| 0x3D | 3 | Reserved |

Reference: [ROM Header — en64 wiki](https://en64.shoutwiki.com/wiki/ROM#Format)

---

## Self-Modifying Code & JIT Feasibility

Self-modifying code on the N64 occurs primarily through:
1. **DMA overlays**: Games load code segments from ROM to RDRAM at runtime
2. **Boot code**: IPL3 copies/decompresses game code
3. **RSP microcode**: DMA'd to IMEM (separate address space, not an issue for CPU JIT)

Direct store-to-instruction-stream SMC (like `SW` to an address that's about to be executed) is **rare** on N64. This makes JIT very practical:

- **Intercept DMA**: When PI or SP DMA writes to RDRAM, invalidate JIT blocks in that range
- **Page-level tracking**: Divide RDRAM into 4KB pages. Track which pages have compiled blocks. On DMA write, only scan affected pages.
- **Physical address keying**: JIT blocks are keyed by physical RDRAM address, not virtual. This way kseg0 and kseg1 accesses share the same compiled code.

Proven by: mupen64plus (dynamic recompiler), ares (JIT), Project64 (JIT).

Reference: [Experiments in NES JIT — Brook Heisler](https://bheisler.github.io/post/experiments-in-nes-jit-compilation/) (concepts apply broadly)
