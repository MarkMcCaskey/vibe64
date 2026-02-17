# N64 Physical Memory Map

Complete address map for the N64, as seen from the system bus (after virtual→physical translation by the CPU).

References:
- [Memory Map — N64brew](https://n64brew.dev/wiki/Memory_map)
- [MIPS Interface — N64brew](https://n64brew.dev/wiki/MIPS_Interface)
- [Peripheral Interface — N64brew](https://n64brew.dev/wiki/Peripheral_Interface)
- [Video Interface — N64brew](https://n64brew.dev/wiki/Video_Interface)
- [Audio Interface — N64brew](https://n64brew.dev/wiki/Audio_Interface)
- [Serial Interface — N64brew](https://n64brew.dev/wiki/Serial_Interface)
- [RDRAM Interface — N64brew](https://n64brew.dev/wiki/RDRAM_Interface)
- [RSP — N64brew](https://n64brew.dev/wiki/Reality_Signal_Processor)
- [RDP — N64brew](https://n64brew.dev/wiki/Reality_Display_Processor)

---

## Overview

```
0x0000_0000 ┬─────────────────────────────┐
            │  RDRAM (4 MB / 8 MB)        │  Main system memory
0x03FF_FFFF ┴─────────────────────────────┘
0x0400_0000 ┬─────────────────────────────┐
            │  RSP DMEM (4 KB)            │  Signal Processor data memory
0x0400_0FFF ┴─────────────────────────────┘
0x0400_1000 ┬─────────────────────────────┐
            │  RSP IMEM (4 KB)            │  Signal Processor instruction memory
0x0400_1FFF ┴─────────────────────────────┘
0x0404_0000 ┬─────────────────────────────┐
            │  RSP Registers              │  SP_STATUS, SP_DMA, etc.
0x040F_FFFF ┴─────────────────────────────┘
0x0410_0000 ┬─────────────────────────────┐
            │  RDP Command Registers      │  DP_START, DP_END, DP_STATUS
0x041F_FFFF ┴─────────────────────────────┘
0x0430_0000 ┬─────────────────────────────┐
            │  MI (MIPS Interface)        │  Interrupt controller
0x043F_FFFF ┴─────────────────────────────┘
0x0440_0000 ┬─────────────────────────────┐
            │  VI (Video Interface)       │  Framebuffer, resolution, timing
0x044F_FFFF ┴─────────────────────────────┘
0x0450_0000 ┬─────────────────────────────┐
            │  AI (Audio Interface)       │  DMA audio playback
0x045F_FFFF ┴─────────────────────────────┘
0x0460_0000 ┬─────────────────────────────┐
            │  PI (Peripheral Interface)  │  Cartridge ROM DMA
0x046F_FFFF ┴─────────────────────────────┘
0x0470_0000 ┬─────────────────────────────┐
            │  RI (RDRAM Interface)       │  RDRAM configuration
0x047F_FFFF ┴─────────────────────────────┘
0x0480_0000 ┬─────────────────────────────┐
            │  SI (Serial Interface)      │  Controllers, EEPROM via PIF
0x048F_FFFF ┴─────────────────────────────┘
0x0490_0000 — 0x04FF_FFFF  (unused)
0x0500_0000 — 0x05FF_FFFF  N64DD registers (disk drive, rare)
0x0600_0000 — 0x07FF_FFFF  N64DD IPL ROM
0x0800_0000 — 0x0FFF_FFFF  (unused / SRAM on some carts)
0x1000_0000 ┬─────────────────────────────┐
            │  Cartridge ROM Domain 1     │  Game ROM data (up to ~64 MB)
0x1FBF_FFFF ┴─────────────────────────────┘
0x1FC0_0000 ┬─────────────────────────────┐
            │  PIF Boot ROM (1984 bytes)  │  Initial boot code
0x1FC0_07BF ┴─────────────────────────────┘
0x1FC0_07C0 ┬─────────────────────────────┐
            │  PIF RAM (64 bytes)         │  Controller/EEPROM communication
0x1FC0_07FF ┴─────────────────────────────┘
```

---

## Detailed Register Maps

### RDRAM (0x0000_0000 — 0x03FF_FFFF)

Plain read/write memory. 4 MB on base N64, 8 MB with Expansion Pak.

The RDRAM also has configuration registers at 0x03F0_0000—0x03FF_FFFF (RDRAM registers per module), but these are rarely accessed by games and can be stubbed.

---

### RSP Registers (0x0404_0000 — 0x040F_FFFF)

| Address | Name | R/W | Description |
|---------|------|-----|-------------|
| 0x0404_0000 | SP_MEM_ADDR | RW | RSP DMEM/IMEM address for DMA |
| 0x0404_0004 | SP_DRAM_ADDR | RW | RDRAM address for DMA |
| 0x0404_0008 | SP_RD_LEN | RW | Read DMA length (RDRAM → RSP) |
| 0x0404_000C | SP_WR_LEN | RW | Write DMA length (RSP → RDRAM) |
| 0x0404_0010 | SP_STATUS | RW | RSP status (halt, broke, interrupt, etc.) |
| 0x0404_0014 | SP_DMA_FULL | R | DMA full flag |
| 0x0404_0018 | SP_DMA_BUSY | R | DMA busy flag |
| 0x0404_001C | SP_SEMAPHORE | RW | Hardware semaphore (read returns + sets, write clears) |
| 0x0408_0000 | SP_PC | RW | RSP program counter (12-bit, IMEM offset) |

**SP_STATUS register bits (read):**
- Bit 0: Halt (RSP is halted)
- Bit 1: Broke (RSP hit a BREAK instruction)
- Bit 2: DMA busy
- Bit 3: DMA full
- Bit 4: IO full
- Bit 5: Single step mode
- Bit 6: Interrupt on break
- Bit 7-14: Signal 0-7 (software-defined flags)

**SP_STATUS register bits (write):** Set/clear pairs (bit 0 clears halt, bit 1 sets halt, etc.)

---

### RDP Command Registers (0x0410_0000 — 0x041F_FFFF)

| Address | Name | R/W | Description |
|---------|------|-----|-------------|
| 0x0410_0000 | DPC_START | RW | Start of RDP command buffer in RDRAM |
| 0x0410_0004 | DPC_END | RW | End of RDP command buffer (writing triggers RDP) |
| 0x0410_0008 | DPC_CURRENT | R | Current RDP read position |
| 0x0410_000C | DPC_STATUS | RW | RDP status flags |
| 0x0410_0010 | DPC_CLOCK | R | RDP clock counter |
| 0x0410_0014 | DPC_BUFBUSY | R | Buffer busy counter |
| 0x0410_0018 | DPC_PIPEBUSY | R | Pipe busy counter |
| 0x0410_001C | DPC_TMEM | R | TMEM load counter |

---

### MI — MIPS Interface (0x0430_0000 — 0x043F_FFFF)

The interrupt controller. Aggregates interrupts from all RCP components into a single line to the CPU (COP0 Cause IP2).

| Address | Name | R/W | Description |
|---------|------|-----|-------------|
| 0x0430_0000 | MI_MODE | RW | MI mode (init length, ebus test mode) |
| 0x0430_0004 | MI_VERSION | R | MI version (0x0202_0102 for retail) |
| 0x0430_0008 | MI_INTR | R | Pending interrupts (6 bits) |
| 0x0430_000C | MI_MASK | RW | Interrupt mask (6 bits) |

**MI_INTR bits (active-high, read-only):**

| Bit | Interrupt | Source |
|-----|-----------|--------|
| 0 | SP | RSP finished task or hit BREAK |
| 1 | SI | SI DMA complete |
| 2 | AI | AI DMA buffer empty |
| 3 | VI | VI reaches target scanline (V_INTR) |
| 4 | PI | PI DMA complete |
| 5 | DP | RDP finished rendering |

**MI_MASK (write): Set/clear pairs**
- Bits [1:0]: SP mask (0=clear, 1=set... actually bit 0=clear, bit 1=set)
- Bits [3:2]: SI mask
- Bits [5:4]: AI mask
- Bits [7:6]: VI mask
- Bits [9:8]: PI mask
- Bits [11:10]: DP mask

The MI asserts COP0 Cause IP2 when `(MI_INTR & MI_MASK) != 0`.

---

### VI — Video Interface (0x0440_0000 — 0x044F_FFFF)

Controls video output. The framebuffer is in RDRAM; VI reads from it.

| Address | Name | R/W | Description |
|---------|------|-----|-------------|
| 0x0440_0000 | VI_CTRL | RW | Video mode (pixel size, AA mode, etc.) |
| 0x0440_0004 | VI_ORIGIN | RW | Framebuffer origin in RDRAM (physical addr) |
| 0x0440_0008 | VI_WIDTH | RW | Framebuffer width in pixels |
| 0x0440_000C | VI_V_INTR | RW | Vertical interrupt line (triggers MI VI interrupt) |
| 0x0440_0010 | VI_V_CURRENT | RW | Current scanline (write clears VI interrupt) |
| 0x0440_0014 | VI_BURST | RW | Video burst timing |
| 0x0440_0018 | VI_V_SYNC | RW | Total lines per frame (e.g., 525 for NTSC) |
| 0x0440_001C | VI_H_SYNC | RW | H-sync timing |
| 0x0440_0020 | VI_H_SYNC_LEAP | RW | H-sync leap pattern |
| 0x0440_0024 | VI_H_VIDEO | RW | Horizontal active video range |
| 0x0440_0028 | VI_V_VIDEO | RW | Vertical active video range |
| 0x0440_002C | VI_V_BURST | RW | Vertical burst range |
| 0x0440_0030 | VI_X_SCALE | RW | Horizontal scale factor |
| 0x0440_0034 | VI_Y_SCALE | RW | Vertical scale factor |

**VI_CTRL pixel size (bits [1:0]):**
- 0: Blank (no data)
- 1: Reserved
- 2: 16-bit (5/5/5/1 RGBA)
- 3: 32-bit (8/8/8/8 RGBA)

Common N64 resolutions: 320x240 (most games), 640x480 (rare, e.g., some menus).

---

### AI — Audio Interface (0x0450_0000 — 0x045F_FFFF)

DMA-based audio output. Double-buffered.

| Address | Name | R/W | Description |
|---------|------|-----|-------------|
| 0x0450_0000 | AI_DRAM_ADDR | W | RDRAM address of audio data |
| 0x0450_0004 | AI_LEN | RW | R: remaining DMA length; W: start DMA with this length |
| 0x0450_0008 | AI_CONTROL | W | Bit 0: DMA enable |
| 0x0450_000C | AI_STATUS | RW | R: status flags; W: clear AI interrupt |
| 0x0450_0010 | AI_DACRATE | W | DAC sample rate = VIClock / (value + 1) |
| 0x0450_0014 | AI_BITRATE | W | Bit rate (usually 15 for 16-bit stereo) |

---

### PI — Peripheral Interface (0x0460_0000 — 0x046F_FFFF)

Handles DMA between cartridge ROM and RDRAM. **This is critical for emulation** — it's how games load data from the cartridge.

| Address | Name | R/W | Description |
|---------|------|-----|-------------|
| 0x0460_0000 | PI_DRAM_ADDR | RW | RDRAM address for DMA |
| 0x0460_0004 | PI_CART_ADDR | RW | Cartridge address for DMA |
| 0x0460_0008 | PI_RD_LEN | RW | DMA length: RDRAM → Cart (rarely used) |
| 0x0460_000C | PI_WR_LEN | RW | DMA length: Cart → RDRAM (write triggers DMA!) |
| 0x0460_0010 | PI_STATUS | RW | R: DMA busy/IO busy; W: reset/clear interrupt |
| 0x0460_0014 | PI_BSD_DOM1_LAT | RW | Domain 1 latency |
| 0x0460_0018 | PI_BSD_DOM1_PWD | RW | Domain 1 pulse width |
| 0x0460_001C | PI_BSD_DOM1_PGS | RW | Domain 1 page size |
| 0x0460_0020 | PI_BSD_DOM1_RLS | RW | Domain 1 release |
| 0x0460_0024 | PI_BSD_DOM2_LAT | RW | Domain 2 latency |
| 0x0460_0028 | PI_BSD_DOM2_PWD | RW | Domain 2 pulse width |
| 0x0460_002C | PI_BSD_DOM2_PGS | RW | Domain 2 page size |
| 0x0460_0030 | PI_BSD_DOM2_RLS | RW | Domain 2 release |

**Key behavior**: Writing to `PI_WR_LEN` triggers a DMA transfer of `(value + 1)` bytes from `PI_CART_ADDR` to `PI_DRAM_ADDR`. On completion, PI raises MI interrupt bit 4 (PI).

**PI_STATUS (read):**
- Bit 0: DMA busy
- Bit 1: IO busy
- Bit 2: DMA error

**PI_STATUS (write):**
- Bit 0: Reset DMA controller
- Bit 1: Clear PI interrupt

**JIT note**: Every PI DMA write is a potential code invalidation event. After DMA completes, the JIT should invalidate any compiled blocks in the range `[PI_DRAM_ADDR, PI_DRAM_ADDR + len)`.

---

### RI — RDRAM Interface (0x0470_0000 — 0x047F_FFFF)

Configures RDRAM timing. Rarely accessed by games; can be stubbed with safe defaults.

| Address | Name | R/W | Description |
|---------|------|-----|-------------|
| 0x0470_0000 | RI_MODE | RW | RDRAM operating mode |
| 0x0470_0004 | RI_CONFIG | RW | RDRAM configuration |
| 0x0470_0008 | RI_CURRENT_LOAD | W | Force load RDRAM timing |
| 0x0470_000C | RI_SELECT | RW | Select RDRAM bank/module |
| 0x0470_0010 | RI_REFRESH | RW | RDRAM refresh rate |
| 0x0470_0014 | RI_LATENCY | RW | RDRAM latency |
| 0x0470_0018 | RI_RERROR | R | RDRAM error status |
| 0x0470_001C | RI_WERROR | W | RDRAM write error |

---

### SI — Serial Interface (0x0480_0000 — 0x048F_FFFF)

Handles communication with controllers and EEPROM through PIF RAM.

| Address | Name | R/W | Description |
|---------|------|-----|-------------|
| 0x0480_0000 | SI_DRAM_ADDR | RW | RDRAM address for SI DMA |
| 0x0480_0004 | SI_PIF_AD_RD64B | W | Write triggers PIF→RDRAM DMA (64 bytes) |
| 0x0480_0010 | SI_PIF_AD_WR64B | W | Write triggers RDRAM→PIF DMA (64 bytes) |
| 0x0480_0018 | SI_STATUS | RW | R: status; W: clear SI interrupt |

**SI_STATUS (read):**
- Bit 0: DMA busy
- Bit 1: IO read busy
- Bit 3: DMA error
- Bit 12: Interrupt pending

The SI DMAs 64 bytes between RDRAM and PIF RAM. Games write controller commands to PIF RAM, trigger SI DMA, and read back responses.

---

### PIF (0x1FC0_0000 — 0x1FC0_07FF)

| Range | Size | Description |
|-------|------|-------------|
| 0x1FC0_0000 — 0x1FC0_07BF | 1984 bytes | PIF Boot ROM (read-only) |
| 0x1FC0_07C0 — 0x1FC0_07FF | 64 bytes | PIF RAM (controller/EEPROM channel) |

The PIF RAM is structured as a series of commands:
- Bytes 0-63: Channel data (controller commands and responses)
- Last byte (0x3F): Status/control byte

Controller command format in PIF RAM:
```
[TX length] [RX length] [command bytes...] [response bytes...]
```

Common controller commands:
- 0x00: Get status (returns controller type + pak status)
- 0x01: Read buttons (returns 4 bytes of button state)
- 0x02: Read from controller pak
- 0x03: Write to controller pak

---

## Virtual Address Segments (CPU-side)

The CPU uses 64-bit virtual addresses. In 32-bit compatibility mode (most N64 games), the upper 32 bits are sign-extended from bit 31.

```
kuseg:  0x0000_0000 — 0x7FFF_FFFF → TLB mapped (user mode)
kseg0:  0x8000_0000 — 0x9FFF_FFFF → physical = vaddr - 0x8000_0000 (cached)
kseg1:  0xA000_0000 — 0xBFFF_FFFF → physical = vaddr - 0xA000_0000 (uncached)
ksseg:  0xC000_0000 — 0xDFFF_FFFF → TLB mapped (supervisor mode)
kseg2:  0xE000_0000 — 0xFFFF_FFFF → TLB mapped (kernel mode)
```

**kseg0 and kseg1 map to the same physical memory.** The only difference is cache behavior. Emulators typically don't model caching, so they're functionally identical.

Common virtual addresses in N64 games:
- `0x8000_0000` = RDRAM start (cached)
- `0x8000_0400` = Common game entry point
- `0xA000_0000` = RDRAM start (uncached)
- `0xA400_0000` = RSP DMEM (uncached)
- `0xA400_0040` = Boot code entry point (DMEM + 0x40, uncached)
- `0xA460_0000` = PI registers (uncached)
- `0xB000_0000` = Cartridge ROM (uncached)
- `0xBFC0_0000` = PIF Boot ROM (uncached, reset vector)

Games typically run code from kseg0 (cached) but access hardware registers through kseg1 (uncached) to bypass the cache.
