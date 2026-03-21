# Gauntlet Legends — Debugging Notes

ROM: `test_roms/gauntlet-legends.z64`
Game code: NGXE
CIC: 6102

## Status

The game boots past OS init and begins its main loop. It generates a few GFX
frames before stalling. The root cause is a Midway-proprietary OS that manages
RSP tasks through a custom ring buffer + server thread architecture that differs
substantially from libultra.

## Midway OS Architecture

### Server Thread

The main game server runs in a loop reading messages from a circular queue at
`E012EDC8` (phys `0x0012EDC8`). Queue descriptor layout:

| Offset | Field      | Value          |
|--------|------------|----------------|
| +0x00  | obj_ptr    | E00A6AC0       |
| +0x04  | obj_ptr    | E00A6AC0       |
| +0x08  | count      | (runtime)      |
| +0x0C  | tail       | (runtime)      |
| +0x10  | capacity   | 60 (0x3C)      |
| +0x14  | buffer_ptr | E0128A00       |

Entries are **pointers** to message structs where offset 0 is a halfword code.

Key message codes:
- `0x0001` — retrace notification
- `0x0002` — unknown (follows 0x0004)
- `0x0004` — reply from ABE handler
- `0x0ABB` — CF90 clear trigger
- `0x0ABD` — "frame ready" / init trigger
- `0x0ABE` — VI retrace frame timer event

### Server State Machine (register s2)

The server uses MIPS register `s2` as its primary state variable:

- `s2 = -1`: idle, waiting for work
- `s2 = 0`: received ABE, sent reply, awaiting 0x0ABD
- `s2 = 1`: processing 0x0ABD init path
- `s2 >= 2`: extended init path (sends 0x0ABB, clears CF90)

Flow:
1. ABE arrives → reply mechanism runs → s2 becomes 0
2. 0x0ABD arrives → s2 increments (+1) → if s2 >= 0, enter init path
3. Init path: sets CF90=1, processes sub-messages, eventually sets s2 = -1

### Ring Buffer (0x029A–0x029F)

Descriptor at `E01277B0` (phys `0x001277B0`):

| Offset | Field      | Value     |
|--------|------------|-----------|
| +0x00  | sched_ptr  | E00A6AC0  |
| +0x04  | sched_ptr  | E00A6AC0  |
| +0x08  | count      | (runtime) |
| +0x0C  | tail       | (runtime) |
| +0x10  | capacity   | 8         |
| +0x14  | buffer_ptr | E01277C8  |

Message codes 0x029A through 0x029F are enqueued here.
- `0x029A` — periodic (retrace-related)
- `0x029E` — RSP task event
- `0x029F` — GFX completion event

### Ring Buffer Notification (the core problem)

The enqueue function at `E0099B30` checks `[desc[0]][0]` for a callback
function pointer. In Gauntlet, `desc[0] = E00A6AC0`, and `[E00A6AC0][0] = 0`
(**NULL**). The callback is **never initialized** — BSS clear writes zero, and
no game code ever sets it.

Because the callback is NULL, the ring buffer notification is skipped. On real
hardware, this callback would presumably send message `0x0ABD` to the server
queue after GFX completion. Without it, the server never receives the "frame
ready" signal.

### Consumer Thread

At `E00077A4`, polls the ring buffer and dispatches via jump table at `E00A8680`:

| Index | Code   | Handler  |
|-------|--------|----------|
| 0     | 0x029A | E0007804 |
| 1     | 0x029B | E0007814 |
| 2     | 0x029C | E0007824 |
| 3     | 0x029D | E0007874 |
| 4     | 0x029E | E0007834 |
| 5     | 0x029F | E0007844 |

The `0x029F` handler at `E0007844`:
1. Checks `*(E0133A50)` and `*(E00FCFC4)` — if either non-zero, discards
2. If both zero, calls `E000F580` (frame timing calculation, ~140 insns)
3. `E000F580` is pure computation — stores timing values, returns
4. Does NOT send any messages to the server

### ABE Handler

When the server receives `0x0ABE`:
1. Reads CF90 flag at `E00FCF90` (phys `0x000FCF90`)
2. If CF90=0, computes `f20` from timer value `0x3C888889` (= 1/60 sec)
3. REPLY_DECISION: compares f20 vs f0. If `f20 >= f0` → cond=true → send reply
4. If CF90=1, f20 stays at 0 → cond always false → no reply → game stalls

### CF90 Flag

Physical address: `0x000FCF90` (virtual `E00FCF90`).
NOTE: `lui v0, 0xE010` + `lw v0, 0xCF90(v0)` uses **signed** 16-bit offset:
`0xCF90 = -12400`, so address = `0xE0100000 - 12400 = 0xE00FCFC4`. The physical
address is `0x000FCF90`, NOT `0x0010CF90`.

- Set to 1 by init path at `E0003134` during 0x0ABD processing
- Cleared to 0 by `E0003354` (`sw zero, 0(s8)`) during 0x0ABB processing
- 0x0ABB flow only triggers when s2 reaches 2 (double 0x0ABD injection)

### CFC4 Flag

Physical address: `0x000FCFC4` (virtual `E00FCFC4`).
Same signed-offset issue as CF90. Gate flag for the 0x029F handler — when
non-zero, the handler discards the message.

## What Was Tried

### 1. Bootstrap 0x0ABD Injection (partially working)

Injected message `0x0ABD` into the server's message queue at GFX completion.
Used a fake message struct at `0x003FFF00` (phys) with code `0x0ABD` as the
upper halfword.

**Results:**
- First frame works (ABE → reply → game generates GFX)
- Init path sets CF90=1, blocking subsequent ABE replies
- After init path, game's own mechanisms generate natural 0x0ABDs
- But CF90=1 persists — only cleared via 0x0ABB (requires s2=2 / double injection)

### 2. Deferred Injection (200k cycles)

Delayed injection to avoid preempting REPLY messages.

**Results:**
- Natural 0x0ABD from game + our delayed injection = double → triggers 0x0ABB → CF90 cleared
- Worked for 3 GFX frames
- Eventually s2 drifts (game sees s2=0 at ABE time instead of -1)
- After long gap (7+ retraces with no GFX), double ABE fires before 0x0ABD arrives

### 3. CF90 Clear at VI Retrace

Clear `0x000FCF90` at every VI interrupt to ensure ABE handler sees CF90=0.

**Results:**
- Works for first few frames
- Init path can set CF90=1 after retrace clear but before next ABE
- Timing-dependent: if ABE arrives within same retrace interval as init, sees CF90=1

### 4. CF90 Write Suppression

Intercept writes to `0x000FCF90` and suppress non-zero values.

**Status:** Not fully tested. Risk: init path reads CF90 at `E0003520`/`E0003530`
and branches based on value. Forcing CF90=0 might change init path behavior.

## Key Leads for Future Work

### The Real 0x0ABD Producer

On real hardware, something sends `0x0ABD` to the server after GFX completion.
The scheduler callback at `E00A6AC0[0]` is NULL and never set. The consumer
thread's 0x029F handler calls `E000F580` (timing only, no messages). After a
successful REPLY_DECISION (cond=true), the game's own code generates a natural
`0x0ABD` within ~3-4k CPU cycles — but only after the init path has run at least
once.

**Best theory:** The init path (triggered by the first 0x0ABD) sets up state
that enables the REPLY mechanism to generate subsequent 0x0ABDs. The bootstrap
0x0ABD is what's missing in HLE — on real hardware, it likely comes from a
hardware event (RSP completion timing, DMA completion, or similar) that our
instant HLE skips.

### Thread Scheduling Issue

The Midway OS appears to be multi-threaded. In the single-threaded CPU emulation,
the server processes messages without yielding between them. On real hardware,
context switches between server and game threads allow the game to enqueue
responses between server messages. This explains why `0x0004` and `0x0002`
arrive 144 cycles apart after a long gap (server processes both without yielding)
vs 46k cycles apart normally (game thread runs between them).

### GLideN64 Precedent

GLideN64's 2017 release notes say Gauntlet HLE support required "changes not
only in graphics plugin, but also in core and RSP." The ares emulator also had
Gauntlet hanging in the intro. This is a known hard case.

### Possible Approaches

1. **Identify the bootstrap event**: What hardware-visible event on real N64
   sends the first `0x0ABD`? Could be RSP completion timing, RDP status change,
   or a timer-based mechanism.

2. **Emulate the scheduler callback**: Instead of injecting into the server queue,
   make the ring buffer enqueue function's callback work. Write a valid function
   pointer to `E00A6AC0[0]` that sends `0x0ABD`.

3. **CF90 suppression**: May work if the init path doesn't need CF90=1 for its
   branch logic. Needs testing of what `E0003520`/`E0003530` (CF90 reads during
   init path) actually branch on.

4. **Per-frame 0x0ABD with s2 monitoring**: Inject 0x0ABD only when s2 is at -1
   (ready for next frame). Requires reading s2 from the CPU register file during
   injection, which is architecturally unclean.

## Scheduler Object Dump

```
@0x000A6AC0: 0x00000000 0xFFFFFFFF 0xE0132CA8 0xE00E4FE0
             0xE0128210 0x00000000 0x00000000 0x00000000
             0xE0131C60 0x00000000 0x00000000 0x00000000
             0x00000000 0x00000000 0x00000000 0x00000000
```

Offset 0 = callback function pointer (NULL)
Offset 4 = 0xFFFFFFFF (sentinel / uninitialized)
Offset 8 = 0xE0132CA8 (data pointer)
Offset 12 = 0xE00E4FE0 (function pointer)

## Key Addresses

| Address (virt) | Address (phys) | Description |
|---------------|----------------|-------------|
| E00077A4 | 0x000077A4 | Consumer thread entry |
| E0003134 | 0x00003134 | Init path — sets CF90=1 |
| E00031C8 | 0x000031C8 | `addiu v0, zero, 0x0ABB` |
| E0003354 | 0x00003354 | `sw zero, 0(s8)` — clears CF90 |
| E0003520 | 0x00003520 | `lw v0, 0xCF90(v0)` — reads CF90 |
| E0007844 | 0x00007844 | 0x029F handler |
| E000F580 | 0x0000F580 | Frame timing calculation |
| E0099A00 | 0x00099A00 | Ring buffer dequeue |
| E0099B30 | 0x00099B30 | Ring buffer enqueue |
| E009F64C | 0x0009F64C | RSP task submission PC |
| E00A6AC0 | 0x000A6AC0 | Scheduler object |
| E00A8680 | 0x000A8680 | Consumer jump table |
| E0128A00 | 0x00128A00 | Server queue buffer |
| E012EDC8 | 0x0012EDC8 | Server queue descriptor |
| E01277B0 | 0x001277B0 | Ring buffer descriptor |
| E00FCF90 | 0x000FCF90 | CF90 flag |
| E00FCFC4 | 0x000FCFC4 | CFC4 gate flag |
