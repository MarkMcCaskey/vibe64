# Dynarec Performance Backlog

Shortlist of high-impact follow-ups to revisit after correctness hardening.

## 1) Native Invalidation Queue Without Sort
- Replace per-block `SmallVec + sort/merge` with a fixed page-bitset or interval accumulator.
- Goal: remove hot-path sort overhead on store-heavy code.

## 2) Precise Interrupt Event Accounting In-Chunk
- Current external-event budgeting is intentionally conservative.
- Track event-arm points inside native execution (or split around MMIO-arming stores) so we can raise gas safely without pessimistic early exits.

## 3) Faster MMIO Classification in JIT Stores
- Add a compact fast path for common RDRAM store addresses and a branchless reject path for non-MMIO.
- Goal: reduce callback overhead where most stores are not timer/IRQ relevant.

## 4) Reduce Callback Round-Trips for Memory Ops
- Expand direct fastmem coverage and reduce helper calls for aligned integer/floating memory operations.
- Goal: shrink hostcall overhead in gameplay-heavy loops.

## 5) Tiering Cost Controls
- Revisit promote threshold and async queue policy using gameplay savestates (not synthetic loops).
- Goal: avoid wasted promotes and stale async results.

## 6) Block Formation Around Stores/Branches
- Evaluate selective block splitting around potential interrupt/timer-arm instructions.
- Goal: improve correctness margin without forcing globally low gas.

## 7) Native Link Cache Locality
- Profile link cache miss patterns and tune fanout/eviction strategy.
- Goal: improve chained native residency in hot loops.

## 8) Differential Gas-Invariance Test Matrix
- Add automated tests that compare interpreter vs dynarec across multiple gas values on the same ROM-state windows.
- Goal: keep correctness independent of gas tuning.
