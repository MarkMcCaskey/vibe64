# Dynarec Current Work and Performance Backlog

Last updated: 2026-02-22

Architecture and diagrams live in `doc/dynarec.md`.

## 1. Replay Snapshot

Measured on local replay benches:

- `replays/load_game_and_exit_house.replay`
  - `warmup=60`, `frames=900`
  - `fps=102.527`, `mcycles_s=160.508`
- `replays/oot_forest_escape.replay`
  - `warmup=120`, `frames=1800`
  - `fps=99.686`, `mcycles_s=156.077`

Notable counters from the longer replay:

- `native_instr=2,772,716,762`
- `fallback_instr=45,530,049` (~1.6% of retired instructions)
- `native_link_hits=39,755,243`
- `native_link_misses=4,527,877` (~89.8% hit ratio)
- `ensure_async_queue_full=4,266`
- `ensure_async_dropped_stale=3,989`
- `promote_async_queue_full=714`
- `promote_async_dropped_stale=638`
- `ensure_calls=0` (no sync tier1 compiles in this run)

Current interpretation:

- Steady-state native execution is strong.
- Most remaining waste is async compile churn (queue-full + stale drops) and invalidation noise from store-heavy paths.

## 2. Encapsulated Wins (Prioritized)

### P0: Async enqueue churn reduction

Scope:

- `crates/n64-core/src/jit/dynarec.rs`

Changes:

- Skip enqueue when cached block already satisfies requested instruction budget.
- Keep one pending job per `start_phys`, but allow "upgrade in place" when a higher-budget request appears.
- Prefer tier1 admission over promotion when queue is saturated.

Success criteria:

- Lower `ensure_async_queue_full`, `ensure_async_dropped_stale`, and `promote_async_dropped_stale`.
- Keep or improve replay FPS.

### P0: Cheaper fast-store invalidation aggregation

Scope:

- `run_native_block` invalidation merge path in `crates/n64-core/src/jit/dynarec.rs`

Changes:

- Replace per-block `SmallVec + sort/merge` with page-dedup accumulation.
- Emit one coalesced invalidate span per page run.
- Avoid redundant overlap probes for duplicate writes in the same page(s).

Success criteria:

- Fewer `invalidate_calls` and lower epilogue overhead.
- Same correctness and same invalidated-block behavior.

### P1: Compile-path allocation/finalization cleanup

Scope:

- `crates/n64-dynarec/src/lib.rs`

Changes:

- Reuse transient decode/lowering allocations.
- Audit Cranelift finalize cadence and symbol lifecycle to avoid avoidable global finalization work.

Success criteria:

- Lower promotion compile-time aggregate (`promote_time_us`) without correctness regressions.

### P1: Partial-spill path for `Op::Interp`

Scope:

- `Op::Interp` lowering in `crates/n64-dynarec/src/lib.rs`

Changes:

- Keep full-spill as default safety path.
- Add a conservative whitelist where only touched GPR subsets are spilled/reloaded.

Success criteria:

- Lower fallback overhead on mixed native/interp traces.
- Differential tests remain green.

### P2: Native link hit-path micro-optimization

Scope:

- `native_links` lookup in `crates/n64-core/src/jit/dynarec.rs`

Changes:

- Add one-entry last-target fast path before scanning fanout vector.

Success criteria:

- Lower overhead per link hit with unchanged hit/miss correctness.

## 3. Recommended Execution Order

1. P0 async enqueue churn reduction.
2. P0 invalidation aggregation rewrite.
3. P1 compile-path allocation/finalization cleanup.
4. P1 partial-spill path for `Op::Interp`.
5. P2 native-link hit fast path.

## 4. Longer-Horizon Items

- Precise in-chunk interrupt-event accounting to safely raise gas caps.
- Faster MMIO classification for store slow paths.
- Expand direct fastmem coverage for more memory operations.
- Block-formation heuristics around interrupt-arming stores/branches.
- Differential gas-invariance matrix on replay windows.
