# Dynarec Plan (Cranelift-First)

## Scope

The dynarec system is introduced behind feature gates and currently starts with one compiler backend implementation:

- `CraneliftCompiler` (staged, metadata/block formation + cache/invalidation)

Execution remains interpreter-backed for safety while the compile/cache pipeline is validated.

## Feature Gating

In `n64-core`:

- `dynarec`: enables dynarec integration and the `n64-dynarec` crate.

Default builds do not include dynarec.

## Runtime Selection

`N64` now picks the engine at startup:

- default: interpreter
- dynarec: set `N64_DYNAREC=1` (or `on`, `true`, `cranelift`) when built with `n64-core/dynarec`

Example:

```sh
N64_DYNAREC=1 cargo run -p n64-frontend --release --features n64-core/dynarec -- test_roms/zelda-oot.z64
```

## Coherency Model

Code-cache invalidation is now wired for writes that can modify executable RDRAM:

- CPU stores to RDRAM (`write_u32` path)
- PI DMA to RDRAM
- SP DMA writes to RDRAM
- SI DMA reads into RDRAM

`Interconnect` accumulates invalidation ranges and `N64` drains them after each CPU step/frame tick to invalidate the active engine cache.

## Testing Strategy

Current tests include:

- `n64-dynarec` unit tests for block formation, cache hits, invalidation, and compile failures.
- `n64-core` differential tests (with `--features dynarec`) comparing dynarec engine behavior to interpreter on:
  - arithmetic + load/store program
  - branch delay-slot semantics

## Next Stage

1. Add Cranelift IR lowering for a safe subset of VR4300 ops.
2. Emit executable blocks and call through typed trampolines.
3. Keep differential tests as a gate for each expanded opcode family.
4. Add performance and correctness regression benches for OOT/SM64 traces.
