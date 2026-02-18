# HLE Rendering Architecture

## Overview

This emulator uses **High-Level Emulation (HLE)** for graphics: instead of
running RSP microcode cycle-by-cycle, we intercept the RSP task start and
interpret the F3DEX2 display list directly in Rust. A software rasterizer
writes pixels to RDRAM in the format the VI reads for display.

```
Game Code → CPU writes display list → SP_STATUS triggers task
         → HLE intercepts: reads OSTask header from DMEM
         → process_display_list() walks GBI commands
         → Renderer methods update state / rasterize
         → Pixels written directly to RDRAM framebuffer
         → VI reads RDRAM for display
```

### Why HLE?

Real RSP microcode is 4KB of custom MIPS-like code running on a specialized
vector processor. Full LLE (Low-Level Emulation) requires a complete RSP
interpreter with 128-bit SIMD and is significantly more work. HLE works well
for standard SDK microcodes (F3DEX2, S2DEX) that most games use.

---

## File Layout

| File | Purpose |
|------|---------|
| `rcp/gbi.rs` | GBI command dispatch — walks display list, decodes opcodes |
| `rcp/renderer.rs` | All rendering state and rasterization logic |
| `rcp/rsp.rs` | RSP register interface, auto-complete, DMA |
| `bus/map.rs` | `process_rsp_task()` — reads OSTask, triggers display list processing |

---

## Display List Processing

Entry point: `Interconnect::process_rsp_task()` in `bus/map.rs`.

1. RSP task starts when game writes to SP_STATUS (clear halt + set task bit)
2. `process_rsp_task()` reads the OSTask header from DMEM[0xFC0] (last 64 bytes)
3. For graphics tasks (type 1), extracts the display list physical address
4. Calls `gbi::process_display_list()` which iterates GBI commands
5. Each 64-bit command word is decoded: high byte = opcode, remaining = parameters
6. Display lists can branch/call (`G_DL`) with a 10-deep stack

### Segment Address Resolution

N64 games use segment-relative addresses (bits 27-24 = segment ID). The
renderer maintains a 16-entry segment table set by `G_MOVEWORD`. All addresses
from display list commands pass through `resolve_segment()` before RDRAM access.

---

## Transformation Pipeline

```
Object Space → [Modelview] → World Space → [Projection] → Clip Space
            → Perspective Divide (1/w) → NDC → [Viewport] → Screen Space
```

- **Modelview stack:** Push/pop via G_MTX params. Supports nested transforms.
- **Projection:** Typically perspective (guPerspective) × view (guLookAt).
- **MVP = MV × P** (N64 uses row-vector convention: vertex × matrix).
- **Matrices stored in RDRAM** as 16.16 fixed-point (first 32 bytes = integer
  parts, next 32 bytes = fractional parts).
- **Viewport:** Scale + translate from G_MOVEMEM, converts NDC [-1,1] to pixel
  coordinates. Default: scale=(160,120), translate=(160,120) for 320×240.

### FR=0 FPU Mode (Critical)

N64 games use FR=0 FPU mode where double-precision values span even/odd
register pairs. The SDK's matrix functions (guLookAtF, guPerspectiveF) use
double-precision division for normalization accuracy. `read_f64(n)` must
combine `fpr[n]` (low 32 bits) with `fpr[n|1]` (high 32 bits).

---

## Vertex Processing

`G_VTX` loads vertices into a 32-entry buffer. Each vertex (16 bytes from
RDRAM) contains:

| Offset | Field | Format |
|--------|-------|--------|
| 0-1 | X | s16 |
| 2-3 | Y | s16 |
| 4-5 | Z | s16 |
| 6-7 | (padding/flag) | u16 |
| 8-9 | S (tex coord) | s16 (10.5 fixed) |
| 10-11 | T (tex coord) | s16 (10.5 fixed) |
| 12 | R or Normal.X | u8 / s8 |
| 13 | G or Normal.Y | u8 / s8 |
| 14 | B or Normal.Z | u8 / s8 |
| 15 | A | u8 |

When **lighting is enabled** (G_LIGHTING in geometry_mode), bytes 12-14 are
normals. The normal is transformed by the upper 3×3 of the modelview matrix,
then dotted with each light direction. Ambient + directional contributions
produce the final vertex color.

When **lighting is disabled**, bytes 12-14 are vertex colors (R, G, B).

---

## Triangle Rasterization

**Algorithm:** Edge-function (half-space) rasterizer.

1. Compute bounding box, clip to scissor rectangle
2. Compute signed triangle area (2D cross product of edges)
3. Face culling check (G_CULL_FRONT / G_CULL_BACK vs area sign)
4. For each pixel in bounding box:
   - Three edge function evaluations → barycentric weights (w0, w1, w2)
   - Skip if any weight < 0 (outside triangle)
   - Interpolate Z, colors, tex coords via barycentric weights
   - Z-buffer test (if enabled): 16-bit depth, compare + conditional update
   - Texture sampling (if enabled): point sampling from TMEM
   - Color combiner: (A-B)×C+D formula, 1-cycle or 2-cycle mode
   - Alpha compare: threshold test against blend_color alpha
   - Write pixel to RDRAM (RGBA5551 or RGBA8888)

---

## Texture System

### TMEM (Texture Memory)

4KB on-chip texture cache, loaded from RDRAM via:
- **G_LOADBLOCK:** Contiguous copy (for power-of-2 textures)
- **G_LOADTILE:** Rectangular region copy (respects stride)
- **G_LOADTLUT:** Palette (color index table) into upper TMEM (0x800-0xFFF)

### Tile Descriptors

8 tile descriptors (indices 0-7), each specifying:
- Format (RGBA, CI, IA, I) and size (4/8/16/32 bpp)
- TMEM offset and line stride
- Wrap/mirror/clamp mode per S/T axis
- Coordinate bounds (10.2 fixed-point)

### Supported Texture Formats

| Size | Formats |
|------|---------|
| 4-bit | I4 (grayscale), CI4 (indexed with TLUT) |
| 8-bit | I8, IA8 (4-bit intensity + 4-bit alpha), CI8 (indexed) |
| 16-bit | RGBA5551 (5-5-5-1), IA16 (8+8) |
| 32-bit | RGBA8888 (8-8-8-8) |

---

## Color Combiner

The RDP color combiner computes per-pixel color using:

```
output = (A - B) × C + D
```

Applied separately to RGB and Alpha channels. In 2-cycle mode, the formula
runs twice: cycle 0 output feeds cycle 1 as COMBINED.

### Input Sources

| Selector | RGB Options | Alpha Options |
|----------|-------------|---------------|
| A (4-bit) | COMBINED, TEXEL0, PRIM, SHADE, ENV, 1.0, 0 | COMBINED, TEXEL0, PRIM, SHADE, ENV, 1.0, 0 |
| B (4-bit) | same as A | same as A |
| C (5-bit) | same + SHADE_ALPHA, ENV_ALPHA, PRIM_LOD_FRAC, etc. | TEXEL0, PRIM, SHADE, ENV, 0 |
| D (3-bit) | COMBINED, TEXEL0, PRIM, SHADE, ENV, 0 | same |

---

## Implemented GBI Commands

### Fully Implemented
- Display list: G_DL, G_ENDDL, G_NOOP
- Drawing: G_VTX, G_TRI1, G_TRI2, G_QUAD, G_FILLRECT, G_TEXRECT, G_TEXRECTFLIP
- Matrices: G_MTX, G_POPMTX
- State: G_GEOMETRYMODE, G_TEXTURE, G_SETSCISSOR
- RDP: G_SETCIMG, G_SETZIMG, G_SETTIMG, G_SETFILLCOLOR, G_SETENVCOLOR,
  G_SETPRIMCOLOR, G_SETBLENDCOLOR, G_SETFOGCOLOR, G_SETCOMBINE,
  G_SETOTHERMODE_H, G_SETOTHERMODE_L
- Tiles: G_SETTILE, G_SETTILESIZE, G_LOADBLOCK, G_LOADTILE, G_LOADTLUT
- Memory: G_MOVEMEM (viewport, lights), G_MOVEWORD (segments, numlight)
- Sync: G_RDPFULLSYNC, G_RDPTILESYNC, G_RDPPIPESYNC, G_RDPLOADSYNC

### Stubbed / No-op
- G_SETCONVERT (YUV coefficients stored, not applied)
- G_SETKEYR, G_SETKEYGB (chroma key — no-op)
- G_MODIFYVTX (modify single vertex — no-op)
- G_SPECIAL_1 (conditional branch — no-op)

---

## Current Limitations

1. **No blending** — Alpha threshold only, no Porter-Duff source/dest blending
2. **Point sampling only** — No bilinear/trilinear texture filtering
3. **No perspective-correct texturing** — Linear barycentric interpolation
4. **No fog** — Fog color stored but never applied
5. **No mipmapping** — Single texture level only
6. **TEXEL0 only** — No dual-texture (TEXEL1 aliases TEXEL0)
7. **No dithering** — No alpha or color dither patterns
8. **No clipping** — Triangles clipped only to scissor bounding box, no near/far plane clip
