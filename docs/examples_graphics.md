# Graphics Pipeline Example (fenced)

A minimal host-side sequence showing descriptor alignment, fences, and commands for a fixed-function draw.

## Descriptor Setup (VRAM)
- `GSTATE` (128B, align 64B) at `gstate_ptr`: VBO/IBO pointers/strides, MVP/normal, light0, cull/clip, viewport/scissor.
- `GPARAM` (64B, align 32B) at `gparam_ptr`: model or model*view, material color, gloss/spec.
- `RSTATE` (64B, align 32B) at `rstate_ptr`: fb/depth bases/strides/formats, viewport/scissor, blend/depth func, sampler index.
- `GDRAW`  (24B, align 16B) at `gdraw_ptr`: {vtx_base, idx_base, count, topology, state_id, param_id, flags}.
- `RDRAW`  (16B, align 16B) at `rdraw_ptr`: {state_id, vtx_ptr (optional), flags}.
- Vertex data and index buffer already written to VRAM; issue `MEMBAR` before commands if produced by GPU.

## Command FIFO Sequence (with fences)
| Step | Opcode | Args | Flags | Notes |
| --- | --- | --- | --- | --- |
| 1 | `0x40 LOAD_GSTATE` | arg0=stateId0, arg1=gstate_ptr, arg2=0 | emit | Load geometry state, emit fence0 |
| 2 | `0x41 LOAD_GPARAM` | arg0=paramId0, arg1=gparam_ptr, arg2=0 | emit | Param block, emits fence0 (same rdst) |
| 3 | `0x43 LOAD_RSTATE` | arg0=rstateId0, arg1=rstate_ptr, arg2=0 | emit | Render state, emits fence0 |
| 4 | `0x42 GDRAW` | arg0=gdraw_ptr, arg1=0, arg2=flags(topology) | wait+emit | wait fence0, emit fence1 on completion |
| 5 | `0x44 RDRAW` | arg0=rdraw_ptr, arg1=0, arg2=flags(depth/blend) | wait | wait fence1 to ensure geometry ready; rasterizes |

- Flags column: `emit` sets header.flags bit1; `wait` sets bit2; IRQ optional via bit0.
- If using CU-assist vertex shader, insert `DISPATCH_1D` before step 4 with emit fence1, then make GDRAW wait on fence1 and emit fence2; RDRAW waits fence2.

## Two-Pass Fragment Shading Example
1) `LOAD_*` states as above with emit fence0.
2) `GDRAW` wait+emit fence0: write varyings/depth into G-buffer tile (disable blend).
3) `DISPATCH_2D` wait fence0, emit fence1: fragment compute reads G-buffer, shades, blends to FB via VATOM/ROP or writes to color buffer.
4) Present/swap after `MEMBAR` if host reads back.

## Notes
- Keep descriptor alignments: GSTATE 64B, GPARAM 32B, RSTATE 32B, GDRAW 16B, RDRAW 16B.
- Use `MEMBAR` on the producing side (GPU or host) before queueing commands that consume those buffers.
- For indirect draws, set GDRAW `arg1` to the count pointer (align 4); firmware reads count and issues that many GDRAWs.
