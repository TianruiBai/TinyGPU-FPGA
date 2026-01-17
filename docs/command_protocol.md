# Command Protocol

- Two host-visible windows: **CMD** (write-only FIFO) and **VRAM** (read/write aperture).
- Commands: 16-byte header + optional payload; little endian.
- Host only sees a bounded VRAM sub-window; firmware validates addresses/lengths before dispatch.

## Header Fields
- `opcode   [7:0]`
- `flags    [7:0]` (bit0: IRQ on done, bit1: fence emit, bit2: fence wait, bit3: payload follows)
- `rdst     [7:0]` (completion slot/token)
- `len      [7:0]` (payload bytes / 4 = words)
- `arg0     [31:0]`
- `arg1     [31:0]`
- `arg2     [31:0]`

### Fence Semantics (flags bits1/2)
- **Emit (bit1):** command writes a monotonically increasing fence value (FW-managed) to the completion slot `rdst`. Used to signal “work up to here is done.”
- **Wait (bit2):** command does not start until the fence value in completion slot `rdst` meets/exceeds the current counter. Host sets `rdst` to the fence ID to wait on. FW enforces ordering before queuing to GPU.
- Emit and wait can co-exist (e.g., wait on prior fence, then emit a new one). If both bits clear, command is fire-and-forget.
- IRQ (bit0) still applies to completion. Payload bit (bit3) unchanged.

## Minimal Opcode Set (extensible)
- `0x01 WRITE_MEM`   : write payload to VRAM at `arg0`, length = len words
- `0x02 READ_MEM`    : read len words from VRAM `arg0` -> completion pipe
- `0x10 FILL_RECT`   : args: dst_addr, `arg1`=w/h packed (16b each), `arg2`=color
- `0x11 BLIT`        : args: dst_addr, src_addr, `arg2`=w/h packed
- `0x20 DISPATCH_1D` : args: grid, block, kernel_id (firmware maps kernel_id to CU program)
- `0x30 SET_FB`      : args: fb_base, stride, format
- `0x31 SWAP_FB`     : args: front_base, back_base

## Extended Graphics/GPGPU Opcodes
- `0x12 SPRITE_DRAW` : args: dst_addr (layer base), src_addr (sprite base), `arg2`=w/h packed; payload optional for per-draw params
- `0x13 LAYER_BLEND` : args: dst_layer_base, src_layer_base, `arg2`=alpha (8:8 fixed)
- `0x14 CLEAR_LAYER` : args: layer_base, `arg1`=w/h packed, `arg2`=color
- `0x21 DISPATCH_2D` : args: grid_xy (16b each), block_xy (16b each), kernel_id; optional shared mem in desc.reserved
- `0x22 BARRIER`     : no args; orders previously issued compute/graphics before following commands
- `0x23 COPY_BUFFER` : args: dst_addr, src_addr, len (words in `len`)
- `0x40 LOAD_GSTATE` : args: `arg0`=dst state ID, `arg1`=VRAM ptr to 128B GSTATE (align 64B), `arg2`=flags (bit0: cache/preload)
- `0x41 LOAD_GPARAM` : args: `arg0`=dst param ID, `arg1`=VRAM ptr to 64B GPARAM (align 32B)
- `0x42 GDRAW`       : args: `arg0`=VRAM ptr to 24B GDRAW descriptor (align 16B), `arg1`=optional indirect count ptr (0 if none), `arg2`=flags (bit0 CU-assist, bit1 topology strip)
- `0x43 LOAD_RSTATE` : args: `arg0`=dst state ID, `arg1`=VRAM ptr to 64B RSTATE (align 32B)
- `0x44 RDRAW`       : args: `arg0`=VRAM ptr to 16B RDRAW desc (align 16B), `arg1`=vertex block ptr (RSETUP) or 0 to reuse, `arg2`=flags (depth/blend/tex)
- `0x45 RRECT`       : args: `arg0`=VRAM ptr to 16B RRECT desc (align 16B), `arg1`=state ID, `arg2`=flags

## Completion
- Optional mailbox writeback keyed by `rdst`.
- Optional IRQ when flags bit0 is set.
- Fence emit/wait via flags bits1/2 to order work and host waits.

## Kernel Descriptor (in VRAM)
Used by `DISPATCH_1D` when `DISPATCH_KICK` points to this layout.

```
struct tgpu_kernel_desc {
	uint32_t kernel_id; // index into firmware-managed kernel table
	uint32_t grid;      // total threads or groups (1D)
	uint32_t block;     // threads per block/workgroup (1D)
	uint32_t arg_ptr;   // VRAM address of argument blob
	uint32_t reserved;  // future (e.g., shared mem size)
};

For `DISPATCH_2D`, reinterpret `grid` and `block` as `{y[31:16], x[15:0]}`.

## Graphics Descriptor Layouts (host-visible)
- **RSTATE (64B, align 32B):** fb_base, fb_stride, fb_format; depth_base/stride/format; viewport/scissor; blend/depth func; sampler indices.
- **RSETUP vertex block (96B, align 32B):** 3x {x,y,z,w,u,v,color} fp32/ARGB8888.
- **RDRAW/RRECT (16B, align 16B):** state_id, vtx_ptr or rect, flags.
- **GSTATE (128B, align 64B):** vbo_base/stride, ibo_base/type, mvp/normal matrices, light0 (dir/color/ambient), cull/clip, viewport/scissor.
- **GPARAM (64B, align 32B):** model or model*view, material color, gloss/spec.
- **GDRAW (24B, align 16B):** vtx_base, idx_base, count, topology, state_id, param_id, flags.

## Indirect Dispatch/Draw
- **DISPATCH_1D/2D:** when `len` > 0 and payload flag set, payload may contain an indirect kernel descriptor; firmware copies/validates before launch. If `arg0` points to a kernel descriptor in VRAM and `len`==0, FW can fetch indirectly (guarded by flags bit2 wait if needed).
- **GDRAW indirect:** `arg1` points to a `uint32_t count` (align 4). FW reads count, then issues GDRAW that many times starting at `arg0` descriptor (descriptor stride fixed at 24B). If count=0, no-op.
- **RDRAW indirect:** not supported; use GDRAW to feed raster path. RRECT uses direct descriptors only.

## Job / Tile Barrier Handling (CP firmware)
- The RISC-V CP sequences commands and fences; it must enforce ordering between raster passes (GDRAW/RDRAW writing G-buffer) and compute passes (DISPATCH shading) by placing wait/emit fences and issuing MEMBAR before host-visible readback.
- Implement a watchdog/abort path so a hung kernel (bad loop) can be preempted; do not rely solely on WFI.

## Example Command Sequences
### Basic draw (fixed-function path)
1. `LOAD_GSTATE` (opcode 0x40) flags:emit, rdst=fence0.
2. `LOAD_GPARAM` (0x41) flags:emit, rdst=fence0.
3. `LOAD_RSTATE` (0x43) flags:emit, rdst=fence0.
4. `GDRAW` (0x42) flags:wait+emit, rdst=fence0 (wait on prior loads), arg0=GDRAW desc, arg1=0.
5. `RDRAW` (0x44) flags:wait, rdst=fence0 (consumes the geometry output/raster token if needed) or leave unset if GDRAW hands directly to raster internally.

### Compute-assisted vertex then draw
1. `DISPATCH_1D` microkernel (vertex transform) flags:emit, rdst=fence1.
2. `GDRAW` with CU-assist flag set, flags:wait+emit, rdst=fence1 (ensures transformed VBO ready).
3. `RDRAW` flags:wait, rdst=fence1 to present.

### Two-pass fragment shading (compute-based)
1. `LOAD_*` states as above (GSTATE/GPARAM/RSTATE) with emit fence0.
2. `GDRAW` (0x42) wait+emit, rdst=fence0, renders positions/varyings/depth into a G-buffer tile (FB render target) with blend disabled.
3. `DISPATCH_2D` (0x21) wait fence0, emit fence1: compute kernel shades the tile from the G-buffer, samples textures, and blends to FB via VATOM/ROP (or writes color then BLITs).
4. Optional: `BARRIER`/`MEMBAR` then present/swap.
```
