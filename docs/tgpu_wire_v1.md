# TinyGPU Wire Protocol v1 (API → Packets)

This document makes the “mini‑Vulkan” host API concrete by defining a **v1 on‑wire mapping**:
- host API calls → command stream entries
- opcode/args/payload rules
- minimal MMIO needed for bring‑up

It is intended to complement (not replace):
- `docs/command_protocol.md` (packet header and current opcode list)
- `docs/system_contracts.md` (queue/doorbell/fence contracts and a minimal MMIO map)
- `docs/memory_map.md` (addressing conventions)

---

## 1. Two submission models (pick one for v1)

This repo currently contains **two** compatible-but-different submission stories:

### Model A — CMD FIFO window (simplest bring‑up)
Defined in `docs/command_protocol.md`.
- Host sends commands via a write-only “CMD” path (`submit_cmd_bytes()`)
- Bulk data is transferred via `WRITE_MEM`/`READ_MEM` commands

This model is easiest for QSPI/OSPI/Hex‑PIO because it needs almost no MMIO beyond “device ID / status”.

### Model B — VRAM ring + doorbell (scales better)
Defined in `docs/system_contracts.md`.
- Host writes commands into a VRAM ring
- Host updates producer pointer + rings a doorbell via MMIO

Roadmap alignment:
- This VRAM ring is also the natural substrate for an on-device **Command Streamer** (DMA) that feeds the graphics/compute engines.
- Long-term, the scalar ISA graphics opcodes (`RSTATE/RSETUP/GDRAW/...`) can be implemented as *enqueue-only* writes into the same ring, preserving binary compatibility while removing CPU-side stalls.

This is better long-term (batched DMA writes, fewer small transactions), and matches the “mini‑Vulkan queue submit” model naturally.

**Recommendation for v1**
- Start with Model A to get real systems working.
- Keep Model B as “v1.1+” once the firmware/RTL ring and MMIO are stable.

---

## 2. Packet format (Model A)

A command is:
- **16‑byte header** (little-endian) as described in `docs/command_protocol.md`
- optional payload (present if `flags.bit3` is set)

Header fields:
- `opcode [7:0]`
- `flags  [7:0]`
- `rdst   [7:0]`
- `len    [7:0]` (payload words; bytes = `len * 4`)
- `arg0 [31:0]`, `arg1 [31:0]`, `arg2 [31:0]`

### 2.1 Alignment
- Header is 16B.
- Payload must be 4B-aligned.

### 2.2 Completion / fences (v1 rule)
`docs/command_protocol.md` defines `rdst` as a completion slot/token.
To keep v1 implementable while we converge on a full “completion slots” design:

- **v1 supports a single fence timeline**: `rdst == 0`.
- `flags.bit1 (EMIT)` updates the global fence value.
- `flags.bit2 (WAIT)` waits on the global fence value.

On the MMIO side, the fence is exposed as `FENCE_VALUE` (see `docs/system_contracts.md`).

---

## 3. Opcode table (v1 semantics)

This section is a normative “what the device must do” view. The opcode numbers follow `docs/command_protocol.md`.

### 3.1 Data movement
- `0x01 WRITE_MEM`
  - args: `arg0 = dst_vram_addr`
  - payload: `len*4` bytes copied to VRAM
  - must validate address/length against a safety window

- `0x02 READ_MEM`
  - args: `arg0 = src_vram_addr`
  - payload: none
  - completion: device returns `len*4` bytes via the transport’s readback mechanism
  - v1 note: if your transport cannot return variable-length data as a “completion pipe”, implement `READ_MEM` as a two-step: issue command, then host performs `mem_read()` directly.

### 3.2 2D helpers (optional fast path)
- `0x10 FILL_RECT`
  - `arg0 = dst_addr`
  - `arg1 = (h<<16) | w`
  - `arg2 = color` (format depends on current FB/target)

- `0x11 BLIT`
  - `arg0 = dst_addr`, `arg1 = src_addr`
  - `arg2 = (h<<16) | w`

- `0x23 COPY_BUFFER`
  - `arg0 = dst_addr`, `arg1 = src_addr`
  - length uses `len` (words)

### 3.3 Compute dispatch
- `0x20 DISPATCH_1D`
  - `arg0 = grid`, `arg1 = block`, `arg2 = kernel_id`
  - kernel args are supplied by a descriptor in VRAM (see `docs/command_protocol.md`) or by a future payload format

- `0x21 DISPATCH_2D`
  - `arg0 = grid_xy (y<<16 | x)`
  - `arg1 = block_xy (y<<16 | x)`
  - `arg2 = kernel_id`

### 3.4 Ordering
- `0x22 BARRIER`
  - no args
  - must provide the same “drain + order” guarantees as `MEMBAR` described in `docs/system_contracts.md`

### 3.5 Present / scanout control
- `0x30 SET_FB`
  - `arg0 = fb_base`, `arg1 = stride`, `arg2 = format`

- `0x31 SWAP_FB`
  - `arg0 = front_base`, `arg1 = back_base`
  - v1: swap must be ordered after prior rendering (host should use `BARRIER` / fence wait)

### 3.6 Graphics state and draws
These opcodes are the bridge between a Vulkan-like API and the current “descriptor-in-VRAM” approach.

Implementation note (planned):
- `LOAD_*` and `GDRAW/RDRAW/RRECT` can be treated as entries in the same internal GPU command stream as ISA `RSTATE/GSTATE/GPARAM/GDRAW/...`.
- A command streamer can prefetch descriptors (RSTATE/GSTATE/GPARAM) into shadow state to hide latency before draw execution.

- `0x40 LOAD_GSTATE`
  - `arg0 = dst_state_id`
  - `arg1 = vram_ptr_to_128B`
  - `arg2 = flags`

- `0x41 LOAD_GPARAM`
  - `arg0 = dst_param_id`
  - `arg1 = vram_ptr_to_64B`

- `0x43 LOAD_RSTATE`
  - `arg0 = dst_state_id`
  - `arg1 = vram_ptr_to_64B`

- `0x42 GDRAW`
  - `arg0 = vram_ptr_to_24B_gdraw_desc`
  - `arg1 = optional_indirect_count_ptr (0 if none)`
  - `arg2 = flags`

- `0x44 RDRAW`
  - `arg0 = vram_ptr_to_16B_rdraw_desc`
  - `arg1 = vram_ptr_to_vertex_block (or 0)`
  - `arg2 = flags`

- `0x45 RRECT`
  - `arg0 = vram_ptr_to_16B_rrect_desc`
  - `arg1 = state_id`
  - `arg2 = flags`

---

## 4. Mapping: “mini‑Vulkan” API → v1 packets

This is the intended lowering (host library side). The library may batch many packets into one `submit_cmd_bytes()` call.

### 4.1 Resource upload
- `tgpu_upload_buffer()` / `tgpu_upload_texture()` → `WRITE_MEM`

### 4.2 Barriers
- `tgpu_cmd_barrier()` → `BARRIER` (and optionally additional cache-control opcodes later)

### 4.3 Compute
- `tgpu_cmd_bind_compute_pipeline()` → (library selects `kernel_id`)
- `tgpu_cmd_dispatch(x,y,z)` → `DISPATCH_1D/2D`

### 4.4 Graphics
- `begin_render_pass()` → `SET_FB` (+ optional clear as `FILL_RECT` or `RRECT`)
- `bind_*` → `LOAD_GSTATE/LOAD_RSTATE` (or future “bind by handle” commands)
- `draw*()` → `GDRAW` then `RDRAW` (or a single combined draw opcode later)
- `present()` → `SWAP_FB` (plus fence wait/barrier as required)

---

## 5. Minimal MMIO map (what the host library needs)

For Model A (CMD FIFO), the host *can* operate with just ID/status plus an optional fence register.
For Model B (VRAM ring), the required MMIO set is already defined in `docs/system_contracts.md` section 9.

**Minimum recommended set (both models)**
- `ID_VERSION`, `ID_FEATURES`
- `CTRL` (reset/errors)
- `FENCE_VALUE` (monotonic timeline)
- `LAST_ERROR` / `LAST_STATUS`

**Ring submission (Model B) also needs**
- `CMDQ_BASE`, `CMDQ_SIZE_BYTES`, `CMDQ_PROD_BYTES`, `CMDQ_CONS_BYTES`, `CMDQ_DOORBELL`

---

## 6. Versioning / negotiation

- Host must read `ID_VERSION`/`ID_FEATURES` and refuse unknown major versions.
- v1 wire is the opcode set above plus the 16B header.
- Any extension must either:
  - introduce new opcodes, or
  - use payloads with explicit per-opcode sub-versions.
