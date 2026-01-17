# System Contracts (Host OSPI + VRAM OSPI + Display)

This document defines the **system-level contracts** that must be stable for RTL + firmware to interoperate.
It complements:
- `docs/isa.md`
- `docs/isa_instructions.md`
- `docs/microarchitecture.md`

Scope: **host/command processor interface**, **VRAM interface**, **display interface**, **memory ordering**, **fences/doorbells**, and **descriptor locality**.

---

## 1. Roles & Buses

### 1.1 Actors
- **Host MCU (ESP32S3)**: submits work, uploads descriptors, reads back status.
- **Command Processor (CP)**: firmware-controlled command parser/dispatcher (may run on MCU or an embedded soft-core).
- **Compute Unit(s) (CU)**: SIMD cores executing kernels and issuing graphics macro-ops.
- **Display Engine**: reads framebuffer and drives HUB75/HDMI-TMDS.

### 1.2 Physical buses (assumed)
- **OSPI-H (Host)**: MCU ↔ FPGA register/doorbell/status path. Not used for bulk VRAM traffic.
- **OSPI-V (VRAM)**: FPGA ↔ external PSRAM/VRAM.
- **Display link**: HUB75 or HDMI TMDS from FPGA.

If OSPI-H and OSPI-V share pins in a given build, this document must still be satisfied logically (time-multiplexed arbitration is allowed but must preserve ordering rules below).

---

## 2. Address Map (Normative)

All addresses are 32-bit. Exact sizes are implementation-defined; regions and behavior are normative.

### 2.1 Regions
- **MMIO (FPGA registers over OSPI-H)**: `0x0000_0000 .. 0x0000_FFFF`
  - Doorbells, queue heads/tails, fence registers, debug counters.
- **Shared/Scratch BRAM (tile-local)**: implementation-defined base (recommended `0x1000_0000 .. 0x1000_FFFF`)
  - Not coherent with host.
  - Coherent between the 4 cores only if you explicitly implement it; default is “per-core scratch”.
- **VRAM (external PSRAM over OSPI-V)**: implementation-defined base (recommended `0x8000_0000 ..`)
  - Accessible by LSU, TEX, raster/ROP, and display engine.

### 2.2 Alignment / access granularity
- Scalar loads/stores: 8/16/32-bit.
- Vector loads/stores: 128-bit architectural, implemented as bursts.
- The VRAM fabric may internally enforce a minimum burst (e.g., 32B/64B); software must assume **small scattered writes are expensive**.

---

## 3. Command Submission & Completion

### 3.1 Command queue model
- CP owns one or more **ring buffers** in VRAM for commands.
- Host advances a **producer index** and rings a **doorbell** over OSPI-H.
- CP advances a **consumer index** and emits **fence completion** records.

Minimum required MMIO registers (names illustrative):
- `CMDQ_BASE`, `CMDQ_SIZE`, `CMDQ_PROD`, `CMDQ_CONS`
- `DOORBELL` (host writes 1)
- `FENCE_VALUE` (monotonic counter)
- `FENCE_WAIT` (host writes target fence; CP blocks submission until reached)

### 3.2 Fence semantics
- CP maintains a **monotonic fence counter** `F`.
- Commands may include flags:
  - `WAIT_FENCE >= X`: do not start this command until `F >= X`.
  - `EMIT_FENCE`: upon command completion, increment `F` (or set to specified value).

Fences are **system-visible** (host-readable over OSPI-H). Fence ID width must be documented (recommend 32-bit wrap-safe compare).

### 3.3 Abort / watchdog
- There must be a host-controlled **ABORT** path that guarantees forward recovery:
  - `ABORT_REQUEST`: host sets.
  - CP stops launching new work and requests CUs/G-pipe to quiesce.
  - If quiesce times out, CP resets tile/CU state and advances fence with an error status.

The CU/graphics “preemption” described in `docs/microarchitecture.md` is considered satisfied if:
- outstanding macro-ops can be prevented from accepting new work, and
- queues can be flushed and the machine returns to a known idle state.

---

## 4. Memory Ordering & Coherency

This section defines the *only* supported coherency model unless explicitly extended.

### 4.1 Global rule
- **Host is non-coherent during kernel execution**: there is no snooping on OSPI.
- Memory visibility is established only by **explicit handoff points**.

### 4.2 `MEMBAR` (architectural)
`MEMBAR` must provide, at minimum:
1. **Ordering**: all CU writes issued before `MEMBAR` become visible to VRAM clients that observe after the barrier.
2. **Drain**: write-merge/streaming buffers are drained for affected address spaces.

Implementation note (normative behavior, not micro-architecture): `MEMBAR` completes only when:
- all prior LSU stores are committed to the VRAM fabric, and
- any CU-side write combining affecting VRAM is drained.

For full graphics support, `MEMBAR` must also account for fixed-function clients:
- all prior **ROP/raster stores** are committed to the VRAM fabric, and
- any ROP-side write combining affecting VRAM is drained.

Whether `MEMBAR` also drains TEX descriptor caches / data caches must be stated by implementation:
- **Baseline recommendation**: `MEMBAR` also invalidates/flushes TEX descriptor cache (if any) to prevent stale descriptors.
- If a read-only TEX data cache exists, invalidation on `MEMBAR` is optional; if not invalidated, firmware must not rely on `MEMBAR` to observe host writes via TEX without an explicit cache control.

### 4.3 Atomics vs non-atomics
From `docs/isa_instructions.md`:
- Atomics are globally coherent for participating agents.
- Regular accesses are weakly ordered; use `MEMBAR` to observe atomic updates with non-atomic loads.

Contract:
- Atomic RMW operations must be **serialized at the VRAM arbiter** for the addressed word.
- If display engine reads the same region, you must define whether display can observe partial results mid-frame; baseline assumes framebuffer swap is fenced.

### 4.4 Framebuffer visibility
To safely present a frame:
- CU/ROP writes → `MEMBAR` → host/CP flips display base pointer (or updates scanout state) → optional additional `MEMBAR` if scanout shares caches.

---

## 5. VRAM Arbitration & QoS (Critical)

The VRAM fabric must arbitrate among at least:
- Display engine reads
- CU scalar LSU
- CU vector LSU
- TEX unit
- Raster/ROP (if separate)

### 5.1 QoS rule (normative)
- **Display engine must not underflow** in steady state.
- Therefore: display reads have highest priority or reserved bandwidth.

### 5.2 Forward progress rule (normative)
- Non-display clients must not starve indefinitely.
- Arbitration must include fairness (e.g., weighted round robin with display weight).

### 5.3 Transaction sizing
- Write-merge buffer behavior from `docs/microarchitecture.md` must be reflected in the VRAM port: sequential writes should be merged into larger bursts.
- Streaming store path (for clears/blits) must bypass cache pollution.

---

## 6. Descriptors & Locality

### 6.1 Sampler descriptors
Sampler descriptor is 32 bytes as in `docs/isa_instructions.md`:
- `0x00` base, `0x04` stride, `0x08` width, `0x0C` height,
- `0x10` format, `0x14` wrap, `0x18` filter, `0x1C` misc.

### 6.2 Graphics state descriptors
From `docs/isa_instructions.md`:
- RSTATE: 64B
- RSETUP: 96B
- GDRAW: 24B

Contract (normative): firmware and RTL must agree on the concrete layout in `docs/isa_instructions.md` section 9.1.

Contract:
- Descriptors may live in VRAM, but **high-frequency descriptors (RSTATE/GSTATE)** should be staged in on-chip SRAM when possible.
- If descriptors are fetched from VRAM, the G-pipe must tolerate 100+ cycle latency and the CP must avoid thrashing state.

### 6.3 Descriptor cacheability
If any descriptor caching exists, you must define:
- which descriptor types are cacheable,
- what invalidates them (recommended: explicit CP command, plus `MEMBAR` for safety),
- whether host writes to VRAM require a fence before GPU reads.

---

## 7. Display Engine Contract

### 7.1 Scanout
- Display engine reads a framebuffer region in VRAM.
- Scanout parameters (base, stride, format, dimensions) are programmed via MMIO.

### 7.2 Double buffering
Baseline contract:
- Display reads from **front buffer** while CU renders to **back buffer**.
- Swap occurs only after: render complete → `MEMBAR` → update scanout base (MMIO) at vblank or safe boundary.

### 7.3 Formats
At minimum, the display engine must specify supported framebuffer formats (e.g., RGB565, ARGB8888). Any conversion stages must be stated (gamma/sRGB).

---

## 8. Required “Next Specs” Checklist

Before scaling RTL features, lock these down:
1. Exact MMIO register set for command queue + doorbells + fences + abort.
2. VRAM address map and any reserved regions (framebuffers, command buffers, descriptor SRAM windows).
3. Exact `MEMBAR` meaning (drains which buffers/caches) and what constitutes “visible to display”.
4. VRAM arbiter policy (display QoS + fairness) and minimum burst sizes.
5. Swap protocol and whether display can ever read from a buffer being written.

---

## 9. MMIO Register Map (Normative, Minimal)

All MMIO registers are **32-bit**, little-endian, word-addressable.
The MMIO base is `0x0000_0000` (over OSPI-H).

Reset behavior:
- Unless stated otherwise, registers reset to 0.
- Fields marked **RO** are read-only.

### 9.1 Identification / status

| Offset | Name | R/W | Description |
| :--- | :--- | :--- | :--- |
| `0x0000` | `ID_VERSION` | RO | `[31:24]=major`, `[23:16]=minor`, `[15:0]=build` |
| `0x0004` | `ID_FEATURES` | RO | Feature bits (implementation-defined; must be documented) |
| `0x0008` | `INT_STATUS` | RO | Interrupt status (write-1-to-clear via `INT_CLEAR`) |
| `0x000C` | `INT_MASK` | R/W | Interrupt mask enable bits |
| `0x0010` | `INT_CLEAR` | WO | Write-1-to-clear bits in `INT_STATUS` |
| `0x0014` | `LAST_ERROR` | RO | Sticky error code (0 = none). Cleared by writing `CTRL_RESET_ERRORS=1` |

Recommended `INT_STATUS` bits (minimum if interrupts are implemented):
- bit0: `CMDQ_NONEMPTY`
- bit1: `FENCE_REACHED`
- bit2: `ABORT_DONE`
- bit3: `DISPLAY_VBLANK` (optional)

### 9.2 Command queue control

The command queue is a ring of fixed-size entries in **VRAM**.

| Offset | Name | R/W | Description |
| :--- | :--- | :--- | :--- |
| `0x0100` | `CMDQ_BASE` | R/W | 32-bit VRAM address of ring base. Must be 32-byte aligned |
| `0x0104` | `CMDQ_SIZE_BYTES` | R/W | Ring size in bytes. Must be a power of two and a multiple of 32 |
| `0x0108` | `CMDQ_PROD_BYTES` | R/W | Producer pointer in bytes (host writes). Must be modulo `CMDQ_SIZE_BYTES` |
| `0x010C` | `CMDQ_CONS_BYTES` | RO | Consumer pointer in bytes (CP updates). Modulo `CMDQ_SIZE_BYTES` |
| `0x0110` | `CMDQ_DOORBELL` | WO | Write `1` to notify CP that new entries are available |
| `0x0114` | `CMDQ_STATUS` | RO | bit0 `EN`, bit1 `BUSY`, bit2 `FAULT`, bit3 `ABORTING` |
| `0x0118` | `CTRL` | R/W | bit0 `CMDQ_ENABLE`, bit1 `RESET_CP`, bit2 `RESET_CU`, bit3 `RESET_ERRORS` |

Rules:
- Host must program `CMDQ_BASE`, `CMDQ_SIZE_BYTES`, set `CMDQ_PROD_BYTES=0`, then set `CTRL.CMDQ_ENABLE=1`.
- Host updates `CMDQ_PROD_BYTES` only after writing command entries into VRAM.
- CP must never advance `CMDQ_CONS_BYTES` past `CMDQ_PROD_BYTES` (modulo ring arithmetic).

### 9.3 Fence / completion

| Offset | Name | R/W | Description |
| :--- | :--- | :--- | :--- |
| `0x0120` | `FENCE_VALUE` | RO | Current completed fence value `F` (monotonic) |
| `0x0124` | `FENCE_TARGET` | R/W | Optional: target fence to generate `FENCE_REACHED` interrupt when `F >= target` |
| `0x0128` | `LAST_STATUS` | RO | Status of last completed command (0 = OK; nonzero = error) |

Fence comparison must be wrap-safe for 32-bit values (typical signed-delta compare).

### 9.4 Abort / watchdog

| Offset | Name | R/W | Description |
| :--- | :--- | :--- | :--- |
| `0x0130` | `ABORT_REQUEST` | WO | Write `1` to request abort/quiesce |
| `0x0134` | `ABORT_STATUS` | RO | bit0 `IN_PROGRESS`, bit1 `DONE`, bit2 `FORCED_RESET` |
| `0x0138` | `ABORT_TIMEOUT_CYCLES` | R/W | Timeout before forced reset (0 = implementation default) |

---

## 10. Command Ring Format (Normative)

### 10.1 General
- Ring lives in **VRAM** at `CMDQ_BASE`.
- Entry size is **32 bytes** (8 words). All fields little-endian.
- Pointers `CMDQ_PROD_BYTES` / `CMDQ_CONS_BYTES` count bytes from `CMDQ_BASE` and wrap modulo `CMDQ_SIZE_BYTES`.

### 10.2 Entry layout (32 bytes)

| Word | Bits | Name | Description |
| :--- | :--- | :--- | :--- |
| 0 | `[15:0]` | `OPCODE` | Command opcode |
| 0 | `[31:16]` | `FLAGS` | Per-command flags (see below) |
| 1 | `[31:0]` | `WAIT_FENCE` | Do not start until `FENCE_VALUE` satisfies wait (wrap-safe compare). 0 disables |
| 2 | `[31:0]` | `EMIT_FENCE` | If nonzero, set/advance completed fence to this value on success |
| 3 | `[31:0]` | `ARG0` | Opcode-specific |
| 4 | `[31:0]` | `ARG1` | Opcode-specific |
| 5 | `[31:0]` | `ARG2` | Opcode-specific |
| 6 | `[31:0]` | `ARG3` | Opcode-specific |
| 7 | `[31:0]` | `RESERVED` | Must be 0 for now |

### 10.3 `FLAGS`
Recommended initial flags (others reserved, must be 0):
- bit0 `INT_ON_COMPLETE`: raise `FENCE_REACHED` interrupt after completion (if interrupts enabled)
- bit1 `ALLOW_ABORT`: if 1, command is abortable; if 0, CP may choose to complete or force-reset

### 10.4 Minimal opcode set

| `OPCODE` | Name | Args | Effect |
| :---: | :--- | :--- | :--- |
| `0x0000` | `NOP` | none | No effect |
| `0x0001` | `MMIO_WRITE` | `ARG0=offset`, `ARG1=value` | Write 32-bit `value` to MMIO at `offset` |
| `0x0002` | `MEMBAR_SYS` | none | Execute a system memory barrier equivalent to architectural `MEMBAR` |
| `0x0010` | `DISPATCH` | `ARG0=kernel_pc`, `ARG1=arg_base`, `ARG2=grid_xy`, `ARG3=flags` | Launch a kernel on the CU grid |
| `0x0011` | `BARRIER_TILE` | none | Optional: tile barrier across 4 cores (if implemented) |
| `0x0020` | `DISPLAY_SET` | `ARG0=fb_base`, `ARG1=stride`, `ARG2=wh`, `ARG3=format` | Program scanout registers (no implicit swap timing) |
| `0x0021` | `DISPLAY_SWAP` | `ARG0=fb_base` | Swap scanout base at a safe boundary (vblank if supported) |

Encoding notes:
- `kernel_pc` is an implementation-defined address/offset into instruction memory.
- `arg_base` is a pointer to the kernel argument block (VRAM address or a mapped CSR window; must be documented by the implementation).
- `grid_xy`: `ARG2[15:0]=grid_x`, `ARG2[31:16]=grid_y`.

Completion semantics:
- On successful completion, CP updates `LAST_STATUS=0` and if `EMIT_FENCE!=0`, updates `FENCE_VALUE` to `EMIT_FENCE`.
- On failure, CP updates `LAST_STATUS!=0`, may still update fence (implementation-defined; must be documented if used).

---

## 11. Example Sequences

### 11.1 Submit a kernel and wait
1. Host writes a `DISPATCH` entry with `WAIT_FENCE=0`, `EMIT_FENCE=F+1`.
2. Host advances `CMDQ_PROD_BYTES` and writes `CMDQ_DOORBELL=1`.
3. Host polls `FENCE_VALUE` until it reaches `F+1` (or uses interrupt).

### 11.2 Render then swap
1. Render work completes and emits fence `F_render_done`.
2. Host/CP enqueues `MEMBAR_SYS` with `WAIT_FENCE=F_render_done`.
3. Host/CP enqueues `DISPLAY_SWAP` to the new front buffer.

### 11.3 Abort
1. Host writes `ABORT_REQUEST=1`.
2. CP transitions `ABORT_STATUS.IN_PROGRESS=1`, stops launching new work, and attempts to quiesce.
3. CP sets `ABORT_STATUS.DONE=1` when safe; if timeout triggers, CP forces reset and sets `ABORT_STATUS.FORCED_RESET=1`.

