# TinyGPU Host API Plan (Mini‑Vulkan)

This document is the **forward plan/spec** for the TinyGPU host library (“tGPU API”).
It is transport‑agnostic and intended to work over:
- **QSPI** (4‑bit)
- **OSPI** (8‑bit)
- **Hex SPI / PIO‑style parallel** (6‑bit) where supported by MCU programmable I/O

It complements:
- `docs/command_protocol.md` (wire framing/protocol)
- `docs/tgpu_wire_v1.md` (v1 API → opcode mapping)
- `docs/memory_map.md` (address map)
- `docs/system_contracts.md` (ordering/fences/QoS)
- `docs/microarchitecture.md` (as-built CU/graphics behavior)

Scope here: host‑side API surface, object model, command submission, synchronization, and the device/transport split.

---

## 0. Design Goals (Why mini‑Vulkan)

- **Explicit**: no hidden synchronization; barriers and fences are first-class.
- **Asynchronous**: host records commands into a buffer; submission is non-blocking by default.
- **Small**: fits embedded constraints; avoids huge reflection and state machines.
- **Stable wire**: transport is pluggable; commands are versioned and feature‑queryable.
- **Practical**: supports both **graphics** (raster/ROP) and **compute** (kernels) with one API.

Non-goals (for v1):
- Full OpenGL‑style immediate mode.
- Full Vulkan compatibility.
- Complex shader compilation on host.

---

## 1. Architecture Overview

### 1.1 Layers

1. **Application**
2. **tGPU API (C library)**
3. **Transport backend**: `qspi`, `ospi`, `hexpio` (PIO‑like), `mock` (unit tests)
4. **Wire protocol**: packet framing + mailbox/ring submission
5. **Device**: command processor + scheduler + compute units + display

The API must not assume a specific transport speed or full‑duplex capability.

### 1.2 Key Design Choice: Two planes

- **Control plane**: small MMIO/register ops (doorbells, status, fences, queue control)
- **Data plane**: bulk VRAM transfers (textures/VBO/IBO, readback) using best transport mode

Both planes share the same transport object but have different performance expectations.

---

## 2. Versioning, Capabilities, and Feature Bits

### 2.1 ABI/API version

- Library exposes `TGPU_API_VERSION` (semantic version)
- Device exposes `ID_VERSION` + `ID_FEATURES`

### 2.2 Capability query

```c
typedef struct {
  uint32_t api_version;      // tGPU library version
  uint32_t dev_version;      // device build
  uint32_t features;         // feature bits
  uint32_t limits_flags;     // which limits are valid
  uint32_t max_cmd_bytes;
  uint32_t max_inflight_submits;
  uint32_t max_queues;
  uint32_t max_textures;
  uint32_t max_pipelines;
  uint32_t max_push_bytes;
  uint32_t max_vbo_bytes_per_submit;
  uint32_t max_readback_bytes;
} tgpu_caps_t;

tgpu_err_t tgpu_query_caps(tgpu_ctx_t* ctx, tgpu_caps_t* out);
```

Feature bits should include at least:
- `TGPU_FEAT_GFX`
- `TGPU_FEAT_TEX`
- `TGPU_FEAT_COMPUTE`
- `TGPU_FEAT_RGB565`
- `TGPU_FEAT_RGBA8888`
- `TGPU_FEAT_PRESENT`
- `TGPU_FEAT_TIMESTAMPS` (optional)

---

## 3. Transport Abstraction

### 3.1 Transport interface (host side)

The transport backend is responsible for:
- register reads/writes
- burst memory reads/writes
- optional async DMA usage
- optional half‑duplex/full‑duplex constraints

```c
typedef struct tgpu_transport_vtbl {
  tgpu_err_t (*mmio_write32)(void* user, uint32_t addr, uint32_t value);
  tgpu_err_t (*mmio_read32) (void* user, uint32_t addr, uint32_t* out);

  tgpu_err_t (*mem_write)(void* user, uint32_t vram_addr, const void* src, size_t bytes);
  tgpu_err_t (*mem_read) (void* user, uint32_t vram_addr, void* dst, size_t bytes);

  tgpu_err_t (*submit_cmd_bytes)(void* user, const void* cmd, size_t bytes);

  void (*sleep_us)(void* user, uint32_t us);
  uint64_t (*time_us)(void* user);
} tgpu_transport_vtbl_t;

typedef struct {
  const tgpu_transport_vtbl_t* vtbl;
  void* user;
  uint32_t flags; // e.g. supports_async_dma, supports_full_duplex
} tgpu_transport_t;
```

### 3.2 QSPI/OSPI/Hex‑PIO notes

- **QSPI/OSPI**: prefer hardware SPI with DMA for `mem_write`/`mem_read`.
- **Hex‑PIO**: treat as a “wide synchronous shift bus” with programmable I/O; best for MCUs with flexible I/O engines.
- All transports must support **aligned burst** writes (library will pad).

---

## 4. Object Model (Mini‑Vulkan)

### 4.1 Core objects

- `tgpu_instance_t` (optional; may be compile‑time omitted)
- `tgpu_device_t` (one physical GPU)
- `tgpu_queue_t` (submission endpoint)
- `tgpu_cmd_pool_t` / `tgpu_cmd_buf_t`

### 4.2 Resources

- `tgpu_buffer_t` (VRAM allocation: vertex/index/storage/uniform)
- `tgpu_texture_t` (descriptor + backing memory)
- `tgpu_sampler_t` (filter/wrap state; may be baked into texture descriptor)

### 4.3 Pipelines

- `tgpu_compute_pipeline_t` (kernel id + layout)
- `tgpu_gfx_pipeline_t` (raster/ROP state, format)

### 4.4 Descriptors / binding model

Mini‑Vulkan binding sets:
- `set=0`: frame globals (push constants / small uniforms)
- `set=1`: resource table (buffers/textures)

For v1 simplicity, use:
- **fixed small descriptor table** in VRAM
- bind by **indices** (32-bit handles)

---

## 5. Command Recording

### 5.1 Command buffer lifecycle

```c
tgpu_err_t tgpu_cmd_alloc(tgpu_device_t* dev, tgpu_cmd_buf_t** out);
void tgpu_cmd_reset(tgpu_cmd_buf_t* cb);
void tgpu_cmd_begin(tgpu_cmd_buf_t* cb);
void tgpu_cmd_end(tgpu_cmd_buf_t* cb);
```

### 5.2 Graphics commands (v1)

- `begin_render_pass(target, clear)`
- `set_viewport/scissor`
- `bind_gfx_pipeline`
- `bind_vertex_buffer` / `bind_index_buffer`
- `bind_texture(slot, tex)`
- `push_constants(bytes<=N)`
- `draw` / `draw_indexed`
- `end_render_pass`

### 5.3 Compute commands (v1)

- `bind_compute_pipeline`
- `bind_storage_buffer`
- `dispatch(x,y,z)`

### 5.4 Copy/transfer commands (optional v1)

- `copy_buffer`
- `blit_texture` (future)

---

## 6. Submission Model

### 6.1 Queue submit

```c
typedef struct {
  tgpu_cmd_buf_t* const* cmd_bufs;
  uint32_t cmd_buf_count;
  uint32_t signal_fence;   // 0 means "no fence"
  uint32_t wait_fence;     // optional dependency
} tgpu_submit_info_t;

tgpu_err_t tgpu_queue_submit(tgpu_queue_t* q, const tgpu_submit_info_t* info);
```

### 6.2 Timeline fences

- Fences are **monotonic** 32-bit values (wrap-safe compare).
- Host can `wait` (blocking) or `poll`.

```c
uint32_t tgpu_fence_get(tgpu_device_t* dev);
tgpu_err_t tgpu_fence_wait(tgpu_device_t* dev, uint32_t value, uint32_t timeout_ms);
bool tgpu_fence_poll(tgpu_device_t* dev, uint32_t value);
```

### 6.3 Present

Two approaches:
1. **Explicit present command** (recommended): `tgpu_queue_present(queue, swapchain)`
2. **MMIO flip** (fallback): host writes scanout base/stride registers

---

## 7. Memory Management

### 7.1 Allocators

Provide a simple default allocator:
- linear arena for transient allocations
- free‑list for long‑lived buffers

```c
tgpu_err_t tgpu_vram_alloc(tgpu_device_t* dev, size_t bytes, uint32_t align, tgpu_buffer_t* out);
void tgpu_vram_free(tgpu_device_t* dev, tgpu_buffer_t buf);
```

### 7.2 Host↔device transfers

- `tgpu_upload_buffer(buf, offset, src, bytes)` uses **data plane** burst writes.
- `tgpu_readback_buffer(buf, offset, dst, bytes)` optional.

Transfers should be optionally **staged** through an upload ring to minimize per‑transfer overhead.

---

## 8. Synchronization and Barriers

### 8.1 Barrier primitive

```c
typedef enum {
  TGPU_STAGE_HOST = 1<<0,
  TGPU_STAGE_COPY = 1<<1,
  TGPU_STAGE_COMPUTE = 1<<2,
  TGPU_STAGE_GFX = 1<<3,
  TGPU_STAGE_TEX = 1<<4,
  TGPU_STAGE_ROP = 1<<5,
  TGPU_STAGE_DISPLAY = 1<<6,
} tgpu_stage_t;

typedef enum {
  TGPU_ACCESS_READ  = 1<<0,
  TGPU_ACCESS_WRITE = 1<<1,
} tgpu_access_t;

typedef struct {
  tgpu_stage_t src_stage;
  tgpu_access_t src_access;
  tgpu_stage_t dst_stage;
  tgpu_access_t dst_access;
  uint32_t vram_addr;   // 0 = whole VRAM
  uint32_t bytes;       // 0 = whole range
} tgpu_barrier_t;

void tgpu_cmd_barrier(tgpu_cmd_buf_t* cb, const tgpu_barrier_t* b);
```

Mapping to hardware:
- v1 barrier lowers to `MEMBAR` + (optional) cache invalidations.
- Presentation requires a barrier ensuring ROP writes are visible to scanout.

---

## 9. Error Handling and Robustness

### 9.1 Error codes

- `TGPU_OK`, `TGPU_ERR_TIMEOUT`, `TGPU_ERR_IO`, `TGPU_ERR_PROTO`, `TGPU_ERR_OOM`, `TGPU_ERR_UNSUPPORTED`.

### 9.2 Validation layers (optional)

A compile‑time `TGPU_ENABLE_VALIDATION` mode:
- bounds checks for uploads
- object lifetime tracking
- command buffer size limits

---

## 10. Wire Encoding (Command Stream)

The host API records commands into a byte buffer; the transport sends it via `submit_cmd_bytes()`.

Requirements:
- 32-bit alignment
- little-endian words
- versioned header: `MAGIC`, `VER`, `LEN`, `CRC` (optional)

Command set should be structured like Vulkan:
- `CMD_BEGIN`, `CMD_END`
- resource bindings
- draw/dispatch
- barrier
- signal fence

This is intentionally not fully specified here; see `docs/command_protocol.md` for on-wire formats.

---

## 11. Typical Usage Patterns

### 11.1 “Draw triangle”
1. allocate/upload VBO
2. create pipeline
3. record render pass + draw
4. submit + signal fence
5. present

### 11.2 “Compute + readback”
1. allocate storage buffer
2. upload inputs
3. record dispatch + barrier
4. submit + wait fence
5. read back

### 11.3 “Streaming UI”
- persistent mapped upload ring
- multiple in-flight frames with timeline fences
- present at vblank with minimal stalls

---

## 12. Implementation Roadmap (Library)

### Phase A (bring-up)
- transport abstraction
- caps query
- simple VRAM alloc
- submit raw cmd bytes
- fences

### Phase B (mini‑gfx)
- pipeline + render pass
- vertex/index buffers
- texture upload + sampling

### Phase C (mini‑compute)
- compute pipelines
- storage buffers
- barriers

### Phase D (polish)
- validation layer
- profiling counters/timestamps
- async uploads and batched readback
