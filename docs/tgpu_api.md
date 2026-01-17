# TinyGPU Host API (TGPU)

For the forward-looking detailed plan/spec ("mini-Vulkan" direction, transports, queues, barriers, and roadmap), see:
- `docs/tgpu_api_plan.md`

The TinyGPU Host API is a low-overhead, asynchronous command submission library designed for the ESP32-S3 (Host) to control the FPGA-based GPU. It draws inspiration from modern APIs like WebGPU and Metal but is simplified for embedded systems.

## Design Philosophy
1.  **Asynchronous:** The Host pushes commands to a FIFO. The GPU executes them when able.
2.  **Explicit Synchronization:** Fences are used to track completion.
3.  **Unified Memory (Virtual):** The Host has a window into VRAM, but transfers should be explicit to manage bandwidth.
4.  **State Objects:** Pipeline states are immutable and pre-created to minimize command traffic.

---

## 1. Core Types & Context

```c
#include <stdint.h>
#include <stdbool.h>

// Opaque handles
typedef uint32_t tgpu_handle_t;
typedef uint32_t tgpu_buffer_t;     // VRAM pointer
typedef uint32_t tgpu_texture_t;    // Texture descriptor ID
typedef uint32_t tgpu_pipeline_t;   // Pipeline State Object ID

typedef struct {
    void*     spi_device_handle;    // OSPI/QSPI Device Handle
    uint32_t  vram_base_addr;       // Base address map 
    uint8_t   *cmd_buffer;          // Local RAM buffer for batching
    size_t    cmd_buffer_limit;
    size_t    cursor;
} tgpu_ctx_t;

typedef enum {
    TGPU_OK = 0,
    TGPU_ERR_TIMEOUT,
    TGPU_ERR_OOM,
    TGPU_ERR_INVALID
} tgpu_err_t;

// Initialize the context
tgpu_err_t tgpu_init(tgpu_ctx_t *ctx, void *spi_handle);
```

---

## 2. Memory Management (VRAM)

Direct access to VRAM is slow. Use bulk transfers for assets.

```c
// Allocate VRAM (Simple linear allocator in Firmware)
tgpu_buffer_t tgpu_malloc(tgpu_ctx_t *ctx, size_t size);

// Free VRAM
void tgpu_free(tgpu_ctx_t *ctx, tgpu_buffer_t ptr);

// FAST DATA PATH: Direct OSPI write (bypasses Command FIFO)
// Use this for uploading Textures, Vertex Buffers, etc.
void tgpu_memcpy_to_device(tgpu_ctx_t *ctx, tgpu_buffer_t dst, const void *src, size_t size);
```

---

## 3. Pipeline & State

To reduce bandwidth, all render state is compiled into a Pipeline Object.

```c
typedef enum {
    TGPU_FMT_RGBA8888,
    TGPU_FMT_RGB565,
    TGPU_FMT_A8
} tgpu_format_t;

typedef enum {
    TGPU_BLEND_NONE,
    TGPU_BLEND_ALPHA,
    TGPU_BLEND_ADDITIVE
} tgpu_blend_t;

typedef struct {
    uint16_t width;
    uint16_t height;
    tgpu_format_t format;
} tgpu_texture_desc_t;

// Create a texture resource descriptor
tgpu_texture_t tgpu_create_texture(tgpu_ctx_t *ctx, tgpu_texture_desc_t desc, tgpu_buffer_t vram_ptr);

// Compute Pipeline (Shader)
tgpu_pipeline_t tgpu_create_compute_pipeline(tgpu_ctx_t *ctx, uint32_t kernel_id);

// Graphics Pipeline (Rasterization State)
typedef struct {
    uint32_t        vertex_shader_id;
    uint32_t        pixel_shader_id;
    tgpu_blend_t    blend_mode;
    tgpu_format_t   target_format;
    bool            depth_test;
} tgpu_graphics_pipeline_desc_t;

tgpu_pipeline_t tgpu_create_graphics_pipeline(tgpu_ctx_t *ctx, tgpu_graphics_pipeline_desc_t *desc);
```

---

## 4. Command Encoding (The Render Loop)

Commands are recorded locally and flushed in bursts.

```c
// Begin a new command batch
void tgpu_cmd_begin(tgpu_ctx_t *ctx);

// --- RENDER PASS ---
typedef struct {
    tgpu_texture_t target;      // 0 for Screen/Backbuffer
    uint32_t       clear_color; // 0xAABBGGRR
    bool           clear;       // Perform clear on load?
} tgpu_pass_desc_t;

void tgpu_cmd_begin_render_pass(tgpu_ctx_t *ctx, tgpu_pass_desc_t pass);
void tgpu_cmd_end_render_pass(tgpu_ctx_t *ctx);

// --- STATE & BINDING ---
void tgpu_cmd_bind_pipeline(tgpu_ctx_t *ctx, tgpu_pipeline_t pipeline);
void tgpu_cmd_bind_vertex_buffer(tgpu_ctx_t *ctx, tgpu_buffer_t buffer);
void tgpu_cmd_bind_texture(tgpu_ctx_t *ctx, uint8_t slot, tgpu_texture_t texture);

// Push Constants (Small globals: Matrices, Colors, Time) - Max 128 bytes
void tgpu_cmd_push_constants(tgpu_ctx_t *ctx, void *data, size_t size);

// --- DRAWING ---
void tgpu_cmd_draw(tgpu_ctx_t *ctx, uint32_t vertex_count, uint32_t first_vertex);
void tgpu_cmd_draw_indexed(tgpu_ctx_t *ctx, uint32_t index_count, tgpu_buffer_t index_buf);

// --- COMPUTE ---
void tgpu_cmd_dispatch(tgpu_ctx_t *ctx, uint32_t x, uint32_t y, uint32_t z);

// --- BARRIERS ---
// Wait for all previous Draw/Dispatch to finish writing VRAM
void tgpu_cmd_pipeline_barrier(tgpu_ctx_t *ctx);

// --- SUBMISSION ---
// Flushes the command buffer to the FPGA FIFO. 
// Returns a Fence ID to track progress.
uint32_t tgpu_cmd_submit(tgpu_ctx_t *ctx);
```

---

## 5. Synchronization & Status

The GPU runs asynchronously. Frame swapping is synchronized to V-Sync.

```c
// Blocking wait for a fence value
void tgpu_wait_fence(tgpu_ctx_t *ctx, uint32_t fence_id);

// Check if fence is passed (Non-blocking)
bool tgpu_check_fence(tgpu_ctx_t *ctx, uint32_t fence_id);

// Present the current Backbuffer to the display (Flips on Next VSync)
void tgpu_present(tgpu_ctx_t *ctx);
```

---

## Usage Example (C++)

```cpp
tgpu_ctx_t gpu;
tgpu_init(&gpu, my_spi_handle);

// 1. Setup Assets (Once)
tgpu_buffer_t vbo = tgpu_malloc(&gpu, sizeof(vertices));
tgpu_memcpy_to_device(&gpu, vbo, vertices, sizeof(vertices));

tgpu_pipeline_t pipeline = tgpu_create_graphics_pipeline(&gpu, &my_pipeline_desc);

// 2. Render Loop
while(1) {
    tgpu_cmd_begin(&gpu);
    
    // Clear screen to Blue
    tgpu_pass_desc_t pass = { .target=0, .clear_color=0xFFFF0000, .clear=true };
    tgpu_cmd_begin_render_pass(&gpu, pass);

    tgpu_cmd_bind_pipeline(&gpu, pipeline);
    tgpu_cmd_bind_vertex_buffer(&gpu, vbo);
    
    // Update Rotation Matrix
    Matrix4x4 mat = GetRotation();
    tgpu_cmd_push_constants(&gpu, &mat, sizeof(mat));
    
    tgpu_cmd_draw(&gpu, 3, 0); // Draw Triangle
    
    tgpu_cmd_end_render_pass(&gpu);
    
    // Submit
    uint32_t fence = tgpu_cmd_submit(&gpu);
    
    // Swap Buffers
    tgpu_present(&gpu); 
    
    // Optional: Wait to prevent queue flooding
    // tgpu_wait_fence(&gpu, fence);
}
```

## Binary Wire Protocol (Packet Format)

All commands are sent as 32-bit aligned packets over QSPI.

| Bits [31:24] | Bits [23:0] | Description |
| :--- | :--- | :--- |
| `0x01` | `Count` | **NOP** / Padding |
| `0x10` | `Reg Addr` | **WRITE_REG** (Followed by 32-bit Value) |
| `0x20` | `PipelineID` | **SET_PIPELINE** |
| `0x30` | `Count` | **DRAW** (Start vertex implicit or next word) |
| `0x31` | `Count` | **DRAW_INDEXED** |
| `0x40` | `X Dim` | **DISPATCH** (Followed by Y, Z words) |
| `0x50` | `FenceID` | **SIGNAL_FENCE** |
| `0xFF` | `0` | **END_BATCH** |

> Note: Texture Uploads use a special "Raw Mode" on the SPI bus (Address + Data Burst) and are not part of the command stream FIFO.
