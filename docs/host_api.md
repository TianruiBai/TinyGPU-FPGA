# Host API

**DEPRECATED**. Please refer to [tgpu_api.md](tgpu_api.md) for the definitive Host API documentation.

This file is kept for historical reference only.

// 2D ops
void tgpu_cmd_fill_rect(tgpu_ctx_t *ctx, uint32_t dst_addr, uint16_t w, uint16_t h, uint32_t color);
void tgpu_cmd_blit(tgpu_ctx_t *ctx, uint32_t dst_addr, uint32_t src_addr, uint16_t w, uint16_t h);

// Compute
void tgpu_cmd_dispatch_1d(tgpu_ctx_t *ctx, uint32_t kernel_id, uint32_t grid, uint32_t block, uint32_t arg_ptr);

// Present
void tgpu_cmd_set_fb(tgpu_ctx_t *ctx, uint32_t base, uint32_t stride, uint32_t fmt);
void tgpu_cmd_swap_fb(tgpu_ctx_t *ctx, uint32_t front_base, uint32_t back_base);

// Sprites / Layers
void tgpu_cmd_sprite_draw(tgpu_ctx_t *ctx, uint32_t layer_base, uint32_t sprite_base, uint16_t w, uint16_t h);
void tgpu_cmd_layer_blend(tgpu_ctx_t *ctx, uint32_t dst_layer, uint32_t src_layer, uint8_t alpha_8_8);
void tgpu_cmd_clear_layer(tgpu_ctx_t *ctx, uint32_t layer_base, uint16_t w, uint16_t h, uint32_t color);

// Sync
void tgpu_wait_fence(tgpu_ctx_t *ctx, uint32_t fence);
bool tgpu_poll_fence(tgpu_ctx_t *ctx, uint32_t fence);
```

Notes:
- Commands are queued in an in-memory buffer then pushed to CMD window on `tgpu_cmd_end_submit`.
- `irq_on_done` sets the IRQ flag; fences let MCU poll/await completion without blocking CPU.
- `arg_ptr` points to kernel arguments in VRAM; firmware loads it when kicking the CU.
- Keep payload uploads explicit via `tgpu_upload`; avoids hidden copies for MCUs.
- Host uploads/reads should stay within the exposed VRAM aperture; firmware enforces bounds for safety.

## Example: Packing and Submitting a Command Buffer
```c
tgpu_ctx_t ctx;
tgpu_init(&ctx, CMD_BASE, VRAM_BASE);

tgpu_cmd_begin(&ctx);
tgpu_upload(&ctx, TEX_ADDR, tex_data, TEX_SIZE);
tgpu_cmd_sprite_draw(&ctx, LAYER0_BASE, TEX_ADDR, 64, 64);
tgpu_cmd_layer_blend(&ctx, FB_BACK, LAYER0_BASE, 0x0100); // alpha=1.0 in 8.8
uint32_t fence = tgpu_cmd_end_submit(&ctx, true);

tgpu_wait_fence(&ctx, fence);
tgpu_cmd_begin(&ctx);
tgpu_cmd_swap_fb(&ctx, FB_FRONT, FB_BACK);
tgpu_cmd_end_submit(&ctx, false);
```
