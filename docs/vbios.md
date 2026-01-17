# VBIOS & Boot Sequence

TinyGPU supports two boot modes:
1.  **Standalone (I2C EEPROM):** Traditional GPU style. The MCU loads firmware from an attached EEPROM.
2.  **Host-Driven (Direct Load):** The ESP32 holds the firmware image and uploads it at startup. (Preferred for lower BOM).

This document focuses on the **host-driven path** today and lays out a concrete plan for a more robust “VBIOS” container format and fallback behavior.

## Host-Driven Boot Flow (Recommended)
This approach removes the need for a dedicated EEPROM on the FPGA board.

1.  **Power On:** FPGA loads bitstream. MCU enters `RESET` state.
2.  **Enumeration:** ESP32 probes OSPI. Reads `DEVICE_ID` register (hardcoded in RTL).
3.  **Firmware Upload:** 
    -   ESP32 writes the generic `GPU_FIRMWARE.bin` to VRAM at address `0x2000_0000` (Fast Path).
4.  **Boot Vectors:**
    -   ESP32 writes the start address to `MCU_BOOT_ADDR` register.
    -   ESP32 releases `MCU_RESET`.
5.  **Initialization:**
    -   MCU boots, reads VRAM resident code to Instruction Cache.
    -   MCU configures default Display Mode.
    -   MCU signals `GPU_READY` in Status Register.

### Recommended responsibilities split
- **Boot ROM (minimal, immutable)**
    - Brings up clocks/resets.
    - Initializes the VRAM interface enough to fetch a small firmware header.
    - Provides a safe “no firmware” display/test pattern path.
- **CP firmware (updatable, loaded image)**
    - Implements the command protocol parser.
    - Configures display modes and scanout.
    - Implements watchdog/abort policy.

---

## Firmware / VBIOS Image Format (v1 plan)

The current document describes a simple `.bin` layout. For bring-up that is fine, but for real boards we want:
- integrity checking (CRC)
- explicit segment table (no “magic offsets”)
- optional authenticity (signature) later

### v1 container header (proposed)
All fields little-endian.

| Offset | Size | Field |
| :--- | :--- | :--- |
| `0x0000` | 4B | Magic = `TGPU` |
| `0x0004` | 2B | Container major |
| `0x0006` | 2B | Container minor |
| `0x0008` | 4B | Total image bytes |
| `0x000C` | 4B | Header bytes |
| `0x0010` | 4B | Entry point (VRAM address or relative-to-load-base; choose one and document) |
| `0x0014` | 4B | Segment table offset |
| `0x0018` | 4B | Segment count |
| `0x001C` | 4B | CRC32 of image (excluding this field), or CRC32 of payload region |

### v1 segment entry (proposed)
| Field | Size | Meaning |
| :--- | :--- | :--- |
| `type` | 4B | `TEXT`, `RODATA`, `DATA`, `BSS`, `KERNEL_PACK`, `SYMBOLS` (optional) |
| `dst_vram` | 4B | Destination VRAM address |
| `src_off` | 4B | Source offset in image |
| `size_bytes` | 4B | Bytes to copy |
| `flags` | 4B | e.g. compress, readonly, exec |

Notes:
- `BSS` has no `src_off` bytes; firmware zero-fills `size_bytes`.
- `KERNEL_PACK` is a table of `{kernel_id, code_ptr, code_size, caps}`.

---

## VRAM reservations (plan)

To avoid conflicts between firmware, command buffers, and app assets, define a small reserved map *in VRAM*.
Exact base addresses depend on the final “VRAM base” convention; prefer expressing as offsets from `VRAM_BASE`.

Recommended regions:
- `VRAM_BASE + 0x0000_0000 .. +0x0007_FFFF`: firmware staging / image copy
- `VRAM_BASE + 0x0008_0000 .. +0x0008_FFFF`: command buffers / rings (if using ring submission)
- `VRAM_BASE + 0x0009_0000 .. +0x0009_FFFF`: completion/status records (optional)
- remaining: framebuffers, textures, VBO/IBO, user allocations

---

## Standalone / fallback boot (re-evaluated plan)

### v0 (today)
- Host-driven only: ESP32 uploads image and releases reset.

### v1 (robust host-driven)
- Add CRC32 validation of the uploaded image.
- If CRC fails: keep CP in reset and show test pattern; expose error via `LAST_ERROR`.

### v2 (dual-image fallback)
- Support two images: `A` (active) and `B` (fallback) either:
    - on EEPROM, or
    - in a protected VRAM region that host can update explicitly.
- Boot ROM picks newest valid image; if it crashes, fall back.

### v3 (auth + field updates)
- Optional signature on image header.
- Version negotiation: host can query `ID_VERSION` and decide which firmware image to upload.

---

## Implementation Notes
- **Performance:** executing directly from VRAM is acceptable for bring-up but strongly consider a small I-cache/TCM for CP firmware.
- **Safety:** boot ROM must validate lengths/addresses before copying segments into VRAM.
- **Recovery:** define a host-visible “force reset CP” control bit and an “abort/quiesce” path.

## Legacy `.bin` layout (kept for reference)
If you keep a simple flat binary for early bring-up, keep the first 16–32 bytes compatible with the v1 container header so it’s forward-migratable.
