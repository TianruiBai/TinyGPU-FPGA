# Architecture

## Goals
- Offload render/compute from ESP32-S3 over QSPI/OSPI.
- Reuse ProtoTracer compute units and display IP where possible.
- Keep a small, deterministic command protocol and host library.

## Current RTL Coverage (This Workspace)
This document describes the intended system architecture, but the RTL currently present in this workspace is concentrated in the compute unit and simulation stubs:

- **Compute unit RTL**: `ip/compute unit/compute_unit_top.sv` (plus front-end/exec/LSU/graphics submodules).
- **System memory bridge stub (simulation/bring-up)**: `ip/interface/octal-spi/ospi_controller.sv` (bridges the compute-unit global `req/resp` bus to an internal RAM).
- **Integration evidence**: testbenches under `testbench/` (e.g. `compute_unit_full_tb.sv`, `gfx_*_tb.sv`) include ROM + memory models and exercise the global bus.

Blocks described below that are not present (or are empty directories) should be treated as **planned** rather than verified RTL in this repo snapshot.

Notes from the current tree:
- `ip/control_proc/` is empty.
- `ip/interface/hub75/` exists but has no controller RTL in this workspace.

UML diagram (implemented vs planned): `docs/diagrams/system_architecture.puml`

## System Overview
### As-built (current RTL scope)
- **Host-facing link**: not integrated in RTL here; testbenches model the global memory interface directly.
- **Compute Unit**: `compute_unit_top.sv` with scalar/FP/vector, LSU, and graphics pipeline.
- **Global memory**: 32-bit request/response interface (`data_req_*` / `data_resp_*`).
- **Local memory**: on-chip BRAM via `local_mem_banked.sv` (selected when `addr[31]==0`).
- **Global memory selection**: `addr[31]==1` selects the external/global path (OSPI/PSRAM in the full system).

### Planned (full system)
- **Host (ESP32-S3)**: sends commands/data via QSPI/OSPI.
- **Ingress Bridge**: CDC FIFO + command parser.
	- **Fast Path (Data)**: DMA directly to VRAM (textures, buffers).
	- **Slow Path (Control)**: mailbox for a control MCU.
- **Control MCU (RISC-V)**: consumes mailbox, configures registers, manages fences.
- **Compute Unit Cluster (4x)**: shared VRAM + texture cache.
- **Memory Subsystem**: VRAM controller + QoS arbiter (Display > Compute > Host > MCU).
- **Display Path**: scanout engine with line buffers; HUB75/HDMI.

## Data Flow
### As-built (current RTL)
1. Testbench/host model provides instruction fetch and global memory responses.
2. CU executes scalar/FP/vector and graphics ops.
3. LSU targets local BRAM when `addr[31]==0`, and global interface when `addr[31]==1`.

### Planned (full system)
1. **Host** writes packet stream via QSPI/OSPI.
2. **Ingress HW** parses header.
   - If `WRITE_MEM`: payload piped directly to VRAM controller.
   - If `CMD_PKT`: payload pushed to MCU mailbox.
3. **MCU** reads mailbox, configures work distribution.
4. **Work Distributor** assigns tiles (X,Y) to free CUs.
5. **CUs** fetch instructions/data from VRAM, process, and write back.
6. **Display Scanout** continuously reads framebuffer to drive HUB75/HDMI.

## Display Path Architecture
The system supports two distinct physical output modes sharing the same Framebuffer fetch engine.

1.  **HUB75 Engine (LED Matrix):**
    -   **Target:** Low-resolution LED panels (e.g., 64x64, 128x64).
    -   **Features:** Hardware Binary Code Modulation (BCM) for high color depth.
    -   **Chaining:** Supports up to 4 panels in series (e.g., 256x64 total resolution).
    -   **Gamma:** Hardware Look-Up Table (LUT) for precise color calibration on non-linear LEDs.

2.  **TMDS Engine (HDMI):**
    -   **Target:** External Monitors (720p, 1080p).
    -   **Features:** Full DVI/HDMI signaling with 10:1 DDR serialization.
    -   **Usage:** Activated when `DISP_RES` is set to standard video resolutions (e.g., > 640x480).

## Compute Unit Cluster / Arbiter (sketch)
```
Host QSPI/OSPI -> Ingress FIFO -> Cmd Processor -> Scheduler
												 |             |
												 v             |
											Descriptors       |
												 |             |
										 +-----+-------------+
										 | round-robin / queued
										 | dispatch arbiter
										 +--+--+--+--+
											 |  |  |  |
											 v  v  v  v
										 CU0 CU1 CU2 CU3   (ProtoTracer compute_unit RTL)
											 \  |  |  /
											  \ |  | /
												\|  |/
											VRAM Controller -> VRAM (SRAM/PSRAM)
													  |
											Tile/Line Buffer
													  |
												  Scanout
```
- Scheduler issues kernels to an input queue; arbiter drains queue and feeds the first available CU.
- Each CU shares the unified VRAM port; VRAM controller arbitrates with scanout QoS.
- Optional per-CU outstanding limit to avoid over-subscribing VRAM bandwidth.

## Display Path
- Uses existing ProtoTracer display controllers (HUB75/APA102).
- Scanout is bandwidth-protected via tile/line buffer; double-buffer expected.

## Open Decisions
- QSPI vs OSPI DDR timings, burst sizes, watermark/IRQ policy.
- RISC-V core choice (RV32IMC vs RV32I) and CDC with ingress.
- CU kernel/shader format: ProtoTracer microcode vs trimmed ISA.
- VRAM choice (SRAM vs PSRAM) and cache/buffer policy.
- Security/robustness and bounds checking.
