# Tiny GPU Implementation Roadmap

This document outlines the step-by-step execution plan to build the Tiny GPU. It moves from "Hello World" (blinking LED equivalent) to a fully functional GPU.

## Phase 0: Environment & Simulation Setup
**Goal:** A working simulation environment where Python (Host) talks to Verilog (GPU).

1.  **Repo Structure Setup**:
    -   Create folders: `rtl/`, `sim/`, `firmware/`, `ip/`.
    -   Setup **Cocotb** for testbenches.
    -   Create a `Makefile` that compiles RTL with Verilator/Icarus.

2.  **Model Creation**:
    -   **PSRAM Model:** A simple Python/Verilog behavioral model that mimics Octal SPI RAM (latency + bursts).
    -   **Host Model:** A Python class that sends OSPI command packets (CS, CLK, D0-D7).

## Phase 1: The Memory Backbone (Host <-> VRAM)
**Goal:** The ESP32 (Host) can transparently read/write the PSRAM (VRAM) attached to the FPGA. This is the foundation of everything.

### Sprint 1.1: Interfaces (PHY Layer)
-   **`rtl/host_if/ospi_slave_phy.sv`**:
    -   Implement DDR Input Flip-Flops (IDDR).
    -   Handle DQS (Data Strobe) synchronization.
    -   Shift registers to deserialize bytes.
-   **`rtl/vram_if/ospi_master_phy.sv`**:
    -   Implement DDR Output Flip-Flops (ODDR).
    -   Clock generation for the PSRAM (90-degree phase shift key).

### Sprint 1.2: Controller Logic & Fabric
-   **`rtl/vram_if/psram_controller.sv`**:
    -   State Machine: `IDLE` -> `CMD` -> `ADDR` -> `LATENCY` -> `DATA` -> `CS_HIGH`.
    -   **Startup:** Implements Infineon initialization sequence (Latency Code, Drive Strength).
    -   **Interface:** Native high-speed interface.
-   **`rtl/vram_if/wb_async_bridge.sv` (CRITICAL)**:
    -   Bridges the **Fast VRAM Domain (~166MHz)** to the **Slower System Domain (~50MHz)**.
    -   Handles Wishbone handshake synchronization (ACK/STALL) across safe clock domain boundaries.
-   **`rtl/core/interconnect.sv`**:
    -   **Bus Architecture:** Pipelined Wishbone (Simple 32-bit Address/Data).
    -   **Arbiter:** Weighted Round-Robin (Starvation avoidance).
    -   **Masters:** Host Bridge, MCU, Display, Compute Cluster.
    -   **Slaves:** VRAM Controller (via Bridge), CSR Bank, MCU RAM.

### Sprint 1.3: Verification (Loopback)
-   **Test:** Write `0xDEADBEEF` to Address `0x100` via Host BFM -> Wishbone Fabric -> Async Bridge -> VRAM.
-   **Metric:** Ensure no data corruption and proper `ACK` signal propagation across clock domains.

## Phase 2: The Display Engine (Video Out)
**Goal:** Get a stable image on screen. Requires strict QoS guarantees.

### Sprint 2.1: Timing & Scanout
-   **`rtl/display/timing_gen.sv`**: Generates HSYNC, VSYNC, DE signals.
-   **`rtl/display/scanout_engine.sv`**:
    -   Acts as a **Wishbone Master** (Read-Only).
    -   Line Buffer: Fetches scanlines ahead of time.
    -   **Critical:** High priority on the Bus Arbiter.

### Sprint 2.2: Video PHY (Dual-Mode)
The design must support low-res LED matrices and high-res HDMI monitors.
-   **`rtl/display/phy_hub75.sv` (Primary Target):**
    -   Implements Binary Code Modulation (BCM) for color depth.
    -   **Chaining Support:** Configurable up to **4 chained panels** (e.g., 64x128 total).
    -   **Gamma Correction:** LUT-based correction for LEDs.
-   **`rtl/display/phy_tmds.sv` (High-Res Target):**
    -   Full 10:1 Serializer (DDR) for 720p/1080p output.
    -   Active when higher resolutions are selected.
-   **Test:** Host uploads a test pattern. Verify output on LED Matrix (Real HW) and HDMI (Sim/HW).

## Phase 3: The Brain (MCU & Custom Extensions)
**Goal:** Low-latency control using a customized RISC-V core. We avoid complex AXI buses in favor of tight integration.

### Sprint 3.1: Custom RISC-V Core Setup
-   **Core Selection:** `PicoRV32` (Native Interface) or `FemtoRV` (easy to hack).
-   **Integration Strategy:** Use the core's **Co-Processor Interface (PCPI)** or "Lookaside Interface" to intercept specific opcodes.
-   **`rtl/core/mcu_subsystem.sv`**:
    -   **MCU:** The RISC-V Core.
    -   **Instruction Helper:** A custom module listening to the execute stage.
    -   **Bus Adapter:** Native Core Mem Interface -> Pipelined Wishbone Master.

### Sprint 3.2: The "TinyGPU ISA" (Custom Instructions)
Instead of slow Memory-Mapped IO (SW/LW) to control the GPU, we implement custom instructions for single-cycle dispatch.

-   **`CUSTOM_0 ("KICK")`**:
    -   Format: `kick r1 (X), r2 (Y), r3 (Z/KernelID)`
    -   Hardware: Directly signals the `Work Distributor` to spawn threads. No bus traffic generated.
-   **`CUSTOM_1 ("WAIT")`**:
    -   Hardware: Stalls the CPU pipeline until the `Compute Cluster` reports "Idle". Efficient barriers.
-   **`CUSTOM_2 ("W_VRAM")`**:
    -   Special pointer arithmetic logic or fast-path DMA triggers.

### Sprint 3.3: Firmware Loop
-   **`firmware/main.c`**:
    -   Map `CUSTOM_0` to C inline assembly (`asm volatile`).
    -   Main Loop: Pop Command -> `asm("kick ...")` -> Loop.
    -   This reduces the "Driver Overhead" to almost zero cycles.

## Phase 4: Compute Acceleration
**Goal:** Hardware acceleration for pixels using the Fabric.

### Sprint 4.1: Compute Unit Wrapper & Cache
-   **`rtl/compute/cu_cluster.sv`**:
    -   Wraps 4x Compute Units.
    -   **L1/L2 Texture Cache:**
        -   Sits between CUs and the Wishbone Fabric.
        -   buffers small 2D tiles (e.g., 4x4 pixels) to reduce VRAM contention.
-   **Bus Interface:** The Cluster appears as a single **Wishbone Master** to the system.

### Sprint 4.2: Work Distributor (Hardware)
-   **`rtl/compute/work_distributor.sv`**:
    -   Connected directly to the MCU's **Custom Instruction Port**.
    -   Receives `{X, Y, KernelID}` from the `KICK` instruction.
    -   Round-Robin allocates tasks to the 4 CUs.

### Sprint 4.3: Integration & Texture Cache
-   **`rtl/memory_if/l2_cache.sv`**:
    -   Read-Only cache for Texture samplers.
    -   Connect to all 4 CUs.
-   **Test:** Run the "Spinning Cube" demo.

## Implementation Checklist / Status

| Module | Status | Priority |
| :--- | :--- | :--- |
| **Sim Environtment** | ⬜ Todo | Critical |
| **Host OSPI Slave** | ⬜ Todo | High |
| **VRAM Controller** | ⬜ Todo | Critical |
| **QoS Arbiter** | ⬜ Todo | High |
| **Display Engine** | ⬜ Todo | Medium |
| **MCU Wrapper** | ⬜ Todo | Medium |
| **Compute Integration**| ⬜ Todo | Low |
