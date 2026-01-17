# Tiny GPU Documentation

## Core Design Documents
- **[Architecture](architecture.md)**: System overview, clock domains, and data flow.
- **[Master Implementation Plan](master_plan.md)**: Four-phase development roadmap.
- **[Memory Map](memory_map.md)**: Address spaces for Host and Core.
- **[VBIOS & Boot](vbios.md)**: Boot sequence and firmware loading.

## Interfaces
- **[TinyGPU Host API (TGPU)](tgpu_api.md)**: C Library for ESP32.
- **[TinyGPU Host API Plan (Mini‑Vulkan)](tgpu_api_plan.md)**: Detailed future-facing API + transport plan.
- **[TinyGPU Wire Protocol v1 (API → Packets)](tgpu_wire_v1.md)**: Concrete v1 opcode/MMIO mapping.
- **[Command Protocol](command_protocol.md)**: Wire format for commands.

## Internals
- **[Control Firmware](firmware.md)**: RISC-V firmware logic.
- **[System Control Processor](control_processor.md)**: RISC-V 5-stage control core plan/spec.

## RTL + Verification
- **[Compute Unit Microarchitecture (RTL)](microarchitecture.md)**
- **[Compute Unit Bring-up Plan / RTL Map](compute_unit_plan.md)**
- **[Testbenches](testbenches.md)**

## Diagrams (PlantUML)
- Rendering tips: `diagrams/README.md`
- **Compute-unit microarchitecture**: `diagrams/compute_unit_microarch.puml`
- **Raster → ROP handshake**: `diagrams/raster_rop_readyvalid.puml`
- **TEX miss/refill (texture_cache ↔ LSU)**: `diagrams/tex_cache_miss_refill.puml`
- **System architecture (from architecture.md sketch)**: `diagrams/system_architecture.puml`

## Legacy / Deprecated
- *[Host API (Old)](host_api.md)*
- *[GPU Architecture Index](gpu_architecture.md)*
