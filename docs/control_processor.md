# System Control Processor (RISC‑V, 5‑Stage) — Plan & Spec

> **Goal:** a slow, reliable, and deterministic control core (PPE) that runs firmware to manage command queues, fences, and multi‑CU orchestration. Performance is secondary to correctness, observability, and fault recovery.

## 1. Scope & Design Priorities
- **Reliability first:** precise traps, robust CSR handling, no speculative side effects on MMIO.
- **Deterministic timing:** simple 5‑stage pipeline, in‑order, single‑issue.
- **Low complexity:** RV32I base with a small, curated subset of extensions.
- **Observable:** rich debug CSRs, watchdog hooks, and trace points.

**Non‑goals (for v1):** branch prediction, out‑of‑order execution, MMU, cache coherency.

## 2. ISA Profile
**Base:** RV32IMAC_Zicsr

**Included extensions:**
- **M**: integer multiply.
- **A**: atomic ops are available if needed for mailbox or shared data structures.
- **C**: compressed instructions for ROM/TCM efficiency.
- **Zicsr**: required for CSR access (mandatory for this control CPU).

**Excluded:** F/D, V, MMU/Sv.

## 3. Pipeline Overview (Classic 5‑Stage)
Stages: **IF → ID → EX → MEM → WB**

### 3.1 Stage Responsibilities
- **IF:** fetch 32‑bit instruction from IMEM (TCM). PC+4 or branch target.
- **ID:** decode, register file read, immediate generation, hazard check.
- **EX:** ALU ops, branch compare, address calc.
- **MEM:** data loads/stores to system bus (MMIO + VRAM window if needed).
- **WB:** writeback to integer register file.

### 3.2 Hazard Handling
- **RAW:** bypass from EX/MEM/WB to ID/EX.
- **Load‑use:** one‑cycle stall when EX depends on MEM load result.
- **Control:** branch resolved in EX; IF/ID flushed on taken branch.

### 3.3 Memory Ordering Rules
- **No speculation on MMIO:** stores issue in program order; loads cannot bypass older stores to MMIO.
- **`FENCE` / `MEMBAR`**: firmware uses a single barrier instruction to drain prior MMIO writes (documented in `system_contracts.md`).

## 4. Address Map (MCU View)
- **TCM/IMEM**: `0x0000_0000..0x0000_7FFF` (private)
- **Local scratch**: optional `0x0000_8000..0x0000_FFFF`
- **System CSRs (MMIO)**: `0x1000_0000..0x1000_FFFF`
- **VRAM window**: `0x8000_0000..` (optional, slow path)

> **Note:** the control CPU should minimize VRAM reads/writes; prefer DMA or CU assistance.

## 5. CSR Set (Control CPU)
### 5.1 Standard CSRs (subset)
- `mstatus`, `mtvec`, `mepc`, `mcause`, `mtval`
- `mie`, `mip`

### 5.2 Custom CSRs (system control)
| CSR | Name | Description |
| --- | --- | --- |
| `0x7C0` | `CP_UID` | Core ID / revision |
| `0x7C1` | `CP_CAPS` | Feature bits (M, C, debug, mailbox) |
| `0x7C2` | `CP_WDOG` | Watchdog timeout cycles |
| `0x7C3` | `CP_WDOG_CTRL` | bit0 enable, bit1 reset on expiry |
| `0x7C4` | `CP_MAILBOX_BASE` | Base of ingress mailbox (MMIO) |
| `0x7C5` | `CP_MAILBOX_STATUS` | FIFO empty/full flags |
| `0x7C6` | `CP_TRACE_CTRL` | Enable minimal instruction tracing |

## 6. Interconnect & MMIO Rules
- **Single master** on system bus.
- **MMIO writes** are strongly ordered and visible after a `MEMBAR`.
- **MMIO reads** are non‑speculative; side effects occur once.

## 7. Interrupts & Faults
### 7.1 Sources
- `CMDQ_NONEMPTY`
- `FENCE_REACHED`
- `ABORT_DONE`
- `DISPLAY_VBLANK` (optional)
- `MPU_FAULT` (any CU fault)

### 7.2 Behavior
- Interrupts are precise; `mepc` points to the interrupted instruction.
- Firmware should service mailbox and CU faults with bounded time.

## 8. Boot & Firmware Flow
1. **Reset:** PC=0, stack pointer set.
2. **Init:** configure watchdog, interrupt mask, and mailbox base.
3. **Main loop:**
   - If FIFO empty → `WFI`.
   - Else decode commands and program system MMIO.
   - Issue fences and track completion.
4. **Fault path:** on CU fault, read per‑CU fault CSRs and perform reset or kill job.

## 9. Verification Plan
- **Unit tests:** ALU ops, branches, load‑use stalls.
- **MMIO tests:** register write/read ordering with `MEMBAR`.
- **Interrupt tests:** pending/enable/clear semantics.
- **Watchdog tests:** expiry behavior and reset path.

## 10. RTL Module Breakdown (Planned)
- `mcu_core.sv` — top‑level pipeline
- `mcu_ifetch.sv` — instruction fetch
- `mcu_decode.sv` — decode + immediate
- `mcu_regfile.sv` — 32x32 int regs
- `mcu_alu.sv` — arithmetic/branch
- `mcu_lsu.sv` — MMIO + memory access
- `mcu_csr.sv` — CSR file + interrupts
- `mcu_wb.sv` — writeback mux

## 11. Integration Checklist
- [ ] Add control‑path CSR window (system_contracts.md)
- [ ] Hook mailbox FIFO to CP
- [ ] Connect `MPU_FAULT` interrupt
- [ ] Implement watchdog
- [ ] Provide minimal ROM image and bring‑up test
