# RISC-V ISA Migration Plan — TinyGPU Compute Units

> **Status:** IMPLEMENTED (Phase 1-3 complete: scalar ISA + RVV vector extension + audit/fixes)  
> **Scope:** Replace the custom TinyGPU CU instruction set with **RV32IMA_Zicsr** base + **RVV v1.0** vector extension + **Xgpu** custom graphics/vector/FP16 extension.  
> **Goal:** Leverage the RISC-V ecosystem (GCC/LLVM, debuggers, compliance tests) while preserving 100% of the GPU's graphics, vector, and texture capabilities as a custom extension.
>
> **Implementation Notes (Phase 1 — Scalar ISA migration):**
> - isa_pkg.sv, decoder.sv, branch_unit.sv, branch_predictor_bht.sv, compute_unit_top.sv updated
> - All 20 testbenches updated (opcode names, TEX funct7, VST/VATOM funct3)
> - CUSTOM2 sub-class uses **funct3** discriminator (not funct6): VLD=000, VST=001, VATOM=1xx
>
> **Implementation Notes (Phase 2 — RVV vector extension):**
> - Vector pipeline migrated from Xgpu-only encoding to standard RVV v1.0 (OP_V=0x57, OP_VL=0x07, OP_VS=0x27)
> - isa_pkg.sv: Added RVV funct3 categories (OPIVV/OPFVV/OPMVV/OPIVI/OPIVX/OPFVF/OPMVX/OPCFG), RVV funct6 constants, VEW width encodings, vector CSR addresses (vl/vtype/vlenb/vstart/vxsat/vxrm/vcsr)
> - decoder.sv: Added RVV funct6→internal ALU translation (22+ operations), operand class routing per funct3 category, vsetvl/vsetvli/vsetivli decode
> - compute_unit_top.sv: Added vtype/vl/vill CSRs, vsetvl execution, RVV SEW→type_sel mapping, LMUL=1-only validation
> - alu_vector.sv: Added VRSUB, expanded CMP to 8 sub-ops, fixed FP min/max encoding split from FP CMP, added VFDIV, fixed VRCP/VRSQRT FP type check
> - All testbenches updated with RVV encoding helpers and standard instruction formats
>
> **Implementation Notes (Phase 3 — Audit and fixes):**
> - 7 missing RVV funct6 decoder cases added (VRSUB, VMSNE, VMSLEU, VMSLE, VMSGTU, VMSGT, VFDIV)
> - FP max/CMP encoding collision resolved (FP max moved to funct6=000010/funct3[0]=1)
> - VRCP/VRSQRT type check corrected (TYPE_I32→TYPE_FP32)
> - LMUL validation added (non-zero vlmul sets vill=1, vlmax=0)
> - compute_unit_tb.sv and gfx_teapot_tb.sv interfaces updated to I-cache miss + D-cache memory model
> - This preserves I-type/S-type immediate offsets for vector load/store

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Current ISA Inventory](#2-current-isa-inventory)
3. [Target RISC-V Profile](#3-target-risc-v-profile)
4. [Opcode Mapping: Custom → RISC-V](#4-opcode-mapping-custom--risc-v)
5. [Custom Extension: Xgpu](#5-custom-extension-xgpu)
6. [Encoding Details](#6-encoding-details)
7. [Register File Strategy](#7-register-file-strategy)
8. [RTL Files Requiring Changes](#8-rtl-files-requiring-changes)
9. [Testbench & Assembly Migration](#9-testbench--assembly-migration)
10. [Documentation Updates](#10-documentation-updates)
11. [Toolchain Plan](#11-toolchain-plan)
12. [Phased Implementation Roadmap](#12-phased-implementation-roadmap)
13. [Verification & Regression Strategy](#13-verification--regression-strategy)
14. [Risk Assessment & Mitigations](#14-risk-assessment--mitigations)
15. [Appendix A: Full Opcode Table](#appendix-a-full-opcode-table)
16. [Appendix B: RISC-V Opcode Map Reference](#appendix-b-risc-v-opcode-map-reference)
17. [Appendix C: Xgpu CSR Map](#appendix-c-xgpu-csr-map)

---

## 1. Executive Summary

### Why migrate?

The current TinyGPU compute unit uses a **custom 7-bit opcode space** that is RISC-V *inspired* but **not compliant**. This means:

- **No toolchain reuse:** GCC/LLVM cannot target the CU directly; all test programs are hand-assembled in SystemVerilog testbenches using encoding helper functions.
- **No compliance testing:** Cannot use `riscv-tests` or `riscv-formal` for base integer verification.
- **Duplicated effort:** The control processor (`ip/control_proc/`) already implements standard RV32IMA_Zicsr. Having two incompatible ISAs increases maintenance burden.
- **Ecosystem lock-out:** OpenCL/POCL, debugger support, and JTAG infrastructure all assume standard RISC-V.

### What changes?

| Aspect | Current | Target |
|--------|---------|--------|
| Scalar integer opcodes | Custom (OP_INT=0x0B, OP_INT_IMM=0x17, OP_BRANCH=0x0E, etc.) | Standard RV32I (OP=0x33, OP-IMM=0x13, BRANCH=0x63, etc.) |
| FP16 scalar | Encoded within OP_INT using funct7=0x08/0x09 | Promote to RV32F-style using `custom-1` (0x2F) or dedicated Xgpu FP16 encoding |
| Vector/SIMD | OP_VEC_ALU=0x2F (coincides with RV AMO!) | Move to `custom-2` (0x5B) or `custom-3` (0x7B) |
| Graphics macro-ops | Shared with OP_ATOM_SC=0x14 | Move to `custom-0` (0x0B) |
| Texture sampling | OP_TEX=0x13 (conflicts with RV OP-IMM!) | Move to Xgpu `custom-0` (0x0B) |
| Memory operations | OP_LOAD=0x0C, OP_STORE=0x0D (non-standard) | Standard RV32I LOAD=0x03, STORE=0x23 |
| Atomics | OP_ATOM_SC=0x14 | Standard RV32A AMO=0x2F |
| System/CSR | OP_SYSTEM=0x0F (non-standard) | Standard RV SYSTEM=0x73 |
| JAL | OP_JAL=0x6F (correct!) | Keep as-is (already standard) |
| LUI | OP_LUI=0x37 (correct!) | Keep as-is (already standard) |

### Key insight: Minimal disruption

Two opcodes are already standard RISC-V: **OP_JAL** (0x6F) and **OP_LUI** (0x37). The scalar ALU operations already use the same funct3/funct7 encoding as RV32I (ADD/SUB/AND/OR/XOR/SLL/SRL/SRA/SLT/SLTU plus M-extension MUL). The migration is primarily an **opcode remapping** for the base integer subset, plus a clean separation of GPU-specific instructions into the RISC-V custom opcode space.

---

## 2. Current ISA Inventory

### 2.1 Current Opcode Map (from `isa_pkg.sv`)

| Mnemonic | 7-bit Opcode | Hex | Standard RV32? | Conflict? |
|----------|-------------|-----|----------------|-----------|
| `OP_INT` | `0001011` | 0x0B | **No** — RV custom-0 space | Usable for custom, but currently used for base integer R-type |
| `OP_INT_IMM` | `0010111` | 0x17 | **No** — this is RV `AUIPC`! | **CONFLICT** — blocks AUIPC |
| `OP_LUI` | `0110111` | 0x37 | **Yes** | ✅ Already correct |
| `OP_LOAD` | `0001100` | 0x0C | **No** — non-standard | RV LOAD is 0x03 |
| `OP_STORE` | `0001101` | 0x0D | **No** — non-standard | RV STORE is 0x23 |
| `OP_BRANCH` | `0001110` | 0x0E | **No** — non-standard | RV BRANCH is 0x63 |
| `OP_JAL` | `1101111` | 0x6F | **Yes** | ✅ Already correct |
| `OP_SYSTEM` | `0001111` | 0x0F | **No** — this is RV `MISC-MEM`/FENCE | RV SYSTEM is 0x73 |
| `OP_VEC_ALU` | `0101111` | 0x2F | **No** — this is RV `AMO`! | **CONFLICT** — blocks RV32A atomics |
| `OP_VLD` | `0010001` | 0x11 | **No** — non-standard | Not in RV base map |
| `OP_VST` | `0010010` | 0x12 | **No** — non-standard | Not in RV base map |
| `OP_TEX` | `0010011` | 0x13 | **No** — this is RV `OP-IMM`! | **CONFLICT** — blocks ADDI/XORI/etc. |
| `OP_ATOM_SC` | `0010100` | 0x14 | **No** — non-standard | Not in RV base map |
| `OP_ATOM_V` | `0010101` | 0x15 | **No** — non-standard | Not in RV base map |

**Critical Conflicts (3):**
1. `OP_INT_IMM` (0x17) collides with RV32 `AUIPC`
2. `OP_VEC_ALU` (0x2F) collides with RV32A `AMO`
3. `OP_TEX` (0x13) collides with RV32I `OP-IMM`

### 2.2 Current Instruction Count

| Category | Instructions | Encoding space used |
|----------|-------------|-------------------|
| Scalar integer (R-type) | ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, SLT, SLTU, MUL, MIN, MAX, CMP, CLZ, CTZ | funct7 × funct3 under OP_INT |
| Scalar integer (I-type) | ADDI, ANDI, ORI, XORI, SLLI, SRLI, SRAI | funct3 under OP_INT_IMM |
| Upper immediate | LUI | U-type |
| Scalar FP16 | FADD, FSUB, FMUL, FMA, FMIN, FMAX, FCVT.i2f, FCVT.f2i, FRECIP, FRSQRT | funct7=0x08/0x09 under OP_INT |
| Load/Store | LB, LH, LW, SB, SH, SW | funct3 under OP_LOAD/OP_STORE |
| Branch/Jump | BEQ, BNE, BLT, BGE, BLTU, BGEU, JAL, JALR | funct3 under OP_BRANCH + OP_JAL |
| System | MEMBAR, CSRRW, CSRRS, SETMISC, WFI | funct3 under OP_SYSTEM |
| Vector ALU (16 ops) | VADD, VSUB, VMIN, VCMP/VMAX, VDOT, VCROSS, VSEL, VSWIZ, VPACK, VXOR, VAND, VOR, VMUL, VRCP, VRSQRT, VUNPACK | funct6 under OP_VEC_ALU |
| Vector memory | VLD.V, VLD.S, VLDX, VLD.C, VST variants | funct3 under OP_VLD/OP_VST |
| Texture | TEX2D | funct3 under OP_TEX |
| Graphics macro | RSTATE, RSETUP, RDRAW, RRECT, GSTATE, GPARAM, GDRAW | funct3 under OP_ATOM_SC |
| Scalar atomics | ATOM.ADD/MIN/MAX/XCHG/CAS/AND/OR/XOR | funct3 under OP_ATOM_SC |
| Vector atomics | VATOM.ADD/MIN/MAX/XCHG/AND/OR/XOR | funct3 under OP_ATOM_V |

**Total: ~70+ unique instruction variants.**

---

## 3. Target RISC-V Profile

### 3.1 Base ISA: RV32I (+ selected standard extensions)

| Extension | Status | Rationale |
|-----------|--------|-----------|
| **RV32I** | Required | Base integer (ADD/SUB/AND/OR/XOR/SLL/SRL/SRA/SLT/SLTU/LUI/AUIPC/JAL/JALR/BEQ/BNE/BLT/BGE/BLTU/BGEU/LB/LH/LW/LBU/LHU/SB/SH/SW/FENCE/ECALL/EBREAK) |
| **M** | Required | Integer multiply/divide (MUL already implemented; adds MULH/MULHU/MULHSU/DIV/DIVU/REM/REMU) |
| **A** | Required | Atomic operations (AMO instructions — replaces custom OP_ATOM_SC) |
| **Zicsr** | Required | CSR access (CSRRW/CSRRS/CSRRC/CSRRWI/CSRRSI/CSRRCI — replaces custom OP_SYSTEM CSR ops) |
| **Zifencei** | Recommended | FENCE.I for I-cache coherence (useful for self-modifying code / kernel upload) |
| **F** | Optional | Single-precision FP (if we upgrade from FP16 to FP32 scalar in future) |
| **C** | Optional | Compressed 16-bit instructions (improves code density, helpful for small IMEM) |

### 3.2 Custom Extension: Xgpu (GPU Graphics + Vector + Texture)

All GPU-specific instructions use the RISC-V **custom opcode space**:

| RISC-V Custom Slot | Opcode bits [6:0] | Xgpu Usage |
|--------------------|--------------------|------------|
| `custom-0` | `0001011` (0x0B) | **GPU Graphics + Texture + FP16 scalar** |
| `custom-1` | `0101011` (0x2B) | **GPU Vector ALU** |
| `custom-2` | `1011011` (0x5B) | **GPU Vector Load/Store + Vector Atomics** |
| `custom-3` | `1111011` (0x7B) | *Reserved for future (matrix/tensor)* |

This layout:
- Keeps all standard RV32IMAZicsr opcodes untouched
- Uses only the 4 designated custom slots
- Leaves `custom-3` free for future matrix core extensions (see `docs/matrix_core_plan.md`)

---

## 4. Opcode Mapping: Custom → RISC-V

### 4.1 Standard RV32I Replacements (direct mapping)

| Current Custom | Current Hex | → RV32 Standard | RV32 Hex | Change Type |
|---------------|-------------|-----------------|----------|-------------|
| `OP_INT` (R-type ALU) | 0x0B | `OP` (R-type ALU) | **0x33** | Opcode value change only; funct3/funct7 already match RV32I |
| `OP_INT_IMM` (I-type ALU) | 0x17 | `OP-IMM` | **0x13** | Opcode value change only; funct3 already matches RV32I |
| `OP_LUI` | 0x37 | `LUI` | **0x37** | **No change** |
| `OP_LOAD` | 0x0C | `LOAD` | **0x03** | Opcode change; add LBU/LHU support (funct3=100/101) |
| `OP_STORE` | 0x0D | `STORE` | **0x23** | Opcode change; immediate encoding changes to S-type split |
| `OP_BRANCH` | 0x0E | `BRANCH` | **0x63** | Opcode change; B-type immediate encoding |
| `OP_JAL` | 0x6F | `JAL` | **0x6F** | **No change** |
| (missing) | — | `JALR` | **0x67** | **New** — currently JALR is encoded in OP_BRANCH funct3=010; must move to standard opcode |
| (missing) | — | `AUIPC` | **0x17** | **New** — currently blocked by OP_INT_IMM; enables PC-relative addressing |
| `OP_SYSTEM` (CSR ops) | 0x0F | `SYSTEM` | **0x73** | Opcode change; CSR encoding changes to standard I-type with CSR address in imm[11:0] |
| (missing) | — | `MISC-MEM` (FENCE) | **0x0F** | **New** — standard fence; replaces custom MEMBAR encoding |
| Atomics (funct7-distinguised in OP_ATOM_SC) | 0x14 | `AMO` | **0x2F** | Opcode change; use standard RV32A encoding (funct5+aq+rl+rs2+rs1+funct3+rd) |

### 4.2 MUL extension mapping

| Current | → RV32M Standard |
|---------|-----------------|
| MUL (funct7=0x01, funct3=000 under OP_INT) | MUL (funct7=0x01, funct3=000 under OP=0x33) — **identical funct encoding** |

RV32M also adds: MULH(001), MULHSU(010), MULHU(011), DIV(100), DIVU(101), REM(110), REMU(111). These are currently unimplemented but the encoding is compatible; they can be added incrementally.

### 4.3 Custom instructions → Xgpu extension

| Current | Current Opcode | → Xgpu | Xgpu Opcode | Notes |
|---------|---------------|--------|-------------|-------|
| FP16 scalar (FADD/FSUB/FMUL/FMA/FMIN/FMAX/FCVT/FRECIP/FRSQRT) | OP_INT + funct7=0x08/0x09 | **`custom-0`** | **0x0B** | Separate from integer ALU; uses same funct3/funct7 |
| Graphics macros (RSTATE/RSETUP/RDRAW/RRECT/GSTATE/GPARAM/GDRAW) | OP_ATOM_SC + funct7=0x00 | **`custom-0`** | **0x0B** | Distinguished by funct7=0x40 (bit pattern to avoid overlap with FP16) |
| Texture (TEX2D) | OP_TEX (0x13) | **`custom-0`** | **0x0B** | Distinguished by funct7=0x41 |
| Vector ALU (VADD..VUNPACK, 16 ops) | OP_VEC_ALU (0x2F) | **`custom-1`** | **0x2B** | Same funct6/funct3 encoding preserved |
| Vector Load | OP_VLD (0x11) | **`custom-2`** | **0x5B** | funct3 distinguishes VLD.V/VLD.S/VLDX/VLD.C |
| Vector Store | OP_VST (0x12) | **`custom-2`** | **0x5B** | Distinguished from VLD by funct7 bit |
| Vector Atomics | OP_ATOM_V (0x15) | **`custom-2`** | **0x5B** | Distinguished by funct7 pattern |

---

## 5. Custom Extension: Xgpu

### 5.1 Design Principles

1. **All Xgpu instructions are 32-bit** (no compressed variants initially)
2. **Register file routing** is determined by `rd_class`/`rs1_class`/`rs2_class` derived from the opcode+funct combination (same as current design)
3. **Predication** (vm bit) is preserved for vector instructions at inst[25]
4. **Backward-compatible funct3/funct6/funct7** — the operation-selection bits remain identical to current RTL where possible; only the 7-bit base opcode changes

### 5.2 `custom-0` (0x0B) — Scalar FP16 + Graphics + Texture

**Encoding: R-type** `[31:25]=funct7 | [24:20]=rs2 | [19:15]=rs1 | [14:12]=funct3 | [11:7]=rd | [6:0]=0001011`

| Sub-class | funct7 | funct3 | Operations |
|-----------|--------|--------|------------|
| **FP16 ALU** | `0001000` | 000-111 | FADD, FSUB, FMUL, FMA, FMIN, FMAX, FCVT.i2f, FCVT.f2i |
| **FP16 SFU** | `0001001` | 000-001 | FRECIP, FRSQRT |
| **Graphics: Raster** | `1000000` | 000-011 | RSTATE, RSETUP, RDRAW, RRECT |
| **Graphics: Geometry** | `1000000` | 100-110 | GSTATE, GPARAM, GDRAW |
| **Texture** | `1000010` | 000 | TEX2D.nearest |
| **Texture** | `1000010` | 001 | TEX2D.bilinear (reserved) |
| **Config** | `1000100` | 000-111 | SETMISC, GPU-specific config |

**Encoding: I-type** `[31:20]=imm12 | [19:15]=rs1 | [14:12]=funct3 | [11:7]=rd | [6:0]=0001011`
- Used when graphics macro-ops need an immediate offset (e.g., descriptor pointer + offset).

### 5.3 `custom-1` (0x2B) — Vector ALU

**Encoding (R-type, vector):** `[31:26]=funct6 | [25]=vm | [24:20]=rs2 | [19:15]=rs1 | [14:12]=funct3 | [11:7]=rd | [6:0]=0101011`

| funct6 | Mnemonic | Description |
|--------|----------|-------------|
| `000000` | `VADD` | Vector add (I32/I16/I8/FP32/FP16/FP8) |
| `000001` | `VSUB` | Vector subtract |
| `000010` | `VMIN` | Vector min (FP types) |
| `000011` | `VCMP`/`VMAX` | Compare→mask (int) / Max (FP) |
| `000100` | `VDOT` | Dot product (reduce to lane 0) |
| `000101` | `VCROSS` | Cross product (lanes 0-2) |
| `000110` | `VSEL` | Predicated blend |
| `000111` | `VSWIZ` | Swizzle/permute |
| `001000` | `VPACK` | Pack lanes → ARGB8888 scalar |
| `001001` | `VXOR` | Bitwise XOR |
| `001010` | `VAND` | Bitwise AND |
| `001011` | `VOR` | Bitwise OR |
| `001100` | `VMUL` | Element-wise multiply |
| `001101` | `VRCP` | Reciprocal (FP, unary) |
| `001110` | `VRSQRT` | Reciprocal sqrt (FP, unary) |
| `001111` | `VUNPACK` | Unpack ARGB8888 → lanes |

**Element type encoding (funct3):** unchanged from current — `000`=I32, `001`=I16, `010`=I8, `011`=FP32, `100`=FP16, `101`=FP8.

### 5.4 `custom-2` (0x5B) — Vector Memory + Vector Atomics

> **IMPLEMENTED:** Uses funct3 as the primary sub-class discriminator (not funct6 as originally planned).
> This preserves I-type/S-type immediate offsets for VLD/VST, reducing instruction count.

**Sub-class discrimination via funct3:**

| funct3 range | Class | Format | Immediate |
|-------------|-------|--------|-----------|
| `000`       | VLD (vector load) | I-type | imm_i (rs1 + sign-ext 12-bit offset) |
| `001`       | VST (vector store) | S-type | imm_s (rs1 + sign-ext 12-bit offset) |
| `1xx`       | VATOM (vector atomic) | S-type | imm_s (rs1 + sign-ext 12-bit offset) |

**Vector Load (I-type):** `[31:20]=imm[11:0] | [19:15]=rs1 | [14:12]=000 | [11:7]=vd | [6:0]=1011011`

**Vector Store (S-type):** `[31:25]=imm[11:5] | [24:20]=vs | [19:15]=rs1 | [14:12]=001 | [11:7]=imm[4:0] | [6:0]=1011011`

**Vector Atomics (S-type):** `[31:25]=imm[11:5] | [24:20]=vs | [19:15]=rs1 | [14:12]=1xx | [11:7]=imm[4:0] | [6:0]=1011011`

| funct3 | Operation |
|--------|-----------|
| `100` | VATOM.ADD |
| `101` | VATOM.MIN |
| `110` | VATOM.MAX |
| `111` | VATOM.XCHG |
| `111` | VATOM.XOR |

### 5.5 `custom-3` (0x7B) — Reserved for Matrix/Tensor Extension

Reserved for future matrix core instructions (MCFG, MLOAD, MSTORE, MMUL, MWAIT) per `docs/matrix_core_plan.md`. Not allocated in this migration.

---

## 6. Encoding Details

### 6.1 Immediate Formats (now fully standard RV32)

| Type | Fields | Usage |
|------|--------|-------|
| **I-type** | `imm[11:0] \| rs1 \| funct3 \| rd \| opcode` | ADDI, loads, JALR, CSR, shifts |
| **S-type** | `imm[11:5] \| rs2 \| rs1 \| funct3 \| imm[4:0] \| opcode` | Stores |
| **B-type** | `imm[12\|10:5] \| rs2 \| rs1 \| funct3 \| imm[4:1\|11] \| opcode` | Branches |
| **U-type** | `imm[31:12] \| rd \| opcode` | LUI, AUIPC |
| **J-type** | `imm[20\|10:1\|11\|19:12] \| rd \| opcode` | JAL |
| **R-type** | `funct7 \| rs2 \| rs1 \| funct3 \| rd \| opcode` | ALU register-register |

**Key change for stores:** Current OP_STORE uses I-type encoding (imm in bits [31:20]). Standard RV32 STORE uses S-type (imm split: [31:25] and [11:7]). The LSU and store data path must be updated.

**Key change for branches:** Current OP_BRANCH B-type immediate already matches RV32 B-type encoding. The testbench `b_type()` helper already implements this correctly. Only the opcode value changes.

**Key change for JALR:** Currently encoded as `OP_BRANCH` with `funct3=010`. Must move to dedicated `JALR` opcode `0x67` with I-type encoding.

### 6.2 Addressing Modes

Current custom load/store addressing uses funct7 to select mode:
- `0000000`: base + imm12
- `0000001`: base + rs2
- `0000010`: pc + imm12
- `0000011`: base only

**Standard RV32I** only supports base + imm12 for loads and base + imm12 (S-type) for stores. The `base + rs2` and `pc + imm12` modes are non-standard and must be handled:

| Current Mode | Migration Strategy |
|-------------|-------------------|
| `base + imm12` | Standard RV32 (no change needed) |
| `base + rs2` | Emit `ADD tmp, rs1, rs2` then `LW rd, 0(tmp)` in software (or keep as Xgpu custom if HW wants it) |
| `pc + imm12` | Use `AUIPC` + `LW` pair (standard RV32 idiom) |
| `base only` | Encode as `LW rd, 0(rs1)` with imm=0 |

### 6.3 CSR Encoding Changes

Current: custom OP_SYSTEM(0x0F) with funct3 selecting operation.

Standard RV32 SYSTEM(0x73) CSR encoding:
```
imm[11:0] = CSR address (12-bit)
funct3 = 001 (CSRRW), 010 (CSRRS), 011 (CSRRC), 101 (CSRRWI), 110 (CSRRSI), 111 (CSRRCI)
```

**MEMBAR** → Maps to standard `FENCE` instruction at opcode `MISC-MEM` (0x0F). The `fm`/`pred`/`succ` fields can encode GPU-specific ordering semantics.

**WFI** → Standard `WFI` encoding within `SYSTEM` opcode (imm=0x105, funct3=000, rs1=0, rd=0).

**SETMISC** → Moves to Xgpu `custom-0` (GPU-specific config space).

---

## 7. Register File Strategy

### 7.1 Current Register Files

| File | Size | Usage |
|------|------|-------|
| Scalar `s0-s31` | 32×32-bit | General integer (`s0`=hardwired zero) |
| FP `f0-f31` | 32×16-bit | FP16 values |
| Vector `v0-v31` | 32×128-bit | 4xFP32/8xFP16/packed int |

### 7.2 Target Register Files

| File | Size | RV32 Mapping | Notes |
|------|------|-------------|-------|
| Integer `x0-x31` | 32×32-bit | Standard RV32 `x` registers | **Rename only**: `s` → `x`; `x0`=zero (already implemented) |
| FP `f0-f31` | 32×16-bit (or 32-bit if F ext) | RV32F `f` registers | If enabling full F ext, widen to 32-bit; otherwise keep FP16 as Xgpu custom |
| Vector `v0-v31` | 32×128-bit | Xgpu custom | Not RVV (too complex); use Xgpu-defined vector file |

**Register naming convention change in assembly/tests:**
- `s0-s31` → `x0-x31` (or ABI names: `zero`, `ra`, `sp`, `gp`, `tp`, `t0-t6`, `s0-s11`, `a0-a7`)
- `f0-f31` → `f0-f31` (same naming, different encoding context)
- `v0-v31` → `v0-v31` (Xgpu custom, no change)

### 7.3 Why Not Use Standard RVV (RISC-V Vector Extension)?

RVV is extremely complex (LMUL, VL, vtype CSR, mask registers, ~300 instructions). The TinyGPU vector model is much simpler:
- Fixed 128-bit width (4 lanes at FP32/INT32)
- Fixed element types per instruction (no dynamic vtype)
- Simple 4-bit predicate mask (vmask CSR, not RVV mask register)
- 16 custom vector operations (VDOT, VCROSS, VPACK, VUNPACK, etc.) are GPU-specific

**Decision:** Keep vector as a custom Xgpu extension. This is a common approach in GPU architectures (NVIDIA PTX, AMD RDNA are not RVV-compliant either).

---

## 8. RTL Files Requiring Changes

### 8.1 Critical (ISA-defining)

| File | Change Scope | Effort |
|------|-------------|--------|
| `ip/compute unit/isa_pkg.sv` | **Rewrite** opcode_t enum values to RV32 standard + Xgpu custom slots | High |
| `ip/compute unit/decoder.sv` | **Major rewrite** — change all opcode matches, add AUIPC/JALR decode, fix S-type immediate extraction for stores, update CSR encoding | High |

### 8.2 Execution Units (moderate changes)

| File | Change Scope | Effort |
|------|-------------|--------|
| `ip/compute unit/alu_scalar.sv` | Update opcode checks (`OP_INT` → `OP`, `OP_BRANCH` → `BRANCH`); add AUIPC result path | Medium |
| `ip/compute unit/branch_unit.sv` | Update opcode references; separate JALR from BRANCH; handle JALR as I-type | Medium |
| `ip/compute unit/compute_unit_top.sv` | Update opcode references in issue logic, dual-issue rules, pipeline control | Medium |
| `ip/compute unit/lsu.sv` | Update opcode references; add LBU/LHU decoding; handle S-type store immediate | Medium |
| `ip/compute unit/csr_file.sv` | Update CSR-instruction decoding to standard RV Zicsr format; add standard mstatus/mie/mip if desired | Medium |
| `ip/compute unit/fp_alu.sv` | Minimal — FP operations move from OP_INT to custom-0, but funct3/funct7 stay same | Low |
| `ip/compute unit/scoreboard.sv` | Update instruction-class detection to match new opcodes | Low |
| `ip/compute unit/fetch_unit.sv` | Minimal — fetch is opcode-agnostic; may need update for C extension (16-bit instructions) | Low |
| `ip/compute unit/icache.sv` | No change expected | None |

### 8.3 Graphics/Texture Pipeline (minor changes)

| File | Change Scope | Effort |
|------|-------------|--------|
| `ip/compute unit/graphics_pipeline.sv` | Update opcode references for graphics macro-ops | Low |
| `ip/compute unit/gfx_cmd_streamer.sv` | Internal command opcodes are independent of ISA encoding — no change | None |
| `ip/compute unit/gpipe_dispatcher.sv` | Update gfx detection logic for new custom-0 encoding | Low |
| `ip/compute unit/raster_unit.sv` | No ISA dependency — no change | None |
| `ip/compute unit/rop_unit.sv` | No ISA dependency — no change | None |
| `ip/compute unit/texture_cache.sv` | No ISA dependency — no change | None |
| `ip/compute unit/shade_unit.sv` | No ISA dependency — no change | None |

### 8.4 Register Files (naming only)

| File | Change Scope | Effort |
|------|-------------|--------|
| `ip/compute unit/regfile_scalar.sv` | No functional change; optionally rename ports | None |
| `ip/compute unit/regfile_fp.sv` | No functional change | None |
| `ip/compute unit/regfile_vector.sv` | No functional change | None |

### 8.5 Support Units

| File | Change Scope | Effort |
|------|-------------|--------|
| `ip/compute unit/alu_vector.sv` | Update opcode constant reference (OP_VEC_ALU → custom-1) | Low |
| `ip/compute unit/scalar_wb_arb_pending2.sv` | No ISA dependency | None |
| `ip/compute unit/write_merge_buf.sv` | No ISA dependency | None |
| `ip/compute unit/local_mem_banked.sv` | No ISA dependency | None |
| `ip/compute unit/branch_predictor_bht.sv` | No ISA dependency | None |
| `ip/compute unit/lsu_core.sv` | May need minor opcode reference updates | Low |
| `ip/compute unit/lsu_gfx.sv` | No ISA dependency | None |

---

## 9. Testbench & Assembly Migration

### 9.1 Testbench Files (all need opcode constant updates)

Every testbench file that uses the encoding helper functions (`r_type()`, `i_type()`, `b_type()`, `s_type()`, `u_type()`) and references opcode constants needs updating:

**Compute Unit Testbenches:**
| File | Scope |
|------|-------|
| `testbench/compute_unit_full_tb.sv` | Full regression — all opcodes used |
| `testbench/compute_unit_tb.sv` | Core compute — ALU, vector, memory |
| `testbench/compute_unit_top_tb.sv` | Integration test |
| `testbench/branch_tb.sv` | Branch/jump validation |
| `testbench/coremark_tb.sv` | Benchmark (scalar integer heavy) |
| `testbench/dhrystone_tb.sv` | Benchmark |

**Graphics Testbenches:**
| File | Scope |
|------|-------|
| `testbench/gfx_console_tb.sv` | Graphics console |
| `testbench/gfx_console_tb2.sv` | Graphics console variant |
| `testbench/gfx_console_tb copy.sv` | Backup |
| `testbench/gfx_console_tb copy 3.sv` | Backup |
| `testbench/gfx_console_tb copy 4.sv` | Backup |
| `testbench/gfx_sw_rt_tb.sv` | Software ray tracing |
| `testbench/gfx_sw_rt_tb copy.sv` | Backup |
| `testbench/gfx_sw_rt_tb copy 2.sv` | Backup |
| `testbench/gfx_triangle_tb.sv` | Triangle rasterization |
| `testbench/gfx_textured_triangle_tb.sv` | Textured rendering |
| `testbench/gfx_teapot_tb.sv` | Teapot model |
| `testbench/gfx_gdraw_tb.sv` | Geometry draw |

### 9.2 Encoding Helper Function Updates

The testbench helper functions encode instructions and reference opcode constants. Required changes:

```systemverilog
// OLD:
rom[pc >> 2] = r_type(7'b0000000, rs2, rs1, 3'b000, rd, OP_INT);      // ADD
rom[pc >> 2] = i_type(imm, rs1, 3'b000, rd, OP_INT_IMM);              // ADDI
rom[pc >> 2] = i_type(0, rs1, funct3, rd, OP_LOAD);                    // LW
rom[pc >> 2] = s_type(imm, rs1, rs2, funct3, OP_STORE);               // SW
rom[pc >> 2] = b_type(offset, rs1, rs2, 3'b000, OP_BRANCH);           // BEQ
rom[pc >> 2] = i_type(0, rs1, 3'b000, rd, OP_ATOM_SC);                // RSTATE

// NEW:
rom[pc >> 2] = r_type(7'b0000000, rs2, rs1, 3'b000, rd, OP_REG);      // ADD  (0x33)
rom[pc >> 2] = i_type(imm, rs1, 3'b000, rd, OP_IMM);                  // ADDI (0x13)
rom[pc >> 2] = i_type(0, rs1, funct3, rd, OP_LOAD);                    // LW   (0x03)
rom[pc >> 2] = s_type(imm, rs1, rs2, funct3, OP_STORE);               // SW   (0x23)
rom[pc >> 2] = b_type(offset, rs1, rs2, 3'b000, OP_BRANCH);           // BEQ  (0x63)
rom[pc >> 2] = r_type(7'b1000000, 5'd0, rs1, 3'b000, rd, OP_CUSTOM0); // RSTATE (0x0B)
```

### 9.3 Assembly Test Files

| File | Change Needed |
|------|--------------|
| `test/full_testbench.S` | Update opcodes in assembly directives; use standard RV32 mnemonics |
| `test/graphics_test.S` | Update opcodes; graphics instructions use `.insn` pseudo-op for custom encoding |

### 9.4 Migration Strategy for Testbenches

1. **Create a new `rv_isa_pkg.sv`** (or rename `isa_pkg.sv`) with both old and new constants during transition
2. **Add `RV_` prefix constants** alongside old ones for parallel testing
3. **Update testbenches in batches**, starting with the simplest (branch_tb) → most complex (compute_unit_full_tb)
4. **Verify each testbench passes** before moving to the next

---

## 10. Documentation Updates

| Document | Change Scope |
|----------|-------------|
| `docs/isa.md` | **Major rewrite** — update all opcode listings, encoding tables, and examples to RV32 base + Xgpu |
| `docs/isa_instructions.md` | **Major rewrite** — update instruction reference with standard RV32 encodings + Xgpu extension reference |
| `doc/isa.md` (old location) | Update or mark as deprecated |
| `doc/isa_instructions.md` (old location) | Update or mark as deprecated |
| `docs/microarchitecture.md` | Update opcode references in pipeline descriptions |
| `docs/compute_unit_plan.md` | Update ISA assumptions section and module hierarchy references |
| `docs/architecture.md` | Update ISA description to indicate RV32 compliance |
| `docs/system_plan.md` | Update Phase C to reflect RV32 ISA adoption (completed) |
| `docs/design_roadmap.md` | Update Phase C RISC-V section to reflect migration plan |
| `docs/master_plan.md` | Update Sprint 3.2 "TinyGPU ISA" section |
| `docs/matrix_core_plan.md` | Confirm custom-3 reservation for matrix extension |
| `docs/system_contracts.md` | Update CSR and MEMBAR encoding references |
| `docs/memory_map.md` | Minor — update any ISA-specific notes |
| `docs/testbenches.md` | Update encoding examples |
| `docs/graphics_pipeline_overhaul.md` | Update opcode references in ISA compatibility section |
| `docs/control_processor.md` | Note ISA convergence between CP and CU |
| `docs/firmware.md` | Note shared toolchain possibility |
| `docs/tgpu_api.md` | Minor — host API is above ISA level |

---

## 11. Toolchain Plan

### 11.1 Assembler

**Phase 1 (immediate):** Continue using SystemVerilog testbench encoding helpers with updated constants. This requires zero external toolchain.

**Phase 2 (short-term):** Use GNU `as` (RISC-V target) for standard RV32 instructions + `.insn` pseudo-op for Xgpu custom instructions:

```asm
# Standard RV32 instructions — assembled natively
    addi x2, x0, 100       # x2 = 100
    lw   x3, 0(x2)         # x3 = mem[x2]
    add  x4, x2, x3        # x4 = x2 + x3
    beq  x4, x5, label     # branch if x4 == x5

# Xgpu custom instructions — using .insn directive
    .insn r  0x2B, 0, 0x00, v3, v1, v2   # VADD v3, v1, v2 (custom-1, funct3=0, funct7=0)
    .insn r  0x0B, 0, 0x08, f1, f2, f3   # FADD f1, f2, f3 (custom-0, funct3=0, funct7=0x08)
    .insn r  0x0B, 0, 0x40, x0, x2, x0   # RSTATE x2 (custom-0, funct3=0, funct7=0x40)
```

**Phase 3 (long-term):** Define LLVM/GCC intrinsics for Xgpu instructions, enabling C-level access:

```c
// Intrinsic examples (future)
__builtin_xgpu_vadd_f32(v3, v1, v2);
__builtin_xgpu_tex2d(vcolor, vcoord, sampler_id);
__builtin_xgpu_rstate(descriptor_ptr);
```

### 11.2 Compiler

- **GCC/LLVM cross-compiler**: `riscv32-unknown-elf-gcc` with `-march=rv32ima -mabi=ilp32`
- **Xgpu intrinsics**: inline assembly macros initially, then LLVM backend extension
- **Linker scripts**: standard RV32 linker scripts with TinyGPU memory map sections

### 11.3 Debugger

- **GDB**: `riscv32-unknown-elf-gdb` — works for standard RV32 instructions
- **JTAG**: standard RISC-V debug spec (dm spec 0.13.2) — can be used for CU debug if debug module is added
- **Waveform correlation**: instruction trace in simulation will now show standard mnemonics

### 11.4 Compliance Testing

- **riscv-tests**: Run the `rv32ui`, `rv32um`, `rv32ua` test suites to validate base integer/multiply/atomic correctness
- **riscv-formal**: Formal verification of the decoder against the RV32 spec
- **Custom Xgpu tests**: keep existing testbench suite for GPU-specific instruction validation

---

## 12. Phased Implementation Roadmap

### Phase 1: ISA Package + Decoder (Foundation)
**Duration estimate:** Focused effort
**Risk:** Medium (core change, affects everything downstream)

**Tasks:**
1. Create new `isa_pkg.sv` with RV32 standard opcodes + Xgpu custom opcodes
2. Add `AUIPC` (0x17) and `JALR` (0x67) to opcode enum
3. Rewrite `decoder.sv` opcode case statements
4. Fix store immediate extraction (I-type → S-type for standard stores)
5. Fix CSR instruction decoding (Zicsr format)
6. Add unsigned load decoding (LBU=funct3[2], LHU)
7. Move JALR from BRANCH funct3=010 to dedicated JALR opcode
8. Separate FENCE from OP_SYSTEM (MISC-MEM opcode)

**Verification gate:** Existing `branch_tb.sv` passes with updated encodings.

### Phase 2: Execution Unit Updates
**Duration estimate:** Moderate
**Risk:** Low-Medium (mostly opcode constant swaps)

**Tasks:**
1. Update `alu_scalar.sv` opcode references
2. Update `branch_unit.sv` for JALR separation
3. Implement AUIPC computation in ALU (PC + imm20)
4. Update `lsu.sv` for new LOAD/STORE opcodes and LBU/LHU
5. Update `csr_file.sv` for Zicsr encoding
6. Update `compute_unit_top.sv` issue/pipeline control
7. Update `scoreboard.sv` instruction class detection

**Verification gate:** All scalar integer tests pass (`addi`, `add`, `sub`, `branches`, `load/store`, `csr`).

### Phase 3: Custom Extension Opcodes
**Duration estimate:** Moderate
**Risk:** Low (moving existing functionality to new opcode slot)

**Tasks:**
1. Move FP16 ops from OP_INT to custom-0 (0x0B)
2. Move vector ALU from OP_VEC_ALU to custom-1 (0x2B)
3. Move vector load/store from OP_VLD/OP_VST to custom-2 (0x5B)
4. Move graphics macros from OP_ATOM_SC to custom-0 (0x0B)
5. Move texture from OP_TEX to custom-0 (0x0B)
6. Move scalar atomics to standard AMO (0x2F)
7. Move vector atomics to custom-2 (0x5B)

**Verification gate:** Vector + FP16 + texture + graphics testbenches pass.

### Phase 4: Testbench Migration
**Duration estimate:** Significant (many files)
**Risk:** Low (mechanical changes)

**Tasks:**
1. Update encoding helper functions in all testbenches
2. Update all test ROM generation to use new opcode constants
3. Add AUIPC test cases
4. Add LBU/LHU test cases
5. Run full regression suite
6. Update all graphics testbenches
7. Update coremark/dhrystone testbenches

**Verification gate:** Full regression (all testbenches) passes with zero failures.

### Phase 5: Documentation + Toolchain
**Duration estimate:** Moderate
**Risk:** Low

**Tasks:**
1. Rewrite `docs/isa.md` for RV32I + Xgpu
2. Rewrite `docs/isa_instructions.md` for RV32I + Xgpu
3. Update all other documentation (see Section 10)
4. Create GNU assembler example using `.insn` for Xgpu
5. Port `test/full_testbench.S` to standard RISC-V assembly
6. Port `test/graphics_test.S` to standard RISC-V assembly with `.insn`
7. Add riscv-tests compliance suite to CI
8. Update `scripts/` build scripts for RISC-V toolchain

**Verification gate:** Assembly tests assemble with `riscv32-unknown-elf-as` and execute correctly.

### Phase 6: Optimization + Cleanup (Optional)
**Tasks:**
1. Add C extension (compressed instructions) support if IMEM savings are needed
2. Add full F extension (FP32 scalar) if upgrading from FP16
3. Unify `isa_pkg.sv` and `mcu_isa_pkg.sv` into a shared package
4. Profile ISA encoding density and optimize Xgpu funct allocation
5. Explore RVV (RISC-V Vector Extension) partial compatibility layer

---

## 13. Verification & Regression Strategy

### 13.1 Test Categories

| Category | Source | Coverage |
|----------|--------|----------|
| **RV32I compliance** | riscv-tests `rv32ui-*` | All base integer instructions |
| **RV32M compliance** | riscv-tests `rv32um-*` | MUL/MULH/DIV/REM |
| **RV32A compliance** | riscv-tests `rv32ua-*` | AMO instructions |
| **Xgpu vector** | Existing testbenches (migrated) | VADD through VUNPACK |
| **Xgpu FP16** | Existing testbenches (migrated) | FADD/FSUB/FMUL/FMA/FCVT/FRECIP/FRSQRT |
| **Xgpu graphics** | Existing testbenches (migrated) | RSTATE/RSETUP/RDRAW/RRECT/GSTATE/GPARAM/GDRAW |
| **Xgpu texture** | Existing testbenches (migrated) | TEX2D nearest/bilinear |
| **Integration** | compute_unit_full_tb | End-to-end pipeline verification |
| **Graphics pipeline** | gfx_*_tb suite | Full raster/ROP/texture pipeline |
| **Benchmarks** | coremark_tb, dhrystone_tb | Performance regression |

### 13.2 Migration Testing Strategy

1. **Parallel build**: Maintain old and new isa_pkg side-by-side during transition
2. **Per-phase verification**: Each phase has a defined verification gate
3. **Waveform comparison**: Compare key signals (PC trace, register writes) between old and new for same test program
4. **Performance counters**: Ensure IPC and stall behavior does not regress (opcode changes should be performance-neutral)

### 13.3 Continuous Integration

- **Verilator lint**: `verilator --lint-only` on all RTL (per `docs/SystemVerilog_RTL_Policy.md`)
- **Yosys smoke synthesis**: ensure no synthesis regressions
- **riscv-tests**: automated compliance suite
- **Existing testbench regression**: all existing tests must pass

---

## 14. Risk Assessment & Mitigations

### High Risk

| Risk | Impact | Mitigation |
|------|--------|-----------|
| Store immediate encoding change (I-type → S-type) | Breaks all store instructions in RTL + testbenches | Implement and verify store path first in Phase 1; keep old encoding as fallback flag |
| JALR opcode separation | Changes branch/jump control flow | Test thoroughly with branch_tb; verify with riscv-tests `rv32ui-p-jalr` |
| Decoder complexity increase | May affect timing closure | Profile critical path after decoder rewrite; register outputs if needed |

### Medium Risk

| Risk | Impact | Mitigation |
|------|--------|-----------|
| Opcode collision during transition | Partially migrated builds may have ambiguous decoding | Never mix old/new opcodes in same build; use ifdef guards |
| Testbench migration volume (~25 files) | Tedious, error-prone | Automate with search-replace scripts; verify each file independently |
| CSR encoding change | Different immediate field layout | Test with riscv-tests CSR suite |

### Low Risk

| Risk | Impact | Mitigation |
|------|--------|-----------|
| Custom extension conflicts with future RV standards | Unlikely for custom-0..3 slots | Monitor RISC-V ISA specification changes; custom slots are designated for this use |
| Performance regression | Opcode routing changes should not affect timing | Verify with benchmarks |
| Toolchain integration issues | `.insn` directive may have assembler quirks | Test with both GNU and LLVM assemblers |

---

## Appendix A: Full Opcode Table

### Standard RV32IMA_Zicsr Instructions (CU must implement)

| Opcode | Hex | Type | Instructions |
|--------|-----|------|-------------|
| `LUI` | 0x37 | U | LUI |
| `AUIPC` | 0x17 | U | AUIPC *(new)* |
| `JAL` | 0x6F | J | JAL |
| `JALR` | 0x67 | I | JALR *(currently in BRANCH)* |
| `BRANCH` | 0x63 | B | BEQ, BNE, BLT, BGE, BLTU, BGEU |
| `LOAD` | 0x03 | I | LB, LH, LW, LBU *(new)*, LHU *(new)* |
| `STORE` | 0x23 | S | SB, SH, SW |
| `OP-IMM` | 0x13 | I | ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI |
| `OP` | 0x33 | R | ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND |
| `OP` (M-ext) | 0x33 | R | MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU |
| `MISC-MEM` | 0x0F | I | FENCE, FENCE.I *(Zifencei)* |
| `SYSTEM` | 0x73 | I | ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI, WFI |
| `AMO` | 0x2F | R | AMOADD.W, AMOSWAP.W, AMOAND.W, AMOOR.W, AMOXOR.W, AMOMIN.W, AMOMAX.W, AMOMINU.W, AMOMAXU.W, LR.W, SC.W |

### Xgpu Custom Extension Instructions

| Opcode | Hex | Slot | Category | Instructions |
|--------|-----|------|----------|-------------|
| `custom-0` | 0x0B | Xgpu-FP/Gfx | FP16 Scalar | FADD.H, FSUB.H, FMUL.H, FMA.H, FMIN.H, FMAX.H, FCVT.W.H, FCVT.H.W, FRECIP.H, FRSQRT.H |
| `custom-0` | 0x0B | Xgpu-Gfx | Graphics | RSTATE, RSETUP, RDRAW, RRECT, GSTATE, GPARAM, GDRAW |
| `custom-0` | 0x0B | Xgpu-Tex | Texture | TEX2D |
| `custom-0` | 0x0B | Xgpu-Cfg | Config | SETMISC |
| `custom-1` | 0x2B | Xgpu-Vec | Vector ALU | VADD, VSUB, VMIN, VCMP, VMAX, VDOT, VCROSS, VSEL, VSWIZ, VPACK, VXOR, VAND, VOR, VMUL, VRCP, VRSQRT, VUNPACK |
| `custom-2` | 0x5B | Xgpu-VMem | Vector Memory | VLD.V, VLD.S, VLDX, VLD.C, VST.V, VST.S, VSTX |
| `custom-2` | 0x5B | Xgpu-VAtom | Vector Atomics | VATOM.ADD, VATOM.MIN, VATOM.MAX, VATOM.XCHG, VATOM.CAS, VATOM.AND, VATOM.OR, VATOM.XOR |
| `custom-3` | 0x7B | *(reserved)* | Matrix/Tensor | *(future: MCFG, MLOAD, MSTORE, MMUL, MWAIT)* |

---

## Appendix B: RISC-V Opcode Map Reference

Standard RV32 7-bit opcode space (inst[6:0]):

```
0x03  LOAD        (I-type)   ← currently OP_LOAD=0x0C, MUST CHANGE
0x07  LOAD-FP     (I-type)   — not used (no F ext in base)
0x0B  custom-0    (R-type)   ← Xgpu FP16 + Graphics + Texture
0x0F  MISC-MEM    (I-type)   ← FENCE (currently OP_SYSTEM=0x0F for MEMBAR)
0x13  OP-IMM      (I-type)   ← currently OP_TEX=0x13, CONFLICT! MUST CHANGE 
0x17  AUIPC       (U-type)   ← currently OP_INT_IMM=0x17, CONFLICT! MUST CHANGE
0x1B  OP-IMM-32   (I-type)   — RV64 only, not used
0x23  STORE       (S-type)   ← currently OP_STORE=0x0D, MUST CHANGE
0x27  STORE-FP    (S-type)   — not used
0x2B  custom-1    (R-type)   ← Xgpu Vector ALU
0x2F  AMO         (R-type)   ← currently OP_VEC_ALU=0x2F, CONFLICT! MUST CHANGE (→ standard atomics)
0x33  OP          (R-type)   ← currently OP_INT=0x0B, MUST CHANGE
0x37  LUI         (U-type)   ← ✅ already correct
0x3B  OP-32       (R-type)   — RV64 only, not used
0x43  MADD        (R4-type)  — not used
0x47  MSUB        (R4-type)  — not used
0x4B  NMSUB       (R4-type)  — not used
0x4F  NMADD       (R4-type)  — not used
0x53  OP-FP       (R-type)   — reserved for future full F ext
0x57  OP-V        (R-type)   — reserved for RVV (not used; Xgpu uses custom slots)
0x5B  custom-2    (R-type)   ← Xgpu Vector Memory + Vector Atomics
0x63  BRANCH      (B-type)   ← currently OP_BRANCH=0x0E, MUST CHANGE
0x67  JALR        (I-type)   ← NEW (currently encoded in OP_BRANCH funct3=010)
0x6F  JAL         (J-type)   ← ✅ already correct
0x73  SYSTEM      (I-type)   ← currently OP_SYSTEM=0x0F, MUST CHANGE
0x7B  custom-3    (R-type)   ← reserved for future matrix/tensor
```

---

## Appendix C: Xgpu CSR Map

### Standard Machine-Level CSRs (subset, matching control processor)

| Address | Name | Description |
|---------|------|-------------|
| 0x300 | `mstatus` | Machine status (MIE, MPIE) |
| 0x304 | `mie` | Machine interrupt enable |
| 0x305 | `mtvec` | Machine trap vector base |
| 0x340 | `mscratch` | Machine scratch register |
| 0x341 | `mepc` | Machine exception PC |
| 0x342 | `mcause` | Machine trap cause |
| 0x343 | `mtval` | Machine trap value |
| 0x344 | `mip` | Machine interrupt pending |
| 0xF11 | `mvendorid` | Vendor ID |
| 0xF12 | `marchid` | Architecture ID |
| 0xF13 | `mimpid` | Implementation ID |
| 0xF14 | `mhartid` | Hart ID (core ID within cluster) |

### Xgpu Custom CSRs (0x800–0x8FF, machine custom read-write)

| Address | Name | Description | Current equivalent |
|---------|------|-------------|-------------------|
| 0x800 | `gpu_vmask` | Vector predicate mask (4-bit) | CSR `VMASK` |
| 0x801 | `gpu_flags` | Integer condition flags (Z/N/C/V) | CSR `FLAGS` |
| 0x802 | `gpu_fstatus` | FP status flags (NV/DZ/OF/UF/NX) | CSR `FSTATUS` |
| 0x803 | `gpu_vstatus` | Vector status (sat/overflow) | CSR `VSTATUS` |
| 0x804 | `gpu_config` | GPU config (rounding mode, sat enable) | CSR `CONFIG` |
| 0x805 | `gpu_core_id` | Core ID within tile (0-3) | CSR `CORE_ID` |
| 0x806 | `gpu_tile_offset` | Tile origin X/Y | CSR `TILE_OFFSET` |
| 0x807 | `gpu_arg_base` | Kernel argument base pointer | CSR `ARG_BASE` |
| 0x808 | `gpu_cycle` | Cycle counter | CSR `TIME` |
| 0x810 | `gpu_status` | Scalar pipeline status | CSR `STATUS` |
| 0x820 | `gpu_mpu_ctrl` | MPU control | CSR `MPU_CTRL` |
| 0x821 | `gpu_mpu_kbase` | MPU kernel region base | CSR `MPU_KREGION_BASE` |
| 0x822 | `gpu_mpu_klimit` | MPU kernel region limit | CSR `MPU_KREGION_LIMIT` |

### Benefits of Standard CSR Encoding

1. Standard `CSRRW`/`CSRRS`/`CSRRC` instructions access all CSRs uniformly
2. CSR addresses follow RISC-V privilege level conventions (0x800-0x8FF = machine custom R/W)
3. Debugger tools can enumerate and display custom CSRs
4. `mhartid` replaces custom `CORE_ID` CSR (standard way to identify cores)

---

*Document created for TinyGPU-FPGA RISC-V ISA migration planning.*
*All encoding details are proposals subject to RTL verification.*
