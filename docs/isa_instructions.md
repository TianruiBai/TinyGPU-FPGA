# Instruction Set Architecture Reference

This document provides a detailed reference for the Instruction Set Architecture (ISA) of the tinyGPU compute unit. The CU implements **RV32IMA_Zicsr** (standard RISC-V base + atomics + CSRs) plus the **RVV v1.0** vector extension and an **Xgpu** custom extension for graphics, texture, FP16, and GPU-specific vector ops.

> **Migration note:** The ISA was migrated from a custom TinyGPU encoding to standard RISC-V in two phases. See `docs/riscv_isa_migration_plan.md` for history and rationale.

**Register Files:**
- **Integer (`x`):** `x0`–`x31` (32-bit). `x0` is hardwired to 0. Standard RISC-V integer registers.
- **Floating-Point (`f`):** `f0`–`f31` (FP16 values). Used by Xgpu FP16 scalar ops under `OP_CUSTOM0`.
- **Vector (`v`):** `v0`–`v31` (VLEN=128-bit). RVV vector registers. Lane count depends on SEW: 4×i32/f32, 8×i16/f16, 16×i8/f8.

**Execution Model:**
- **Scalar pipe:** RV32IMA_Zicsr — standard integer, branch, load/store, atomics, CSRs.
- **Vector pipe:** RVV v1.0 + Xgpu custom vector ops — dual-ALU, decoupled issue queue.
- **LMUL:** Only LMUL=1 is supported. Non-zero `vlmul` in `vtype` sets `vill=1`.
- **Masking:** RVV uses `v0` as mask register when `vm=0`. Xgpu custom VCMP writes a scalar lane-mask to `rd`.
- **Endianness:** Little-endian.

---

## 1. Conventions & Addressing Modes
- **Operands:** Registers are addressed by 5-bit fields (`0–31`). The opcode class determines the register file (`x`, `f`, or `v`).
- **Immediates:**
  - **I-type:** `imm[11:0]` (signed 12-bit).
  - **S-type:** `{imm[11:5], imm[4:0]}` (signed 12-bit, split across funct7 + rd fields).
  - **B-type:** PC-relative 13-bit (bit 0 always 0).
  - **U-type:** `imm[31:12]` (for `LUI`, `AUIPC`).
  - **J-type:** PC-relative 21-bit (for `JAL`).
- **Memory Addressing:**
  - Scalar load: `addr = rs1 + sign_ext(imm12)` (I-type)
  - Scalar store: `addr = rs1 + sign_ext(imm12)` (S-type)
  - Vector load/store: `addr = rs1` (unit-stride)

---

## 2. Encoding Overview

### Opcode Map (7-bit, inst[6:0])

| Opcode | Hex | Mnemonic | Description |
| :--- | :--- | :--- | :--- |
| `0110111` | 0x37 | `OP_LUI` | U-type upper immediate |
| `0010111` | 0x17 | `OP_AUIPC` | U-type add upper immediate to PC |
| `1101111` | 0x6F | `OP_JAL` | J-type jump and link |
| `1100111` | 0x67 | `OP_JALR` | I-type jump and link register |
| `1100011` | 0x63 | `OP_BRANCH` | B-type conditional branches |
| `0000011` | 0x03 | `OP_LOAD` | I-type scalar loads |
| `0100011` | 0x23 | `OP_STORE` | S-type scalar stores |
| `0010011` | 0x13 | `OP_IMM` | I-type ALU (register-immediate) |
| `0110011` | 0x33 | `OP_REG` | R-type ALU (register-register) |
| `0001111` | 0x0F | `OP_FENCE` | MISC-MEM (fence) |
| `1110011` | 0x73 | `OP_SYSTEM` | CSR / ECALL / EBREAK |
| `0101111` | 0x2F | `OP_AMO` | R-type atomics (RV32A) |
| `1010111` | 0x57 | `OP_V` | RVV vector arithmetic + vsetvl{i} |
| `0000111` | 0x07 | `OP_VL` | RVV vector loads (LOAD-FP) |
| `0100111` | 0x27 | `OP_VS` | RVV vector stores (STORE-FP) |
| `0001011` | 0x0B | `OP_CUSTOM0` | Xgpu: FP16 scalar + Graphics + Texture |
| `0101011` | 0x2B | `OP_CUSTOM1` | Xgpu: Custom vector (VDOT/VCROSS/VSWIZ/VPACK/VRCP/VRSQRT/VUNPACK) |
| `1011011` | 0x5B | `OP_CUSTOM2` | Xgpu: Vector atomics (VATOM) |

### Encoding Formats
- **Scalar:** Standard RV32 R/I/S/B/U/J formats with 7-bit opcode.
- **RVV Vector (OP_V):**
  - `[31:26] = funct6` (operation)
  - `[25]    = vm` (0=masked using v0, 1=unmasked)
  - `[24:20] = vs2/rs2`
  - `[19:15] = vs1/rs1`
  - `[14:12] = funct3` (operand category: OPIVV/OPFVV/OPMVV/OPIVI/OPIVX/OPFVF/OPMVX/OPCFG)
  - `[11:7]  = vd/rd`
  - `[6:0]   = 1010111`
- **Xgpu Custom-1 (OP_CUSTOM1):** R-type shape. `funct6 = inst[31:26]`, `funct3` selects element type (TYPE_I32=000..TYPE_FP8=101).

---

## 3. Scalar Integer (RV32I + RV32M)

### Register-Register ALU (OP_REG = 0x33)

| Instruction | funct7 | funct3 | Description |
| :--- | :--- | :--- | :--- |
| `ADD` | `0000000` | 000 | `rd = rs1 + rs2` |
| `SUB` | `0100000` | 000 | `rd = rs1 - rs2` |
| `SLL` | `0000000` | 001 | `rd = rs1 << rs2[4:0]` |
| `SLT` | `0000000` | 010 | `rd = (signed(rs1) < signed(rs2)) ? 1 : 0` |
| `SLTU` | `0000000` | 011 | `rd = (unsigned(rs1) < unsigned(rs2)) ? 1 : 0` |
| `XOR` | `0000000` | 100 | `rd = rs1 ^ rs2` |
| `SRL` | `0000000` | 101 | `rd = rs1 >> rs2[4:0]` (logical) |
| `SRA` | `0100000` | 101 | `rd = rs1 >>> rs2[4:0]` (arithmetic) |
| `OR` | `0000000` | 110 | `rd = rs1 \| rs2` |
| `AND` | `0000000` | 111 | `rd = rs1 & rs2` |
| `MUL` | `0000001` | 000 | `rd = (rs1 × rs2)[31:0]` (RV32M) |

### Register-Immediate ALU (OP_IMM = 0x13)

| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `ADDI` | 000 | `rd = rs1 + sign_ext(imm12)` |
| `SLTI` | 010 | `rd = (signed(rs1) < signed(imm12)) ? 1 : 0` |
| `SLTIU` | 011 | `rd = (unsigned(rs1) < unsigned(imm12)) ? 1 : 0` |
| `XORI` | 100 | `rd = rs1 ^ sign_ext(imm12)` |
| `ORI` | 110 | `rd = rs1 \| sign_ext(imm12)` |
| `ANDI` | 111 | `rd = rs1 & sign_ext(imm12)` |
| `SLLI` | 001 | `rd = rs1 << shamt` (funct7=`0000000`, shamt=imm[4:0]) |
| `SRLI` | 101 | `rd = rs1 >> shamt` (funct7=`0000000`) |
| `SRAI` | 101 | `rd = rs1 >>> shamt` (funct7=`0100000`) |

### Upper Immediates
| Instruction | Opcode | Description |
| :--- | :--- | :--- |
| `LUI` | `0110111` (0x37) | `rd = imm20 << 12` |
| `AUIPC` | `0010111` (0x17) | `rd = pc + (imm20 << 12)` |

### Control Flow

**Conditional Branches (OP_BRANCH = 0x63, B-type):**

| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `BEQ` | 000 | Branch if `rs1 == rs2` |
| `BNE` | 001 | Branch if `rs1 != rs2` |
| `BLT` | 100 | Branch if `signed(rs1) < signed(rs2)` |
| `BGE` | 101 | Branch if `signed(rs1) >= signed(rs2)` |
| `BLTU` | 110 | Branch if `unsigned(rs1) < unsigned(rs2)` |
| `BGEU` | 111 | Branch if `unsigned(rs1) >= unsigned(rs2)` |

**Jumps:**

| Instruction | Opcode | Description |
| :--- | :--- | :--- |
| `JAL` | `1101111` (0x6F) | `rd = pc+4; pc = pc + sign_ext(imm21)` (J-type) |
| `JALR` | `1100111` (0x67) | `rd = pc+4; pc = (rs1 + sign_ext(imm12)) & ~1` (I-type) |

### System / CSR (OP_SYSTEM = 0x73)

| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `ECALL` | 000 | Environment call (imm=0x000) |
| `EBREAK` | 000 | Breakpoint (imm=0x001) |
| `CSRRW` | 001 | CSR Read/Write: `t=CSR[csr]; CSR[csr]=rs1; rd=t` |
| `CSRRS` | 010 | CSR Read/Set: `t=CSR[csr]; CSR[csr]=t\|rs1; rd=t` |
| `CSRRC` | 011 | CSR Read/Clear: `t=CSR[csr]; CSR[csr]=t&~rs1; rd=t` |
| `WFI` | 000 | Wait For Interrupt (imm=0x105) |

**Fence (OP_FENCE = 0x0F):**

| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `FENCE` | 000 | Memory ordering fence |

---

## 4. Scalar Floating-Point (FP16) — Xgpu Extension
**Opcode:** `OP_CUSTOM0` (`0001011` / 0x0B) | **funct7:** `0x08` (ALU) or `0x09` (SFU)

| Instruction | funct7 | funct3 | Description |
| :--- | :--- | :--- | :--- |
| `FADD` | `0001000` | 000 | `fd = fs1 + fs2` (FP16) |
| `FSUB` | `0001000` | 001 | `fd = fs1 - fs2` |
| `FMUL` | `0001000` | 010 | `fd = fs1 × fs2` |
| `FMA` | `0001000` | 011 | `fd = (fs1 × fs2) + addend` |
| `FMIN` | `0001000` | 100 | FP16 Minimum |
| `FMAX` | `0001000` | 101 | FP16 Maximum |
| `FCVT.i2f` | `0001000` | 110 | Int32 (scalar `rs1`) → FP16 (`fd`) |
| `FCVT.f2i` | `0001000` | 111 | FP16 (`fs1`) → Int32 (scalar `rd`) |
| `FRECIP` | `0001001` | 000 | FP16 Reciprocal Approximation |
| `FRSQRT` | `0001001` | 001 | FP16 Reciprocal Sqrt Approximation |

---

## 5. Memory Access

### Scalar Loads (OP_LOAD = 0x03, I-type)

| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `LB` | 000 | Load byte (sign-extended) |
| `LH` | 001 | Load halfword (sign-extended) |
| `LW` | 010 | Load word |
| `LBU` | 100 | Load byte (zero-extended) |
| `LHU` | 101 | Load halfword (zero-extended) |

Address: `rs1 + sign_ext(imm12)`

### Scalar Stores (OP_STORE = 0x23, S-type)

| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `SB` | 000 | Store byte |
| `SH` | 001 | Store halfword |
| `SW` | 010 | Store word |

Address: `rs1 + sign_ext({imm[11:5], imm[4:0]})`

### Scalar Atomics (OP_AMO = 0x2F, R-type — RV32A)

Standard RV32A atomic memory operations. All are 32-bit width, R-type format with `funct5` in inst[31:27] and `aq`/`rl` bits.

| Instruction | funct5 | Description |
| :--- | :--- | :--- |
| `LR.W` | `00010` | Load-Reserved Word |
| `SC.W` | `00011` | Store-Conditional Word |
| `AMOSWAP.W` | `00001` | Atomic Swap |
| `AMOADD.W` | `00000` | Atomic Add |
| `AMOAND.W` | `01100` | Atomic AND |
| `AMOOR.W` | `01000` | Atomic OR |
| `AMOXOR.W` | `00100` | Atomic XOR |
| `AMOMIN.W` | `10000` | Atomic Signed Min |
| `AMOMAX.W` | `10100` | Atomic Signed Max |
| `AMOMINU.W` | `11000` | Atomic Unsigned Min |
| `AMOMAXU.W` | `11100` | Atomic Unsigned Max |

---

## 6. RVV Vector Extension (OP_V / OP_VL / OP_VS)

### 6.0 Vector Configuration

**VLEN = 128 bits.** LMUL = 1 only (non-zero `vlmul` in `vtype` sets `vill`).

| SEW | Lanes (VLMAX) | Type Code |
| :--- | :--- | :--- |
| 8-bit | 16 | TYPE_I8 / TYPE_FP8 |
| 16-bit | 8 | TYPE_I16 / TYPE_FP16 |
| 32-bit | 4 | TYPE_I32 / TYPE_FP32 |

**Vector CSRs:**

| CSR | Address | Description |
| :--- | :--- | :--- |
| `vstart` | `0x008` | Vector start position |
| `vxsat` | `0x009` | Fixed-point saturation flag |
| `vxrm` | `0x00A` | Fixed-point rounding mode |
| `vcsr` | `0x00F` | Vector control and status |
| `vl` | `0xC20` | Vector length (read-only) |
| `vtype` | `0xC21` | Vector data type (read-only) |
| `vlenb` | `0xC22` | VLEN/8 in bytes = 16 (read-only) |

**Configuration instructions (OP_V, funct3=OPCFG=111):**

| Instruction | Encoding | Description |
| :--- | :--- | :--- |
| `vsetvli` | inst[31]=0 | `rd = vl; vtype = zimm[10:0]` (AVL from rs1) |
| `vsetivli` | inst[31:30]=11 | `rd = vl; vtype = zimm[9:0]` (AVL from uimm5) |
| `vsetvl` | inst[31:30]=10 | `rd = vl; vtype = rs2` (both from registers) |

### 6.1 RVV Arithmetic (OP_V = 0x57)

**Operand categories (funct3):**

| funct3 | Name | Source operands |
| :--- | :--- | :--- |
| 000 | OPIVV | vector-vector (integer) |
| 001 | OPFVV | vector-vector (FP) |
| 010 | OPMVV | vector-vector (mask/multiply) |
| 011 | OPIVI | vector-immediate (integer) |
| 100 | OPIVX | vector-scalar (integer) |
| 101 | OPFVF | vector-scalar (FP) |
| 110 | OPMVX | vector-scalar (mask/multiply) |
| 111 | OPCFG | vsetvl/vsetvli/vsetivli |

**Supported RVV funct6 operations and internal ALU mapping:**

| RVV funct6 | RVV Mnemonic | Internal funct6 | Internal funct3 | Description |
| :--- | :--- | :--- | :--- | :--- |
| `000000` | `vadd` / `vfadd` | `000000` | `000` | Vector add |
| `000010` | `vsub` / `vfsub` | `000001` | `000` | Vector subtract (a − b) |
| `000011` | `vrsub` | `000001` | `001` | Reverse subtract (b − a) |
| `000100` | `vminu` / `vfmin` | `000010` | `000` | Unsigned min / FP min |
| `000101` | `vmin` | `000010` | `010` | Signed min |
| `000110` | `vmaxu` / `vfmax` | `000010` | `001` | Unsigned max / FP max |
| `000111` | `vmax` | `000010` | `011` | Signed max |
| `001001` | `vand` | `001010` | `000` | Bitwise AND |
| `001010` | `vor` | `001011` | `000` | Bitwise OR |
| `001011` | `vxor` | `001001` | `000` | Bitwise XOR |
| `010111` | `vmerge`/`vmv` | `000110` | `000` | Merge / Move |
| `011000` | `vmseq` / `vmfeq` | `000011` | `000` | Compare: equal |
| `011001` | `vmsne` / `vmfle` | `000011` | `011` | Compare: not-equal / FP ≤ |
| `011010` | `vmsltu` | `000011` | `010` | Compare: unsigned less-than |
| `011011` | `vmslt` / `vmflt` | `000011` | `001` | Compare: signed less-than / FP < |
| `011100` | `vmsleu` / `vmfne` | `000011` | `110` | Compare: unsigned ≤ / FP ≠ |
| `011101` | `vmsle` | `000011` | `101` | Compare: signed ≤ |
| `011110` | `vmsgtu` | `000011` | `100` | Compare: unsigned > |
| `011111` | `vmsgt` | `000011` | `111` | Compare: signed > |
| `100100` | `vfmul` | `001100` | `000` | FP multiply |
| `100000` | `vfdiv` | `001100` | `001` | FP division (via mul × rcp) |
| `100101` | `vsll` / `vmul` | `000000/001` or `001100/000` | — | Shift left (OPIVV) or multiply (OPMVV) |
| `101000` | `vsrl` | `000000` | `101` | Logical shift right |
| `101001` | `vsra` | `000000` | `101` | Arithmetic shift right |

**Compare operations** write a scalar lane-mask to integer `rd` (not a vector result). The mask has 1 bit per active lane.

**FP type override:** When `type_sel` is FP32/FP16/FP8, internal funct6 `000010` uses `funct3[0]` to select min (0) or max (1). Internal funct6 `000011` performs FP comparison with NaN-aware semantics (any NaN input → result 0).

### 6.2 RVV Vector Loads / Stores

**Vector Loads (OP_VL = 0x07):** Unit-stride only. `addr = rs1`. Width from `inst[14:12]`:

| Width encoding | Element width |
| :--- | :--- |
| `000` | 8-bit (vle8) |
| `101` | 16-bit (vle16) |
| `110` | 32-bit (vle32) |

**Vector Stores (OP_VS = 0x27):** Unit-stride only. Same width encoding. Store data comes from `vs3` (inst[11:7]).

**Current RTL limitation:** Vector loads/stores target **local memory** (`addr[31]==0`) only. Use scalar loads/stores for global (external) addressing.

---

## 6.3 Xgpu Custom Vector Ops (OP_CUSTOM1 = 0x2B)

These use R-type encoding with `funct6 = inst[31:26]` and `funct3` selecting element type directly (TYPE_I32=000..TYPE_FP8=101).

| funct6 | Mnemonic | Description |
| :--- | :--- | :--- |
| `000100` | `VDOT` | Dot product: per-lane multiply, reduce-sum into lane 0. Lanes 1..N zeroed. |
| `000101` | `VCROSS` | Cross product (lanes 0–2) → vector rd. |
| `000110` | `VSEL` | Predicated blend: `mask ? rs1 : rs2` per lane via CSR VMASK. |
| `000111` | `VSWIZ` | Swizzle/Shuffle: rs2 scalar mask holds packed 2-bit lane selectors. |
| `001000` | `VPACK` | Pack lanes 0..3 into scalar ARGB8888 (writes scalar rd). Unary. |
| `001001` | `VXOR` | Bitwise XOR on raw lane bits. |
| `001010` | `VAND` | Bitwise AND on raw lane bits. |
| `001011` | `VOR` | Bitwise OR on raw lane bits. |
| `001100` | `VMUL` | Element-wise multiply (INT/FP). |
| `001101` | `VRCP` | Reciprocal approximation (FP only). Unary: rs1 only, rs2 ignored. |
| `001110` | `VRSQRT` | Reciprocal sqrt approximation (FP only). Unary: rs1 only, rs2 ignored. |
| `001111` | `VUNPACK` | Unpack scalar ARGB8888 (from scalar rs2) into vector lanes r,g,b,a. |

**Internal ALU encoding for CUSTOM1:** `funct6 = ctrl.funct7[6:1]`, sub-op = `ctrl.funct3`. These pass through directly to `alu_vector.sv`.

**Internal CMP sub-op encoding (funct6=000011):**

| funct3 | Compare operation |
| :--- | :--- |
| `000` | Equal (eq) |
| `001` | Signed less-than (slt) |
| `010` | Unsigned less-than (sltu) |
| `011` | Not-equal (ne) |
| `100` | Unsigned greater-than (gtu) |
| `101` | Signed less-or-equal (sle) |
| `110` | Unsigned less-or-equal (sleu) |
| `111` | Signed greater-than (sgt) |

**Internal min/max encoding (funct6=000010):**

| funct3 | Operation |
| :--- | :--- |
| `000` | Unsigned min (integer) / FP min (FP types via funct3[0]=0) |
| `001` | Unsigned max (integer) / FP max (FP types via funct3[0]=1) |
| `010` | Signed min |
| `011` | Signed max |

### VMUL / VRCP / VRSQRT / VPACK / VUNPACK Notes

**VMUL:** Element-wise multiply. Integer types wrap/truncate. FP32 uses simplified IEEE multiply (subnormals flushed). FP16/FP8 compute in FP32 internally and convert back.

**VRCP / VRSQRT:** FP-only (TYPE_FP32/FP16/FP8). `VRSQRT` uses `0x5f3759df` initial guess + 1 Newton-Raphson. `VRCP` uses mantissa LUT + 1 Newton-Raphson. FP16/FP8 compute in FP32 and convert back.

**VFDIV (RVV):** Implemented as `mul(a, rcp(b))` — approximate FP division.

**VPACK:** Reads FP lanes from `rs1`, converts to [0,255] integers, packs as `{A,B,G,R}` (ARGB8888 little-endian: byte 0=R, byte 1=G, byte 2=B, byte 3=A). Produces scalar result in `rd`.

**VUNPACK:** Takes scalar ARGB8888 from `rs2`, expands bytes to normalized [0,1] floats in vector lanes {R,G,B,A}.

**VDOT:** INT and FP supported. Multiplies per-lane and reduces (sums) into lane 0. FP16/FP8 accumulate in FP32. Masked-off lanes contribute 0.

### 6.4 Vector Atomics (OP_CUSTOM2 = 0x5B)

Vector atomics use S-type encoding with `funct3[2]=1`. Per-lane load→RMW→store.

---

## 7. RVV Vector Transfer & Memory (Legacy)

### Transfer Ops
**Status:** **Not implemented in current RTL.**
- `VBCAST.s`, `VINS.s`, `VEXTR.s` are documented here for ISA planning but have no decode entry.

### Vector Memory (via OP_VL / OP_VS)
See Section 6.2 above for the current RVV vector load/store encoding.

---

## 8. Texture & Sampler — Xgpu Extension
**Opcode:** `OP_CUSTOM0` (`0001011` / 0x0B) | **funct7:** `0x42` (`C0_TEX`) | **funct3:** `000` (Nearest)

**Operands:**
- `rd`: Result Vector (Color)
- `rs1`: Coordinates Vector `{u, v, 0, 0}`
- `rs2`: Sampler Handle.
  - **Index Mode:** Index 0-31 (On-chip table, preloaded by host).
  - **Pointer Mode:** 32-byte descriptor pointer (Higher latency).

**Sampler Descriptor (32 bytes):**
- `0x00`: Base Address
- `0x04`: Stride
- `0x08`: Width
- `0x0C`: Height
- `0x10`: Format (RGB565, ARGB888, PAL8, Swizzle)
- `0x14`: Wrap Mode (Clamp/Repeat)
- `0x18`: Filter Mode (Nearest, Bilinear)
- `0x1C`: Misc (Palette/LUT Handle, sRGB/UNORM Decode Enable)

---

## 9. Graphics / Raster Ops — Xgpu Extension
**Opcode:** `OP_CUSTOM0` (`0001011` / 0x0B) | **funct7:** `0x00` (`C0_GFX`)
Provides fixed-function graphics acceleration and ROP. `rs1` points to a descriptor.

| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| **Raster** | | |
| `RSTATE` | 000 | Load Render State (Framebuf base/fmt, Depth, Viewport, Blend mode). |
| `RSETUP` | 001 | Load Vertex Block (3 verts `{x,y,z,w,u,v,color}`) & compute edges/barycentrics. |
| `RDRAW` | 010 | Kick triangle using last setup data. Flags select textured/depth/blend. |
| `RRECT` | 011 | Axis-aligned rect fill/blit (supports color fill or textured). |
| **Geometry** | | |
| `GSTATE` | 100 | Load Geometry State (VBO/IBO ptrs, MVP/Norm matrices, Lights, Cull/Clip). |
| `GPARAM` | 101 | Load Per-Draw Params (Model matrix, Material color) - avoids full GSTATE reload. |
| `GDRAW` | 110 | Issue Draw: `{vtx_base, idx_base, count, topology, state_id, param_id, flags}`. |

**Descriptors:**
- **RSTATE:** 64B. FB/Depth config, Viewport, Scissor, Blend/Sampler indices.
- **RSETUP:** 96B. 3 vertices.
- **GDRAW:** 24B. Command block including topology (tri-list/strip) and flags (cull, clip, CU-assist).
  - Descriptor locality: keep RSTATE/GSTATE near (e.g., firmware-managed cache or SRAM). Fetching descriptors from VRAM over OSPI can add 100+ cycles of stall on a miss.
  - **Issue semantics (target architecture):** RSTATE/RSETUP/RDRAW/GSTATE/GPARAM/GDRAW are *non-blocking submits*.
    - Each instruction enqueues a small 32-bit **GPU command packet** into a ring buffer in VRAM.
    - The scalar core resumes immediately unless the ring is full (backpressure case).
    - A dedicated **Command Streamer** (autonomous DMA) consumes the ring and feeds the pipelined graphics blocks.
  - **Ordering rule:** use `MEMBAR` to force a full GPU drain/flush when the program needs “all prior graphics complete and visible”.

### 9.1 Descriptor Layouts (Normative)

All descriptor words are **32-bit little-endian**.

#### 9.1.1 `RSTATE` descriptor (64 bytes)

`RSTATE rs1=<ptr>` loads the active render state used by `RDRAW` and `RRECT`.

| Off | Name | Description |
| :-- | :--- | :--- |
| `0x00` | `FB_BASE` | Framebuffer base address in VRAM |
| `0x04` | `FB_STRIDE_BYTES` | Bytes per row |
| `0x08` | `FB_FORMAT` | `0=ARGB8888`, `1=RGB565` (others reserved) |
| `0x0C` | `FB_WH` | `[15:0]=width`, `[31:16]=height` (pixels) |
| `0x10` | `DEPTH_BASE` | Depth buffer base address (0 disables depth) |
| `0x14` | `DEPTH_STRIDE_BYTES` | Bytes per row |
| `0x18` | `DEPTH_FORMAT` | `0=disabled`, `1=D16` (others reserved) |
| `0x1C` | `BLEND_MODE` | `0=opaque`, `1=alpha`, `2=add` (others reserved) |
| `0x20` | `CONST_COLOR` | ARGB8888 constant color (used by `RRECT` fill and optional flat shading) |
| `0x24` | `SAMPLER_HANDLE` | For textured ops: TEX sampler handle (index 0–31 or descriptor pointer) |
| `0x28` | `SCISSOR_XY` | `[15:0]=x0`, `[31:16]=y0` |
| `0x2C` | `SCISSOR_WH` | `[15:0]=w`, `[31:16]=h` |
| `0x30` | `FLAGS0` | bit0 `ENABLE_SCISSOR`, bit1 `ENABLE_TEXTURE`, bit2 `ENABLE_DEPTH`, others reserved |
| `0x34` | `RESERVED` | 0 |
| `0x38` | `RESERVED` | 0 |
| `0x3C` | `RESERVED` | 0 |

#### 9.1.2 `RSETUP` vertex block (96 bytes)

`RSETUP rs1=<ptr>` loads 3 vertices (screen-space) and prepares rasterization.

Vertex $i \in \{0,1,2\}$ is at base offset `i*32`:

| Off | Name | Description |
| :-- | :--- | :--- |
| `0x00` | `X` | Signed 32-bit screen X (pixels; fractional formats reserved) |
| `0x04` | `Y` | Signed 32-bit screen Y (pixels; fractional formats reserved) |
| `0x08` | `Z` | Optional depth (implementation-defined; may be ignored) |
| `0x0C` | `W` | Optional clip W / 1/W (implementation-defined; may be ignored) |
| `0x10` | `U` | Optional texture U (implementation-defined) |
| `0x14` | `V` | Optional texture V (implementation-defined) |
| `0x18` | `COLOR` | ARGB8888 per-vertex color (optional; may be ignored) |
| `0x1C` | `RESERVED` | 0 |

#### 9.1.3 `RRECT` descriptor (32 bytes)

`RRECT rs1=<ptr>` describes a rectangle blit/fill.

| Off | Name | Description |
| :-- | :--- | :--- |
| `0x00` | `X0` | Signed 32-bit left |
| `0x04` | `Y0` | Signed 32-bit top |
| `0x08` | `X1` | Signed 32-bit right (exclusive) |
| `0x0C` | `Y1` | Signed 32-bit bottom (exclusive) |
| `0x10` | `COLOR` | ARGB8888 fill color (overrides `RSTATE.CONST_COLOR` if nonzero; optional) |
| `0x14` | `FLAGS` | bit0 `TEXTURED`, bit1 `USE_STATE_COLOR`, bit2 `USE_STATE_SAMPLER`, others reserved |
| `0x18` | `SAMPLER_HANDLE` | Optional override sampler handle |
| `0x1C` | `RESERVED` | 0 |

#### 9.1.4 `GSTATE` descriptor (64 bytes, minimal)

`GSTATE rs1=<ptr>` loads geometry state used by `GDRAW`.

| Off | Name | Description |
| :-- | :--- | :--- |
| `0x00` | `VBO_BASE` | Vertex buffer base |
| `0x04` | `VBO_STRIDE_BYTES` | Stride in bytes |
| `0x08` | `IBO_BASE` | Index buffer base (0 means non-indexed) |
| `0x0C` | `IBO_FORMAT` | `0=U16`, `1=U32` |
| `0x10` | `MVP_PTR` | Pointer to a 4x4 matrix block (layout implementation-defined) |
| `0x14` | `LIGHT0_PTR` | Pointer to light block (layout implementation-defined) |
| `0x18` | `CULL_CLIP_FLAGS` | bit0 `CULL_ENABLE`, bit1 `CLIP_ENABLE`, others reserved |
| `0x1C` | `RESERVED` | 0 |
| `0x20` | `RESERVED` | 0 |
| `0x24` | `RESERVED` | 0 |
| `0x28` | `RESERVED` | 0 |
| `0x2C` | `RESERVED` | 0 |
| `0x30` | `RESERVED` | 0 |
| `0x34` | `RESERVED` | 0 |
| `0x38` | `RESERVED` | 0 |
| `0x3C` | `RESERVED` | 0 |

#### 9.1.5 `GPARAM` descriptor (32 bytes, minimal)

`GPARAM rs1=<ptr>` loads per-draw parameters.

| Off | Name | Description |
| :-- | :--- | :--- |
| `0x00` | `BASE_VERTEX` | Added to indices/vertex fetch |
| `0x04` | `BASE_INSTANCE` | Reserved (0) |
| `0x08` | `MATERIAL_COLOR` | ARGB8888 base material color |
| `0x0C` | `RESERVED` | 0 |
| `0x10` | `RESERVED` | 0 |
| `0x14` | `RESERVED` | 0 |
| `0x18` | `RESERVED` | 0 |
| `0x1C` | `RESERVED` | 0 |

#### 9.1.6 `GDRAW` command block (24 bytes)

`GDRAW rs1=<ptr>` issues a geometry draw.

| Off | Name | Description |
| :-- | :--- | :--- |
| `0x00` | `FIRST` | First index (indexed) or first vertex (non-indexed) |
| `0x04` | `COUNT` | Vertex/index count |
| `0x08` | `TOPOLOGY` | `0=tri_list`, `1=tri_strip` (others reserved) |
| `0x0C` | `FLAGS` | bit0 `INDEXED`, bit1 `SHADE_FLAT`, bit2 `TEXTURED`, others reserved |
| `0x10` | `RSTATE_PTR` | Optional pointer to RSTATE override (0=use current) |
| `0x14` | `RESERVED` | 0 |

### 9.2 Implementation Status (Current RTL)

This section is informative and may lag the ISA.

- `TEX2D` is implemented as descriptor-driven fetch with nearest + a simple 2x2 average when `FILTER_MODE.bilinear=1`.
- Raster macro-ops are implemented only to the extent of feeding triangle setup into an internal rasterizer.
- `RSTATE`, `RRECT`, `GSTATE`, `GPARAM`, `GDRAW` are reserved for the full graphics pipeline and may currently retire without effect depending on the build configuration.
- Target behavior for these opcodes is **enqueue-only** (VRAM ring submit) with work executed asynchronously by the graphics pipeline.

### Raster / Fragment Behavior
- Stages: edge eval -> coverage -> barycentric interp (color/UV/z) -> optional depth test -> blend (opaque/alpha/additive) -> writeback.
- Textured draws reuse TEX unit; sampler index comes from state descriptor.
- Default fragment path is interpolated color; programmable per-pixel shading is done via a compute pass over a G-buffer/tile produced by RDRAW (two-pass shading using DISPATCH + VATOM/ROP for blend).

### Vector Packing / Saturation Notes
- Use VPACK/VPACKN for ARGB8888<->vector conversion; keep them single-cycle in hardware.
- Provide VUNPACK to expand ARGB8888/I8/I16 into FP16/FP32 (or widened int) in a single cycle; support optional 1/255 normalization for color paths.
- Saturation: expose sat mode via `vstatus`/config so VADD/VSUB narrows can clamp for color math; ensure pack narrows respect sat mode.

---

## 10. System Resources & ABI

### Control & Synchronization
- **MEMBAR:** Orders memory visibility. Host/Display see writes after barrier.
  - Target behavior extends this to graphics: `MEMBAR` must also wait until the GPU command ring is drained (consumer catches producer) and any in-flight ROP/VRAM traffic completes.
- **BARRIER:** Tile-wide barrier. All 4 cores in tile must arrive before any proceed. Used for shared memory reductions.
- **Atomic Ordering:** Atomics are globally coherent. Regular accesses require `MEMBAR` to observe atomic updates.
- **Coherency:**
  - **Shared/Scratch:** Per-core, not coherent across cores.
  - **Global:** Coherent across cores only for atomics; otherwise weakly ordered.
  - **Host:** Non-coherent during kernel execution. Requires explicit handoff via `MEMBAR` and `DISPATCH`.

### Kernel ABI
- **Registers:** `s0`=0 on entry. All other scalar/vector/fp regs undefined.
- **Inputs:** `arg_base` CSR points to argument block packed by host.
- **Built-in CSRs:**
  - `CORE_ID`: 0-3 (Current core index)
  - `TILE_OFFSET`: Tile Origin (X, Y) for tile-based rendering
  - `TIME`: Cycle Counter

### Exceptions
- **Misalignment:** Allowed with penalty. Sets sticky status flag in `status`.
- **FP Exceptions:** Sets `fstatus` flags (NV, DZ, OF, UF, NX). No traps generated (NaN propagation).
- **Int Overflow:** Wraps unless saturation mode is enabled (`vstatus`/`config`).

---

## Future Extensions & Reserved Features
- **Vector LUT Gather:** Reserved `funct3=111` in `VPERM` for LUT-based gather.
- **Texture Bilinear:** Reserved `funct3 > 000` in Texture opcode.
- **Power/Debug:** System `funct3` 101/110 reserved.
- **VATOM FP16:** Potential future support for FP16 atomics (blending).
