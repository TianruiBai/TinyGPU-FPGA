# Instruction Set Architecture Reference

This document provides a detailed reference for the Instruction Set Architecture (ISA) of the tinyGPU. It is organized by instruction class with encoding and usage details.

**Register Files:**
- **Scalar (`s`):** `s0`–`s31` (32-bit). `s0` is hardwired to 0.
- **Floating-Point (`f`):** `f0`–`f31` (FP16 values). Implementation may widen internally.
- **Vector (`v`):** `v0`–`v31` (128-bit, writable; `v0` is not hardwired to 0). Native view is 4x FP32 or packed types.
- **Constant (`c`):** `c0`–`c31` (Optional, 128-bit, read-only).

**Execution Model:**
- **SIMD:** Four independent cores per tile.
- **Masking:** Predicate masking uses the `vm` bit in the vector encoding and the CSR `VMASK` (see System/CSR map). `VCMP` produces a scalar lane-mask in `rd`; software may write that value into `VMASK` via CSR instructions.
- **Endianness:** Little-endian.

---

## 1. Conventions & Addressing Modes
- **Operands:** Registers are addressed by 5-bit fields (`0–31`). The opcode class determines the register file (`s`, `f`, or `v`).
- **Immediates:**
  - **I-type:** `imm[11:0]` (signed).
  - **U-type:** `imm[19:0]` (for `LUI`).
  - **B-type / J-type:** PC-relative (see encodings).
- **Memory Addressing:**
  - `base` (register only): `addr = rs1`
  - `base + imm12`: `addr = rs1 + imm12` (signed)
  - `base + reg`: `addr = rs1 + rs2`
  - `pc + imm12`: `addr = pc + imm12` (Position Independent)

---

## 2. Encoding Overview
- **Scalar:** Follows standard R/I/B/J/U shapes with a 7-bit opcode.
- **Vector:** Uses a 32-bit R-type shape.
  - `[31:26] = funct6` (Sub-operation)
  - `[25]    = vm` (Mask Enable: uses `vmask` predicate)
  - `[24:20] = rs2`
  - `[19:15] = rs1`
  - `[14:12] = funct3` (Type / Sub-class)
  - `[11:7]  = rd`
  - `[6:0]   = opcode`
- **Vector Opcodes:**
  - `0101111`: ALU / Misc
  - `0010001`: Vector Load (`VLD`)
  - `0010010`: Vector Store (`VST`)
  - `0010011`: Texture (`TEX`)
  - `0010101`: Vector Atomic (`VATOM`)
  - `0010000`: Transfer (`VMOV`, `VBCAST`)

---

## 3. Scalar Integer
**Opcode (R-type / register-register):** `0001011` (`OP_INT`)

**Opcode (I-type / register-immediate):** `0010111` (`OP_INT_IMM`)

### ALU Operations (funct7 = 0000000)
| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `ADD` / `ADDI` | 000 | `rd = rs1 + rs2` (R-type `OP_INT`) / `rd = rs1 + imm12` (I-type `OP_INT_IMM`) |
| `SUB` | 000 | `rd = rs1 - rs2` (funct7=`0100000`) |
| `AND` / `ANDI` | 111 | `rd = rs1 & rs2` / `imm12` (Bitwise) |
| `OR` / `ORI` | 110 | `rd = rs1 | rs2` / `imm12` (Bitwise) |
| `XOR` / `XORI` | 100 | `rd = rs1 ^ rs2` / `imm12` (Bitwise) |
| `SHL` | 001 | Logical Left Shift (shamt: `rs2[4:0]` or `imm5`) |
| `LSR` | 101 | Logical Right Shift (shamt: `rs2[4:0]` or `imm5`) |
| `ASR` | 101 | Arithmetic Right Shift (funct7=`0100000`) |
| `MIN` | 010 | Signed Minimum |
| `MAX` | 011 | Signed Maximum |
| `ABS` | 001 | Absolute value (requires `rs2=0`) |
| `MOVI` | n/a | Pseudo: `movi rd, imm32` expands to `LUI` + `ADDI` |

### Upper Immediate (Opcode 0110111)
**Opcode:** `0110111`

**Layout (U-type):** `imm[31:12] | rd | opcode`

| Instruction | Description |
| :--- | :--- |
| `LUI` | `rd = imm20 << 12` (upper immediate; low 12 bits cleared) |

### Multiply (funct7 = 0000001)
| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `MUL` | 000 | Signed multiply, low 32 bits. R-type `OP_INT` only (no `MULI`). |

**Not implemented (reserved encodings):** `MULH`, `MULHU`, `MAC`.

### Comparison / Count (funct7 = 0000010 / 0000011)
| Instruction | funct7 | funct3 | Description |
| :--- | :--- | :--- | :--- |
| `CMP` | 0000010 | 000 | Signed Compare (Sets Z/N/C/V flags) |
| `CMP` | 0000010 | 001 | Unsigned Compare |
| `CMP` | 0000010 | 010 | Equality Compare |
| `CLZ` | 0000011 | 000 | Count Leading Zeros |
| `CTZ` | 0000011 | 001 | Count Trailing Zeros |

### Control Flow (Opcode 0001110)
| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `BEQ` / `BNE` | 000 / 001 | Branch Equal / Not Equal (PC-relative B-Type) |
| `BLT` / `BGE` | 100 / 101 | Branch Less Than / Greater Equal (Signed) |
| `BLTU` / `BGEU`| 110 / 111 | Branch Less Than / Greater Equal (Unsigned) |
| `JAL` | 011 | Jump and Link (J-Type `imm[20:1]` assembled, signed) |
| `JALR` | 010 | Jump and Link Register (`target = (rs1 + imm12) & ~1`) |

### System (Opcode 0001111)
| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `MEMBAR` | 000 | Memory Barrier (Orders memory vs display/host) |
| `CSRRW` | 001 | CSR Read/Write (Swap) |
| `CSRRS` | 010 | CSR Read/Set (Bitwise OR) |
| `SETMISC` | 011 | Update config bits (Rounding, Saturation, etc.) |
| `WFI` | 111 | Wait For Interrupt (Low power / Doorbell) |

---

## 4. Scalar Floating-Point (FP16)
**Opcode:** `0001011` | **funct7:** `0001000`

| Instruction | funct3 | Description |
| :--- | :--- | :--- |
| `FADD` | 000 | `rd = rs1 + rs2` |
| `FSUB` | 001 | `rd = rs1 - rs2` |
| `FMUL` | 010 | `rd = rs1 * rs2` |
| `FMA` | 011 | `rd = (rs1 * rs2) + rs2_low16` (Current impl: rs2 low 16 bits as addend) |
| `FMIN` | 100 | FP Minimum |
| `FMAX` | 101 | FP Maximum |
| `FCVT.i2f` | 110 | Int32 (`rs1`) -> FP16 (`rd`) |
| `FCVT.f2i` | 111 | FP16 (`rs1`) -> Int32 (`rd`) |
| `FABS` / `FNEG` | macro | Via sign-bit macro-op |
| `FRECIP` | 000* | Reciprocal Approximation (*funct7=0001001*) |
| `FRSQRT` | 001* | Reciprocal Sqrt Approximation (*funct7=0001001*) |

---

## 5. Memory Access

### Scalar Memory
**Opcodes:** Load `0001100` | Store `0001101`

**Addressing Modes (funct7):**
- `0000000`: `base + imm12` (Default)
- `0000001`: `base + rs2`
- `0000010`: `pc + imm12`
- `0000011`: `base` (no offset)

**Data Size (funct3):**
- `000`: Byte (8-bit) - `LB`/`SB`
- `001`: Half (16-bit) - `LH`/`SH`
- `010`: Word (32-bit) - `LW`/`SW`

**Unsigned Loads:** `LBU`/`LHU` are not encoded as native LOAD variants in the tinyGPU scalar ISA. Tooling may emulate them (e.g., `LB`/`LH` followed by shifts) when translating from RV32.

### Atomic Memory
**Opcode:** `0010100` (Shares class with Raster/Geom, distinguished by funct7/funct3)
**Width:** 32-bit (mandatory).
**Return:** Old value in `rd`.

| Operation | funct3 | Description |
| :--- | :--- | :--- |
| `ADD` | 000 | Atomic Add |
| `MIN` | 001 | Atomic Signed Min |
| `MAX` | 010 | Atomic Signed Max |
| `XCHG` | 011 | Atomic Exchange |
| `CAS` | 100 | Compare and Swap (`rs2`=expected) |
| `AND` | 101 | Atomic AND |
| `OR` | 110 | Atomic OR |
| `XOR` | 111 | Atomic XOR |

---

## 6. Vector / Tensor ALU
**Opcode:** `0101111`
**Layout:** `funct6 | 1 | rs2 | rs1 | funct3 | rd | opcode`

**Element Types (funct3 / vf):**
- `000`: I32 (4 lanes)
- `001`: I16 (8 lanes)
- `010`: I8 (16 lanes)
- `011`: FP32 (4 lanes)
- `100`: FP16 (8 lanes)
- `101`: FP8 (E4M3, 16 lanes)

### Operations (funct6)
| funct6 | Mnemonic | Description |
| :--- | :--- | :--- |
| `000000` | `VADD` | Integer add (I32) / FP add (FP32/FP16/FP8) |
| `000001` | `VSUB` | Integer sub (I32) / FP sub (FP32/FP16/FP8) |
| `000010` | `VMIN` | FP min (FP32/FP16/FP8). (Integer min is reserved/implementation-defined.) |
| `000011` | `VCMP` / `VMAX` | **Overloaded by element type:** for integer element types (I32/I16/I8), this is `VCMP` producing a scalar lane-mask in `rd` (HW packs bits `[lanes-1:0]`). For FP element types (FP32/FP16/FP8), this is `VMAX` (vector result). |
| `000100` | `VDOT` | Dot Product & Widening (Detailed below) |
| `000101` | `VCROSS` | Cross Product (lanes 0-2) -> Vector `rd` |
| `000110` | `VSEL` | Vector Select (predicated blend). If `vm=1`, selects per lane using CSR `VMASK`: `mask?rs1:rs2`. If `vm=0`, implementation may use a scalar mask source (legacy/test mode). |
| `000111` | `VSWIZ` / `VPERM` | Swizzle/Shuffle (rs2 scalar mask holds packed 2-bit selectors) |
| `001000` | `VPACK` / `VPACKN` | Pack/Narrow. **Current RTL:** `VPACK` packs lanes 0..3 into a scalar `ARGB8888` (writes scalar `rd`). `VPACKN` is reserved. |
| `001001` | `VXOR` | Bitwise XOR on raw lane bits (honors `vm` predication). `funct3` selects lane width/type. |
| `001010` | `VAND` | Bitwise AND on raw lane bits (honors `vm` predication). `funct3` selects lane width/type. |
| `001011` | `VOR` | Bitwise OR on raw lane bits (honors `vm` predication). `funct3` selects lane width/type. |
| `001100` | `VMUL` | Element-wise multiply (I32/I16/I8 and FP32/FP16/FP8). |
| `001101` | `VRCP` | Reciprocal approximation (FP32/FP16/FP8). Unary: uses `rs1` only; ignores `rs2`. |
| `001110` | `VRSQRT` | Reciprocal sqrt approximation (FP32/FP16/FP8). Unary: uses `rs1` only; ignores `rs2`. |
| `001111` | `VUNPACK` | Unpack `ARGB8888` from a scalar payload into lanes 0..3 (writes vector `rd`). Payload is taken from scalar `rs2` (see notes). |

**FP Vector Math (current encoding):**
- For `funct3` = FP32/FP16/FP8, `funct6` selects: `000000` add, `000001` sub, `000010` min, `000011` max.

**Compare & Predication Note:**
- `VCMP` returns a scalar lane-mask in `rd`. To use it for predication (`vm=1`), software should write that mask into CSR `VMASK`.

**Dot Product & Widening (Crucial for AI/ML):**
- **VDOT.I8:** (funct6=`000100`, funct3=`010`) Treats operands as packed INT8. Performs 4-way dot product per 32-bit chunk. Result accumulated into INT32 destination.
- **VDOT.FP8:** (funct6=`000100`, funct3=`101`) Treats operands as packed FP8. Performs 2-way or 4-way dot product. Result widening to FP16 or FP32.
- **VEXT.I8:** Extract/Sign-extend INT8 lane to INT32.

**Pseudo Instructions:**
- `VMOV rd, rs1` -> `VADD rd, rs1, v0` (requires software to provide a zero vector; `v0` is not hardwired).
- `VCLR rd` -> `VXOR rd, rd, rd` (portable way to create a zero vector).

**Permute Operations (funct6=0001111, sub-selected by funct3):**
- `000`: Byte permute (within 32-bit chunks) - rs2 pattern per lane.
- `001`: Lane rotate.
- `010`: Shuffle halves.
- `011`: Broadcast Lane 0 to all.
- `100`: Pack Even lanes.

---

## 6.1 Notes: VMUL / VRCP / VRSQRT / VPACK / VUNPACK

### VMUL (Vector Multiply)
- **Encoding:** `OP_VEC_ALU`, `funct6=001100`.
- **Behavior:** element-wise multiply. Integer types wrap/truncate to the element width. FP32 uses a simplified IEEE-ish multiply (flushes subnormals). FP16/FP8 are supported via convert-to-FP32, multiply, convert back (approximate; subnormals flushed).

### VRCP (Reciprocal) and VRSQRT (Reciprocal Square Root)
- **Encoding:** `OP_VEC_ALU`, `funct6=001101` (`VRCP`) / `001110` (`VRSQRT`), `funct3=011` (FP32) / `100` (FP16) / `101` (FP8).
- **Unary:** reads `rs1` only; `rs2` is ignored.
- **Implementation intent:**
  - `VRSQRT` uses the classic `0x5f3759df` initial guess + 1 Newton-Raphson iteration.
  - `VRCP` uses a small mantissa LUT for the initial guess + 1 Newton-Raphson iteration.
  - **FP16/FP8 note:** current RTL computes the approximation in FP32 internally and converts back to FP16/FP8.

### VUNPACK / VPACK (Color conversions)
- `VUNPACK` is intended for color math/shading: takes a packed 32-bit `ARGB8888` value and expands to lanes `r,g,b,a`.
- **Current RTL convention:** the 32-bit payload is sourced from **scalar** register `rs2` (the core reads scalar `rs2` in parallel with vector `rs1/rs2`).
- Suggested workflow:
  - `VUNPACK.{FP32,FP16,FP8}` to get normalized 0..1 floats in lanes.
  - Perform lighting with `VMUL.{FP32,FP16,FP8}`.
  - `VPACK.{FP32,FP16,FP8}` to get a scalar `ARGB8888` for framebuffer stores.
- `101`: Pack Odd lanes.
- `110`: Unzip/Zip (even/odd split/merge).
- `111`: RESERVED (LUT Gather).

---

## 7. Vector Transfer & Memory

### Transfer Ops
**Opcode:** `0010000` (Reuses Vector ALU class)
- `VBCAST.s` (funct3=000): Broadcast scalar `rs1` to all lanes of `rd`.
- `VINS.s` (funct3=001): Insert scalar `rs1` into lane `imm3`.
- `VEXTR.s` (funct3=010): Extract lane 0 of `rs1` to scalar `rd`.

### Vector Memory
**Opcodes:** Load `0010001` | Store `0010010`

| Mode | funct3 | Description |
| :--- | :--- | :--- |
| `VLD.V` | 000 | Unit-stride. Prefer natural alignment. Unaligned allowed with penalty. |
| `VLD.S` | 001 | Strided. No coalescing hardware; acts as series of loads. |
| `VLDX` | 010 | Indexed / Gather. Serialized; low throughput. |
| `VLD.C` | 011 | Load from Constant/Uniform space. |

*(Stores share the same `funct3` mapping. Format conversion e.g. RGB565->FP16 handled by LSU/VPACK)*

### Vector Atomics
**Opcode:** `0010101`
Same `funct3` mapping as scalar atomics. Performed per-lane (Load -> RMW -> Store).

---

## 8. Texture & Sampler
**Opcode:** `0010011` | **funct3:** `000` (Nearest)

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

## 9. Graphics / Raster Ops
**Opcode:** `0010100` (Scalar I-Type)
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
  - Issue semantics: RSTATE/RSETUP/RDRAW/GSTATE/GDRAW issue as macro-ops into the fixed-function pipe. The core serializes until the graphics command slot is accepted.
  - Fence rule: A CP firmware barrier (WAIT_FENCE) ensures all prior graphics ops complete. `RDRAW` does NOT implicitly fence the core logic, allowing overlap, unless the command queue is full.

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
- `RSTATE`, `RRECT`, `GSTATE`, `GPARAM`, `GDRAW` are reserved for the full graphics pipeline and may currently retire without effect.

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
