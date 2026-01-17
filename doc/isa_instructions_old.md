# Instruction Reference (Draft)

Organized by class with encoding and usage. Opcodes/functs follow `isa.md`. Registers: scalar `s0..s31` (`s0`=0), floating-point `f0..f31` (FP16 values, impl may widen internally), vector `v0..v31` (128-bit, 4xFP32 native), optional constant `c0..c31` read-only. A cost-reduced build may drop to 16 entries, but the baseline assumes 32-wide files to relieve OSPI spill pressure. No implicit execution mask; masking is done with scalar masks plus `VSEL`/`VCMP`.

## Conventions & Addressing Modes
- Register fields are 5 bits (0–31 used). Instruction bits: `[31:0]` little-endian fetch. The opcode class selects which file the fields address (`s`, `f`, or `v`).
- Immediates: I-type imm[11:0] (signed), B-type/J-type per `isa.md` (PC-relative), U-type imm[19:0] for LUI/MOVI.
- Addressing modes for memory:
	- `base` (register only): addr = `rs1`.
	- `base+imm12`: addr = `rs1 + imm12` (signed).
	- `base+reg`: addr = `rs1 + rs2`.
	- `pc+imm12`: addr = `pc + imm12` (for position-independent data loads/stores).
	Modes are selected by funct3/funct7 sub-encodings listed below.

## Encoding Details (bit budget resolved)
- Scalar encodings stay R/I/B/J/U with 7-bit opcode.
- Vector-class opcodes keep a full 7-bit slot (`0101111` ALU/misc, `0010001` VLD, `0010010` VST, `0010011` TEX, `0010101` VATOM). Element type is encoded per opcode/funct; no `vtype` CSR or dynamic VL.
- Vector R-type shape (RISC-V style):
	- `[31:26]=funct6` (subop)
	- `[25]=reserved` (set to 1)
	- `[24:20]=rs2`, `[19:15]=rs1`, `[14:12]=funct3` (subclass / type selector), `[11:7]=rd`, `[6:0]=opcode`.
- Masking is explicit: `VCMP` writes a scalar mask; `VSEL` applies it.
- Type encoding: use `funct3` when an op needs explicit element type (e.g., F32/F16/I32 variants). VL is fixed by 128-bit width (4 lanes for FP32/INT32).

## Coherence & Multi-Core Model
- Four SIMD cores may run concurrently (one per tile). Global memory is coherent **between cores only** for atomics at cache-line granularity; non-atomic LD/ST are weakly ordered and see other-core writes after `MEMBAR` or kernel boundary.
- Host/MCU is **not coherent during kernel execution**. Handshake: host writes -> MEMBAR -> DISPATCH; GPU completes -> MEMBAR -> host reads. No snooping over OSPI.
- Atomics are globally ordered with respect to each other at a given address across GPU cores; scalar/vector LD/ST are not implicitly ordered with atomics unless `MEMBAR` is used.
- Texture path uses read-only caches; freshness vs. recent writes requires `MEMBAR` before sampling. Sampler descriptors follow the same rules.
- Shared/Scratch (`sm_base`) is per-core/per-block and not coherent across cores.

## SIMD Execution Model (tile renderer)
- Four independent cores; each core issues a single instruction stream to a 128-bit SIMD ALU (4xFP32 lanes native).
- No implicit `exec`/warp/reconvergence. Conditional work uses scalar masks produced by `VCMP` and applied with `VSEL`.
- Scoreboard hides TEX/LD/ST latency; overlap math to cover OSPI misses. No warp swapping—tile-level parallelism comes from the four cores.
- CSRs: `CORE_ID` (0–3) and `TILE_OFFSET` (X/Y origin for the current tile) for tile-based rendering flows.

## Scalar Integer
- **Encoding:** opcode `0001011` (R/I), funct3/funct7 per op.
- **ALU ops** (funct7=0000000 unless noted):
	- `ADD`/`ADDI` (funct3=000) — add rs1 + rs2/imm12.
	- `SUB` (funct7=0100000, funct3=000) — rs1 - rs2.
	- `AND/ANDI` (111), `OR/ORI` (110), `XOR/XORI` (100) — bitwise.
	- `SHL` (001), `LSR` (101), `ASR` (funct7=0100000, funct3=101) — shifts; shamt from rs2[4:0] or imm5.
	- `MIN` (010), `MAX` (011) — signed min/max.
	- `ABS` (funct7=0000000, funct3=001 with rs2=0 convention) — absolute; INT_MIN stays INT_MIN unless sat config.
	- `CLZ`/`CTZ` (funct7=0000011, funct3=000/001) — leading/trailing zero count.
- **Multiply/MAC** (funct7=0000001): `MUL.lo` (funct3=000), `MULH` signed/unsigned (001/010), `MAC` (funct3=011) rd += rs1*rs2.
- **Compare/Flags** (funct7=0000010): funct3 selects signed/unsigned/eq; sets Z/N/C/V; rd can take boolean result when funct3 requests.
- **Immediates/Upper:** `MOVI` (pseudo: LUI+addi) and `LUI rd, imm20` load upper 20 bits.
- **Branches/Jumps (opcode 0001110):**
	- `BEQ/BNE/BLT/BGE/BLTU/BGEU` (funct3 cond) — PC-relative B-type imm.
	- `JAL rd, imm20` — PC-relative J-type; rd gets link.
	- `JALR rd, rs1, imm12` — target = rs1+imm; rd gets link.
	- Branch imm is B-type: imm[12|10:5|4:1|11] << 1, sign-extended; target = pc + imm.
	- Conditions: BEQ (rs1==rs2), BNE (rs1!=rs2), BLT/BGE (signed <, >=), BLTU/BGEU (unsigned <, >=).
	- `JAL` target: J-type imm assembled from imm[20|10:1|11|19:12] << 1, sign-extended; rd=pc+4.
	- `JALR` target: (rs1 + imm12) & ~1; rd=pc+4.
- **System (opcode 0001111):** `MEMBAR` (funct3=000), `CSRRW` (001), `CSRRS` (010), `SETMISC` (011 config bits), `WFI` (111).

### Scalar Integer Encoding Table
| Instr | Opcode | funct7 | funct3 | Notes |
| --- | --- | --- | --- | --- |
| ADD | 0001011 | 0000000 | 000 | rd = rs1 + rs2 |
| ADDI | 0001011 | imm | 000 | imm12 signed |
| SUB | 0001011 | 0100000 | 000 | rd = rs1 - rs2 |
| AND / ANDI | 0001011 | 0000000 / imm | 111 | bitwise AND |
| OR / ORI | 0001011 | 0000000 / imm | 110 | bitwise OR |
| XOR / XORI | 0001011 | 0000000 / imm | 100 | bitwise XOR |
| SHL | 0001011 | 0000000 | 001 | logical left |
| LSR | 0001011 | 0000000 | 101 | logical right |
| ASR | 0001011 | 0100000 | 101 | arithmetic right |
| MIN | 0001011 | 0000000 | 010 | signed min |
| MAX | 0001011 | 0000000 | 011 | signed max |
| MUL.lo | 0001011 | 0000001 | 000 | low 32 bits |
| MULH | 0001011 | 0000001 | 001/010 | high signed/unsigned |
| MAC | 0001011 | 0000001 | 011 | rd += rs1*rs2 |
| CMP | 0001011 | 0000010 | 000/001/010 | sets flags; boolean optional |
| CLZ / CTZ | 0001011 | 0000011 | 000/001 | leading/trailing zeros |
| ABS | 0001011 | 0000000 | 001 (rs2=0) | absolute |
| MOVI | 0001011 | imm20 | n/a | pseudo (LUI+ADDI) |
| LUI | 0001011 | imm20 | n/a | load upper |

### Branch / Jump Encodings
| Instr | Opcode | funct3 | Notes |
| --- | --- | --- | --- |
| BEQ / BNE | 0001110 | 000 / 001 | B-type imm (PC-rel) |
| BLT / BGE | 0001110 | 100 / 101 | signed compare |
| BLTU / BGEU | 0001110 | 110 / 111 | unsigned compare |
| JAL | 0001110 | 011 | J-type imm; rd=link |
| JALR | 0001110 | 010 | I-type imm; rd=link; target=rs1+imm |

## Scalar FP16
- **Encoding:** opcode `0001011`, funct7=`0001000`, funct3 selects op. rd/rs1/rs2 name the `f` register file; conversion ops move between `f` and `s` files.
- **Ops:** `FADD`(000), `FSUB`(001), `FMUL`(010), `FMA`(011), `FMIN`(100), `FMAX`(101), `FCVT.i2f`(110), `FCVT.f2i`(111). Extras: `FABS`/`FNEG` (via sign-bit macro-op), optional `FRECIP/FRSQRT` (reserve funct7=0001001 block). Rounding/FTZ/sat per config/fstatus.

### Scalar FP16 Encoding Table
| Instr | Opcode | funct7 | funct3 | Notes |
| --- | --- | --- | --- | --- |
| FADD | 0001011 | 0001000 | 000 | fp16 add |
| FSUB | 0001011 | 0001000 | 001 | fp16 sub |
| FMUL | 0001011 | 0001000 | 010 | fp16 mul |
| FMA | 0001011 | 0001000 | 011 | fused mul-add |
| FMIN | 0001011 | 0001000 | 100 | fp16 min |
| FMAX | 0001011 | 0001000 | 101 | fp16 max |
| FCVT.i2f | 0001011 | 0001000 | 110 | int32 (rs1 scalar) -> fp16 |
| FCVT.f2i | 0001011 | 0001000 | 111 | fp16 (rs1 fp) -> int32 (scalar rd) |
| FMA | 0001011 | 0001000 | 011 | (rs1*rs2)+rs2_low16 (current impl uses rs2 low 16 bits as addend) |
| FABS / FNEG | 0001011 | 0001000 | macro | sign-bit ops |
| FRECIP / FRSQRT | 0001011 | 0001001 | 000 / 001 | optional approx |

## Scalar Memory
- **Opcodes:** LOAD `0001100`, STORE `0001101`.
- **funct3:** 000=byte, 001=half, 010=word (add 011 for LBU/LHU if enabled).
- **Addressing modes (selected by funct7 upper bits):**
	- `base+imm12` (default): funct7=0000000 — addr = rs1 + imm12.
	- `base+reg`: funct7=0000001 — addr = rs1 + rs2 (imm ignored).
	- `pc+imm12`: funct7=0000010 — addr = pc + imm12 (rs1 ignored, set to 0 by assembler pseudo).
	- `base` (no offset): funct7=0000011 — addr = rs1.
- **Loads:** `LB/LH/LW` sign-extend; `LBU/LHU` zero-extend (if 011 funct3 added).
- **Stores:** `SB/SH/SW` store low byte/half/word of rs2.
- Misaligned: allowed with penalty; fault flag optional.

## Atomic Memory
- **Opcode:** `0010100` (dedicated atomic class to keep LD/ST simple).
- **Width:** word (32-bit) mandatory; byte/half optional via funct3 if implemented.
- **Addressing:** base+imm12 (I-type) or base+rs2 (funct7=0000001) mirroring scalar LD/ST modes.
- **Ops (funct3):** 000=ADD, 001=MIN (signed), 010=MAX (signed), 011=XCHG, 100=CAS (rs2=expected), 101=AND, 110=OR, 111=XOR. Result returned in `rd` (old value) for all ops.
- **Semantics:**
	- Atomicity: read-modify-write completes indivisibly.
	- Ordering bits in funct7[2:1]: 00=relaxed, 01=acquire, 10=release, 11=acq_rel. Default relaxed.
	- Scope: global memory across GPU cores and host; coherent for atomics. Non-atomic accesses require `MEMBAR` to order with atomics.
- **CAS encoding:** rs2 holds expected, rs1 base; imm12 immediate forms may zero-extend expected via separate instruction variant if desired.

### Scalar Memory Encoding Table
| Instr | Opcode | funct7 (addr mode) | funct3 | Addr mode |
| --- | --- | --- | --- | --- |
| LB / LBU | 0001100 | 0000000 | 000 / 011 | base + imm12 |
| LH / LHU | 0001100 | 0000000 | 001 / 011 | base + imm12 |
| LW | 0001100 | 0000000 | 010 | base + imm12 |
| LB/LH/LW | 0001100 | 0000001 | 000/001/010 | base + rs2 |
| LB/LH/LW | 0001100 | 0000010 | 000/001/010 | pc + imm12 |
| LB/LH/LW | 0001100 | 0000011 | 000/001/010 | base (rs1) |
| SB/SH/SW | 0001101 | 0000000 | 000/001/010 | base + imm12 |
| SB/SH/SW | 0001101 | 0000001 | 000/001/010 | base + rs2 |
| SB/SH/SW | 0001101 | 0000010 | 000/001/010 | pc + imm12 |
| SB/SH/SW | 0001101 | 0000011 | 000/001/010 | base (rs1) |

## Vector/Tensor ALU
- **Encoding layout (opcode `0101111`):** `[31:26]=funct6 | [25]=1 | [24:20]=rs2 | [19:15]=rs1 | [14:12]=funct3 | [11:7]=rd | [6:0]=opcode`.
	- Types are encoded in funct6/funct3 (e.g., `.F32`, `.F16`, `.I32`).
	- **Element type map (funct3):** `000`=I32, `001`=I16, `010`=I8, `011`=FP16, `100`=FP8 (E4M3). All vector ALU/memory ops use this mapping.
- **Subops (funct6):**
	- 000000: `VADD`/logic/shift via funct3.
	- 000001: `VSUB` variants via funct3.
	- 000010: `VMIN/VMAX` via funct3.
	- 000011: `VCMP` -> mask bits replicated per lane; if `rd` is scalar, HW packs bits[lanes-1:0] of the comparisons into `rd`.
	- 000100: `VDOT` horizontal (4 lanes) -> scalar rd.
	- 000101: `VCROSS` (lanes 0–2 x,y,z) -> vector rd.
	- 000110: `VSEL` rd = (mask & rs1) | (~mask & rs2); mask comes from the scalar register named in the `rs2` field (bits[31:0] used; bits[lanes-1:0] select lanes).
	- 000111: `VSWIZ/VSHUF` using 2-bit selectors in the scalar mask (rs2) packed as `[1:0]=lane0 source, [3:2]=lane1, [5:4]=lane2, [7:6]=lane3`; lanes index 0–3 of `rs1`.
- **Behaviors:**
	- Lane ALU int/fp: add/sub/logic/shifts/min/max/abs, fp add/sub/mul/fma with widened accum for fp.
	- Dot: horizontal dot over 4 lanes to scalar rd (FP32/INT32).
	- Cross: compute 3D cross from lanes 0–2 of rs1/rs2 -> rd lanes 0–2.
	- Compare: `VCMP.{eq,lt,gt}` writes mask to scalar rd; combine masks in scalar space.
	- Select: `VSEL` uses scalar mask bits[3:0] (rs2 field) to blend rs1/rs2 operands per lane.

### Vector ALU Encoding Table (opcode 0101111)
| Subop | funct6 | funct3 | Notes |
| --- | --- | --- | --- |
| VADD/logic/shift | 000000 | 000-111 | add/and/or/xor/shl/shr/sar via funct3 |
| VSUB | 000001 | 000 | subtract |
| VMIN/VMAX | 000010 | 010/011 | signed min/max |
| VCMP | 000011 | 000-010 | compares -> scalar mask rd (bits[lanes-1:0]); vector mask also produced when rd is vector |
| VDOT | 000100 | 100 | horizontal dot -> scalar rd |
| VCROSS | 000101 | 101 | lanes 0–2 cross |
| VSEL | 000110 | 000 | blend using scalar mask in rs2 |
| VSWIZ/VSHUF | 000111 | 000-111 | pattern from rs2/imm |

### Pseudos
- `VMOV rd, rs1` is a pseudo for `VADD rd, rs1, v0` with `v0`=zero; preferred over manual add for readability.

## Scalar / Vector Transfer
- **Opcode:** `0010000` (reuses vector ALU class).
- **Subops:**
	- `VBCAST.s` (funct7=0001000, funct3=000): broadcast scalar `rs1` to all lanes of `rd` (per type encoding).
	- `VINS.s` (funct7=0001000, funct3=001): insert scalar `rs1` into a single lane of `rd`; lane index from imm3 in bits [14:12].
	- `VEXTR.s` (funct7=0001000, funct3=010): move lane0 of `rs1` into scalar `rd`.
- **Safety / ordering:** register-only; order against memory via MEMBAR if needed.

## Vector/Tensor Memory
- **Opcodes:** VLD `0010001`, VST `0010010`.
- **funct3:** 000=unit-stride, 001=strided (rs2 scalar stride bytes), 010=indexed/gather-scatter (rs2 index vector), 011=VLD.C (constant/uniform space). Reserve others for format-convert/bilinear later.
- **Addressing modes:** same as scalar but vectorized per lane. Unit-stride uses imm12 (in rs2 slot) + base rs1; if imm12 is insufficient, use `base+rs2` unit-stride form for a dynamic row pointer or the strided form. Strided uses base + lane*rs2 (rs2 scalar stride); indexed uses base+index+imm. pc+imm form allowed by setting funct7=0000010 (base ignored).
- Element type comes from opcode/funct; no predication bit.

### Vector Memory Encoding Table
| Instr | Opcode | funct3 | funct7 (addr mode) | Notes |
| --- | --- | --- | --- | --- |
| VLD.V | 0010001 | 000 | 0000000 | unit-stride, base+imm12 (imm in rs2 field) — HW uses lane_addr = base + lane*4 |
| VLD.V | 0010001 | 000 | 0000001 | unit-stride, base+rs2 |
| VLD.V | 0010001 | 000 | 0000010 | unit-stride, pc+imm12 |
| VLD.V | 0010001 | 000 | 0000011 | unit-stride, base (rs1) |
| VLD.S | 0010001 | 001 | 0000000 | strided: base + lane*rs2 (rs2 scalar stride) — implemented |
| VLDX | 0010001 | 010 | 0000000 | indexed/gather: base+index(rs2) — implemented (imm ignored for now) |
| VLD.C | 0010001 | 011 | 0000000 | load from constant/uniform space |
| VST.V | 0010010 | 000 | 0000000 | unit-stride store, base+imm12 — HW lane_addr = base + lane*4 |
| VST.V | 0010010 | 000 | 0000001 | unit-stride store, base+rs2 |
| VST.V | 0010010 | 000 | 0000010 | unit-stride store, pc+imm12 |
| VST.V | 0010010 | 000 | 0000011 | unit-stride store, base |
| VST.S | 0010010 | 001 | 0000000 | strided: base + lane*rs2 (rs2 scalar stride) — implemented |
| VSTX | 0010010 | 010 | 0000000 | indexed/scatter: base+index(rs2) — implemented (imm ignored for now) |

## Vector Atomics (per-lane RMW)
- **Opcode:** `0010101` (vector atomic class).
- **funct3:** 000=ADD, 001=MIN (signed), 010=MAX (signed), 011=XCHG, 101=AND, 110=OR, 111=XOR. CAS (100) currently behaves as XCHG.
- **Addressing:** unit-stride, strided, and indexed (base+index[lane]) forms are implemented; returns old value in `rd` vector.
- **Semantics (current HW):** serial per-core sequence: vector load -> per-lane RMW -> vector store. No acquire/release bits yet; assumes 32-bit lanes. Setting `rd=0` elides the return writeback.
- **Use cases:** Z-buffer depth test/update, histogram/counter updates without ROP.

## Texture/Sampler
- **Opcode:** `0010011`, funct3=000 TEX2D.nearest.
- **Fields:** `rd` = sample result vector, `rs1` = coords vector containing `{u, v, 0, 0}`, `rs2` = sampler handle.
- **Sampler handle modes:**
	- **Index mode (preferred):** `rs2` is an index 0–31 into a small on-chip sampler table preloaded by the host. No VRAM fetch on TEX, avoiding descriptor bandwidth.
	- **Pointer mode (optional):** if index mode is disabled, `rs2` is a pointer to a 32-byte descriptor in memory; hardware should cache it or expect higher latency.
- **Filtering:** funct3=000 is nearest only. Reserve other funct3 values for bilinear, mip bias, or format-convert loads.
- **Format conversion:** sampler format can be RGB565/ARGB8888/8bpp; hardware converts to rd format per opcode/funct.
- **Latency/ordering:** texture unit may be multi-cycle; scoreboard stalls dependent ops. Visibility ordered by MEMBAR if sampling data recently written by core/host.

### Sampler Descriptor (resolved)
- 32-byte descriptor (aligned 32 bytes) addressed by `rs2`:
	- 0x00: `base` (32-bit address of texture start)
	- 0x04: `stride` (bytes per row)
	- 0x08: `width` (u extent, in texels)
	- 0x0C: `height` (v extent, in texels)
	- 0x10: `format` (bits: [1:0] 0=RGB565,1=ARGB8888,2=PAL8; [3:2] palette/LUT idx; [5:4] swizzle; [7:6] reserved)
	- 0x14: `wrap` (bits: [1:0] u wrap 0=clamp,1=repeat; [3:2] v wrap 0=clamp,1=repeat; [7:4] reserved)
	- 0x18: `filter` (bits: [1:0] 0=nearest,1=bilinear (future); [7:2] reserved)
	- 0x1C: `misc` (palette base address index or LUT handle; reserved otherwise)
- `rs2` holds the descriptor address; descriptor is read via vector memory path (coherent after MEMBAR).

### Sampler Preload Flow
- Host/MCU preloads up to 32 descriptors into the sampler table (indices 0–31) before dispatch via VST/CSRRW or dedicated sideband. TEX2D index mode consumes these without VRAM fetches. Pointer mode remains for fallback but may stall if uncached.

### Texture Latency / Cores
- OSPI latency can be hundreds of cycles on a miss. To hide this, keep multiple tiles in flight across the four cores and sufficient outstanding TEX/LD depth per core; scoreboard must track outstanding operations.

### Texture Coordinates (resolved)
- Default: integer texel coordinates (u,v) in lanes 0–1 of `rs1`; out-of-range handled by wrap/clamp per descriptor.
- Optional normalized fixed: if config bit `tex_norm` is set, coords are Q0.16 fixed in lanes 0–1; hardware multiplies by width/height then floors.
- Out-of-range: clamp uses edge texel; repeat wraps mod width/height.

## Raster / Render Ops
- **Opcode class:** `0010100` (shared with geometry/atomics); scalar-format instructions.
- **Instruction type:** scalar I-type (rd, rs1, imm12). `rs1` is a pointer to a descriptor; `imm12` is usually 0; `rd` optionally returns a token/state ID.
- **Role:** fixed-function ROP path (triangle/rect), depth test, and blend, analogous to a modern ROP unit fed by upstream geometry.

### Raster Opcode Map (funct3 under opcode 0010100)
| Instr  | funct3 | Type | Notes |
| ---    | ---    | --- | --- |
| RSTATE | 000    | I-type | Load render state descriptor (fb/depth/viewport/blend/sampler); rd may return state ID/token |
| RSETUP | 001    | I-type | Load 3-vertex block `{x,y,z,w,u,v,color}`; computes edges/barycentrics and enqueues primitive |
| RDRAW  | 010    | I-type | Kick triangle using descriptor or last setup; flags in descriptor select textured/depth/blend |
| RRECT  | 011    | I-type | Axis-aligned rect fill/blit using descriptor; supports color or textured blit |

### Raster Descriptors
- **State (RSTATE rs1=ptr):** framebuffer base/stride/format, optional depth base/stride/format, viewport/scissor, blend mode (opaque/alpha/additive), depth test enable/function, sampler/texture indices.
- **Vertex block (RSETUP rs1=ptr):** 3 vertices `{x,y,z,w,u,v,color}` (fp32 positions/uv, ARGB8888 color).
- **Triangle/Rect (RDRAW/RRECT rs1=ptr):** references vertex block pointer + state ID and flags (tri/rect, textured, depth enable, blend select).

### Raster Pipeline Behavior
- Stages: edge eval -> coverage -> barycentric interp (color/UV/z) -> optional depth test -> blend (opaque/alpha/additive) -> writeback.
- Textured draws reuse TEX unit; sampler index comes from state descriptor.
- Raster is a VRAM arbiter client; display QoS remains enforced.

### Raster Ordering & Visibility
- Fire-and-forget; completion ordered via MEMBAR or command-processor fence.
- Inputs (vertices/descriptors/textures) must be written before RSETUP/RDRAW; use MEMBAR to order.
- Outputs (color/depth) visible to host/CU after completion; MEMBAR before host readback or CU sampling.
- Command fences: protocol flags can wait on or emit a fence token before/after RDRAW/RRECT submission; use with MEMBAR to ensure host/CU visibility.

## Geometry / Vertex Pipeline (modernized)
- **Goal:** mirror a modern graphics pipeline pre-ROP: vertex fetch, transform, lighting, cull/clip, and assembly before handoff to raster.
- **Opcode class:** reuse `0010100` with additional funct3 values; scalar I-type (rd, rs1, imm12=descriptor ptr).
- **Pipeline alignment with modern GPUs:**
	1) **Vertex Fetch/Format:** read vertex buffers (position/normal/uv/color) with stride/format conversion.
	2) **Vertex Transform:** apply model-view-projection; optional normal matrix.
	3) **Lighting (fixed-function light0):** directional + ambient; optional spec power from constants.
	4) **Primitive Assembly:** gather 3 verts via index buffer; support tri-list/tri-strip; face cull (CW/CCW/none).
	5) **Clip / Guard-band:** clip against frustum (+ optional user plane); perspective divide; viewport/scissor; emit to raster in screen space.
	6) **Handoff:** push assembled screen-space tri (plus varyings) to raster; depth precomputed z/w carried through.
- **CU-assist path:** GDRAW flag can route vertices through a CU microkernel (programmable vertex shader) that writes a transformed vertex buffer the fixed pipeline consumes (vs. fully fixed-function path above).

### Geometry Opcode Map (funct3 additions under opcode 0010100)
| Instr  | funct3 | Type | Notes |
| ---    | ---    | --- | --- |
| GSTATE | 100    | I-type | Load geometry state (VBO/IBO pointers, strides, MVP/normal matrices, lighting params, cull/clip enables); rd optional state ID |
| GPARAM | 101    | I-type | Load per-draw params (model matrix/material color) to avoid rebinding GSTATE |
| GDRAW  | 110    | I-type | Issue draw: `{vtx_base, idx_base, count, topology (tri-list/strip), state_id, param_id, flags}`; flags include cull mode, clip enable, CU-assist |

### Geometry Descriptors & Formats
- **GSTATE:**
	- `vbo_base`, `vbo_stride`, `index_base`, `index_type` (u16/u32).
	- `mvp` (4x4 fp32), `normal_mat` (3x3 fp32 optional), `light_dir`, `light_color`, `ambient`.
	- `cull` (none/CW/CCW), `clip_enable`, `viewport/scissor` (shared with RSTATE if desired).
- **GPARAM:** compact per-draw block: model matrix or model*view (optional), material color, gloss/spec power.
- **GDRAW:** count/offsets/topology, references to `state_id`/`param_id`; flag for CU-assist vs fixed-function.
- **Vertex layout:** position (x,y,z,w) fp32; normal (nx,ny,nz) fp32; uv fp32 or fp16; color ARGB8888; stride supplied in descriptor.

### Descriptor Sizes / Alignment (host toolchains)
- `RSTATE`: 64 bytes, align 32B. Contains fb/depth bases/strides/formats, viewport/scissor, blend+depth func, sampler indices.
- `RSETUP` vertex block: 3 vertices * 32 bytes = 96 bytes, align 32B.
- `RDRAW`/`RRECT` descriptors: 16 bytes, align 16B (state_id, vtx_ptr/rect, flags).
- `GSTATE`: 128 bytes, align 64B. Includes VBO/IBO pointers/strides, matrices, light0 params, cull/clip bits, viewport/scissor.
- `GPARAM`: 64 bytes, align 32B. Model or model*view, material color, gloss/spec.
- `GDRAW`: 24 bytes, align 16B. `{vtx_base, idx_base, count, topology, state_id, param_id, flags}`.

### Geometry Ordering & Synchronization
- Vertex/Index buffers must be populated before GDRAW; MEMBAR to order host/CU writes.
- If CU-assist is used, the CU microkernel must complete writing the transformed VBO before GDRAW; fence/MEMBAR between stages.
- Color/depth visibility follows raster rules after handoff.
- Command fences: GDRAW may be queued with wait/emit flags (from command protocol) to enforce host/GPU ordering between state loads, vertex transforms, and draws.

### CU-Assist Vertex Microkernel ABI
- **Invocation:** Host sets `arg_base` to a small argument block and issues a compute dispatch for the microkernel; GDRAW flag tells geometry to consume the transformed buffer it produces.
- **Arg block layout (32B, align 16B):**
	- 0x00 `in_vbo` (addr), 0x04 `out_vbo` (addr), 0x08 `count` (verts), 0x0C `stride` (bytes in/out, assumed equal),
	- 0x10 `mvp_ptr` (addr), 0x14 `normal_ptr` (addr or 0), 0x18 `param_ptr` (addr to GPARAM-like material), 0x1C `flags` (bit0: has_normals, bit1: apply_light0).
- **Register/ABI expectations:**
	- `arg_base` CSR points at the block; kernel loads addresses/flags from there.
	- Kernel writes transformed vertices to `out_vbo` in-place, same stride/layout as geometry expects (position/normal/uv/color).
	- When complete, kernel issues `MEMBAR` so geometry sees the buffer; host/command processor sequences GDRAW after the kernel fence.

## Control / System
- **Opcode:** `0001111`.
- `MEMBAR` (funct3=000) — orders memory vis-à-vis display fetch, host OSPI, and non-atomic accesses around atomics; use before buffer swap or after writes the host will read.
- `CSRRW/CSRRS` (001/010) — CSR read/modify/write. Key CSRs: `CORE_ID` (0–3), `TILE_OFFSET` (tile X/Y), `status` (int/fp flags), `config` (rounding, sat), timers/perf counters, interrupt masks/status.
- `SETMISC` (011) — shorthand to update config bits (rounding mode, saturate default, predicate invert, FTZ enable).
- `BARR.sync` (funct3=100) — in-core barrier when software needs ordering.
- `WFI` (111) — optional low-power wait for interrupt/doorbell.
- `DISPATCH` — host-issued command (not an ISA op) to launch kernels; included for completeness.

### Exception / Ordering (resolved)
- Misaligned scalar/vector loads/stores: allowed with penalty; set a sticky misalign flag in `status`; no trap by default.
- FP exceptions: set flags in `fstatus` (NV, DZ, OF, UF, NX). No traps; behavior is flag+propagate NaNs. Signaling NaNs quieted; min/max follow IEEE (snan quiet then compare).
- Integer overflow wraps unless sat mode set; saturating converts clamp to min/max representable.
- Atomics are globally ordered at their address; acquire/release bits control ordering with surrounding accesses. Non-atomic accesses need `MEMBAR` to order with atomics across cores/host.

## Kernel ABI & Built-ins (SIMD tile)
- Entry state: `status`/`fstatus`/`vstatus` cleared, scalar regs undefined except `s0`=0, fp regs (`f` file) undefined. PC from dispatch packet.
- Built-in CSRs (read-only unless stated):
	- `core_id` (0..3), `tile_offset` (x,y of tile origin for this core).
	- `arg_base` (pointer to kernel arg blob), `arg_size`.
	- `sm_base`/`sm_size` (shared/scratch region) if scratch SRAM exists.
	- `const_base`/`const_size` for read-only uniform data.
	- `time_lo/hi` cycle counter (optional).
- Kernel args: packed by host; use scalar loads from `arg_base` to pull parameters; constant data can be staged in constant memory and accessed via `VLD.C`.
- Dispatch packet (host -> core): PC, tile grid, arg_base, arg_size, shared_size, priority. `DISPATCH` is host-side, not an ISA instruction.

## Memory Model & Spaces
- Global: default space for scalar/vector LD/ST/TEX; coherent with host after MEMBAR or dispatch boundary.
- Shared/Scratch (optional): per-block region at `sm_base` with size `sm_size`; use normal LD/ST with base in that window; `BARR.sync` + MEMBAR for ordering.
- Constant: read-only region at `const_base/size`; cached read path; faults if written.
- Texture: uses TEX opcode and sampler descriptor; sampler reads through texture path (separate from scalar/vector cache hierarchy).
- Atomics: coherent and ordered at their address across GPU cores; host is non-coherent during kernels and observes results after MEMBAR and completion.
- Ordering: intra-warp is in-order; cross-warp visibility requires MEMBAR; host visibility requires MEMBAR or kernel completion.

## Blending / Z Path Note
- For transparent blending or depth tests, prefer `VATOM.add`/`VATOM.min` to update VRAM directly when ROP hardware is absent.
- Optionally, a fixed-function ROP after shader retire can handle blend/depth without ISA atomics; if omitted, the VATOM path is the correctness mechanism.

## Sampler Descriptor Example
```c
struct Sampler32 {
	uint32_t base;    // texture start
	uint32_t stride;  // bytes per row
	uint32_t width;   // texels in u
	uint32_t height;  // texels in v
	uint8_t  format;  // see format bits above
	uint8_t  wrap;    // u/v wrap bits
	uint8_t  filter;  // 0=nearest
	uint8_t  pad;
	uint32_t misc;    // palette/LUT handle
};
```
Descriptor is 32B aligned; additional padding is reserved for future mip level pointers.

### Stride Handling (resolved)
- Unit-stride VLD/VST use imm12; if imm12 is too small, `vstride` CSR supplies a larger base stride (imm12 adds to it). The unit-stride base+rs2 form can also carry a dynamic row pointer when software increments rs2 each row. Strided VLD.S/VST.S always use rs2 as scalar stride; `vstride` CSR is ignored there. Indexed ops use base+index+imm. For very wide rows (e.g., bitmaps >4KB stride), use either `vstride`+imm12, the unit-stride base+rs2 form, or the strided form with rs2 holding the stride.

### Permute/Pack Encodings (resolved)

## Remaining Reserved Slots
- Vector permute funct7=0000111/funct3=111 kept for LUT gather.
- Texture funct3>000 reserved for bilinear/mip features.
- System opcode funct3=101/110 reserved for future power/debug controls.
- `VPERM` funct3 usage inside funct7=0000111:
	- funct3=000: byte permute within 32-bit chunks (rs2 provides pattern per lane)
	- funct3=001: lane rotate (by rs2 scalar low bits)
	- funct3=010: shuffle low/high halves
	- funct3=011: broadcast lane0 to all
	- funct3=100: pack even lanes
	- funct3=101: pack odd lanes
	- funct3=110: unzip/zip (even/odd split/merge)
	- funct3=111: reserved for future LUT-based gather

## Defaults & Modes
- Predication: no implicit mask; use `VCMP` -> scalar mask + `VSEL` for conditional blends.
- Saturation: controlled by config/vstatus; int narrows can sat; fp FTZ/sat configurable.
- Rounding: fstatus/config hold rm for fp/cvt.
- Visibility: framebuffer writes ordered by MEMBAR or dispatch boundaries.

## Open TBDs
- Document funct3 subpatterns for future vector LUT gather (permute funct3=111) and texture bilinear/mip modes.
- Allocate CSR indices for `sm_base/size`, `const_base/size`, `vstride`, and counters; keep contiguous block for simplicity.
- Decide whether VATOM supports fp16 add (for blend) or stays integer-only; if fp16 is added, define rounding/saturation and type tag.
