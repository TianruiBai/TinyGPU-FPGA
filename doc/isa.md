# Simple GPGPU ISA Concept

## Goals
- SIMD-first ISA for four independent cores in a tile-based renderer on ESP32S3 + FPGA over OSPI.
- Fixed register files: 32 scalar `s0–s31` (32-bit), 32 floating-point `f0–f31` (hold FP16 operands, optionally widened internally), 32 vector `v0–v31` (128-bit: 4xFP32 / 8xFP16 / packed int), optional 32 constant `c0–c31` (128-bit read-only uniforms). A cost-reduced implementation may halve these to 16, but the baseline assumes 32 to reduce OSPI spills.
- Support raster ops, simple compute, and framebuffer post effects; external QSPI/OSPI PSRAM as VRAM; four cores process four tiles concurrently.
- Latency strategy: explicit SIMD. Latency hiding relies on ILP within a core and four tiles in flight; long TEX/VRAM misses will stall a core. Scoreboard tracks up to 32 entries; TEX queue depth is 8.
- Preemption/watchdog: firmware running on the RISC-V command processor enforces a watchdog/abort path for hung kernels/G-pipe ops (e.g., TDR > 2 sec) to reset the tile.

## Register Files
- Scalar: `s0`–`s31`, 32-bit (with `s0` hardwired zero).
- Floating-point: `f0`–`f31`, hold FP16 values (implementation may store as FP16 or widened). No hardwired zero.
- Vector: `v0`–`v31`, 128-bit each. Native view: 4xFP32 or packed FP16/int variants for ALU.
- Constant (optional but recommended): `c0`–`c31`, 128-bit read-only, mapped to constant/uniform memory (e.g., bone matrices).
- Special: `pc`, `flags` (int), `fstatus` (fp), `vstatus` (vector sat/ovf), `config`, `csr_core_id` (0–3), `csr_tile_offset` (tile X/Y), `vmask` (4-bit predicate). Element type is encoded per opcode/funct.

## Data Formats
- Scalar int: 32-bit.
- Scalar fp: FP16 in the `f` register file (rounding selectable via status), optional widening internal ops.
- Vector/tensor: INT32/INT16/INT8/FP32/FP16/FP8; accumulators up to 32-bit for int and FP16/FP32.
- Color/texture: RGB565, ARGB8888, 8bpp paletted (conversion handled by load/store/texture units).

## Instruction Widths & Formats
- Scalar int/fp instructions: 32-bit words.
- Vector/tensor instructions: 32-bit words, R-type shape: `[31:26]=funct6 | [25]=reserved(1) | [24:20]=rs2 | [19:15]=rs1 | [14:12]=funct3 | [11:7]=rd | [6:0]=opcode`. Element type is encoded by funct3/funct6 (e.g., F32/F16/I32). VL is fixed by the 128-bit width (4 lanes at FP32/INT32).
- Addressing: 32-bit addresses; immediates typically 12–16 bits.
- Endianness: little.

## Instruction Types (encoding shapes)
- Regular (register): rd, rs1, rs2 with opcode+funct fields; used by most ALU (int/fp) and vector lane ops. Example shape: `opcode | funct | rd | rs1 | rs2`.
- Immediate: rd, rs1, imm (12–20 bits depending on op); used for MOVI/LUI, add-immediate variants, and branches (offset immediates). Example shape: `opcode | funct | rd | rs1 | imm[11:0]` or `rd | imm[19:0]` for long immediates.
- Memory: rd/rs2, base=rs1, offset immediate (typically 12 bits signed). Scalar LD/ST use this; vector LD/ST use base + stride or indexed forms with a stride field and lane scaling. Example shape: `opcode | funct | rd(rs2 for store) | rs1(base) | imm[11:0]`.

Notes
- Branches use immediate encoding with PC-relative offsets; jumps use rd as link (JAL/JALR).
- Vector/tensor ops share the same 32-bit width; element type is per opcode/funct. Predication uses `vm`+`vmask`; no dynamic VL.
- Texture sample uses a dedicated opcode with fields for sampler/format plus rd/rs operands.

## Vector Instruction Encoding
- R-type: `[31:26]=funct6 | [25]=vm(mask_enable) | [24:20]=rs2 | [19:15]=rs1 | [14:12]=funct3 | [11:7]=rd | [6:0]=opcode`.
- Types are baked into funct3/funct6 (e.g., `.F32`, `.F16`, `.I32`).
- Predication: If `vm=1`, the operation is predicated by the `vmask` register (bits [3:0] correspond to lanes). `vmask` is updated by `VCMP`.
- Vector LD/ST:
  - `VLD.V`: opcode `0010001`, funct3=000 unit-stride using imm12; if imm12 overflows, use unit-stride base+rs2 or strided form.
  - `VLD.S`: funct3=001 strided, rs2 holds scalar stride in bytes.
  - `VLDX`: funct3=010 indexed/gather, rs2 vector index.
  - `VLD.C`: funct3=011 load from constant/uniform memory into vector or scalar.
  - Stores mirror loads under opcode `0010010`.

- Dot: `VDOT` horizontal over 4 lanes (FP32/INT32) -> scalar rd.
- Multiply: `VMUL` element-wise (FP32/FP16/FP8 + integer types).
- SFU: `VRCP` (reciprocal) and `VRSQRT` (reciprocal sqrt) for FP32/FP16/FP8.
- Cross product: `VCROSS` over lanes 0–2 (FP32/INT32) -> vector rd.
- Permute/shuffle: `VSWIZ/VSHUF` for component swizzle/shuffle (e.g., XYZW via 8-bit imm).
- Mask/predicate ops: `VCMP.*` writes a 4-bit mask into scalar rd; used with `VSEL` for control.
- Select/blend: `VSEL` rd = (mask & rs1) | (~mask & rs2), mask from scalar bits[3:0].

Encoding budget (rough):
- Reserve 1–2 opcodes for vector ALU/dot/misc, 1 opcode for vector loads, 1 for vector stores, 1 for texture/sample. Keep funct space for future bilinear/format-convert loads.

## Encoding Details (proposed numeric)
### Global
- Instruction word: bits [31:0]. Little endian fetch.
- Register fields: rd/rs1/rs2 are 5 bits (00–31 used; no banking). The active register file (scalar `s`, fp `f`, vector `v`, constant `c`) is implied by the opcode class.
- Scalar immediates: I-type imm[11:0] sign-extended; JAL imm[20|10:1|11|19:12]; B-type imm[12|10:5|4:1|11].

### Opcode map (7-bit opcode)
- `0001011` INT/FP ALU (R-type register-register ops; funct3/7 select op)
- `0010111` INT_IMM (I-type register-immediate integer ALU; funct3 selects op; inst[31:25] is immediate bits except for shift-immediates)
- `0110111` LUI (U-type upper immediate)
- `0001100` LOAD (scalar)
- `0001101` STORE (scalar)
- `0001110` BRANCH/JUMP (BEQ/BNE/BLT/BGE/JAL/JALR via funct3)
- `0001111` SYSTEM (CSR, MEMBAR, WFI)
- `0101111` VECTOR-ALU/DOT/MISC (RVV-compatible opcode)
- `0010001` VECTOR-LOAD
- `0010010` VECTOR-STORE
- `0010011` TEXTURE/SAMPLER
- `0010100` GRAPHICS/RASTER/ATOMICS (raster/geometry/atomics share class; funct3 selects op)

### Scalar funct3/funct7 (opcode 0001011 unless noted)
- INT ALU (funct7=0000000): ADD=000, SUB (funct7=0100000, funct3=000), AND=111, OR=110, XOR=100, SHL=001, LSR=101, ASR (funct7=0100000, funct3=101), MIN=010, MAX=011.
- INT_IMM ALU (opcode 0010111): same funct3 meanings as INT ALU for immediate forms (ADDI/ANDI/ORI/XORI/etc). For shifts, inst[31:25] distinguishes SRLI vs SRAI.
- MUL: funct7=0000001, funct3=000 MUL (signed multiply, low 32 bits). (No MULI.)
- CMP sets flags: funct7=0000010, funct3 chooses signed/unsigned/eq.
- FP16: funct7=0001000 block, funct3: FADD=000, FSUB=001, FMUL=010, FMA=011, FMIN=100, FMAX=101, FCVT.i2f=110, FCVT.f2i=111.
- SYSTEM (opcode 0001111): funct3=000 MEMBAR, 001 CSRRW, 010 CSRRS, 111 WFI.
- BR/JAL (opcode 0001110): funct3 encodes cond; JAL uses dedicated pattern (rd=link, imm20 form); JALR uses rs1+imm.
- LOAD (opcode 0001100): funct3=000 LB, 001 LH, 010 LW.
- STORE (opcode 0001101): funct3=000 SB, 001 SH, 010 SW.

### Vector fields (opcode 0010000 ALU/DOT/MISC)
- Bit layout: `[31:26]=funct6 | [25]=vm | [24:22]=vf | [21:17]=rs2 | [16:12]=rs1 | [11:7]=rd | [6:0]=opcode`.
- vf: 000=i8, 001=i16, 010=i32, 011=fp32, 100=fp16, 101=fp8.
- Predication: `vm` gates lanes using `vmask` CSR bits[3:0].
- VL: Fixed at 128-bit width (4x32b, 8x16b, 16x8b).
- funct7 subops (examples):
  - 0000000 VADD/logic/shift selected by funct3 (taken from bits [14:12]).
  - 0000001 VSUB/neg variants (funct3 differentiate).
  - 0000010 VMIN/VMAX.
  - 0000100 VDOT (funct3=dot); reduce bit in funct7[0] (0=vector accumulate, 1=reduce to scalar rd).
  - 0000101 VCROSS (funct3=cross=101) lanes 0–2.
  - 0000110 VREDUCE (add/max/min via funct3).
  - 0000111 VPERM/VSHUF (pattern via funct3 + small imm in rs2 or secondary encoding).

### Vector load/store
- VLD (opcode 0010001): `funct3=000` unit-stride, uses imm[11:0] in place of rs2 (rs2 field repurposed as imm). `vf` selects element size for lane decode. Stride field can be imm or separate stride CSR.
- VST (opcode 0010010): same layout; rs2=source vector, rd unused (set 0). `funct3=000` unit-stride; future funct3 for indexed/gather.
- Indexed VLDX/VSTX: `funct3=001`, rs2 holds index vector, imm adds base offset.

### Texture (opcode 0010011)
- TEX2D.nearest: funct3=000. rd = result vector, rs1 = coord vector (expects {u,v,0,0}), rs2 = sampler/descriptor handle.

### Specials
- VSEL: vector select/blend using scalar mask bits (rs2 field points to scalar register carrying mask; bits[lanes-1:0]).
- SETMISC (mode): opcode 0001111, funct3=011 writes config bits (rounding mode, sat default, pred invert).

### Scalar Integer (examples)
- ALU: ADD, SUB, ADC/SBC, AND/OR/XOR/NOT, SHL/LSR/ASR, MIN/MAX, ABS, CLZ/CTZ, CMP.
- MUL: MUL.lo, MULH (signed/unsigned), MAC (accumulate).
- Moves/immediates: MOV, `LUI` (upper imm); `MOVI` is a pseudo-op typically expanded as `LUI` + `ADDI`.
- Branch/jump: BEQ/BNE/BLT/BGE/BLTU/BGEU, JAL, JALR.
- Memory: LD/ST byte/half/word with imm offset.
- System: CSRRW/CSRRS (special regs), MEMBAR/FENCE, WFI (optional).

### Scalar FP16 (examples)
- ALU: FADD, FSUB, FMUL, FMA, FMIN, FMAX, FABS, FNEG.
- Convert: FCVT.f2i, FCVT.i2f, FCVT.f2f (rounding mode field).
- Compare: FCMP {eq, lt, le} sets int flags or predicate; conditional branch BF{cond}.
- Optional approx: FRECIP, FRSQRT (fast).

### Vector (examples)
- Lane ALU: VADD/VSUB/VAND/VOR/VXOR/VSHL/VSHR/VSAR/VABS/VMIN/VMAX with type suffixes {.F32,.F16,.I32} encoded in funct.
- Dot: VDOT (horizontal over 4 lanes) -> scalar rd (F32/I32).
- Cross: VCROSS over lanes 0–2 -> vector rd (F32/I32).
- Swizzle/shuffle: VSWIZ/VSHUF using packed 2-bit selectors from scalar mask (rs2) for lanes 0–3; future imm/LUT forms reserved.
- Select/blend: VSEL using scalar mask bits[3:0]; VCMP.{eq,lt,gt} writes mask to scalar rd.
- Pack/extend: VEXT (sx/zx), VPACK/VPACKN (narrow with saturate), VUNPACK (expand ARGB8888 -> vector lanes).
- Vector atomics (optional): per-lane RMW (add/min/max/xchg/cas/and/or/xor); rd=0 elides return value.

### Memory & Texture
- Scalar LD/ST: LD/ST.b/h/w with imm12 offset; misaligned allowed with penalty.
- Vector LD/ST: VLD.V/VST.V with stride; VLDX/VSTX indexed (base + lane*stride or base + idx[lane]).
- Constant load: VLD.C from constant/uniform space into vector or scalar.
- Format convert on load/store selectable (e.g., RGB565 -> FP16 triplet lanes, or paletted -> RGB).
- Texture sample: TEX2D.nearest rd, coords, sampler_id; rs1 coords explicitly contain {u,v,0,0}; sampler_id indexes preloaded sampler table (pointer mode optional); bilinear can be added later.

### Control/Dispatch
- DISPATCH pc, grid_xy, args_ptr: launch kernel (host issues via command processor).
- BARRIER: Tile-local synchronization (all cores in tile must arrive).
- MEMBAR/FENCE: order vis-à-vis display/host.
- CSRs: CORE_ID (0–3), TILE_OFFSET (tile X/Y for current core), config/status.

## Graphics / Raster Summary
- Opcode class `0010100` hosts both geometry and raster control (scalar I-type; rs1=descriptor ptr, rd optional token/state ID, imm12 usually 0).
- **Geometry (funct3 100–110):**
  - `GSTATE`(100): load geometry state (VBO/IBO pointers/strides, MVP/normal matrices, light0 params, cull/clip enables).
  - `GPARAM`(101): per-draw params (model matrix/material) to avoid rebinding GSTATE.
  - `GDRAW`(110): issue draw `{vtx_base, idx_base, count, topology tri-list/strip, state_id, param_id, flags(cull/clip/CU-assist)}`.
- **Raster (funct3 000–011):**
  - `RSTATE`(000): load render state (fb/depth base+stride+format, viewport/scissor, blend, depth func, sampler indices).
  - `RSETUP`(001): load 3-vertex block `{x,y,z,w,u,v,color}` and compute edges/barycentrics.
  - `RDRAW`(010): kick triangle using descriptor or last setup; flags select textured/depth/blend.
  - `RRECT`(011): axis-aligned rect fill/blit (color or textured).
- **Atomics (funct3 000–111 when opcode interpreted as atomic class):** existing scalar atomics share the opcode; decoder distinguishes by funct7/funct3.
- Pipeline alignment to modern GPUs: vertex fetch/format -> MVP/normal transform -> light0 (dir+ambient) -> primitive assembly (tri-list/strip, cull) -> clip/guard-band -> viewport/scissor -> handoff -> raster (edge, barycentric, depth, blend) -> writeback.
- Ordering: host/CU data feeding geometry/raster must be ordered with `MEMBAR`; outputs visible after completion or `MEMBAR`/dispatch boundary.
- Fences: command protocol supports emit/wait flags; waits delay submission until a named fence is satisfied, emits publish completion. Use with MEMBAR for host/GPU visibility.
- VRAM client: geometry/raster use the VRAM arbiter; TEX unit reused for textured draws.
- Fragment shading model: default path is interpolated color from vertices. Programmable per-pixel shading is done via a compute pass over a G-buffer/tile produced by RDRAW (two-pass shading); no dedicated fragment opcode. Use DISPATCH + VATOM/ROP for blend.
- Preemption/watchdog: firmware running on the RISC-V command processor should enforce a watchdog/abort path for hung kernels to avoid a wedged tile; add a stop/doorbell path in CP firmware.

## Minimal Opcode Set to Start
- Scalar int: MOVI, ADD, SUB, AND, OR, XOR, SHL, ASR, MUL, MULH, MAC, CMP, BEQ/BNE/BLT/BGE, JAL/JALR, LD/ST b/h/w.
- Scalar fp16: FADD, FMUL, FMA (current HW uses rs2 low 16 bits as addend), FMIN, FMAX, FCVT.i2f (rs1 scalar), FCVT.f2i (rd scalar), FCMP, BF{cond}.
- Vector: VADD.{F32,F16,I32}, VSUB, VMUL, VAND, VOR, VSHL, VSHR, VMIN, VMAX, VRCP/VRSQRT (FP32/FP16/FP8), VDOT (horizontal to scalar), VCROSS, VSWIZ/VSHUF (scalar mask selectors), VSEL (mask), VCMP.{eq,lt,gt}->{scalar mask rd}, VUNPACK/VPACK (color expand/pack), VATOM add/min/max/xchg/and/or/xor.
- Texture: TEX2D.nearest (coords explicit {u,v,0,0}).
- Raster/geom: RSTATE/RSETUP/RDRAW/RRECT, GSTATE/GPARAM/GDRAW under opcode `0010100`.
- Sync/ctrl: MEMBAR (current HW local fence), BARRIER, CSRRW, CSR_CORE_ID, CSR_TILE_OFFSET.

## Notes on Execution Model
- Four independent SIMD cores, each single-instruction-stream; no warps or exec masks. Conditional work is done via scalar masks + VSEL.
- Scoreboard hides TEX/LD/ST latency; rely on four tiles in flight plus ILP within a core (no warp swap).
- Texture latency: OSPI misses can be hundreds of cycles; keep outstanding TEX/LD depth sufficient per core and overlap with math.
- Memory: display fetch is separate port; ensure framebuffer visibility with MEMBAR or dispatch boundary.
- Exceptions: invalid opcode or misaligned accesses raise flag in status CSR; host can query via command completion.

## Open Items
- Exact bit encodings for opcodes/funct fields.
- Precise rounding modes and flush-to-zero policy for FP16/FP8.
- Tensor tile shapes supported in hardware vs. emulated by microcode.
- Cache/line size and coherency rules with display/host DMA.
