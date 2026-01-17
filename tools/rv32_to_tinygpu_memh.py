#!/usr/bin/env python3
"""Translate RV32 (RISC-V) instruction words into tinyGPU scalar ISA words.

Purpose
- Near-term path to run C code (e.g. CoreMark) on the tinyGPU compute core
  without a full LLVM backend.
- Input is a flat stream of 32-bit RV32 instruction words (little-endian) as
  either:
  - .memh (one 32-bit hex word per line, no 0x prefix), or
  - .bin (raw little-endian 32-bit words)

Limitations (by design)
- This only supports a conservative RV32IM subset.
- It errors out on AUIPC, compressed (RVC), CSR, atomics, and most system ops.
- It can optionally insert a NOP bubble after control-flow instructions.
    When enabled, the translator also re-encodes PC-relative immediates (JAL/BRANCH)
    so targets remain correct.

This script is intentionally self-contained (no external Python deps).
"""

from __future__ import annotations

import argparse
import pathlib
import struct
from dataclasses import dataclass


# -------------------------
# tinyGPU opcode map
# -------------------------
OP_INT = 0b0001011
# Keep separate from OP_TEX (0b0010011); use 0b0010111 (RV32 AUIPC slot).
OP_INT_IMM = 0b0010111
OP_LOAD = 0b0001100
OP_STORE = 0b0001101
OP_BRANCH = 0b0001110
OP_SYSTEM = 0b0001111
OP_LUI = 0b0110111
OP_JAL = 0b1101111


# RV32 opcodes (subset)
RV_JAL = 0b1101111
RV_JALR = 0b1100111
RV_BRANCH = 0b1100011


def _u32(x: int) -> int:
    return x & 0xFFFF_FFFF


def _bits(x: int, hi: int, lo: int) -> int:
    return (x >> lo) & ((1 << (hi - lo + 1)) - 1)


def sign_extend(value: int, bits: int) -> int:
    sign_bit = 1 << (bits - 1)
    return (value & (sign_bit - 1)) - (value & sign_bit)


# -------------------------
# RV32 decode helpers
# -------------------------
@dataclass(frozen=True)
class RVInst:
    raw: int

    @property
    def opcode(self) -> int:
        return _bits(self.raw, 6, 0)

    @property
    def rd(self) -> int:
        return _bits(self.raw, 11, 7)

    @property
    def funct3(self) -> int:
        return _bits(self.raw, 14, 12)

    @property
    def rs1(self) -> int:
        return _bits(self.raw, 19, 15)

    @property
    def rs2(self) -> int:
        return _bits(self.raw, 24, 20)

    @property
    def funct7(self) -> int:
        return _bits(self.raw, 31, 25)

    def imm_i(self) -> int:
        return sign_extend(_bits(self.raw, 31, 20), 12)

    def imm_s(self) -> int:
        imm = (_bits(self.raw, 31, 25) << 5) | _bits(self.raw, 11, 7)
        return sign_extend(imm, 12)

    def imm_b(self) -> int:
        imm = (
            (_bits(self.raw, 31, 31) << 12)
            | (_bits(self.raw, 7, 7) << 11)
            | (_bits(self.raw, 30, 25) << 5)
            | (_bits(self.raw, 11, 8) << 1)
        )
        return sign_extend(imm, 13)

    def imm_u(self) -> int:
        return _bits(self.raw, 31, 12)

    def imm_j(self) -> int:
        imm = (
            (_bits(self.raw, 31, 31) << 20)
            | (_bits(self.raw, 19, 12) << 12)
            | (_bits(self.raw, 20, 20) << 11)
            | (_bits(self.raw, 30, 21) << 1)
        )
        return sign_extend(imm, 21)


# -------------------------
# tinyGPU encode helpers
# -------------------------
def enc_r(funct7: int, rs2: int, rs1: int, funct3: int, rd: int, opcode: int) -> int:
    return _u32(
        ((funct7 & 0x7F) << 25)
        | ((rs2 & 0x1F) << 20)
        | ((rs1 & 0x1F) << 15)
        | ((funct3 & 0x7) << 12)
        | ((rd & 0x1F) << 7)
        | (opcode & 0x7F)
    )


def enc_i(imm: int, rs1: int, funct3: int, rd: int, opcode: int) -> int:
    return _u32(
        ((imm & 0xFFF) << 20)
        | ((rs1 & 0x1F) << 15)
        | ((funct3 & 0x7) << 12)
        | ((rd & 0x1F) << 7)
        | (opcode & 0x7F)
    )


def enc_s(imm: int, rs1: int, rs2: int, funct3: int, opcode: int) -> int:
    return _u32(
        (((imm >> 5) & 0x7F) << 25)
        | ((rs2 & 0x1F) << 20)
        | ((rs1 & 0x1F) << 15)
        | ((funct3 & 0x7) << 12)
        | ((imm & 0x1F) << 7)
        | (opcode & 0x7F)
    )


def enc_b(imm: int, rs1: int, rs2: int, funct3: int, opcode: int) -> int:
    # imm is signed byte offset
    imm &= 0x1FFF
    return _u32(
        (((imm >> 12) & 0x1) << 31)
        | (((imm >> 5) & 0x3F) << 25)
        | ((rs2 & 0x1F) << 20)
        | ((rs1 & 0x1F) << 15)
        | ((funct3 & 0x7) << 12)
        | (((imm >> 1) & 0xF) << 8)
        | (((imm >> 11) & 0x1) << 7)
        | (opcode & 0x7F)
    )


def enc_u(imm20: int, rd: int, opcode: int) -> int:
    return _u32(((imm20 & 0xFFFFF) << 12) | ((rd & 0x1F) << 7) | (opcode & 0x7F))


def enc_j(imm: int, rd: int, opcode: int) -> int:
    # imm is signed byte offset
    imm &= 0x1F_FFFF
    return _u32(
        (((imm >> 20) & 0x1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 0x1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | ((rd & 0x1F) << 7)
        | (opcode & 0x7F)
    )


def tinygpu_nop() -> int:
    # ADDI x0, x0, 0 under OP_INT_IMM
    return enc_i(0, 0, 0b000, 0, OP_INT_IMM)


# -------------------------
# Translation
# -------------------------
class Unsupported(Exception):
    pass


@dataclass(frozen=True)
class _Fixup:
    kind: str  # 'jal' | 'branch'
    out_index: int
    old_pc: int
    target_old_pc: int
    rd: int = 0
    rs1: int = 0
    rs2: int = 0
    funct3: int = 0


def translate_one(rv_word: int) -> tuple[list[int], bool]:
    """Return (tinygpu_words, is_control_flow).

    Most RV32 instructions translate 1:1. Some unsupported RV32 encodings are
    expanded into short instruction sequences (e.g. LBU/LHU).
    """

    inst = RVInst(rv_word)
    op = inst.opcode

    # RV32 opcodes
    RV_LUI = 0b0110111
    RV_AUIPC = 0b0010111
    RV_LOAD = 0b0000011
    RV_STORE = 0b0100011
    RV_OP_IMM = 0b0010011
    RV_OP = 0b0110011
    RV_MISC_MEM = 0b0001111  # fence/fence.i

    if op == RV_AUIPC:
        raise Unsupported("AUIPC not supported (compile with -mcmodel=medlow / no PIC)")

    if op == RV_LUI:
        return [enc_u(inst.imm_u(), inst.rd, OP_LUI)], False

    if op == RV_JAL:
        # tinyGPU: dedicated JAL opcode (J-type)
        return [enc_j(inst.imm_j(), inst.rd, OP_JAL)], True

    if op == RV_JALR:
        # tinyGPU: OP_BRANCH + funct3=010 + I-type imm
        return [enc_i(inst.imm_i(), inst.rs1, 0b010, inst.rd, OP_BRANCH)], True

    if op == RV_BRANCH:
        # funct3 mapping matches RISC-V for BEQ/BNE/BLT/BGE/BLTU/BGEU
        return [enc_b(inst.imm_b(), inst.rs1, inst.rs2, inst.funct3, OP_BRANCH)], True

    if op == RV_LOAD:
        # Base ISA: LB/LH/LW.
        if inst.funct3 in (0b000, 0b001, 0b010):
            return [enc_i(inst.imm_i(), inst.rs1, inst.funct3, inst.rd, OP_LOAD)], False

        # LBU/LHU are not native in the tinyGPU LOAD encoding; emulate.
        # LBU:  LB;  slli rd,rd,24; srli rd,rd,24
        # LHU:  LH;  slli rd,rd,16; srli rd,rd,16
        if inst.funct3 in (0b100, 0b101):
            is_lhu = inst.funct3 == 0b101
            base_f3 = 0b001 if is_lhu else 0b000
            shamt = 16 if is_lhu else 24
            lb_lh = enc_i(inst.imm_i(), inst.rs1, base_f3, inst.rd, OP_LOAD)
            slli = enc_i(shamt, inst.rd, 0b001, inst.rd, OP_INT)
            srli = enc_r(0b0000000, shamt, inst.rd, 0b101, inst.rd, OP_INT)
            return [lb_lh, slli, srli], False

        raise Unsupported(f"LOAD funct3={inst.funct3:03b} not supported")

    if op == RV_STORE:
        if inst.funct3 not in (0b000, 0b001, 0b010):
            raise Unsupported(f"STORE funct3={inst.funct3:03b} not supported")
        return [enc_s(inst.imm_s(), inst.rs1, inst.rs2, inst.funct3, OP_STORE)], False

    if op == RV_OP_IMM:
        # ADDI/SLTI/SLTIU/ANDI/ORI/XORI + shifts (SLLI/SRLI/SRAI)
        f3 = inst.funct3
        if f3 in (0b000, 0b010, 0b011, 0b100, 0b110, 0b111):
            return [enc_i(inst.imm_i(), inst.rs1, f3, inst.rd, OP_INT_IMM)], False
        if f3 == 0b001:
            # SLLI: imm[11:5] must be 0
            shamt = _bits(inst.raw, 24, 20)
            imm = shamt
            return [enc_i(imm, inst.rs1, 0b001, inst.rd, OP_INT_IMM)], False
        if f3 == 0b101:
            shamt = _bits(inst.raw, 24, 20)
            is_srai = inst.funct7 == 0b0100000
            if inst.funct7 not in (0b0000000, 0b0100000):
                raise Unsupported(f"shift-imm funct7={inst.funct7:07b} unsupported")
            # Keep RISC-V-style shift-imm encoding: imm[11:5]=funct7, imm[4:0]=shamt.
            imm = ((0b0100000 if is_srai else 0b0000000) << 5) | shamt
            return [enc_i(imm, inst.rs1, 0b101, inst.rd, OP_INT_IMM)], False
        raise Unsupported(f"OP-IMM funct3={f3:03b} unsupported")

    if op == RV_OP:
        # R-type: keep funct7/funct3, change opcode
        return [enc_r(inst.funct7, inst.rs2, inst.rs1, inst.funct3, inst.rd, OP_INT)], False

    if op == RV_MISC_MEM:
        # fence/fence.i -> MEMBAR
        return [enc_i(0, 0, 0b000, 0, OP_SYSTEM)], False

    raise Unsupported(f"RV32 opcode {op:07b} not supported")


def read_words(path: pathlib.Path) -> list[int]:
    data = path.read_bytes()
    if path.suffix.lower() == ".bin":
        if len(data) % 4 != 0:
            raise ValueError(".bin length must be multiple of 4 bytes")
        return [struct.unpack_from("<I", data, i)[0] for i in range(0, len(data), 4)]

    words: list[int] = []
    for line in data.decode("utf-8").splitlines():
        s = line.strip()
        if not s or s.startswith("#"):
            continue
        if s.lower().startswith("0x"):
            s = s[2:]
        words.append(int(s, 16) & 0xFFFF_FFFF)
    return words


def write_memh(path: pathlib.Path, words: list[int]) -> None:
    path.write_text("\n".join(f"{w:08x}" for w in words) + "\n")


def _check_imm_range(kind: str, imm: int, bits: int) -> None:
    # imm is a signed byte offset; bit0 must be 0
    if (imm & 0x1) != 0:
        raise ValueError(f"{kind} immediate must be 2-byte aligned, got {imm}")
    min_imm = -(1 << (bits - 1))
    max_imm = (1 << (bits - 1)) - 2
    if imm < min_imm or imm > max_imm:
        raise ValueError(f"{kind} immediate out of range for {bits}-bit signed offset: {imm}")


def translate_stream(rv_words: list[int], *, delay_slot_nop: bool) -> list[int]:
    """Translate a stream of RV32 words to tinyGPU words.

    If delay_slot_nop is enabled, a NOP is inserted after every control-flow
    instruction and PC-relative targets (JAL/BRANCH) are re-encoded so that they
    still reach the same RV32 target instruction.
    """

    out: list[int] = []
    pc_map: dict[int, int] = {}  # old_pc -> new_pc (bytes)
    fixups: list[_Fixup] = []

    for i, w in enumerate(rv_words):
        old_pc = i * 4
        pc_map[old_pc] = len(out) * 4

        inst = RVInst(w)
        is_cf = False

        # For PC-relative control-flow, emit a placeholder and fix up later.
        if inst.opcode == RV_JAL:
            out_index = len(out)
            out.append(0)
            fixups.append(
                _Fixup(
                    kind="jal",
                    out_index=out_index,
                    old_pc=old_pc,
                    target_old_pc=old_pc + inst.imm_j(),
                    rd=inst.rd,
                )
            )
            is_cf = True
        elif inst.opcode == RV_BRANCH:
            out_index = len(out)
            out.append(0)
            fixups.append(
                _Fixup(
                    kind="branch",
                    out_index=out_index,
                    old_pc=old_pc,
                    target_old_pc=old_pc + inst.imm_b(),
                    rs1=inst.rs1,
                    rs2=inst.rs2,
                    funct3=inst.funct3,
                )
            )
            is_cf = True
        else:
            try:
                tg_words, is_cf = translate_one(w)
            except Unsupported as e:
                raise ValueError(
                    f"at word {i} (pc=0x{old_pc:08x}) instr=0x{w:08x}: {e}"
                ) from e
            out.extend(tg_words)

        if delay_slot_nop and is_cf:
            out.append(tinygpu_nop())

    # Apply PC-relative fixups now that we know the final old_pc->new_pc mapping.
    for fu in fixups:
        if fu.target_old_pc not in pc_map:
            raise ValueError(
                f"control-flow target pc=0x{fu.target_old_pc:08x} is outside the input stream"
            )
        src_new_pc = pc_map[fu.old_pc]
        tgt_new_pc = pc_map[fu.target_old_pc]
        imm_new = tgt_new_pc - src_new_pc

        if fu.kind == "jal":
            _check_imm_range("JAL", imm_new, bits=21)
            out[fu.out_index] = enc_j(imm_new, fu.rd, OP_JAL)
        elif fu.kind == "branch":
            _check_imm_range("BRANCH", imm_new, bits=13)
            out[fu.out_index] = enc_b(imm_new, fu.rs1, fu.rs2, fu.funct3, OP_BRANCH)
        else:
            raise AssertionError(f"unknown fixup kind {fu.kind!r}")

    return out


def selftest() -> None:
    rv_addi = 0x8000_0093  # addi x1,x0,-2048
    tg_words, is_cf = translate_one(rv_addi)
    assert not is_cf
    assert tg_words == [0x8000_0097]

    rv_lui = 0x1234_52B7  # lui x5,0x12345
    tg_words, _ = translate_one(rv_lui)
    assert tg_words == [0x1234_52B7]

    rv_beq = 0x0020_8463  # beq x1,x2,+8
    tg_words, is_cf = translate_one(rv_beq)
    assert is_cf
    assert (tg_words[0] & 0x7F) == OP_BRANCH

    # Verify that --delay-slot-nop preserves PC-relative targets via fixups.
    # Program:
    #   0x00: beq x1,x2,+8
    #   0x04: addi x0,x0,0
    #   0x08: addi x0,x0,0
    # With a NOP bubble inserted after the branch, the target moves from +8 to +12.
    rv_nop = 0x0000_0013  # RV32 NOP (addi x0,x0,0)
    fixed = translate_stream([rv_beq, rv_nop, rv_nop], delay_slot_nop=True)
    assert fixed[0] == enc_b(12, 1, 2, 0b000, OP_BRANCH)
    assert fixed[1] == tinygpu_nop()

    # Same idea for JAL.
    rv_jal = enc_j(8, 1, RV_JAL)  # jal x1,+8
    fixed = translate_stream([rv_jal, rv_nop, rv_nop], delay_slot_nop=True)
    assert fixed[0] == enc_j(12, 1, OP_JAL)
    assert fixed[1] == tinygpu_nop()

    rv_fence = 0x0000_000F
    tg_words, _ = translate_one(rv_fence)
    assert tg_words == [enc_i(0, 0, 0b000, 0, OP_SYSTEM)]

    print("selftest OK")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("input", type=pathlib.Path, nargs="?", help="Input RV32 .memh or .bin")
    ap.add_argument("-o", "--output", type=pathlib.Path, required=False, help="Output tinyGPU .memh")
    ap.add_argument("--delay-slot-nop", action="store_true", help="Insert a NOP after every control-flow instruction")
    ap.add_argument("--pad-even", action="store_true", help="Pad with a final NOP so word count is even (8B bundle aligned)")
    ap.add_argument("--selftest", action="store_true", help="Run a small internal self-test")
    args = ap.parse_args()

    if args.selftest:
        selftest()
        return 0

    if args.input is None or args.output is None:
        ap.error("input and -o/--output are required unless --selftest is used")

    rv_words = read_words(args.input)
    try:
        out = translate_stream(rv_words, delay_slot_nop=args.delay_slot_nop)
    except ValueError as e:
        raise SystemExit(f"ERROR: {e}")

    if args.pad_even and (len(out) % 2 == 1):
        out.append(tinygpu_nop())

    write_memh(args.output, out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
