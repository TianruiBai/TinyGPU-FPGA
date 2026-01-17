#!/usr/bin/env python3

import argparse
import pathlib


def iter_words(data: bytes, word_size: int) -> list[bytes]:
    if len(data) % word_size != 0:
        pad = word_size - (len(data) % word_size)
        data = data + (b"\x00" * pad)
    return [data[i : i + word_size] for i in range(0, len(data), word_size)]


def main() -> None:
    ap = argparse.ArgumentParser(description="Convert raw binary into 32-bit word-per-line .memh for $readmemh")
    ap.add_argument("input", type=pathlib.Path)
    ap.add_argument("-o", "--output", type=pathlib.Path, required=True)
    ap.add_argument(
        "--origin-words",
        type=lambda s: int(s, 0),
        default=0,
        help=(
            "Optional $readmemh origin, in WORDS (array indices). "
            "When nonzero, emits a leading '@<hex>' directive."
        ),
    )
    ap.add_argument("--word-bytes", type=int, default=4, choices=(1, 2, 4), help="word size in bytes (default: 4)")
    ap.add_argument(
        "--endian",
        type=str,
        default="little",
        choices=("little", "big"),
        help="byte order used to assemble words (default: little)",
    )
    args = ap.parse_args()

    raw = args.input.read_bytes()
    words = iter_words(raw, args.word_bytes)

    with args.output.open("w", encoding="utf-8") as f:
        if args.origin_words:
            f.write(f"@{args.origin_words:x}\n")
        for w in words:
            if args.endian == "little":
                val = int.from_bytes(w, "little", signed=False)
            else:
                val = int.from_bytes(w, "big", signed=False)

            # $readmemh accepts plain hex; keep fixed width for readability.
            width = args.word_bytes * 2
            f.write(f"{val:0{width}x}\n")


if __name__ == "__main__":
    main()
