# CoreMark Testbench Flow

This repo provides a ModelSim/Questa testbench that can run an externally-built program image (e.g. CoreMark) on `compute_unit_top`.

## What you get

- `testbench/coremark_tb.sv`: loads a 32-bit instruction word image via `$readmemh`, runs until a DONE write happens.
- `testbench/coremark_tb.do`: compiles RTL + the CoreMark TB and runs batch.
- `tools/bin2memh.py`: converts a raw binary into `.memh` (one word per line) for `$readmemh`.

## Memory map / conventions

- Instruction ROM is at address 0x0000_0000 (byte addressed).
- Global data memory window is modeled at:
  - `base` (default `0xFFFF_F800`)
  - The DUT selects global memory by `addr[31]==1`, so keep `base[31]=1`.
- Program completion signal:
  - Write any value to `base + done_off` (default `0xFFFF_F800 + 0x1F0`).
  - Important: scalar global stores are buffered (write-merge buffer). To guarantee the DONE write appears on the global bus, follow it with a `MEMBAR` (SYSTEM `funct3=000`) or otherwise force a flush.
  - The TB ends the simulation immediately when it observes the DONE store on the global bus and prints the value.

## Running the TB (smoke)

The TB has a built-in default ROM that just writes DONE=1 and spins:

- `vsim -c -do testbench/coremark_tb.do`

## Running with an external ROM image

Create a `.memh` file containing 32-bit instruction words (one per line, hex, no `0x`), then run:

- `vsim -c -do testbench/coremark_tb.do -voptargs=+acc -sv_seed random +rom=path/to/coremark.memh +max_cycles=50000000`

Optional:

- `+mem_init=path/to/mem_init.memh` (initializes global memory words)
- `+base=FFFFF800`
- `+done_off=000001F0`

## Converting a raw binary to .memh

If you have a flat raw binary (little-endian words):

- `python3 tools/bin2memh.py coremark.bin -o coremark.memh --word-bytes 4 --endian little`

## CoreMark build notes

CoreMark needs a toolchain/port for your ISA.

This repo includes a practical short-term path:

1. Build CoreMark as RV32IM (freestanding) using `coremark/tinygpu/`.
2. Translate the RV32 binary words to tinyGPU ISA words with `tools/rv32_to_tinygpu_memh.py`.
3. Run `coremark_tb` with `+rom=...`.

Suggested approach (high level):

1. Port CoreMark to a freestanding environment (no libc), replace `printf` with a memory-mapped log or store final CRCs to memory.
2. Ensure your `main()` (or `coremark_main()`) writes a DONE value to `base+done_off` before exiting.
3. Produce a flat ROM image (`.memh`) and pass it to the TB via `+rom=...`.

### Addressing note (important)

The default `coremark_tb` base is `0xFFFF_F800`, which wraps after 2KB.
For CoreMark-sized RAM, use `+base=80000000` (still selects global memory via `addr[31]==1`).
