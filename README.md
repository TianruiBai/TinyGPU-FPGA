# TinyGPU — FPGA Front End / Tiny GPU Prototype

Short description

TinyGPU is a collection of SystemVerilog RTL, firmware, testbenches and tooling used to prototype a small GPU-like subsystem ("Tiny GPU"). This repository contains the front-end RTL, the mailbox interconnect, endpoints, testbenches using Verilator, and several benchmarking programs and utilities used to validate performance and correctness.

Project goals

- Provide a compact, modular **Tiny GPU** prototype for exploration and research (compute units, raster pipeline, mailbox interconnect, firmware & host API).
- Maintain a set of reproducible simulation flows (Verilator) and benchmarks (CoreMark, Dhrystone) to measure progress.
- Keep documentation (under `docs/`) describing design, ISA and test plans.

Quick links

- RTL and IP: `ip/` (mailbox, endpoints, switches, fifo, compute unit, ...)
- Testbenches: `testbench/` (mailbox_tb.sv, dhrystone_tb.sv, etc.)
- Simulation scripts: `scripts/` (run_mailbox_verilator.sh, run_dhrystone_verilator.sh, run_coremark_*, ...)
- Docs: `docs/` (architecture, mailbox interconnect spec, testbench notes, API docs)
- Benchmarks: `coremark/`, `benchmark-dhrystone/`

Requirements

- Verilator (recommended recent version, e.g., 4.x/5.x)
- g++ (C++ build for verilated C++)
- make
- A POSIX shell environment (Linux tested)

Running the mailbox testbench (example)

1. Build & run the dedicated script:

   ./scripts/run_mailbox_verilator.sh +max_cycles=120000

   - `+max_cycles=<n>` is forwarded to the test binary and controls the simulation timeout.

2. Waveforms are generated (by default) as `mailbox_tb.vcd` in the working directory — open in any waveform viewer (`gtkwave`, etc.).

Enabling debug prints

- Many debug prints are gated.
  - Switch arbitration prints are controlled by the `VERBOSE` parameter in `ip/mailbox/mailbox_switch_2x1.sv`. Set the instance parameter in the testbench instantiation to enable them, e.g.:

    mailbox_switch_2x1 #(.MY_CLUSTER_ID(8'h11), .FIFO_DEPTH(4), .VERBOSE(1'b1)) sw0 ( ... );

  - FIFO write prints use a plusarg test `$test$plusargs("VERBOSE")`. Run the TB with `+VERBOSE` to see FIFO writes:

    ./obj_dir/Vmailbox_tb +VERBOSE +max_cycles=120000

Repository layout (high level)

- `ip/` — mailbox, endpoints, FIFOs, compute units and other RTL modules
- `testbench/` — SystemVerilog testbenches and Verilator glue
- `scripts/` — helper scripts to build and run Verilator simulations and benchmarks
- `coremark/` — CoreMark benchmark port and artifacts
- `benchmark-dhrystone/` — Dhrystone benchmark and ported support code
- `docs/` — architecture, mailbox interconnect spec, testbench guidance, API and firmware docs

Benchmarks & credits

- CoreMark (EEMBC / CoreMark) — included under the CoreMark acceptable use arrangement. See `coremark/LICENSE.md` in this repository and follow EEMBC requirements for benchmark usage/attribution.
  - Original CoreMark project: https://www.eembc.org/coremark/

- Dhrystone — the Dhrystone benchmark (Weicker / Richardson ports) distributed here for convenience; original authors and ports are credited in `benchmark-dhrystone/README.md` and `benchmark-dhrystone/LICENSE`. The upstream source used for some artifacts was https://fossies.org/linux/privat/old/dhrystone-2.1.tar.gz.

Documentation

Please consult `docs/` for detailed design documents:
- `docs/architecture.md` — system overview
- `docs/mailbox_interconnect_spec.md` — mailbox routes/packets & interconnect behavior
- `docs/testbenches.md` — testbench patterns, how to run verification & debug
- Other API and firmware notes in `docs/`.

Testing & verification notes

- Verilator is used for functional simulation; the `scripts/` directory contains the example invocations used during development (e.g., `run_mailbox_verilator.sh`, `run_dhrystone_verilator.sh`).
- Tests use `+max_cycles` plusargs to limit simulation runs; other plusargs such as `+VERBOSE` are used to enable specific debug prints.

Contribution & style

- Please open PRs for changes. Follow the coding style already present in the repo, add or update docs in `docs/` and add or update `testbench/` files to exercise changes.

First commit suggestion (detailed) — copy/paste into your `git commit -m` when you create the initial commit

"Initial import: TinyGPU front-end prototype

- Add RTL for mailbox interconnect (endpoints, switch, center, FIFO) and compute-unit front-end
- Add SystemVerilog testbenches and Verilator run scripts for mailbox and benchmark flows
- Add benchmark imports and ports: CoreMark (EEMBC CoreMark, see coremark/LICENSE.md) and Dhrystone (Weicker / Richardson sources, see benchmark-dhrystone/LICENSE)
- Add documentation under `docs/` (architecture, mailbox spec, testbench guide, API)
- Add utility scripts for building/running Verilator simulations and generating waveforms

Notes: Benchmarks are included for testing and are subject to their respective license / acceptable use agreements (see `coremark/LICENSE.md` and `benchmark-dhrystone/LICENSE`)."

License & legal

- This repository contains third-party benchmark code which is distributed under their own licenses (see `coremark/LICENSE.md` and `benchmark-dhrystone/LICENSE`).

Contact / authors

- See `docs/` and git history (after first commit) for authorship notes. If you provide a preferred copyright notice and license, I can add a `LICENSE` file and integrate it into the README.
