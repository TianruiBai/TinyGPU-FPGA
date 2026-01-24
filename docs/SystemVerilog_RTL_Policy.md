# SystemVerilog RTL Implementation Policy

**Purpose:** Enforce a strict, synthesizable, and reviewable SystemVerilog style to avoid simulation/synthesis mismatches, improve timing closure, and make RTL audit-friendly.

---

## 🚩 Scope
- Applies to all design RTL (modules under `rtl/`, `ip/`, and other design folders).
- Testbench/verification code (UVM, SVA-only test harnesses) can follow relaxed rules; mark such files with a header comment and `// TESTBENCH`.

---

## ✅ High-Level Principles
- Keep sequential logic minimal and predictable.
- Compute next-state and combinational logic outside `always_ff` blocks.
- Avoid constructs that synthesize poorly or cause latches.
- Make intent explicit with typedefs, enums, and clear naming.

---

## 🔧 Mandatory Rules (MUST / MUST NOT)
- File header: every RTL file MUST start with a well-formed header block that documents license, purpose, authorship, tags and any synthesis/simulation notes. The header is used by code reviewers and automated tools (CI, license scanners, and linters).

```verilog
`default_nettype none
// SPDX-License-Identifier: Apache-2.0
// Module: <module_name>
// Description: <one-line summary>
//
// Detailed description (optional, multi-line):
//   - What the module does at a high level
//   - Any special synthesis considerations (async reset polarity, clock gating, etc.)
//
// Maintainer: <Team / Person>
// Created: YYYY-MM-DD
// Modified: YYYY-MM-DD - <short description>
// Tags: RTL, SYNTH, TESTBENCH (if applicable)
// Notes:
//   - Use `// TESTBENCH` tag for files that are purely verification/test code.
//   - Wrap simulation-only code with `// synthesis translate_off` / `// synthesis translate_on` or `ifndef SYNTHESIS` guards and add a short reason here.
//   - For tool-specific lint suppression, add comments like `// verilator lint_off <RULE>` plus an explanatory comment and re-enable with `// verilator lint_on <RULE>`.
```

**Rules & rationale:**
- `` `default_nettype none `` MUST be present and should appear as the first non-comment directive to catch implicit nets.
- **SPDX license identifier** is recommended on every source file to enable automated compliance scanning; use the repo's declared license (e.g., `Apache-2.0`).
- **`Module`** must match the module name inside the file topology.
- **`Description`** should be terse (1 line) and an optional multi-line block may follow for details.
- **`Tags`** must include `TESTBENCH` for verification-only files so CI/lint rules can treat them with the correct exceptions.
- **Document vendor pragmas / synthesis notes** in the header (e.g., use of vendor-specific attributes or allowed non-synth constructs) with an explicit justification.
- **Use of lint-suppression** comments must be documented inline and in the header `Notes:` with reason and ticket/PR cross-reference if applicable.

- Sequential logic:
  - MUST use `always_ff` for registers.
  - MUST use **non-blocking** assignments (`<=`) inside `always_ff`.
  - Sensitivity: use `@(posedge clk or negedge rst_n)` for async active-low reset by default.
  - `always_ff` MUST contain only reset and register update (`q <= d`) statements — no combinational computation.

- Combinational logic:
  - MUST use `always_comb` or `assign`.
  - MUST use **blocking** assignments (`=`) inside `always_comb`.
  - MUST define default values at the top of the block to avoid inferred latches.

- Latches & implicit nets:
  - MUST NOT infer latches. Always provide an assignment for all signal conditions.
  - MUST use `` `default_nettype none `` at top of files to catch typos.

- Loops & generates:
  - Procedural loops (`for`, `while`) inside `always_*` are **prohibited**.
  - Use `generate` + `genvar` loops only for replication/structural expansion (static bounds).

- FSMs:
  - MUST use `typedef enum logic [N:0]` for state encodings.
  - MUST implement FSMs in two blocks: `always_ff` for registers and `always_comb` for next state and outputs.

- Types & naming:
  - Use `logic` for ports and internal nets unless `wire` is required for multi-driver nets or `inout`.
  - Use `_r` for register names, `_nxt` for next-state signals, `_i/_o` for ports.
  - Numeric literals MUST include width and base (e.g., `8'hFF`, `1'b0`).

- Tooling & CI:
  - Linting (Verilator) and a smoke synthesis pass (Yosys) MUST be included in CI.

---

## 🧭 Recommended Patterns & Examples

### Minimal `always_ff` (Dumb FF)
```systemverilog
always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        cnt_r <= '0;
    end else begin
        cnt_r <= cnt_nxt; // _nxt computed in always_comb
    end
end
```

### Latch-free `always_comb` with defaults
```systemverilog
always_comb begin
    // Defaults
    state_nxt = state_r;
    out_val = '0;

    case (state_r)
        IDLE: if (start_i) begin state_nxt = BUSY; out_val = data_i; end
        BUSY: begin
            // ...
        end
    endcase
end
```

### Allowed generate loop (structural replication only)
```systemverilog
generate
    genvar i;
    for (i = 0; i < WIDTH; i++) begin : bits
        assign parity = parity ^ data_i[i];
    end
endgenerate
```

---

## ✅ PR / Code Review Checklist
- [ ] `default_nettype none` present
- [ ] `always_ff` used and only non-blocking assigns inside
- [ ] `always_comb` used for combinational logic and defines defaults at top
- [ ] No latches inferred (verify with lint)
- [ ] FSMs using enums + two-block style
- [ ] No procedural loops in `always_*` blocks
- [ ] Assertions added for critical interfaces/FSMs where practical
- [ ] Verilator lint & Yosys smoke pass added to CI or local checks

---

## 🔁 How to run checks locally
- Lint with Verilator:
```bash
# From repo root
verilator --lint-only -sv $(git ls-files "*.sv" "*.v")
```
- Smoke synthesis with Yosys:
```bash
# Basic smoke: ensure yosys is installed
yosys -p "read_verilog -sv $(git ls-files 'rtl/*.sv' 'ip/**/*.sv'); synth; opt; stat"
```

> Tip: Use `scripts/rtl_lint.sh` and `scripts/rtl_smoke_synth.sh` included in the repo to run the checks.

---

## 🧾 Exceptions and Process
- If a rule must be relaxed for micro-architectural reasons, document the exception in the PR title/body and obtain reviewer approval (tag the owner).

---

## 📎 Appendix: Template snippets
- Minimal file header (copy into new RTL files):
```verilog
`default_nettype none
// Module: <name>
// Purpose: <short description>
```

- FSM typedef example:
```systemverilog
typedef enum logic [1:0] { IDLE, READ, WRITE } state_t;
```

---

*Last updated: 2026-01-23*
