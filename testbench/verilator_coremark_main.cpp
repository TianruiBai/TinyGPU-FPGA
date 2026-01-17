#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include "Vcoremark_tb.h"
#include "verilated.h"

static uint64_t parse_u64_plusarg(int argc, char** argv, const char* key, uint64_t defval) {
    const std::string prefix = std::string("+") + key + "=";
    for (int i = 1; i < argc; i++) {
        const char* a = argv[i];
        if (!a) continue;
        if (std::strncmp(a, prefix.c_str(), prefix.size()) == 0) {
            const char* v = a + prefix.size();
            if (!v || !*v) return defval;
            // decimal expected for max_cycles
            return std::strtoull(v, nullptr, 0);
        }
    }
    return defval;
}

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    // Default matches SV TB.
    const uint64_t max_cycles = parse_u64_plusarg(argc, argv, "max_cycles", 5'000'000ULL);
    const uint64_t hard_limit_half_cycles = (max_cycles + 1000ULL) * 2ULL;

    Vcoremark_tb* top = new Vcoremark_tb;

    // Drive reset low initially.
    top->clk = 0;
    top->rst_n = 0;

    uint64_t half_cycles = 0;
    while (!Verilated::gotFinish() && half_cycles < hard_limit_half_cycles) {
        // Toggle clock
        top->clk = !top->clk;

        // Release reset after 5 rising edges (matches TB behavior)
        // Rising edges are when clk is 1 after toggle.
        if (top->clk && half_cycles >= (5ULL * 2ULL)) {
            top->rst_n = 1;
        }

        top->eval();
        Verilated::timeInc(1);
        half_cycles++;
    }

    if (!Verilated::gotFinish()) {
        std::cerr << "VERILATOR_COREMARK: hard limit reached (max_cycles=" << max_cycles << ")\n";
        delete top;
        return 2;
    }

    delete top;
    return 0;
}
