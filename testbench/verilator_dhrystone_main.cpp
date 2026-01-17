#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>

#include "Vdhrystone_tb.h"
#include "verilated.h"

#if VM_TRACE
#include "verilated_vcd_c.h"
#endif

static uint64_t parse_u64_plusarg(int argc, char** argv, const char* key, uint64_t defval) {
    const std::string prefix = std::string("+") + key + "=";
    for (int i = 1; i < argc; i++) {
        const char* a = argv[i];
        if (!a) continue;
        if (std::strncmp(a, prefix.c_str(), prefix.size()) == 0) {
            const char* v = a + prefix.size();
            if (!v || !*v) return defval;
            return std::strtoull(v, nullptr, 0);
        }
    }
    return defval;
}

static std::string parse_str_plusarg(int argc, char** argv, const char* key, const char* defval) {
    const std::string prefix = std::string("+") + key + "=";
    for (int i = 1; i < argc; i++) {
        const char* a = argv[i];
        if (!a) continue;
        if (std::strncmp(a, prefix.c_str(), prefix.size()) == 0) {
            const char* v = a + prefix.size();
            if (!v) break;
            return std::string(v);
        }
    }
    return std::string(defval ? defval : "");
}

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    const uint64_t max_cycles = parse_u64_plusarg(argc, argv, "max_cycles", 50'000'000ULL);
    const uint64_t hard_limit_half_cycles = (max_cycles + 1000ULL) * 2ULL;

    const bool waves_en = (parse_u64_plusarg(argc, argv, "waves", 0ULL) != 0ULL);
    const uint64_t waves_start_cycle = parse_u64_plusarg(argc, argv, "waves_start", 0ULL);
    const uint64_t waves_depth = parse_u64_plusarg(argc, argv, "waves_depth", 0ULL);
    const std::string vcd_file = parse_str_plusarg(argc, argv, "vcd", "dhrystone_tb.vcd");

    Vdhrystone_tb* top = new Vdhrystone_tb;

#if VM_TRACE
    VerilatedVcdC* tfp = nullptr;
    if (waves_en) {
        Verilated::traceEverOn(true);
        tfp = new VerilatedVcdC;
        const int depth = (waves_depth == 0ULL) ? 99 : static_cast<int>(waves_depth);
        top->trace(tfp, depth);
        tfp->open(vcd_file.c_str());
        std::cerr << "VERILATOR_DHRYSTONE: waves enabled: file=" << vcd_file
                  << " depth=" << depth << " start_cycle=" << waves_start_cycle << "\n";
    }
#else
    if (waves_en) {
        std::cerr << "VERILATOR_DHRYSTONE: WARNING: built without VM_TRACE; rebuild with --trace to enable waves\n";
    }
#endif

    top->clk = 0;
    top->rst_n = 0;

    uint64_t half_cycles = 0;
    while (!Verilated::gotFinish() && half_cycles < hard_limit_half_cycles) {
        top->clk = !top->clk;

        if (top->clk && half_cycles >= (5ULL * 2ULL)) {
            top->rst_n = 1;
        }

        top->eval();

#if VM_TRACE
        if (tfp) {
            const uint64_t cur_cycle = half_cycles / 2ULL;
            if ((waves_start_cycle == 0ULL) || (cur_cycle >= waves_start_cycle)) {
                tfp->dump(Verilated::time());
            }
        }
#endif

        Verilated::timeInc(1);
        half_cycles++;
    }

    if (!Verilated::gotFinish()) {
        std::cerr << "VERILATOR_DHRYSTONE: hard limit reached (max_cycles=" << max_cycles << ")\n";
        delete top;
        return 2;
    }

#if VM_TRACE
    if (tfp) {
        tfp->close();
        delete tfp;
    }
#endif

    delete top;
    return 0;
}
