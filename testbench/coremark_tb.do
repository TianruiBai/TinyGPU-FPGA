transcript file modelsim_coremark_tb_transcript.txt

# Use an explicit repo root so this script works regardless of vsim's cwd.
set repo_root "/home/polar/ProtoTracer-FPGA-FrontEND"

if {[file exists work]} {
    vdel -all
}

vlib work

set rtl_dir "$repo_root/ip/compute unit"

# Compile package(s) first
vlog -sv -work work "$rtl_dir/isa_pkg.sv"

# Compile remaining RTL
set rtl_files [glob -nocomplain "$rtl_dir/*.sv"]
set rtl_files [lsearch -all -inline -not -exact $rtl_files "$rtl_dir/isa_pkg.sv"]

# Compile testbench
vlog -sv -work work {*}$rtl_files "$repo_root/testbench/coremark_tb.sv"

# NOTE: provide +rom=... +mem_init=... +max_cycles=... via env var COREMARK_TB_ARGS.
# Example: COREMARK_TB_ARGS=+debug=1 vsim -c -do testbench/coremark_tb.do
set tb_args ""
if {[info exists ::env(COREMARK_TB_ARGS)]} {
    set tb_args $::env(COREMARK_TB_ARGS)
}
eval vsim -c -t 1ns work.coremark_tb $tb_args

run -all
quit -f
