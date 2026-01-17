transcript file modelsim_compute_unit_full_tb_transcript.txt

# NOTE: Questa/Modelsim 'do' scripts don't reliably set [info script].
# Use an explicit repo root so this script works regardless of vsim's cwd.
set repo_root "/home/polar/ProtoTracer-FPGA-FrontEND"

if {[file exists work]} {
    vdel -all
}

vlib work

set rtl_dir "$repo_root/ip/compute unit"

# Compile package(s) first (required since RTL no longer `include`s the package).
vlog -sv -work work "$rtl_dir/isa_pkg.sv"

# Compile remaining RTL from compute unit (paths include spaces).
set rtl_files [glob -nocomplain "$rtl_dir/*.sv"]
set rtl_files [lsearch -all -inline -not -exact $rtl_files "$rtl_dir/isa_pkg.sv"]

# Compile testbench
vlog -sv -work work {*}$rtl_files "$repo_root/testbench/compute_unit_full_tb.sv"

vsim -c -t 1ns work.compute_unit_full_tb

run -all
quit -f
