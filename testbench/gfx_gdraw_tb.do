transcript file gfx_gdraw_tb_transcript.txt

if {[file exists work]} {
    vdel -all
}

vlib work

# Paths include spaces; use braces.
vlog -sv -work work \
    {../ip/compute unit/isa_pkg.sv} \
    {../ip/compute unit/raster_unit.sv} \
    {../ip/compute unit/rop_unit.sv} \
    {../ip/compute unit/graphics_pipeline.sv} \
    {gfx_gdraw_tb.sv}

vsim -c -t 1ns work.gfx_gdraw_tb

run -all
quit -f
