transcript file gfx_textured_triangle_tb_transcript.txt

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
    {gfx_textured_triangle_tb.sv}

vsim -c -t 1ns work.gfx_textured_triangle_tb

run -all
quit -f
