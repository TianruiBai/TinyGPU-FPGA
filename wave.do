onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/clk
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/rst_n
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/inst_rdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/inst_addr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_req_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_req_is_load
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_req_addr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_req_wdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_req_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/err_fp_overflow
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/err_fp_invalid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/err_vec_overflow
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/err_vec_invalid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/csr_status
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/csr_fstatus
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/csr_vstatus
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_req_ready
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_resp_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_resp_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/data_resp_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/if_pc
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/if_inst
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/if_valid
add wave -noupdate -radix hexadecimal -childformat {{/compute_unit_tb/dut/id_ctrl.is_valid -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_scalar_int -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_scalar_fp -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_vector -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_system -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_tex -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_load -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_store -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_branch -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.is_atomic -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.uses_rs1 -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.uses_rs2 -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.uses_rd -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.rd_is_vec -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.rd_is_fp -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.rs1_class -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.rs2_class -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.rd_class -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.rs1 -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.rs2 -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.rd -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.funct3 -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.funct7 -radix hexadecimal} {/compute_unit_tb/dut/id_ctrl.imm -radix hexadecimal}} -expand -subitemconfig {/compute_unit_tb/dut/id_ctrl.is_valid {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_scalar_int {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_scalar_fp {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_vector {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_system {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_tex {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_load {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_store {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_branch {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.is_atomic {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.uses_rs1 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.uses_rs2 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.uses_rd {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.rd_is_vec {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.rd_is_fp {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.rs1_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.rs2_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.rd_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.rs1 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.rs2 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.rd {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.funct3 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.funct7 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/id_ctrl.imm {-height 15 -radix hexadecimal}} /compute_unit_tb/dut/id_ctrl
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/id_valid
add wave -noupdate -radix hexadecimal -childformat {{/compute_unit_tb/dut/rr_ctrl.is_valid -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_scalar_int -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_scalar_fp -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_vector -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_system -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_tex -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_load -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_store -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_branch -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.is_atomic -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.uses_rs1 -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.uses_rs2 -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.uses_rd -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.rd_is_vec -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.rd_is_fp -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.rs1_class -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.rs2_class -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.rd_class -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.rs1 -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.rs2 -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.rd -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.funct3 -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.funct7 -radix hexadecimal} {/compute_unit_tb/dut/rr_ctrl.imm -radix hexadecimal}} -expand -subitemconfig {/compute_unit_tb/dut/rr_ctrl.is_valid {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_scalar_int {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_scalar_fp {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_vector {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_system {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_tex {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_load {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_store {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_branch {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.is_atomic {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.uses_rs1 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.uses_rs2 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.uses_rd {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.rd_is_vec {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.rd_is_fp {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.rs1_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.rs2_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.rd_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.rs1 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.rs2 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.rd {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.funct3 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.funct7 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/rr_ctrl.imm {-height 15 -radix hexadecimal}} /compute_unit_tb/dut/rr_ctrl
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/rr_valid
add wave -noupdate -radix hexadecimal -childformat {{/compute_unit_tb/dut/ex_ctrl.is_valid -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_scalar_int -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_scalar_fp -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_vector -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_system -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_tex -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_load -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_store -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_branch -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.is_atomic -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.uses_rs1 -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.uses_rs2 -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.uses_rd -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.rd_is_vec -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.rd_is_fp -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.rs1_class -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.rs2_class -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.rd_class -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.rs1 -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.rs2 -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.rd -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.funct3 -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.funct7 -radix hexadecimal} {/compute_unit_tb/dut/ex_ctrl.imm -radix hexadecimal}} -expand -subitemconfig {/compute_unit_tb/dut/ex_ctrl.is_valid {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_scalar_int {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_scalar_fp {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_vector {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_system {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_tex {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_load {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_store {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_branch {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.is_atomic {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.uses_rs1 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.uses_rs2 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.uses_rd {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.rd_is_vec {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.rd_is_fp {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.rs1_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.rs2_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.rd_class {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.rs1 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.rs2 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.rd {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.funct3 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.funct7 {-height 15 -radix hexadecimal} /compute_unit_tb/dut/ex_ctrl.imm {-height 15 -radix hexadecimal}} /compute_unit_tb/dut/ex_ctrl
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_op_a
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_op_b
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_mask_scalar
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_fp_a
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_fp_b
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_fp_scalar
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_vec_a
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_vec_b
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_scalar_res
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_fp_res
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_branch_taken
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_addr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/mem_ctrl
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/mem_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/mem_scalar_res
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/mem_fp_res
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/mem_addr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/mem_vec_wdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/mem_scalar_wdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/wb_ctrl
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/wb_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/wb_scalar_res
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/wb_fp_res
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/stall_scoreboard
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/lsu_stall
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/lsu_busy
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/stall_membar
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/stall_any
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/s_rdata_a
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/s_rdata_b
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/s_wdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/s_we
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/s_waddr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/f_rdata_a
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/f_rdata_b
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/f_wdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/f_we
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/f_waddr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/v_rdata_a
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/v_rdata_b
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/v_wdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/v_we
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/v_waddr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_ready
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_wb_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_wb_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_wb_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_wb_is_scalar
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_err_overflow
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_err_invalid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_wb_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_wb_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_alu_wb_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_scalar_wb_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_scalar_wb_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_scalar_wb_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_wb_err_overflow
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_wb_err_invalid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/lsu_wb_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/lsu_wb_is_vector
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/lsu_wb_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/lsu_wb_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/pending_scalar_wb
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/pending_scalar_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/pending_scalar_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/pending_vector_wb
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/pending_vector_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/pending_vector_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/local_req_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/local_we
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/local_req_is_vector
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/local_addr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/local_wdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/local_rdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/local_bank_sel
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/csr_en
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/csr_csrrs
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/csr_addr_ex
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/csr_wdata_ex
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/csr_rdata
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_stall
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/rr_is_membar
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/ex_is_membar
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/mem_is_membar
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_req_ready
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_wb_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_wb_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_wb_rd
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_miss_req_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_miss_req_addr
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_miss_req_ready
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_miss_resp_valid
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/tex_miss_resp_data
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/lsu_scalar_wb
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/lsu_vector_wb
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/alu_scalar_wb
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_scalar_wb
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_scalar_wb
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/scalar_wb_from_pending
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/fp_wb_fp
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/valuv_vector_wb
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/vector_wb_from_pending
add wave -noupdate -radix hexadecimal /compute_unit_tb/dut/vector_fire
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {109824 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 264
configure wave -valuecolwidth 207
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {0 ps} {261039 ps}
