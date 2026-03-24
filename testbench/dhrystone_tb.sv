`timescale 1ns/1ps

// Dhrystone runner TB for compute_unit_top (Verilator/ModelSim).
// - Loads ROM via +rom=... (.memh, one 32-bit word per line)
// - Loads global RAM init via +mem_init=... (optional)
// - Waits for a DONE write at (base + done_off)
// - Checks the Dhrystone self-check result word at (base + result_off)
module dhrystone_tb(
    input logic clk,
    input logic rst_n
);
    import isa_pkg::*;

    // -------------------------
    // Optional waveform dumping
    // -------------------------
    // Enable with:
    //   +waves=1
    // Optional:
    //   +vcd=foo.vcd
    //   +waves_depth=0            (0 = full hierarchy)
    //   +waves_start=100000       (cycle to start dumping; 0 = start immediately)
    bit          waves_en;
    string       vcd_file;
    int unsigned waves_depth;
    int unsigned waves_start_cycle;

    // -------------------------
    // DUT interfaces
    // -------------------------
    // Instruction miss interface (I-cache -> memory)
    logic        inst_miss_req_valid;
    logic [31:0] inst_miss_req_addr;
    logic        inst_miss_req_ready;
    logic        inst_miss_resp_valid;
    logic [63:0] inst_miss_resp_data;

    // Legacy data interface (unused — CU routes all loads/stores through D-cache)
    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;
    logic        data_req_ready;
    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    // D-cache memory interface (L1 -> memory)
    logic        dcache_mem_req_valid;
    logic        dcache_mem_req_rw;
    logic [31:0] dcache_mem_req_addr;
    logic [7:0]  dcache_mem_req_size;
    logic [3:0]  dcache_mem_req_qos;
    logic [7:0]  dcache_mem_req_id;
    logic [511:0] dcache_mem_req_wdata;
    logic [7:0]  dcache_mem_req_wstrb;
    logic        dcache_mem_req_ready;
    logic        dcache_mem_resp_valid;
    logic [63:0] dcache_mem_resp_data;
    logic [7:0]  dcache_mem_resp_id;

    // Framebuffer AXI (unused)
    logic        fb_aw_valid;
    logic [31:0] fb_aw_addr;
    logic [7:0]  fb_aw_len;
    logic [2:0]  fb_aw_size;
    logic [1:0]  fb_aw_burst;
    logic        fb_aw_ready;
    logic [31:0] fb_w_data;
    logic [3:0]  fb_w_strb;
    logic        fb_w_last;
    logic        fb_w_valid;
    logic        fb_w_ready;
    logic        fb_b_valid;
    logic        fb_b_ready;

    // Mailbox (tied off)
    import mailbox_pkg::*;
    mailbox_pkg::mailbox_flit_t mailbox_tx_data;
    mailbox_pkg::mailbox_flit_t mailbox_rx_data;
    logic mailbox_tx_valid;
    logic mailbox_tx_ready;
    logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_tx_dest_id;
    logic mailbox_rx_valid;
    logic mailbox_rx_ready;
    logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_rx_dest_id;

    logic        err_fp_overflow;
    logic        err_fp_invalid;
    logic        err_vec_overflow;
    logic        err_vec_invalid;

    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    compute_unit_top #(
        .CORE_ID(32'hD),
        .TILE_OFFSET(32'h000D_0000)
    ) dut (
        .clk(clk),
        .rst_n(rst_n),
        .inst_miss_req_valid(inst_miss_req_valid),
        .inst_miss_req_addr(inst_miss_req_addr),
        .inst_miss_req_ready(inst_miss_req_ready),
        .inst_miss_resp_valid(inst_miss_resp_valid),
        .inst_miss_resp_data(inst_miss_resp_data),
        .data_req_valid(data_req_valid),
        .data_req_is_load(data_req_is_load),
        .data_req_addr(data_req_addr),
        .data_req_wdata(data_req_wdata),
        .data_req_rd(data_req_rd),
        .err_fp_overflow(err_fp_overflow),
        .err_fp_invalid(err_fp_invalid),
        .err_vec_overflow(err_vec_overflow),
        .err_vec_invalid(err_vec_invalid),
        .csr_status(csr_status),
        .csr_fstatus(csr_fstatus),
        .csr_vstatus(csr_vstatus),
        .data_req_ready(data_req_ready),
        .data_resp_valid(data_resp_valid),
        .data_resp_rd(data_resp_rd),
        .data_resp_data(data_resp_data),
        .dcache_mem_req_valid(dcache_mem_req_valid),
        .dcache_mem_req_rw(dcache_mem_req_rw),
        .dcache_mem_req_addr(dcache_mem_req_addr),
        .dcache_mem_req_size(dcache_mem_req_size),
        .dcache_mem_req_qos(dcache_mem_req_qos),
        .dcache_mem_req_id(dcache_mem_req_id),
        .dcache_mem_req_wdata(dcache_mem_req_wdata),
        .dcache_mem_req_wstrb(dcache_mem_req_wstrb),
        .dcache_mem_req_ready(dcache_mem_req_ready),
        .dcache_mem_resp_valid(dcache_mem_resp_valid),
        .dcache_mem_resp_data(dcache_mem_resp_data),
        .dcache_mem_resp_id(dcache_mem_resp_id),
        .fb_aw_valid(fb_aw_valid),
        .fb_aw_addr(fb_aw_addr),
        .fb_aw_len(fb_aw_len),
        .fb_aw_size(fb_aw_size),
        .fb_aw_burst(fb_aw_burst),
        .fb_aw_ready(fb_aw_ready),
        .fb_w_data(fb_w_data),
        .fb_w_strb(fb_w_strb),
        .fb_w_last(fb_w_last),
        .fb_w_valid(fb_w_valid),
        .fb_w_ready(fb_w_ready),
        .fb_b_valid(fb_b_valid),
        .fb_b_ready(fb_b_ready),
        .mailbox_tx_valid(mailbox_tx_valid),
        .mailbox_tx_ready(mailbox_tx_ready),
        .mailbox_tx_data(mailbox_tx_data),
        .mailbox_tx_dest_id(mailbox_tx_dest_id),
        .mailbox_rx_valid(mailbox_rx_valid),
        .mailbox_rx_ready(mailbox_rx_ready),
        .mailbox_rx_data(mailbox_rx_data),
        .mailbox_rx_dest_id(mailbox_rx_dest_id)
    );

    // NOTE: Waveform dumping is configured in the init +args block.

    // -------------------------
    // Tie-offs for unused interfaces
    // -------------------------
    assign data_req_ready  = 1'b1;
    assign data_resp_valid = 1'b0;
    assign data_resp_rd    = '0;
    assign data_resp_data  = 32'h0;

    assign mailbox_tx_ready   = 1'b1;
    assign mailbox_rx_valid   = 1'b0;
    assign mailbox_rx_data    = '0;
    assign mailbox_rx_dest_id = '0;

    assign fb_aw_ready = 1'b1;
    assign fb_w_ready  = 1'b1;
    assign fb_b_valid  = 1'b0;

    assign dcache_mem_req_ready = 1'b1;
    assign inst_miss_req_ready  = 1'b1;

    // -------------------------
    // ROM (instruction memory)
    // -------------------------
    localparam int ROM_WORDS = 65536; // 256KB of 32-bit instructions
    logic [31:0] rom [0:ROM_WORDS-1];

    // I-cache miss handler
    logic        inst_pending;
    logic [31:0] inst_req_addr_q;

    function automatic logic [31:0] rom_word_at(input logic [31:0] addr);
        int unsigned idx;
        idx = addr >> 2;
        if (idx < ROM_WORDS) rom_word_at = rom[idx];
        else rom_word_at = 32'h0000_0017; // tinyGPU NOP (ADDI x0,x0,0 under OP_IMM)
    endfunction

    // -------------------------
    // Global memory model (32-bit words)
    // -------------------------
    localparam int MEM_WORDS = 262144; // 1MB

    logic [31:0] base_addr;
    logic [31:0] done_off;
    logic [31:0] result_off;
    logic [31:0] max_cycles;

    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        // Use signed subtraction so addresses below base become negative indices.
        mem_index = ($signed(addr) - $signed(base_addr)) >>> 2;
    endfunction

    function automatic logic [7:0] mem_byte_at(input logic [31:0] addr);
        int idx;
        logic [31:0] w;
        idx = mem_index({addr[31:2], 2'b00});
        if ((idx < 0) || (idx >= MEM_WORDS)) begin
            mem_byte_at = 8'hxx;
        end else begin
            w = mem[idx];
            unique case (addr[1:0])
                2'd0: mem_byte_at = w[7:0];
                2'd1: mem_byte_at = w[15:8];
                2'd2: mem_byte_at = w[23:16];
                default: mem_byte_at = w[31:24];
            endcase
        end
    endfunction

    function automatic logic [31:0] mem_word_at(input logic [31:0] addr);
        int idx;
        idx = mem_index({addr[31:2], 2'b00});
        if ((idx < 0) || (idx >= MEM_WORDS)) begin
            mem_word_at = 32'hxxxxxxxx;
        end else begin
            mem_word_at = mem[idx];
        end
    endfunction

    // D-cache backing memory responder
    localparam int DCACHE_LINE_BYTES = 64;
    localparam int DCACHE_BEATS      = DCACHE_LINE_BYTES / 8;

    logic        dcache_tx_active;
    logic        dcache_tx_rw;
    logic [31:0] dcache_tx_addr;
    logic [7:0]  dcache_tx_id_q;
    logic [2:0]  dcache_tx_beat;

    function automatic [63:0] dcache_read_beat(input logic [31:0] line_addr, input logic [2:0] beat);
        int base_word;
        begin
            dcache_read_beat = 64'h0;
            base_word = mem_index(line_addr) + (beat * 2);
            if ((base_word >= 0) && ((base_word + 1) < MEM_WORDS)) begin
                dcache_read_beat = {mem[base_word + 1], mem[base_word + 0]};
            end
        end
    endfunction

    task automatic dcache_write_line(
        input logic [31:0] line_addr,
        input logic [511:0] line_data,
        input logic [7:0]  line_wstrb  // 1 strobe bit per 8-byte beat
    );
        int base_word;
        begin
            for (int b = 0; b < DCACHE_BEATS; b++) begin
                if (line_wstrb[b]) begin
                    base_word = mem_index(line_addr) + (b * 2);
                    if ((base_word >= 0) && ((base_word + 1) < MEM_WORDS)) begin
                        mem[base_word + 0] = line_data[(b*64) +: 32];
                        mem[base_word + 1] = line_data[(b*64) + 32 +: 32];
                    end
                end
            end
        end
    endtask

    // DONE tracking
    logic        done_seen;
    logic [31:0] done_value;
    int unsigned cycle_count;

    // Optional watchpoint for a specific global word address.
    // Enable with: +watch_addr=80005db8 (hex)
    logic        watch_en;
    logic [31:0] watch_addr;
    int unsigned watch_prints;
    int unsigned watch_print_limit;

    // Debug: count observed stores into the result block address range.
    int unsigned result_store_count;
    int unsigned local_result_store_count;
    logic [31:0] first_result_store_pc;
    int unsigned first_result_store_cycle;

    int unsigned done_store_count;
    logic [31:0] first_done_store_pc;
    int unsigned first_done_store_cycle;

    // Post-DONE drain: allow buffered stores (e.g., WMB) to reach memory
    // before sampling the result block.
    logic        done_latched;
    int unsigned post_done_cycles;
    localparam int unsigned POST_DONE_MAX_CYCLES = 100000;

    // Debug controls (match coremark_tb style)
    bit          debug_en;
    bit          check_bounds;

    // Print controls
    bit          dump_on_watch;
    bit          dump_regs_on_watch;
    int unsigned print_every;

    // -------------------------
    // Debug formatting helpers
    // -------------------------
    function automatic string reg_class_name(input reg_class_e c);
        unique case (c)
            CLASS_SCALAR: reg_class_name = "S";
            CLASS_FP:     reg_class_name = "F";
            CLASS_VEC:    reg_class_name = "V";
            default:               reg_class_name = "?";
        endcase
    endfunction

    task automatic dump_inst_fields(input string tag, input logic valid, input logic [31:0] inst);
        if (!valid) begin
            $display("DHRY_TB: %s inst: <invalid>", tag);
        end else begin
            $display(
                "DHRY_TB: %s inst=%08h opcode=%02h rd=%0d f3=%0h rs1=%0d rs2=%0d f7=%02h",
                tag,
                inst,
                inst[6:0],
                inst[11:7],
                inst[14:12],
                inst[19:15],
                inst[24:20],
                inst[31:25]
            );
        end
    endtask

    task automatic dump_ctrl(input string tag, input logic valid, input decode_ctrl_t ctrl);
        if (!valid) begin
            $display("DHRY_TB: %s ctrl: <invalid>", tag);
        end else begin
            $display(
                "DHRY_TB: %s ctrl: f3=%0h f7=%02h imm=%08h rs1=%s%0d rs2=%s%0d rd=%s%0d uses(rs1/rs2/rd)=%0d%0d%0d flags[int=%0d fp=%0d vec=%0d vm=%0d sys=%0d tex=%0d gfx=%0d ld=%0d st=%0d br=%0d lui=%0d at=%0d rdv=%0d rdf=%0d]",
                tag,
                ctrl.funct3,
                ctrl.funct7,
                ctrl.imm,
                reg_class_name(ctrl.rs1_class),
                ctrl.rs1,
                reg_class_name(ctrl.rs2_class),
                ctrl.rs2,
                reg_class_name(ctrl.rd_class),
                ctrl.rd,
                ctrl.uses_rs1,
                ctrl.uses_rs2,
                ctrl.uses_rd,
                ctrl.is_scalar_int,
                ctrl.is_scalar_fp,
                ctrl.is_vector,
                ctrl.vm_enable,
                ctrl.is_system,
                ctrl.is_tex,
                ctrl.is_gfx,
                ctrl.is_load,
                ctrl.is_store,
                ctrl.is_branch,
                ctrl.is_lui,
                ctrl.is_atomic,
                ctrl.rd_is_vec,
                ctrl.rd_is_fp
            );
        end
    endtask

    // Catch instruction-fetch going out of the loaded ROM range (commonly indicates a bad redirect)
    bit          ifetch_oob_seen;

    // Did we ever fetch from the translated main() region?
    bit          saw_main_region;
    bit          saw_result_block_region;
    bit          saw_done_store_region;

    // Heuristic: detect a large backward jump into the low-address boot/_start region.
    // We record the *most recent* such jump, since the earliest one can be part of init.
    logic [31:0] prev_if_pc;
    bit          saw_return_to_start;
    int unsigned last_return_to_start_cycle;
    logic [31:0] last_return_from_pc;
    logic [31:0] last_return_to_pc;

    // Recent IFetch PC trace (helps diagnose unexpected returns/redirects)
    localparam int IF_TRACE_DEPTH = 128;
    logic [31:0] if_trace_pc     [0:IF_TRACE_DEPTH-1];
    logic        if_trace_valid  [0:IF_TRACE_DEPTH-1];
    int unsigned if_trace_cycle  [0:IF_TRACE_DEPTH-1];
    logic [$clog2(IF_TRACE_DEPTH)-1:0] if_trace_wp;
    int unsigned if_trace_total;

    // Recent memory request trace (helps debug bad pointer/OOB issues)
    localparam int REQ_TRACE_DEPTH = 256;
    logic [31:0] req_trace_addr   [0:REQ_TRACE_DEPTH-1];
    logic        req_trace_is_ld  [0:REQ_TRACE_DEPTH-1];
    logic [31:0] req_trace_wdata  [0:REQ_TRACE_DEPTH-1];
    logic [4:0]  req_trace_rd     [0:REQ_TRACE_DEPTH-1];
    logic [31:0] req_trace_pc     [0:REQ_TRACE_DEPTH-1];
    int unsigned req_trace_cycle  [0:REQ_TRACE_DEPTH-1];
    logic [$clog2(REQ_TRACE_DEPTH)-1:0] req_trace_wp;
    int unsigned req_trace_total;

    task automatic dump_req_trace();
        $display("DHRY_TB: recent data requests (oldest->newest):");
        for (int k = 0; k < REQ_TRACE_DEPTH; k++) begin
            int idx;
            idx = (req_trace_wp + k) % REQ_TRACE_DEPTH;
            $display(
                "  cyc=%0d pc=%08h %s addr=%08h wdata=%08h rd=%0d",
                req_trace_cycle[idx],
                req_trace_pc[idx],
                req_trace_is_ld[idx] ? "LD" : "ST",
                req_trace_addr[idx],
                req_trace_wdata[idx],
                req_trace_rd[idx]
            );
        end
    endtask

    task automatic dump_ifetch_trace(input int unsigned max_entries);
        int unsigned avail;
        int unsigned count;
        int unsigned start;

        avail = (if_trace_total < IF_TRACE_DEPTH) ? if_trace_total : IF_TRACE_DEPTH;
        count = (max_entries < avail) ? max_entries : avail;
        start = (if_trace_wp + IF_TRACE_DEPTH - count) % IF_TRACE_DEPTH;

        $display("DHRY_TB: recent IFetch PCs (oldest->newest), showing %0d entries:", count);
        for (int k = 0; k < count; k++) begin
            int idx;
            idx = (start + k) % IF_TRACE_DEPTH;
            $display(
                "  cyc=%0d if_valid=%0d if_pc=%08h",
                if_trace_cycle[idx],
                if_trace_valid[idx],
                if_trace_pc[idx]
            );
        end
    endtask

    task automatic dump_status();
        $display(
            "%0t DHRY_TB: if_pc=%08h if_valid=%0d rr_v=%0d ex_v=%0d mem_v=%0d wb_v=%0d stall_any=%0d (sb=%0d lsu_busy=%0d lsu_stall=%0d membar=%0d)",
            $time,
            dut.if_pc,
            dut.if_valid,
            dut.rr_valid,
            dut.ex_valid,
            dut.mem_valid,
            dut.wb_valid,
            dut.stall_any,
            dut.stall_scoreboard,
            dut.lsu_busy,
            dut.lsu_stall,
            dut.stall_membar
        );

        $display(
            "%0t DHRY_TB: IF inst_miss_req=%08h inst0_v=%0d inst0=%08h inst1_v=%0d inst1=%08h pred_taken=%0d pred_tgt=%08h ex_redirect=%0d ex_tgt=%08h",
            $time,
            inst_miss_req_addr,
            dut.if_inst0_valid,
            dut.if_inst0,
            dut.if_inst1_valid,
            dut.if_inst1,
            dut.if_pred_taken,
            dut.if_pred_target,
            dut.ex_redirect_valid,
            dut.ex_redirect_target
        );

        $display(
            "%0t DHRY_TB: issue0_v=%0d accept0=%0d stall_sb0=%0d stall_issue=%0d pc_adv=%0d d0(op=%02h f3=%0h f7=%02h rs1=%0d rs2=%0d rd=%0d uses(rs1,rs2,rd)=%0d%0d%0d)",
            $time,
            dut.issue0_valid,
            dut.accept0,
            dut.stall_sb0,
            dut.stall_issue,
            dut.pc_advance_bytes,
            dut.if_inst0[6:0],
            dut.d0_ctrl.funct3,
            dut.d0_ctrl.funct7,
            dut.d0_ctrl.rs1,
            dut.d0_ctrl.rs2,
            dut.d0_ctrl.rd,
            dut.d0_ctrl.uses_rs1,
            dut.d0_ctrl.uses_rs2,
            dut.d0_ctrl.uses_rd
        );

        $display(
            "%0t DHRY_TB: SB busy_s=%08h busy_f=%08h busy_v=%08h",
            $time,
            dut.u_scoreboard.busy_s,
            dut.u_scoreboard.busy_f,
            dut.u_scoreboard.busy_v
        );
            // LSU internal state/handshake breadcrumbs (helps root-cause hangs).
            $display(
                "%0t DHRY_TB: LSU busy=%0d stall=%0d scalar_wait=%0d state=%0d pending_rd=%0d",
                $time,
                dut.u_lsu.busy,
                dut.u_lsu.stall_pipeline,
                dut.u_lsu.scalar_global_wait,
                dut.u_lsu.state,
                dut.u_lsu.pending_scalar_rd
            );
            $display(
                "%0t DHRY_TB: LSU req_valid=%0d req_ready=%0d arb_valid=%0d arb_ready=%0d adapt_state=%0d",
                $time,
                dut.u_lsu.lsu_internal_req_valid,
                dut.u_lsu.lsu_internal_req_ready,
                dut.u_lsu.arb_req_valid,
                dut.u_lsu.arb_req_ready,
                dut.u_lsu.adapt_state
            );
    endtask

    task automatic dump_scalar_regfile();
        $display("DHRY_TB: scalar regfile (x0..x31):");
        $display(
            "  x0=%08h x1=%08h x2=%08h x3=%08h",
            dut.u_regfile_scalar.mem[0],
            dut.u_regfile_scalar.mem[1],
            dut.u_regfile_scalar.mem[2],
            dut.u_regfile_scalar.mem[3]
        );
        $display(
            "  x4=%08h x5=%08h x6=%08h x7=%08h",
            dut.u_regfile_scalar.mem[4],
            dut.u_regfile_scalar.mem[5],
            dut.u_regfile_scalar.mem[6],
            dut.u_regfile_scalar.mem[7]
        );
        $display(
            "  x8=%08h x9=%08h x10=%08h x11=%08h",
            dut.u_regfile_scalar.mem[8],
            dut.u_regfile_scalar.mem[9],
            dut.u_regfile_scalar.mem[10],
            dut.u_regfile_scalar.mem[11]
        );
        $display(
            "  x12=%08h x13=%08h x14=%08h x15=%08h",
            dut.u_regfile_scalar.mem[12],
            dut.u_regfile_scalar.mem[13],
            dut.u_regfile_scalar.mem[14],
            dut.u_regfile_scalar.mem[15]
        );
        $display(
            "  x16=%08h x17=%08h x18=%08h x19=%08h",
            dut.u_regfile_scalar.mem[16],
            dut.u_regfile_scalar.mem[17],
            dut.u_regfile_scalar.mem[18],
            dut.u_regfile_scalar.mem[19]
        );
        $display(
            "  x20=%08h x21=%08h x22=%08h x23=%08h",
            dut.u_regfile_scalar.mem[20],
            dut.u_regfile_scalar.mem[21],
            dut.u_regfile_scalar.mem[22],
            dut.u_regfile_scalar.mem[23]
        );
        $display(
            "  x24=%08h x25=%08h x26=%08h x27=%08h",
            dut.u_regfile_scalar.mem[24],
            dut.u_regfile_scalar.mem[25],
            dut.u_regfile_scalar.mem[26],
            dut.u_regfile_scalar.mem[27]
        );
        $display(
            "  x28=%08h x29=%08h x30=%08h x31=%08h",
            dut.u_regfile_scalar.mem[28],
            dut.u_regfile_scalar.mem[29],
            dut.u_regfile_scalar.mem[30],
            dut.u_regfile_scalar.mem[31]
        );
    endtask

    task automatic dump_scalar_pipeline();
        logic [31:0] ex_op_b_sel;
        bit d0_is_vec_alu;
        bit d0_is_gfx;
        bit d0_is_scalar_pipe;
        bit d1_is_vec_alu;
        bit d1_is_gfx;
        bit can_dual_raw;
        bit bypass_rs1;
        bit bypass_rs2;
        bit bypass_rr1;
        ex_op_b_sel = (dut.ex_ctrl.is_load || dut.ex_ctrl.is_store || !dut.ex_ctrl.uses_rs2) ? dut.ex_ctrl.imm : dut.ex_op_b;

        d0_is_vec_alu = (dut.if_valid && dut.if_inst0_valid && dut.d0_ctrl.is_vector && !dut.d0_ctrl.is_load && !dut.d0_ctrl.is_store && !dut.d0_ctrl.is_tex && !dut.d0_ctrl.is_atomic);
        d0_is_gfx = (dut.if_valid && dut.if_inst0_valid && (dut.d0_ctrl.is_tex || dut.d0_ctrl.is_gfx));
        d0_is_scalar_pipe = (dut.if_valid && dut.if_inst0_valid && !d0_is_vec_alu && !d0_is_gfx);
        d1_is_vec_alu = (dut.if_valid && dut.if_inst1_valid && dut.d1_ctrl.is_vector && !dut.d1_ctrl.is_load && !dut.d1_ctrl.is_store && !dut.d1_ctrl.is_tex && !dut.d1_ctrl.is_atomic);
        d1_is_gfx = (dut.if_valid && dut.if_inst1_valid && (dut.d1_ctrl.is_tex || dut.d1_ctrl.is_gfx));
        can_dual_raw = (dut.if_valid && dut.if_inst0_valid && dut.if_inst1_valid && d0_is_scalar_pipe && !dut.d0_ctrl.is_branch && !dut.d0_ctrl.is_system && (d1_is_vec_alu || d1_is_gfx));

        bypass_rs1 = (dut.s_we && (dut.s_waddr != 5'd0) && (dut.s_waddr == dut.rr_ctrl.rs1));
        bypass_rs2 = (dut.s_we && (dut.s_waddr != 5'd0) && (dut.s_waddr == dut.rr_ctrl.rs2));
        bypass_rr1 = (dut.s_we && (dut.s_waddr != 5'd0) && (dut.s_waddr == dut.rr1_scalar_raddr));

        $display(
            "DHRY_TB: PIPE STALLS stall_any=%0d stall_pipe=%0d lsu_stall=%0d lsu_busy=%0d stall_membar=%0d sb=%0d (sb0=%0d sb1=%0d) issue0_v=%0d issue1_v=%0d accept0=%0d accept1=%0d",
            dut.stall_any,
            dut.stall_pipe,
            dut.lsu_stall,
            dut.lsu_busy,
            dut.stall_membar,
            dut.stall_scoreboard,
            dut.stall_sb0,
            dut.stall_sb1,
            dut.issue0_valid,
            dut.issue1_valid,
            dut.accept0,
            dut.accept1
        );

        $display(
            "DHRY_TB: PIPE FE   pc_adv=%0d can_dual_raw=%0d d0(vec=%0d gfx=%0d scalar=%0d) d1(vec=%0d gfx=%0d) pred_taken=%0d",
            dut.pc_advance_bytes,
            can_dual_raw,
            d0_is_vec_alu,
            d0_is_gfx,
            d0_is_scalar_pipe,
            d1_is_vec_alu,
            d1_is_gfx,
            dut.if_pred_taken
        );

        $display(
            "DHRY_TB: PIPE IF  valid=%0d pc=%08h inst_miss_req=%08h inst0_v=%0d inst0=%08h inst1_v=%0d inst1=%08h",
            dut.if_valid,
            dut.if_pc,
            inst_miss_req_addr,
            dut.if_inst0_valid,
            dut.if_inst0,
            dut.if_inst1_valid,
            dut.if_inst1
        );

        dump_inst_fields("IF0", dut.if_valid && dut.if_inst0_valid, dut.if_inst0);
        dump_inst_fields("IF1", dut.if_valid && dut.if_inst1_valid, dut.if_inst1);
        dump_ctrl("D0 ", dut.if_valid && dut.if_inst0_valid, dut.d0_ctrl);
        dump_ctrl("D1 ", dut.if_valid && dut.if_inst1_valid, dut.d1_ctrl);

        $display(
            "DHRY_TB: PRED if_pred_taken=%0d if_pred_tgt=%08h rr_pred_taken=%0d rr_pred_tgt=%08h ex_pred_taken=%0d ex_pred_tgt=%08h",
            dut.if_pred_taken,
            dut.if_pred_target,
            dut.rr_pred_taken,
            dut.rr_pred_target,
            dut.ex_pred_taken,
            dut.ex_pred_target
        );

        $display(
            "DHRY_TB: PIPE RR  v=%0d pc=%08h f3=%0h f7=%02h rs1=%0d rs2=%0d rd=%0d imm=%08h is_ld=%0d is_st=%0d is_br=%0d is_lui=%0d is_at=%0d (uses rs1/rs2/rd=%0d%0d%0d)",
            dut.rr_valid,
            dut.rr_pc,
            dut.rr_ctrl.funct3,
            dut.rr_ctrl.funct7,
            dut.rr_ctrl.rs1,
            dut.rr_ctrl.rs2,
            dut.rr_ctrl.rd,
            dut.rr_ctrl.imm,
            dut.rr_ctrl.is_load,
            dut.rr_ctrl.is_store,
            dut.rr_ctrl.is_branch,
            dut.rr_ctrl.is_lui,
            dut.rr_ctrl.is_atomic,
            dut.rr_ctrl.uses_rs1,
            dut.rr_ctrl.uses_rs2,
            dut.rr_ctrl.uses_rd
        );
        dump_ctrl("RR ", dut.rr_valid, dut.rr_ctrl);
        $display(
            "DHRY_TB: RF RD  rr.rs1=x%0d(%s) raw=%08h fwd=%08h  rr.rs2=x%0d(%s) raw=%08h fwd=%08h  (wb_bypass s_we=%0d s_waddr=x%0d s_wdata=%08h)",
            dut.rr_ctrl.rs1,
            reg_class_name(dut.rr_ctrl.rs1_class),
            dut.s_rdata_a,
            dut.s_rdata_a_fwd,
            dut.rr_ctrl.rs2,
            reg_class_name(dut.rr_ctrl.rs2_class),
            dut.s_rdata_b,
            dut.s_rdata_b_fwd,
            dut.s_we,
            dut.s_waddr,
            dut.s_wdata
        );
        $display(
            "DHRY_TB: RF BYP  hit_rs1=%0d hit_rs2=%0d hit_rr1=%0d",
            bypass_rs1,
            bypass_rs2,
            bypass_rr1
        );

        $display(
            "DHRY_TB: PIPE RR1 v=%0d f3=%0h f7=%02h rs1=%0d rs2=%0d rd=%0d imm=%08h is_ld=%0d is_st=%0d is_br=%0d",
            dut.rr1_valid,
            dut.rr1_ctrl.funct3,
            dut.rr1_ctrl.funct7,
            dut.rr1_ctrl.rs1,
            dut.rr1_ctrl.rs2,
            dut.rr1_ctrl.rd,
            dut.rr1_ctrl.imm,
            dut.rr1_ctrl.is_load,
            dut.rr1_ctrl.is_store,
            dut.rr1_ctrl.is_branch
        );
        dump_ctrl("RR1", dut.rr1_valid, dut.rr1_ctrl);
        $display(
            "DHRY_TB: RF RD1 rr1_scalar_raddr=x%0d raw=%08h fwd=%08h",
            dut.rr1_scalar_raddr,
            dut.s_rdata_c,
            dut.s_rdata_c_fwd
        );

        $display(
            "DHRY_TB: PIPE ISSUE v=%0d op_a=%08h op_b=%08h (ctrl f3=%0h f7=%02h rd=%0d is_ld=%0d is_st=%0d)",
            dut.gp_issue_valid,
            dut.gp_issue_op_a,
            dut.gp_issue_op_b,
            dut.gp_issue_ctrl.funct3,
            dut.gp_issue_ctrl.funct7,
            dut.gp_issue_ctrl.rd,
            dut.gp_issue_ctrl.is_load,
            dut.gp_issue_ctrl.is_store
        );
        if (dut.gp_issue_valid) begin
            dump_ctrl("ISS", 1'b1, dut.gp_issue_ctrl);
        end

        $display(
            "DHRY_TB: PIPE EX  v=%0d pc=%08h op_a=%08h op_b=%08h op_b_sel=%08h addr=%08h alu=%08h scalar_res=%08h is_ld=%0d is_st=%0d",
            dut.ex_valid,
            dut.ex_pc,
            dut.ex_op_a,
            dut.ex_op_b,
            ex_op_b_sel,
            dut.ex_addr,
            dut.ex_alu_res,
            dut.ex_scalar_res,
            dut.ex_ctrl.is_load,
            dut.ex_ctrl.is_store
        );
        dump_ctrl("EX ", dut.ex_valid, dut.ex_ctrl);
        $display(
            "DHRY_TB: ALU int: funct3=%0h is_sub=%0d (funct7=%02h)  br_resolved_taken=%0d br_tgt=%08h redirect=%0d redir_tgt=%08h",
            dut.ex_ctrl.funct3,
            (dut.ex_ctrl.funct7 == 7'b0100000),
            dut.ex_ctrl.funct7,
            dut.ex_cf_taken,
            dut.ex_cf_target,
            dut.ex_redirect_valid,
            dut.ex_redirect_target
        );

        $display(
            "DHRY_TB: PIPE MEM v=%0d pc=%08h addr=%08h wdata=%08h f3=%0h scalar_res=%08h (req_v=%0d is_ld=%0d)",
            dut.mem_valid,
            dut.mem_pc,
            dut.mem_addr,
            dut.mem_scalar_wdata,
            dut.mem_ctrl.funct3,
            dut.mem_scalar_res,
            data_req_valid,
            data_req_is_load
        );
        dump_ctrl("MEM", dut.mem_valid, dut.mem_ctrl);
        $display(
            "DHRY_TB: MEM IFACE req_v=%0d ready=%0d is_ld=%0d addr=%08h wdata=%08h rd=%0d resp_v=%0d resp_rd=%0d resp_data=%08h",
            data_req_valid,
            data_req_ready,
            data_req_is_load,
            data_req_addr,
            data_req_wdata,
            data_req_rd,
            data_resp_valid,
            data_resp_rd,
            data_resp_data
        );

        $display(
            "DHRY_TB: LSU INT  in_v=%0d in_store=%0d in_addr=%08h in_f3=%0h in_wdata=%08h busy=%0d stall_pipe=%0d state=%0d pending_rd=%0d scalar_wait=%0d",
            dut.u_lsu.valid_in,
            dut.u_lsu.is_store,
            dut.u_lsu.addr,
            dut.u_lsu.scalar_funct3,
            dut.u_lsu.write_data[31:0],
            dut.u_lsu.busy,
            dut.u_lsu.stall_pipeline,
            dut.u_lsu.state,
            dut.u_lsu.pending_scalar_rd,
            dut.u_lsu.scalar_global_wait
        );

        $display(
            "DHRY_TB: LSU RMW  rmw_state=%0d f3=%0h addr=%08h st_data=%08h old=%08h new=%08h ld_issued=%0d",
            dut.u_lsu.rmw_state,
            dut.u_lsu.rmw_funct3,
            dut.u_lsu.rmw_addr,
            dut.u_lsu.rmw_store_data,
            dut.u_lsu.rmw_old_word,
            dut.u_lsu.rmw_new_word,
            dut.u_lsu.rmw_load_issued
        );

        $display(
            "DHRY_TB: LSU LOC  local_v=%0d local_we=%0d local_addr=%08h local_bank=%0d",
            dut.u_lsu.local_req_valid,
            dut.u_lsu.local_we,
            dut.u_lsu.local_addr,
            dut.u_lsu.local_bank_sel
        );

        $display(
            "DHRY_TB: PIPE WB  v=%0d rd=%0d scalar_res=%08h s_we=%0d s_waddr=%0d s_wdata=%08h",
            dut.wb_valid,
            dut.wb_ctrl.rd,
            dut.wb_scalar_res,
            dut.s_we,
            dut.s_waddr,
            dut.s_wdata
        );
        dump_ctrl("WB ", dut.wb_valid, dut.wb_ctrl);
    endtask

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            inst_miss_resp_valid <= 1'b0;
            inst_miss_resp_data  <= 64'h0;
            inst_pending         <= 1'b0;
            inst_req_addr_q      <= 32'h0;

            dcache_mem_resp_valid <= 1'b0;
            dcache_mem_resp_data  <= 64'h0;
            dcache_mem_resp_id    <= 8'h0;
            dcache_tx_active      <= 1'b0;
            dcache_tx_rw          <= 1'b0;
            dcache_tx_addr        <= 32'h0;
            dcache_tx_id_q        <= 8'h0;
            dcache_tx_beat        <= 3'd0;

            done_seen   <= 1'b0;
            done_value  <= 32'h0;
            result_store_count <= 0;
            local_result_store_count <= 0;
            first_result_store_pc <= 32'h0;
            first_result_store_cycle <= 0;
            done_store_count <= 0;
            first_done_store_pc <= 32'h0;
            first_done_store_cycle <= 0;
            done_latched <= 1'b0;
            post_done_cycles <= 0;
            cycle_count <= 0;
            req_trace_wp <= '0;
            req_trace_total <= 0;
            for (int k = 0; k < REQ_TRACE_DEPTH; k++) begin
                req_trace_addr[k]  <= 32'h0;
                req_trace_is_ld[k] <= 1'b0;
                req_trace_wdata[k] <= 32'h0;
                req_trace_rd[k]    <= '0;
                req_trace_pc[k]    <= 32'h0;
                req_trace_cycle[k] <= 0;
            end
            ifetch_oob_seen <= 1'b0;
            saw_main_region <= 1'b0;
            saw_result_block_region <= 1'b0;
            saw_done_store_region <= 1'b0;

            prev_if_pc <= 32'h0;
            saw_return_to_start <= 1'b0;
            last_return_to_start_cycle <= 0;
            last_return_from_pc <= 32'h0;
            last_return_to_pc <= 32'h0;

            if_trace_wp <= '0;
            if_trace_total <= 0;
            for (int k = 0; k < IF_TRACE_DEPTH; k++) begin
                if_trace_pc[k] <= 32'h0;
                if_trace_valid[k] <= 1'b0;
                if_trace_cycle[k] <= 0;
            end

            watch_prints <= 0;

            // If waveform dumping is enabled but configured to start later,
            // ensure we are not dumping during reset.
            if (waves_en && (waves_start_cycle != 0)) begin
                $dumpoff;
            end
        end else begin
            cycle_count <= cycle_count + 1;

            // Periodic debug snapshots (disabled by default)
            if (debug_en && (print_every != 0) && ((cycle_count % print_every) == 0)) begin
                $display("%0t DHRY_TB: periodic snapshot cyc=%0d", $time, cycle_count);
                dump_status();
                dump_scalar_pipeline();
            end

            // Start waveform dumping at a chosen cycle.
            if (waves_en && (waves_start_cycle != 0) && (cycle_count == waves_start_cycle)) begin
                $display("%0t DHRY_TB: enabling waves at cyc=%0d", $time, cycle_count);
                $dumpon;
            end

            // Record IFetch PCs to help debug control-flow anomalies.
            if_trace_pc[if_trace_wp] <= dut.if_pc;
            if_trace_valid[if_trace_wp] <= dut.if_valid;
            if_trace_cycle[if_trace_wp] <= cycle_count;
            if_trace_wp <= if_trace_wp + 1;
            if_trace_total <= if_trace_total + 1;

            if ((dut.if_pc >= 32'h0000_0EB4) && (dut.if_pc < 32'h0000_10B0)) begin
                saw_main_region <= 1'b1;
            end
            if ((dut.if_pc >= 32'h0000_1040) && (dut.if_pc < 32'h0000_1098)) begin
                saw_result_block_region <= 1'b1;
            end
            if ((dut.if_pc >= 32'h0000_1098) && (dut.if_pc < 32'h0000_10B0)) begin
                saw_done_store_region <= 1'b1;
            end

            // Detect a big jump from high PC down into the low boot region.
            if ((prev_if_pc >= 32'h0000_0800) && (dut.if_pc < 32'h0000_0200)) begin
                saw_return_to_start <= 1'b1;
                last_return_to_start_cycle <= cycle_count;
                last_return_from_pc <= prev_if_pc;
                last_return_to_pc <= dut.if_pc;
            end
            prev_if_pc <= dut.if_pc;

            if (done_seen && !done_latched) begin
                done_latched <= 1'b1;
                post_done_cycles <= 0;
            end else if (done_latched) begin
                post_done_cycles <= post_done_cycles + 1;
            end

            // --------------------------------------------------
            // I-cache miss responder (1-cycle latency, 64-bit)
            // --------------------------------------------------
            if (inst_miss_req_valid && inst_miss_req_ready) begin
                inst_pending    <= 1'b1;
                inst_req_addr_q <= inst_miss_req_addr;
            end

            if (inst_pending) begin
                inst_miss_resp_valid <= 1'b1;
                inst_miss_resp_data  <= {rom_word_at(inst_req_addr_q + 32'd4),
                                         rom_word_at(inst_req_addr_q)};
                inst_pending <= 1'b0;

                if (check_bounds && !ifetch_oob_seen) begin
                    int unsigned if_idx;
                    if_idx = inst_req_addr_q >> 2;
                    if (if_idx >= ROM_WORDS) begin
                        ifetch_oob_seen <= 1'b1;
                        $display(
                            "%0t DHRY_TB: IFETCH OOB inst_req_addr=%08h (idx=%0d ROM_WORDS=%0d) if_pc=%08h",
                            $time,
                            inst_req_addr_q,
                            if_idx,
                            ROM_WORDS,
                            dut.if_pc
                        );
                        dump_status();
                        dump_req_trace();
                        dump_ifetch_trace(80);
                        $fatal(1);
                    end
                end
            end else begin
                inst_miss_resp_valid <= 1'b0;
            end

            // --------------------------------------------------
            // D-cache memory responder (8-beat reads / 1-shot writes)
            // --------------------------------------------------
            if (!dcache_tx_active) begin
                dcache_mem_resp_valid <= 1'b0;

                if (dcache_mem_req_valid && dcache_mem_req_ready) begin
                    if (dcache_mem_req_rw == 1'b0) begin
                        // READ: start 8-beat burst
                        dcache_tx_active <= 1'b1;
                        dcache_tx_rw     <= 1'b0;
                        dcache_tx_addr   <= {dcache_mem_req_addr[31:6], 6'd0};
                        dcache_tx_id_q   <= dcache_mem_req_id;
                        dcache_tx_beat   <= 3'd0;
                        // First beat in same cycle
                        dcache_mem_resp_valid <= 1'b1;
                        dcache_mem_resp_data  <= dcache_read_beat({dcache_mem_req_addr[31:6], 6'd0}, 3'd0);
                        dcache_mem_resp_id    <= dcache_mem_req_id;
                    end else begin
                        // WRITE: commit to mem[] and respond immediately
                        dcache_write_line(
                            {dcache_mem_req_addr[31:6], 6'd0},
                            dcache_mem_req_wdata,
                            dcache_mem_req_wstrb
                        );
                        dcache_mem_resp_valid <= 1'b1;
                        dcache_mem_resp_data  <= 64'h0;
                        dcache_mem_resp_id    <= dcache_mem_req_id;

                        // Record trace for the write
                        req_trace_addr[req_trace_wp]  <= dcache_mem_req_addr;
                        req_trace_is_ld[req_trace_wp] <= 1'b0;
                        req_trace_wdata[req_trace_wp] <= dcache_mem_req_wdata[31:0];
                        req_trace_rd[req_trace_wp]    <= '0;
                        req_trace_pc[req_trace_wp]    <= dut.if_pc;
                        req_trace_cycle[req_trace_wp] <= cycle_count;
                        req_trace_wp <= req_trace_wp + 1'b1;
                        req_trace_total <= req_trace_total + 1;

                        if (debug_en) begin
                            $display("%0t DHRY_TB: DCACHE WR addr=%08h wstrb=%02h cyc=%0d",
                                     $time, dcache_mem_req_addr, dcache_mem_req_wstrb, cycle_count);
                        end

                        // Check if this write touches the DONE address
                        begin
                            logic [31:0] line_base;
                            logic [31:0] done_addr;
                            line_base = {dcache_mem_req_addr[31:6], 6'd0};
                            done_addr = base_addr + done_off;
                            if ((done_addr >= line_base) && (done_addr < (line_base + 32'd64))) begin
                                int w_idx;
                                w_idx = mem_index(done_addr);
                                if ((w_idx >= 0) && (w_idx < MEM_WORDS)) begin
                                    done_store_count <= done_store_count + 1;
                                    if (done_store_count == 0) begin
                                        first_done_store_pc <= dut.mem_pc;
                                        first_done_store_cycle <= cycle_count;
                                    end
                                    done_seen  <= 1'b1;
                                    done_value <= mem[w_idx];
                                    $display("%0t DHRY_TB: DONE write value=%08h (via D-cache writeback)", $time, mem[w_idx]);
                                end
                            end
                        end

                        // Check for result block writes
                        begin
                            logic [31:0] line_base;
                            logic [31:0] res_start, res_end;
                            line_base = {dcache_mem_req_addr[31:6], 6'd0};
                            res_start = base_addr + result_off;
                            res_end   = res_start + 32'd32;
                            if ((res_start < (line_base + 32'd64)) && (res_end > line_base)) begin
                                result_store_count <= result_store_count + 1;
                                if (result_store_count == 0) begin
                                    first_result_store_pc <= dut.mem_pc;
                                    first_result_store_cycle <= cycle_count;
                                end
                                if (debug_en) begin
                                    $display("%0t DHRY_TB: RESULT store (D-cache line) cyc=%0d addr=%08h",
                                             $time, cycle_count, dcache_mem_req_addr);
                                end
                            end
                        end

                        // Watchpoint on D-cache writes
                        if (watch_en && (watch_prints < watch_print_limit)) begin
                            logic [31:0] line_base;
                            line_base = {dcache_mem_req_addr[31:6], 6'd0};
                            if ((watch_addr >= line_base) && (watch_addr < (line_base + 32'd64))) begin
                                $display(
                                    "%0t DHRY_TB: WATCH DCACHE_WR cyc=%0d addr=%08h line=%08h wstrb=%02h mem[watch]=%08h",
                                    $time, cycle_count, dcache_mem_req_addr, line_base,
                                    dcache_mem_req_wstrb, mem_word_at(watch_addr)
                                );
                                watch_prints <= watch_prints + 1;
                            end
                        end
                    end
                end
            end else begin
                // Ongoing read burst
                if (dcache_tx_beat < 3'd7) begin
                    dcache_tx_beat <= dcache_tx_beat + 3'd1;
                    dcache_mem_resp_valid <= 1'b1;
                    dcache_mem_resp_data  <= dcache_read_beat(dcache_tx_addr, dcache_tx_beat + 3'd1);
                    dcache_mem_resp_id    <= dcache_tx_id_q;
                end else begin
                    // Last beat was delivered; finish transaction
                    dcache_tx_active      <= 1'b0;
                    dcache_mem_resp_valid <= 1'b0;
                end
            end

            // Registerfile writeback trace (scalar only)
            if (debug_en && dut.s_we && (dut.s_waddr != 5'd0)) begin
                $display(
                    "%0t DHRY_TB: RF WB cyc=%0d if_pc=%08h rd=x%0d data=%08h",
                    $time,
                    cycle_count,
                    dut.if_pc,
                    dut.s_waddr,
                    dut.s_wdata
                );
            end

            // If something is accidentally being treated as local memory (addr[31]==0),
            // count any local stores into the result-block range (0x00000200..0x0000021f).
            if (dut.u_lsu.local_req_valid && dut.u_lsu.local_we) begin
                if ((dut.u_lsu.local_addr >= result_off) && (dut.u_lsu.local_addr < (result_off + 32'd32))) begin
                    local_result_store_count <= local_result_store_count + 1;
                    if (debug_en) begin
                        $display(
                            "%0t DHRY_TB: LOCAL_RESULT store cyc=%0d addr=%08h wdata[31:0]=%08h",
                            $time,
                            cycle_count,
                            dut.u_lsu.local_addr,
                            dut.u_lsu.local_wdata[31:0]
                        );
                    end
                end
            end

            if (!done_seen && (cycle_count >= max_cycles)) begin
                $display("%0t DHRY_TB: TIMEOUT max_cycles=%0d", $time, max_cycles);
                dump_status();
                dump_scalar_pipeline();
                dump_scalar_regfile();
                dump_req_trace();
                dump_ifetch_trace(80);
                $fatal(1);
            end
        end
    end

    // -------------------------
    // Init (ROM + RAM) from +args
    // -------------------------
    string rom_file;
    string mem_file;
    initial begin
        base_addr  = 32'h8000_0000;
        done_off   = 32'h0000_01F0;
        result_off = 32'h0000_0200;
        max_cycles = 50_000_000;

        debug_en     = 1'b0;
        check_bounds = 1'b0;

        dump_on_watch       = 1'b0;
        dump_regs_on_watch  = 1'b0;
        print_every         = 0;

        waves_en            = 1'b0;
        vcd_file            = "dhrystone_tb.vcd";
        waves_depth         = 0;
        waves_start_cycle   = 0;

        watch_en          = 1'b0;
        watch_addr        = 32'h0;
        watch_prints      = 0;
        watch_print_limit = 80;

        void'($value$plusargs("base=%h", base_addr));
        void'($value$plusargs("done_off=%h", done_off));
        void'($value$plusargs("result_off=%h", result_off));
        void'($value$plusargs("max_cycles=%d", max_cycles));
        void'($value$plusargs("debug=%d", debug_en));
        void'($value$plusargs("check_bounds=%d", check_bounds));

        void'($value$plusargs("dump_on_watch=%d", dump_on_watch));
        void'($value$plusargs("dump_regs_on_watch=%d", dump_regs_on_watch));
        void'($value$plusargs("print_every=%d", print_every));

        void'($value$plusargs("waves=%d", waves_en));
        void'($value$plusargs("vcd=%s", vcd_file));
        void'($value$plusargs("waves_depth=%d", waves_depth));
        void'($value$plusargs("waves_start=%d", waves_start_cycle));

`ifndef VERILATOR
        if (waves_en) begin
            $display("DHRY_TB: waves enabled: file=%s depth=%0d start_cycle=%0d", vcd_file, waves_depth, waves_start_cycle);
            $dumpfile(vcd_file);
            $dumpvars(waves_depth, dhrystone_tb);
            if (waves_start_cycle != 0) begin
                $dumpoff;
            end
        end
`endif

        // Watchpoint parsing: Verilator's $value$plusargs return code can be
        // quirky for some %h usages. Parse the value as a string first, then
        // convert via $sscanf.
        begin
            string watch_addr_str;
            int n;
            watch_addr_str = "";
            if ($value$plusargs("watch_addr=%s", watch_addr_str)) begin
                n = $sscanf(watch_addr_str, "%h", watch_addr);
                if (n == 1) begin
                    watch_en = 1'b1;
                end else begin
                    $display("DHRY_TB: ERROR: could not parse +watch_addr=%s", watch_addr_str);
                    $fatal(1);
                end
            end
        end

        rom_file = "";
        mem_file = "";
        void'($value$plusargs("rom=%s", rom_file));
        void'($value$plusargs("mem_init=%s", mem_file));

        // Default ROM fill must be a tinyGPU NOP; 0x00000013 decodes as OP_CUSTOM0 in tinyGPU.
        for (int i = 0; i < ROM_WORDS; i++) rom[i] = 32'h0000_0017;
        for (int i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;

        if (rom_file == "") begin
            $display("DHRY_TB: ERROR: missing +rom=... (tinyGPU .memh)");
            $fatal(1);
        end

        $display("DHRY_TB: loading ROM from %s", rom_file);
        $readmemh(rom_file, rom);

        if (mem_file != "") begin
            $display("DHRY_TB: loading MEM from %s", mem_file);
            $readmemh(mem_file, mem);
        end

        $display("DHRY_TB: base=%08h done_off=%08h result_off=%08h max_cycles=%0d debug=%0d check_bounds=%0d", base_addr, done_off, result_off, max_cycles, debug_en, check_bounds);
        if (watch_en) begin
            $display("DHRY_TB: watch_addr=%08h (printing first %0d hits)", watch_addr, watch_print_limit);
        end
        if (dump_on_watch) begin
            $display("DHRY_TB: dump_on_watch=%0d dump_regs_on_watch=%0d", dump_on_watch, dump_regs_on_watch);
        end
        if (print_every != 0) begin
            $display("DHRY_TB: print_every=%0d", print_every);
        end
    end

    task automatic dump_result_block();
        $display("DHRY_TB: result[0]=%08h", mem[mem_index(base_addr + result_off + 32'd0)]);
        $display("DHRY_TB: result[1]=%08h", mem[mem_index(base_addr + result_off + 32'd4)]);
        $display("DHRY_TB: result[2]=%08h", mem[mem_index(base_addr + result_off + 32'd8)]);
        $display("DHRY_TB: result[3]=%08h", mem[mem_index(base_addr + result_off + 32'd12)]);
        $display("DHRY_TB: result[4]=%08h", mem[mem_index(base_addr + result_off + 32'd16)]);
        $display("DHRY_TB: result[5]=%08h", mem[mem_index(base_addr + result_off + 32'd20)]);
        $display("DHRY_TB: result[6]=%08h", mem[mem_index(base_addr + result_off + 32'd24)]);
        $display("DHRY_TB: result[7]=%08h", mem[mem_index(base_addr + result_off + 32'd28)]);
    endtask

    // End simulation on DONE, but verify the self-check result word.
    // NOTE: Stores are buffered/merged by the LSU write-merge buffer and may not
    // be globally visible in program order immediately when DONE is observed.
    // Wait for the WMB to drain (or a small timeout) before sampling result.
    always_ff @(posedge clk) begin
        if (rst_n && done_seen) begin
            if (!done_latched) begin
                // Latching is handled in the main always_ff; just wait.
            end

            if (done_latched) begin
                bit wmb_idle;
                wmb_idle = (dut.u_lsu.wmb_busy == 1'b0) && (dut.u_lsu.u_wmb.busy == 1'b0);

                if (!wmb_idle && (post_done_cycles < POST_DONE_MAX_CYCLES)) begin
                    // keep simulating until stores drain
                end else begin
            logic [31:0] r0;
                    r0 = mem[mem_index(base_addr + result_off)];

                    if (r0 !== 32'h0000_0001) begin
                        $display("DHRY_TB: FAIL (done=%08h cycles=%0d post_done=%0d wmb_idle=%0d)", done_value, cycle_count, post_done_cycles, wmb_idle);
                        $display("DHRY_TB: result_store_count=%0d", result_store_count);
                        $display("DHRY_TB: local_result_store_count=%0d", local_result_store_count);
                        $display("DHRY_TB: done_store_count=%0d", done_store_count);
                        $display("DHRY_TB: first_done_store_pc=%08h first_done_store_cycle=%0d", first_done_store_pc, first_done_store_cycle);
                        $display("DHRY_TB: first_result_store_pc=%08h first_result_store_cycle=%0d", first_result_store_pc, first_result_store_cycle);
                        $display("DHRY_TB: saw_main_region=%0d", saw_main_region);
                        $display("DHRY_TB: saw_result_block_region=%0d", saw_result_block_region);
                        $display("DHRY_TB: saw_done_store_region=%0d", saw_done_store_region);
                        $display("DHRY_TB: saw_return_to_start=%0d", saw_return_to_start);
                        if (saw_return_to_start) begin
                            $display("DHRY_TB: last_return_to_start cyc=%0d from_pc=%08h to_pc=%08h", last_return_to_start_cycle, last_return_from_pc, last_return_to_pc);
                        end
                        dump_scalar_pipeline();
                        dump_scalar_regfile();
                        dump_req_trace();
                        dump_ifetch_trace(80);
                        dump_result_block();
                        $fatal(1);
                    end

                    $display("DHRY_TB: PASS (cycles=%0d post_done=%0d)", cycle_count, post_done_cycles);
                    if (debug_en) begin
                        dump_result_block();
                    end
                    $finish;
                end
            end
        end
    end

endmodule
