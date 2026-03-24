`timescale 1ns/1ps

// Comprehensive testbench for compute_unit_top
module compute_unit_tb;
    import isa_pkg::*;

    // Clock/reset
    logic clk;
    logic rst_n;

    // I-cache miss interface
    logic        inst_miss_req_valid;
    logic [31:0] inst_miss_req_addr;
    logic        inst_miss_req_ready;
    logic        inst_miss_resp_valid;
    logic [63:0] inst_miss_resp_data;

    // Debug mirrors
    logic [63:0] inst_rdata;
    logic [31:0] inst_addr;

    // Legacy data interface (unused, kept for port tie-off)
    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;
    logic        data_req_ready;
    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    // D-cache memory side interface
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

    // Mailbox stream (tied off)
    import mailbox_pkg::*;
    mailbox_pkg::mailbox_flit_t mailbox_tx_data;
    mailbox_pkg::mailbox_flit_t mailbox_rx_data;
    logic mailbox_tx_valid;
    logic mailbox_tx_ready;
    logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_tx_dest_id;
    logic mailbox_rx_valid;
    logic mailbox_rx_ready;
    logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_rx_dest_id;

    // CSR sideband (optional visibility)
    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    // Error flags
    logic err_fp_overflow;
    logic err_fp_invalid;
    logic err_vec_overflow;
    logic err_vec_invalid;

    // DUT instance (parameterized core/tile IDs)
    compute_unit_top #(
        .CORE_ID(32'h1),
        .TILE_OFFSET(32'h0001_0002)
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

    // Clock generation: 100MHz
    initial begin
        clk = 1'b0;
        forever #5 clk = ~clk;
    end

    // Reset sequence
    initial begin
        rst_n = 1'b0;
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
    end

    // Tie off unused interfaces
    assign data_req_ready  = 1'b1;
    assign data_resp_valid = 1'b0;
    assign data_resp_rd    = 5'd0;
    assign data_resp_data  = 32'h0;
    assign fb_aw_ready     = 1'b1;
    assign fb_w_ready      = 1'b1;
    assign fb_b_valid      = 1'b0;
    assign mailbox_tx_ready   = 1'b1;
    assign mailbox_rx_valid   = 1'b0;
    assign mailbox_rx_data    = '0;
    assign mailbox_rx_dest_id = '0;

    // Waveform dump for debugging
    initial begin
        $dumpfile("compute_unit_tb.vcd");
        $dumpvars(0, compute_unit_tb);
    end

    // ------------------------------------------------------------------
    // Instruction ROM (word-addressable)
    // ------------------------------------------------------------------
    localparam int ROM_WORDS = 128;
    logic [31:0] rom [0:ROM_WORDS-1];

    // I-cache miss handler (1-cycle latency)
    assign inst_miss_req_ready = 1'b1;
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            inst_miss_resp_valid <= 1'b0;
            inst_miss_resp_data  <= 64'h0;
            inst_addr            <= 32'h0;
            inst_rdata           <= 64'h0;
        end else begin
            inst_miss_resp_valid <= 1'b0;
            if (inst_miss_req_valid && inst_miss_req_ready) begin
                automatic logic [5:0] pair_idx;
                automatic logic [6:0] idx0, idx1;
                pair_idx = inst_miss_req_addr[8:3];
                idx0 = {pair_idx, 1'b0};
                idx1 = {pair_idx, 1'b1};
                inst_addr  <= inst_miss_req_addr;
                inst_rdata <= {rom[idx1], rom[idx0]};
                inst_miss_resp_data  <= {rom[idx1], rom[idx0]};
                inst_miss_resp_valid <= 1'b1;
            end
        end
    end

    // ------------------------------------------------------------------
    // Global memory model (backing store for D-cache)
    // ------------------------------------------------------------------
    localparam int MEM_WORDS = 4096; // 16KB global memory
    localparam logic [31:0] BASE_ADDR = 32'hFFFF_F800;
    localparam int DCACHE_BEATS = 8;

    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - BASE_ADDR) >> 2;
    endfunction

    function automatic logic [63:0] dcache_read_beat(input logic [31:0] base, input logic [2:0] beat);
        automatic int idx = ((base & ~32'h3F) - BASE_ADDR) >> 2;
        dcache_read_beat = {mem[idx + beat*2 + 1], mem[idx + beat*2]};
    endfunction

    task automatic dcache_write_line(input logic [31:0] base, input logic [511:0] wdata, input logic [7:0] wstrb);
        automatic int idx = ((base & ~32'h3F) - BASE_ADDR) >> 2;
        for (int b = 0; b < 8; b++) begin
            if (wstrb[b]) begin
                mem[idx + b*2]     = wdata[b*64 +: 32];
                mem[idx + b*2 + 1] = wdata[b*64 + 32 +: 32];
            end
        end
    endtask

    // D-cache memory responder (multi-beat reads, single-cycle writes)
    assign dcache_mem_req_ready = 1'b1;
    logic        dcache_tx_active;
    logic        dcache_tx_rw;
    logic [31:0] dcache_tx_addr;
    logic [7:0]  dcache_tx_id_q;
    logic [2:0]  dcache_tx_beat;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            dcache_tx_active      <= 1'b0;
            dcache_tx_rw          <= 1'b0;
            dcache_tx_addr        <= 32'h0;
            dcache_tx_id_q        <= 8'h0;
            dcache_tx_beat        <= 3'd0;
            dcache_mem_resp_valid <= 1'b0;
            dcache_mem_resp_data  <= 64'h0;
            dcache_mem_resp_id    <= 8'h0;
        end else begin
            dcache_mem_resp_valid <= 1'b0;

            if (dcache_mem_req_valid && dcache_mem_req_ready) begin
                dcache_tx_active <= !dcache_mem_req_rw;
                dcache_tx_rw     <= dcache_mem_req_rw;
                dcache_tx_addr   <= dcache_mem_req_addr;
                dcache_tx_id_q   <= dcache_mem_req_id;
                dcache_tx_beat   <= 3'd0;

                if (dcache_mem_req_rw) begin
                    dcache_write_line(dcache_mem_req_addr, dcache_mem_req_wdata, dcache_mem_req_wstrb);
                    dcache_mem_resp_valid <= 1'b1;
                    dcache_mem_resp_id    <= dcache_mem_req_id;
                    dcache_mem_resp_data  <= 64'h0;
                end
            end else if (dcache_tx_active) begin
                dcache_mem_resp_valid <= 1'b1;
                dcache_mem_resp_id    <= dcache_tx_id_q;
                dcache_mem_resp_data  <= dcache_read_beat(dcache_tx_addr, dcache_tx_beat);

                if (dcache_tx_beat == DCACHE_BEATS-1) begin
                    dcache_tx_active <= 1'b0;
                end else begin
                    dcache_tx_beat <= dcache_tx_beat + 1'b1;
                end
            end
        end
    end

    // ------------------------------------------------------------------
    // Mini assembler helpers (RV-like encoding with custom opcode map)
    // ------------------------------------------------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_IMM); // ADDI x0, x0, 0
    endfunction

    // --- RVV encoding helpers ---
    function automatic [31:0] rvv_vl(input [4:0] vd, input [4:0] rs1, input [2:0] width, input vm);
        rvv_vl = {3'b000, 1'b0, 2'b00, vm, 5'b00000, rs1, width, vd, 7'b0000111};
    endfunction
    function automatic [31:0] rvv_vs(input [4:0] vs3, input [4:0] rs1, input [2:0] width, input vm);
        rvv_vs = {3'b000, 1'b0, 2'b00, vm, 5'b00000, rs1, width, vs3, 7'b0100111};
    endfunction
    function automatic [31:0] vsetvli_inst(input [4:0] rd, input [4:0] rs1, input [10:0] zimm);
        vsetvli_inst = {1'b0, zimm, rs1, 3'b111, rd, 7'b1010111};
    endfunction

    // ------------------------------------------------------------------
    // Program image
    // ------------------------------------------------------------------
    initial begin
        // pc tracks byte addresses; rom is word-addressable
        automatic int pc;
        for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();

        pc = 0;
        // x1 = BASE_ADDR (ADDI x1, x0, -2048)
        rom[pc >> 2] = i_type(-2048, 5'd0, 3'b000, 5'd1, OP_IMM);
        pc += 4;
        // x2 = 5; x3 = 7
        rom[pc >> 2] = i_type(5, 5'd0, 3'b000, 5'd2, OP_IMM);
        pc += 4;
        rom[pc >> 2] = i_type(7, 5'd0, 3'b000, 5'd3, OP_IMM);
        pc += 4;
        // x4 = x2 + x3 = 12
        rom[pc >> 2] = r_type(7'b0000000, 5'd3, 5'd2, 3'b000, 5'd4, OP_REG);
        pc += 4;
        // SW x4, 0(x1)
        rom[pc >> 2] = s_type(0, 5'd1, 5'd4, 3'b010, OP_STORE);
        pc += 4;
        // LW x5, 0(x1)
        rom[pc >> 2] = i_type(0, 5'd1, 3'b010, 5'd5, OP_LOAD);
        pc += 4;
        // SW x5, 8(x1)
        rom[pc >> 2] = s_type(8, 5'd1, 5'd5, 3'b010, OP_STORE);
        pc += 4;
        // x6 = 7
        rom[pc >> 2] = i_type(7, 5'd0, 3'b000, 5'd6, OP_IMM);
        pc += 4;
        // CSRRW status (imm=0) with x6 -> rd=x7
        rom[pc >> 2] = i_type(0, 5'd6, 3'b001, 5'd7, OP_SYSTEM);
        pc += 4;
        // CSRRS status read into x8 using x0 (no set)
        rom[pc >> 2] = i_type(0, 5'd0, 3'b010, 5'd8, OP_SYSTEM);
        pc += 4;
        // MEMBAR
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM);
        pc += 4;
        // vsetvli x0, x0, e32,m1 (SEW=32, VL=4)
        rom[pc >> 2] = vsetvli_inst(5'd0, 5'd0, 11'h010);
        pc += 4;
        // VLE32.V v1, (x1+16)
        rom[pc >> 2] = i_type(16, 5'd1, 3'b000, 5'd2, OP_IMM);
        pc += 4;
        rom[pc >> 2] = rvv_vl(5'd1, 5'd2, RVV_VEW32, 1'b1);
        pc += 4;
        // VLE32.V v2, (x1+32)
        rom[pc >> 2] = i_type(32, 5'd1, 3'b000, 5'd2, OP_IMM);
        pc += 4;
        rom[pc >> 2] = rvv_vl(5'd2, 5'd2, RVV_VEW32, 1'b1);
        pc += 4;
        // vadd.vv v3, v2, v1
        rom[pc >> 2] = r_type({RVV_VADD, 1'b1}, 5'd2, 5'd1, RVV_OPIVV, 5'd3, OP_V);
        pc += 4;
        // VSE32.V v3, (x1+48)
        rom[pc >> 2] = i_type(48, 5'd1, 3'b000, 5'd2, OP_IMM);
        pc += 4;
        rom[pc >> 2] = rvv_vs(5'd3, 5'd2, RVV_VEW32, 1'b1);
        pc += 4;
        // vmseq.vv v1,v1 -> scalar mask in x9
        rom[pc >> 2] = r_type({RVV_VMSEQ, 1'b1}, 5'd1, 5'd1, RVV_OPIVV, 5'd9, OP_V);
        pc += 4;
        // SW x9, 64(x1)
        rom[pc >> 2] = s_type(64, 5'd1, 5'd9, 3'b010, OP_STORE);
        pc += 4;
        // VATOM.ADD v5, 48(x1), v2 (funct3=100 → vector atomic class)
        // Encode as S-type so rs2 carries the source vector index (v2)
        rom[pc >> 2] = s_type(48, 5'd1, 5'd2, 3'b100, OP_CUSTOM2);
        pc += 4;
        // VLE32.V v6, (x1+48) ; expect summed atomics
        rom[pc >> 2] = i_type(48, 5'd1, 3'b000, 5'd2, OP_IMM);
        pc += 4;
        rom[pc >> 2] = rvv_vl(5'd6, 5'd2, RVV_VEW32, 1'b1);
        pc += 4;
        // VSE32.V v6, (x1+80) ; capture final vector result
        rom[pc >> 2] = i_type(80, 5'd1, 3'b000, 5'd2, OP_IMM);
        pc += 4;
        rom[pc >> 2] = rvv_vs(5'd6, 5'd2, RVV_VEW32, 1'b1);
        pc += 4;
        // Final MEMBAR
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM);
    end

    // ------------------------------------------------------------------
    // Initialize data memory
    // ------------------------------------------------------------------
    initial begin
        for (int i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;
        // Seed vector operands at base+16 and base+32
        mem[mem_index(BASE_ADDR + 32'd16)] = 32'd1;
        mem[mem_index(BASE_ADDR + 32'd20)] = 32'd2;
        mem[mem_index(BASE_ADDR + 32'd24)] = 32'd3;
        mem[mem_index(BASE_ADDR + 32'd28)] = 32'd4;

        mem[mem_index(BASE_ADDR + 32'd32)] = 32'd10;
        mem[mem_index(BASE_ADDR + 32'd36)] = 32'd20;
        mem[mem_index(BASE_ADDR + 32'd40)] = 32'd30;
        mem[mem_index(BASE_ADDR + 32'd44)] = 32'd40;
    end

    // ------------------------------------------------------------------
    // Scoreboard / checks
    // ------------------------------------------------------------------
    task automatic check_equal(input string name, input logic [31:0] got, input logic [31:0] exp);
        if (got !== exp) begin
            $fatal(1, "%s mismatch: got %0d (0x%08x) expected %0d (0x%08x)", name, got, got, exp, exp);
        end else begin
            $display("[OK] %s = %0d (0x%08x)", name, got, got);
        end
    endtask

    initial begin
        wait(rst_n);
        repeat (800) @(posedge clk);

        check_equal("scalar store @0", mem[mem_index(BASE_ADDR + 32'd0)], 32'd12);
        check_equal("scalar store @8", mem[mem_index(BASE_ADDR + 32'd8)], 32'd12);
        check_equal("csr_status", csr_status, 32'd7);
        check_equal("vcmp mask store", mem[mem_index(BASE_ADDR + 32'd64)], 32'hF);

        // Vector after VADD stored at base+48; VATOM then updates it in place
        check_equal("vadd lane0", mem[mem_index(BASE_ADDR + 32'd48)], 32'd21);
        check_equal("vadd lane1", mem[mem_index(BASE_ADDR + 32'd52)], 32'd42);
        check_equal("vadd lane2", mem[mem_index(BASE_ADDR + 32'd56)], 32'd63);
        check_equal("vadd lane3", mem[mem_index(BASE_ADDR + 32'd60)], 32'd84);

        // After atomic add and capture to base+80
        check_equal("vatom lane0", mem[mem_index(BASE_ADDR + 32'd80)], 32'd21);
        check_equal("vatom lane1", mem[mem_index(BASE_ADDR + 32'd84)], 32'd42);
        check_equal("vatom lane2", mem[mem_index(BASE_ADDR + 32'd88)], 32'd63);
        check_equal("vatom lane3", mem[mem_index(BASE_ADDR + 32'd92)], 32'd84);

        $display("\nAll checks passed.");
        $finish;
    end
endmodule
