`timescale 1ns/1ps

// Branch/control-flow stress TB for compute_unit_top.
// Focus: redirect/flush correctness, slot1 handling, and branch-after-load hazards.
module branch_tb(
    input logic clk,
    input logic rst_n
);
    import isa_pkg::*;

    // -------------------------
    // DUT interfaces
    // -------------------------
    // Instruction miss interface (I-cache -> L2/memory)
    logic        inst_miss_req_valid;
    logic [31:0] inst_miss_req_addr;
    logic        inst_miss_req_ready;
    logic        inst_miss_resp_valid;
    logic [63:0] inst_miss_resp_data;

    // D-cache memory interface (L1 -> L2/memory)
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

    logic        err_fp_overflow;
    logic        err_fp_invalid;
    logic        err_vec_overflow;
    logic        err_vec_invalid;

    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    compute_unit_top #(
        .CORE_ID(32'hB),
        .TILE_OFFSET(32'h000B_0000)
    ) dut (
        .clk(clk),
        .rst_n(rst_n),
        .inst_miss_req_valid(inst_miss_req_valid),
        .inst_miss_req_addr(inst_miss_req_addr),
        .inst_miss_req_ready(inst_miss_req_ready),
        .inst_miss_resp_valid(inst_miss_resp_valid),
        .inst_miss_resp_data(inst_miss_resp_data),
        .err_fp_overflow(err_fp_overflow),
        .err_fp_invalid(err_fp_invalid),
        .err_vec_overflow(err_vec_overflow),
        .err_vec_invalid(err_vec_invalid),
        .csr_status(csr_status),
        .csr_fstatus(csr_fstatus),
        .csr_vstatus(csr_vstatus),
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

        .fb_aw_valid(),
        .fb_aw_addr(),
        .fb_aw_len(),
        .fb_aw_size(),
        .fb_aw_burst(),
        .fb_aw_ready(1'b1),
        .fb_w_data(),
        .fb_w_strb(),
        .fb_w_last(),
        .fb_w_valid(),
        .fb_w_ready(1'b1),
        .fb_b_valid(1'b0),
        .fb_b_ready(),

        .mailbox_tx_valid(),
        .mailbox_tx_ready(1'b1),
        .mailbox_tx_data(),
        .mailbox_tx_dest_id(),
        .mailbox_rx_valid(1'b0),
        .mailbox_rx_ready(),
        .mailbox_rx_data('0),
        .mailbox_rx_dest_id('0)
    );

    // Always-ready memory interface
    initial dcache_mem_req_ready = 1'b1;
    initial inst_miss_req_ready  = 1'b1;

    // -------------------------
    // ROM (instruction memory)
    // -------------------------
    localparam int ROM_WORDS = 1024;
    localparam int ROM_AW    = $clog2(ROM_WORDS);
    logic [31:0] rom [0:ROM_WORDS-1];

    // I-cache miss response model (8-byte line)
    logic        inst_pending;
    logic [31:0] inst_req_addr_q;

    // -------------------------
    // Global memory model (32-bit words)
    // -------------------------
    localparam int MEM_WORDS = 16384; // 64KB
    localparam int MEM_BYTES = (MEM_WORDS * 4);
    localparam logic [31:0] BASE_ADDR = 32'hFFFF_F800;
    localparam logic [31:0] DONE_OFF  = 32'h0000_01F0;

    logic [7:0] mem_bytes [0:MEM_BYTES-1];

    // D-cache memory response state
    logic        mem_read_pending;
    logic [31:0] mem_read_addr_q;
    logic [2:0]  mem_read_beat;
    logic [7:0]  mem_read_id_q;
    logic        mem_write_resp_pending;

    function automatic int mem_byte_index(input logic [31:0] addr);
        mem_byte_index = addr - BASE_ADDR;
    endfunction

    function automatic logic [31:0] mem_read_word(input logic [31:0] addr);
        int bi;
        logic [31:0] w;
        begin
            bi = mem_byte_index(addr);
            w = {mem_bytes[bi+3], mem_bytes[bi+2], mem_bytes[bi+1], mem_bytes[bi+0]};
            mem_read_word = w;
        end
    endfunction

    // DONE tracking
    logic        done_seen;
    logic [31:0] done_value;
    int unsigned cycle_count;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            dcache_mem_resp_valid <= 1'b0;
            dcache_mem_resp_data  <= 64'h0;
            dcache_mem_resp_id    <= 8'h0;
            mem_read_pending       <= 1'b0;
            mem_read_addr_q        <= 32'h0;
            mem_read_beat          <= 3'd0;
            mem_read_id_q          <= 8'h0;
            mem_write_resp_pending <= 1'b0;
            inst_miss_resp_valid  <= 1'b0;
            inst_miss_resp_data   <= 64'h0;
            inst_pending          <= 1'b0;
            inst_req_addr_q       <= 32'h0;
            done_seen   <= 1'b0;
            done_value  <= 32'h0;
            cycle_count <= 0;
        end else begin
            cycle_count <= cycle_count + 1;

            // Instruction miss response (single beat)
            inst_miss_resp_valid <= 1'b0;
            if (inst_miss_req_valid && inst_miss_req_ready && !inst_pending) begin
                inst_pending    <= 1'b1;
                inst_req_addr_q <= {inst_miss_req_addr[31:3], 3'b000};
            end
            if (inst_pending) begin
                int widx;
                widx = int'(inst_req_addr_q >> 2);
                if ((widx < 0) || (widx + 1 >= ROM_WORDS)) begin
                    $display("%0t BRANCH_TB: OOB IROM access addr=%08h", $time, inst_req_addr_q);
                    $fatal(1);
                end
                inst_miss_resp_valid <= 1'b1;
                inst_miss_resp_data  <= {rom[widx+1], rom[widx]};
                inst_pending <= 1'b0;
            end

            // D-cache memory response model
            dcache_mem_resp_valid <= 1'b0;
            if (mem_write_resp_pending) begin
                dcache_mem_resp_valid <= 1'b1;
                dcache_mem_resp_data  <= 64'h0;
                dcache_mem_resp_id    <= mem_read_id_q;
                mem_write_resp_pending <= 1'b0;
            end else if (mem_read_pending) begin
                int base_r;
                base_r = mem_byte_index(mem_read_addr_q);
                dcache_mem_resp_valid <= 1'b1;
                dcache_mem_resp_data  <= {
                    mem_bytes[base_r + (mem_read_beat*8) + 7],
                    mem_bytes[base_r + (mem_read_beat*8) + 6],
                    mem_bytes[base_r + (mem_read_beat*8) + 5],
                    mem_bytes[base_r + (mem_read_beat*8) + 4],
                    mem_bytes[base_r + (mem_read_beat*8) + 3],
                    mem_bytes[base_r + (mem_read_beat*8) + 2],
                    mem_bytes[base_r + (mem_read_beat*8) + 1],
                    mem_bytes[base_r + (mem_read_beat*8) + 0]
                };
                dcache_mem_resp_id <= mem_read_id_q;
                if (mem_read_beat == 3'd7) begin
                    mem_read_pending <= 1'b0;
                    mem_read_beat    <= 3'd0;
                end else begin
                    mem_read_beat <= mem_read_beat + 1'b1;
                end
            end

            if (dcache_mem_req_valid && dcache_mem_req_ready && !mem_read_pending && !mem_write_resp_pending) begin
                int base;
                base = mem_byte_index(dcache_mem_req_addr & 32'hFFFF_FFC0);
                if ((base < 0) || ((base + 64) > MEM_BYTES)) begin
                    $display("%0t BRANCH_TB: OOB D$ MEM req addr=%08h", $time, dcache_mem_req_addr);
                    $fatal(1);
                end

                if (dcache_mem_req_rw) begin
                    logic [31:0] done_word_next;
                    done_word_next = mem_read_word(BASE_ADDR + DONE_OFF);
                    for (int b = 0; b < 64; b++) begin
                        if (dcache_mem_req_wstrb[b]) begin
                            mem_bytes[base + b] <= dcache_mem_req_wdata[b*8 +: 8];
                            if ((BASE_ADDR + DONE_OFF) >= (dcache_mem_req_addr & 32'hFFFF_FFC0)
                                && (BASE_ADDR + DONE_OFF) < ((dcache_mem_req_addr & 32'hFFFF_FFC0) + 32'h40)) begin
                                int done_bi;
                                done_bi = mem_byte_index(BASE_ADDR + DONE_OFF);
                                if ((base + b) >= done_bi && (base + b) <= (done_bi + 3)) begin
                                    done_word_next[((base + b - done_bi) * 8) +: 8] = dcache_mem_req_wdata[b*8 +: 8];
                                end
                            end
                        end
                    end

                    if ((dcache_mem_req_addr <= (BASE_ADDR + DONE_OFF))
                        && ((dcache_mem_req_addr + 32'h40) > (BASE_ADDR + DONE_OFF))) begin
                        done_value <= done_word_next;
                        done_seen  <= (done_word_next != 32'h0);
                        if (done_word_next != 32'h0) begin
                            $display("%0t BRANCH_TB: DONE write value=%08h (cycles=%0d)", $time, done_word_next, cycle_count);
                        end
                    end

                    mem_write_resp_pending <= 1'b1;
                    mem_read_id_q <= dcache_mem_req_id;
                end else begin
                    mem_read_pending <= 1'b1;
                    mem_read_addr_q  <= (dcache_mem_req_addr & 32'hFFFF_FFC0);
                    mem_read_beat    <= 3'd0;
                    mem_read_id_q    <= dcache_mem_req_id;
                end
            end

            if (!done_seen && (cycle_count >= 500_000)) begin
                $display("%0t BRANCH_TB: TIMEOUT cycles=%0d", $time, cycle_count);
                $display("%0t BRANCH_TB: if_pc=%08h rr_v=%0d ex_v=%0d mem_v=%0d wb_v=%0d stall_any=%0d", $time, dut.if_pc, dut.rr_valid, dut.ex_valid, dut.mem_valid, dut.wb_valid, dut.stall_any);
                $fatal(1);
            end
        end
    end

    // -------------------------
    // Mini assembler helpers
    // -------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] j_type(input integer imm, input [4:0] rd, input [6:0] opcode);
        j_type = {imm[20], imm[10:1], imm[11], imm[19:12], rd, opcode};
    endfunction

    function automatic [31:0] b_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        b_type = {imm[12], imm[10:5], rs2, rs1, funct3, imm[4:1], imm[11], opcode};
    endfunction

    function automatic [31:0] nop();
        nop = i_type(0, 5'd0, 3'b000, 5'd0, OP_INT_IMM);
    endfunction

    // -------------------------
    // Program image (directed branch/redirect tests)
    // -------------------------
    initial begin
        int pc;
        int jal_slot;
        int jal_delay_slot;
        int j_slot;
        int sub_pc;
        int after_pc;

        for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
        for (int i = 0; i < MEM_BYTES; i++) mem_bytes[i] = 8'h0;

        pc = 0;

        // x1 = BASE_ADDR (ADDI x1, x0, -2048)
        rom[pc >> 2] = i_type(-2048, 5'd0, 3'b000, 5'd1, OP_INT_IMM); pc += 4;

        // -------------------------
        // JAL/JALR redirect + delay-slot flush
        // -------------------------
        // Reserve: JAL x9, sub
        jal_slot = (pc >> 2);
        rom[jal_slot] = nop(); pc += 4;

        // Post-JAL word can be speculatively fetched; do not rely on it committing.
        jal_delay_slot = (pc >> 2);
        rom[jal_delay_slot] = nop(); pc += 4;

        // Return address block: jump over sub body placement
        j_slot = (pc >> 2);
        rom[j_slot] = nop(); pc += 4;

        // sub:
        sub_pc = pc;
        // x4 = 0x11
        rom[pc >> 2] = i_type(32'h11, 5'd0, 3'b000, 5'd4, OP_INT_IMM); pc += 4;
        // Store link (x9) to BASE+0x100
        rom[pc >> 2] = s_type(32'h100, 5'd1, 5'd9, 3'b010, OP_STORE); pc += 4;
        // Store marker that sub executed to BASE+0x104 (x4 should still be 0x11)
        rom[pc >> 2] = s_type(32'h104, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;
        // Align JALR into slot0 so redirects are handled by the primary pipe.
        if (pc[2]) begin
            rom[pc >> 2] = nop(); pc += 4;
        end
        // Return via JALR x0, 0(x9)
        rom[pc >> 2] = i_type(0, 5'd9, 3'b010, 5'd0, OP_BRANCH); pc += 4;
        // JALR delay-slot sentinel: must NOT execute if flush works
        rom[pc >> 2] = s_type(32'h108, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;

        after_pc = pc;
        // Marker after return
        rom[pc >> 2] = i_type(32'h22, 5'd0, 3'b000, 5'd5, OP_INT_IMM); pc += 4; // x5=0x22
        rom[pc >> 2] = s_type(32'h10C, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4;

        // Patch call + post-return jump
        rom[jal_slot] = j_type(sub_pc - (jal_slot << 2), 5'd9, OP_JAL);
        rom[j_slot]   = j_type(after_pc - (j_slot << 2), 5'd0, OP_JAL);

        // -------------------------
        // Conditional branches taken/not taken + sentinels
        // -------------------------
        // x2=7, x3=7, x6=8
        rom[pc >> 2] = i_type(7, 5'd0, 3'b000, 5'd2, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(7, 5'd0, 3'b000, 5'd3, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = i_type(8, 5'd0, 3'b000, 5'd6, OP_INT_IMM); pc += 4;

        // BEQ x2,x3,+8 (taken)
        rom[pc >> 2] = b_type(8, 5'd2, 5'd3, 3'b000, OP_BRANCH); pc += 4;
        // fall-through sentinel (must be flushed)
        rom[pc >> 2] = s_type(32'h200, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;
        // taken target marker
        rom[pc >> 2] = s_type(32'h204, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4;

        // BNE x2,x3,+12 (not taken)
        rom[pc >> 2] = b_type(12, 5'd2, 5'd3, 3'b001, OP_BRANCH); pc += 4;
        // fall-through marker (must execute)
        rom[pc >> 2] = s_type(32'h208, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4;
        rom[pc >> 2] = nop(); pc += 4;
        // jump over (would-be) taken block
        rom[pc >> 2] = j_type(8, 5'd0, OP_JAL); pc += 4;
        // taken block marker (must NOT execute)
        rom[pc >> 2] = s_type(32'h20C, 5'd1, 5'd5, 3'b010, OP_STORE); pc += 4;

        // -------------------------
        // Slot1 branch test: ensure inst1 branch is not dropped
        // -------------------------
        if (pc[2]) begin
            rom[pc >> 2] = nop(); pc += 4;
        end
        // inst0: x20=0xAA ; inst1: BEQ x0,x0,+8 (taken)
        rom[pc >> 2] = i_type(32'hAA, 5'd0, 3'b000, 5'd20, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = b_type(8, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
        // fall-through sentinel (must NOT execute)
        rom[pc >> 2] = s_type(32'h220, 5'd1, 5'd20, 3'b010, OP_STORE); pc += 4;
        // target marker (must execute)
        rom[pc >> 2] = i_type(32'hBB, 5'd0, 3'b000, 5'd21, OP_INT_IMM); pc += 4;
        rom[pc >> 2] = s_type(32'h224, 5'd1, 5'd21, 3'b010, OP_STORE); pc += 4;

        // -------------------------
        // Branch-after-load hazard
        // -------------------------
        // Store x6 (=8) to BASE+0x240, MEMBAR, load to x7, then BEQ x7,x6 taken.
        rom[pc >> 2] = s_type(32'h240, 5'd1, 5'd6, 3'b010, OP_STORE); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // MEMBAR
        rom[pc >> 2] = i_type(32'h240, 5'd1, 3'b010, 5'd7, OP_LOAD); pc += 4; // LW x7,0x240(x1)
        rom[pc >> 2] = b_type(8, 5'd7, 5'd6, 3'b000, OP_BRANCH); pc += 4; // BEQ taken
        rom[pc >> 2] = s_type(32'h244, 5'd1, 5'd4, 3'b010, OP_STORE); pc += 4; // sentinel (flushed)
        rom[pc >> 2] = s_type(32'h248, 5'd1, 5'd6, 3'b010, OP_STORE); pc += 4; // target marker

        // -------------------------
        // DONE
        // -------------------------
        rom[pc >> 2] = i_type(1, 5'd0, 3'b000, 5'd10, OP_INT_IMM); pc += 4; // x10=1
        rom[pc >> 2] = s_type(DONE_OFF, 5'd1, 5'd10, 3'b010, OP_STORE); pc += 4;
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // MEMBAR
        rom[pc >> 2] = j_type(0, 5'd0, OP_JAL);

        $display("BRANCH_TB: program loaded (%0d words)", ROM_WORDS);
    end

    task automatic check_eq(input string name, input logic [31:0] got, input logic [31:0] exp);
        if (got !== exp) begin
            $fatal(1, "BRANCH_TB: %s mismatch got=%08h exp=%08h", name, got, exp);
        end else begin
            $display("BRANCH_TB: [OK] %s = %08h", name, got);
        end
    endtask

    // Validate once DONE is observed
    always_ff @(posedge clk) begin
        if (rst_n && done_seen) begin
            check_eq("done_value", done_value, 32'h0000_0001);

            // JAL/JALR markers
            check_eq("sub stored link nonzero", (mem_read_word(BASE_ADDR + 32'h100) != 32'h0) ? 32'h1 : 32'h0, 32'h1);
            check_eq("sub executed marker", mem_read_word(BASE_ADDR + 32'h104), 32'h0000_0011);
            check_eq("jalr delay-slot flushed", mem_read_word(BASE_ADDR + 32'h108), 32'h0000_0000);
            check_eq("after return marker", mem_read_word(BASE_ADDR + 32'h10C), 32'h0000_0022);

            // BEQ taken
            check_eq("beq fall-through flushed", mem_read_word(BASE_ADDR + 32'h200), 32'h0);
            check_eq("beq taken marker", mem_read_word(BASE_ADDR + 32'h204), 32'h0000_0022);

            // BNE not taken
            check_eq("bne fall-through marker", mem_read_word(BASE_ADDR + 32'h208), 32'h0000_0011);
            check_eq("bne taken block not executed", mem_read_word(BASE_ADDR + 32'h20C), 32'h0);

            // Slot1 branch
            check_eq("slot1 fall-through flushed", mem_read_word(BASE_ADDR + 32'h220), 32'h0);
            check_eq("slot1 target marker", mem_read_word(BASE_ADDR + 32'h224), 32'h0000_00BB);

            // Branch-after-load
            check_eq("bal sentinel flushed", mem_read_word(BASE_ADDR + 32'h244), 32'h0);
            check_eq("bal target marker", mem_read_word(BASE_ADDR + 32'h248), 32'h0000_0008);

            $display("BRANCH_TB: PASS (cycles=%0d)", cycle_count);
            $finish;
        end
    end

endmodule
