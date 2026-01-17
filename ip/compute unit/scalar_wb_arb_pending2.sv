module scalar_wb_arb_pending2 (
    input  logic        clk,
    input  logic        rst_n,

    // Candidate scalar writeback sources
    input  logic        lsu_valid,
    input  logic [4:0]  lsu_rd,
    input  logic [31:0] lsu_data,

    input  logic        fp_valid,
    input  logic [4:0]  fp_rd,
    input  logic [31:0] fp_data,
    input  logic        fp_err_overflow,
    input  logic        fp_err_invalid,
    output logic        fp_ready,

    input  logic        valuv_valid,
    input  logic [4:0]  valuv_rd,
    input  logic [31:0] valuv_data,
    input  logic        valuv_err_overflow,
    input  logic        valuv_err_invalid,
    output logic        valuv_ready,

    input  logic        alu_valid,
    input  logic [4:0]  alu_rd,
    input  logic [31:0] alu_data,
    output logic        alu_ready,

    // Selected scalar writeback to regfile
    output logic        s_we,
    output logic [4:0]  s_waddr,
    output logic [31:0] s_wdata,

    // Commit metadata for CSR alignment
    output logic        wb_from_fp,
    output logic        wb_from_valu,
    output logic        wb_err_overflow,
    output logic        wb_err_invalid,

    // Optional debug visibility (matches legacy signal names)
    output logic        dbg_from_pending,
    output logic        dbg_from_lsu,
    output logic        dbg_from_fp,
    output logic        dbg_from_valu,
    output logic        dbg_from_alu
);

    localparam int PEND_DEPTH = 4;
    localparam int PEND_PTR_W = (PEND_DEPTH <= 2) ? 1 : $clog2(PEND_DEPTH);

    localparam logic [1:0] SRC_FP   = 2'd0;
    localparam logic [1:0] SRC_VALU = 2'd1;
    localparam logic [1:0] SRC_ALU  = 2'd2;

    logic [PEND_PTR_W-1:0] pending_head;
    logic [PEND_PTR_W-1:0] pending_tail;
    logic [$clog2(PEND_DEPTH+1)-1:0] pending_count;

    logic [4:0]  pending_rd   [PEND_DEPTH];
    logic [31:0] pending_data [PEND_DEPTH];
    logic [1:0]  pending_src  [PEND_DEPTH];
    logic        pending_ovf  [PEND_DEPTH];
    logic        pending_inv  [PEND_DEPTH];

    // Deterministic scalar writeback priority: LSU > Pending > FP scalar > VALU scalar > ALU scalar
    wire has_pending  = (pending_count != 0);
    wire take_pending = has_pending && !lsu_valid;
    wire take_lsu     = lsu_valid;
    wire take_fp      = fp_valid    && !take_pending && !take_lsu;
    wire take_valu    = valuv_valid && !take_pending && !take_lsu && !take_fp;
    wire take_alu     = alu_valid   && !take_pending && !take_lsu && !take_fp && !take_valu;

    assign s_we = take_pending | take_lsu | take_fp | take_valu | take_alu;

    assign dbg_from_pending = take_pending;
    assign dbg_from_lsu     = take_lsu;
    assign dbg_from_fp      = take_fp;
    assign dbg_from_valu    = take_valu;
    assign dbg_from_alu     = take_alu;

    // Pending head decode (combinational)
    logic [4:0]  pend0_rd;
    logic [31:0] pend0_data;
    logic [1:0]  pend0_src;
    logic        pend0_ovf;
    logic        pend0_inv;

    always_comb begin
        pend0_rd   = pending_rd[pending_head];
        pend0_data = pending_data[pending_head];
        pend0_src  = pending_src[pending_head];
        pend0_ovf  = pending_ovf[pending_head];
        pend0_inv  = pending_inv[pending_head];
    end

    // Selected writeback mux + commit metadata
    always_comb begin
        s_waddr = 5'd0;
        s_wdata = 32'h0;
        wb_from_fp = 1'b0;
        wb_from_valu = 1'b0;
        wb_err_overflow = 1'b0;
        wb_err_invalid  = 1'b0;

        if (take_pending) begin
            s_waddr = pend0_rd;
            s_wdata = pend0_data;
            wb_from_fp = (pend0_src == SRC_FP);
            wb_from_valu = (pend0_src == SRC_VALU);
            wb_err_overflow = pend0_ovf;
            wb_err_invalid  = pend0_inv;
        end else if (take_lsu) begin
            s_waddr = lsu_rd;
            s_wdata = lsu_data;
        end else if (take_fp) begin
            s_waddr = fp_rd;
            s_wdata = fp_data;
            wb_from_fp = 1'b1;
            wb_err_overflow = fp_err_overflow;
            wb_err_invalid  = fp_err_invalid;
        end else if (take_valu) begin
            s_waddr = valuv_rd;
            s_wdata = valuv_data;
            wb_from_valu = 1'b1;
            wb_err_overflow = valuv_err_overflow;
            wb_err_invalid  = valuv_err_invalid;
        end else if (take_alu) begin
            s_waddr = alu_rd;
            s_wdata = alu_data;
        end
    end

    // Determine which sources need buffering (lose arbitration)
    wire fp_loser   = fp_valid    && !take_fp;
    wire valuv_loser= valuv_valid && !take_valu;
    wire alu_loser  = alu_valid   && !take_alu;

    // Capacity after optional pop
    wire [$clog2(PEND_DEPTH+1)-1:0] pend_cnt_after_pop = pending_count - (take_pending ? 1 : 0);
    wire [$clog2(PEND_DEPTH+1)-1:0] free_slots = PEND_DEPTH - pend_cnt_after_pop;

    // Ready/backpressure: accept losers into pending in priority order FP > VALU > ALU
    wire [2:0] slots_needed_before_valu = fp_loser ? 3'd1 : 3'd0;
    wire [2:0] slots_needed_before_alu  = (fp_loser ? 3'd1 : 3'd0) + (valuv_loser ? 3'd1 : 3'd0);

    always_comb begin
        fp_ready    = !fp_valid    || take_fp    || (!take_fp    && (free_slots >= 1));
        valuv_ready = !valuv_valid || take_valu  || (!take_valu  && (free_slots > slots_needed_before_valu));
        alu_ready   = !alu_valid   || take_alu   || (!take_alu   && (free_slots > slots_needed_before_alu));
    end

    // Pending FIFO update (pop + multi-push)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pending_head  <= '0;
            pending_tail  <= '0;
            pending_count <= '0;
        end else begin
            logic [PEND_PTR_W-1:0] head;
            logic [PEND_PTR_W-1:0] tail;
            logic [$clog2(PEND_DEPTH+1)-1:0] cnt;

            head = pending_head;
            tail = pending_tail;
            cnt  = pending_count;

            // Pop head if it was committed this cycle
            if (take_pending) begin
                head = head + 1'b1;
                cnt  = cnt - 1'b1;
            end

            // Push losers (only if their ready is asserted)
            if (fp_loser && fp_ready) begin
                pending_rd[tail]   <= fp_rd;
                pending_data[tail] <= fp_data;
                pending_src[tail]  <= SRC_FP;
                pending_ovf[tail]  <= fp_err_overflow;
                pending_inv[tail]  <= fp_err_invalid;
                tail = tail + 1'b1;
                cnt  = cnt + 1'b1;
            end
            if (valuv_loser && valuv_ready) begin
                pending_rd[tail]   <= valuv_rd;
                pending_data[tail] <= valuv_data;
                pending_src[tail]  <= SRC_VALU;
                pending_ovf[tail]  <= valuv_err_overflow;
                pending_inv[tail]  <= valuv_err_invalid;
                tail = tail + 1'b1;
                cnt  = cnt + 1'b1;
            end
            if (alu_loser && alu_ready) begin
                pending_rd[tail]   <= alu_rd;
                pending_data[tail] <= alu_data;
                pending_src[tail]  <= SRC_ALU;
                pending_ovf[tail]  <= 1'b0;
                pending_inv[tail]  <= 1'b0;
                tail = tail + 1'b1;
                cnt  = cnt + 1'b1;
            end

            pending_head  <= head;
            pending_tail  <= tail;
            pending_count <= cnt;
        end
    end

endmodule
