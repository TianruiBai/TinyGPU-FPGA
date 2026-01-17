module branch_predictor_bht #(
    parameter int ENTRIES = 64
) (
    input  logic                 clk,
    input  logic                 rst_n,

    // Query (combinational)
    input  logic                 query_valid,
    input  isa_pkg::decode_ctrl_t query_ctrl,
    input  logic [31:0]          query_pc,
    output logic                 pred_taken,
    output logic [31:0]          pred_target,

    // Update from resolved branch in EX
    input  logic                 update_valid,
    input  isa_pkg::decode_ctrl_t update_ctrl,
    input  logic [31:0]          update_pc,
    input  logic                 update_taken
);
    import isa_pkg::*;

    localparam int IDX_BITS = (ENTRIES <= 1) ? 1 : $clog2(ENTRIES);

    typedef struct packed {
        logic                 valid;
        logic [31-(2+IDX_BITS):0] tag;
        logic [1:0]           ctr; // 00/01 NT, 10/11 T
    } bht_entry_t;

    bht_entry_t bht [ENTRIES];

    function automatic logic [IDX_BITS-1:0] idx_of(input logic [31:0] pc);
        idx_of = pc[2 +: IDX_BITS];
    endfunction

    function automatic logic [31-(2+IDX_BITS):0] tag_of(input logic [31:0] pc);
        tag_of = pc[31:(2+IDX_BITS)];
    endfunction

    wire query_is_jal  = query_valid && query_ctrl.is_branch && (query_ctrl.funct3 == 3'b011);
    wire query_is_jalr = query_valid && query_ctrl.is_branch && (query_ctrl.funct3 == 3'b010);
    wire query_is_cond = query_valid && query_ctrl.is_branch && !query_is_jal && !query_is_jalr;

    wire [IDX_BITS-1:0] q_idx = idx_of(query_pc);
    wire [31-(2+IDX_BITS):0] q_tag = tag_of(query_pc);

    bht_entry_t q_ent;
    always_comb q_ent = bht[q_idx];

    always_comb begin
        pred_taken  = 1'b0;
        pred_target = 32'h0;

        // Target for direct control-flow is PC-relative.
        if (query_valid && query_ctrl.is_branch && !query_is_jalr) begin
            pred_target = query_pc + query_ctrl.imm;
        end

        if (query_is_jal) begin
            pred_taken = 1'b1;
        end else if (query_is_cond) begin
            if (q_ent.valid && (q_ent.tag == q_tag)) begin
                pred_taken = q_ent.ctr[1];
            end else begin
                pred_taken = 1'b0;
            end
        end
    end

    wire update_is_jal  = update_valid && update_ctrl.is_branch && (update_ctrl.funct3 == 3'b011);
    wire update_is_jalr = update_valid && update_ctrl.is_branch && (update_ctrl.funct3 == 3'b010);
    wire update_is_cond = update_valid && update_ctrl.is_branch && !update_is_jal && !update_is_jalr;

    wire [IDX_BITS-1:0] u_idx = idx_of(update_pc);
    wire [31-(2+IDX_BITS):0] u_tag = tag_of(update_pc);

    function automatic logic [1:0] sat_inc(input logic [1:0] v);
        sat_inc = (v == 2'b11) ? 2'b11 : (v + 2'b01);
    endfunction

    function automatic logic [1:0] sat_dec(input logic [1:0] v);
        sat_dec = (v == 2'b00) ? 2'b00 : (v - 2'b01);
    endfunction

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            for (int i = 0; i < ENTRIES; i++) begin
                bht[i].valid <= 1'b0;
                bht[i].tag   <= '0;
                bht[i].ctr   <= 2'b01; // weakly not-taken
            end
        end else begin
            if (update_is_cond) begin
                if (!bht[u_idx].valid || (bht[u_idx].tag != u_tag)) begin
                    bht[u_idx].valid <= 1'b1;
                    bht[u_idx].tag   <= u_tag;
                    bht[u_idx].ctr   <= update_taken ? 2'b10 : 2'b01;
                end else begin
                    bht[u_idx].ctr <= update_taken ? sat_inc(bht[u_idx].ctr) : sat_dec(bht[u_idx].ctr);
                end
            end
        end
    end
endmodule
