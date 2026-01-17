module texture_cache #(
    parameter LINE_BYTES = 64,
    parameter LINES      = 64
) (
    input  logic        clk,
    input  logic        rst_n,
    // TEX-side request
    input  logic        req_valid,
    input  logic [31:0] req_addr,
    input  logic [4:0]  req_rd,
    output logic        req_ready,
    output logic        resp_valid,
    output logic [31:0] resp_data,
    output logic [4:0]  resp_rd,
    // Miss refill interface (to external memory/OSPI controller)
    output logic        miss_req_valid,
    output logic [31:0] miss_req_addr,
    input  logic        miss_req_ready,
    input  logic        miss_resp_valid,
    input  logic [LINE_BYTES*8-1:0] miss_resp_data
);
    localparam LINE_BITS   = LINE_BYTES * 8;
    localparam INDEX_BITS  = $clog2(LINES);
    localparam OFFSET_BITS = $clog2(LINE_BYTES);
    localparam TAG_BITS    = 32 - INDEX_BITS - OFFSET_BITS;

    typedef struct packed {
        logic               valid;
        logic [TAG_BITS-1:0] tag;
    } tag_entry_t;

    tag_entry_t tag_ram   [0:LINES-1];
    logic [LINE_BITS-1:0] data_ram  [0:LINES-1];

    // Pipeline registers for stage 2 (hit check)
    logic               req_valid_q;
    logic [TAG_BITS-1:0] tag_q;
    logic [INDEX_BITS-1:0] index_q;
    logic [OFFSET_BITS-1:0] offset_q;
    logic [4:0]         req_rd_q;

    // RAM read outputs
    tag_entry_t           tag_entry_rd;
    logic [LINE_BITS-1:0] line_rd;

    // Address decode
    logic [TAG_BITS-1:0]   tag_in;
    logic [INDEX_BITS-1:0] index_in;
    logic [OFFSET_BITS-1:0] offset_in;

    assign tag_in    = req_addr[31 -: TAG_BITS];
    assign index_in  = req_addr[OFFSET_BITS +: INDEX_BITS];
    assign offset_in = req_addr[OFFSET_BITS-1:0];

    // Stall logic: if stage 2 has a valid request that is a MISS, we stall stage 1
    // Use 4-state-safe comparisons so X on miss_resp_valid can't poison the handshake.
    wire stage2_stall; // Defined later based on hit
    wire miss_resp_valid_1 = (miss_resp_valid === 1'b1);
    wire req_latched_has_x = ((^tag_q === 1'bx)
                           || (^index_q === 1'bx)
                           || (^offset_q === 1'bx)
                           || (^req_rd_q === 1'bx));
    assign req_ready = rst_n && !stage2_stall;

    wire req_fire = (req_valid === 1'b1) && (req_ready === 1'b1);

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            req_valid_q <= 1'b0;
            req_rd_q    <= '0;
            tag_q       <= '0;
            index_q     <= '0;
            offset_q    <= '0;
        end else begin
            // Latch a new request only on handshake. Hold latched fields stable while pending.
            if (req_fire) begin
                req_valid_q <= 1'b1;
                req_rd_q    <= req_rd;
                tag_q       <= tag_in;
                index_q     <= index_in;
                offset_q    <= offset_in;
            end else if (req_valid_q && req_latched_has_x) begin
                // Defensive: if upstream ever violates the protocol (valid with X addr/rd),
                // drop the poisoned request so ready/valid don't go X forever.
                req_valid_q <= 1'b0;
            end else if (resp_valid) begin
                // One-cycle pulse per request (no resp_ready): clear once a response is presented.
                req_valid_q <= 1'b0;
            end
        end
    end

    // Tag/data lookup uses the latched index (index_q). This keeps the hit check aligned
    // even if the requester deasserts req_valid or drives a default req_addr while waiting.
    always_comb begin
        tag_entry_rd = tag_ram[index_q];
        line_rd      = data_ram[index_q];
    end

    // Stage 2: Hit Check
    // Compare Registered Tag (tag_q) with RAM Output (tag_entry_rd)
    // Note: This relies on index_in (stage 1) being the same as index_q (stage 2) regarding the specific RAM read.
    // In a pipelined cache:
    // Cycle T: index_in driven. RAM read clocked.
    // Cycle T+1: RAM out available. tag_q available (registered inputs).
    // So tag_entry_rd corresponds to the address that generated tag_q. Correct.
    
    wire hit = tag_entry_rd.valid && (tag_entry_rd.tag == tag_q);
    
    // Stall generation: If valid request in S2, and MISS, then STALL.
    // Allow forward progress as soon as refill data arrives to avoid livelock if hit check lags one cycle.
    assign stage2_stall = req_valid_q && !req_latched_has_x && !hit && !miss_resp_valid_1;

    // Output generation (hit or immediate after refill)
    assign resp_valid = rst_n && req_valid_q && !req_latched_has_x && (hit || miss_resp_valid_1);
    assign resp_data  = miss_resp_valid_1 ? miss_resp_data[{offset_q[OFFSET_BITS-1:2], 5'b0} +: 32]
                                        : line_rd[{offset_q[OFFSET_BITS-1:2], 5'b0} +: 32];
    assign resp_rd    = req_rd_q;

    // Miss handling
    assign miss_req_valid = rst_n && stage2_stall; // Request refill while stalled
    // Reconstruct full address from registered parts
    assign miss_req_addr  = req_latched_has_x ? 32'h0 : {tag_q, index_q, {OFFSET_BITS{1'b0}}};

    // Refill update
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            integer i;
            for (i = 0; i < LINES; i = i + 1) begin
                tag_ram[i].valid <= 1'b0;
            end
        end else if (miss_resp_valid) begin
            // Update the cache line that caused the miss
            tag_ram[index_q].valid <= 1'b1;
            tag_ram[index_q].tag   <= tag_q;
            data_ram[index_q]      <= miss_resp_data;
        end
    end
endmodule
