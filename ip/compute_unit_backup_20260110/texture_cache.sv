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
    wire stage2_stall; // Defined later based on hit
    assign req_ready = !stage2_stall;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            req_valid_q <= 1'b0;
            req_rd_q    <= '0;
            tag_q       <= '0;
            index_q     <= '0;
            offset_q    <= '0;
        end else if (req_ready) begin
            // Advance pipeline if not stalled
            req_valid_q <= req_valid;
            req_rd_q    <= req_rd;
            tag_q       <= tag_in;
            index_q     <= index_in;
            offset_q    <= offset_in;
        end
        // If stalled, hold values.
    end

    // RAM Read (Synchronous)
    // We read at index_in (stage 1 address).
    // If stalled, req_addr should be stable (held by previous stage), so index_in is stable.
    always_ff @(posedge clk) begin
        // If we are accepting new data (or holding stable address), RAM output updates or holds?
        // Standard BRAM updates on clock. If address is stable, output is stable (same value).
        // If we write (refill), we might need careful forwarding or just rely on next read.
        tag_entry_rd <= tag_ram[index_in];
        line_rd      <= data_ram[index_in];
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
    assign stage2_stall = req_valid_q && !hit && !miss_resp_valid;

    // Output generation (hit or immediate after refill)
    assign resp_valid = req_valid_q && (hit || miss_resp_valid);
    assign resp_data  = miss_resp_valid ? miss_resp_data[{offset_q[OFFSET_BITS-1:2], 5'b0} +: 32]
                                        : line_rd[{offset_q[OFFSET_BITS-1:2], 5'b0} +: 32];
    assign resp_rd    = req_rd_q;

    // Miss handling
    assign miss_req_valid = stage2_stall; // Request refill while stalled
    // Reconstruct full address from registered parts
    assign miss_req_addr  = {tag_q, index_q, {OFFSET_BITS{1'b0}}};

    // Refill update
    always_ff @(posedge clk) begin
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
