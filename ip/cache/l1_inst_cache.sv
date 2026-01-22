// L1 Instruction Cache (direct-mapped, single-miss outstanding)
// Shared by compute unit and control processor via small wrappers.

module l1_inst_cache #(
    parameter bit ENABLED = 1'b1,
    parameter int LINE_BYTES = 32,
    parameter int LINES = 64,
    parameter int FETCH_DATA_BITS = 32
) (
    input  logic        clk,
    input  logic        rst_n,

    // Request from fetch
    input  logic        req_valid,
    input  logic [31:0] req_addr,
    output logic        req_ready,

    // Response to fetch
    output logic        resp_valid,
    output logic [FETCH_DATA_BITS-1:0] resp_data,
    output logic        resp_err,

    // Miss refill interface (to external memory/OSPI controller)
    output logic        miss_req_valid,
    output logic [31:0] miss_req_addr,
    input  logic        miss_req_ready,
    input  logic        miss_resp_valid,
    input  logic [LINE_BYTES*8-1:0] miss_resp_data
);
    localparam int LINE_BITS   = LINE_BYTES * 8;
    localparam int INDEX_BITS  = $clog2(LINES);
    localparam int OFFSET_BITS = $clog2(LINE_BYTES);
    localparam int TAG_BITS    = 32 - INDEX_BITS - OFFSET_BITS;
    localparam int FETCH_BYTES = FETCH_DATA_BITS / 8;
    localparam int FETCH_SHIFT = (FETCH_BYTES <= 1) ? 0 : $clog2(FETCH_BYTES);

    typedef struct packed {
        logic               valid;
        logic [TAG_BITS-1:0] tag;
    } tag_entry_t;

    // Storage
    tag_entry_t tag_ram   [0:LINES-1];
    logic [LINE_BITS-1:0] data_ram  [0:LINES-1];

    // Request pipeline
    logic                 req_valid_q;
    logic [31:0]          req_addr_q;
    logic [TAG_BITS-1:0]  tag_in_q;
    logic [INDEX_BITS-1:0] index_in_q;
    logic [OFFSET_BITS-1:0] offset_in_q;

    // RAM read outputs (sync)
    tag_entry_t           tag_entry_rd;
    logic [LINE_BITS-1:0] line_rd;

    // Miss tracking
    logic                 miss_pending;
    logic                 miss_issued;
    logic [TAG_BITS-1:0]  miss_tag;
    logic [INDEX_BITS-1:0] miss_index;
    logic [OFFSET_BITS-1:0] miss_offset;
    logic [31:0]          miss_addr_aligned;

    // Address decode for current request
    wire [TAG_BITS-1:0]   tag_in    = req_addr[31 -: TAG_BITS];
    wire [INDEX_BITS-1:0] index_in  = req_addr[OFFSET_BITS +: INDEX_BITS];
    wire [OFFSET_BITS-1:0] offset_in = req_addr[OFFSET_BITS-1:0];

    // Request accept
    wire req_fire = req_valid && req_ready;

    // Synchronous read for cache arrays
    wire [INDEX_BITS-1:0] read_index = req_fire ? index_in : index_in_q;
    always_ff @(posedge clk) begin
        tag_entry_rd <= tag_ram[read_index];
        line_rd      <= data_ram[read_index];
    end

    wire hit = req_valid_q && tag_entry_rd.valid && (tag_entry_rd.tag == tag_in_q);

    // Resp data mux
    always_comb begin
        integer base_bit;
        resp_data = '0;
        base_bit = (hit ? (offset_in_q >> FETCH_SHIFT) : (miss_offset >> FETCH_SHIFT)) * FETCH_DATA_BITS;
        if (hit) begin
            resp_data = line_rd[base_bit +: FETCH_DATA_BITS];
        end else if (miss_pending && miss_resp_valid) begin
            resp_data = miss_resp_data[base_bit +: FETCH_DATA_BITS];
        end
    end

    assign resp_valid = hit || (miss_pending && miss_resp_valid);
    assign resp_err   = 1'b0;

    // Ready when idle (single outstanding request/miss)
    assign req_ready = ENABLED && !miss_pending && !req_valid_q;

    // Miss request
    assign miss_req_valid = ENABLED && miss_pending && !miss_issued;
    assign miss_req_addr  = miss_addr_aligned;

    // Cache control
    integer i;
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            req_valid_q      <= 1'b0;
            req_addr_q       <= 32'h0;
            tag_in_q         <= '0;
            index_in_q       <= '0;
            offset_in_q      <= '0;
            miss_pending     <= 1'b0;
            miss_issued      <= 1'b0;
            miss_tag         <= '0;
            miss_index       <= '0;
            miss_offset      <= '0;
            miss_addr_aligned <= 32'h0;
            for (i = 0; i < LINES; i = i + 1) begin
                tag_ram[i].valid <= 1'b0;
                tag_ram[i].tag   <= '0;
                data_ram[i]      <= '0;
            end
        end else begin
            // Latch request
            if (req_fire) begin
                req_valid_q <= 1'b1;
                req_addr_q  <= req_addr;
                tag_in_q    <= tag_in;
                index_in_q  <= index_in;
                offset_in_q <= offset_in;
            end

            // Detect miss (after tag/data read)
            if (req_valid_q && !hit && !miss_pending) begin
                miss_pending      <= 1'b1;
                miss_issued       <= 1'b0;
                miss_tag          <= tag_in_q;
                miss_index        <= index_in_q;
                miss_offset       <= offset_in_q;
                miss_addr_aligned <= {req_addr_q[31:OFFSET_BITS], {OFFSET_BITS{1'b0}}};
            end

            // Miss request handshake
            if (miss_req_valid && miss_req_ready) begin
                miss_issued <= 1'b1;
            end

            // Refill on miss response
            if (miss_pending && miss_resp_valid) begin
                tag_ram[miss_index].valid <= 1'b1;
                tag_ram[miss_index].tag   <= miss_tag;
                data_ram[miss_index]      <= miss_resp_data;
                miss_pending              <= 1'b0;
                miss_issued               <= 1'b0;
                req_valid_q               <= 1'b0;
            end

            // Clear request on hit response
            if (hit) begin
                req_valid_q <= 1'b0;
            end
        end
    end

    // Synthesis-time checks
    initial begin
        if (FETCH_DATA_BITS % 8 != 0) $error("FETCH_DATA_BITS must be byte-aligned");
        if (FETCH_BYTES > LINE_BYTES) $error("FETCH_DATA_BITS larger than LINE_BYTES");
    end
endmodule
