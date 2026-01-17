module icache #(
    parameter LINE_BYTES = 32,
    parameter LINES      = 64
) (
    input  logic        clk,
    input  logic        rst_n,
    // CPU side
    input  logic        req_valid,
    input  logic [31:0] req_addr,
    output logic        req_ready,
    output logic        resp_valid,
    output logic [31:0] resp_data,
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

    // Storage
    tag_entry_t tag_ram   [0:LINES-1];
    logic [LINE_BITS-1:0] data_ram  [0:LINES-1];

    logic [TAG_BITS-1:0]  tag_in;
    logic [INDEX_BITS-1:0] index_in;
    logic [OFFSET_BITS-1:0] offset_in;
    logic [TAG_BITS-1:0]  tag_rd;
    tag_entry_t           tag_entry_rd;
    logic [LINE_BITS-1:0] line_rd;

    assign tag_in    = req_addr[31 -: TAG_BITS];
    assign index_in  = req_addr[OFFSET_BITS +: INDEX_BITS];
    assign offset_in = req_addr[OFFSET_BITS-1:0];

    // Synchronous read
    always_ff @(posedge clk) begin
        tag_entry_rd <= tag_ram[index_in];
        line_rd      <= data_ram[index_in];
    end

    wire hit = tag_entry_rd.valid && (tag_entry_rd.tag == tag_in);

    // Output mux for 32-bit word aligned
    assign resp_data  = line_rd[{offset_in[OFFSET_BITS-1:2], 5'b0} +: 32];
    assign resp_valid = req_valid && hit;
    assign req_ready  = !req_valid || hit; // simple: only ready when hit or idle

    // Miss handling: issue one line refill when miss
    assign miss_req_valid = req_valid && !hit;
    assign miss_req_addr  = {req_addr[31:OFFSET_BITS], {OFFSET_BITS{1'b0}}};

    // Refill on miss response
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            integer i;
            for (i = 0; i < LINES; i = i + 1) begin
                tag_ram[i].valid <= 1'b0;
            end
        end else if (miss_resp_valid) begin
            tag_ram[index_in].valid <= 1'b1;
            tag_ram[index_in].tag   <= tag_in;
            data_ram[index_in]      <= miss_resp_data;
        end
    end
endmodule
