// Placeholder skeleton for graphics/ROP LSU write path
module lsu_gfx (
    input  logic        clk,
    input  logic        rst_n,
    // GFX store enqueue
    input  logic        st_valid,
    input  logic [31:0] st_addr,
    input  logic [31:0] st_data,
    input  logic [3:0]  st_strb,
    output logic        st_ready,

    // D-cache port (gfx/tex port) -- simplified for now
    output logic        dc_req_valid,
    output logic [1:0]  dc_req_type, // 0=LOAD,1=STORE
    output logic [31:0] dc_req_addr,
    output logic [127:0] dc_req_wdata,
    output logic [7:0]  dc_req_wstrb,
    output logic [7:0]  dc_req_id,
    input  logic        dc_req_ready,

    input  logic        dc_resp_valid,
    input  logic [127:0] dc_resp_data,
    input  logic [7:0]  dc_resp_id,
    input  logic        dc_resp_err,

    // Framebuffer AXI write (optional direct path)
    output logic        fb_aw_valid,
    output logic [31:0] fb_aw_addr,
    output logic [7:0]  fb_aw_len,
    output logic [2:0]  fb_aw_size,
    output logic [1:0]  fb_aw_burst,
    input  logic        fb_aw_ready,

    output logic [31:0] fb_w_data,
    output logic [3:0]  fb_w_strb,
    output logic        fb_w_last,
    output logic        fb_w_valid,
    input  logic        fb_w_ready,

    input  logic        fb_b_valid,
    output logic        fb_b_ready
);

    // Single-entry skid buffer so gfx stores are not dropped when the cache port stalls.
    logic        pend_valid;
    logic [31:0] pend_addr;
    logic [31:0] pend_data;
    logic [3:0]  pend_strb;

    wire use_pend = pend_valid;

    assign dc_req_valid = use_pend || (st_valid && !pend_valid);
    assign dc_req_type  = 2'b01; // store only
    assign dc_req_addr  = use_pend ? pend_addr : st_addr;
    assign dc_req_wdata = {96'h0, (use_pend ? pend_data : st_data)};
    assign dc_req_wstrb = {4'h0, (use_pend ? pend_strb : st_strb)};
    assign dc_req_id    = 8'h0;

    // Accept from the graphics pipeline whenever the buffer is free; cache backpressure is handled internally.
    assign st_ready = !pend_valid;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            pend_valid <= 1'b0;
            pend_addr  <= 32'h0;
            pend_data  <= 32'h0;
            pend_strb  <= 4'h0;
        end else begin
            // Capture when cache cannot take the beat immediately
            if (st_valid && !pend_valid && !dc_req_ready) begin
                pend_valid <= 1'b1;
                pend_addr  <= st_addr;
                pend_data  <= st_data;
                pend_strb  <= st_strb;
            end else if (dc_req_valid && dc_req_ready && pend_valid) begin
                pend_valid <= 1'b0;
            end
        end
    end

    // Ignore cache responses for gfx stores
    // Framebuffer AXI remains tied off
    assign fb_aw_valid    = 1'b0;
    assign fb_aw_addr     = 32'h0;
    assign fb_aw_len      = 8'h0;
    assign fb_aw_size     = 3'b010;
    assign fb_aw_burst    = 2'b01;
    assign fb_w_data      = 32'h0;
    assign fb_w_strb      = 4'h0;
    assign fb_w_last      = 1'b1;
    assign fb_w_valid     = 1'b0;
    assign fb_b_ready     = 1'b1;

endmodule
