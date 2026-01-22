// L1 Instruction Cache wrapper for MCU (control processor)
module mcu_icache #(
    parameter int LINE_BYTES = 32,
    parameter int LINES      = 64
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
    l1_inst_cache #(
        .ENABLED(1'b1),
        .LINE_BYTES(LINE_BYTES),
        .LINES(LINES),
        .FETCH_DATA_BITS(32)
    ) u_l1_inst_cache (
        .clk(clk),
        .rst_n(rst_n),
        .req_valid(req_valid),
        .req_addr(req_addr),
        .req_ready(req_ready),
        .resp_valid(resp_valid),
        .resp_data(resp_data),
        .resp_err(),
        .miss_req_valid(miss_req_valid),
        .miss_req_addr(miss_req_addr),
        .miss_req_ready(miss_req_ready),
        .miss_resp_valid(miss_resp_valid),
        .miss_resp_data(miss_resp_data)
    );
endmodule
