module local_mem_banked #(
    parameter DEPTH_WORDS = 1024 // 4 banks * 1024 words * 4 bytes = 16KB
) (
    input  logic         clk,
    input  logic         rst_n,
    input  logic         req_valid,
    input  logic         req_we,
    input  logic         req_is_vector,
    input  logic [1:0]   req_bank_sel,
    input  logic [31:0]  req_addr,
    input  logic [127:0] req_wdata,
    output logic [127:0] resp_rdata
);
    localparam ADDR_LSB = 2; // word aligned
    localparam INDEX_W  = $clog2(DEPTH_WORDS);

    logic [INDEX_W-1:0] index;

    logic [31:0] bank0 [0:DEPTH_WORDS-1];
    logic [31:0] bank1 [0:DEPTH_WORDS-1];
    logic [31:0] bank2 [0:DEPTH_WORDS-1];
    logic [31:0] bank3 [0:DEPTH_WORDS-1];

    assign index = req_addr[ADDR_LSB +: INDEX_W];

    // Synchronous read and write to map cleanly to BRAM
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            resp_rdata <= '0;
        end else if (req_valid) begin
            // Write path
            if (req_we) begin
                if (req_is_vector) begin
                    bank0[index] <= req_wdata[31:0];
                    bank1[index] <= req_wdata[63:32];
                    bank2[index] <= req_wdata[95:64];
                    bank3[index] <= req_wdata[127:96];
                end else begin
                    case (req_bank_sel)
                        2'b00: bank0[index] <= req_wdata[31:0];
                        2'b01: bank1[index] <= req_wdata[31:0];
                        2'b10: bank2[index] <= req_wdata[31:0];
                        default: bank3[index] <= req_wdata[31:0];
                    endcase
                end
            end

            // Read path (1-cycle latency)
            resp_rdata[31:0]   <= bank0[index];
            resp_rdata[63:32]  <= bank1[index];
            resp_rdata[95:64]  <= bank2[index];
            resp_rdata[127:96] <= bank3[index];
        end
    end
endmodule
