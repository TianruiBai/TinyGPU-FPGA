module csr_file (
    input  logic        clk,
    input  logic        rst_n,
    // CSR access
    input  logic        csr_en,
    input  logic        csr_csrrs,   // 0: CSRRW, 1: CSRRS
    input  logic [11:0] csr_addr,
    input  logic [31:0] csr_wdata,
    output logic [31:0] csr_rdata,
    // Static identifiers
    input  logic [31:0] core_id,
    input  logic [31:0] tile_offset,
    // Error flag inputs (sticky until cleared by write)
    input  logic        fp_err_overflow,
    input  logic        fp_err_invalid,
    input  logic        vec_err_overflow,
    input  logic        vec_err_invalid,
    // Optional visibility of current CSR contents
    output logic [31:0] status_out,
    output logic [31:0] fstatus_out,
    output logic [31:0] vstatus_out
);
    // CSR map (small subset per isa_instructions.md)
    localparam logic [11:0] CSR_STATUS     = 12'h000; // bit0 fp_invalid, bit1 fp_overflow, bit2 vec_invalid, bit3 vec_overflow
    localparam logic [11:0] CSR_FSTATUS    = 12'h001; // bit0 fp_invalid, bit1 fp_overflow
    localparam logic [11:0] CSR_VSTATUS    = 12'h002; // bit0 vec_invalid, bit1 vec_overflow
    localparam logic [11:0] CSR_CONFIG     = 12'h003; // placeholder config
    localparam logic [11:0] CSR_CORE_ID    = 12'hC00; // read-only
    localparam logic [11:0] CSR_TILE_OFF   = 12'hC01; // read-only

    logic [31:0] r_status;
    logic [31:0] r_fstatus;
    logic [31:0] r_vstatus;
    logic [31:0] r_config;

    // Sticky error accumulation
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            r_status  <= 32'h0;
            r_fstatus <= 32'h0;
            r_vstatus <= 32'h0;
            r_config  <= 32'h0;
        end else begin
            // accumulate error pulses
            if (fp_err_invalid)  begin r_status[0]  <= 1'b1; r_fstatus[0] <= 1'b1; end
            if (fp_err_overflow) begin r_status[1]  <= 1'b1; r_fstatus[1] <= 1'b1; end
            if (vec_err_invalid) begin r_status[2]  <= 1'b1; r_vstatus[0] <= 1'b1; end
            if (vec_err_overflow)begin r_status[3]  <= 1'b1; r_vstatus[1] <= 1'b1; end

            if (csr_en) begin
                case (csr_addr)
                    CSR_STATUS: begin
                        if (csr_csrrs) r_status  <= r_status  | csr_wdata;
                        else            r_status  <= csr_wdata;
                    end
                    CSR_FSTATUS: begin
                        if (csr_csrrs) r_fstatus <= r_fstatus | csr_wdata;
                        else            r_fstatus <= csr_wdata;
                    end
                    CSR_VSTATUS: begin
                        if (csr_csrrs) r_vstatus <= r_vstatus | csr_wdata;
                        else            r_vstatus <= csr_wdata;
                    end
                    CSR_CONFIG: begin
                        if (csr_csrrs) r_config  <= r_config  | csr_wdata;
                        else            r_config  <= csr_wdata;
                    end
                    default: ; // read-only or unused
                endcase
            end
        end
    end

    always_comb begin
        case (csr_addr)
            CSR_STATUS:   csr_rdata = r_status;
            CSR_FSTATUS:  csr_rdata = r_fstatus;
            CSR_VSTATUS:  csr_rdata = r_vstatus;
            CSR_CONFIG:   csr_rdata = r_config;
            CSR_CORE_ID:  csr_rdata = core_id;
            CSR_TILE_OFF: csr_rdata = tile_offset;
            default:      csr_rdata = 32'h0;
        endcase
    end

    assign status_out  = r_status;
    assign fstatus_out = r_fstatus;
    assign vstatus_out = r_vstatus;
endmodule
