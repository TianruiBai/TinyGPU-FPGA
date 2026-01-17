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
    output logic [31:0] vstatus_out,
    output logic [15:0] vmask_out,

    // Command streamer configuration outputs
    output logic        cmd_enable,
    output logic [31:0] cmd_ring_base,
    output logic [31:0] cmd_ring_size_bytes,
    output logic [31:0] cmd_cons_ptr_bytes,
    output logic [31:0] cmd_completion_base
);
    // CSR map (small subset per isa_instructions.md)
    localparam logic [11:0] CSR_STATUS     = 12'h000; // bit0 fp_invalid, bit1 fp_overflow, bit2 vec_invalid, bit3 vec_overflow
    localparam logic [11:0] CSR_FSTATUS    = 12'h001; // bit0 fp_invalid, bit1 fp_overflow
    localparam logic [11:0] CSR_VSTATUS    = 12'h002; // bit0 vec_invalid, bit1 vec_overflow
    localparam logic [11:0] CSR_CONFIG     = 12'h003; // placeholder config
        localparam logic [11:0] CSR_VMASK      = 12'h004; // 16-bit predicate mask for vm predication
    localparam logic [11:0] CSR_CORE_ID    = 12'hC00; // read-only
    localparam logic [11:0] CSR_TILE_OFF   = 12'hC01; // read-only

    // Command streamer CSRs (host/firmware writes before enabling)
    localparam logic [11:0] CSR_CMD_RING_BASE = 12'h310;
    localparam logic [11:0] CSR_CMD_RING_SIZE = 12'h311;
    localparam logic [11:0] CSR_CMD_CONS_PTR  = 12'h312;
    localparam logic [11:0] CSR_CMD_COMP_BASE = 12'h313;

    logic [31:0] r_status;
    logic [31:0] r_fstatus;
    logic [31:0] r_vstatus;
    logic [31:0] r_config;
        logic [15:0] r_vmask;

    logic [31:0] r_cmd_ring_base;
    logic [31:0] r_cmd_ring_size;
    logic [31:0] r_cmd_cons_ptr;
    logic [31:0] r_cmd_comp_base;

    // Sticky error accumulation
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            r_status  <= 32'h0;
            r_fstatus <= 32'h0;
            r_vstatus <= 32'h0;
            r_config  <= 32'h0;
                r_vmask   <= 16'hFFFF; // default all lanes active

            r_cmd_ring_base <= 32'h0;
            r_cmd_ring_size <= 32'h0;
            r_cmd_cons_ptr  <= 32'h0;
            r_cmd_comp_base <= 32'h0;
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
                    CSR_CMD_RING_BASE: begin
                        if (csr_csrrs) r_cmd_ring_base <= r_cmd_ring_base | csr_wdata;
                        else           r_cmd_ring_base <= csr_wdata;
                    end
                    CSR_CMD_RING_SIZE: begin
                        if (csr_csrrs) r_cmd_ring_size <= r_cmd_ring_size | csr_wdata;
                        else           r_cmd_ring_size <= csr_wdata;
                    end
                    CSR_CMD_CONS_PTR: begin
                        if (csr_csrrs) r_cmd_cons_ptr <= r_cmd_cons_ptr | csr_wdata;
                        else           r_cmd_cons_ptr <= csr_wdata;
                    end
                    CSR_CMD_COMP_BASE: begin
                        if (csr_csrrs) r_cmd_comp_base <= r_cmd_comp_base | csr_wdata;
                        else           r_cmd_comp_base <= csr_wdata;
                    end
                        CSR_VMASK: begin
                            if (csr_csrrs) r_vmask <= r_vmask | csr_wdata[15:0];
                            else            r_vmask <= csr_wdata[15:0];
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
            CSR_CMD_RING_BASE: csr_rdata = r_cmd_ring_base;
            CSR_CMD_RING_SIZE: csr_rdata = r_cmd_ring_size;
            CSR_CMD_CONS_PTR:  csr_rdata = r_cmd_cons_ptr;
            CSR_CMD_COMP_BASE: csr_rdata = r_cmd_comp_base;
                CSR_VMASK:    csr_rdata = {16'h0, r_vmask};
            CSR_CORE_ID:  csr_rdata = core_id;
            CSR_TILE_OFF: csr_rdata = tile_offset;
            default:      csr_rdata = 32'h0;
        endcase
    end

    assign status_out  = r_status;
    assign fstatus_out = r_fstatus;
    assign vstatus_out = r_vstatus;
    assign vmask_out   = r_vmask;

    // Command streamer config exposure
    assign cmd_enable          = r_config[0];
    assign cmd_ring_base       = r_cmd_ring_base;
    assign cmd_ring_size_bytes = r_cmd_ring_size;
    assign cmd_cons_ptr_bytes  = r_cmd_cons_ptr;
    assign cmd_completion_base = r_cmd_comp_base;
endmodule
