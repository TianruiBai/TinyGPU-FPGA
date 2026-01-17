// CSR file + simple trap writes (Zicsr)
module mcu_csr (
    input  logic        clk,
    input  logic        rst_n,

    // CSR read
    input  logic [11:0] csr_addr,
    output logic [31:0] csr_rdata,

    // CSR write (from CSR instructions)
    input  logic        csr_we,
    input  logic [11:0] csr_waddr,
    input  logic [31:0] csr_wdata,

    // Trap/interrupt writes
    input  logic        trap_we,
    input  logic [31:0] trap_mepc,
    input  logic [31:0] trap_mcause,
    input  logic [31:0] trap_mtval,

    // Exposed CSRs for core control
    output logic [31:0] csr_mstatus,
    output logic [31:0] csr_mtvec
);

    import mcu_isa_pkg::*;

    logic [31:0] csr_mie, csr_mscratch, csr_mepc, csr_mcause, csr_mtval, csr_mip;
    logic [31:0] csr_cp_uid, csr_cp_caps, csr_cp_wdog, csr_cp_wdog_ctrl, csr_cp_mailbox_base, csr_cp_mailbox_status, csr_cp_trace_ctrl;

    // Combinational read
    always_comb begin
        unique case (csr_addr)
            CSR_MSTATUS: csr_rdata = csr_mstatus;
            CSR_MIE:     csr_rdata = csr_mie;
            CSR_MTVEC:   csr_rdata = csr_mtvec;
            CSR_MSCRATCH:csr_rdata = csr_mscratch;
            CSR_MEPC:    csr_rdata = csr_mepc;
            CSR_MCAUSE:  csr_rdata = csr_mcause;
            CSR_MTVAL:   csr_rdata = csr_mtval;
            CSR_MIP:     csr_rdata = csr_mip;
            CSR_CP_UID:  csr_rdata = csr_cp_uid;
            CSR_CP_CAPS: csr_rdata = csr_cp_caps;
            CSR_CP_WDOG: csr_rdata = csr_cp_wdog;
            CSR_CP_WDOG_CTRL: csr_rdata = csr_cp_wdog_ctrl;
            CSR_CP_MAILBOX_BASE: csr_rdata = csr_cp_mailbox_base;
            CSR_CP_MAILBOX_STATUS: csr_rdata = csr_cp_mailbox_status;
            CSR_CP_TRACE_CTRL: csr_rdata = csr_cp_trace_ctrl;
            default:     csr_rdata = 32'h0;
        endcase
    end

    // Sequential write
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            csr_mstatus <= 32'h0;
            csr_mie <= 32'h0;
            csr_mtvec <= 32'h0;
            csr_mscratch <= 32'h0;
            csr_mepc <= 32'h0;
            csr_mcause <= 32'h0;
            csr_mtval <= 32'h0;
            csr_mip <= 32'h0;

            csr_cp_uid <= 32'h0001_0001;
            csr_cp_caps <= 32'h0000_0001;
            csr_cp_wdog <= 32'h0;
            csr_cp_wdog_ctrl <= 32'h0;
            csr_cp_mailbox_base <= 32'h1000_0000;
            csr_cp_mailbox_status <= 32'h0;
            csr_cp_trace_ctrl <= 32'h0;
        end else begin
            if (trap_we) begin
                csr_mepc <= trap_mepc;
                csr_mcause <= trap_mcause;
                csr_mtval <= trap_mtval;
            end

            if (csr_we) begin
                unique case (csr_waddr)
                    CSR_MSTATUS: csr_mstatus <= csr_wdata;
                    CSR_MIE:     csr_mie     <= csr_wdata;
                    CSR_MTVEC:   csr_mtvec   <= csr_wdata;
                    CSR_MSCRATCH:csr_mscratch <= csr_wdata;
                    CSR_MEPC:    csr_mepc    <= csr_wdata;
                    CSR_MCAUSE:  csr_mcause  <= csr_wdata;
                    CSR_MTVAL:   csr_mtval   <= csr_wdata;
                    CSR_MIP:     csr_mip     <= csr_wdata;
                    CSR_CP_WDOG: csr_cp_wdog <= csr_wdata;
                    CSR_CP_WDOG_CTRL: csr_cp_wdog_ctrl <= csr_wdata;
                    CSR_CP_MAILBOX_BASE: csr_cp_mailbox_base <= csr_wdata;
                    CSR_CP_TRACE_CTRL: csr_cp_trace_ctrl <= csr_wdata;
                    default: ;
                endcase
            end
        end
    end

endmodule
