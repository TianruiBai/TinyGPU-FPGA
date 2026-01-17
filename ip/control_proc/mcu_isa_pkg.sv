`ifndef MCU_ISA_PKG_SV
`define MCU_ISA_PKG_SV

package mcu_isa_pkg;

    // Opcodes
    localparam logic [6:0] OPC_LUI     = 7'b0110111;
    localparam logic [6:0] OPC_AUIPC   = 7'b0010111;
    localparam logic [6:0] OPC_JAL     = 7'b1101111;
    localparam logic [6:0] OPC_JALR    = 7'b1100111;
    localparam logic [6:0] OPC_BRANCH  = 7'b1100011;
    localparam logic [6:0] OPC_LOAD    = 7'b0000011;
    localparam logic [6:0] OPC_STORE   = 7'b0100011;
    localparam logic [6:0] OPC_OPIMM   = 7'b0010011;
    localparam logic [6:0] OPC_OP      = 7'b0110011;
    localparam logic [6:0] OPC_SYSTEM  = 7'b1110011;
    localparam logic [6:0] OPC_AMO     = 7'b0101111;

    // CSR addresses (minimal subset)
    localparam logic [11:0] CSR_MSTATUS        = 12'h300;
    localparam logic [11:0] CSR_MIE            = 12'h304;
    localparam logic [11:0] CSR_MTVEC          = 12'h305;
    localparam logic [11:0] CSR_MSCRATCH       = 12'h340;
    localparam logic [11:0] CSR_MEPC           = 12'h341;
    localparam logic [11:0] CSR_MCAUSE         = 12'h342;
    localparam logic [11:0] CSR_MTVAL          = 12'h343;
    localparam logic [11:0] CSR_MIP            = 12'h344;

    // Custom CSRs (control CPU)
    localparam logic [11:0] CSR_CP_UID          = 12'h7C0;
    localparam logic [11:0] CSR_CP_CAPS         = 12'h7C1;
    localparam logic [11:0] CSR_CP_WDOG         = 12'h7C2;
    localparam logic [11:0] CSR_CP_WDOG_CTRL    = 12'h7C3;
    localparam logic [11:0] CSR_CP_MAILBOX_BASE = 12'h7C4;
    localparam logic [11:0] CSR_CP_MAILBOX_STATUS = 12'h7C5;
    localparam logic [11:0] CSR_CP_TRACE_CTRL   = 12'h7C6;

    typedef enum logic [1:0] {
        WB_ALU = 2'd0,
        WB_MEM = 2'd1,
        WB_CSR = 2'd2
    } wb_sel_e;

    typedef enum logic [1:0] {
        CSR_NONE = 2'd0,
        CSR_W    = 2'd1,
        CSR_S    = 2'd2,
        CSR_C    = 2'd3
    } csr_op_e;

    typedef struct packed {
        logic       mem_read;
        logic       mem_write;
        logic [3:0] wstrb;
        logic       wb_en;
        wb_sel_e    wb_sel;
        logic [11:0] csr_addr;
        csr_op_e    csr_op;
        logic       csr_imm;
        logic       is_amo;
        logic [4:0] amo_funct5;
        logic       uses_rs2;
    } decode_ctrl_t;

endpackage

`endif // MCU_ISA_PKG_SV
