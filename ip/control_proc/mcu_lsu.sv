// Load/Store Unit (MEM)
module mcu_lsu #(
    parameter bit MAILBOX_ENABLE = 1'b0
) (
    input  logic        clk,
    input  logic        rst_n,

    // EX/MEM stage inputs
    input  logic        ex_mem_valid,
    input  logic [4:0]  ex_mem_rd,
    input  logic [2:0]  ex_mem_funct3,
    input  logic [31:0] ex_mem_alu,
    input  logic [31:0] ex_mem_rs2,
    input  logic        ex_mem_mem_read,
    input  logic        ex_mem_mem_write,
    input  logic [3:0]  ex_mem_wstrb,
    input  logic        ex_mem_is_amo,
    input  logic [4:0]  ex_mem_amo_funct5,

    // Data memory / MMIO interface
    output logic        dmem_req_valid,
    output logic        dmem_req_write,
    output logic [31:0] dmem_req_addr,
    output logic [31:0] dmem_req_wdata,
    output logic [3:0]  dmem_req_wstrb,
    input  logic        dmem_resp_valid,
    input  logic [31:0] dmem_resp_rdata,

    // Mailbox sideband toward endpoint (internal)
    output logic        mailbox_tx_valid,
    input  logic        mailbox_tx_ready,
    output logic [15:0] mailbox_tx_dest,
    output logic [31:0] mailbox_tx_data,
    output logic        mailbox_tx_prio,
    output logic        mailbox_tx_eop,
    output logic [3:0]  mailbox_tx_opcode,
    output logic        mailbox_rd_valid,
    input  logic        mailbox_rd_ready,
    output logic [15:0] mailbox_rd_dest,
    output logic        mailbox_rd_prio,
    output logic [3:0]  mailbox_rd_opcode,
    input  logic        mailbox_rd_resp_valid,
    output logic        mailbox_rd_resp_ready,
    input  logic [31:0] mailbox_rd_resp_data,
    input  mailbox_pkg::mailbox_tag_t mailbox_rd_resp_tag,

    // Pipeline control / WB
    output logic        mem_wait,
    output logic        wb_valid,
    output logic [4:0]  wb_rd,
    output logic [31:0] wb_data
);

    // AMO/LR/SC state
    logic [1:0] amo_state; // 0=idle,1=wait_load,2=issue_store
    logic [31:0] amo_load_data;
    logic [31:0] amo_addr;
    logic [31:0] amo_rs2;
    logic [4:0]  amo_rd;
    logic [4:0]  amo_funct5;
    logic        lr_valid;
    logic [31:0] lr_addr;

    // Mailbox tracking
    logic        mailbox_pending;
    logic        mailbox_hit;
    logic [15:0] mailbox_dest_r;
    logic [31:0] mailbox_data_r;
    logic        mailbox_load_hit;
    logic        mailbox_load_pending;
    logic [4:0]  mailbox_load_rd;
    logic [2:0]  mailbox_load_funct3;
    logic [15:0] mailbox_load_dest;
    logic        mailbox_rd_outstanding;
    wire         mailbox_ready_int;

    // Request generation
    always_comb begin
        dmem_req_valid = 1'b0;
        dmem_req_write = 1'b0;
        dmem_req_addr  = 32'h0;
        dmem_req_wdata = 32'h0;
        dmem_req_wstrb = 4'h0;

        // Mailbox accesses bypass DMEM when enabled
        if (MAILBOX_ENABLE && (mailbox_hit || mailbox_pending || mailbox_load_hit || mailbox_load_pending)) begin
            // Hold DMEM idle while mailbox handshake completes
        end else if (amo_state == 2'd2) begin

            dmem_req_valid = 1'b1;
            dmem_req_write = 1'b1;
            dmem_req_addr  = amo_addr;
            dmem_req_wdata = amo_rs2;
            dmem_req_wstrb = 4'hF;
        end else if (ex_mem_valid && ex_mem_is_amo && (amo_state == 2'd0)) begin
            dmem_req_valid = 1'b1;
            dmem_req_write = 1'b0;
            dmem_req_addr  = ex_mem_alu;
            dmem_req_wdata = 32'h0;
            dmem_req_wstrb = 4'h0;
        end else if (ex_mem_valid && (ex_mem_mem_read || ex_mem_mem_write)) begin
            dmem_req_valid = 1'b1;
            dmem_req_write = ex_mem_mem_write;
            dmem_req_addr  = ex_mem_alu;
            dmem_req_wdata = ex_mem_rs2;
            dmem_req_wstrb = ex_mem_wstrb;
        end
    end

    assign mem_wait = (amo_state != 2'd0)
                   || (ex_mem_valid && ex_mem_mem_read && !(mailbox_load_hit || mailbox_load_pending) && !dmem_resp_valid)
                   || (MAILBOX_ENABLE && (mailbox_hit || mailbox_pending || mailbox_load_hit || mailbox_load_pending));

    assign mailbox_ready_int = MAILBOX_ENABLE ? mailbox_tx_ready : 1'b1;

    // Load data formatting
    function automatic logic [31:0] load_format(input logic [31:0] data, input logic [2:0] f3, input logic [1:0] addr_low);
        logic [31:0] r;
        case (f3)
            3'b000: r = {{24{data[{addr_low,3'b000}+7]}}, data[{addr_low,3'b000} +: 8]};        // LB
            3'b001: r = {{16{data[{addr_low,3'b000}+15]}}, data[{addr_low,3'b000} +: 16]};      // LH
            3'b010: r = data;                                                                   // LW
            3'b100: r = {24'h0, data[{addr_low,3'b000} +: 8]};                                  // LBU
            3'b101: r = {16'h0, data[{addr_low,3'b000} +: 16]};                                 // LHU
            default: r = data;
        endcase
        load_format = r;
    endfunction

    // AMO / load writeback handling
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            amo_state <= 2'd0;
            amo_load_data <= 32'h0;
            amo_addr <= 32'h0;
            amo_rs2 <= 32'h0;
            amo_rd <= 5'd0;
            amo_funct5 <= 5'd0;
            lr_valid <= 1'b0;
            lr_addr <= 32'h0;
            wb_valid <= 1'b0;
            wb_rd <= 5'd0;
            wb_data <= 32'h0;
            mailbox_pending <= 1'b0;
            mailbox_load_pending <= 1'b0;
            mailbox_dest_r <= 16'h0;
            mailbox_data_r <= 32'h0;
            mailbox_load_dest <= 16'h0;
            mailbox_rd_outstanding <= 1'b0;
        end else begin
            wb_valid <= 1'b0;

            // Mailbox sideband: capture store/load to 0x7000_xxxx and hold until ready
            if (MAILBOX_ENABLE) begin
                if (!mailbox_pending && mailbox_hit) begin
                    mailbox_pending <= 1'b1;
                    mailbox_dest_r  <= ex_mem_alu[15:0];
                    mailbox_data_r  <= ex_mem_rs2;
                end else if (mailbox_pending && mailbox_ready_int) begin
                    mailbox_pending <= 1'b0;
                end

                if (!mailbox_load_pending && mailbox_load_hit) begin
                    mailbox_load_pending <= 1'b1;
                    mailbox_load_rd      <= ex_mem_rd;
                    mailbox_load_funct3  <= ex_mem_funct3;
                    mailbox_load_dest    <= ex_mem_alu[15:0];
                end else if (mailbox_load_pending && mailbox_rd_resp_valid && mailbox_rd_resp_ready) begin
                    mailbox_load_pending <= 1'b0;
                    wb_valid <= 1'b1;
                    wb_rd    <= mailbox_load_rd;
                    // Mailbox returns full word; respect requested sign/zero rules for load
                    wb_data  <= load_format(mailbox_rd_resp_data, mailbox_load_funct3, 2'b00);
                end
            end else begin
                mailbox_pending <= 1'b0;
                mailbox_load_pending <= 1'b0;
            end

            // Track mailbox read outstanding
            if (mailbox_rd_valid && mailbox_rd_ready) begin
                mailbox_rd_outstanding <= 1'b1;
            end else if (mailbox_rd_resp_valid && mailbox_rd_resp_ready) begin
                mailbox_rd_outstanding <= 1'b0;
            end

            case (amo_state)
                2'd0: begin
                    if (ex_mem_valid && ex_mem_is_amo) begin
                        amo_addr <= ex_mem_alu;
                        amo_rs2 <= ex_mem_rs2;
                        amo_rd <= ex_mem_rd;
                        amo_funct5 <= ex_mem_amo_funct5;
                        if (ex_mem_amo_funct5 == 5'b00011) begin
                            // SC.W
                            if (lr_valid && (lr_addr == ex_mem_alu)) begin
                                amo_state <= 2'd2; // issue store next
                                lr_valid <= 1'b0;
                            end else begin
                                wb_valid <= 1'b1;
                                wb_rd <= ex_mem_rd;
                                wb_data <= 32'd1; // SC failed
                                lr_valid <= 1'b0;
                            end
                        end else begin
                            amo_state <= 2'd1; // wait for load response
                        end
                    end else if (dmem_resp_valid && ex_mem_mem_read) begin
                        // Regular load completion
                        wb_valid <= 1'b1;
                        wb_rd <= ex_mem_rd;
                        wb_data <= load_format(dmem_resp_rdata, ex_mem_funct3, ex_mem_alu[1:0]);
                    end
                end

                2'd1: begin
                    if (dmem_resp_valid) begin
                        amo_load_data <= dmem_resp_rdata;
                        if (amo_funct5 == 5'b00010) begin
                            // LR.W
                            lr_valid <= 1'b1;
                            lr_addr <= amo_addr;
                            wb_valid <= 1'b1;
                            wb_rd <= amo_rd;
                            wb_data <= dmem_resp_rdata;
                            amo_state <= 2'd0;
                        end else begin
                            // AMO ops compute store data
                            case (amo_funct5)
                                5'b00000: amo_rs2 <= dmem_resp_rdata + amo_rs2;                         // AMOADD
                                5'b00100: amo_rs2 <= dmem_resp_rdata ^ amo_rs2;                         // AMOXOR
                                5'b01000: amo_rs2 <= dmem_resp_rdata | amo_rs2;                         // AMOOR
                                5'b01100: amo_rs2 <= dmem_resp_rdata & amo_rs2;                         // AMOAND
                                5'b10000: amo_rs2 <= ($signed(dmem_resp_rdata) < $signed(amo_rs2)) ? dmem_resp_rdata : amo_rs2; // AMOMIN
                                5'b10100: amo_rs2 <= ($signed(dmem_resp_rdata) > $signed(amo_rs2)) ? dmem_resp_rdata : amo_rs2; // AMOMAX
                                5'b11000: amo_rs2 <= (dmem_resp_rdata < amo_rs2) ? dmem_resp_rdata : amo_rs2;                     // AMOMINU
                                5'b11100: amo_rs2 <= (dmem_resp_rdata > amo_rs2) ? dmem_resp_rdata : amo_rs2;                     // AMOMAXU
                                default:  amo_rs2 <= amo_rs2;
                            endcase
                            amo_state <= 2'd2; // issue store
                        end
                    end
                end

                2'd2: begin
                    // store issued; complete
                    wb_valid <= 1'b1;
                    wb_rd <= amo_rd;
                    wb_data <= amo_load_data;
                    amo_state <= 2'd0;
                    lr_valid <= 1'b0;
                end
                default: amo_state <= 2'd0;
            endcase
        end
    end

    // Mailbox outputs (stable while pending)
    assign mailbox_hit        = MAILBOX_ENABLE && ex_mem_valid && ex_mem_mem_write && (ex_mem_alu[31:16] == 16'h7000);
    assign mailbox_load_hit   = MAILBOX_ENABLE && ex_mem_valid && ex_mem_mem_read  && (ex_mem_alu[31:16] == 16'h7000);
    assign mailbox_rd_valid   = MAILBOX_ENABLE ? (mailbox_load_pending && !mailbox_rd_outstanding) : 1'b0;
    assign mailbox_rd_dest    = MAILBOX_ENABLE ? mailbox_load_dest : 16'h0;
    assign mailbox_rd_prio    = 1'b0;
    assign mailbox_rd_opcode  = 4'h0;
    assign mailbox_rd_resp_ready = MAILBOX_ENABLE ? 1'b1 : 1'b0;
    assign mailbox_tx_valid  = MAILBOX_ENABLE ? mailbox_pending : 1'b0;
    assign mailbox_tx_dest   = MAILBOX_ENABLE ? mailbox_dest_r  : 16'h0;
    assign mailbox_tx_data   = MAILBOX_ENABLE ? mailbox_data_r  : 32'h0;
    assign mailbox_tx_prio   = MAILBOX_ENABLE ? 1'b0 : 1'b0;
    assign mailbox_tx_eop    = MAILBOX_ENABLE ? 1'b1 : 1'b0;
    assign mailbox_tx_opcode = MAILBOX_ENABLE ? 4'h0 : 4'h0;

endmodule
