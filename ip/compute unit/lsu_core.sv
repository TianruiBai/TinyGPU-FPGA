// Refactored core LSU (slot0/slot1) with atomic RMW sequencing.
module lsu_core #(
    parameter bit MAILBOX_ENABLE = 1'b0
) (
    input  logic         clk,
    input  logic         rst_n,
    // Pipeline request
    input  logic         req_valid,
    input  logic         req_is_store,
    input  logic         req_is_vector,
    input  logic         req_is_atomic,
    input  logic [2:0]   req_funct3,
    input  logic [1:0]   req_vec_mode,
    input  logic [31:0]  req_addr,
    input  logic [127:0] req_wdata,
    input  logic [7:0]   req_wstrb,
    input  logic [3:0]   req_vec_wmask,
    input  logic [4:0]   req_rd,
    output logic         req_ready,

    // L1 data cache port (single-port view for now; will map to lsu0/lsu1)
    output logic         dc_req_valid,
    output logic [1:0]   dc_req_type, // 0=LOAD,1=STORE,2=ATOM (unused here)
    output logic [2:0]   dc_req_atomic_op,
    output logic [31:0]  dc_req_addr,
    output logic [127:0] dc_req_wdata,
    output logic [7:0]   dc_req_wstrb,
    output logic         dc_req_is_vector,
    output logic [3:0]   dc_req_vec_wmask,
    output logic [7:0]   dc_req_id,
    input  logic         dc_req_ready,

    input  logic         dc_resp_valid,
    input  logic [127:0] dc_resp_data,
    input  logic [7:0]   dc_resp_id,
    input  logic         dc_resp_err,

    // Writeback toward pipeline
    output logic         wb_valid,
    output logic         wb_is_vector,
    output logic [4:0]   wb_rd,
    output logic [127:0] wb_data,
    output logic         busy,

    // Mailbox sideband (kept for parity with existing LSU; not yet wired)
    output logic         mailbox_tx_valid,
    output logic [15:0]  mailbox_tx_dest,
    output logic [31:0]  mailbox_tx_data,
    output logic         mailbox_tx_prio,
    output logic         mailbox_tx_eop,
    output logic [3:0]   mailbox_tx_opcode,
    input  logic         mailbox_tx_ready,
    output logic         mailbox_rd_valid,
    input  logic         mailbox_rd_ready,
    output logic [15:0]  mailbox_rd_dest,
    output logic         mailbox_rd_prio,
    output logic [3:0]   mailbox_rd_opcode,
    input  logic         mailbox_rd_resp_valid,
    input  logic [31:0]  mailbox_rd_resp_data,
    output logic         mailbox_rd_resp_ready
);

    import mailbox_pkg::*;

    localparam logic [15:0] MAILBOX_ADDR_MSB = 16'h7000;

    // Simple single-outstanding tracker
    logic [4:0]  pend_rd;
    logic        pend_is_vector;
    logic [1:0]  pend_type;

    // Atomic single-request sequencing (cache applies op and returns old value)
    logic         atomic_pending;
    logic         atom_is_vector;
    logic [2:0]   atom_op;
    logic [3:0]   atom_vec_wmask;
    logic [7:0]   atom_wstrb;
    logic [4:0]   atom_rd;
    logic [31:0]  atom_addr;
    logic [127:0] atom_src;
    logic [31:0]  atom_src_s;
    logic [127:0] atom_old;
    logic [31:0]  atom_old_s;

    // Mailbox tracking
    logic        mb_is_hit;
    logic        mb_is_store;
    logic        mb_is_load;
    logic        mb_load_pending;
    logic        mb_load_req_sent;
    logic [4:0]  mb_load_rd;
    logic [2:0]  mb_load_funct3;
    logic [15:0] mb_dest;
    logic [31:0] mb_wdata;

    // Ready when not busy and cache can accept
    assign req_ready = !busy && (
        mb_is_store ? mailbox_tx_ready :
        mb_is_load  ? mailbox_rd_ready :
                      dc_req_ready
    );

    // Combinational request mux: either pipeline request or in-flight atomic
    logic        dc_req_valid_r;
    logic [1:0]  dc_req_type_r;
    logic [2:0]  dc_req_atomic_op_r;
    logic [31:0] dc_req_addr_r;
    logic [127:0] dc_req_wdata_r;
    logic [7:0]  dc_req_wstrb_r;
    logic        dc_req_is_vector_r;
    logic [3:0]  dc_req_vec_wmask_r;
    logic [7:0]  dc_req_id_r;

    always_comb begin
        dc_req_valid_r     = 1'b0;
        dc_req_type_r      = 2'b00;
        dc_req_atomic_op_r = 3'b000;
        dc_req_addr_r      = 32'h0;
        dc_req_wdata_r     = 128'h0;
        dc_req_wstrb_r     = 8'h0;
        dc_req_is_vector_r = 1'b0;
        dc_req_vec_wmask_r = 4'h0;
        dc_req_id_r        = 8'h0;

        if (atomic_pending) begin
            // Single atomic request: cache performs RMW and returns old value
            dc_req_valid_r     = !mb_is_hit;
            dc_req_type_r      = 2'b10;
            dc_req_atomic_op_r = atom_op;
            dc_req_addr_r      = atom_addr;
            dc_req_wdata_r     = atom_src;
            dc_req_wstrb_r     = atom_wstrb;
            dc_req_is_vector_r = atom_is_vector;
            dc_req_vec_wmask_r = atom_vec_wmask;
            dc_req_id_r        = {3'b000, atom_rd};
        end else begin
            dc_req_valid_r     = req_valid && !busy && !mb_is_hit;
            dc_req_type_r      = req_is_atomic ? 2'b10 : (req_is_store ? 2'b01 : 2'b00);
            dc_req_atomic_op_r = req_funct3;
            dc_req_addr_r      = req_addr;
            dc_req_wdata_r     = req_wdata;
            dc_req_wstrb_r     = req_wstrb;
            dc_req_is_vector_r = req_is_vector;
            dc_req_vec_wmask_r = req_vec_wmask;
            dc_req_id_r        = {3'b000, req_rd};
        end
    end

    assign dc_req_valid     = dc_req_valid_r;
    assign dc_req_type      = dc_req_type_r;
    assign dc_req_atomic_op = dc_req_atomic_op_r;
    assign dc_req_addr      = dc_req_addr_r;
    assign dc_req_wdata     = dc_req_wdata_r;
    assign dc_req_wstrb     = dc_req_wstrb_r;
    assign dc_req_is_vector = dc_req_is_vector_r;
    assign dc_req_vec_wmask = dc_req_vec_wmask_r;
    assign dc_req_id        = dc_req_id_r;

    // Busy bookkeeping and atomic control
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            busy              <= 1'b0;
            pend_rd           <= 5'h0;
            pend_is_vector    <= 1'b0;
            pend_type         <= 2'b00;
            atomic_pending    <= 1'b0;
            atom_is_vector    <= 1'b0;
            atom_op           <= 3'b000;
            atom_vec_wmask    <= 4'h0;
            atom_wstrb        <= 8'h0;
            atom_rd           <= 5'h0;
            atom_addr         <= 32'h0;
            atom_src          <= 128'h0;
            atom_src_s        <= 32'h0;
            atom_old          <= 128'h0;
            atom_old_s        <= 32'h0;
            mb_load_pending   <= 1'b0;
            mb_load_req_sent  <= 1'b0;
            mb_load_rd        <= 5'h0;
            mb_load_funct3    <= 3'h0;
            mb_dest           <= 16'h0;
            mb_wdata          <= 32'h0;
        end else begin
            logic busy_next;
            busy_next = busy;

            // Clear takes priority over set in the same cycle (fixes hit-response deadlock)
            if (dc_resp_valid) begin
                busy_next = 1'b0;
                atomic_pending <= 1'b0;
            end

            if (mailbox_rd_resp_valid && mb_load_pending) begin
                busy_next        = 1'b0;
                mb_load_pending  <= 1'b0;
                mb_load_req_sent <= 1'b0;
            end

            // Set on acceptance
            if (req_valid && req_ready) begin
                if (mb_is_store) begin
                    busy_next = 1'b0;
                end else if (mb_is_load) begin
                    busy_next        = 1'b1;
                    mb_load_pending  <= 1'b1;
                    mb_load_req_sent <= 1'b0;
                    mb_load_rd       <= req_rd;
                    mb_load_funct3   <= req_funct3;
                    mb_dest          <= req_addr[15:0];
                end else if (req_is_atomic) begin
                    // Atomic issues as single request
                    busy_next        = 1'b1;
                    atomic_pending   <= 1'b1;
                    atom_is_vector   <= req_is_vector;
                    atom_op          <= req_funct3;
                    atom_vec_wmask   <= req_vec_wmask;
                    atom_wstrb       <= req_wstrb;
                    atom_rd          <= req_rd;
                    atom_addr        <= req_addr;
                    atom_src         <= req_wdata;
                    atom_src_s       <= req_wdata[31:0];
                    pend_rd          <= req_rd;
                    pend_is_vector   <= req_is_vector;
                    pend_type        <= 2'b10;
                end else begin
                    busy_next     = (dc_req_type_r != 2'b01); // stores don't need response for WB
                    pend_rd        <= req_rd;
                    pend_is_vector <= req_is_vector;
                    pend_type      <= dc_req_type_r;
                end
                mb_wdata <= req_wdata[31:0];
            end

            busy <= busy_next;
        end
    end

    // Atomic data path: capture old value for scalar/vector AMO return
    always_ff @(posedge clk or negedge rst_n) begin
        integer li;
        logic signed [31:0] atom_a;
        logic signed [31:0] atom_b;
        if (!rst_n) begin
            atom_old       <= 128'h0;
            atom_old_s     <= 32'h0;
        end else begin
            if (dc_resp_valid && atomic_pending) begin
                if (atom_is_vector) begin
                    atom_old <= dc_resp_data;
                end else begin
                    atom_old_s <= dc_resp_data[31:0];
                end
            end
        end
    end

    // Writeback
    assign wb_valid     = (dc_resp_valid && !dc_resp_err)
                       || (MAILBOX_ENABLE && mb_load_pending && mailbox_rd_resp_valid);
    assign wb_is_vector = (MAILBOX_ENABLE && mb_load_pending) ? 1'b0 : pend_is_vector;
    assign wb_rd        = (MAILBOX_ENABLE && mb_load_pending) ? mb_load_rd : pend_rd;
    assign wb_data      = (MAILBOX_ENABLE && mb_load_pending) ? {96'h0, mailbox_rd_resp_data}
                                                              : dc_resp_data;

    // Mailbox outputs (not implemented yet)
    assign mb_is_hit    = MAILBOX_ENABLE && !req_is_vector && !req_is_atomic && (req_addr[31:16] == MAILBOX_ADDR_MSB);
    assign mb_is_store  = mb_is_hit && req_is_store;
    assign mb_is_load   = mb_is_hit && !req_is_store;

    assign mailbox_tx_valid  = req_valid && !busy && mb_is_store;
    assign mailbox_tx_dest   = req_addr[15:0];
    assign mailbox_tx_data   = req_wdata[31:0];
    assign mailbox_tx_prio   = 1'b0;
    assign mailbox_tx_eop    = 1'b1;
    assign mailbox_tx_opcode = OPC_DATA;

    assign mailbox_rd_valid  = mb_load_pending && !mb_load_req_sent;
    assign mailbox_rd_dest   = mb_dest;
    assign mailbox_rd_prio   = 1'b0;
    assign mailbox_rd_opcode = OPC_DATA;
    assign mailbox_rd_resp_ready = 1'b1;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            mb_load_req_sent <= 1'b0;
        end else if (mb_load_pending && !mb_load_req_sent && mailbox_rd_ready) begin
            mb_load_req_sent <= 1'b1;
        end else if (!mb_load_pending) begin
            mb_load_req_sent <= 1'b0;
        end
    end

endmodule
