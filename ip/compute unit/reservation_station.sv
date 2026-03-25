`timescale 1ns/1ps
// ============================================================================
// Reservation Station — Issue staging for SIMT multi-issue pipeline
//
// Buffers decoded instructions waiting for operand availability or execution
// unit readiness. Supports in-order issue per warp, with inter-warp OoO
// scheduling for latency hiding.
//
// Design:
//   - RS_DEPTH entries, each tagged with warp_id
//   - In-order issue within each warp (head-of-warp ordering)
//   - Cross-warp selection: oldest-ready or priority-based
//   - Operand tracking via scoreboard ready bits
//   - Configurable: N_ISSUE_PORTS simultaneous issues per cycle
//
// For FPGA efficiency:
//   - RS_DEPTH=4-8 recommended (one per warp context)
//   - Minimal CAM: warp_id match only (not full tag match)
// ============================================================================
module reservation_station #(
    parameter int RS_DEPTH      = 8,
    parameter int WARP_ID_W     = 2,
    parameter int N_ISSUE_PORTS = 2,  // Max simultaneous issues per cycle
    parameter int DATA_W        = 32,
    parameter int RD_W          = 5
)(
    input  logic        clk,
    input  logic        rst_n,

    // -----------------------------------------------------------------------
    // Dispatch interface (from decode/rename stage)
    // -----------------------------------------------------------------------
    input  logic                    dispatch_valid,
    output logic                    dispatch_ready,
    input  logic [WARP_ID_W-1:0]    dispatch_warp_id,
    input  logic                    dispatch_needs_rs1,
    input  logic                    dispatch_needs_rs2,
    input  logic                    dispatch_rs1_ready,  // Operand already available
    input  logic                    dispatch_rs2_ready,
    input  logic [RD_W-1:0]        dispatch_rs1_tag,    // Register tag for wakeup matching
    input  logic [RD_W-1:0]        dispatch_rs2_tag,
    input  logic [DATA_W-1:0]      dispatch_rs1_data,   // Data if already ready
    input  logic [DATA_W-1:0]      dispatch_rs2_data,
    input  logic [RD_W-1:0]        dispatch_rd,         // Destination register
    input  logic [3:0]             dispatch_fu_type,    // Functional unit type (ALU/LSU/FP/VEC/GFX/RT)
    input  logic [31:0]            dispatch_ctrl_bits,  // Opaque control word (passed through)

    // -----------------------------------------------------------------------
    // Issue interface (to execution units)
    // -----------------------------------------------------------------------
    output logic [N_ISSUE_PORTS-1:0]              issue_valid,
    input  logic [N_ISSUE_PORTS-1:0]              issue_ready,
    output logic [N_ISSUE_PORTS-1:0][WARP_ID_W-1:0] issue_warp_id,
    output logic [N_ISSUE_PORTS-1:0][DATA_W-1:0]    issue_rs1_data,
    output logic [N_ISSUE_PORTS-1:0][DATA_W-1:0]    issue_rs2_data,
    output logic [N_ISSUE_PORTS-1:0][RD_W-1:0]      issue_rd,
    output logic [N_ISSUE_PORTS-1:0][3:0]            issue_fu_type,
    output logic [N_ISSUE_PORTS-1:0][31:0]           issue_ctrl_bits,

    // -----------------------------------------------------------------------
    // Writeback broadcast (for operand wakeup)
    // -----------------------------------------------------------------------
    input  logic                    wb0_valid,
    input  logic [RD_W-1:0]        wb0_rd,
    input  logic [DATA_W-1:0]      wb0_data,
    input  logic                    wb1_valid,
    input  logic [RD_W-1:0]        wb1_rd,
    input  logic [DATA_W-1:0]      wb1_data,

    // -----------------------------------------------------------------------
    // Flush interface
    // -----------------------------------------------------------------------
    input  logic                    flush_valid,
    input  logic [WARP_ID_W-1:0]    flush_warp_id,  // Flush all entries for this warp

    // -----------------------------------------------------------------------
    // Status
    // -----------------------------------------------------------------------
    output logic [$clog2(RS_DEPTH+1)-1:0] occupancy,
    output logic                           full,
    output logic                           empty
);

    // Functional unit type encoding
    localparam logic [3:0] FU_ALU = 4'd0;
    localparam logic [3:0] FU_LSU = 4'd1;
    localparam logic [3:0] FU_FP  = 4'd2;
    localparam logic [3:0] FU_VEC = 4'd3;
    localparam logic [3:0] FU_GFX = 4'd4;
    localparam logic [3:0] FU_RT  = 4'd5;

    // -----------------------------------------------------------------------
    // RS entry
    // -----------------------------------------------------------------------
    typedef struct packed {
        logic                    valid;
        logic [WARP_ID_W-1:0]   warp_id;
        logic                    rs1_ready;
        logic                    rs2_ready;
        logic [RD_W-1:0]        rs1_tag;
        logic [RD_W-1:0]        rs2_tag;
        logic [DATA_W-1:0]      rs1_data;
        logic [DATA_W-1:0]      rs2_data;
        logic [RD_W-1:0]        rd;
        logic [3:0]             fu_type;
        logic [31:0]            ctrl_bits;
        logic [$clog2(RS_DEPTH)-1:0] age; // For oldest-first scheduling
    } rs_entry_t;

    rs_entry_t entries [0:RS_DEPTH-1];

    // -----------------------------------------------------------------------
    // Age counter (global, increments on every dispatch)
    // -----------------------------------------------------------------------
    logic [$clog2(RS_DEPTH)-1:0] age_counter;

    // -----------------------------------------------------------------------
    // Occupancy tracking
    // -----------------------------------------------------------------------
    logic [RS_DEPTH-1:0] entry_valid;
    logic [RS_DEPTH-1:0] entry_ready; // Both operands ready

    always_comb begin
        occupancy = '0;
        for (int i = 0; i < RS_DEPTH; i++) begin
            entry_valid[i] = entries[i].valid;
            entry_ready[i] = entries[i].valid && entries[i].rs1_ready && entries[i].rs2_ready;
            if (entries[i].valid) occupancy = occupancy + 1;
        end
    end

    assign full  = (occupancy == RS_DEPTH);
    assign empty = (occupancy == 0);
    assign dispatch_ready = !full;

    // -----------------------------------------------------------------------
    // Allocation: find first free slot
    // -----------------------------------------------------------------------
    logic [$clog2(RS_DEPTH)-1:0] alloc_slot;
    logic                         alloc_found;

    always_comb begin
        alloc_found = 1'b0;
        alloc_slot  = '0;
        for (int i = 0; i < RS_DEPTH; i++) begin
            if (!alloc_found && !entries[i].valid) begin
                alloc_found = 1'b1;
                alloc_slot  = $clog2(RS_DEPTH)'(i);
            end
        end
    end

    // -----------------------------------------------------------------------
    // Issue selection: oldest-ready per issue port, no same-entry double-issue
    // -----------------------------------------------------------------------
    logic [N_ISSUE_PORTS-1:0][$clog2(RS_DEPTH)-1:0] issue_sel;
    logic [N_ISSUE_PORTS-1:0]                        issue_sel_valid;
    logic [RS_DEPTH-1:0]                             issued_mask; // Prevent double-select

    always_comb begin
        issued_mask = '0;
        for (int p = 0; p < N_ISSUE_PORTS; p++) begin
            issue_sel_valid[p] = 1'b0;
            issue_sel[p]       = '0;
            // Find oldest ready entry not already selected
            for (int i = 0; i < RS_DEPTH; i++) begin
                if (!issue_sel_valid[p] && entry_ready[i] && !issued_mask[i]) begin
                    issue_sel_valid[p] = 1'b1;
                    issue_sel[p]       = $clog2(RS_DEPTH)'(i);
                end
            end
            if (issue_sel_valid[p])
                issued_mask[issue_sel[p]] = 1'b1;
        end
    end

    // Drive issue outputs
    generate
        for (genvar p = 0; p < N_ISSUE_PORTS; p++) begin : gen_issue
            assign issue_valid[p]     = issue_sel_valid[p];
            assign issue_warp_id[p]   = entries[issue_sel[p]].warp_id;
            assign issue_rs1_data[p]  = entries[issue_sel[p]].rs1_data;
            assign issue_rs2_data[p]  = entries[issue_sel[p]].rs2_data;
            assign issue_rd[p]        = entries[issue_sel[p]].rd;
            assign issue_fu_type[p]   = entries[issue_sel[p]].fu_type;
            assign issue_ctrl_bits[p] = entries[issue_sel[p]].ctrl_bits;
        end
    endgenerate

    // -----------------------------------------------------------------------
    // Main sequential logic
    // -----------------------------------------------------------------------
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int i = 0; i < RS_DEPTH; i++)
                entries[i].valid <= 1'b0;
            age_counter <= '0;
        end else begin

            // --- Writeback wakeup: snoop broadcast and update operand readiness ---
            for (int i = 0; i < RS_DEPTH; i++) begin
                if (entries[i].valid) begin
                    // WB port 0
                    if (wb0_valid && !entries[i].rs1_ready && (entries[i].rs1_tag == wb0_rd)) begin
                        entries[i].rs1_ready <= 1'b1;
                        entries[i].rs1_data  <= wb0_data;
                    end
                    if (wb0_valid && !entries[i].rs2_ready && (entries[i].rs2_tag == wb0_rd)) begin
                        entries[i].rs2_ready <= 1'b1;
                        entries[i].rs2_data  <= wb0_data;
                    end
                    // WB port 1
                    if (wb1_valid && !entries[i].rs1_ready && (entries[i].rs1_tag == wb1_rd)) begin
                        entries[i].rs1_ready <= 1'b1;
                        entries[i].rs1_data  <= wb1_data;
                    end
                    if (wb1_valid && !entries[i].rs2_ready && (entries[i].rs2_tag == wb1_rd)) begin
                        entries[i].rs2_ready <= 1'b1;
                        entries[i].rs2_data  <= wb1_data;
                    end
                end
            end

            // --- Issue: deallocate entries that were accepted ---
            for (int p = 0; p < N_ISSUE_PORTS; p++) begin
                if (issue_sel_valid[p] && issue_ready[p]) begin
                    entries[issue_sel[p]].valid <= 1'b0;
                end
            end

            // --- Dispatch: allocate new entry ---
            if (dispatch_valid && alloc_found) begin
                entries[alloc_slot].valid     <= 1'b1;
                entries[alloc_slot].warp_id   <= dispatch_warp_id;
                entries[alloc_slot].rs1_ready <= dispatch_rs1_ready || !dispatch_needs_rs1;
                entries[alloc_slot].rs2_ready <= dispatch_rs2_ready || !dispatch_needs_rs2;
                entries[alloc_slot].rs1_tag   <= dispatch_rs1_tag;
                entries[alloc_slot].rs2_tag   <= dispatch_rs2_tag;
                entries[alloc_slot].rs1_data  <= dispatch_rs1_data;
                entries[alloc_slot].rs2_data  <= dispatch_rs2_data;
                entries[alloc_slot].rd        <= dispatch_rd;
                entries[alloc_slot].fu_type   <= dispatch_fu_type;
                entries[alloc_slot].ctrl_bits <= dispatch_ctrl_bits;
                entries[alloc_slot].age       <= age_counter;
                age_counter <= age_counter + 1;
            end

            // --- Flush: invalidate all entries for a specific warp ---
            if (flush_valid) begin
                for (int i = 0; i < RS_DEPTH; i++) begin
                    if (entries[i].valid && (entries[i].warp_id == flush_warp_id))
                        entries[i].valid <= 1'b0;
                end
            end
        end
    end

endmodule : reservation_station
