`timescale 1ns/1ps
// ============================================================================
// Warp Scheduler — SIMT-style multi-warp hardware thread scheduler
//
// Manages N_WARPS hardware thread contexts, each identified by a warp_id.
// On stall (cache miss, RT query, MEMBAR), the current warp yields and
// the scheduler picks the next ready warp for execution.
//
// This module sits between the fetch unit and the rest of the pipeline,
// providing the active warp ID and PC, and accepting stall/yield signals.
//
// Design for FPGA: N_WARPS=1 degenerates to passthrough (zero overhead).
// N_WARPS=2-4 provides latency hiding for memory-bound workloads.
//
// Each warp context maintains:
//   - PC
//   - State (READY, RUNNING, STALLED, DONE)
//   - Stall reason (cache miss, RT pending, barrier, etc.)
//   - Regfile bank ID (for banked register file, external)
//   - Priority (round-robin with optional priority boost)
// ============================================================================
module warp_scheduler #(
    parameter int N_WARPS     = 4,    // Number of hardware warp contexts
    parameter int WARP_ID_W   = $clog2(N_WARPS > 1 ? N_WARPS : 2),
    parameter int PRIORITY_W  = 2     // Priority bits per warp
)(
    input  logic        clk,
    input  logic        rst_n,

    // -----------------------------------------------------------------------
    // Warp lifecycle (from cluster controller / dispatch unit)
    // -----------------------------------------------------------------------
    input  logic                    warp_alloc_valid,   // Allocate new warp
    input  logic [31:0]             warp_alloc_pc,      // Initial PC
    input  logic [PRIORITY_W-1:0]   warp_alloc_prio,    // Priority
    output logic                    warp_alloc_ready,
    output logic [WARP_ID_W-1:0]    warp_alloc_id,      // Assigned warp ID

    input  logic                    warp_free_valid,    // Free completed warp
    input  logic [WARP_ID_W-1:0]    warp_free_id,

    // -----------------------------------------------------------------------
    // Pipeline interface
    // -----------------------------------------------------------------------
    // Active warp output (drives fetch unit)
    output logic                    active_valid,
    output logic [WARP_ID_W-1:0]    active_warp_id,
    output logic [31:0]             active_pc,
    output logic [PRIORITY_W-1:0]   active_prio,

    // PC update from pipeline (branch taken, sequential advance)
    input  logic                    pc_update_valid,
    input  logic [WARP_ID_W-1:0]    pc_update_warp_id,
    input  logic [31:0]             pc_update_pc,

    // Stall/yield from pipeline
    input  logic                    yield_valid,        // Current warp cannot proceed
    input  logic [WARP_ID_W-1:0]    yield_warp_id,
    input  logic [2:0]              yield_reason,       // See YIELD_* below

    // Wake-up from memory/RT subsystem
    input  logic                    wake_valid,
    input  logic [WARP_ID_W-1:0]    wake_warp_id,

    // -----------------------------------------------------------------------
    // Status
    // -----------------------------------------------------------------------
    output logic [N_WARPS-1:0]      warp_active_mask,   // Which warps are allocated
    output logic [N_WARPS-1:0]      warp_ready_mask,    // Which warps can run
    output logic [N_WARPS-1:0]      warp_stalled_mask,  // Which warps are stalled
    output logic [$clog2(N_WARPS+1)-1:0] active_warp_count
);

    // Yield reasons
    localparam logic [2:0] YIELD_CACHE_MISS  = 3'd0;
    localparam logic [2:0] YIELD_RT_PENDING  = 3'd1;
    localparam logic [2:0] YIELD_BARRIER     = 3'd2;
    localparam logic [2:0] YIELD_MEMBAR      = 3'd3;
    localparam logic [2:0] YIELD_RESOURCE    = 3'd4;
    localparam logic [2:0] YIELD_VOLUNTARY   = 3'd5;

    // -----------------------------------------------------------------------
    // Warp state machine per warp
    // -----------------------------------------------------------------------
    typedef enum logic [1:0] {
        W_FREE     = 2'b00,
        W_READY    = 2'b01,
        W_RUNNING  = 2'b10,
        W_STALLED  = 2'b11
    } warp_state_e;

    warp_state_e             warp_state [0:N_WARPS-1];
    logic [31:0]             warp_pc    [0:N_WARPS-1];
    logic [PRIORITY_W-1:0]   warp_prio  [0:N_WARPS-1];
    logic [2:0]              warp_yield_reason [0:N_WARPS-1];

    // Current running warp
    logic [WARP_ID_W-1:0]   running_warp;
    logic                    any_running;

    // -----------------------------------------------------------------------
    // Selection logic: pick highest-priority ready warp (round-robin tiebreak)
    // -----------------------------------------------------------------------
    logic [WARP_ID_W-1:0]   rr_ptr;
    logic [WARP_ID_W-1:0]   selected_warp;
    logic                    selected_valid;

    always_comb begin
        selected_valid = 1'b0;
        selected_warp  = '0;

        // Priority scan starting from rr_ptr (round-robin fairness)
        for (int i = 0; i < N_WARPS; i++) begin
            automatic int idx = (int'(rr_ptr) + i) % N_WARPS;
            if (!selected_valid && (warp_state[idx] == W_READY)) begin
                selected_valid = 1'b1;
                selected_warp  = WARP_ID_W'(idx);
            end
        end
    end

    // -----------------------------------------------------------------------
    // Mask generation
    // -----------------------------------------------------------------------
    always_comb begin
        warp_active_mask  = '0;
        warp_ready_mask   = '0;
        warp_stalled_mask = '0;
        active_warp_count = '0;
        for (int i = 0; i < N_WARPS; i++) begin
            if (warp_state[i] != W_FREE) begin
                warp_active_mask[i] = 1'b1;
                active_warp_count   = active_warp_count + 1;
            end
            if (warp_state[i] == W_READY)   warp_ready_mask[i]   = 1'b1;
            if (warp_state[i] == W_STALLED) warp_stalled_mask[i] = 1'b1;
        end
    end

    // -----------------------------------------------------------------------
    // Allocation: find first free warp slot
    // -----------------------------------------------------------------------
    logic                    alloc_found;
    logic [WARP_ID_W-1:0]   alloc_slot;

    always_comb begin
        alloc_found = 1'b0;
        alloc_slot  = '0;
        for (int i = 0; i < N_WARPS; i++) begin
            if (!alloc_found && (warp_state[i] == W_FREE)) begin
                alloc_found = 1'b1;
                alloc_slot  = WARP_ID_W'(i);
            end
        end
    end

    assign warp_alloc_ready = alloc_found;
    assign warp_alloc_id    = alloc_slot;

    // -----------------------------------------------------------------------
    // Active warp output
    // -----------------------------------------------------------------------
    assign any_running = (warp_state[running_warp] == W_RUNNING);
    assign active_valid   = any_running;
    assign active_warp_id = running_warp;
    assign active_pc      = warp_pc[running_warp];
    assign active_prio    = warp_prio[running_warp];

    // -----------------------------------------------------------------------
    // Main FSM
    // -----------------------------------------------------------------------
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int i = 0; i < N_WARPS; i++) begin
                warp_state[i] <= W_FREE;
                warp_pc[i]    <= '0;
                warp_prio[i]  <= '0;
            end
            running_warp <= '0;
            rr_ptr       <= '0;
        end else begin

            // --- Warp allocation ---
            if (warp_alloc_valid && alloc_found) begin
                warp_state[alloc_slot] <= W_READY;
                warp_pc[alloc_slot]    <= warp_alloc_pc;
                warp_prio[alloc_slot]  <= warp_alloc_prio;
            end

            // --- Warp free ---
            if (warp_free_valid) begin
                warp_state[warp_free_id] <= W_FREE;
            end

            // --- PC update ---
            if (pc_update_valid) begin
                warp_pc[pc_update_warp_id] <= pc_update_pc;
            end

            // --- Yield: move running warp to STALLED, schedule next ---
            if (yield_valid && (warp_state[yield_warp_id] == W_RUNNING)) begin
                warp_state[yield_warp_id]        <= W_STALLED;
                warp_yield_reason[yield_warp_id] <= yield_reason;
            end

            // --- Wake: move stalled warp to READY ---
            if (wake_valid && (warp_state[wake_warp_id] == W_STALLED)) begin
                warp_state[wake_warp_id] <= W_READY;
            end

            // --- Scheduling: if no warp running, dispatch next ready ---
            if (!any_running || (yield_valid && (yield_warp_id == running_warp))) begin
                if (selected_valid) begin
                    warp_state[selected_warp] <= W_RUNNING;
                    running_warp              <= selected_warp;
                    rr_ptr                    <= WARP_ID_W'((int'(selected_warp) + 1) % N_WARPS);
                end
            end
        end
    end

endmodule : warp_scheduler
