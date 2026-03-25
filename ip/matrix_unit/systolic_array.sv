`timescale 1ns/1ps
// ============================================================================
// Systolic Array — 4×4 MAC array for Q16.16 fixed-point
//
// Computes C += A × B where A, B, C are TILE_DIM × TILE_DIM tiles.
// Weight-stationary architecture:
//   1. Load B tile into weight registers (one cycle per element)
//   2. Stream A rows with skew, collect C rows with de-skew
//
// Pipeline: TILE_DIM + TILE_DIM - 1 = 7 cycles for results to emerge,
//           plus register stages. Total latency = SA_LATENCY.
//
// Each PE: acc += a_in × w_reg (Q16.16 multiply-accumulate)
//          Pass a_in to the right, pass partial sum down.
//
// Interface:
//   - load_weight: pulse to load B[row][col] into PE weight registers
//   - start_compute: pulse to begin streaming A rows through the array
//   - a_row_in: one row of A per cycle (TILE_DIM elements, skewed externally)
//   - c_out: accumulated result tile (available after SA_LATENCY cycles)
//   - done: pulses when computation is complete
// ============================================================================
module systolic_array
  import mat_pkg::*;
(
    input  logic        clk,
    input  logic        rst_n,

    // Control
    input  logic        clear_acc,    // Clear all accumulators
    input  logic        load_weight,  // Load weight into PE[w_row][w_col]
    input  logic [1:0]  w_row,        // Weight load row index
    input  logic [1:0]  w_col,        // Weight load column index
    input  logic signed [31:0] w_data, // Weight data

    input  logic        compute_valid, // Activation data valid
    input  logic signed [31:0] a_in [TILE_DIM-1:0], // Activation row input

    // Output — accumulated results
    output logic        c_valid,
    output logic signed [31:0] c_out [TILE_DIM-1:0][TILE_DIM-1:0]
);

    // -----------------------------------------------------------------------
    // Processing Element (PE) storage
    // -----------------------------------------------------------------------
    logic signed [31:0] pe_weight [TILE_DIM-1:0][TILE_DIM-1:0];
    logic signed [31:0] pe_acc    [TILE_DIM-1:0][TILE_DIM-1:0];

    // DSP48-friendly multiply-accumulate
    (* use_dsp = "yes" *)
    function automatic logic signed [31:0] fxp_mac(
        input logic signed [31:0] acc,
        input logic signed [31:0] a,
        input logic signed [31:0] b
    );
        logic signed [63:0] prod;
        prod = 64'(a) * 64'(b);
        return acc + 32'(prod >>> FXP_FRAC);
    endfunction

    // Compute valid delay chain for output timing
    logic [SA_LATENCY-1:0] valid_shift;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            valid_shift <= '0;
        end else begin
            valid_shift <= {valid_shift[SA_LATENCY-2:0], compute_valid};
        end
    end

    assign c_valid = valid_shift[SA_LATENCY-1];

    // -----------------------------------------------------------------------
    // Weight loading
    // -----------------------------------------------------------------------
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int r = 0; r < TILE_DIM; r++)
                for (int c = 0; c < TILE_DIM; c++)
                    pe_weight[r][c] <= '0;
        end else if (load_weight) begin
            pe_weight[w_row][w_col] <= w_data;
        end
    end

    // -----------------------------------------------------------------------
    // Parallel MAC computation
    // -----------------------------------------------------------------------
    // Each cycle with compute_valid: PE(r,c) += a_in[r] * pe_weight[k][c]
    // where k tracks the current column of A / row of B.
    logic [1:0] compute_k;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int r = 0; r < TILE_DIM; r++)
                for (int c = 0; c < TILE_DIM; c++)
                    pe_acc[r][c] <= '0;
            compute_k <= '0;
        end else begin
            if (clear_acc) begin
                for (int r = 0; r < TILE_DIM; r++)
                    for (int c = 0; c < TILE_DIM; c++)
                        pe_acc[r][c] <= '0;
                compute_k <= '0;
            end

            if (compute_valid) begin
                for (int r = 0; r < TILE_DIM; r++)
                    for (int c = 0; c < TILE_DIM; c++)
                        pe_acc[r][c] <= fxp_mac(pe_acc[r][c], a_in[r], pe_weight[compute_k][c]);
                compute_k <= compute_k + 1;
            end
        end
    end

    // -----------------------------------------------------------------------
    // Output: accumulated results are always visible
    // -----------------------------------------------------------------------
    always_comb begin
        for (int r = 0; r < TILE_DIM; r++)
            for (int c = 0; c < TILE_DIM; c++)
                c_out[r][c] = pe_acc[r][c];
    end

endmodule : systolic_array
