module raster_unit (
    input  logic        clk,
    input  logic        rst_n,
    
    // Interface to Triangle Setup / Input
    input  logic        tri_valid,
    output logic        tri_ready,
    input  logic [31:0] tri_x0, tri_y0,
    input  logic [31:0] tri_x1, tri_y1,
    input  logic [31:0] tri_x2, tri_y2,
    // Additional setup data (edge equations, etc.) would go here or be computed internally

    // Interface to Core / Scheduler
    output logic        quad_valid,
    input  logic        quad_ready,
    output logic [31:0] quad_x,
    output logic [31:0] quad_y,
    output logic [3:0]  quad_mask, // Coverage mask for 2x2 pixels
    output logic [31:0] quad_bary_w0,
    output logic [31:0] quad_bary_w1,
    output logic [31:0] quad_bary_w2
);

    // TODO: Implement Step 6: Hardware Rasterization & Triangle Setup
    // 1. Edge equation evaluation
    // 2. Bounding box traversal
    // 3. Coverage mask generation
    // 4. Barycentric interpolation

endmodule
