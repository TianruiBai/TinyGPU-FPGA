module raster_unit (
    input  logic        clk,
    input  logic        rst_n,
    
    // Command Interface from Dispatcher
    input  logic        cmd_valid,
    input  logic [2:0]  cmd_type, // 000: NOP, 001: SET_V0, 010: SET_V1, 011: SET_V2, 100: DRAW
    input  logic [31:0] cmd_data_x,
    input  logic [31:0] cmd_data_y,
    output logic        cmd_ready,

    // Output Interface to Backend (Quad Fragment)
    output logic        quad_valid,
    input  logic        quad_ready,
    output logic [31:0] quad_x,
    output logic [31:0] quad_y,
    output logic [3:0]  quad_mask,
    output logic [31:0] quad_bary_w0,
    output logic [31:0] quad_bary_w1,
    output logic [31:0] quad_bary_w2

    ,
    output logic signed [31:0] tri_area_out

    ,
    output logic        busy
);
    // Vertex Storage
    logic [31:0] v0_x, v0_y;
    logic [31:0] v1_x, v1_y;
    logic [31:0] v2_x, v2_y;

    // Rasterizer State
    typedef enum logic [1:0] {
        IDLE,
        SETUP,
        RASTER,
        DONE
    } state_t;
    state_t state;

    // Bounding Box
    logic signed [31:0] min_x, max_x, min_y, max_y;
    logic signed [31:0] cur_x, cur_y;

    // Triangle signed area (for winding)
    logic signed [31:0] tri_area;

    // Helper: Orient 2D
    function automatic logic signed [31:0] orient2d(
        input logic signed [31:0] ax, input logic signed [31:0] ay,
        input logic signed [31:0] bx, input logic signed [31:0] by,
        input logic signed [31:0] cx, input logic signed [31:0] cy
    );
        return (bx - ax) * (cy - ay) - (by - ay) * (cx - ax);
    endfunction

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state <= IDLE;
            v0_x <= 0; v0_y <= 0;
            v1_x <= 0; v1_y <= 0;
            v2_x <= 0; v2_y <= 0;
            quad_valid <= 0;
            cmd_ready <= 1'b1;
        end else begin
            // Default ready unless busy
            cmd_ready <= (state == IDLE);
            
            // Command Processing
            if (cmd_valid && cmd_ready) begin
                case (cmd_type)
                    3'b001: begin v0_x <= cmd_data_x; v0_y <= cmd_data_y; end
                    3'b010: begin v1_x <= cmd_data_x; v1_y <= cmd_data_y; end
                    3'b011: begin v2_x <= cmd_data_x; v2_y <= cmd_data_y; end
                    3'b100: begin state <= SETUP; cmd_ready <= 1'b0; end
                    default: ;
                endcase
            end

            // Rasterization Logic
            case (state)
                IDLE: begin
                    quad_valid <= 1'b0;
                end
                
                SETUP: begin
                    // Compute Bounding Box
                    logic signed [31:0] min_x_n, max_x_n, min_y_n, max_y_n;

                    min_x_n = (signed'(v0_x) < signed'(v1_x)) ? ((signed'(v0_x) < signed'(v2_x)) ? signed'(v0_x) : signed'(v2_x)) : ((signed'(v1_x) < signed'(v2_x)) ? signed'(v1_x) : signed'(v2_x));
                    max_x_n = (signed'(v0_x) > signed'(v1_x)) ? ((signed'(v0_x) > signed'(v2_x)) ? signed'(v0_x) : signed'(v2_x)) : ((signed'(v1_x) > signed'(v2_x)) ? signed'(v1_x) : signed'(v2_x));
                    min_y_n = (signed'(v0_y) < signed'(v1_y)) ? ((signed'(v0_y) < signed'(v2_y)) ? signed'(v0_y) : signed'(v2_y)) : ((signed'(v1_y) < signed'(v2_y)) ? signed'(v1_y) : signed'(v2_y));
                    max_y_n = (signed'(v0_y) > signed'(v1_y)) ? ((signed'(v0_y) > signed'(v2_y)) ? signed'(v0_y) : signed'(v2_y)) : ((signed'(v1_y) > signed'(v2_y)) ? signed'(v1_y) : signed'(v2_y));

                    min_x <= min_x_n;
                    max_x <= max_x_n;
                    min_y <= min_y_n;
                    max_y <= max_y_n;

                    tri_area <= orient2d(v0_x, v0_y, v1_x, v1_y, v2_x, v2_y);

                    // Align to even (2x2 quads)
                    cur_x <= (min_x_n & 32'shFFFF_FFFE);
                    cur_y <= (min_y_n & 32'shFFFF_FFFE);

                    state <= RASTER;
                end

                RASTER: begin
                    logic signed [31:0] w0_cur, w1_cur, w2_cur;
                    logic p0_in, p1_in, p2_in, p3_in;
                    
                    // Pixel 00
                    w0_cur = orient2d(v1_x, v1_y, v2_x, v2_y, cur_x, cur_y);
                    w1_cur = orient2d(v2_x, v2_y, v0_x, v0_y, cur_x, cur_y);
                    w2_cur = orient2d(v0_x, v0_y, v1_x, v1_y, cur_x, cur_y);
                    if (tri_area >= 0) p0_in = (w0_cur >= 0 && w1_cur >= 0 && w2_cur >= 0);
                    else               p0_in = (w0_cur <= 0 && w1_cur <= 0 && w2_cur <= 0);
                    
                    // Pixel 10
                    if (tri_area >= 0) begin
                        p1_in = (orient2d(v1_x, v1_y, v2_x, v2_y, cur_x+1, cur_y) >= 0 && 
                                 orient2d(v2_x, v2_y, v0_x, v0_y, cur_x+1, cur_y) >= 0 && 
                                 orient2d(v0_x, v0_y, v1_x, v1_y, cur_x+1, cur_y) >= 0);
                    end else begin
                        p1_in = (orient2d(v1_x, v1_y, v2_x, v2_y, cur_x+1, cur_y) <= 0 && 
                                 orient2d(v2_x, v2_y, v0_x, v0_y, cur_x+1, cur_y) <= 0 && 
                                 orient2d(v0_x, v0_y, v1_x, v1_y, cur_x+1, cur_y) <= 0);
                    end

                    // Pixel 01
                    if (tri_area >= 0) begin
                        p2_in = (orient2d(v1_x, v1_y, v2_x, v2_y, cur_x, cur_y+1) >= 0 && 
                                 orient2d(v2_x, v2_y, v0_x, v0_y, cur_x, cur_y+1) >= 0 && 
                                 orient2d(v0_x, v0_y, v1_x, v1_y, cur_x, cur_y+1) >= 0);
                    end else begin
                        p2_in = (orient2d(v1_x, v1_y, v2_x, v2_y, cur_x, cur_y+1) <= 0 && 
                                 orient2d(v2_x, v2_y, v0_x, v0_y, cur_x, cur_y+1) <= 0 && 
                                 orient2d(v0_x, v0_y, v1_x, v1_y, cur_x, cur_y+1) <= 0);
                    end

                    // Pixel 11
                    if (tri_area >= 0) begin
                        p3_in = (orient2d(v1_x, v1_y, v2_x, v2_y, cur_x+1, cur_y+1) >= 0 && 
                                 orient2d(v2_x, v2_y, v0_x, v0_y, cur_x+1, cur_y+1) >= 0 && 
                                 orient2d(v0_x, v0_y, v1_x, v1_y, cur_x+1, cur_y+1) >= 0);
                    end else begin
                        p3_in = (orient2d(v1_x, v1_y, v2_x, v2_y, cur_x+1, cur_y+1) <= 0 && 
                                 orient2d(v2_x, v2_y, v0_x, v0_y, cur_x+1, cur_y+1) <= 0 && 
                                 orient2d(v0_x, v0_y, v1_x, v1_y, cur_x+1, cur_y+1) <= 0);
                    end

                    if (p0_in || p1_in || p2_in || p3_in) begin
                        quad_valid <= 1'b1;
                        quad_x <= cur_x;
                        quad_y <= cur_y;
                        quad_mask <= {p3_in, p2_in, p1_in, p0_in};
                        quad_bary_w0 <= w0_cur;
                        quad_bary_w1 <= w1_cur;
                        quad_bary_w2 <= w2_cur;
                    end else begin
                        quad_valid <= 1'b0;
                    end
                    
                    // Scanline stepping
                    if (quad_ready || !quad_valid) begin 
                        if (cur_x + 2 > max_x) begin
                            cur_x <= min_x & (~31'h1);
                            if (cur_y + 2 > max_y) begin
                                state <= DONE;
                            end else begin
                                cur_y <= cur_y + 2;
                            end
                        end else begin
                            cur_x <= cur_x + 2;
                        end
                    end
                end
                
                DONE: begin
                    state <= IDLE;
                    cmd_ready <= 1'b1;
                end
            endcase
            
            // Handshake output
            if (quad_valid && quad_ready) begin
                quad_valid <= 1'b0; // Consumed
            end
        end
    end

    assign tri_area_out = tri_area;

    // Busy whenever a draw is active or a quad is pending
    always_comb begin
        busy = (state != IDLE) || quad_valid;
    end

endmodule
