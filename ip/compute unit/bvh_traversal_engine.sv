`timescale 1ns/1ps
// ============================================================================
// BVH Traversal Engine — Autonomous BVH tree walker
//
// Reads BVH nodes from memory, tests ray-AABB intersection for each internal
// node, and feeds leaf triangles to the triangle intersection unit. Maintains
// an internal stack for depth-first traversal.
//
// Memory interface: AXI-like request/response (connects to L2 cache port)
// ============================================================================
module bvh_traversal_engine
  import rt_pkg::*;
(
    input  logic        clk,
    input  logic        rst_n,

    // Control
    input  logic        start,        // pulse: begin traversal for loaded ray
    output logic        busy,
    output logic        done,         // pulse: traversal complete
    output logic        irq,          // interrupt request (if enabled)

    // Ray parameters (latched on start)
    input  logic signed [31:0] ray_ox, ray_oy, ray_oz,
    input  logic signed [31:0] ray_dx, ray_dy, ray_dz,
    input  logic signed [31:0] inv_dx, inv_dy, inv_dz,
    input  logic signed [31:0] ray_tmin,
    input  logic signed [31:0] ray_tmax_in,
    input  logic        [31:0] bvh_root_addr,  // byte address of root BVH node

    // Result
    output logic        result_hit,
    output logic signed [31:0] result_t,
    output logic signed [31:0] result_u,
    output logic signed [31:0] result_v,
    output logic        [31:0] result_tri_id,

    // Performance counters
    output logic [31:0] perf_nodes_visited,
    output logic [31:0] perf_tris_tested,

    // Memory read interface (to L2 cache / memory system)
    output logic        mem_req_valid,
    output logic [31:0] mem_req_addr,
    output logic [7:0]  mem_req_size,   // burst length in bytes
    output logic [7:0]  mem_req_id,
    input  logic        mem_req_ready,

    input  logic        mem_resp_valid,
    input  logic [63:0] mem_resp_data,  // 64-bit data beats
    input  logic [7:0]  mem_resp_id
);

    // -----------------------------------------------------------------------
    // FSM states
    // -----------------------------------------------------------------------
    typedef enum logic [3:0] {
        ST_IDLE         = 4'd0,
        ST_FETCH_NODE   = 4'd1,  // Issue memory read for BVH node
        ST_WAIT_NODE    = 4'd2,  // Collect node data beats
        ST_TEST_AABB    = 4'd3,  // Feed AABB to intersection unit
        ST_WAIT_AABB    = 4'd4,  // Wait for AABB result
        ST_PROCESS_HIT  = 4'd5,  // Process AABB hit: push children or fetch triangles
        ST_FETCH_TRI    = 4'd6,  // Issue memory read for triangle data
        ST_WAIT_TRI     = 4'd7,  // Collect triangle data beats
        ST_TEST_TRI     = 4'd8,  // Feed triangle to intersection unit
        ST_WAIT_TRI_RES = 4'd9,  // Wait for triangle result
        ST_POP_STACK    = 4'd10, // Pop next node from traversal stack
        ST_DONE         = 4'd11
    } state_e;

    state_e state, state_next;

    // -----------------------------------------------------------------------
    // Internal registers
    // -----------------------------------------------------------------------
    // Latched ray (persists for entire traversal)
    logic signed [31:0] r_ox, r_oy, r_oz;
    logic signed [31:0] r_dx, r_dy, r_dz;
    logic signed [31:0] r_inv_dx, r_inv_dy, r_inv_dz;
    logic signed [31:0] r_tmin;
    logic signed [31:0] r_tmax; // Updated when closer hit found

    // Traversal stack
    logic [31:0] stack_addr [0:BVH_STACK_DEPTH-1];
    logic [$clog2(BVH_STACK_DEPTH)-1:0] stack_ptr;

    // Current node data buffer (8 words = 256 bits)
    logic [31:0] node_buf [0:BVH_NODE_WORDS-1];
    logic [2:0]  node_beat_cnt; // counts 64-bit beats (4 beats = 8 words)

    // Decoded node fields
    wire [1:0]  node_type       = node_buf[0][31:30];
    wire [29:0] node_payload    = node_buf[0][29:0];
    wire [29:0] node_secondary  = node_buf[1][29:0];
    wire signed [31:0] node_aabb_min_x = $signed(node_buf[2]);
    wire signed [31:0] node_aabb_min_y = $signed(node_buf[3]);
    wire signed [31:0] node_aabb_min_z = $signed(node_buf[4]);
    wire signed [31:0] node_aabb_max_x = $signed(node_buf[5]);
    wire signed [31:0] node_aabb_max_y = $signed(node_buf[6]);
    wire signed [31:0] node_aabb_max_z = $signed(node_buf[7]);

    // Triangle data buffer (12 words = first 10 needed)
    logic [31:0] tri_buf [0:TRI_WORDS-1];
    logic [3:0]  tri_beat_cnt;

    // Leaf iteration state
    logic [31:0] leaf_tri_addr;   // Current triangle address (byte)
    logic [31:0] leaf_tri_remain; // Triangles remaining in leaf

    // Best hit so far
    logic        best_hit;
    logic signed [31:0] best_t;
    logic signed [31:0] best_u;
    logic signed [31:0] best_v;
    logic [31:0] best_tri_id;

    // Current node address
    logic [31:0] cur_node_addr;

    // -----------------------------------------------------------------------
    // AABB intersection unit wiring
    // -----------------------------------------------------------------------
    logic        aabb_in_valid, aabb_in_ready;
    logic        aabb_out_valid, aabb_out_ready;
    logic        aabb_hit;
    logic signed [31:0] aabb_t_near, aabb_t_far;
    logic [31:0] aabb_tag_out;

    ray_intersect_aabb u_aabb (
        .clk(clk), .rst_n(rst_n),
        .in_valid(aabb_in_valid), .in_ready(aabb_in_ready),
        .ray_ox(r_ox), .ray_oy(r_oy), .ray_oz(r_oz),
        .inv_dx(r_inv_dx), .inv_dy(r_inv_dy), .inv_dz(r_inv_dz),
        .ray_tmin(r_tmin), .ray_tmax(r_tmax),
        .aabb_min_x(node_aabb_min_x), .aabb_min_y(node_aabb_min_y), .aabb_min_z(node_aabb_min_z),
        .aabb_max_x(node_aabb_max_x), .aabb_max_y(node_aabb_max_y), .aabb_max_z(node_aabb_max_z),
        .tag_in(cur_node_addr),
        .out_valid(aabb_out_valid), .out_ready(aabb_out_ready),
        .hit(aabb_hit), .t_near_out(aabb_t_near), .t_far_out(aabb_t_far),
        .tag_out(aabb_tag_out)
    );

    // -----------------------------------------------------------------------
    // Triangle intersection unit wiring
    // -----------------------------------------------------------------------
    logic        tri_in_valid, tri_in_ready;
    logic        tri_out_valid, tri_out_ready;
    logic        tri_hit;
    logic signed [31:0] tri_t, tri_u, tri_v;
    logic [31:0] tri_id_out;

    ray_intersect_tri u_tri (
        .clk(clk), .rst_n(rst_n),
        .in_valid(tri_in_valid), .in_ready(tri_in_ready),
        .ray_ox(r_ox), .ray_oy(r_oy), .ray_oz(r_oz),
        .ray_dx(r_dx), .ray_dy(r_dy), .ray_dz(r_dz),
        .ray_tmin(r_tmin), .ray_tmax(r_tmax),
        .v0_x($signed(tri_buf[0])), .v0_y($signed(tri_buf[1])), .v0_z($signed(tri_buf[2])),
        .v1_x($signed(tri_buf[3])), .v1_y($signed(tri_buf[4])), .v1_z($signed(tri_buf[5])),
        .v2_x($signed(tri_buf[6])), .v2_y($signed(tri_buf[7])), .v2_z($signed(tri_buf[8])),
        .tri_id_in(tri_buf[9]),
        .out_valid(tri_out_valid), .out_ready(tri_out_ready),
        .hit(tri_hit), .t_out(tri_t), .u_out(tri_u), .v_out(tri_v),
        .tri_id_out(tri_id_out)
    );

    // -----------------------------------------------------------------------
    // Control FSM
    // -----------------------------------------------------------------------
    assign busy = (state != ST_IDLE) && (state != ST_DONE);
    assign done = (state == ST_DONE);
    assign irq  = done; // simple: IRQ on completion

    // Default: not driving intersection units
    assign aabb_in_valid = (state == ST_TEST_AABB);
    assign aabb_out_ready = (state == ST_WAIT_AABB);
    assign tri_in_valid  = (state == ST_TEST_TRI);
    assign tri_out_ready = (state == ST_WAIT_TRI_RES);

    // Memory request generation
    logic        mem_req_pending;
    logic [31:0] mem_req_addr_r;
    logic [7:0]  mem_req_size_r;
    logic [7:0]  mem_req_id_r;

    assign mem_req_valid = mem_req_pending;
    assign mem_req_addr  = mem_req_addr_r;
    assign mem_req_size  = mem_req_size_r;
    assign mem_req_id    = mem_req_id_r;

    // Outputs
    assign result_hit    = best_hit;
    assign result_t      = best_t;
    assign result_u      = best_u;
    assign result_v      = best_v;
    assign result_tri_id = best_tri_id;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state            <= ST_IDLE;
            stack_ptr        <= '0;
            node_beat_cnt    <= '0;
            tri_beat_cnt     <= '0;
            best_hit         <= 1'b0;
            mem_req_pending  <= 1'b0;
            perf_nodes_visited <= '0;
            perf_tris_tested   <= '0;
        end else begin
            case (state)
                // ---------------------------------------------------------
                ST_IDLE: begin
                    if (start) begin
                        // Latch ray parameters
                        r_ox <= ray_ox; r_oy <= ray_oy; r_oz <= ray_oz;
                        r_dx <= ray_dx; r_dy <= ray_dy; r_dz <= ray_dz;
                        r_inv_dx <= inv_dx; r_inv_dy <= inv_dy; r_inv_dz <= inv_dz;
                        r_tmin <= ray_tmin;
                        r_tmax <= ray_tmax_in;
                        cur_node_addr <= bvh_root_addr;
                        stack_ptr     <= '0;
                        best_hit      <= 1'b0;
                        best_t        <= FXP_MAX;
                        perf_nodes_visited <= '0;
                        perf_tris_tested   <= '0;
                        state <= ST_FETCH_NODE;
                    end
                end

                // ---------------------------------------------------------
                ST_FETCH_NODE: begin
                    // Issue read for BVH node (32 bytes = 4 × 64-bit beats)
                    mem_req_pending <= 1'b1;
                    mem_req_addr_r  <= cur_node_addr;
                    mem_req_size_r  <= 8'd32; // 32 bytes
                    mem_req_id_r    <= 8'hB0; // BVH node read ID
                    node_beat_cnt   <= '0;
                    state <= ST_WAIT_NODE;
                end

                // ---------------------------------------------------------
                ST_WAIT_NODE: begin
                    // Clear pending when accepted
                    if (mem_req_pending && mem_req_ready)
                        mem_req_pending <= 1'b0;

                    // Collect 64-bit response beats
                    if (mem_resp_valid && (mem_resp_id == 8'hB0)) begin
                        node_buf[{node_beat_cnt, 1'b0}]     <= mem_resp_data[31:0];
                        node_buf[{node_beat_cnt, 1'b1}]     <= mem_resp_data[63:32];
                        node_beat_cnt <= node_beat_cnt + 1;
                        if (node_beat_cnt == 3'd3) begin // 4 beats = 8 words done
                            perf_nodes_visited <= perf_nodes_visited + 1;
                            state <= ST_TEST_AABB;
                        end
                    end
                end

                // ---------------------------------------------------------
                ST_TEST_AABB: begin
                    // AABB test unit is fed via combinational aabb_in_valid
                    if (aabb_in_ready)
                        state <= ST_WAIT_AABB;
                end

                // ---------------------------------------------------------
                ST_WAIT_AABB: begin
                    if (aabb_out_valid) begin
                        if (aabb_hit) begin
                            state <= ST_PROCESS_HIT;
                        end else begin
                            // Miss: pop stack or done
                            state <= ST_POP_STACK;
                        end
                    end
                end

                // ---------------------------------------------------------
                ST_PROCESS_HIT: begin
                    if (node_type == BVH_LEAF) begin
                        // Leaf: start testing triangles
                        leaf_tri_addr   <= {node_payload, 2'b00}; // word addr → byte addr
                        leaf_tri_remain <= {2'b00, node_secondary};
                        if (node_secondary == 0) begin
                            state <= ST_POP_STACK;
                        end else begin
                            state <= ST_FETCH_TRI;
                        end
                    end else if (node_type == BVH_INTERNAL) begin
                        // Internal: push right child, traverse left child
                        if (stack_ptr < BVH_STACK_DEPTH) begin
                            stack_addr[stack_ptr] <= {node_secondary, 2'b00};
                            stack_ptr <= stack_ptr + 1;
                        end
                        cur_node_addr <= {node_payload, 2'b00};
                        state <= ST_FETCH_NODE;
                    end else begin
                        // Empty or reserved: skip
                        state <= ST_POP_STACK;
                    end
                end

                // ---------------------------------------------------------
                ST_FETCH_TRI: begin
                    // Read triangle (48 bytes = 6 × 64-bit beats)
                    mem_req_pending <= 1'b1;
                    mem_req_addr_r  <= leaf_tri_addr;
                    mem_req_size_r  <= 8'd48;
                    mem_req_id_r    <= 8'hB1; // Triangle read ID
                    tri_beat_cnt    <= '0;
                    state <= ST_WAIT_TRI;
                end

                // ---------------------------------------------------------
                ST_WAIT_TRI: begin
                    if (mem_req_pending && mem_req_ready)
                        mem_req_pending <= 1'b0;

                    if (mem_resp_valid && (mem_resp_id == 8'hB1)) begin
                        tri_buf[{tri_beat_cnt, 1'b0}]  <= mem_resp_data[31:0];
                        tri_buf[{tri_beat_cnt, 1'b1}]   <= mem_resp_data[63:32];
                        tri_beat_cnt <= tri_beat_cnt + 1;
                        if (tri_beat_cnt == 4'd5) begin // 6 beats = 12 words done
                            perf_tris_tested <= perf_tris_tested + 1;
                            state <= ST_TEST_TRI;
                        end
                    end
                end

                // ---------------------------------------------------------
                ST_TEST_TRI: begin
                    if (tri_in_ready)
                        state <= ST_WAIT_TRI_RES;
                end

                // ---------------------------------------------------------
                ST_WAIT_TRI_RES: begin
                    if (tri_out_valid) begin
                        if (tri_hit && (tri_t < best_t)) begin
                            best_hit    <= 1'b1;
                            best_t      <= tri_t;
                            best_u      <= tri_u;
                            best_v      <= tri_v;
                            best_tri_id <= tri_id_out;
                            r_tmax      <= tri_t; // Tighten t_max for closer-hit culling
                        end
                        // Next triangle or pop
                        leaf_tri_remain <= leaf_tri_remain - 1;
                        leaf_tri_addr   <= leaf_tri_addr + TRI_BYTES;
                        if (leaf_tri_remain <= 1)
                            state <= ST_POP_STACK;
                        else
                            state <= ST_FETCH_TRI;
                    end
                end

                // ---------------------------------------------------------
                ST_POP_STACK: begin
                    if (stack_ptr == 0) begin
                        state <= ST_DONE;
                    end else begin
                        stack_ptr     <= stack_ptr - 1;
                        cur_node_addr <= stack_addr[stack_ptr - 1];
                        state <= ST_FETCH_NODE;
                    end
                end

                // ---------------------------------------------------------
                ST_DONE: begin
                    // Stay until external agent reads results and re-starts
                    if (start)
                        state <= ST_IDLE; // Reset for next ray
                end

                default: state <= ST_IDLE;
            endcase
        end
    end

endmodule : bvh_traversal_engine
