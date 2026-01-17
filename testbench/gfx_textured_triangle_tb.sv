`timescale 1ns/1ps

module gfx_textured_triangle_tb;
    import isa_pkg::*;

    logic clk;
    logic rst_n;
    logic flush_all;

    // Issue interface
    logic         issue_valid;
    decode_ctrl_t issue_ctrl;
    logic [31:0]  issue_op_a;
    logic [31:0]  issue_op_b;
    logic [127:0] issue_vec_a;
    logic [127:0] issue_vec_b;
    logic         queue_full;
    logic [3:0]   queue_count;

    // Writeback (ignored)
    logic         wb_valid;
    logic [4:0]   wb_rd;
    logic [127:0] wb_data;
    logic         wb_is_scalar;

    // Texture cache interface (memory model below)
    logic         tex_req_valid;
    logic [31:0]  tex_req_addr;
    logic [4:0]   tex_req_rd;
    logic         tex_req_ready;
    logic         tex_resp_valid;
    logic [31:0]  tex_resp_data;
    logic [4:0]   tex_resp_rd;

    // GFX store interface
    logic         gfx_st_valid;
    logic [31:0]  gfx_st_addr;
    logic [31:0]  gfx_st_wdata;
    logic [3:0]   gfx_st_wstrb;
    logic         gfx_st_ready;

    graphics_pipeline dut (
        .clk(clk),
        .rst_n(rst_n),
        .flush_all(flush_all),

        .issue_valid(issue_valid),
        .issue_ctrl(issue_ctrl),
        .issue_op_a(issue_op_a),
        .issue_op_b(issue_op_b),
        .issue_vec_a(issue_vec_a),
        .issue_vec_b(issue_vec_b),
        .queue_full(queue_full),
        .queue_count(queue_count),

        .wb_valid(wb_valid),
        .wb_rd(wb_rd),
        .wb_data(wb_data),
        .wb_is_scalar(wb_is_scalar),

        .tex_req_valid(tex_req_valid),
        .tex_req_addr(tex_req_addr),
        .tex_req_rd(tex_req_rd),
        .tex_req_ready(tex_req_ready),
        .tex_resp_valid(tex_resp_valid),
        .tex_resp_data(tex_resp_data),
        .tex_resp_rd(tex_resp_rd),

        .gfx_st_valid(gfx_st_valid),
        .gfx_st_addr(gfx_st_addr),
        .gfx_st_wdata(gfx_st_wdata),
        .gfx_st_wstrb(gfx_st_wstrb),
        .gfx_st_ready(gfx_st_ready)
    );

    // Clock
    initial clk = 1'b0;
    always #5 clk = ~clk;

    // Simple 1-deep synchronous memory model for tex_req/resp
    logic        pending;
    logic [31:0] last_addr;

    localparam logic [31:0] TEXEL_EXPECT = 32'hCAFE_BABE;

    function automatic logic [31:0] mem_read(input logic [31:0] addr);
        case (addr)
            // RSTATE descriptor @ 0x00001000
            32'h0000_1000: mem_read = 32'h0000_2000;          // fb_base
            32'h0000_1004: mem_read = 32'd32;                // fb_stride_bytes (8 px * 4B)
            32'h0000_1008: mem_read = 32'd0;                 // fb_format (ARGB8888)
            32'h0000_100C: mem_read = {16'd8, 16'd8};        // {fb_h, fb_w}
            32'h0000_1020: mem_read = 32'h1122_3344;         // const_color_argb (should be ignored for textured triangle)
            32'h0000_1024: mem_read = 32'h0000_1300;         // sampler_handle (pointer mode)
            32'h0000_1028: mem_read = {16'd0, 16'd0};        // scissor {y0,x0}
            32'h0000_102C: mem_read = {16'd0, 16'd0};        // scissor {h,w}
            32'h0000_1030: mem_read = 32'h0000_0002;         // flags0: bit1 ENABLE_TEXTURE

            // Sampler descriptor @ 0x00001300
            32'h0000_1300: mem_read = 32'h0000_3000;         // base
            32'h0000_1304: mem_read = 32'd16;                // stride (4 px * 4B)
            32'h0000_1308: mem_read = 32'd4;                 // width
            32'h0000_130C: mem_read = 32'd4;                 // height
            32'h0000_1310: mem_read = 32'd0;                 // format (ARGB8888)

            // Texture texel at (u=1, v=2): base + v*stride + u*4 = 0x3000 + 2*16 + 1*4 = 0x3024
            32'h0000_3024: mem_read = TEXEL_EXPECT;

            // RSETUP vertex block @ 0x00001100 (96B; 3 verts at +0x00/+0x20/+0x40)
            32'h0000_1100: mem_read = 32'd0;                 // v0.x
            32'h0000_1104: mem_read = 32'd0;                 // v0.y
            32'h0000_1110: mem_read = 32'd1;                 // v0.u (all verts same -> constant sampled uv)
            32'h0000_1114: mem_read = 32'd2;                 // v0.v

            32'h0000_1120: mem_read = 32'd6;                 // v1.x
            32'h0000_1124: mem_read = 32'd0;                 // v1.y
            32'h0000_1130: mem_read = 32'd1;                 // v1.u
            32'h0000_1134: mem_read = 32'd2;                 // v1.v

            32'h0000_1140: mem_read = 32'd0;                 // v2.x
            32'h0000_1144: mem_read = 32'd6;                 // v2.y
            32'h0000_1150: mem_read = 32'd1;                 // v2.u
            32'h0000_1154: mem_read = 32'd2;                 // v2.v

            // RRECT descriptor @ 0x00001200 (x0,y0,x1,y1,color)
            32'h0000_1200: mem_read = 32'd0;                 // x0
            32'h0000_1204: mem_read = 32'd0;                 // y0
            32'h0000_1208: mem_read = 32'd4;                 // x1
            32'h0000_120C: mem_read = 32'd4;                 // y1
            32'h0000_1210: mem_read = 32'hAABB_CCDD;         // color

            default:      mem_read = 32'hDEAD_BEEF;
        endcase
    endfunction

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            pending        <= 1'b0;
            last_addr      <= 32'h0;
            tex_resp_valid <= 1'b0;
            tex_resp_data  <= 32'h0;
            tex_resp_rd    <= 5'h0;
        end else begin
            tex_resp_valid <= 1'b0;

            if (pending) begin
                tex_resp_valid <= 1'b1;
                tex_resp_data  <= mem_read(last_addr);
                tex_resp_rd    <= 5'h0;
                pending        <= 1'b0;
            end

            if (tex_req_valid && tex_req_ready) begin
                last_addr <= tex_req_addr;
                pending   <= 1'b1;
                $display("%0t LD addr=%08h", $time, tex_req_addr);
            end
        end
    end

    int store_count;
    int quad_count;
    int rect_store_seen;
    int tex_store_seen;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            store_count <= 0;
            quad_count  <= 0;
            rect_store_seen <= 0;
            tex_store_seen  <= 0;
        end else if (gfx_st_valid && gfx_st_ready) begin
            store_count <= store_count + 1;
            $display("%0t ST addr=%08h data=%08h strb=%0h", $time, gfx_st_addr, gfx_st_wdata, gfx_st_wstrb);

            if (gfx_st_wdata == 32'hAABB_CCDD) rect_store_seen <= 1;
            if (gfx_st_wdata == TEXEL_EXPECT) tex_store_seen  <= 1;
        end

        if (rst_n && dut.raster_quad_valid && dut.raster_quad_ready) begin
            quad_count <= quad_count + 1;
            if (quad_count < 8) begin
                $display("%0t QUAD x=%0d y=%0d mask=%b", $time, dut.raster_quad_x, dut.raster_quad_y, dut.raster_quad_mask);
                if (quad_count == 0) begin
                    $display("DEBUG UV v0=(%0d,%0d) v1=(%0d,%0d) v2=(%0d,%0d)",
                             dut.v0u, dut.v0v,
                             dut.v1u, dut.v1v,
                             dut.v2u, dut.v2v);
                    $display("DEBUG bary=(%0d,%0d,%0d) tri_area=%0d",
                             $signed(dut.raster_bary_w0), $signed(dut.raster_bary_w1), $signed(dut.raster_bary_w2),
                             $signed(dut.raster_tri_area));
                    $display("DEBUG tex en=%0d base=%08h stride=%0d w=%0d h=%0d fmt=%0d",
                             dut.rop_tex_en, dut.rop_tex_base, dut.rop_tex_stride, dut.rop_tex_w, dut.rop_tex_h, dut.rop_tex_fmt);
                end
            end
        end
    end

    task automatic push_gfx(input logic [2:0] funct3, input logic [31:0] op_a);
        begin
            issue_ctrl = '0;
            issue_ctrl.is_valid = 1'b1;
            issue_ctrl.is_gfx   = 1'b1;
            issue_ctrl.funct3   = funct3;

            issue_op_a   = op_a;
            issue_op_b   = 32'h0;
            issue_vec_a  = '0;
            issue_vec_b  = '0;

            while (queue_full) @(posedge clk);

            issue_valid = 1'b1;
            @(posedge clk);
            issue_valid = 1'b0;
            @(posedge clk);
        end
    endtask

    initial begin
        flush_all     = 1'b0;
        issue_valid   = 1'b0;
        issue_ctrl    = '0;
        issue_op_a    = 32'h0;
        issue_op_b    = 32'h0;
        issue_vec_a   = '0;
        issue_vec_b   = '0;

        tex_req_ready = 1'b1;
        gfx_st_ready  = 1'b1;

        rst_n = 1'b0;
        repeat (10) @(posedge clk);
        rst_n = 1'b1;

        // RSTATE(ptr): enable texture + load sampler desc
        push_gfx(3'b000, 32'h0000_1000);
        // RSETUP(ptr)
        push_gfx(3'b001, 32'h0000_1100);
        // RDRAW()
        push_gfx(3'b010, 32'h0);

        // RRECT(ptr) should still be flat-color
        push_gfx(3'b011, 32'h0000_1200);

        repeat (2500) @(posedge clk);

        if (store_count == 0) $fatal(1, "Expected at least one store, saw 0");
        if (!tex_store_seen) $fatal(1, "Expected at least one textured store value %08h", TEXEL_EXPECT);
        if (!rect_store_seen) $fatal(1, "Expected at least one VRAM store using RRECT color AABBCCDD");

        $display("PASS: saw %0d stores; textured stores observed", store_count);
        $finish;
    end
endmodule
