`timescale 1ns/1ps

module gfx_gdraw_tb;
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

    localparam logic [31:0] MATERIAL_COLOR = 32'h55AA_1234;

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

    function automatic logic [31:0] mem_read(input logic [31:0] addr);
        case (addr)
            // RSTATE descriptor @ 0x00001000
            32'h0000_1000: mem_read = 32'h0000_2000;          // fb_base
            32'h0000_1004: mem_read = 32'd32;                // fb_stride_bytes (8 px * 4B)
            32'h0000_1008: mem_read = 32'd0;                 // fb_format (ARGB8888)
            32'h0000_100C: mem_read = {16'd8, 16'd8};        // {fb_h, fb_w}
            32'h0000_1020: mem_read = 32'h1122_3344;         // const_color_argb (should be overridden)
            32'h0000_1024: mem_read = 32'd0;                 // sampler_handle (unused)
            32'h0000_1028: mem_read = {16'd0, 16'd0};        // scissor {y0,x0}
            32'h0000_102C: mem_read = {16'd0, 16'd0};        // scissor {h,w}
            32'h0000_1030: mem_read = 32'd0;                 // flags0 (scissor+texture disabled)

            // GSTATE descriptor @ 0x00001300
            32'h0000_1300: mem_read = 32'h0000_1600;         // VBO_BASE
            32'h0000_1304: mem_read = 32'h0000_0020;         // VBO_STRIDE_BYTES
            32'h0000_1308: mem_read = 32'h0000_0000;         // IBO_BASE (unused)
            32'h0000_130C: mem_read = 32'h0000_0000;         // IBO_FORMAT (unused)
            32'h0000_1318: mem_read = 32'h0000_0000;         // CULL_CLIP_FLAGS (unused)

            // GPARAM descriptor @ 0x00001400
            32'h0000_1400: mem_read = 32'h0000_0000;         // BASE_VERTEX
            32'h0000_1408: mem_read = MATERIAL_COLOR;        // MATERIAL_COLOR

            // GDRAW command block @ 0x00001500
            32'h0000_1500: mem_read = 32'h0000_0000;         // FIRST
            32'h0000_1504: mem_read = 32'h0000_0003;         // COUNT (one triangle)
            32'h0000_1508: mem_read = 32'h0000_0000;         // TOPOLOGY (tri_list)
            32'h0000_150C: mem_read = 32'h0000_0000;         // FLAGS (non-indexed, not textured)
            32'h0000_1510: mem_read = 32'h0000_0000;         // RSTATE_PTR (unused by minimal path)

            // VBO @ 0x00001600, stride=0x20; fields x@+0x00 y@+0x04 u@+0x10 v@+0x14
            // v0 = (1,1)
            32'h0000_1600: mem_read = 32'd1;
            32'h0000_1604: mem_read = 32'd1;
            32'h0000_1610: mem_read = 32'd0;
            32'h0000_1614: mem_read = 32'd0;

            // v1 = (6,1)
            32'h0000_1620: mem_read = 32'd6;
            32'h0000_1624: mem_read = 32'd1;
            32'h0000_1630: mem_read = 32'd0;
            32'h0000_1634: mem_read = 32'd0;

            // v2 = (1,6)
            32'h0000_1640: mem_read = 32'd1;
            32'h0000_1644: mem_read = 32'd6;
            32'h0000_1650: mem_read = 32'd0;
            32'h0000_1654: mem_read = 32'd0;

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
            end
        end
    end

    int store_count;
    int wrong_color_count;
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            store_count <= 0;
            wrong_color_count <= 0;
        end else if (gfx_st_valid && gfx_st_ready) begin
            store_count <= store_count + 1;
            if (gfx_st_wdata != MATERIAL_COLOR) wrong_color_count <= wrong_color_count + 1;
            $display("%0t ST addr=%08h data=%08h strb=%0h", $time, gfx_st_addr, gfx_st_wdata, gfx_st_wstrb);
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

        // RSTATE(ptr)
        push_gfx(3'b000, 32'h0000_1000);
        // GSTATE(ptr)
        push_gfx(3'b100, 32'h0000_1300);
        // GPARAM(ptr)
        push_gfx(3'b101, 32'h0000_1400);
        // GDRAW(ptr)
        push_gfx(3'b110, 32'h0000_1500);

        repeat (3000) @(posedge clk);

        if (store_count == 0) begin
            $fatal(1, "Expected at least one VRAM store from GDRAW, saw 0");
        end

        if (wrong_color_count != 0) begin
            $fatal(1, "Expected all GDRAW stores to be MATERIAL_COLOR=%08h; wrong_count=%0d", MATERIAL_COLOR, wrong_color_count);
        end

        $display("PASS: saw %0d stores (GDRAW MATERIAL_COLOR)", store_count);
        $finish;
    end
endmodule
