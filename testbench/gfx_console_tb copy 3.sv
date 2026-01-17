`timescale 1ns/1ps

// Console graphics demo (compute-unit driven)
// - Host only seeds geometry (cube + low-poly teapot), trig LUT, light vector, and descriptors
// - Compute unit program performs rotation, shading, and raster setup on-device
// - Framebuffer is dumped to PPM (and optional ASCII) after each frame
module gfx_console_tb;
    import isa_pkg::*;

    // ------------------------------------------------------------------
    // Tunables
    // ------------------------------------------------------------------
    localparam int W = 128;
    localparam int H = 128;
    localparam int FRAMES = 60;
    localparam int CUBE_FRAMES = 30;
    localparam int TEA_FRAMES  = 30;
    localparam logic [31:0] FB_BASE_ADDR = BASE_ADDR + FB_OFF;
    localparam logic [31:0] FB_END_ADDR  = (BASE_ADDR + FB_OFF) + (W * H * 4);

    localparam int TRIS_CUBE = 12;

    localparam int TEA_RINGS = 5;
    localparam int TEA_SEGS  = 6;
    localparam int TEA_TRIS  = ((TEA_RINGS - 1) * TEA_SEGS * 2) + (TEA_SEGS * 2); // 60

    localparam int TRI_STRIDE_BYTES = 96;

    localparam logic [31:0] BASE_ADDR = 32'h8000_0000;

    // Layout
    localparam logic [31:0] DONE_OFF          = 32'h0000_01F0;
    localparam logic [31:0] TEA_BASE_SAVE_OFF = 32'h0000_0200;
    localparam logic [31:0] RSTATE_OFF        = 32'h0000_0500;
    localparam logic [31:0] RSTATE_TEX_OFF    = 32'h0000_0540;
    localparam logic [31:0] RRECT_OFF         = 32'h0000_0680;
    localparam logic [31:0] TRI_BUF_OFF       = 32'h0000_0800;
    localparam logic [31:0] TRI_BUF_CUBE_OFF  = TRI_BUF_OFF;
    localparam logic [31:0] TRI_BUF_TEA_OFF   = TRI_BUF_OFF + (TRIS_CUBE * TRI_STRIDE_BYTES);
    localparam logic [31:0] LIGHT_VEC_OFF     = 32'h0000_2100;
    localparam logic [31:0] TRIG_LUT_OFF      = 32'h0000_2400; // 256 entries * 8B
    localparam logic [31:0] CUBE_VERT_OFF     = 32'h0000_2C00;
    localparam logic [31:0] CUBE_TRI_OFF      = 32'h0000_2D00;
    localparam logic [31:0] CUBE_NRM_OFF      = 32'h0000_2E00;
    localparam logic [31:0] TEA_VERT_OFF      = 32'h0000_3400;
    localparam logic [31:0] TEA_TRI_OFF       = 32'h0000_3600;
    localparam logic [31:0] TEA_NRM_OFF       = 32'h0000_3C00;
    localparam logic [31:0] TEX_BASE_OFF      = 32'h0000_4000;
    localparam logic [31:0] SAMP_DESC_OFF     = 32'h0000_4800;
    localparam int TEX_W = 16;
    localparam int TEX_H = 16;
    localparam int TEX_STRIDE_BYTES = TEX_W * 4;
    localparam logic [31:0] FB_OFF            = 32'h0001_0000;

    // ------------------------------------------------------------------
    // Clock/reset
    // ------------------------------------------------------------------
    logic clk;
    logic rst_n;

    initial clk = 1'b0;
    always #1 clk = ~clk;

    initial begin
        rst_n = 1'b0;
        repeat (10) @(posedge clk);
        rst_n = 1'b1;
    end

    // ------------------------------------------------------------------
    // DUT interfaces
    // ------------------------------------------------------------------
    logic [63:0] inst_rdata;
    logic [31:0] inst_addr;

    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;
    logic        data_req_ready;

    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    logic err_fp_overflow;
    logic err_fp_invalid;
    logic err_vec_overflow;
    logic err_vec_invalid;

    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    compute_unit_top dut (
        .clk(clk),
        .rst_n(rst_n),
        .inst_rdata(inst_rdata),
        .inst_addr(inst_addr),
        .data_req_valid(data_req_valid),
        .data_req_is_load(data_req_is_load),
        .data_req_addr(data_req_addr),
        .data_req_wdata(data_req_wdata),
        .data_req_rd(data_req_rd),
        .err_fp_overflow(err_fp_overflow),
        .err_fp_invalid(err_fp_invalid),
        .err_vec_overflow(err_vec_overflow),
        .err_vec_invalid(err_vec_invalid),
        .csr_status(csr_status),
        .csr_fstatus(csr_fstatus),
        .csr_vstatus(csr_vstatus),
        .data_req_ready(data_req_ready),
        .data_resp_valid(data_resp_valid),
        .data_resp_rd(data_resp_rd),
        .data_resp_data(data_resp_data)
    );

    // ------------------------------------------------------------------
    // Wave dump
    // ------------------------------------------------------------------
    initial begin
        $dumpfile("gfx_console_tb.vcd");
        $dumpvars(0, gfx_console_tb);
    end

    // ------------------------------------------------------------------
    // Mini assembler helpers
    // ------------------------------------------------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] u_type(input integer imm, input [4:0] rd, input [6:0] opcode);
        u_type = {imm[31:12], rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] b_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        b_type = {imm[12], imm[10:5], rs2, rs1, funct3, imm[4:1], imm[11], opcode};
    endfunction

    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_INT_IMM);
    endfunction

    // ------------------------------------------------------------------
    // Instruction ROM
    // ------------------------------------------------------------------
    localparam int ROM_WORDS = 4096;
    logic [31:0] rom [0:ROM_WORDS-1];
    assign inst_rdata = {rom[{inst_addr[13:3], 1'b1}], rom[{inst_addr[13:3], 1'b0}]};

    // ------------------------------------------------------------------
    // Memory model
    // ------------------------------------------------------------------
    localparam int MEM_WORDS = 262144; // 1MB
    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - BASE_ADDR) >> 2;
    endfunction

    function automatic logic [31:0] pack_abgr(input int r, input int g, input int b);
        int rr, gg, bb;
        begin
            rr = (r < 0) ? 0 : (r > 255) ? 255 : r;
            gg = (g < 0) ? 0 : (g > 255) ? 255 : g;
            bb = (b < 0) ? 0 : (b > 255) ? 255 : b;
            pack_abgr = {8'hFF, bb[7:0], gg[7:0], rr[7:0]};
        end
    endfunction

    // ------------------------------------------------------------------
    // Teapot profile
    // ------------------------------------------------------------------
    function automatic real tea_profile_radius(input real y);
        real yy;
        begin
            yy = y;
            if (yy < -1.0) yy = -1.0;
            if (yy >  1.0) yy =  1.0;
            if (yy < -0.6)       tea_profile_radius = 0.25 + (yy + 1.0) * (0.55 / 0.4);
            else if (yy < 0.0)   tea_profile_radius = 0.80 + (yy + 0.6) * (0.25 / 0.6);
            else if (yy < 0.6)   tea_profile_radius = 1.05 - (yy)       * (0.25 / 0.6);
            else                 tea_profile_radius = 0.80 - (yy - 0.6) * (0.45 / 0.4);
        end
    endfunction

    // ------------------------------------------------------------------
    // Console options
    // ------------------------------------------------------------------
    function automatic int rgb_to_ansi256(input logic [7:0] r, input logic [7:0] g, input logic [7:0] b);
        int ri, gi, bi, gray;
        begin
            if ((r == g) && (g == b)) begin
                gray = (r * 23 + 127) / 255;
                rgb_to_ansi256 = 232 + gray;
            end else begin
                ri = (r * 5 + 127) / 255;
                gi = (g * 5 + 127) / 255;
                bi = (b * 5 + 127) / 255;
                rgb_to_ansi256 = 16 + (36 * ri) + (6 * gi) + bi;
            end
        end
    endfunction

    int console_color_mode;
    int console_pixel_w;
    bit dump_ascii_frames;
    bit dbg_en;
    bit dbg_color_en;
    bit raster_busy_seen;
    bit rop_busy_seen;

    initial begin
        console_color_mode = 0;
        console_pixel_w    = 1;
        dump_ascii_frames  = 1'b0;
        void'($value$plusargs("color=%d", console_color_mode));
        void'($value$plusargs("pixelw=%d", console_pixel_w));
        void'($value$plusargs("dump_ascii=%d", dump_ascii_frames));
        if (console_pixel_w < 1) console_pixel_w = 1;
    end

    initial begin
        dbg_en = 1'b0;
        void'($value$plusargs("dbg=%d", dbg_en));
    end

    initial begin
        dbg_color_en = 1'b0;
        void'($value$plusargs("dbg_color=%d", dbg_color_en));
    end

    // ------------------------------------------------------------------
    // Framebuffer dumps
    // ------------------------------------------------------------------
    task automatic dump_fb_ppm(input int frame_no);
        int y, x, idx;
        logic [31:0] pix;
        logic [7:0] r, g, b;
        string fname;
        int fh;
        begin
            fname = $sformatf("frame_%0d.ppm", frame_no);
            fh = $fopen(fname, "w");
            if (fh == 0) begin
                $display("TB WARN: could not open %s for write", fname);
                return;
            end
            $fwrite(fh, "P3\n%0d %0d\n255\n", W, H);
            for (y = 0; y < H; y++) begin
                for (x = 0; x < W; x++) begin
                    idx = mem_index(BASE_ADDR + FB_OFF + (y * (W*4)) + (x*4));
                    pix = mem[idx];
                    r = pix[7:0]; g = pix[15:8]; b = pix[23:16];
                    $fwrite(fh, "%0d %0d %0d\n", r, g, b);
                end
            end
            $fclose(fh);
            $display("TB: wrote %s", fname);
        end
    endtask

    task automatic dump_fb_ascii(input int frame_no);
        int y, x, p, idx, rgb, last_rgb;
        logic [31:0] pix; logic [7:0] r, g, b; bit last_valid; int ansi_idx;
        begin
            $display("\n=== FRAME %0d ===", frame_no);
            for (y = 0; y < H; y++) begin
                $write("%0d ", y);
                last_valid = 1'b0; last_rgb = 0;
                for (x = 0; x < W; x++) begin
                    idx = mem_index(BASE_ADDR + FB_OFF + (y * (W*4)) + (x*4));
                    pix = mem[idx]; r = pix[7:0]; g = pix[15:8]; b = pix[23:16];
                    if (console_color_mode == 0) begin
                        if ({b,g,r} == 24'h000000) $write("."); else $write("#");
                    end else begin
                        if ({b,g,r} == 24'h000000) begin
                            if (last_valid) begin $write("%c[0m", 8'h1b); last_valid = 1'b0; end
                            for (p = 0; p < console_pixel_w; p++) $write(" ");
                        end else begin
                            rgb = (r<<16)|(g<<8)|b;
                            if (!last_valid || (rgb != last_rgb)) begin
                                case (console_color_mode)
                                    1: $write("%c[38;2;%0d;%0d;%0dm", 8'h1b, r, g, b);
                                    2: $write("%c[48;2;%0d;%0d;%0dm", 8'h1b, r, g, b);
                                    default: begin
                                        ansi_idx = rgb_to_ansi256(r, g, b);
                                        $write("%c[48;5;%0dm", 8'h1b, ansi_idx);
                                    end
                                endcase
                                last_valid = 1'b1; last_rgb = rgb;
                            end
                            if (console_color_mode == 1) $write("█"); else for (p = 0; p < console_pixel_w; p++) $write(" ");
                        end
                    end
                end
                if (last_valid) $write("%c[0m", 8'h1b);
                $write("\n");
            end
        end
    endtask

    // ------------------------------------------------------------------
    // Memory init (geometry + LUTs + descriptors)
    // ------------------------------------------------------------------
    task automatic init_memory();
        int i;
        real tox [0:((TEA_RINGS*TEA_SEGS)+1)];
        real toy [0:((TEA_RINGS*TEA_SEGS)+1)];
        real toz [0:((TEA_RINGS*TEA_SEGS)+1)];
        begin
            for (i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;

            // RSTATE (main framebuffer - untextured)
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h00)] = BASE_ADDR + FB_OFF;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h04)] = W * 4;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h08)] = 32'd0;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h0C)] = {16'(H), 16'(W)};
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h20)] = 32'hFF_FF_FF_FF;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h24)] = 32'd0;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h30)] = 32'd0;

            // RSTATE (textured cube)
            mem[mem_index(BASE_ADDR + RSTATE_TEX_OFF + 32'h00)] = BASE_ADDR + FB_OFF;
            mem[mem_index(BASE_ADDR + RSTATE_TEX_OFF + 32'h04)] = W * 4;
            mem[mem_index(BASE_ADDR + RSTATE_TEX_OFF + 32'h08)] = 32'd0;
            mem[mem_index(BASE_ADDR + RSTATE_TEX_OFF + 32'h0C)] = {16'(H), 16'(W)};
            mem[mem_index(BASE_ADDR + RSTATE_TEX_OFF + 32'h20)] = 32'hFF_FF_FF_FF;
            mem[mem_index(BASE_ADDR + RSTATE_TEX_OFF + 32'h24)] = BASE_ADDR + SAMP_DESC_OFF;
            mem[mem_index(BASE_ADDR + RSTATE_TEX_OFF + 32'h30)] = 32'h0000_0002; // flags0: texture enable

            // RRECT clear descriptor
            mem[mem_index(BASE_ADDR + RRECT_OFF + 32'h00)] = 32'd0;
            mem[mem_index(BASE_ADDR + RRECT_OFF + 32'h04)] = 32'd0;
            mem[mem_index(BASE_ADDR + RRECT_OFF + 32'h08)] = 32'(W);
            mem[mem_index(BASE_ADDR + RRECT_OFF + 32'h0C)] = 32'(H);
            mem[mem_index(BASE_ADDR + RRECT_OFF + 32'h10)] = 32'hFF_00_00_00;

            // Sampler descriptor (ARGB8888 chess texture)
            mem[mem_index(BASE_ADDR + SAMP_DESC_OFF + 32'h00)] = BASE_ADDR + TEX_BASE_OFF;
            mem[mem_index(BASE_ADDR + SAMP_DESC_OFF + 32'h04)] = TEX_STRIDE_BYTES;
            mem[mem_index(BASE_ADDR + SAMP_DESC_OFF + 32'h08)] = TEX_W;
            mem[mem_index(BASE_ADDR + SAMP_DESC_OFF + 32'h0C)] = TEX_H;
            mem[mem_index(BASE_ADDR + SAMP_DESC_OFF + 32'h10)] = 32'd0; // format ARGB8888

            // Light vector (Q1.15)
            mem[mem_index(BASE_ADDR + LIGHT_VEC_OFF + 0)]  = $signed(32'sd19660); // 0.6
            mem[mem_index(BASE_ADDR + LIGHT_VEC_OFF + 4)]  = $signed(32'sd26214); // 0.8
            mem[mem_index(BASE_ADDR + LIGHT_VEC_OFF + 8)]  = $signed(32'sd29491); // 0.9
            mem[mem_index(BASE_ADDR + LIGHT_VEC_OFF + 12)] = 32'sd0;

            // Trig LUT: 256 entries of {sin_q15, cos_q15}
            for (int ti = 0; ti < 256; ti++) begin
                real ang; int s_q, c_q;
                ang = (6.283185307179586 * ti) / 256.0;
                s_q = $rtoi($sin(ang) * 32767.0);
                c_q = $rtoi($cos(ang) * 32767.0);
                if (s_q < -32768) s_q = -32768; if (s_q > 32767) s_q = 32767;
                if (c_q < -32768) c_q = -32768; if (c_q > 32767) c_q = 32767;
                mem[mem_index(BASE_ADDR + TRIG_LUT_OFF + (ti * 8) + 0)] = $signed(s_q);
                mem[mem_index(BASE_ADDR + TRIG_LUT_OFF + (ti * 8) + 4)] = $signed(c_q);
            end

            // Chessboard texture (black/purple)
            for (int ty = 0; ty < TEX_H; ty++) begin
                for (int tx = 0; tx < TEX_W; tx++) begin
                    int tr, tg, tb;
                    if (((tx >> 2) ^ (ty >> 2)) & 1) begin
                        tr = 160; tg = 0; tb = 200;
                    end else begin
                        tr = 0; tg = 0; tb = 0;
                    end
                    mem[mem_index(BASE_ADDR + TEX_BASE_OFF + (ty * TEX_STRIDE_BYTES) + (tx * 4))] = pack_abgr(tr, tg, tb);
                end
            end

            // Cube geometry (object space * scale)
            begin
                int S;
                int vx [0:7];
                int vy [0:7];
                int vz [0:7];
                int ti;
                int nx_q;
                int ny_q;
                int nz_q;

                S = 40;
                vx[0] = -S; vy[0] = -S; vz[0] = -S;
                vx[1] =  S; vy[1] = -S; vz[1] = -S;
                vx[2] =  S; vy[2] =  S; vz[2] = -S;
                vx[3] = -S; vy[3] =  S; vz[3] = -S;
                vx[4] = -S; vy[4] = -S; vz[4] =  S;
                vx[5] =  S; vy[5] = -S; vz[5] =  S;
                vx[6] =  S; vy[6] =  S; vz[6] =  S;
                vx[7] = -S; vy[7] =  S; vz[7] =  S;
                for (int vi = 0; vi < 8; vi++) begin
                    mem[mem_index(BASE_ADDR + CUBE_VERT_OFF + (vi * 12) + 0)] = vx[vi];
                    mem[mem_index(BASE_ADDR + CUBE_VERT_OFF + (vi * 12) + 4)] = vy[vi];
                    mem[mem_index(BASE_ADDR + CUBE_VERT_OFF + (vi * 12) + 8)] = vz[vi];
                end
                ti = 0;
                // z+ face
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 4; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 5; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 6; ti++;
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 4; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 6; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 7; ti++;
                // z- face
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 2; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 1; ti++;
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 3; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 2; ti++;
                // x- face
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 4; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 7; ti++;
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 7; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 3; ti++;
                // x+ face
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 1; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 2; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 6; ti++;
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 1; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 6; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 5; ti++;
                // y+ face
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 3; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 7; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 6; ti++;
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 3; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 6; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 2; ti++;
                // y- face
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 1; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 5; ti++;
                mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+4)] = 5; mem[mem_index(BASE_ADDR + CUBE_TRI_OFF + (ti*12)+8)] = 4; ti++;
                // Normals (Q1.15) per triangle, match ordering above
                ti = 0;
                // z+
                nx_q=0; ny_q=0; nz_q=32767; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = $signed(nx_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = $signed(nx_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                // z-
                nz_q = -32768; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                // x-
                nx_q = -32768; ny_q = 0; nz_q = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = $signed(nx_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = $signed(nx_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                // x+
                nx_q = 32767; ny_q = 0; nz_q = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = $signed(nx_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = $signed(nx_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                // y+
                nx_q = 0; ny_q = 32767; nz_q = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = $signed(nx_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = $signed(nx_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = $signed(nz_q); ti++;
                // y-
                ny_q = -32768; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = 0; ti++;
                mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+0)] = 0; mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+4)] = $signed(ny_q); mem[mem_index(BASE_ADDR + CUBE_NRM_OFF + (ti*12)+8)] = 0; ti++;
            end

            // Teapot geometry (object space ints)
            begin
                int TEA_S;
                int TEA_VBOT;
                int TEA_VTOP;
                int TEA_VERTS;
                int tri_count;

                TEA_S = 44;
                TEA_VBOT  = (TEA_RINGS * TEA_SEGS);
                TEA_VTOP  = (TEA_RINGS * TEA_SEGS) + 1;
                TEA_VERTS = (TEA_RINGS * TEA_SEGS) + 2;

                for (int ri = 0; ri < TEA_RINGS; ri++) begin
                    real yv; real rv; yv = -1.0 + (2.0 * ri) / (TEA_RINGS - 1); rv = tea_profile_radius(yv);
                    for (int sj = 0; sj < TEA_SEGS; sj++) begin
                        real th; int vidx; th = (6.283185307179586 * sj) / TEA_SEGS; vidx = ri * TEA_SEGS + sj;
                        tox[vidx] = rv * $cos(th); toz[vidx] = rv * $sin(th); toy[vidx] = yv;
                    end
                end
                tox[TEA_VBOT] = 0.0; toy[TEA_VBOT] = -1.0; toz[TEA_VBOT] = 0.0;
                tox[TEA_VTOP] = 0.0; toy[TEA_VTOP] =  1.0; toz[TEA_VTOP] = 0.0;

                for (int vi = 0; vi < TEA_VERTS; vi++) begin
                    int fx, fy, fz;
                    fx = $rtoi(tox[vi] * TEA_S); fy = $rtoi(toy[vi] * TEA_S); fz = $rtoi(toz[vi] * TEA_S);
                    mem[mem_index(BASE_ADDR + TEA_VERT_OFF + (vi * 12) + 0)] = fx;
                    mem[mem_index(BASE_ADDR + TEA_VERT_OFF + (vi * 12) + 4)] = fy;
                    mem[mem_index(BASE_ADDR + TEA_VERT_OFF + (vi * 12) + 8)] = fz;
                end

                tri_count = 0;
                for (int ri = 0; ri < (TEA_RINGS - 1); ri++) begin
                    for (int sj = 0; sj < TEA_SEGS; sj++) begin
                        int sjn; int a, b, c, d;
                        sjn = (sj == (TEA_SEGS - 1)) ? 0 : (sj + 1);
                        a = (ri * TEA_SEGS) + sj;
                        b = ((ri + 1) * TEA_SEGS) + sj;
                        c = ((ri + 1) * TEA_SEGS) + sjn;
                        d = (ri * TEA_SEGS) + sjn;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 0)] = a;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 4)] = b;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 8)] = c; tri_count++;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 0)] = a;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 4)] = c;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 8)] = d; tri_count++;
                    end
                end
                for (int sj = 0; sj < TEA_SEGS; sj++) begin
                    int sjn; int a, d; sjn = (sj == (TEA_SEGS - 1)) ? 0 : (sj + 1);
                    a = (0 * TEA_SEGS) + sj; d = (0 * TEA_SEGS) + sjn;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 0)] = TEA_VBOT;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 4)] = d;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 8)] = a; tri_count++;
                end
                for (int sj = 0; sj < TEA_SEGS; sj++) begin
                    int sjn; int a, d; sjn = (sj == (TEA_SEGS - 1)) ? 0 : (sj + 1);
                    a = ((TEA_RINGS - 1) * TEA_SEGS) + sj; d = ((TEA_RINGS - 1) * TEA_SEGS) + sjn;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 0)] = TEA_VTOP;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 4)] = a;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 8)] = d; tri_count++;
                end

                // Precompute normals (Q1.15)
                for (int ti = 0; ti < TEA_TRIS; ti++) begin
                    int a, b, c;
                    real ax, ay, az, bx, by, bz, cx2, cy2, cz2;
                    real ux, uy, uz, vx, vy, vz; real nx, ny, nz; real nn;
                    int nxq, nyq, nzq;
                    a = mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (ti * 12) + 0)];
                    b = mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (ti * 12) + 4)];
                    c = mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (ti * 12) + 8)];
                    ax = tox[a]; ay = toy[a]; az = toz[a];
                    bx = tox[b]; by = toy[b]; bz = toz[b];
                    cx2 = tox[c]; cy2 = toy[c]; cz2 = toz[c];
                    ux = bx - ax; uy = by - ay; uz = bz - az;
                    vx = cx2 - ax; vy = cy2 - ay; vz = cz2 - az;
                    nx = (uy*vz - uz*vy); ny = (uz*vx - ux*vz); nz = (ux*vy - uy*vx);
                    nn = $sqrt(nx*nx + ny*ny + nz*nz);
                    if (nn > 1.0e-9) begin nx = nx/nn; ny = ny/nn; nz = nz/nn; end
                    nxq = $rtoi(nx * 32767.0); nyq = $rtoi(ny * 32767.0); nzq = $rtoi(nz * 32767.0);
                    if (nxq < -32768) nxq = -32768; if (nxq > 32767) nxq = 32767;
                    if (nyq < -32768) nyq = -32768; if (nyq > 32767) nyq = 32767;
                    if (nzq < -32768) nzq = -32768; if (nzq > 32767) nzq = 32767;
                    mem[mem_index(BASE_ADDR + TEA_NRM_OFF + (ti * 12) + 0)] = $signed(nxq);
                    mem[mem_index(BASE_ADDR + TEA_NRM_OFF + (ti * 12) + 4)] = $signed(nyq);
                    mem[mem_index(BASE_ADDR + TEA_NRM_OFF + (ti * 12) + 8)] = $signed(nzq);
                end
            end
        end
    endtask

    // ------------------------------------------------------------------
    // ROM program (CU does shading + transforms)
    // ------------------------------------------------------------------
    // Assembly sketch (ISA doc-aligned, symbolic):
    //
    // init:
    //   LUI   x1, 0x80000              // BASE
    //   ADDI  x2, x1, RSTATE_OFF        // RSTATE ptr
    //   ADDI  x3, x1, RRECT_OFF         // RRECT ptr
    //   LUI   x4, 0x80001 ; ADDI x4, x4, -0x800  // TRI_BUF cube
    //   LUI   x5, 0x80002 ; ADDI x5, x5, 0x400   // TRIG LUT
    //   LUI   x6, 0x80003 ; ADDI x6, x6, -0x400  // CUBE_VERT
    //   LUI   x7, 0x80003 ; ADDI x7, x7, -0x300  // CUBE_TRI
    //   LUI   x8, 0x80003 ; ADDI x8, x8, -0x200  // CUBE_NRM
    //   LUI   x12,0x80002 ; ADDI x12,x12,0x100   // LIGHT_VEC
    //   LW    x9,  0(x12); LW x10,4(x12); LW x11,8(x12)
    //   LUI   x18,0x80001 ; ADDI x18,x18,-0x380  // TRI_BUF tea
    //   LUI   x26,0x80003 ; ADDI x26,x26,0x400   // TEA_VERT
    //   LUI   x27,0x80003 ; ADDI x27,x27,0x600   // TEA_TRI
    //   LUI   x28,0x80004 ; ADDI x28,x28,-0x400  // TEA_NRM
    //   SW    x26, TEA_BASE_SAVE_OFF(x1)
    //   SW    x27, TEA_BASE_SAVE_OFF+4(x1)
    //   SW    x28, TEA_BASE_SAVE_OFF+8(x1)
    //   // pitch sin/cos (idx 32)
    //   ADDI  x23,x0,32; SLLI x23,x23,3; ADD x23,x5,x23
    //   LW    x24,0(x23); LW x25,4(x23)
    //   // RSTATE
    //   RSTATE x2
    //   ADDI  x13,x0,0                 // frame
    //
    // frame_loop:
    //   ANDI  x14,x13,0xFF; SLLI x14,x14,3; ADD x14,x5,x14
    //   LW    x15,0(x14); LW x16,4(x14)          // yaw sin/cos
    //   RRECT  x3                                  // clear
    //   // cube loop: for tri_idx in [0..TRIS_CUBE)
    //   ADDI  x20,x0,64; ADDI x21,x0,64; ADDI x17,x0,0
    // cube_loop:
    //   // build tri_ptr, tri_desc_ptr, nrm_ptr
    //   // shade -> color
    //   // load indices -> vertices -> transform -> write TRI_BUF
    //   RDRAW  x18; GFLUSH
    //   ADDI  x17,x17,1; BLT x17,TRIS_CUBE,cube_loop
    //
    //   // tea loop: reload TEA bases, for tri_idx in [0..TEA_TRIS)
    //   LUI x4,0x80001; ADDI x4,x4,-0x380
    //   LW  x6,0(x1+TEA_BASE_SAVE_OFF)
    //   LW  x7,4(x1+TEA_BASE_SAVE_OFF)
    //   LW  x8,8(x1+TEA_BASE_SAVE_OFF)
    //   ADDI x20,x0,64; ADDI x21,x0,64; ADDI x17,x0,0
    // tea_loop:
    //   // build tri_ptr, tri_desc_ptr, nrm_ptr
    //   // shade -> color
    //   // load indices -> vertices -> transform -> write TRI_BUF
    //   RDRAW  x18; GFLUSH
    //   ADDI  x17,x17,1; BLT x17,TEA_TRIS,tea_loop
    //
    //   MEMBAR
    //   SW    x13, DONE_OFF(x1)
    //   ADDI  x13,x13,1; BLT x13,FRAMES,frame_loop
    //   WFI; J frame_loop
    //
    // Notes:
    // - RSTATE/RRECT/RDRAW/GFLUSH are gfx macro-ops encoded as OP_ATOM_SC with funct7=0.
    // - B-type offsets are PC-relative and patched below.
    task automatic init_rom();
        int pc;
        int loop_cube_frame_pc;
        int loop_tea_frame_pc;
        int loop_cube_pc;
        int blt_cube_pc;
        int loop_tea_pc;
        int blt_tea_pc;
        int blt_cube_frame_pc;
        int blt_tea_frame_pc;
        int imm;
        begin
            for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
            pc = 0;

            // Base pointers (use LUI+ADDI to keep low bits for non-4KB aligned offsets)
            rom[pc>>2] = u_type(BASE_ADDR, 5'd1, OP_LUI); pc += 4;                  // x1 = BASE (0x8000_0000)
            rom[pc>>2] = i_type(RSTATE_TEX_OFF, 5'd1, 3'b000, 5'd2, OP_INT_IMM); pc += 4; // x2 = RSTATE (textured)
            rom[pc>>2] = i_type(RRECT_OFF, 5'd1, 3'b000, 5'd3, OP_INT_IMM); pc += 4;  // x3 = RRECT ptr

            // TRI_BUF cube = BASE + 0x0800 = (LUI 0x8000_1000) + (-0x800)
            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_1000, 5'd4, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'h800, 5'd4, 3'b000, 5'd4, OP_INT_IMM); pc += 4;

            // TRIG LUT base = BASE + 0x2400 = (LUI 0x8000_2000) + 0x400
            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_2000, 5'd5, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'h400, 5'd5, 3'b000, 5'd5, OP_INT_IMM); pc += 4;

            // Cube verts/tris/normals (0x2C00/0x2D00/0x2E00)
            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_3000, 5'd6, OP_LUI); pc += 4; // base 0x8000_3000
            rom[pc>>2] = i_type(32'hC00, 5'd6, 3'b000, 5'd6, OP_INT_IMM); pc += 4;   // -0x400 -> 0x2C00
            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_3000, 5'd7, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'hD00, 5'd7, 3'b000, 5'd7, OP_INT_IMM); pc += 4;   // -0x300 -> 0x2D00
            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_3000, 5'd8, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'hE00, 5'd8, 3'b000, 5'd8, OP_INT_IMM); pc += 4;   // -0x200 -> 0x2E00

            // Light vector (0x2100)
            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_2000, 5'd12, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'h100, 5'd12, 3'b000, 5'd12, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(0, 5'd12, 3'b010, 5'd9, OP_LOAD); pc += 4;            // x9 = lx
            rom[pc>>2] = i_type(4, 5'd12, 3'b010, 5'd10, OP_LOAD); pc += 4;           // x10 = ly
            rom[pc>>2] = i_type(8, 5'd12, 3'b010, 5'd11, OP_LOAD); pc += 4;           // x11 = lz

            // Teapot base pointers staged in x26/x27/x28, tri buf in x18
            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_1000, 5'd18, OP_LUI); pc += 4; // base 0x8000_1000
            rom[pc>>2] = i_type(32'hC80, 5'd18, 3'b000, 5'd18, OP_INT_IMM); pc += 4; // -0x380 -> 0x0C80

            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_3000, 5'd26, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'h400, 5'd26, 3'b000, 5'd26, OP_INT_IMM); pc += 4; // 0x3400

            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_3000, 5'd27, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'h600, 5'd27, 3'b000, 5'd27, OP_INT_IMM); pc += 4; // 0x3600

            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_4000, 5'd28, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'hC00, 5'd28, 3'b000, 5'd28, OP_INT_IMM); pc += 4; // -0x400 -> 0x3C00

            // Save teapot base pointers (x26/x27/x28) to memory for later reload
            rom[pc>>2] = s_type(TEA_BASE_SAVE_OFF + 0, 5'd1, 5'd26, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(TEA_BASE_SAVE_OFF + 4, 5'd1, 5'd27, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(TEA_BASE_SAVE_OFF + 8, 5'd1, 5'd28, 3'b010, OP_STORE); pc += 4;

            // Pitch sin/cos (lookup index 32 -> 45deg)
            rom[pc>>2] = i_type(32, 5'd0, 3'b000, 5'd23, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(3, 5'd23, 3'b001, 5'd23, OP_INT_IMM); pc += 4; // <<3 (entry size)
            rom[pc>>2] = r_type(7'b0000000, 5'd5, 5'd23, 3'b000, 5'd23, OP_INT); pc += 4; // base + offset
            rom[pc>>2] = i_type(0, 5'd23, 3'b010, 5'd24, OP_LOAD); pc += 4; // sin45
            rom[pc>>2] = i_type(4, 5'd23, 3'b010, 5'd25, OP_LOAD); pc += 4; // cos45

            // RSTATE once
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd2, 3'b000, 5'd0, OP_ATOM_SC); pc += 4;

            // frame = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd13, OP_INT_IMM); pc += 4; // x13 = frame

            // ---------------- CUBE FRAME LOOP ----------------
            loop_cube_frame_pc = pc;

            // angle idx = frame & 0xFF -> sin/cos in x15/x16
            rom[pc>>2] = i_type(255, 5'd13, 3'b111, 5'd14, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(3, 5'd14, 3'b001, 5'd14, OP_INT_IMM); pc += 4; // *8
            rom[pc>>2] = r_type(7'b0000000, 5'd5, 5'd14, 3'b000, 5'd14, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd14, 3'b010, 5'd15, OP_LOAD); pc += 4; // sin
            rom[pc>>2] = i_type(4, 5'd14, 3'b010, 5'd16, OP_LOAD); pc += 4; // cos

            // Clear framebuffer (RRECT descriptor already set)
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd3, 3'b011, 5'd0, OP_ATOM_SC); pc += 4;

            // ---------------- CUBE LOOP ----------------
            rom[pc>>2] = i_type(64, 5'd0, 3'b000, 5'd20, OP_INT_IMM); pc += 4; // cx cube (centered)
            rom[pc>>2] = i_type(64, 5'd0, 3'b000, 5'd21, OP_INT_IMM); pc += 4; // cy cube (centered)
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd17, OP_INT_IMM); pc += 4; // tri_idx
            loop_cube_pc = pc;

            // tri_ptr = TRI_BUF_CUBE + tri_idx*96
            rom[pc>>2] = i_type(6, 5'd17, 3'b001, 5'd18, OP_INT_IMM); pc += 4; // <<6
            rom[pc>>2] = i_type(5, 5'd17, 3'b001, 5'd19, OP_INT_IMM); pc += 4; // <<5
            rom[pc>>2] = r_type(7'b0000000, 5'd19, 5'd18, 3'b000, 5'd18, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd4, 5'd18, 3'b000, 5'd18, OP_INT); pc += 4;

            // tri_desc_ptr = CUBE_TRI + tri_idx*12
            rom[pc>>2] = i_type(3, 5'd17, 3'b001, 5'd19, OP_INT_IMM); pc += 4; // <<3
            rom[pc>>2] = i_type(2, 5'd17, 3'b001, 5'd30, OP_INT_IMM); pc += 4; // <<2
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd19, 3'b000, 5'd19, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd7, 5'd19, 3'b000, 5'd19, OP_INT); pc += 4;

            // nrm_ptr = CUBE_NRM + tri_idx*12
            rom[pc>>2] = i_type(3, 5'd17, 3'b001, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd17, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd8, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd26, OP_LOAD); pc += 4; // nx
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4; // ny
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4; // nz
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Rotate normal yaw
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd28, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd28, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd26, 3'b000, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd31, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;

            // Pitch normals
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd27, 3'b000, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd30, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd28, 5'd31, 3'b000, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd31, 3'b101, 5'd31, OP_INT_IMM); pc += 4;

            // dot = nxr*lx + ny2*ly + nzr*lz
            rom[pc>>2] = r_type(7'b0000001, 5'd9, 5'd29, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd10, 5'd31, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd28, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd11, 5'd30, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd28, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd28, 3'b101, 5'd28, OP_INT_IMM); pc += 4;

            // clamp dot >=0
            rom[pc>>2] = b_type(8, 5'd28, 5'd0, 3'b101, OP_BRANCH); pc += 4; rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd28, OP_INT_IMM); pc += 4;

            // intensity
            rom[pc>>2] = i_type(255, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd26, 5'd28, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(80, 5'd26, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(40, 5'd26, 3'b000, 5'd26, OP_INT_IMM); pc += 4;

            // r,g,b
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd26, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd26, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;

            // color
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(16, 5'd29, 3'b001, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd31, 3'b110, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = i_type(8, 5'd28, 3'b001, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd31, 3'b110, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd31, 3'b110, 5'd31, OP_INT); pc += 4;

            // Load indices
            rom[pc>>2] = i_type(0, 5'd19, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd19, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd19, 3'b010, 5'd29, OP_LOAD); pc += 4;

            // Preserve indices (x22=i0, x23=i1, x12=i2)
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd0, 3'b000, 5'd22, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd0, 3'b000, 5'd23, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd29, 5'd0, 3'b000, 5'd12, OP_INT); pc += 4;

            // Preserve indices (x22=i0, x23=i1, x12=i2)
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd0, 3'b000, 5'd22, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd0, 3'b000, 5'd23, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd29, 5'd0, 3'b000, 5'd12, OP_INT); pc += 4;

            // Vertex 0
            rom[pc>>2] = i_type(3, 5'd22, 3'b001, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd22, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd29, OP_LOAD); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd27, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd27, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd29, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd28, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd29, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd20, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h403, 5'd30, 3'b101, 5'd27, OP_INT_IMM); pc += 4; // u = x >> 3
            rom[pc>>2] = i_type(12'h403, 5'd26, 3'b101, 5'd28, OP_INT_IMM); pc += 4; // v = y >> 3
            rom[pc>>2] = s_type(16, 5'd18, 5'd27, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(20, 5'd18, 5'd28, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = s_type(0, 5'd18, 5'd30, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(4, 5'd18, 5'd26, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(8, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(12,5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(24,5'd18, 5'd31,3'b010, OP_STORE); pc += 4;

            // Vertex 1
            rom[pc>>2] = i_type(3, 5'd23, 3'b001, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd23, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd29, OP_LOAD); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd27, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd27, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd29, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd28, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd29, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd20, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h403, 5'd30, 3'b101, 5'd27, OP_INT_IMM); pc += 4; // u = x >> 3
            rom[pc>>2] = i_type(12'h403, 5'd26, 3'b101, 5'd28, OP_INT_IMM); pc += 4; // v = y >> 3
            rom[pc>>2] = s_type(48, 5'd18, 5'd27, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(52, 5'd18, 5'd28, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = s_type(32, 5'd18, 5'd30, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(36, 5'd18, 5'd26, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(40, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(44, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(56, 5'd18, 5'd31,3'b010, OP_STORE); pc += 4;

            // Vertex 2
            rom[pc>>2] = i_type(3, 5'd12, 3'b001, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd12, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd29, OP_LOAD); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd27, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd27, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd29, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd28, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd29, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd20, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h403, 5'd30, 3'b101, 5'd27, OP_INT_IMM); pc += 4; // u = x >> 3
            rom[pc>>2] = i_type(12'h403, 5'd26, 3'b101, 5'd28, OP_INT_IMM); pc += 4; // v = y >> 3
            rom[pc>>2] = s_type(80, 5'd18, 5'd27, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(84, 5'd18, 5'd28, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = s_type(64, 5'd18, 5'd30, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(68, 5'd18, 5'd26, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(72, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(76, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(88, 5'd18, 5'd31,3'b010, OP_STORE); pc += 4;

            // Draw triangle
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd18, 3'b001, 5'd0, OP_ATOM_SC); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b010, 5'd0, OP_ATOM_SC); pc += 4;

            // tri_idx++
            rom[pc>>2] = i_type(1, 5'd17, 3'b000, 5'd17, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(TRIS_CUBE, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            blt_cube_pc = pc;
            rom[pc>>2] = b_type(0, 5'd17, 5'd26, 3'b100, OP_BRANCH); pc += 4; // patched
            rom[pc>>2] = nop(); pc += 4;

            // Frame done (cube)
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // MEMBAR
            rom[pc>>2] = s_type(DONE_OFF, 5'd1, 5'd13, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(1, 5'd13, 3'b000, 5'd13, OP_INT_IMM); pc += 4; // frame++
            rom[pc>>2] = i_type(CUBE_FRAMES, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            blt_cube_frame_pc = pc;
            rom[pc>>2] = b_type(0, 5'd13, 5'd26, 3'b100, OP_BRANCH); pc += 4; // patched
            rom[pc>>2] = nop(); pc += 4;

            // Switch to untextured RSTATE for teapot frames
            rom[pc>>2] = i_type(RSTATE_OFF, 5'd1, 3'b000, 5'd2, OP_INT_IMM); pc += 4; // x2 = RSTATE (untextured)
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd2, 3'b000, 5'd0, OP_ATOM_SC); pc += 4;

            // ---------------- TEAPOT FRAME LOOP ----------------
            loop_tea_frame_pc = pc;

            // angle idx = frame & 0xFF -> sin/cos in x15/x16
            rom[pc>>2] = i_type(255, 5'd13, 3'b111, 5'd14, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(3, 5'd14, 3'b001, 5'd14, OP_INT_IMM); pc += 4; // *8
            rom[pc>>2] = r_type(7'b0000000, 5'd5, 5'd14, 3'b000, 5'd14, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd14, 3'b010, 5'd15, OP_LOAD); pc += 4; // sin
            rom[pc>>2] = i_type(4, 5'd14, 3'b010, 5'd16, OP_LOAD); pc += 4; // cos

            // Clear framebuffer (RRECT descriptor already set)
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd3, 3'b011, 5'd0, OP_ATOM_SC); pc += 4;

            // ---------------- TEAPOT LOOP ----------------
            // TEA tri buf base = BASE + 0x0C80 = (LUI 0x8000_1000) + (-0x380)
            rom[pc>>2] = u_type(BASE_ADDR + 32'h0000_1000, 5'd4, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(32'hC80, 5'd4, 3'b000, 5'd4, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(TEA_BASE_SAVE_OFF + 0, 5'd1, 3'b010, 5'd6, OP_LOAD); pc += 4; // x6 = TEA verts
            rom[pc>>2] = i_type(TEA_BASE_SAVE_OFF + 4, 5'd1, 3'b010, 5'd7, OP_LOAD); pc += 4; // x7 = TEA tris
            rom[pc>>2] = i_type(TEA_BASE_SAVE_OFF + 8, 5'd1, 3'b010, 5'd8, OP_LOAD); pc += 4; // x8 = TEA nrms
            rom[pc>>2] = i_type(64, 5'd0, 3'b000, 5'd20, OP_INT_IMM); pc += 4; // cx tea (centered)
            rom[pc>>2] = i_type(64, 5'd0, 3'b000, 5'd21, OP_INT_IMM); pc += 4; // cy tea (centered)
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd17, OP_INT_IMM); pc += 4; // tri_idx=0
            loop_tea_pc = pc;

            // tri_ptr = TRI_BUF_TEA + tri_idx*96
            rom[pc>>2] = i_type(6, 5'd17, 3'b001, 5'd18, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(5, 5'd17, 3'b001, 5'd19, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd19, 5'd18, 3'b000, 5'd18, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd4, 5'd18, 3'b000, 5'd18, OP_INT); pc += 4;

            // tri_desc_ptr = TEA_TRI + tri_idx*12
            rom[pc>>2] = i_type(3, 5'd17, 3'b001, 5'd19, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd17, 3'b001, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd19, 3'b000, 5'd19, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd7, 5'd19, 3'b000, 5'd19, OP_INT); pc += 4;

            // nrm_ptr = TEA_NRM + tri_idx*12
            rom[pc>>2] = i_type(3, 5'd17, 3'b001, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd17, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd8, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd26, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // (Reuse same shading + vertices as cube loop)
            // Rotate normal yaw
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd28, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd28, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd26, 3'b000, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd31, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd27, 3'b000, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd30, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd28, 5'd31, 3'b000, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd31, 3'b101, 5'd31, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd9, 5'd29, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd10, 5'd31, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd28, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd11, 5'd30, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd28, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd28, 3'b101, 5'd28, OP_INT_IMM); pc += 4;
            rom[pc>>2] = b_type(8, 5'd28, 5'd0, 3'b101, OP_BRANCH); pc += 4; rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd28, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(255, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd26, 5'd28, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(80, 5'd26, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(40, 5'd26, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(200, 5'd0, 3'b000, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd27, 5'd26, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = i_type(8, 5'd27, 3'b101, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(120, 5'd0, 3'b000, 5'd28, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd28, 5'd26, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = i_type(8, 5'd28, 3'b101, 5'd28, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(60, 5'd0, 3'b000, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd29, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(8, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = u_type(32'hFF00_0000, 5'd31, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(16, 5'd29, 3'b001, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd31, 3'b110, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = i_type(8, 5'd28, 3'b001, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd31, 3'b110, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd31, 3'b110, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd19, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd19, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd19, 3'b010, 5'd29, OP_LOAD); pc += 4;

            // Vertex 0
            rom[pc>>2] = i_type(3, 5'd22, 3'b001, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd22, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd29, OP_LOAD); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd27, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd27, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd29, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd28, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd29, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd20, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = s_type(0, 5'd18, 5'd30, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(4, 5'd18, 5'd26, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(8, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(12,5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(24,5'd18, 5'd31,3'b010, OP_STORE); pc += 4;

            // Vertex 1
            rom[pc>>2] = i_type(3, 5'd23, 3'b001, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd23, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd29, OP_LOAD); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd27, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd27, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd29, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd28, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd29, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd20, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = s_type(32, 5'd18, 5'd30, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(36, 5'd18, 5'd26, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(40, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(44, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(56, 5'd18, 5'd31,3'b010, OP_STORE); pc += 4;

            // Vertex 2
            rom[pc>>2] = i_type(3, 5'd12, 3'b001, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(2, 5'd12, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd29, OP_LOAD); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd27, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd27, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd29, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd28, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd29, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd20, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = s_type(64, 5'd18, 5'd30, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(68, 5'd18, 5'd26, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(72, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(76, 5'd18, 5'd0, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = s_type(88, 5'd18, 5'd31,3'b010, OP_STORE); pc += 4;

            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd18, 3'b001, 5'd0, OP_ATOM_SC); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b010, 5'd0, OP_ATOM_SC); pc += 4;

            rom[pc>>2] = i_type(1, 5'd17, 3'b000, 5'd17, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(TEA_TRIS, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            blt_tea_pc = pc;
            rom[pc>>2] = b_type(0, 5'd17, 5'd26, 3'b100, OP_BRANCH); pc += 4; // patched
            rom[pc>>2] = nop(); pc += 4;

            // Frame done (teapot)
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // MEMBAR
            rom[pc>>2] = s_type(DONE_OFF, 5'd1, 5'd13, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(1, 5'd13, 3'b000, 5'd13, OP_INT_IMM); pc += 4; // frame++
            rom[pc>>2] = i_type(FRAMES, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            blt_tea_frame_pc = pc;
            rom[pc>>2] = b_type(0, 5'd13, 5'd26, 3'b100, OP_BRANCH); pc += 4; // patched
            rom[pc>>2] = nop(); pc += 4;

            // WFI
            rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b111, 5'd0, OP_SYSTEM); pc += 4;
            rom[pc>>2] = b_type(-4, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Patch branches
            imm = loop_cube_pc - blt_cube_pc; rom[blt_cube_pc>>2] = b_type(imm, 5'd17, 5'd26, 3'b100, OP_BRANCH);
            imm = loop_tea_pc - blt_tea_pc;   rom[blt_tea_pc>>2]  = b_type(imm, 5'd17, 5'd26, 3'b100, OP_BRANCH);
            imm = loop_cube_frame_pc - blt_cube_frame_pc; rom[blt_cube_frame_pc>>2] = b_type(imm, 5'd13, 5'd26, 3'b100, OP_BRANCH);
            imm = loop_tea_frame_pc - blt_tea_frame_pc; rom[blt_tea_frame_pc>>2] = b_type(imm, 5'd13, 5'd26, 3'b100, OP_BRANCH);

            $display("gfx_console_tb: ROM program loaded (%0d bytes)", pc);
        end
    endtask

    // ------------------------------------------------------------------
    // Memory request/response + frame trigger
    // ------------------------------------------------------------------
    localparam int RESP_DEPTH = 256;
    logic        resp_valid   [0:RESP_DEPTH-1];
    logic [4:0]  resp_rd_q    [0:RESP_DEPTH-1];
    logic [31:0] resp_data_q  [0:RESP_DEPTH-1];
    logic [$clog2(RESP_DEPTH)-1:0] resp_wp;
    logic [$clog2(RESP_DEPTH)-1:0] resp_rp;

    assign data_req_ready = 1'b1;

    int frame_seen;
    bit pending_dump_valid;
    int pending_dump_frame;
    int fb_write_total;
    int fb_write_nonzero;
    int fb_write_rgb_nonzero;
    int dbg_store_count;
    int dbg_draw_count;
    int dbg_tri_store_count;
    int dbg_tri_load_count;
    int dbg_vtx_load_count;
    int dbg_vtx_after_count;
    int dbg_load_count;
    int dbg_local_load_count;
    int dbg_local_after_count;
    int dbg_resp_count;
    int dbg_wb_count;
    int dbg_lsu_wb_count;
    int dbg_arb_resp_count;
    int dbg_load_after_count;
    bit dbg_after_tri_store;

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            resp_wp <= '0; resp_rp <= '0; data_resp_valid <= 1'b0; data_resp_rd <= '0; data_resp_data <= '0;
            frame_seen <= 0; pending_dump_valid <= 1'b0; pending_dump_frame <= 0;
            fb_write_total <= 0; fb_write_nonzero <= 0; fb_write_rgb_nonzero <= 0;
            dbg_store_count <= 0;
            dbg_draw_count <= 0;
            dbg_tri_store_count <= 0;
            dbg_tri_load_count <= 0;
            dbg_vtx_load_count <= 0;
            dbg_vtx_after_count <= 0;
            dbg_load_count <= 0;
            dbg_local_load_count <= 0;
            dbg_local_after_count <= 0;
            dbg_resp_count <= 0;
            dbg_wb_count <= 0;
            dbg_lsu_wb_count <= 0;
            dbg_arb_resp_count <= 0;
            dbg_load_after_count <= 0;
            dbg_after_tri_store <= 1'b0;
            for (int j = 0; j < RESP_DEPTH; j++) begin resp_valid[j] <= 1'b0; resp_rd_q[j] <= '0; resp_data_q[j] <= '0; end
        end else begin
            data_resp_valid <= 1'b0;
            if (data_req_valid) begin
                int idx; idx = mem_index(data_req_addr);
                if (data_req_is_load) begin
                    if (dbg_color_en && (dbg_load_count < 200)) begin
                        $display("dbg: LOAD addr=%h rd=x%0d data=%h is_vec=%b", data_req_addr, data_req_rd, mem[idx], dut.u_lsu.lsu_internal_req_is_vector);
                        dbg_load_count <= dbg_load_count + 1;
                    end
                    if (dbg_color_en && (dbg_tri_load_count < 12)
                        && (data_req_addr >= (BASE_ADDR + CUBE_TRI_OFF))
                        && (data_req_addr < (BASE_ADDR + CUBE_TRI_OFF + 32'h400))) begin
                        $display("dbg: TRI load addr=%h rd=x%0d data=%h", data_req_addr, data_req_rd, mem[idx]);
                        dbg_tri_load_count <= dbg_tri_load_count + 1;
                    end
                    if (dbg_color_en && (dbg_vtx_load_count < 12)
                        && (data_req_addr >= (BASE_ADDR + CUBE_VERT_OFF))
                        && (data_req_addr < (BASE_ADDR + CUBE_VERT_OFF + 32'h400))) begin
                        $display("dbg: VTX load addr=%h rd=x%0d data=%h", data_req_addr, data_req_rd, mem[idx]);
                        dbg_vtx_load_count <= dbg_vtx_load_count + 1;
                    end
                    if (dbg_color_en && dbg_after_tri_store && (dbg_vtx_after_count < 40)
                        && (data_req_addr >= (BASE_ADDR + CUBE_VERT_OFF))
                        && (data_req_addr < (BASE_ADDR + CUBE_VERT_OFF + 32'h400))) begin
                        $display("dbg: VTX2 t=%0t addr=%h rd=x%0d data=%h", $time, data_req_addr, data_req_rd, mem[idx]);
                        dbg_vtx_after_count <= dbg_vtx_after_count + 1;
                    end
                    if (dbg_color_en && dbg_after_tri_store && (dbg_load_after_count < 40)) begin
                        $display("dbg: LOAD2 t=%0t addr=%h rd=x%0d data=%h", $time, data_req_addr, data_req_rd, mem[idx]);
                        dbg_load_after_count <= dbg_load_after_count + 1;
                    end
                    resp_valid[resp_wp] <= 1'b1; resp_rd_q[resp_wp] <= data_req_rd; resp_data_q[resp_wp] <= mem[idx]; resp_wp <= resp_wp + 1'b1;
                end else begin
                    mem[idx] <= data_req_wdata;
                    if (dbg_color_en && (dbg_tri_store_count < 120)
                        && (data_req_addr >= (BASE_ADDR + TRI_BUF_OFF))
                        && (data_req_addr < (BASE_ADDR + TRI_BUF_OFF + 32'h1000))) begin
                        $display("dbg: TRI store t=%0t addr=%h wdata=%h rf30=%h rf26=%h rf27=%h rf28=%h rf29=%h rf6=%h", $time, data_req_addr, data_req_wdata,
                                 dut.u_regfile_scalar.mem[30], dut.u_regfile_scalar.mem[26], dut.u_regfile_scalar.mem[27], dut.u_regfile_scalar.mem[28], dut.u_regfile_scalar.mem[29],
                                 dut.u_regfile_scalar.mem[6]);
                        dbg_tri_store_count <= dbg_tri_store_count + 1;
                        if (!dbg_after_tri_store) dbg_after_tri_store <= 1'b1;
                    end
                    if ((data_req_addr >= FB_BASE_ADDR) && (data_req_addr < FB_END_ADDR)) begin
                        fb_write_total <= fb_write_total + 1;
                        if (data_req_wdata != 32'h0) fb_write_nonzero <= fb_write_nonzero + 1;
                        if ((data_req_wdata & 32'h00FF_FFFF) != 32'h0) fb_write_rgb_nonzero <= fb_write_rgb_nonzero + 1;
                        if (dbg_color_en && ((data_req_wdata & 32'h00FF_FFFF) != 32'h0)) begin
                            $display("dbg: FB store addr=%h wdata=%h tex_en=%b const=%h", data_req_addr, data_req_wdata,
                                     dut.u_graphics_pipeline.u_rop.quad_tex_enable,
                                     dut.u_graphics_pipeline.u_rop.quad_const_color_argb);
                        end
                    end
                    if (data_req_addr == (BASE_ADDR + DONE_OFF)) begin
                        pending_dump_valid <= 1'b1; pending_dump_frame <= $signed(data_req_wdata);
                    end
                end
            end
            if (resp_valid[resp_rp]) begin
                data_resp_valid <= 1'b1; data_resp_rd <= resp_rd_q[resp_rp]; data_resp_data <= resp_data_q[resp_rp]; resp_valid[resp_rp] <= 1'b0; resp_rp <= resp_rp + 1'b1;
            end
            if (dbg_color_en && dbg_after_tri_store && data_resp_valid && (dbg_resp_count < 40)
                && ((data_resp_rd == 5'd27) || (data_resp_rd == 5'd28) || (data_resp_rd == 5'd29))) begin
                $display("dbg: RESP t=%0t rd=x%0d data=%h rf=%h", $time, data_resp_rd, data_resp_data, dut.u_regfile_scalar.mem[data_resp_rd]);
                dbg_resp_count <= dbg_resp_count + 1;
            end
            if (dbg_color_en && dbg_after_tri_store && dut.u_lsu.arb_resp_valid && (dbg_arb_resp_count < 40)
                && ((dut.u_lsu.arb_resp_rd == 5'd27) || (dut.u_lsu.arb_resp_rd == 5'd28) || (dut.u_lsu.arb_resp_rd == 5'd29))) begin
                $display("dbg: ARB_RESP t=%0t rd=x%0d data=%h is_vec=%b is_rmw=%b", $time, dut.u_lsu.arb_resp_rd, dut.u_lsu.arb_resp_data_scalar,
                         dut.u_lsu.arb_resp_is_vector, dut.u_lsu.arb_resp_is_rmw);
                dbg_arb_resp_count <= dbg_arb_resp_count + 1;
            end
            if (dbg_color_en && dbg_after_tri_store && dut.s_we && (dbg_wb_count < 80)
                && ((dut.s_waddr == 5'd27) || (dut.s_waddr == 5'd28) || (dut.s_waddr == 5'd29))) begin
                $display("dbg: WB t=%0t rd=x%0d data=%h from_lsu=%b from_pending=%b", $time, dut.s_waddr, dut.s_wdata,
                         dut.scalar_wb_from_lsu, dut.scalar_wb_from_pending);
                dbg_wb_count <= dbg_wb_count + 1;
            end
            if (dbg_color_en && dbg_after_tri_store && dut.lsu_wb_valid && (dbg_lsu_wb_count < 40)
                && ((dut.lsu_wb_rd == 5'd27) || (dut.lsu_wb_rd == 5'd28) || (dut.lsu_wb_rd == 5'd29))) begin
                $display("dbg: LSU_WB t=%0t rd=x%0d data=%h is_vec=%b", $time, dut.lsu_wb_rd, dut.lsu_wb_data[31:0], dut.lsu_wb_is_vector);
                dbg_lsu_wb_count <= dbg_lsu_wb_count + 1;
            end
            if (dbg_color_en && dut.local_req_valid && !dut.local_we && (dbg_local_load_count < 20)) begin
                $display("dbg: LOCAL LOAD addr=%h data=%h", dut.local_addr, dut.local_rdata[31:0]);
                dbg_local_load_count <= dbg_local_load_count + 1;
            end
            if (dbg_color_en && dbg_after_tri_store && dut.local_req_valid && !dut.local_we && (dbg_local_after_count < 40)) begin
                $display("dbg: LOCAL2 t=%0t addr=%h data=%h x6=%h x22=%h", $time, dut.local_addr, dut.local_rdata[31:0],
                         dut.u_regfile_scalar.mem[6], dut.u_regfile_scalar.mem[22]);
                dbg_local_after_count <= dbg_local_after_count + 1;
            end
        end
    end

    // Defer dumps until writers idle
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            pending_dump_valid <= 1'b0; pending_dump_frame <= 0; frame_seen <= 0;
            raster_busy_seen <= 1'b0; rop_busy_seen <= 1'b0;
        end else begin
            if (dut.u_graphics_pipeline.raster_busy) raster_busy_seen <= 1'b1;
            if (dut.u_graphics_pipeline.rop_busy)    rop_busy_seen <= 1'b1;
            if (pending_dump_valid) begin
                if ((dut.u_graphics_pipeline.raster_busy == 1'b0) && (dut.u_graphics_pipeline.rop_busy == 1'b0) && (dut.u_lsu.u_wmb.busy == 1'b0)) begin
                    dump_fb_ppm(pending_dump_frame);
                    if (dump_ascii_frames) dump_fb_ascii(pending_dump_frame);
                    $display("TB: frame %0d raster_busy_seen=%0b rop_busy_seen=%0b fb_writes=%0d fb_nonzero=%0d fb_rgb_nonzero=%0d", pending_dump_frame, raster_busy_seen, rop_busy_seen, fb_write_total, fb_write_nonzero, fb_write_rgb_nonzero);
                    frame_seen <= frame_seen + 1; pending_dump_valid <= 1'b0;
                    raster_busy_seen <= 1'b0; rop_busy_seen <= 1'b0;
                    fb_write_total <= 0; fb_write_nonzero <= 0; fb_write_rgb_nonzero <= 0;
                    if (frame_seen + 1 >= FRAMES) begin
                        $display("gfx_console_tb: finished %0d frames", FRAMES);
                        $finish;
                    end
                end
            end
        end
    end

    // Safety timeout
    initial begin
        int cyc; cyc = 0;
        while (cyc < 50_000_000) begin
            @(posedge clk); cyc++;
        end
        $fatal(1, "gfx_console_tb: timeout");
    end

    // Debug: flag if PC stops changing for too long
    initial begin
        if (dbg_en) begin
            int same_pc; int last_pc; same_pc = 0; last_pc = 32'hFFFF_FFFF;
            forever begin
                @(posedge clk);
                if (!rst_n) begin
                    same_pc = 0; last_pc = 32'hFFFF_FFFF;
                end else begin
                    if (dut.mem_pc == last_pc) begin
                        same_pc++;
                        if (same_pc == 2000000) begin
                            $display("dbg: PC stalled at %h if_pc=%h inst0=%h stall_sb0=%b busy29=%b busy30=%b if_valid=%b accept0=%b accept1=%b stall_issue=%b stall_pipe=%b pc_adv=%0d rr_valid=%b ex_valid=%b mem_valid=%b wb_valid=%b rs1=%0d rs2=%0d rd=%0d uses_rs1=%b uses_rs2=%b uses_rd=%b at cyc=%0t",
                                     dut.mem_pc, dut.if_pc, dut.if_inst0, dut.stall_sb0,
                                     dut.u_scoreboard.busy_s[29], dut.u_scoreboard.busy_s[30],
                                     dut.if_valid, dut.accept0, dut.accept1, dut.stall_issue, dut.stall_pipe, dut.pc_advance_bytes,
                                     dut.rr_valid, dut.ex_valid, dut.mem_valid, dut.wb_valid,
                                     dut.d0_ctrl.rs1, dut.d0_ctrl.rs2, dut.d0_ctrl.rd, dut.d0_ctrl.uses_rs1, dut.d0_ctrl.uses_rs2, dut.d0_ctrl.uses_rd, $time);
                        end
                    end else begin
                        same_pc = 0;
                    end
                    last_pc = dut.mem_pc;
                end
            end
        end
    end

    // Debug: trace early pipeline handshakes to see if PC/issue advance
    initial begin
        if (dbg_en) begin
            repeat (40) begin
                @(posedge clk);
                $display("dbg: cyc=%0t if_pc=%h inst_addr=%h if_valid=%b if_inst0=%h accept0=%b accept1=%b stall_issue=%b pc_adv=%0d rr_valid=%b ex_valid=%b mem_valid=%b mem_pc=%h",
                         $time, dut.if_pc, dut.inst_addr, dut.if_valid, dut.if_inst0, dut.accept0, dut.accept1, dut.stall_issue, dut.pc_advance_bytes, dut.rr_valid, dut.ex_valid, dut.mem_valid, dut.mem_pc);
            end
        end
    end

    // Debug: sample pipeline stall causes shortly after reset release
    initial begin
        if (dbg_en) begin
            @(posedge rst_n);
            repeat (20) @(posedge clk);
            $display("dbg: stall_any=%b stall_pipe=%b lsu_stall=%b stall_membar=%b if_valid=%b accept0=%b accept1=%b", dut.stall_any, dut.stall_pipe, dut.lsu_stall, dut.stall_membar, dut.if_valid, dut.accept0, dut.accept1);
        end
    end

    // Debug: watch scalar writeback to x29/x30 to see when scoreboard can clear them
    always @(posedge clk) begin
        if (dbg_en) begin
            if (dut.s_we && ((dut.s_waddr == 5'd29) || (dut.s_waddr == 5'd30))) begin
                $display("dbg: s_we waddr=%0d wdata=%h at cyc=%0t", dut.s_waddr, dut.s_wdata, $time);
            end
        end
    end

    // Debug: trace triangle color fetch + RDRAW const color
    always @(posedge clk) begin
        if (dbg_color_en) begin
            if ((dut.u_graphics_pipeline.gfx_state == dut.u_graphics_pipeline.GFX_FETCH_C0_WAIT)
                && dut.u_graphics_pipeline.gfxd_resp_valid) begin
                $display("dbg: C0 fetch rsetup_ptr=%h c0=%h", dut.u_graphics_pipeline.rsetup_ptr, dut.u_graphics_pipeline.gfxd_resp_data);
            end
            if ((dut.u_graphics_pipeline.gfx_state == dut.u_graphics_pipeline.GFX_RSTATE_FLAGS0_WAIT)
                && dut.u_graphics_pipeline.gfxd_resp_valid) begin
                $display("dbg: RSTATE flags0=%h", dut.u_graphics_pipeline.gfxd_resp_data);
            end
            if (dut.u_graphics_pipeline.gfx_state == dut.u_graphics_pipeline.GFX_RDRAW_ENQ) begin
                $display("dbg: RDRAW enq rsetup_color=%h rop_const_color=%h r_const_color=%h rop_tex_en=%b", dut.u_graphics_pipeline.rsetup_color, dut.u_graphics_pipeline.rop_const_color, dut.u_graphics_pipeline.r_const_color, dut.u_graphics_pipeline.rop_tex_en);
                if (dbg_draw_count < 5) begin
                    $display("dbg: RDRAW verts v0=(%0d,%0d) v1=(%0d,%0d) v2=(%0d,%0d)",
                             $signed(dut.u_graphics_pipeline.v0x), $signed(dut.u_graphics_pipeline.v0y),
                             $signed(dut.u_graphics_pipeline.v1x), $signed(dut.u_graphics_pipeline.v1y),
                             $signed(dut.u_graphics_pipeline.v2x), $signed(dut.u_graphics_pipeline.v2y));
                    dbg_draw_count <= dbg_draw_count + 1;
                end
            end
        end
    end

    // Debug: when scoreboard sets busy for x29/x30 (slot0 only, enough for current hang)
    always @(posedge clk) begin
        if (dbg_en) begin
            if (rst_n && dut.accept0 && dut.d0_ctrl.uses_rd && ((dut.d0_ctrl.rd == 5'd29) || (dut.d0_ctrl.rd == 5'd30))) begin
                $display("dbg: busy set rd=%0d if_pc=%h inst0=%h uses_rs1=%b rs1=%0d uses_rs2=%b rs2=%0d at cyc=%0t",
                         dut.d0_ctrl.rd, dut.if_pc, dut.if_inst0, dut.d0_ctrl.uses_rs1, dut.d0_ctrl.rs1, dut.d0_ctrl.uses_rs2, dut.d0_ctrl.rs2, $time);
            end
        end
    end

    // ------------------------------------------------------------------
    // Init
    // ------------------------------------------------------------------
    initial begin
        init_memory();
        init_rom();
    end

endmodule
