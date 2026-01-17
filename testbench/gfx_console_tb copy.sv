`timescale 1ns/1ps

// Console graphics demo (Verilator)
// - Runs a small ROM program on compute_unit_top
// - Program updates triangle vertices from a precomputed frame table
// - Kicks fixed-function raster (RSTATE/RSETUP/RDRAW) and clears via RRECT
// - TB prints framebuffer as ASCII each frame (ARGB8888)
module gfx_console_tb;
    import isa_pkg::*;

    // ---------------------------------------------------------------------
    // Tunables
    // ---------------------------------------------------------------------
    localparam int W = 48;
    localparam int H = 24;
    localparam int FRAMES = 24;

    // Rotating cube render (12 triangles per frame)
    localparam int TRIS = 12;
    // Use the hardware's expected RSETUP vertex block layout (3 vertices * 32B = 96B)
    // so each triangle can have a stable, unique pointer (no races with the gfx queue).
    localparam int TRI_STRIDE_BYTES = 96;
    localparam int FRAME_STRIDE_BYTES = TRIS * TRI_STRIDE_BYTES;

    localparam bit USE_COLOR_CONSOLE = 1'b1;

    // Console color modes (overridable via +color=N)
    // 0 = mono
    // 1 = truecolor foreground (prints "█")
    // 2 = truecolor background (prints spaces) [default]
    // 3 = ANSI 256-color background (prints spaces)

    // Global memory window (addr[31]=1 selects global in this TB)
    // Use a high, non-wrapping base so FB_OFF can be large.
    localparam logic [31:0] BASE_ADDR = 32'h8000_0000;

    // Layout inside the global window
    // NOTE: Only offsets used by ADDI in the ROM must fit signed 12-bit immediates.
    // Framebuffer is placed above the frame-table region to avoid overlap.
    localparam logic [31:0] FB_OFF          = 32'h0000_9000;
    // IMPORTANT: must not overlap the main framebuffer region.
    // FB size = W*H*4 bytes. With W=48,H=24, FB spans [0x9000 .. 0xA1FF].
    localparam logic [31:0] SYNC_FB_OFF     = 32'h0000_B000; // small sync/doorbell buffer (separate from main FB)
    localparam logic [31:0] RSTATE_OFF      = 32'h0000_0500;
    localparam logic [31:0] RSTATE2_OFF     = 32'h0000_0540; // sync-buffer RSTATE
    localparam logic [31:0] VERT_OFF        = 32'h0000_0600; // legacy (no longer used by the ROM)
    localparam logic [31:0] RRECT_OFF       = 32'h0000_0680;
    localparam logic [31:0] RRECT2_OFF      = 32'h0000_06A0; // sentinel rectangle at (0,0)
    // NOTE: Must not overlap the frame table region (which is large: FRAMES*TRIS*96B).
    // Place it on a 4KB boundary so the ROM can load it with a single LUI.
    localparam logic [31:0] SENT_TABLE_OFF  = 32'h0000_8000; // per-frame sentinel descriptors
    localparam logic [31:0] FRAME_TABLE_OFF = 32'h0000_0700;
    localparam logic [31:0] DONE_OFF        = 32'h0000_01F0;

    // (FRAME_STRIDE_BYTES defined above)

    // ---------------------------------------------------------------------
    // Clock/reset
    // ---------------------------------------------------------------------
    logic clk;
    logic rst_n;

    initial clk = 1'b0;
    always #1 clk = ~clk;

    initial begin
        rst_n = 1'b0;
        repeat (10) @(posedge clk);
        rst_n = 1'b1;
    end

    // ---------------------------------------------------------------------
    // DUT interfaces
    // ---------------------------------------------------------------------
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

    // ---------------------------------------------------------------------
    // Wave dump (optional)
    // ---------------------------------------------------------------------
    initial begin
        $dumpfile("gfx_console_tb.vcd");
        $dumpvars(0, gfx_console_tb);
    end

    // ---------------------------------------------------------------------
    // Mini assembler helpers (match compute_unit_full_tb)
    // ---------------------------------------------------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] u_type(input integer imm, input [4:0] rd, input [6:0] opcode);
        // U-type (LUI/AUIPC): imm[31:12]
        u_type = {imm[31:12], rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] b_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        // B-type: imm[12|10:5|4:1|11] << 1
        b_type = {imm[12], imm[10:5], rs2, rs1, funct3, imm[4:1], imm[11], opcode};
    endfunction

    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_INT_IMM);
    endfunction

    // ---------------------------------------------------------------------
    // Instruction ROM (dual-fetch 64b)
    // ---------------------------------------------------------------------
    localparam int ROM_WORDS = 512;
    logic [31:0] rom [0:ROM_WORDS-1];
    assign inst_rdata = {rom[{inst_addr[9:3], 1'b1}], rom[{inst_addr[9:3], 1'b0}]};

    // ---------------------------------------------------------------------
    // Global memory model
    // ---------------------------------------------------------------------
    localparam int MEM_WORDS = 16384; // 64KB backing store
    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - BASE_ADDR) >> 2;
    endfunction

    // Simple response FIFO
    // Must be deep enough (or apply backpressure) to handle bursts of LSU RMW/descriptor loads.
    localparam int RESP_DEPTH = 256;
    logic        resp_valid   [0:RESP_DEPTH-1];
    logic [4:0]  resp_rd_q    [0:RESP_DEPTH-1];
    logic [31:0] resp_data_q  [0:RESP_DEPTH-1];
    logic [$clog2(RESP_DEPTH)-1:0] resp_wp;
    logic [$clog2(RESP_DEPTH)-1:0] resp_rp;

    // Always-ready (the response FIFO is sized to absorb bursts).
    assign data_req_ready = 1'b1;

    // ---------------------------------------------------------------------
    // Console framebuffer dump
    // ---------------------------------------------------------------------
    function automatic int rgb_to_ansi256(input logic [7:0] r, input logic [7:0] g, input logic [7:0] b);
        int ri;
        int gi;
        int bi;
        int gray;
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

    initial begin
        // Default to the original simple ASCII output.
        // (Color modes remain available via +color=...)
        console_color_mode = 0;
        console_pixel_w    = 1;
        void'($value$plusargs("color=%d", console_color_mode));
        void'($value$plusargs("pixelw=%d", console_pixel_w));
        if (console_pixel_w < 1) console_pixel_w = 1;
    end

    task automatic dump_fb_ascii(input int frame_no);
        int y;
        int x;
        int p;
        int idx;
        logic [31:0] pix;
        logic [7:0] r;
        logic [7:0] g;
        logic [7:0] b;
        int rgb;
        int last_rgb;
        bit last_valid;
        int ansi_idx;
        begin
            $display("\n=== FRAME %0d ===", frame_no);
            for (y = 0; y < H; y++) begin
                $write("%0d ", y);
                last_valid = 1'b0;
                last_rgb   = 0;
                for (x = 0; x < W; x++) begin
                    idx = mem_index(BASE_ADDR + FB_OFF + (y * (W*4)) + (x*4));
                    pix = mem[idx];
                    // Packed color is A,B,G,R (R in [7:0])
                    r = pix[7:0];
                    g = pix[15:8];
                    b = pix[23:16];

                    if (console_color_mode == 0) begin
                        if ({b,g,r} == 24'h000000) $write(".");
                        else $write("#");
                    end else begin
                        if ({b,g,r} == 24'h000000) begin
                            if (last_valid) begin
                                $write("%c[0m", 8'h1b);
                                last_valid = 1'b0;
                            end
                            for (p = 0; p < console_pixel_w; p++) $write(" ");
                        end else begin
                            rgb = (r << 16) | (g << 8) | b;
                            if (!last_valid || (rgb != last_rgb)) begin
                                case (console_color_mode)
                                    1: $write("%c[38;2;%0d;%0d;%0dm", 8'h1b, r, g, b);
                                    2: $write("%c[48;2;%0d;%0d;%0dm", 8'h1b, r, g, b);
                                    default: begin
                                        ansi_idx = rgb_to_ansi256(r, g, b);
                                        $write("%c[48;5;%0dm", 8'h1b, ansi_idx);
                                    end
                                endcase
                                last_valid = 1'b1;
                                last_rgb   = rgb;
                            end

                            if (console_color_mode == 1) begin
                                $write("█");
                            end else begin
                                for (p = 0; p < console_pixel_w; p++) $write(" ");
                            end
                        end
                    end
                end
                if (last_valid) $write("%c[0m", 8'h1b);
                $write("\n");
            end
        end
    endtask

    // ---------------------------------------------------------------------
    // Frame table + descriptors init
    // ---------------------------------------------------------------------
    task automatic init_memory();
        int i;
        int f;
        int t;
        int k;
        real ang_y;
        real ang_x;
        real cy;
        real cx;
        real scale;
        real cosy, siny;
        real cosx, sinx;

        // 8 cube vertices in object space
        real ox [0:7];
        real oy [0:7];
        real oz [0:7];

        // rotated + translated (camera space)
        real rx [0:7];
        real ry [0:7];
        real rz [0:7];

        // projected screen coords
        int sx [0:7];
        int sy [0:7];

        // triangle list (vertex indices)
        int tri_i0 [0:TRIS-1];
        int tri_i1 [0:TRIS-1];
        int tri_i2 [0:TRIS-1];
        int tri_face [0:TRIS-1];

        // sorting helper
        real tri_z [0:TRIS-1];
        int  order [0:TRIS-1];
        int  tmpi;
        real tmpr;

        // face colors (packed as A,B,G,R)
        logic [31:0] face_col [0:5];

        int x0, y0, x1, y1, x2, y2;
        logic [31:0] col;
        logic [31:0] rstate_base;
        logic [31:0] rect_base;
        logic [31:0] rstate2_base;
        logic [31:0] frame_base;
        logic [31:0] sent_base;
        begin
            for (i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;

            rstate_base = BASE_ADDR + RSTATE_OFF;
            rstate2_base = BASE_ADDR + RSTATE2_OFF;
            rect_base   = BASE_ADDR + RRECT_OFF;
            frame_base  = BASE_ADDR + FRAME_TABLE_OFF;
            sent_base   = BASE_ADDR + SENT_TABLE_OFF;

            // RSTATE descriptor (main framebuffer)
            mem[mem_index(rstate_base + 32'h00)] = BASE_ADDR + FB_OFF;              // fb_base
            mem[mem_index(rstate_base + 32'h04)] = W * 4;                           // fb_stride_bytes
            mem[mem_index(rstate_base + 32'h08)] = 32'd0;                           // fb_format (ARGB8888)
            mem[mem_index(rstate_base + 32'h0C)] = {16'(H), 16'(W)};                // {fb_h, fb_w}
            mem[mem_index(rstate_base + 32'h20)] = 32'hFFFF_FFFF;                   // CONST_COLOR (ARGB) -> white (alpha=FF)
            mem[mem_index(rstate_base + 32'h24)] = 32'd0;                           // sampler_handle (unused)
            mem[mem_index(rstate_base + 32'h28)] = {16'd0, 16'd0};                  // scissor xy
            mem[mem_index(rstate_base + 32'h2C)] = {16'd0, 16'd0};                  // scissor wh
            mem[mem_index(rstate_base + 32'h30)] = 32'd0;                           // flags0 (scissor disabled)

            // RSTATE2 descriptor (sync/doorbell buffer)
            // Keep this separate from the main FB so completion signaling can't be overwritten by a late drain
            // of the main clear. Use a small 16x16 tile so the sentinel generates enough distinct stores.
            mem[mem_index(rstate2_base + 32'h00)] = BASE_ADDR + SYNC_FB_OFF;         // fb_base
            mem[mem_index(rstate2_base + 32'h04)] = 16 * 4;                         // fb_stride_bytes
            mem[mem_index(rstate2_base + 32'h08)] = 32'd0;                          // fb_format (ARGB8888)
            mem[mem_index(rstate2_base + 32'h0C)] = {16'd16, 16'd16};               // {fb_h, fb_w}
            mem[mem_index(rstate2_base + 32'h20)] = 32'hFFFF_FFFF;                  // CONST_COLOR (ARGB)
            mem[mem_index(rstate2_base + 32'h24)] = 32'd0;                          // sampler_handle
            mem[mem_index(rstate2_base + 32'h28)] = {16'd0, 16'd0};                 // scissor xy
            mem[mem_index(rstate2_base + 32'h2C)] = {16'd0, 16'd0};                 // scissor wh
            mem[mem_index(rstate2_base + 32'h30)] = 32'd0;                          // flags0

            // RRECT descriptor: full-screen clear to black
            mem[mem_index(rect_base + 32'h00)] = 32'd0;         // x0
            mem[mem_index(rect_base + 32'h04)] = 32'd0;         // y0
            mem[mem_index(rect_base + 32'h08)] = 32'(W);        // x1
            mem[mem_index(rect_base + 32'h0C)] = 32'(H);        // y1
            mem[mem_index(rect_base + 32'h10)] = 32'h0000_0000; // color

            // Per-frame sentinel descriptors targeting the sync buffer.
            // Each entry is 0x20 bytes (32B) so the ROM can compute ptr = base + (frame << 5).
            // Color encodes (frame+1) in low byte so the CPU can poll for an exact expected value.
            for (f = 0; f < FRAMES; f++) begin
                logic [31:0] d;
                d = sent_base + (f << 5);
                mem[mem_index(d + 32'h00)] = 32'd0;                          // x0
                mem[mem_index(d + 32'h04)] = 32'd0;                          // y0
                mem[mem_index(d + 32'h08)] = 32'd16;                         // x1
                mem[mem_index(d + 32'h0C)] = 32'd16;                         // y1
                mem[mem_index(d + 32'h10)] = 32'hFF00_0000 | (f + 1);         // color
            end

            // Face colors: A,B,G,R (R in low byte)
            face_col[0] = 32'hFF_00_00_FF; // red
            face_col[1] = 32'hFF_00_FF_00; // green
            face_col[2] = 32'hFF_FF_00_00; // blue
            face_col[3] = 32'hFF_00_FF_FF; // yellow (R+G)
            face_col[4] = 32'hFF_FF_00_FF; // magenta (R+B)
            face_col[5] = 32'hFF_FF_FF_00; // cyan (G+B)

            // Cube vertices
            ox[0] = -1.0; oy[0] = -1.0; oz[0] = -1.0;
            ox[1] =  1.0; oy[1] = -1.0; oz[1] = -1.0;
            ox[2] =  1.0; oy[2] =  1.0; oz[2] = -1.0;
            ox[3] = -1.0; oy[3] =  1.0; oz[3] = -1.0;
            ox[4] = -1.0; oy[4] = -1.0; oz[4] =  1.0;
            ox[5] =  1.0; oy[5] = -1.0; oz[5] =  1.0;
            ox[6] =  1.0; oy[6] =  1.0; oz[6] =  1.0;
            ox[7] = -1.0; oy[7] =  1.0; oz[7] =  1.0;

            // Triangles (2 per face)
            // face 0: front (z=+1)
            tri_i0[0]=4; tri_i1[0]=5; tri_i2[0]=6; tri_face[0]=0;
            tri_i0[1]=4; tri_i1[1]=6; tri_i2[1]=7; tri_face[1]=0;
            // face 1: back (z=-1)
            tri_i0[2]=0; tri_i1[2]=2; tri_i2[2]=1; tri_face[2]=1;
            tri_i0[3]=0; tri_i1[3]=3; tri_i2[3]=2; tri_face[3]=1;
            // face 2: left (x=-1)
            tri_i0[4]=0; tri_i1[4]=4; tri_i2[4]=7; tri_face[4]=2;
            tri_i0[5]=0; tri_i1[5]=7; tri_i2[5]=3; tri_face[5]=2;
            // face 3: right (x=+1)
            tri_i0[6]=1; tri_i1[6]=2; tri_i2[6]=6; tri_face[6]=3;
            tri_i0[7]=1; tri_i1[7]=6; tri_i2[7]=5; tri_face[7]=3;
            // face 4: top (y=+1)
            tri_i0[8]=3; tri_i1[8]=7; tri_i2[8]=6; tri_face[8]=4;
            tri_i0[9]=3; tri_i1[9]=6; tri_i2[9]=2; tri_face[9]=4;
            // face 5: bottom (y=-1)
            tri_i0[10]=0; tri_i1[10]=1; tri_i2[10]=5; tri_face[10]=5;
            tri_i0[11]=0; tri_i1[11]=5; tri_i2[11]=4; tri_face[11]=5;

            cx = (W - 1) / 2.0;
            cy = (H - 1) / 2.0;
            scale = (H < W ? H : W) * 0.85;

            for (f = 0; f < FRAMES; f++) begin
                ang_y = (6.283185307179586 * f) / FRAMES;
                ang_x = (6.283185307179586 * f) / (FRAMES * 2.0);
                cosy = $cos(ang_y);
                siny = $sin(ang_y);
                cosx = $cos(ang_x);
                sinx = $sin(ang_x);

                // Rotate and project vertices
                for (k = 0; k < 8; k++) begin
                    real x;
                    real y;
                    real z;
                    real x1;
                    real z1;
                    real y2;
                    real z2;
                    real zt;
                    x = ox[k];
                    y = oy[k];
                    z = oz[k];
                    // rotate Y
                    x1 = x*cosy + z*siny;
                    z1 = -x*siny + z*cosy;
                    // rotate X
                    y2 = y*cosx - z1*sinx;
                    z2 = y*sinx + z1*cosx;
                    // translate away from camera
                    zt = z2 + 3.5;
                    rx[k] = x1;
                    ry[k] = y2;
                    rz[k] = zt;
                    // perspective projection
                    sx[k] = $rtoi(cx + (x1 / zt) * scale);
                    sy[k] = $rtoi(cy - (y2 / zt) * scale);
                    if (sx[k] < 0) sx[k] = 0; if (sx[k] >= W) sx[k] = W-1;
                    if (sy[k] < 0) sy[k] = 0; if (sy[k] >= H) sy[k] = H-1;
                end

                // Build per-triangle depth (avg z) and sort far->near
                for (t = 0; t < TRIS; t++) begin
                    tri_z[t] = (rz[tri_i0[t]] + rz[tri_i1[t]] + rz[tri_i2[t]]) / 3.0;
                    order[t] = t;
                end
                for (t = 0; t < TRIS; t++) begin
                    int best;
                    best = t;
                    for (k = t+1; k < TRIS; k++) begin
                        if (tri_z[order[k]] > tri_z[order[best]]) best = k; // larger z = farther
                    end
                    tmpi = order[t];
                    order[t] = order[best];
                    order[best] = tmpi;
                end

                // Store triangle list for this frame
                for (t = 0; t < TRIS; t++) begin
                    int ti;
                    logic [31:0] base;
                    ti = order[t];
                    base = frame_base + f*FRAME_STRIDE_BYTES + t*TRI_STRIDE_BYTES;

                    x0 = sx[tri_i0[ti]]; y0 = sy[tri_i0[ti]];
                    x1 = sx[tri_i1[ti]]; y1 = sy[tri_i1[ti]];
                    x2 = sx[tri_i2[ti]]; y2 = sy[tri_i2[ti]];
                    col = face_col[tri_face[ti]];

                    // Vertex block layout (per docs/command_protocol.md), 32B per vertex:
                    //  +0x00 x, +0x04 y, +0x10 u, +0x14 v, +0x18 color (unused for now)
                    //  Vertex 1 at +0x20, vertex 2 at +0x40.
                    mem[mem_index(base + 32'h00)] = x0;
                    mem[mem_index(base + 32'h04)] = y0;
                    mem[mem_index(base + 32'h10)] = 32'h0;
                    mem[mem_index(base + 32'h14)] = 32'h0;
                    mem[mem_index(base + 32'h18)] = col;

                    mem[mem_index(base + 32'h20)] = x1;
                    mem[mem_index(base + 32'h24)] = y1;
                    mem[mem_index(base + 32'h30)] = 32'h0;
                    mem[mem_index(base + 32'h34)] = 32'h0;

                    mem[mem_index(base + 32'h40)] = x2;
                    mem[mem_index(base + 32'h44)] = y2;
                    mem[mem_index(base + 32'h50)] = 32'h0;
                    mem[mem_index(base + 32'h54)] = 32'h0;
                end
            end
        end
    endtask

    initial begin
        init_memory();
    end

    // ---------------------------------------------------------------------
    // ROM program image
    // ---------------------------------------------------------------------
    initial begin
        int pc;
        int loop_frame_pc;
        int loop_tri_pc;
        int blt_frame_pc;
        int blt_tri_pc;
        int wait_fb0_pc;
        int wait_fb0_done_pc;
        int imm;

        for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
        pc = 0;

        // x1 = BASE_ADDR via LUI
        rom[pc>>2] = u_type(BASE_ADDR, 5'd1, OP_LUI); pc += 4;

        // x17 = main FB base (4KB-aligned, so LUI is sufficient)
        rom[pc>>2] = u_type(BASE_ADDR + FB_OFF, 5'd17, OP_LUI); pc += 4;
        // x15 = sync FB base (used for polling)
        rom[pc>>2] = u_type(BASE_ADDR + SYNC_FB_OFF, 5'd15, OP_LUI); pc += 4;


        // x2 = &RSTATE(main), x20 = &RSTATE(sync), x4 = &RRECT, x5 = &FRAME_TABLE, x18 = SENT_TABLE base
        rom[pc>>2] = i_type(RSTATE_OFF,  5'd1, 3'b000, 5'd2,  OP_INT_IMM); pc += 4;
        rom[pc>>2] = i_type(RSTATE2_OFF, 5'd1, 3'b000, 5'd20, OP_INT_IMM); pc += 4;
        rom[pc>>2] = i_type(RRECT_OFF,  5'd1, 3'b000, 5'd4, OP_INT_IMM); pc += 4;
        rom[pc>>2] = i_type(FRAME_TABLE_OFF, 5'd1, 3'b000, 5'd5, OP_INT_IMM); pc += 4;
        rom[pc>>2] = u_type(BASE_ADDR + SENT_TABLE_OFF,  5'd18, OP_LUI); pc += 4;

        // Set initial RSTATE(main)
        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd2, 3'b000, 5'd0, OP_ATOM_SC); pc += 4; // RSTATE(x2)

        // x6 = frame index
        rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd6, OP_INT_IMM); pc += 4;
        // x9 = FRAMES
        rom[pc>>2] = i_type(FRAMES, 5'd0, 3'b000, 5'd9, OP_INT_IMM); pc += 4;
        // x12 = TRIS
        rom[pc>>2] = i_type(TRIS, 5'd0, 3'b000, 5'd12, OP_INT_IMM); pc += 4;

        loop_frame_pc = pc;

        // Ensure we're rendering to the main framebuffer for this frame.
        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd2, 3'b000, 5'd0, OP_ATOM_SC); pc += 4; // RSTATE(x2)

        // Clear framebuffer
        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd4, 3'b011, 5'd0, OP_ATOM_SC); pc += 4; // RRECT(x4)

        // Compute frame_ptr in x7 = x5 + frame_idx * FRAME_STRIDE_BYTES
        // FRAME_STRIDE_BYTES = 1152 = 1024 + 128
        rom[pc>>2] = i_type(10, 5'd6, 3'b001, 5'd7, OP_INT_IMM); pc += 4; // SLLI x7,x6,10
        rom[pc>>2] = i_type(7,  5'd6, 3'b001, 5'd11, OP_INT_IMM); pc += 4; // SLLI x11,x6,7
        rom[pc>>2] = r_type(7'b0000000, 5'd11, 5'd7, 3'b000, 5'd7, OP_INT); pc += 4; // ADD x7,x7,x11
        rom[pc>>2] = r_type(7'b0000000, 5'd7,  5'd5, 3'b000, 5'd7, OP_INT); pc += 4; // ADD x7,x5,x7

        // tri_idx in x8 = 0
        rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd8, OP_INT_IMM); pc += 4;
        loop_tri_pc = pc;

        // tri_ptr in x3 = frame_ptr + tri_idx * TRI_STRIDE_BYTES
        // TRI_STRIDE_BYTES = 96 = 64 + 32
        rom[pc>>2] = i_type(6, 5'd8, 3'b001, 5'd10, OP_INT_IMM); pc += 4; // SLLI x10,x8,6
        rom[pc>>2] = i_type(5, 5'd8, 3'b001, 5'd11, OP_INT_IMM); pc += 4; // SLLI x11,x8,5
        rom[pc>>2] = r_type(7'b0000000, 5'd11, 5'd10, 3'b000, 5'd10, OP_INT); pc += 4; // ADD x10,x10,x11
        rom[pc>>2] = r_type(7'b0000000, 5'd10, 5'd7,  3'b000, 5'd3,  OP_INT); pc += 4; // ADD x3,x7,x10

        // Kick graphics for this triangle: RSETUP(tri_ptr), RDRAW()
        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd3, 3'b001, 5'd0, OP_ATOM_SC); pc += 4;
        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b010, 5'd0, OP_ATOM_SC); pc += 4;

        // tri_idx++
        rom[pc>>2] = i_type(1, 5'd8, 3'b000, 5'd8, OP_INT_IMM); pc += 4;
        // if (tri_idx < TRIS) goto loop_tri
        blt_tri_pc = pc;
        // Branch target base is PC (see branch_unit: target = pc + imm).
        imm = loop_tri_pc - blt_tri_pc;
        rom[pc>>2] = b_type(imm, 5'd8, 5'd12, 3'b100, OP_BRANCH); pc += 4;
        rom[pc>>2] = nop(); pc += 4;

        // Switch to sync buffer, then draw per-frame sentinel rectangle.
        // This avoids being overwritten by any late-draining main-FB clear stores.
        // x19 = sent_base + (frame_idx << 5)
        rom[pc>>2] = i_type(5, 5'd6, 3'b001, 5'd19, OP_INT_IMM); pc += 4; // SLLI x19,x6,5
        rom[pc>>2] = r_type(7'b0000000, 5'd19, 5'd18, 3'b000, 5'd19, OP_INT); pc += 4; // ADD x19,x18,x19

        // x21 = expected color = 0xFF00_0000 | (frame+1)
        rom[pc>>2] = i_type(1, 5'd6, 3'b000, 5'd21, OP_INT_IMM); pc += 4; // ADDI x21,x6,1
        rom[pc>>2] = u_type(32'hFF00_0000, 5'd22, OP_LUI); pc += 4;       // LUI x22,0xFF000
        rom[pc>>2] = r_type(7'b0000000, 5'd22, 5'd21, 3'b110, 5'd21, OP_INT); pc += 4; // OR x21,x21,x22

        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd20, 3'b000, 5'd0, OP_ATOM_SC); pc += 4; // RSTATE(x20)
        // IMPORTANT: avoid dual-issuing two gfx macro-ops in the same cycle.
        // compute_unit_top only forwards one gfx op per cycle (gp_issue_*), so the second gfx op
        // can be lost if it lands in the rr1 slot alongside another gfx op.
        rom[pc>>2] = nop(); pc += 4;
        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd19, 3'b011, 5'd0, OP_ATOM_SC); pc += 4; // RRECT(x19)

        // Busy-wait until SYNC_FB[0] matches expected (GPU completed sentinel draw).
        // Keep the loop simple to ensure forward progress on this core/ISA.
        //
        // wait_fb0:
        //   LW  x14, 0(x15)
        //   BEQ x14, x21, wait_fb0_done
        //   NOP
        //   goto wait_fb0
        //   NOP
        // wait_fb0_done:
        wait_fb0_pc = pc;
        rom[pc>>2] = i_type(0, 5'd15, 3'b010, 5'd14, OP_LOAD); pc += 4; // LW x14,0(x15)
        wait_fb0_done_pc = pc;
        rom[pc>>2] = b_type(0, 5'd14, 5'd21, 3'b000, OP_BRANCH); pc += 4; // BEQ x14,x21, +0 (patched below)
        rom[pc>>2] = nop(); pc += 4;

        imm = wait_fb0_pc - pc;
        rom[pc>>2] = b_type(imm, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // BEQ x0,x0, wait_fb0
        rom[pc>>2] = nop(); pc += 4;

        // Patch the earlier BEQ x14,x21 to jump here (wait_fb0_done)
        imm = pc - wait_fb0_done_pc;
        rom[wait_fb0_done_pc>>2] = b_type(imm, 5'd14, 5'd21, 3'b000, OP_BRANCH);

        // Signal frame done: SW x6, DONE_OFF(x1)
        // NOTE: LSU uses a write-merge buffer; flush it so the TB reliably sees the store.
        rom[pc>>2] = s_type(DONE_OFF, 5'd1, 5'd6, 3'b010, OP_STORE); pc += 4;
        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b000, 5'd0, OP_SYSTEM); pc += 4; // MEMBAR

        // x6++
        rom[pc>>2] = i_type(1, 5'd6, 3'b000, 5'd6, OP_INT_IMM); pc += 4;

        // if (x6 < x9) goto loop_frame
        blt_frame_pc = pc;
        imm = loop_frame_pc - blt_frame_pc;
        rom[pc>>2] = b_type(imm, 5'd6, 5'd9, 3'b100, OP_BRANCH); pc += 4; // BLT
        rom[pc>>2] = nop(); pc += 4; // delay slot (safe)

        // Done: WFI forever
        rom[pc>>2] = r_type(7'b0000000, 5'd0, 5'd0, 3'b111, 5'd0, OP_SYSTEM); pc += 4;
        // Branch back to WFI (PC-relative)
        rom[pc>>2] = b_type(-4, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // BEQ x0,x0, -4
        rom[pc>>2] = nop(); pc += 4;

        $display("gfx_console_tb: ROM program loaded (%0d bytes)", pc);
        $display("gfx_console_tb: labels loop_frame=%08x loop_tri=%08x blt_tri=%08x wait_fb0=%08x wait_fb0_done_br=%08x blt_frame=%08x",
             loop_frame_pc, loop_tri_pc, blt_tri_pc, wait_fb0_pc, wait_fb0_done_pc, blt_frame_pc);

        // Quick sanity dump of the wait/poll region
        $display("gfx_console_tb: ROM[0x84]=%08x ROM[0x88]=%08x ROM[0x8C]=%08x ROM[0x90]=%08x ROM[0x94]=%08x ROM[0x98]=%08x ROM[0x9C]=%08x ROM[0xA0]=%08x",
             rom[32'h84>>2], rom[32'h88>>2], rom[32'h8C>>2], rom[32'h90>>2],
             rom[32'h94>>2], rom[32'h98>>2], rom[32'h9C>>2], rom[32'hA0>>2]);
    end

    // ---------------------------------------------------------------------
    // Memory request/response + frame trigger
    // ---------------------------------------------------------------------
    int frame_seen;

    // Debug helpers
    int dbg_cycle;
    int dbg_last_fb0_load_cyc;
    int dbg_fb0_load_prints;
    int dbg_sync_store_prints;
    int dbg_rect2_reads;
    int dbg_wb_x8;
    int dbg_wb_x6;

    int dbg_any_reqs;

    // Track scalar reg writes for loop-debugging.
    int dbg_wb_any;
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            dbg_wb_x8 <= 0;
            dbg_wb_x6 <= 0;
        end else begin
            if (dut.s_we && (dut.s_waddr == 5'd8) && (dbg_wb_x8 < 32)) begin
                $display("DBG: WB x8(tri_idx)=%0d inst_addr=%08x", dut.s_wdata, inst_addr);
                dbg_wb_x8 <= dbg_wb_x8 + 1;
            end
            if (dut.s_we && (dut.s_waddr == 5'd6) && (dbg_wb_x6 < 16)) begin
                $display("DBG: WB x6(frame)=%0d inst_addr=%08x", dut.s_wdata, inst_addr);
                dbg_wb_x6 <= dbg_wb_x6 + 1;
            end
            if (dut.s_we && (dut.s_waddr == 5'd16)) begin
                // Delay counter writes (helps debug poll/backoff control-flow)
                $display("DBG: WB x16(dly)=%08x inst_addr=%08x", dut.s_wdata, inst_addr);
            end
        end
    end

    // Trace graphics issue events (confirms macro-ops are actually issued into the gfx queue).
    int dbg_gfx_issue_prints;
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            dbg_gfx_issue_prints <= 0;
        end else begin
            if (dut.gp_issue_valid && (dbg_gfx_issue_prints < 128)) begin
                $display("DBG: gfx_issue funct3=%0d op_a=%08x op_b=%08x pc_if=%08x", 
                         dut.gp_issue_ctrl.funct3,
                         dut.gp_issue_op_a,
                         dut.gp_issue_op_b,
                         inst_addr);
                dbg_gfx_issue_prints <= dbg_gfx_issue_prints + 1;
            end
        end
    end

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            resp_wp <= '0;
            resp_rp <= '0;
            data_resp_valid <= 1'b0;
            dbg_wb_any <= 0;
            data_resp_rd <= '0;
            data_resp_data <= '0;
                if (dbg_wb_any < 200) begin
                    $display("DBG: WB rd=%0d data=%08x inst_addr=%08x", dut.s_waddr, dut.s_wdata, inst_addr);
                    dbg_wb_any <= dbg_wb_any + 1;
                end
            for (int i = 0; i < RESP_DEPTH; i++) begin
                resp_valid[i] <= 1'b0;
                resp_rd_q[i] <= '0;
                resp_data_q[i] <= '0;
            end
            frame_seen <= 0;

            dbg_cycle <= 0;
            dbg_last_fb0_load_cyc <= 0;
            dbg_fb0_load_prints <= 0;
            dbg_sync_store_prints <= 0;
            dbg_rect2_reads <= 0;
            dbg_any_reqs <= 0;
        end else begin
            dbg_cycle <= dbg_cycle + 1;
            data_resp_valid <= 1'b0;

            // accept request
            if (data_req_valid && data_req_ready) begin
                int idx;
                idx = mem_index(data_req_addr);

                // Broad request trace (helps confirm tex-cache refills vs scalar loads/stores)
                if (dbg_any_reqs < 96) begin
                    $display("DBG: REQ v=%0b is_load=%0b addr=%08x wdata=%08x rd=%0d", data_req_valid, data_req_is_load, data_req_addr, data_req_wdata, data_req_rd);
                    dbg_any_reqs <= dbg_any_reqs + 1;
                end

                if (data_req_is_load) begin
                    // Debug: confirm sentinel descriptor fetches and sync polling
                    if (dbg_rect2_reads < 64) begin
                        if ((data_req_addr >= (BASE_ADDR + SENT_TABLE_OFF)) && (data_req_addr < (BASE_ADDR + SENT_TABLE_OFF + (FRAMES << 5)))) begin
                            $display("DBG: load sent desc addr=%08x rd=%0d data=%08x", data_req_addr, data_req_rd, mem[idx]);
                            dbg_rect2_reads <= dbg_rect2_reads + 1;
                        end
                    end

                    if (data_req_addr == (BASE_ADDR + SYNC_FB_OFF)) begin
                        if (dbg_fb0_load_prints < 32) begin
                            $display("DBG: load SYNC0 addr=%08x rd=%0d data=%08x pc=%08x x4(rect)=%08x x18(sent_base)=%08x x19(sent_ptr)=%08x x20(rstate2)=%08x x21(exp)=%08x",
                                     data_req_addr, data_req_rd, mem[idx], inst_addr,
                                     dut.u_regfile_scalar.mem[4],
                                     dut.u_regfile_scalar.mem[18],
                                     dut.u_regfile_scalar.mem[19],
                                     dut.u_regfile_scalar.mem[20],
                                     dut.u_regfile_scalar.mem[21]);
                            dbg_fb0_load_prints <= dbg_fb0_load_prints + 1;
                        end
                    end

                    resp_valid[resp_wp]  <= 1'b1;
                    resp_rd_q[resp_wp]   <= data_req_rd;
                    resp_data_q[resp_wp] <= mem[idx];
                    resp_wp <= resp_wp + 1'b1;
                end else begin
                    mem[idx] <= data_req_wdata;

                    if (data_req_addr == (BASE_ADDR + FB_OFF)) begin
                        $display("DBG: store FB0 addr=%08x wdata=%08x", data_req_addr, data_req_wdata);
                    end

                    if ((data_req_addr >= (BASE_ADDR + SYNC_FB_OFF)) && (data_req_addr < (BASE_ADDR + SYNC_FB_OFF + 32'h400))) begin
                        if (dbg_sync_store_prints < 64) begin
                            $display("DBG: store SYNC addr=%08x wdata=%08x pc=%08x x4(rect)=%08x x18(sent_base)=%08x x19(sent_ptr)=%08x x20(rstate2)=%08x x21(exp)=%08x",
                                     data_req_addr, data_req_wdata, inst_addr,
                                     dut.u_regfile_scalar.mem[4],
                                     dut.u_regfile_scalar.mem[18],
                                     dut.u_regfile_scalar.mem[19],
                                     dut.u_regfile_scalar.mem[20],
                                     dut.u_regfile_scalar.mem[21]);
                            dbg_sync_store_prints <= dbg_sync_store_prints + 1;
                        end
                    end

                    // Frame done doorbell
                    if (data_req_addr == (BASE_ADDR + DONE_OFF)) begin
                        dump_fb_ascii($signed(data_req_wdata));
                        frame_seen <= frame_seen + 1;
                        if ((frame_seen + 1) >= FRAMES) begin
                            $display("gfx_console_tb: finished %0d frames", FRAMES);
                            $finish;
                        end
                    end
                end
            end

            // emit response
            if (resp_wp != resp_rp && resp_valid[resp_rp]) begin
                data_resp_valid <= 1'b1;
                data_resp_rd    <= resp_rd_q[resp_rp];
                data_resp_data  <= resp_data_q[resp_rp];
                resp_valid[resp_rp] <= 1'b0;
                resp_rp <= resp_rp + 1'b1;
            end
        end
    end

    // Safety timeout
    initial begin
        int cyc;
        cyc = 0;
        while (cyc < 50_000_000) begin
            @(posedge clk);
            cyc++;
            if ((cyc % 5_000_000) == 0) begin
                int ridx;
                logic [31:0] i0;
                logic [31:0] i1;
                ridx = (inst_addr >> 2);
                i0 = (ridx >= 0 && ridx < ROM_WORDS) ? rom[ridx] : 32'hDEAD_BEEF;
                i1 = (ridx+1 >= 0 && ridx+1 < ROM_WORDS) ? rom[ridx+1] : 32'hDEAD_BEEF;
                $display("TB heartbeat: cyc=%0d frame_seen=%0d inst_addr=%08x inst0=%08x inst1=%08x gfx_q=%0d gfx_state=%0d cmd_ready=%0d tex_req_valid=%0d raster_busy=%0d rop_busy=%0d wmb_busy=%0d x6(frame)=%0d x8(tri)=%0d x12(TRIS)=%0d x16(dly)=%0d", 
                         cyc, frame_seen, inst_addr, i0, i1, dut.gfx_queue_count,
                         dut.u_graphics_pipeline.gfx_state,
                         dut.u_graphics_pipeline.cmd_ready,
                         dut.u_graphics_pipeline.tex_req_valid,
                         dut.u_graphics_pipeline.raster_busy,
                         dut.u_graphics_pipeline.rop_busy,
                         dut.u_lsu.u_wmb.busy,
                         dut.u_regfile_scalar.mem[6],
                         dut.u_regfile_scalar.mem[8],
                         dut.u_regfile_scalar.mem[12],
                         dut.u_regfile_scalar.mem[16]);
            end
        end
        $fatal(1, "gfx_console_tb: timeout");
    end

endmodule
