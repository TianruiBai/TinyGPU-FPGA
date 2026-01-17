`timescale 1ns/1ps

// Teapot graphics testbench (compute unit renders)
// - TB initializes teapot mesh + LUTs + descriptors
// - CU program draws sky, checkerboard ground, and rotating teapot
// - Dumps framebuffer to PPM per frame
module gfx_teapot_tb;
    import isa_pkg::*;

    // ---------------------------------------------------------------------
    // Tunables
    // ---------------------------------------------------------------------
    localparam int W = 128;
    localparam int H = 128;
    localparam int FRAMES = 48;

    localparam logic [31:0] BASE_ADDR = 32'h8000_0000;

    localparam int FB_SIZE_BYTES = W * H * 4;

    // Teapot mesh (lathed surface, low poly)
    localparam int TEA_RINGS = 5;
    localparam int TEA_SEGS  = 6;
    localparam int TEA_TRIS  = ((TEA_RINGS - 1) * TEA_SEGS * 2) + (TEA_SEGS * 2);
    localparam int TEA_VBOT  = (TEA_RINGS * TEA_SEGS);
    localparam int TEA_VTOP  = (TEA_RINGS * TEA_SEGS) + 1;
    localparam int TEA_VERTS = (TEA_RINGS * TEA_SEGS) + 2;

    // Layout inside global window
    localparam logic [31:0] DONE_OFF        = 32'h0000_01F0;
    localparam logic [31:0] RSTATE_OFF      = 32'h0000_0500;
    localparam logic [31:0] RRECT_OFF       = 32'h0000_0680;
    localparam logic [31:0] TRI_BUF_OFF     = 32'h0000_0800;
    localparam logic [31:0] TRIG_LUT_OFF    = 32'h0000_2400; // 256*(sin,cos)
    localparam logic [31:0] TEA_VERT_OFF    = 32'h0000_3400; // verts
    localparam logic [31:0] TEA_TRI_OFF     = 32'h0000_3600; // tris
    localparam logic [31:0] TEA_NRM_OFF     = 32'h0000_3C00; // normals
    localparam logic [31:0] LIGHT_VEC_OFF   = 32'h0000_2100; // light vec Q1.15

    localparam logic [31:0] FB_OFF          = 32'h0001_0000;

    localparam int TRI_STRIDE_BYTES = 96;
    localparam int TRI_BUF_SIZE_B   = TEA_TRIS * TRI_STRIDE_BYTES;

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
        $dumpfile("gfx_teapot_tb.vcd");
        $dumpvars(0, gfx_teapot_tb);
    end

    // ---------------------------------------------------------------------
    // Mini assembler helpers
    // ---------------------------------------------------------------------
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

    // ---------------------------------------------------------------------
    // Instruction ROM (dual-fetch 64b)
    // ---------------------------------------------------------------------
    localparam int ROM_WORDS = 8192;
    logic [31:0] rom [0:ROM_WORDS-1];
    assign inst_rdata = {rom[{inst_addr[13:3], 1'b1}], rom[{inst_addr[13:3], 1'b0}]};

    // ---------------------------------------------------------------------
    // Global memory model
    // ---------------------------------------------------------------------
    localparam int MEM_WORDS = 262144; // 1MB backing store
    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - BASE_ADDR) >> 2;
    endfunction

    // Simple response FIFO
    localparam int RESP_DEPTH = 256;
    logic        resp_valid   [0:RESP_DEPTH-1];
    logic [4:0]  resp_rd_q    [0:RESP_DEPTH-1];
    logic [31:0] resp_data_q  [0:RESP_DEPTH-1];
    logic [$clog2(RESP_DEPTH)-1:0] resp_wp;
    logic [$clog2(RESP_DEPTH)-1:0] resp_rp;

    assign data_req_ready = 1'b1;
    
    // Task to Dump Framebuffer to PPM
    task automatic dump_fb_ppm(input int frame_idx);
        int fd;
        string filename;
        int x, y, addr, word_val;
        byte r, g, b;

        $sformat(filename, "frame_%03d.ppm", frame_idx);
        fd = $fopen(filename, "w");
        if (fd) begin
            $display("Writing frame %0d to %s...", frame_idx, filename);
            $fdisplay(fd, "P3");
            $fdisplay(fd, "%0d %0d", W, H);
            $fdisplay(fd, "255");
            for (y = 0; y < H; y++) begin
                for (x = 0; x < W; x++) begin
                    addr = mem_index(BASE_ADDR + FB_OFF + ((y * W + x) * 4));
                    word_val = mem[addr];
                    r = (word_val >> 16) & 8'hFF;
                    g = (word_val >> 8)  & 8'hFF;
                    b = (word_val >> 0)  & 8'hFF;
                    $fwrite(fd, "%0d %0d %0d ", int'(r), int'(g), int'(b));
                end
                $fwrite(fd, "\n");
            end
            $fclose(fd);
        end
    endtask
    
    int frame_seen = 0;

    always @(posedge clk) begin
        if (!rst_n) begin
            data_resp_valid <= 1'b0;
            resp_wp <= '0;
            resp_rp <= '0;
            frame_seen <= 0;
            for (int i = 0; i < RESP_DEPTH; i++) begin
                resp_valid[i] <= 1'b0;
                resp_rd_q[i] <= '0;
                resp_data_q[i] <= '0;
            end
        end else begin
            data_resp_valid <= 1'b0;
            if (data_req_valid) begin
                if (data_req_is_load) begin
                    int idx;
                    idx = mem_index(data_req_addr);
                    resp_valid[resp_wp] <= 1'b1;
                    resp_rd_q[resp_wp] <= data_req_rd;
                    resp_data_q[resp_wp] <= mem[idx];
                    resp_wp <= resp_wp + 1'b1;
                end else begin
                    int idx;
                    idx = mem_index(data_req_addr);
                    mem[idx] <= data_req_wdata;

                    if (data_req_addr == (BASE_ADDR + DONE_OFF)) begin
                         $display("Frame %0d DONE signaled at time %t", frame_seen, $time);
                        if (frame_seen < FRAMES) begin
                            dump_fb_ppm(frame_seen);
                            frame_seen <= frame_seen + 1;
                        end else begin
                            $finish;
                        end
                    end
                end
            end

            if (resp_valid[resp_rp]) begin
                data_resp_valid <= 1'b1;
                data_resp_rd <= resp_rd_q[resp_rp];
                data_resp_data <= resp_data_q[resp_rp];
                resp_valid[resp_rp] <= 1'b0;
                resp_rp <= resp_rp + 1'b1;
            end
        end
    end

    // ---------------------------------------------------------------------
    // Teapot profile
    // ---------------------------------------------------------------------
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

    // ---------------------------------------------------------------------
    // Memory initialization
    // ---------------------------------------------------------------------
    int tri_count;
    task automatic init_memory();
        int i;
        real tox [0:TEA_VERTS-1];
        real toy [0:TEA_VERTS-1];
        real toz [0:TEA_VERTS-1];
        begin
            for (i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;

            // RSTATE descriptor
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h00)] = BASE_ADDR + FB_OFF;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h04)] = W * 4;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h08)] = 32'd0; // ARGB8888
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h0C)] = {16'(H), 16'(W)};
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h20)] = 32'hFF_00_00_00;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h24)] = 32'd0;
            mem[mem_index(BASE_ADDR + RSTATE_OFF + 32'h30)] = 32'd0;

            // Light vector (Q1.15)
            mem[mem_index(BASE_ADDR + LIGHT_VEC_OFF + 0)]  = $signed(32'sd9830);  // 0.3
            mem[mem_index(BASE_ADDR + LIGHT_VEC_OFF + 4)]  = $signed(32'sd19660); // 0.6
            mem[mem_index(BASE_ADDR + LIGHT_VEC_OFF + 8)]  = $signed(32'sd22937); // 0.7

            // Trig LUT
            for (int ti = 0; ti < 256; ti++) begin
                real ang;
                int s_q;
                int c_q;
                ang = (6.283185307179586 * ti) / 256.0;
                s_q = $rtoi($sin(ang) * 32767.0);
                c_q = $rtoi($cos(ang) * 32767.0);
                if (s_q < -32768) s_q = -32768; if (s_q > 32767) s_q = 32767;
                if (c_q < -32768) c_q = -32768; if (c_q > 32767) c_q = 32767;
                mem[mem_index(BASE_ADDR + TRIG_LUT_OFF + (ti * 8) + 0)] = $signed(s_q);
                mem[mem_index(BASE_ADDR + TRIG_LUT_OFF + (ti * 8) + 4)] = $signed(c_q);
            end

            // Generate teapot vertices
            begin
                int TEA_S;
                TEA_S = 44;

                for (int ri = 0; ri < TEA_RINGS; ri++) begin
                    real yv, rv;
                    yv = -1.0 + (2.0 * ri) / (TEA_RINGS - 1);
                    rv = tea_profile_radius(yv);
                    for (int sj = 0; sj < TEA_SEGS; sj++) begin
                        real th;
                        int vidx;
                        th = (6.283185307179586 * sj) / TEA_SEGS;
                        vidx = ri * TEA_SEGS + sj;
                        tox[vidx] = rv * $cos(th);
                        toz[vidx] = rv * $sin(th);
                        toy[vidx] = yv;
                    end
                end
                tox[TEA_VBOT] = 0.0; toy[TEA_VBOT] = -1.0; toz[TEA_VBOT] = 0.0;
                tox[TEA_VTOP] = 0.0; toy[TEA_VTOP] =  1.0; toz[TEA_VTOP] = 0.0;

                for (int vi = 0; vi < TEA_VERTS; vi++) begin
                    int fx, fy, fz;
                    fx = $rtoi(tox[vi] * TEA_S);
                    fy = $rtoi(toy[vi] * TEA_S);
                    fz = $rtoi(toz[vi] * TEA_S);
                    mem[mem_index(BASE_ADDR + TEA_VERT_OFF + (vi * 12) + 0)] = fx;
                    mem[mem_index(BASE_ADDR + TEA_VERT_OFF + (vi * 12) + 4)] = fy;
                    mem[mem_index(BASE_ADDR + TEA_VERT_OFF + (vi * 12) + 8)] = fz;
                end

                // Build teapot triangle indices
                tri_count = 0;
                for (int ri = 0; ri < (TEA_RINGS - 1); ri++) begin
                    for (int sj = 0; sj < TEA_SEGS; sj++) begin
                        int sjn, a, b, c, d;
                        sjn = (sj == (TEA_SEGS - 1)) ? 0 : (sj + 1);
                        a = (ri * TEA_SEGS) + sj;
                        b = ((ri + 1) * TEA_SEGS) + sj;
                        c = ((ri + 1) * TEA_SEGS) + sjn;
                        d = (ri * TEA_SEGS) + sjn;

                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 0)] = a;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 4)] = b;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 8)] = c;
                        tri_count++;

                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 0)] = a;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 4)] = c;
                        mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 8)] = d;
                        tri_count++;
                    end
                end
                for (int sj = 0; sj < TEA_SEGS; sj++) begin
                    int sjn, a, d;
                    sjn = (sj == (TEA_SEGS - 1)) ? 0 : (sj + 1);
                    a = (0 * TEA_SEGS) + sj;
                    d = (0 * TEA_SEGS) + sjn;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 0)] = TEA_VBOT;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 4)] = d;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 8)] = a;
                    tri_count++;
                end
                for (int sj = 0; sj < TEA_SEGS; sj++) begin
                    int sjn, a, d;
                    sjn = (sj == (TEA_SEGS - 1)) ? 0 : (sj + 1);
                    a = ((TEA_RINGS - 1) * TEA_SEGS) + sj;
                    d = ((TEA_RINGS - 1) * TEA_SEGS) + sjn;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 0)] = TEA_VTOP;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 4)] = a;
                    mem[mem_index(BASE_ADDR + TEA_TRI_OFF + (tri_count * 12) + 8)] = d;
                    tri_count++;
                end

                // Precompute normals
                for (int ti = 0; ti < TEA_TRIS; ti++) begin
                    int a, b, c;
                    real ax, ay, az, bx, by, bz, cx2, cy2, cz2;
                    real ux, uy, uz, vx, vy, vz, nx, ny, nz, nn;
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

    // ---------------------------------------------------------------------
    // ROM program
    // ---------------------------------------------------------------------
    task automatic init_rom();
        int pc;
        int loop_frame_pc, loop_tri_pc, loop_tx_pc, loop_ty_pc;
        int blt_tri_pc, blt_tx_pc, blt_ty_pc, blt_frame_pc;
        begin
            for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();
            pc = 0;

            // x1 = BASE_ADDR
            rom[pc>>2] = u_type(BASE_ADDR, 5'd1, OP_LUI); pc += 4;
            // x2 = RSTATE ptr
            rom[pc>>2] = i_type(RSTATE_OFF, 5'd1, 3'b000, 5'd2, OP_INT_IMM); pc += 4;
            // x3 = RRECT ptr
            rom[pc>>2] = i_type(RRECT_OFF, 5'd1, 3'b000, 5'd3, OP_INT_IMM); pc += 4;
            // x4 = TRI_BUF base
            rom[pc>>2] = u_type(BASE_ADDR + TRI_BUF_OFF, 5'd4, OP_LUI); pc += 4;
            // x5 = trig LUT base
            rom[pc>>2] = u_type(BASE_ADDR + TRIG_LUT_OFF, 5'd5, OP_LUI); pc += 4;
            // x6 = TEA_VERT base
            rom[pc>>2] = u_type(BASE_ADDR + TEA_VERT_OFF, 5'd6, OP_LUI); pc += 4;
            // x7 = TEA_TRI base
            rom[pc>>2] = u_type(BASE_ADDR + TEA_TRI_OFF, 5'd7, OP_LUI); pc += 4;
            // x8 = TEA_NRM base
            rom[pc>>2] = u_type(BASE_ADDR + TEA_NRM_OFF, 5'd8, OP_LUI); pc += 4;
            // x9..x11 light vector
            rom[pc>>2] = u_type(BASE_ADDR + LIGHT_VEC_OFF, 5'd12, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(0, 5'd12, 3'b010, 5'd9, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(4, 5'd12, 3'b010, 5'd10, OP_LOAD); pc += 4;
            rom[pc>>2] = i_type(8, 5'd12, 3'b010, 5'd11, OP_LOAD); pc += 4;

            // Constants: cx=W/2, cy=60, scale=1
            rom[pc>>2] = i_type((W/2), 5'd0, 3'b000, 5'd20, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(60,    5'd0, 3'b000, 5'd21, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(1,     5'd0, 3'b000, 5'd22, OP_INT_IMM); pc += 4;

            // Pitch angle (fixed ~45 deg)
            rom[pc>>2] = i_type(32, 5'd0, 3'b000, 5'd23, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(3, 5'd23, 3'b001, 5'd23, OP_INT_IMM); pc += 4; // idx<<3
            rom[pc>>2] = r_type(7'b0000000, 5'd5, 5'd23, 3'b000, 5'd23, OP_INT); pc += 4; // base+offset
            rom[pc>>2] = i_type(0, 5'd23, 3'b010, 5'd24, OP_LOAD); pc += 4; // sin45
            rom[pc>>2] = i_type(4, 5'd23, 3'b010, 5'd25, OP_LOAD); pc += 4; // cos45

            // Issue RSTATE
            rom[pc>>2] = i_type(0, 5'd2, 3'b000, 5'd0, OP_ATOM_SC); pc += 4; // RSTATE (rs1=x2)

            // frame = 0
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd13, OP_INT_IMM); pc += 4;

            // -------------------------
            // Frame Loop
            // -------------------------
            loop_frame_pc = pc;

            // Update Animation Angle (frame & 0xFF)
            rom[pc>>2] = i_type(255, 5'd13, 3'b111, 5'd14, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(3, 5'd14, 3'b001, 5'd14, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd5, 5'd14, 3'b000, 5'd14, OP_INT); pc += 4;
            rom[pc>>2] = i_type(0, 5'd14, 3'b010, 5'd15, OP_LOAD); pc += 4; // sin(yaw)
            rom[pc>>2] = i_type(4, 5'd14, 3'b010, 5'd16, OP_LOAD); pc += 4; // cos(yaw)

            // Clear Screen (Sky)
            rom[pc>>2] = u_type(32'hFF_90_70_40, 5'd26, OP_LUI); pc += 4; // Color
            rom[pc>>2] = s_type(16, 5'd3, 5'd26, 3'b010, OP_STORE); pc += 4; // Write to RRECT desc
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = s_type(0, 5'd3, 5'd26, 3'b010, OP_STORE); pc += 4; // x0=0
            rom[pc>>2] = s_type(4, 5'd3, 5'd26, 3'b010, OP_STORE); pc += 4; // y0=0
            rom[pc>>2] = i_type(W, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = s_type(8, 5'd3, 5'd26, 3'b010, OP_STORE); pc += 4; // x1=W
            rom[pc>>2] = i_type(H, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            rom[pc>>2] = s_type(12, 5'd3, 5'd26, 3'b010, OP_STORE); pc += 4; // y1=H
            rom[pc>>2] = i_type(0, 5'd3, 3'b011, 5'd0, OP_ATOM_SC); pc += 4; // RRECT

            // Draw Checkerboard
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd27, OP_INT_IMM); pc += 4; // ty
            loop_ty_pc = pc;
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd28, OP_INT_IMM); pc += 4; // tx
            loop_tx_pc = pc;

            // Setup RRECT for tile
            rom[pc>>2] = i_type(4, 5'd28, 3'b001, 5'd29, OP_INT_IMM); pc += 4; // tx<<4
            rom[pc>>2] = s_type(0, 5'd3, 5'd29, 3'b010, OP_STORE); pc += 4; // x0
            rom[pc>>2] = i_type(4, 5'd27, 3'b001, 5'd30, OP_INT_IMM); pc += 4; // ty<<4
            rom[pc>>2] = i_type(64, 5'd30, 3'b000, 5'd30, OP_INT_IMM); pc += 4; // +64
            rom[pc>>2] = s_type(4, 5'd3, 5'd30, 3'b010, OP_STORE); pc += 4; // y0
            rom[pc>>2] = i_type(16, 5'd29, 3'b000, 5'd31, OP_INT_IMM); pc += 4; 
            rom[pc>>2] = s_type(8, 5'd3, 5'd31, 3'b010, OP_STORE); pc += 4; // x1
            rom[pc>>2] = i_type(16, 5'd30, 3'b000, 5'd31, OP_INT_IMM); pc += 4;
            rom[pc>>2] = s_type(12, 5'd3, 5'd31, 3'b010, OP_STORE); pc += 4; // y1
            
            // Tile Color selection
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd28, 3'b000, 5'd31, OP_INT); pc += 4; // tx+ty
            rom[pc>>2] = i_type(1, 5'd31, 3'b111, 5'd31, OP_INT_IMM); pc += 4; // &1
            rom[pc>>2] = b_type(8, 5'd31, 5'd0, 3'b001, OP_BRANCH); pc += 4; // BNE (skip dark)
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = u_type(32'hFF_30_30_30, 5'd31, OP_LUI); pc += 4; // Dark
            rom[pc>>2] = b_type(8, 5'd0, 5'd0, 3'b000, OP_BRANCH); pc += 4; // Jump merge
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = u_type(32'hFF_E0_E0_E0, 5'd31, OP_LUI); pc += 4; // Light
            rom[pc>>2] = s_type(16, 5'd3, 5'd31, 3'b010, OP_STORE); pc += 4;
            rom[pc>>2] = i_type(0, 5'd3, 3'b011, 5'd0, OP_ATOM_SC); pc += 4; // RRECT

            // Loop Controls (Checkerboard)
            rom[pc>>2] = i_type(1, 5'd28, 3'b000, 5'd28, OP_INT_IMM); pc += 4; // tx++
            rom[pc>>2] = i_type(8, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4; // limit
            blt_tx_pc = pc;
            rom[pc>>2] = b_type(loop_tx_pc - pc, 5'd28, 5'd26, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            rom[pc>>2] = i_type(1, 5'd27, 3'b000, 5'd27, OP_INT_IMM); pc += 4; // ty++
            rom[pc>>2] = i_type(4, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4; // limit
            blt_ty_pc = pc;
            rom[pc>>2] = b_type(loop_ty_pc - pc, 5'd27, 5'd26, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // -------------------------
            // Teapot Render Loop
            // -------------------------
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd17, OP_INT_IMM); pc += 4; // tri_idx = 0
            loop_tri_pc = pc;

            // --- Load Normals & Rotate ---
            rom[pc>>2] = i_type(3, 5'd17, 3'b001, 5'd26, OP_INT_IMM); pc += 4; // *8
            rom[pc>>2] = i_type(2, 5'd17, 3'b001, 5'd27, OP_INT_IMM); pc += 4; // *4 -> *12
            rom[pc>>2] = r_type(7'b0000000, 5'd27, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd8, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4; // Addr
            rom[pc>>2] = i_type(0, 5'd26, 3'b010, 5'd26, OP_LOAD); pc += 4; // nx
            rom[pc>>2] = i_type(4, 5'd26, 3'b010, 5'd27, OP_LOAD); pc += 4; // ny
            rom[pc>>2] = i_type(8, 5'd26, 3'b010, 5'd28, OP_LOAD); pc += 4; // nz

            // Rotate Yaw: nxr=(nx*c + nz*s)>>15, nzr=(nz*c - nx*s)>>15
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd26, 3'b000, 5'd29, OP_INT); pc += 4; // nx*c
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd28, 3'b000, 5'd30, OP_INT); pc += 4; // nz*s
            rom[pc>>2] = r_type(7'b0000000, 5'd30, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4; // +
            rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4; // >>15 (nxr)
            
            rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd28, 3'b000, 5'd30, OP_INT); pc += 4; // nz*c
            rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd26, 3'b000, 5'd31, OP_INT); pc += 4; // nx*s
            rom[pc>>2] = r_type(7'b0100000, 5'd31, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4; // -
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4; // >>15 (nzr)

            // Pitch Normals: ny2=(ny*c45 - nzr*s45)>>15, nz2=(ny*s45 + nzr*c45)>>15
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd27, 3'b000, 5'd31, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd30, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0100000, 5'd28, 5'd31, 3'b000, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd27, 3'b101, 5'd27, OP_INT_IMM); pc += 4; // ny2

            rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd27, 3'b000, 5'd31, OP_INT); pc += 4; 
            rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd30, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd31, 3'b000, 5'd30, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4; // nz2
            
            // Lighting Dot Product: (nxr*lx + ny2*ly + nz2*lz) >> 15
            rom[pc>>2] = r_type(7'b0000001, 5'd9, 5'd29, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd10, 5'd27, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000001, 5'd11, 5'd30, 3'b000, 5'd28, OP_INT); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd26, 3'b000, 5'd26, OP_INT); pc += 4;
            rom[pc>>2] = i_type(12'h40F, 5'd26, 3'b101, 5'd26, OP_INT_IMM); pc += 4; // dot

            // Clamp Color & Pack ARGB
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000010, 5'd27, 5'd26, 3'b000, 5'd0, OP_INT); pc += 4; // CMP dot, 0
            rom[pc>>2] = b_type(8, 5'd0, 5'd0, 3'b101, OP_BRANCH); pc += 4; // BGE 0
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4; // dot=0
            rom[pc>>2] = i_type(255, 5'd0, 3'b000, 5'd27, OP_INT_IMM); pc += 4; 
            rom[pc>>2] = r_type(7'b0000010, 5'd26, 5'd27, 3'b000, 5'd0, OP_INT); pc += 4; // CMP dot, 255
            rom[pc>>2] = b_type(8, 5'd0, 5'd0, 3'b100, OP_BRANCH); pc += 4; // BLT 255
            rom[pc>>2] = nop(); pc += 4;
            rom[pc>>2] = i_type(255, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4; // dot=255
            
            // Pack: 0xFF000000 | (dot<<16) | (dot<<8) | dot
            rom[pc>>2] = i_type(8, 5'd26, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd27, 3'b110, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = i_type(8, 5'd27, 3'b001, 5'd27, OP_INT_IMM); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd27, 3'b110, 5'd27, OP_INT); pc += 4;
            rom[pc>>2] = u_type(32'hFF000000, 5'd28, OP_LUI); pc += 4;
            rom[pc>>2] = r_type(7'b0000000, 5'd28, 5'd27, 3'b110, 5'd27, OP_INT); pc += 4; // x27 = Color
            
            // --- Process 3 Vertices ---
            // Unrolling 3 iterations for v=0,1,2
            for (int v = 0; v < 3; v++) begin
                // Load vertex index from TRI_BUF
                // Offset = tri_idx*12 + v*4
                rom[pc>>2] = i_type(12, 5'd0, 3'b000, 5'd28, OP_INT_IMM); pc += 4;
                rom[pc>>2] = r_type(7'b0000001, 5'd17, 5'd28, 3'b000, 5'd28, OP_INT); pc += 4; // idx*12
                rom[pc>>2] = i_type(v*4, 5'd28, 3'b000, 5'd28, OP_INT_IMM); pc += 4;
                rom[pc>>2] = r_type(7'b0000000, 5'd7, 5'd28, 3'b000, 5'd28, OP_INT); pc += 4; // Addr
                rom[pc>>2] = i_type(0, 5'd28, 3'b010, 5'd28, OP_LOAD); pc += 4; // v_idx
                
                // Load Vertex Data
                // Offset = v_idx*12
                rom[pc>>2] = i_type(12, 5'd0, 3'b000, 5'd29, OP_INT_IMM); pc += 4;
                rom[pc>>2] = r_type(7'b0000001, 5'd28, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4;
                rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4; // Addr
                rom[pc>>2] = i_type(0, 5'd29, 3'b010, 5'd28, OP_LOAD); pc += 4; // vx
                rom[pc>>2] = i_type(4, 5'd29, 3'b010, 5'd29, OP_LOAD); pc += 4; // vy (x29 reused addr? No! load overwrites)
                // Wait, need to preserve base addr if reuse. Here I used 29 as dest.
                // Re-calculate addr or use offset. But offset is max 2048. 4 is fine.
                // But x29 is overwritten by first load!
                // Fix:
                rom[pc>>2] = i_type(12, 5'd0, 3'b000, 5'd29, OP_INT_IMM); pc += 4;
                rom[pc>>2] = r_type(7'b0000001, 5'd28, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4;
                rom[pc>>2] = r_type(7'b0000000, 5'd6, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4; // x29 = Addr
                rom[pc>>2] = i_type(0, 5'd29, 3'b010, 5'd28, OP_LOAD); pc += 4; // x28 = vx
                rom[pc>>2] = i_type(8, 5'd29, 3'b010, 5'd31, OP_LOAD); pc += 4; // x31 = vz
                rom[pc>>2] = i_type(4, 5'd29, 3'b010, 5'd29, OP_LOAD); pc += 4; // x29 = vy
                
                // Rotate Vertex (Yaw) -> x28(vx), x29(vy), x31(vz)
                rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd28, 3'b000, 5'd30, OP_INT); pc += 4; // vx*c
                rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd31, 3'b000, 5'd26, OP_INT); pc += 4; // vz*s
                rom[pc>>2] = r_type(7'b0000000, 5'd26, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4; // +
                rom[pc>>2] = i_type(12'h40F, 5'd30, 3'b101, 5'd30, OP_INT_IMM); pc += 4; // vxr
                
                rom[pc>>2] = r_type(7'b0000001, 5'd16, 5'd31, 3'b000, 5'd26, OP_INT); pc += 4; // vz*c
                rom[pc>>2] = r_type(7'b0000001, 5'd15, 5'd28, 3'b000, 5'd31, OP_INT); pc += 4; // vx*s
                rom[pc>>2] = r_type(7'b0100000, 5'd31, 5'd26, 3'b000, 5'd31, OP_INT); pc += 4; // -
                rom[pc>>2] = i_type(12'h40F, 5'd31, 3'b101, 5'd31, OP_INT_IMM); pc += 4; // vzr
                
                // Pitch Vertex: vy2=(vy*c45 - vzr*s45), vz2=...
                rom[pc>>2] = r_type(7'b0000001, 5'd25, 5'd29, 3'b000, 5'd28, OP_INT); pc += 4; 
                rom[pc>>2] = r_type(7'b0000001, 5'd24, 5'd31, 3'b000, 5'd26, OP_INT); pc += 4;
                rom[pc>>2] = r_type(7'b0100000, 5'd26, 5'd28, 3'b000, 5'd29, OP_INT); pc += 4;
                rom[pc>>2] = i_type(12'h40F, 5'd29, 3'b101, 5'd29, OP_INT_IMM); pc += 4; // vy2
                
                // Project (Scale + Center)
                rom[pc>>2] = r_type(7'b0000000, 5'd20, 5'd30, 3'b000, 5'd30, OP_INT); pc += 4; // sx = vxr + cx
                rom[pc>>2] = r_type(7'b0000000, 5'd21, 5'd29, 3'b000, 5'd29, OP_INT); pc += 4; // sy = vy2 + cy (invert Y done in model?)
                // Actually model Y up is positive. Screen Y down is positive. 
                // Let's do sy = cy - vy2
                rom[pc>>2] = r_type(7'b0100000, 5'd29, 5'd21, 3'b000, 5'd29, OP_INT); pc += 4;
                
                // Write to TRI_BUF (Stride 32 bytes per vert)
                // x4 is base. offset = v*32
                rom[pc>>2] = s_type(v*32 + 0, 5'd4, 5'd30, 3'b010, OP_STORE); pc += 4; // X
                rom[pc>>2] = s_type(v*32 + 4, 5'd4, 5'd29, 3'b010, OP_STORE); pc += 4; // Y
                rom[pc>>2] = i_type(0, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4; 
                rom[pc>>2] = s_type(v*32 + 8, 5'd4, 5'd26, 3'b010, OP_STORE); pc += 4; // Z=0
                rom[pc>>2] = i_type(1, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
                rom[pc>>2] = s_type(v*32 + 12, 5'd4, 5'd26, 3'b010, OP_STORE); pc += 4; // W=1
                rom[pc>>2] = s_type(v*32 + 24, 5'd4, 5'd27, 3'b010, OP_STORE); pc += 4; // Color
            end

            // Issue RSETUP (rs1=x4)
            rom[pc>>2] = i_type(0, 5'd4, 3'b001, 5'd0, OP_ATOM_SC); pc += 4;

            // Issue RDRAW (rs1=0? No, just kick)
            // RDRAW rs1 is GDRAW pointer for geometry, but here we used RSETUP.
            // Actually RDRAW with RSETUP state kicks the triangle. 
            // ISA: RDRAW rs1 unused? Or rs1=0? "Kick triangle using last setup data".
            rom[pc>>2] = i_type(0, 5'd0, 3'b010, 5'd0, OP_ATOM_SC); pc += 4;

            // Loop Triangles
            rom[pc>>2] = i_type(1, 5'd17, 3'b000, 5'd17, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(TEA_TRIS, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            blt_tri_pc = pc;
            rom[pc>>2] = b_type(loop_tri_pc - pc, 5'd17, 5'd26, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Signal DONE
            rom[pc>>2] = u_type(BASE_ADDR + DONE_OFF, 5'd28, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(1, 5'd28, 3'b000, 5'd0, OP_STORE); pc += 4;

            // Loop Frame
            rom[pc>>2] = i_type(1, 5'd13, 3'b000, 5'd13, OP_INT_IMM); pc += 4;
            rom[pc>>2] = i_type(FRAMES, 5'd0, 3'b000, 5'd26, OP_INT_IMM); pc += 4;
            blt_frame_pc = pc;
            rom[pc>>2] = b_type(loop_frame_pc - pc, 5'd13, 5'd26, 3'b100, OP_BRANCH); pc += 4;
            rom[pc>>2] = nop(); pc += 4;

            // Finish
            rom[pc>>2] = u_type(BASE_ADDR + DONE_OFF, 5'd28, OP_LUI); pc += 4;
            rom[pc>>2] = i_type(2, 5'd28, 3'b000, 5'd0, OP_STORE); pc += 4; // 2=Finish
            
        end
    endtask

endmodule
