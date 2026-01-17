`timescale 1ns/1ps

// Comprehensive testbench for compute_unit_top
module compute_unit_tb;
    import isa_pkg::*;

    // Clock/reset
    logic clk;
    logic rst_n;

    // Instruction interface
    logic [63:0] inst_rdata;
    logic [31:0] inst_addr;

    // Data interface
    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;
    logic        data_req_ready;
    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    // CSR sideband (optional visibility)
    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    // Error flags
    logic err_fp_overflow;
    logic err_fp_invalid;
    logic err_vec_overflow;
    logic err_vec_invalid;

    // DUT instance (parameterized core/tile IDs)
    compute_unit_top #(
        .CORE_ID(32'h1),
        .TILE_OFFSET(32'h0001_0002)
    ) dut (
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

    // Clock generation: 100MHz
    initial begin
        clk = 1'b0;
        forever #5 clk = ~clk;
    end

    // Reset sequence
    initial begin
        rst_n = 1'b0;
        data_req_ready = 1'b1; // always-ready memory model
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
    end

    // Waveform dump for debugging
    initial begin
        $dumpfile("compute_unit_tb.vcd");
        // Dump full DUT hierarchy; adjust depth if needed
        $dumpvars(0, compute_unit_tb);
    end

    // ------------------------------------------------------------------
    // Instruction ROM (combinational lookup)
    // ------------------------------------------------------------------
    localparam int ROM_WORDS = 128;
    logic [31:0] rom [0:ROM_WORDS-1];

    // 64-bit fetch aligned to 8 bytes: low word at +0, high word at +4
    assign inst_rdata = {rom[{inst_addr[8:3], 1'b1}], rom[{inst_addr[8:3], 1'b0}]};

    // ------------------------------------------------------------------
    // Simple global memory model (32-bit words)
    // - Global if addr[31]==1; local is ignored here
    // - Single-cycle accept, 1-cycle response latency, small FIFO for bursts
    // ------------------------------------------------------------------
    localparam int MEM_WORDS = 4096; // 16KB global memory
    localparam logic [31:0] BASE_ADDR = 32'hFFFF_F800; // addr[31]=1 to drive global path

    logic [31:0] mem [0:MEM_WORDS-1];

    function automatic int mem_index(input logic [31:0] addr);
        mem_index = (addr - BASE_ADDR) >> 2;
    endfunction

    // Response FIFO for loads (depth 16)
    localparam int RESP_DEPTH = 16;
    logic              resp_valid   [0:RESP_DEPTH-1];
    logic [4:0]        resp_rd      [0:RESP_DEPTH-1];
    logic [31:0]       resp_data_q  [0:RESP_DEPTH-1];
    logic [3:0]        resp_wp;
    logic [3:0]        resp_rp;
    logic              resp_empty;

    assign resp_empty = (resp_wp == resp_rp);

    always @(posedge clk) begin
        if (!rst_n) begin
            resp_wp        <= 4'd0;
            resp_rp        <= 4'd0;
            data_resp_valid<= 1'b0;
            data_resp_rd   <= 5'd0;
            data_resp_data <= 32'h0;
            for (int i = 0; i < RESP_DEPTH; i++) begin
                resp_valid[i]  <= 1'b0;
                resp_rd[i]     <= 5'd0;
                resp_data_q[i] <= 32'h0;
            end
        end else begin
            // Accept requests
            if (data_req_valid && data_req_ready) begin
                if (data_req_is_load) begin
                    int idx;
                    idx = mem_index(data_req_addr);
                    resp_valid[resp_wp]  <= 1'b1;
                    resp_rd[resp_wp]     <= data_req_rd;
                    resp_data_q[resp_wp] <= mem[idx];
                    resp_wp <= resp_wp + 1'b1;
                end else begin
                    int idx;
                    idx = mem_index(data_req_addr);
                    mem[idx] <= data_req_wdata;
                end
            end

            // Drive responses (one per cycle if available)
            if (!resp_empty && resp_valid[resp_rp]) begin
                data_resp_valid <= 1'b1;
                data_resp_rd    <= resp_rd[resp_rp];
                data_resp_data  <= resp_data_q[resp_rp];
                resp_valid[resp_rp] <= 1'b0;
                resp_rp <= resp_rp + 1'b1;
            end else begin
                data_resp_valid <= 1'b0;
            end
        end
    end

    // ------------------------------------------------------------------
    // Mini assembler helpers (RV-like encoding with custom opcode map)
    // ------------------------------------------------------------------
    function automatic [31:0] r_type(input [6:0] funct7, input [4:0] rs2, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        r_type = {funct7, rs2, rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] i_type(input integer imm, input [4:0] rs1, input [2:0] funct3, input [4:0] rd, input [6:0] opcode);
        i_type = {imm[11:0], rs1, funct3, rd, opcode};
    endfunction

    function automatic [31:0] s_type(input integer imm, input [4:0] rs1, input [4:0] rs2, input [2:0] funct3, input [6:0] opcode);
        s_type = {imm[11:5], rs2, rs1, funct3, imm[4:0], opcode};
    endfunction

    function automatic [31:0] nop();
        nop = i_type(12'd0, 5'd0, 3'b000, 5'd0, OP_INT); // ADDI x0, x0, 0
    endfunction

    // ------------------------------------------------------------------
    // Program image
    // ------------------------------------------------------------------
    initial begin
        // pc tracks byte addresses; rom is word-addressable
        automatic int pc;
        for (int i = 0; i < ROM_WORDS; i++) rom[i] = nop();

        pc = 0;
        // x1 = BASE_ADDR (ADDI x1, x0, -2048)
        rom[pc >> 2] = i_type(-2048, 5'd0, 3'b000, 5'd1, OP_INT);
        pc += 4;
        // x2 = 5; x3 = 7
        rom[pc >> 2] = i_type(5, 5'd0, 3'b000, 5'd2, OP_INT);
        pc += 4;
        rom[pc >> 2] = i_type(7, 5'd0, 3'b000, 5'd3, OP_INT);
        pc += 4;
        // x4 = x2 + x3 = 12
        rom[pc >> 2] = r_type(7'b0000000, 5'd3, 5'd2, 3'b000, 5'd4, OP_INT);
        pc += 4;
        // SW x4, 0(x1)
        rom[pc >> 2] = s_type(0, 5'd1, 5'd4, 3'b010, OP_STORE);
        pc += 4;
        // LW x5, 0(x1)
        rom[pc >> 2] = i_type(0, 5'd1, 3'b010, 5'd5, OP_LOAD);
        pc += 4;
        // SW x5, 8(x1)
        rom[pc >> 2] = s_type(8, 5'd1, 5'd5, 3'b010, OP_STORE);
        pc += 4;
        // x6 = 7
        rom[pc >> 2] = i_type(7, 5'd0, 3'b000, 5'd6, OP_INT);
        pc += 4;
        // CSRRW status (imm=0) with x6 -> rd=x7
        rom[pc >> 2] = i_type(0, 5'd6, 3'b001, 5'd7, OP_SYSTEM);
        pc += 4;
        // CSRRS status read into x8 using x0 (no set)
        rom[pc >> 2] = i_type(0, 5'd0, 3'b010, 5'd8, OP_SYSTEM);
        pc += 4;
        // MEMBAR
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM);
        pc += 4;
        // VLD.V v1, 16(x1)
        rom[pc >> 2] = i_type(16, 5'd1, 3'b000, 5'd1, OP_VLD);
        pc += 4;
        // VLD.V v2, 32(x1)
        rom[pc >> 2] = i_type(32, 5'd1, 3'b000, 5'd2, OP_VLD);
        pc += 4;
        // VADD v3 = v1 + v2 (funct6=000000)
        rom[pc >> 2] = r_type(7'b0000000, 5'd2, 5'd1, 3'b000, 5'd3, OP_VEC_ALU);
        pc += 4;
        // VST.V v3, 48(x1)
        rom[pc >> 2] = s_type(48, 5'd1, 5'd3, 3'b000, OP_VST);
        pc += 4;
        // VCMP.eq v1, v1 -> scalar mask in x9
        rom[pc >> 2] = r_type(7'b0000110, 5'd1, 5'd1, 3'b000, 5'd9, OP_VEC_ALU);
        pc += 4;
        // SW x9, 64(x1)
        rom[pc >> 2] = s_type(64, 5'd1, 5'd9, 3'b010, OP_STORE);
        pc += 4;
        // VATOM.ADD v5, 48(x1), v2 (funct3=000)
        // Encode as S-type so rs2 carries the source vector index (v2)
        rom[pc >> 2] = s_type(48, 5'd1, 5'd2, 3'b000, OP_ATOM_V);
        pc += 4;
        // VLD.V v6, 48(x1) ; expect summed atomics
        rom[pc >> 2] = i_type(48, 5'd1, 3'b000, 5'd6, OP_VLD);
        pc += 4;
        // VST.V v6, 80(x1) ; capture final vector result
        rom[pc >> 2] = s_type(80, 5'd1, 5'd6, 3'b000, OP_VST);
        pc += 4;
        // Final MEMBAR
        rom[pc >> 2] = i_type(0, 5'd0, 3'b000, 5'd0, OP_SYSTEM);
    end

    // ------------------------------------------------------------------
    // Initialize data memory
    // ------------------------------------------------------------------
    initial begin
        for (int i = 0; i < MEM_WORDS; i++) mem[i] = 32'h0;
        // Seed vector operands at base+16 and base+32
        mem[mem_index(BASE_ADDR + 32'd16)] = 32'd1;
        mem[mem_index(BASE_ADDR + 32'd20)] = 32'd2;
        mem[mem_index(BASE_ADDR + 32'd24)] = 32'd3;
        mem[mem_index(BASE_ADDR + 32'd28)] = 32'd4;

        mem[mem_index(BASE_ADDR + 32'd32)] = 32'd10;
        mem[mem_index(BASE_ADDR + 32'd36)] = 32'd20;
        mem[mem_index(BASE_ADDR + 32'd40)] = 32'd30;
        mem[mem_index(BASE_ADDR + 32'd44)] = 32'd40;
    end

    // ------------------------------------------------------------------
    // Scoreboard / checks
    // ------------------------------------------------------------------
    task automatic check_equal(input string name, input logic [31:0] got, input logic [31:0] exp);
        if (got !== exp) begin
            $fatal(1, "%s mismatch: got %0d (0x%08x) expected %0d (0x%08x)", name, got, got, exp, exp);
        end else begin
            $display("[OK] %s = %0d (0x%08x)", name, got, got);
        end
    endtask

    initial begin
        wait(rst_n);
        repeat (400) @(posedge clk);

        check_equal("scalar store @0", mem[mem_index(BASE_ADDR + 32'd0)], 32'd12);
        check_equal("scalar store @8", mem[mem_index(BASE_ADDR + 32'd8)], 32'd12);
        check_equal("csr_status", csr_status, 32'd7);
        check_equal("vcmp mask store", mem[mem_index(BASE_ADDR + 32'd64)], 32'hF);

        // Vector after VADD stored at base+48; VATOM then updates it in place
        check_equal("vadd lane0", mem[mem_index(BASE_ADDR + 32'd48)], 32'd21);
        check_equal("vadd lane1", mem[mem_index(BASE_ADDR + 32'd52)], 32'd42);
        check_equal("vadd lane2", mem[mem_index(BASE_ADDR + 32'd56)], 32'd63);
        check_equal("vadd lane3", mem[mem_index(BASE_ADDR + 32'd60)], 32'd84);

        // After atomic add and capture to base+80
        check_equal("vatom lane0", mem[mem_index(BASE_ADDR + 32'd80)], 32'd21);
        check_equal("vatom lane1", mem[mem_index(BASE_ADDR + 32'd84)], 32'd42);
        check_equal("vatom lane2", mem[mem_index(BASE_ADDR + 32'd88)], 32'd63);
        check_equal("vatom lane3", mem[mem_index(BASE_ADDR + 32'd92)], 32'd84);

        $display("\nAll checks passed.");
        $finish;
    end
endmodule
