`timescale 1ns/1ps

module compute_unit_top_tb;

    // ========================================================================
    // 1. Parameters & Configuration
    // ========================================================================
    localparam string FW_INST_FILE = "test_gpu_full_inst.hex";
    localparam string FW_DATA_FILE = "test_gpu_full_data.hex";
    
    // Memory Sizes
    localparam int IMEM_SIZE_BYTES = 64 * 1024;    // 64KB Code
    localparam int DRAM_SIZE_BYTES = 1024 * 1024;  // 1MB Data/VRAM
    
    // Address Mapping (Matches memory_map.md)
    localparam logic [31:0] IMEM_BASE = 32'h0000_0000;
    localparam logic [31:0] DRAM_BASE = 32'h8000_0000;
    
    // Test Status Address (from assembly)
    localparam logic [31:0] RESULT_ADDR = 32'h8000_0100;
    
    // Framebuffer Config
    localparam int FB_WIDTH  = 128;
    localparam int FB_HEIGHT = 128;
    localparam logic [31:0] FB_BASE_ADDR = 32'h8001_0000;

    // ========================================================================
    // 2. Signals & DUT Interface
    // ========================================================================
    logic clk;
    logic rst_n;

    // Instruction Fetch Interface (64-bit aligned)
    logic [31:0] inst_addr;
    logic [63:0] inst_rdata;

    // Data Memory Interface (Unified LSU)
    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;
    logic        data_req_ready;

    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    logic        err_fp_overflow;
    logic        err_fp_invalid;
    logic        err_vec_overflow;
    logic        err_vec_invalid;

    logic [31:0] csr_status;
    logic [31:0] csr_fstatus;
    logic [31:0] csr_vstatus;

    // Debug capture (for waveform inspection)
    logic [31:0] dbg_cycle;
    logic [31:0] dbg_last_inst_addr;
    logic [63:0] dbg_last_inst_rdata;
    logic        dbg_last_data_req;
    logic        dbg_last_data_is_load;
    logic [31:0] dbg_last_data_addr;
    logic [31:0] dbg_last_data_wdata;
    logic        dbg_last_data_resp;
    logic [31:0] dbg_last_data_rdata;
    integer      dbg_store_count;

    // ========================================================================
    // 3. DUT Instantiation
    // ========================================================================
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

    // ------------------------------------------------------------------------
    // Wave dump
    // ------------------------------------------------------------------------
    initial begin
        $dumpfile("compute_unit_top_tb.vcd");
        $dumpvars(0, compute_unit_top_tb);
    end

    // ========================================================================
    // 4. Memory Models
    // ========================================================================
    
    // 4.1 Instruction Memory (32-bit word array)
    logic [31:0] imem [0:(IMEM_SIZE_BYTES/4)-1];

    // 4.2 Data Memory (32-bit word array)
    logic [31:0] dmem [0:(DRAM_SIZE_BYTES/4)-1];

    // Simple response FIFO for load responses
    localparam int RESP_DEPTH = 64;
    logic        resp_valid   [0:RESP_DEPTH-1];
    logic [4:0]  resp_rd_q    [0:RESP_DEPTH-1];
    logic [31:0] resp_data_q  [0:RESP_DEPTH-1];
    logic [$clog2(RESP_DEPTH)-1:0] resp_wp;
    logic [$clog2(RESP_DEPTH)-1:0] resp_rp;

    assign data_req_ready = 1'b1;

    // Initial Load
    initial begin
        // Initialize memory to 0
        for (int i=0; i < (IMEM_SIZE_BYTES/4); i++) imem[i] = 32'h0;
        for (int i=0; i < (DRAM_SIZE_BYTES/4); i++) dmem[i] = 32'h0;

        // Load Hex Files
        $display("Loading IMEM from %s...", FW_INST_FILE);
        $readmemh(FW_INST_FILE, imem);
        
        $display("Loading DMEM from %s...", FW_DATA_FILE);
        // Note: $readmemh loads into array starting at index 0. 
        // The Data Hex assumes offset 0 relative to start of .data section.
        // We load it directly into dmem base.
        $readmemh(FW_DATA_FILE, dmem);
    end

    // Instruction Fetch Logic
    // Access is 64-bit aligned. 
    // inst_addr points to bytes. Index = addr / 4.
    always_comb begin
        // Fetch two 32-bit words
        // Little Endian: [31:0] is at addr, [63:32] is at addr+4
        automatic int idx = (inst_addr - IMEM_BASE) >> 2;
        if (inst_addr >= IMEM_BASE && inst_addr < (IMEM_BASE + IMEM_SIZE_BYTES)) begin
            inst_rdata = {imem[idx+1], imem[idx]}; 
        end else begin
            inst_rdata = 64'h0; // Bus Error / NOP
        end
    end

    // Data Access Logic (Synchronous Read, Synchronous Write)
    always @(posedge clk) begin
        if (!rst_n) begin
            resp_wp <= '0;
            resp_rp <= '0;
            data_resp_valid <= 1'b0;
            data_resp_rd <= '0;
            data_resp_data <= '0;
            dbg_cycle <= 0;
            dbg_last_inst_addr <= 0;
            dbg_last_inst_rdata <= 0;
            dbg_last_data_req <= 0;
            dbg_last_data_is_load <= 0;
            dbg_last_data_addr <= 0;
            dbg_last_data_wdata <= 0;
            dbg_last_data_resp <= 0;
            dbg_last_data_rdata <= 0;
            dbg_store_count <= 0;
            for (int i = 0; i < RESP_DEPTH; i++) begin
                resp_valid[i] <= 1'b0;
                resp_rd_q[i] <= '0;
                resp_data_q[i] <= '0;
            end
        end else begin
            data_resp_valid <= 1'b0;
            data_resp_rd <= '0;

            dbg_cycle <= dbg_cycle + 1;
            dbg_last_inst_addr <= inst_addr;
            dbg_last_inst_rdata <= inst_rdata;

            if (data_req_valid) begin
                dbg_last_data_req <= 1'b1;
                dbg_last_data_is_load <= data_req_is_load;
                dbg_last_data_addr <= data_req_addr;
                dbg_last_data_wdata <= data_req_wdata;

                if (data_req_addr >= DRAM_BASE && data_req_addr < (DRAM_BASE + DRAM_SIZE_BYTES)) begin
                    automatic int d_idx = (data_req_addr - DRAM_BASE) >> 2;
                    if (data_req_is_load) begin
                        resp_valid[resp_wp] <= 1'b1;
                        resp_rd_q[resp_wp] <= data_req_rd;
                        resp_data_q[resp_wp] <= dmem[d_idx];
                        resp_wp <= resp_wp + 1'b1;
                    end else begin
                        dmem[d_idx] <= data_req_wdata;
                        if (dbg_store_count < 4) begin
                            $display("[%0t] STORE addr=%h data=%h", $time, data_req_addr, data_req_wdata);
                        end
                        dbg_store_count <= dbg_store_count + 1;
                        if (data_req_addr == RESULT_ADDR) begin
                            check_result(data_req_wdata);
                        end
                    end
                end else begin
                    if (data_req_is_load) begin
                        resp_valid[resp_wp] <= 1'b1;
                        resp_rd_q[resp_wp] <= data_req_rd;
                        resp_data_q[resp_wp] <= 32'h0;
                        resp_wp <= resp_wp + 1'b1;
                    end
                end
            end else begin
                dbg_last_data_req <= 1'b0;
            end

            if (resp_valid[resp_rp]) begin
                data_resp_valid <= 1'b1;
                data_resp_rd <= resp_rd_q[resp_rp];
                data_resp_data <= resp_data_q[resp_rp];
                dbg_last_data_resp <= 1'b1;
                dbg_last_data_rdata <= resp_data_q[resp_rp];
                resp_valid[resp_rp] <= 1'b0;
                resp_rp <= resp_rp + 1'b1;
            end else begin
                dbg_last_data_resp <= 1'b0;
            end
        end
    end

    // ========================================================================
    // 5. Tasks & Verification
    // ========================================================================

    task check_result(input logic [31:0] val);
        $display("[%0t] Write to RESULT_ADDR: 0x%h", $time, val);
        if (val == 32'h0000_600D) begin
            $display("\n------------------------------------------------");
            $display(" TEST PASSED: Success Code Received");
            $display("------------------------------------------------\n");
            dump_framebuffer();
            $finish;
        end else if (val == 32'h0000_DEAD) begin
            $display("\n------------------------------------------------");
            $display(" TEST FAILED: Timeout or Error Trap");
            $display("------------------------------------------------\n");
            $finish;
        end else begin
            $display(" -> Error Code detected: 0x%h", val);
            $finish;
        end
    endtask

    task dump_framebuffer();
        integer fd;
        integer x, y, addr, word_val;
        byte r, g, b;
        
        $display("Dumping Framebuffer to 'frame.ppm'...");
        fd = $fopen("frame.ppm", "w");
        
        // PPM Header (P3: ASCII RGB)
        $fwrite(fd, "P3\n%0d %0d\n255\n", FB_WIDTH, FB_HEIGHT);
        
        for (y = 0; y < FB_HEIGHT; y++) begin
            for (x = 0; x < FB_WIDTH; x++) begin
                // Calculate Address
                addr = FB_BASE_ADDR + ((y * FB_WIDTH + x) * 4);
                
                // Read from Memory Model
                word_val = dmem[(addr - DRAM_BASE) >> 2];
                
                // Extract ARGB8888 (Little Endian in word: B G R A)
                // Note: Standard ARGB: [31:24]A [23:16]R [15:8]G [7:0]B
                // Adjust extraction based on your actual pixel pipe byte ordering
                b = word_val[7:0];
                g = word_val[15:8];
                r = word_val[23:16];
                
                $fwrite(fd, "%0d %0d %0d ", r, g, b);
            end
            $fwrite(fd, "\n");
        end
        
        $fclose(fd);
        $display("Dump complete.");
    endtask

    // ========================================================================
    // 6. Simulation Control
    // ========================================================================
    
    // Clock Gen
    always #5 clk = ~clk; // 100MHz

    // Main
    initial begin
        // Init
        clk = 0;
        rst_n = 0;
        
        // Reset Sequence
        #100;
        rst_n = 1;
        $display("Reset released. Simulation started.");
        
        // Watchdog (Simulation Time limit)
        #1000000; 
        $display("Error: Simulation Watchdog Timeout (Test hung).");
        $display("Timeout debug: pc=%h inst_addr=%h data_req=%b is_load=%b addr=%h", dbg_last_inst_addr, inst_addr, dbg_last_data_req, dbg_last_data_is_load, dbg_last_data_addr);
        $finish;
    end
    
    // Monitor Instructions (Optional Debug)
    /*
    always @(posedge clk) begin
        if (dut.inst_valid && !dut.stall) begin
            $display("[%0t] PC: %h | Inst: %h", $time, dut.pc, dut.curr_inst);
        end
    end
    */

endmodule