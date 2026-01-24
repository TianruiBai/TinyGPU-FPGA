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

    // Instruction cache miss interface
    logic        inst_miss_req_valid;
    logic [31:0] inst_miss_req_addr;
    logic        inst_miss_req_ready;
    logic        inst_miss_resp_valid;
    logic [63:0] inst_miss_resp_data;

    // Legacy data interface (unused now, keep for port tie-off)
    logic        data_req_valid;
    logic        data_req_is_load;
    logic [31:0] data_req_addr;
    logic [31:0] data_req_wdata;
    logic [4:0]  data_req_rd;
    logic        data_req_ready;
    logic        data_resp_valid;
    logic [4:0]  data_resp_rd;
    logic [31:0] data_resp_data;

    // D-cache memory side interface
    logic        dcache_mem_req_valid;
    logic        dcache_mem_req_rw;
    logic [31:0] dcache_mem_req_addr;
    logic [7:0]  dcache_mem_req_size;
    logic [3:0]  dcache_mem_req_qos;
    logic [7:0]  dcache_mem_req_id;
    logic [511:0] dcache_mem_req_wdata;
    logic [7:0]  dcache_mem_req_wstrb;
    logic        dcache_mem_req_ready;
    logic        dcache_mem_resp_valid;
    logic [63:0] dcache_mem_resp_data;
    logic [7:0]  dcache_mem_resp_id;

    // Framebuffer AXI (unused)
    logic        fb_aw_valid;
    logic [31:0] fb_aw_addr;
    logic [7:0]  fb_aw_len;
    logic [2:0]  fb_aw_size;
    logic [1:0]  fb_aw_burst;
    logic        fb_aw_ready;
    logic [31:0] fb_w_data;
    logic [3:0]  fb_w_strb;
    logic        fb_w_last;
    logic        fb_w_valid;
    logic        fb_w_ready;
    logic        fb_b_valid;
    logic        fb_b_ready;

    // Mailbox stream (tied off)
    import mailbox_pkg::*;
    mailbox_pkg::mailbox_flit_t mailbox_tx_data;
    mailbox_pkg::mailbox_flit_t mailbox_rx_data;
    logic mailbox_tx_valid;
    logic mailbox_tx_ready;
    logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_tx_dest_id;
    logic mailbox_rx_valid;
    logic mailbox_rx_ready;
    logic [mailbox_pkg::NODE_ID_WIDTH-1:0] mailbox_rx_dest_id;

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
        .inst_miss_req_valid(inst_miss_req_valid),
        .inst_miss_req_addr(inst_miss_req_addr),
        .inst_miss_req_ready(inst_miss_req_ready),
        .inst_miss_resp_valid(inst_miss_resp_valid),
        .inst_miss_resp_data(inst_miss_resp_data),
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
        .data_resp_data(data_resp_data),
        .dcache_mem_req_valid(dcache_mem_req_valid),
        .dcache_mem_req_rw(dcache_mem_req_rw),
        .dcache_mem_req_addr(dcache_mem_req_addr),
        .dcache_mem_req_size(dcache_mem_req_size),
        .dcache_mem_req_qos(dcache_mem_req_qos),
        .dcache_mem_req_id(dcache_mem_req_id),
        .dcache_mem_req_wdata(dcache_mem_req_wdata),
        .dcache_mem_req_wstrb(dcache_mem_req_wstrb),
        .dcache_mem_req_ready(dcache_mem_req_ready),
        .dcache_mem_resp_valid(dcache_mem_resp_valid),
        .dcache_mem_resp_data(dcache_mem_resp_data),
        .dcache_mem_resp_id(dcache_mem_resp_id),
        .fb_aw_valid(fb_aw_valid),
        .fb_aw_addr(fb_aw_addr),
        .fb_aw_len(fb_aw_len),
        .fb_aw_size(fb_aw_size),
        .fb_aw_burst(fb_aw_burst),
        .fb_aw_ready(fb_aw_ready),
        .fb_w_data(fb_w_data),
        .fb_w_strb(fb_w_strb),
        .fb_w_last(fb_w_last),
        .fb_w_valid(fb_w_valid),
        .fb_w_ready(fb_w_ready),
        .fb_b_valid(fb_b_valid),
        .fb_b_ready(fb_b_ready),
        .mailbox_tx_valid(mailbox_tx_valid),
        .mailbox_tx_ready(mailbox_tx_ready),
        .mailbox_tx_data(mailbox_tx_data),
        .mailbox_tx_dest_id(mailbox_tx_dest_id),
        .mailbox_rx_valid(mailbox_rx_valid),
        .mailbox_rx_ready(mailbox_rx_ready),
        .mailbox_rx_data(mailbox_rx_data),
        .mailbox_rx_dest_id(mailbox_rx_dest_id)
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

    // Tie-offs for unused legacy/global paths
    assign data_req_ready   = 1'b1;
    assign data_resp_valid  = 1'b0;
    assign data_resp_rd     = '0;
    assign data_resp_data   = 32'h0;

    assign mailbox_tx_ready  = 1'b1;
    assign mailbox_rx_valid  = 1'b0;
    assign mailbox_rx_data   = '0;
    assign mailbox_rx_dest_id = '0;
    assign mailbox_rx_ready  = 1'b1;

    assign fb_aw_ready = 1'b1;
    assign fb_w_ready  = 1'b1;
    assign fb_b_valid  = 1'b0;

    // Always-ready backing memory
    assign dcache_mem_req_ready = 1'b1;

    // Initial Load
    initial begin
        for (int i=0; i < (IMEM_SIZE_BYTES/4); i++) imem[i] = 32'h0;
        for (int i=0; i < (DRAM_SIZE_BYTES/4); i++) dmem[i] = 32'h0;

        $display("Loading IMEM from %s...", FW_INST_FILE);
        $readmemh(FW_INST_FILE, imem);
        
        $display("Loading DMEM from %s...", FW_DATA_FILE);
        $readmemh(FW_DATA_FILE, dmem);
    end

    // I-cache miss handler (one-cycle latency)
    assign inst_miss_req_ready = 1'b1;
    logic        inst_miss_pending;
    logic [31:0] inst_miss_addr_q;
    always_ff @(posedge clk) begin
        if (!rst_n) begin
            inst_miss_resp_valid <= 1'b0;
            inst_miss_resp_data  <= 64'h0;
            inst_miss_pending    <= 1'b0;
            inst_miss_addr_q     <= 32'h0;
            dbg_cycle            <= 32'h0;
            dbg_last_inst_addr   <= 32'h0;
            dbg_last_inst_rdata  <= 64'h0;
        end else begin
            inst_miss_resp_valid <= 1'b0;
            dbg_cycle <= dbg_cycle + 1;
            dbg_last_inst_addr <= inst_miss_req_addr;

            if (inst_miss_req_valid && inst_miss_req_ready) begin
                inst_miss_addr_q  <= inst_miss_req_addr;
                inst_miss_pending <= 1'b1;
            end

            if (inst_miss_pending) begin
                automatic int idx = (inst_miss_addr_q - IMEM_BASE) >> 2;
                if ((inst_miss_addr_q >= IMEM_BASE) && (inst_miss_addr_q < (IMEM_BASE + IMEM_SIZE_BYTES - 4))) begin
                    inst_miss_resp_data <= {imem[idx+1], imem[idx]};
                end else begin
                    inst_miss_resp_data <= 64'h0;
                end
                inst_miss_resp_valid <= 1'b1;
                dbg_last_inst_rdata  <= inst_miss_resp_data;
                inst_miss_pending    <= 1'b0;
            end
        end
    end

    // D-cache backing memory: accept full-line transactions and return 64-bit beats
    localparam int DCACHE_LINE_BYTES = 64;
    localparam int DCACHE_BEATS      = DCACHE_LINE_BYTES / 8;

    logic        dcache_tx_active;
    logic        dcache_tx_rw;
    logic [31:0] dcache_tx_addr;
    logic [7:0]  dcache_tx_id_q;
    logic [2:0]  dcache_tx_beat;
    logic [511:0] dcache_tx_wdata;
    logic [7:0]  dcache_tx_wstrb;

    function automatic [63:0] dcache_read_beat(input logic [31:0] line_addr, input logic [2:0] beat);
        int base_word;
        begin
            dcache_read_beat = 64'h0;
            if ((line_addr >= DRAM_BASE) && (line_addr < (DRAM_BASE + DRAM_SIZE_BYTES))) begin
                base_word = ((line_addr - DRAM_BASE) >> 2) + (beat*2);
                dcache_read_beat[31:0]  = dmem[base_word];
                dcache_read_beat[63:32] = dmem[base_word + 1];
            end
        end
    endfunction

    task automatic dcache_write_line(
        input logic [31:0] line_addr,
        input logic [511:0] line_data,
        input logic [7:0]  line_wstrb
    );
        int base_word;
        for (int b = 0; b < DCACHE_BEATS; b++) begin
            if (line_wstrb[b]) begin
                base_word = ((line_addr - DRAM_BASE) >> 2) + (b*2);
                if ((line_addr >= DRAM_BASE) && (line_addr < (DRAM_BASE + DRAM_SIZE_BYTES))) begin
                    dmem[base_word]     <= line_data[b*64 +: 32];
                    dmem[base_word + 1] <= line_data[b*64 + 32 +: 32];
                    if ((line_addr + (b*8)) == RESULT_ADDR) begin
                        check_result(line_data[b*64 +: 32]);
                    end else if ((line_addr + (b*8) + 4) == RESULT_ADDR) begin
                        check_result(line_data[b*64 + 32 +: 32]);
                    end
                end
            end
        end
    endtask

    always_ff @(posedge clk) begin
        if (!rst_n) begin
            dcache_tx_active    <= 1'b0;
            dcache_tx_rw        <= 1'b0;
            dcache_tx_addr      <= 32'h0;
            dcache_tx_id_q      <= 8'h0;
            dcache_tx_beat      <= 3'd0;
            dcache_tx_wdata     <= '0;
            dcache_tx_wstrb     <= '0;
            dcache_mem_resp_valid <= 1'b0;
            dcache_mem_resp_data  <= 64'h0;
            dcache_mem_resp_id    <= 8'h0;
            dbg_last_data_req     <= 1'b0;
            dbg_last_data_is_load <= 1'b0;
            dbg_last_data_addr    <= 32'h0;
            dbg_last_data_wdata   <= 32'h0;
            dbg_last_data_resp    <= 1'b0;
            dbg_last_data_rdata   <= 32'h0;
            dbg_store_count       <= 0;
        end else begin
            dcache_mem_resp_valid <= 1'b0;
            dbg_last_data_resp    <= 1'b0;
            dbg_last_data_req     <= 1'b0;

            if (dcache_mem_req_valid && dcache_mem_req_ready) begin
                dcache_tx_active <= !dcache_mem_req_rw;
                dcache_tx_rw     <= dcache_mem_req_rw;
                dcache_tx_addr   <= dcache_mem_req_addr;
                dcache_tx_id_q   <= dcache_mem_req_id;
                dcache_tx_wdata  <= dcache_mem_req_wdata;
                dcache_tx_wstrb  <= dcache_mem_req_wstrb;
                dcache_tx_beat   <= 3'd0;

                dbg_last_data_req     <= 1'b1;
                dbg_last_data_is_load <= !dcache_mem_req_rw;
                dbg_last_data_addr    <= dcache_mem_req_addr;
                dbg_last_data_wdata   <= dcache_mem_req_wdata[31:0];

                if (dcache_mem_req_rw) begin
                    dcache_write_line(dcache_mem_req_addr, dcache_mem_req_wdata, dcache_mem_req_wstrb);
                    dcache_mem_resp_valid <= 1'b1;
                    dcache_mem_resp_id    <= dcache_mem_req_id;
                    dcache_mem_resp_data  <= 64'h0;
                    dbg_store_count       <= dbg_store_count + 1;
                end
            end else if (dcache_tx_active) begin
                dcache_mem_resp_valid <= 1'b1;
                dcache_mem_resp_id    <= dcache_tx_id_q;
                dcache_mem_resp_data  <= dcache_read_beat(dcache_tx_addr, dcache_tx_beat);
                dbg_last_data_resp    <= 1'b1;
                dbg_last_data_rdata   <= dcache_mem_resp_data;

                if (dcache_tx_beat == DCACHE_BEATS-1) begin
                    dcache_tx_active <= 1'b0;
                end else begin
                    dcache_tx_beat <= dcache_tx_beat + 1'b1;
                end
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
        $display("Timeout debug: last_miss_addr=%h data_req=%b is_load=%b addr=%h", dbg_last_inst_addr, dbg_last_data_req, dbg_last_data_is_load, dbg_last_data_addr);
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