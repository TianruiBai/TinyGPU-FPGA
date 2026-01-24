// Simple L1 cache unit testbench (Verilator friendly)
// Tests basic hit/miss/refill behavior using a mock memory model.

`timescale 1ns / 1ps

module l1_cache_tb;
    logic clk;
    logic rst_n;

    // DUT signals
    logic req_valid;
    logic [1:0] req_type;
    logic [31:0] req_addr;
    logic [127:0] req_wdata;
    logic [7:0] req_id;
    logic [63:0] resp_data;
    logic resp_valid;
    logic resp_err;

    // Memory interface
    logic mem_req_valid;
    logic mem_req_rw;
    logic [31:0] mem_req_addr;
    logic [511:0] mem_req_wdata;
    logic [7:0] mem_req_wstrb;
    logic mem_req_ready;
    logic mem_resp_valid;
    logic [63:0] mem_resp_data;
    logic cfg_flush;

    localparam int LINE_BEATS = 64/8; // line bytes / beat bytes

    localparam int VEC_WORDS = 4;

    // signals for vector/scalar control
    logic lsu0_req_is_vector;
    logic [VEC_WORDS-1:0] lsu0_req_vec_wmask;
    logic lsu0_req_ready;
    logic lsu1_req_valid;
    logic [1:0] lsu1_req_type;
    logic [31:0] lsu1_req_addr;
    logic [127:0] lsu1_req_wdata;
    logic [7:0] lsu1_req_id;
    logic lsu1_req_is_vector;
    logic [VEC_WORDS-1:0] lsu1_req_vec_wmask;
    logic lsu1_req_ready;

    // DUT (connect testbench to LSU0 port; tie LSU1/LSU_TEX inactive)
    // Instantiate with WRITEBACK enabled to test writeback flush behavior
    l1_data_cache #(.WRITEBACK(1), .LINE_BYTES(64), .NUM_MSHR(6)) dut (
        .clk(clk),
        .rst_n(rst_n),
        .lsu0_req_valid(req_valid),
        .lsu0_req_type(req_type),
        .lsu0_req_atomic_op(3'b000),
        .lsu0_req_addr(req_addr),
        .lsu0_req_wdata(req_wdata),
        .lsu0_req_wstrb('0),
        .lsu0_req_is_vector(lsu0_req_is_vector),
        .lsu0_req_vec_wmask(lsu0_req_vec_wmask),
        .lsu0_req_id(req_id),
        .lsu0_req_ready(lsu0_req_ready),
        .lsu0_resp_valid(resp_valid),
        .lsu0_resp_data(),
        .lsu0_resp_id(),
        .lsu0_resp_err(resp_err),

        .lsu1_req_valid(lsu1_req_valid),
        .lsu1_req_type(lsu1_req_type),
        .lsu1_req_atomic_op(3'b000),
        .lsu1_req_addr(lsu1_req_addr),
        .lsu1_req_wdata(lsu1_req_wdata),
        .lsu1_req_wstrb('0),
        .lsu1_req_is_vector(lsu1_req_is_vector),
        .lsu1_req_vec_wmask(lsu1_req_vec_wmask),
        .lsu1_req_id(lsu1_req_id),
        .lsu1_req_ready(lsu1_req_ready),
        .lsu1_resp_valid(),
        .lsu1_resp_data(),
        .lsu1_resp_id(),
        .lsu1_resp_err(),

        .lsu_tex_req_valid(1'b0),
        .lsu_tex_req_type(2'b0),
        .lsu_tex_req_addr(32'b0),
        .lsu_tex_req_wdata(32'b0),
        .lsu_tex_req_wstrb('0),
        .lsu_tex_req_id(8'b0),
        .lsu_tex_req_ready(),
        .lsu_tex_resp_valid(),
        .lsu_tex_resp_data(),
        .lsu_tex_resp_id(),
        .lsu_tex_resp_err(),

        .cfg_flush(cfg_flush),
        .cfg_invalidate(1'b0),
        .mem_req_valid(mem_req_valid),
        .mem_req_rw(mem_req_rw),
        .mem_req_addr(mem_req_addr),
        .mem_req_size(),
        .mem_req_qos(),
        .mem_req_id(),
        .mem_req_wdata(mem_req_wdata),
        .mem_req_wstrb(mem_req_wstrb),
        .mem_req_ready(mem_req_ready),
        .mem_resp_valid(mem_resp_valid),
        .mem_resp_data(mem_resp_data),
        .mem_resp_id('0)
    );

    // Mock memory: respond to mem requests after N cycles
    int mem_latency = 8;
    int mem_pending = 0;
    logic [31:0] mem_pending_addr;
    int resp_beats;

    // Clock
    initial begin
        clk = 0;
        forever #5 clk = ~clk; // 100MHz
    end

    // Reset + test sequences
    integer i;
    initial begin
        $display("Starting L1 cache TB");
        rst_n = 0;
        req_valid = 0;
        cfg_flush = 0;
        #20;
        rst_n = 1;
        #20;
        $display("DUT WRITEBACK=%0d", dut.WRITEBACK);

        // initialize control signals
        lsu0_req_is_vector = 1'b0;
        lsu0_req_vec_wmask = 4'b0001;
        lsu1_req_valid = 1'b0;
        lsu1_req_type = 2'b00;
        lsu1_req_addr = 32'b0;
        lsu1_req_wdata = 128'b0;
        lsu1_req_id = 8'b0;
        lsu1_req_is_vector = 1'b0;
        lsu1_req_vec_wmask = 4'b0001;

        // 1) Simple scalar load miss -> observe mem_req
        req_type = 0; // LOAD
        req_addr = 32'h0000_1000; req_id = 8'd1; req_wdata = 0;
        req_valid = 1;
        while (!lsu0_req_ready) @(posedge clk);
        @(posedge clk); // handshake edge
        @(negedge clk);
        req_valid = 0;

        // Wait for memory request & simulate response (scalar case)
        wait (mem_req_valid);
        $display("Observed mem_req addr: 0x%08x", mem_req_addr);
        // mem model handles response
        @(posedge mem_resp_valid);
        @(negedge mem_resp_valid);
        $display("Scalar load refill completed @%0t", $time);

        // 1b) Vector load miss test: issue a vector load to a aligned address
        #40;
        req_type = 0; // LOAD
        req_addr = 32'h0000_2000; req_id = 8'd3; req_wdata = 0;
        lsu0_req_is_vector = 1'b1;
        lsu0_req_vec_wmask = 4'b1111; // full vector
        req_valid = 1;
        while (!lsu0_req_ready) @(posedge clk);
        @(posedge clk);
        @(negedge clk);
        req_valid = 0;
        // Wait for the refill to come back (mem_req is printed in the handshake monitor)
        @(posedge mem_resp_valid);
        $display("Vector load saw mem_resp_valid posedge @%0t", $time);
        @(negedge mem_resp_valid);
        $display("Vector load saw mem_resp_valid negedge @%0t", $time);
        $display("Vector load refill completed @%0t", $time);

        // 2) Vector store test (store 4 words) -> should mark dirty (WB) or write-through (WT)
        #40;
        req_type = 1; // STORE
        req_addr = 32'h0000_3000; req_id = 8'd4; req_wdata = 128'hDEAD_BEEF_DEAD_BEEF_DEAD_BEEF_DEAD_BEEF;
        lsu0_req_is_vector = 1'b1;
        lsu0_req_vec_wmask = 4'b1111;
        req_valid = 1;
        while (!lsu0_req_ready) @(posedge clk);
        @(posedge clk);
        @(negedge clk);
        req_valid = 0;
        // With WRITEBACK enabled we expect no immediate mem_req for the store; wait for refill to finish and cache to idle, then trigger a flush
        wait (dut.state == 0 && !dut.mem_busy && !dut.mem_req_valid_r);
        $display("Triggering cfg_flush to force writebacks (cache idle) @%0t state=%0d wb_count=%0d", $time, dut.state, dut.wb_count);
        // hold cfg_flush for two cycles to guarantee latch inside DUT
        cfg_flush = 1'b1; $display("TB cfg_flush asserted @%0t dut.cfg_flush=%0b", $time, dut.cfg_flush);
        @(posedge clk);
        @(posedge clk);
        cfg_flush = 1'b0;
        $display("TB cfg_flush deasserted @%0t (dut.cfg_flush=%0b)", $time, dut.cfg_flush);
        #1;
        $display("TB post-flush sample: dut.flush_active=%0b wb_count=%0d state=%0d", dut.flush_active, dut.wb_count, dut.state);
        // wait for mem_req writeback to be seen
        wait (mem_req_valid);
        $display("Observed writeback mem_req addr: 0x%08x", mem_req_addr);
        // mem model handles response
        @(posedge mem_resp_valid);
        @(negedge mem_resp_valid);

        // 2b) LSU1 scalar store test
        #40;
        lsu1_req_valid = 1'b1;
        lsu1_req_type = 2'b01; // STORE
        lsu1_req_addr = 32'h0000_4000;
        lsu1_req_wdata = 128'h0000_0000_0000_0000_0000_0000_FEED_FACE;
        lsu1_req_id = 8'd5;
        lsu1_req_is_vector = 1'b0;
        lsu1_req_vec_wmask = 4'b0001;
        while (!lsu1_req_ready) @(posedge clk);
        @(posedge clk);
        @(negedge clk);
        lsu1_req_valid = 1'b0;

        // 2c) Simple load hit (same line) - should be fast
        #10;
        req_addr = 32'h0000_1004; req_id = 8'd2; req_valid = 1; // same cache line
        while (!lsu0_req_ready) @(posedge clk);
        @(posedge clk);
        @(negedge clk);
        req_valid = 0;

        // 3) Fill more than cache capacity to force evictions (smoke test)
        #100;
        for (i=0; i<200; i++) begin
            req_addr = 32'h0000_2000 + (i*64); req_id = i[7:0];
            req_valid = 1;
            while (!lsu0_req_ready) @(posedge clk);
            @(posedge clk);
            @(negedge clk);
            req_valid = 0; #2;
            // memory model will respond
            @(posedge mem_resp_valid);
            @(negedge mem_resp_valid);
        end

        #100;
        $display("L1 cache TB finished");
        $finish;
    end

    // Lightweight visibility to catch stalls early
    initial begin
        @(posedge rst_n);
        wait (lsu0_req_ready === 1'b1);
        $display("TB observe lsu0_req_ready=1 @%0t", $time);
    end

    initial begin
        wait (mem_req_valid === 1'b1);
        $display("TB observe mem_req_valid=1 addr=%08h rw=%0b @%0t", mem_req_addr, mem_req_rw, $time);
    end

    initial begin
        #2000;
        $display("TB watchdog fired @%0t ready0=%0b ready1=%0b mem_req_valid=%0b mem_busy=%0b state=%0d mem_req_valid_r=%0b", $time,
                 lsu0_req_ready, lsu1_req_ready, mem_req_valid, dut.mem_busy, dut.state, dut.mem_req_valid_r);
        $finish;
    end

    // Track mem_resp_valid edges to debug waits
    always @(posedge mem_resp_valid or negedge mem_resp_valid) begin
        $display("TB mem_resp_valid=%0b @%0t", mem_resp_valid, $time);
    end

    // Early cycle trace to confirm req_valid/ready are driving as expected
    initial begin
        repeat (12) begin
            @(posedge clk);
            $display("TB early t=%0t req_valid=%0b ready0=%0b hshake=%0b state=%0d mem_busy=%0b", $time,
                     req_valid, lsu0_req_ready, (req_valid && lsu0_req_ready), dut.state, dut.mem_busy);
        end
    end

    // Handshake visibility for LSU and memory
    always @(posedge clk) begin
        if ($time < 150000) begin
            $display("HB t=%0t req_valid=%0b ready0=%0b", $time, req_valid, lsu0_req_ready);
        end
        if (req_valid && lsu0_req_ready) begin
            $display("TB LSU0 handshake type=%0d addr=%08h @%0t", req_type, req_addr, $time);
        end else if (req_valid) begin
            $display("TB LSU0 valid without ready=%0b @%0t", lsu0_req_ready, $time);
        end
        if (lsu1_req_valid && lsu1_req_ready) begin
            $display("TB LSU1 handshake type=%0d addr=%08h @%0t", lsu1_req_type, lsu1_req_addr, $time);
        end
        if (mem_req_valid && mem_req_ready) begin
            $display("TB mem_req fire rw=%0b addr=%08h @%0t", mem_req_rw, mem_req_addr, $time);
        end
        if (dut.wb_push) begin
            $display("TB observed wb_push addr=%08h dirty=%0b idx=%0d @%0t", dut.wb_push_addr, dut.wb_push_dirty_flag, dut.flush_index, $time);
        end
        if (dut.flush_active) begin
            $display("TB flush_active idx=%0d wb_count=%0d @%0t", dut.flush_index, dut.wb_count, $time);
        end
    end

    // Simple memory model: track one outstanding request and respond after mem_latency cycles
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            mem_req_ready <= 1'b1;
            mem_pending <= 0;
            mem_pending_addr <= '0;
            resp_beats <= 0;
            mem_resp_valid <= 1'b0;
            mem_resp_data <= '0;
        end else begin
            mem_resp_valid <= 1'b0;
            if (mem_req_valid && mem_req_ready) begin
                mem_pending <= mem_latency;
                mem_pending_addr <= mem_req_addr;
                resp_beats <= 0;
                $display("TB mem accept rw=%0b addr=%08h @%0t", mem_req_rw, mem_req_addr, $time);
            end else if (mem_pending > 0) begin
                mem_pending <= mem_pending - 1;
                if (mem_pending == 1) begin
                    resp_beats <= 8; // 64B line / 8B beat
                    mem_resp_valid <= 1'b1; // pulse first beat
                    mem_resp_data <= 64'hDEAD_BEEF_DEAD_BEEF ^ {32'b0, mem_pending_addr} ^ resp_beats[7:0];
                end
            end else if (resp_beats > 0) begin
                mem_resp_valid <= 1'b1;
                mem_resp_data <= 64'hDEAD_BEEF_DEAD_BEEF ^ {32'b0, mem_pending_addr} ^ resp_beats[7:0];
                resp_beats <= resp_beats - 1;
                if (resp_beats == 1) begin
                    $display("TB mem_resp last beat addr=%08h data=%016h @%0t", mem_pending_addr, mem_resp_data, $time);
                end
            end
        end
    end

endmodule
