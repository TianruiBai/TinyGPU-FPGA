`timescale 1ns/1ps

module l1_l2_cache_tb;
    import mailbox_pkg::*;
    localparam int CLK_HZ = 50_000_000;
    localparam int AXI_DATA_BITS = 64;
    localparam int LINE_BYTES = 64;
    localparam int L1_SIZE_BYTES = 4096;
    localparam int L2_SIZE_BYTES = 16384;
    localparam int NUM_PORTS = 2;
    localparam bit CLUSTER_WRITEBACK = 1'b1; // set to 1 for WB mode

    logic clk;
    logic rst_n;

    // L1 port 0 signals
    logic l1_0_req_valid;
    logic [1:0] l1_0_req_type;
    logic [31:0] l1_0_req_addr;
    logic [127:0] l1_0_req_wdata;
    logic [LINE_BYTES/8-1:0] l1_0_req_wstrb;
    logic l1_0_req_is_vector;
    logic [3:0] l1_0_req_vec_wmask;
    logic [7:0] l1_0_req_id;
    logic l1_0_req_ready;

    logic l1_0_resp_valid;
    logic [127:0] l1_0_resp_data;
    logic [7:0] l1_0_resp_id;

    // L1 port 1 signals
    logic l1_1_req_valid;
    logic [1:0] l1_1_req_type;
    logic [31:0] l1_1_req_addr;
    logic [127:0] l1_1_req_wdata;
    logic [LINE_BYTES/8-1:0] l1_1_req_wstrb;
    logic l1_1_req_is_vector;
    logic [3:0] l1_1_req_vec_wmask;
    logic [7:0] l1_1_req_id;
    logic l1_1_req_ready;

    logic l1_1_resp_valid;
    logic [127:0] l1_1_resp_data;
    logic [7:0] l1_1_resp_id;

    // L1 mem interface to L2
    logic [NUM_PORTS-1:0]                    l2_req_valid;
    logic [NUM_PORTS-1:0]                    l2_req_rw;
    logic [NUM_PORTS-1:0][31:0]              l2_req_addr;
    logic [NUM_PORTS-1:0][7:0]               l2_req_size;
    logic [NUM_PORTS-1:0][3:0]               l2_req_qos;
    logic [NUM_PORTS-1:0][7:0]               l2_req_id;
    logic [NUM_PORTS-1:0][LINE_BYTES*8-1:0]  l2_req_wdata;
    logic [NUM_PORTS-1:0][LINE_BYTES/8-1:0]  l2_req_wstrb;
    logic [NUM_PORTS-1:0]                    l2_req_ready;

    logic [NUM_PORTS-1:0]                    l2_resp_valid;
    logic [NUM_PORTS-1:0][AXI_DATA_BITS-1:0] l2_resp_data;
    logic [NUM_PORTS-1:0][7:0]               l2_resp_id;

    // AXI to memory
    logic [31:0] m_axi_awaddr;
    logic [7:0]  m_axi_awlen;
    logic [2:0]  m_axi_awsize;
    logic [1:0]  m_axi_awburst;
    logic        m_axi_awvalid;
    logic        m_axi_awready;

    logic [AXI_DATA_BITS-1:0] m_axi_wdata;
    logic [AXI_DATA_BITS/8-1:0] m_axi_wstrb;
    logic        m_axi_wlast;
    logic        m_axi_wvalid;
    logic        m_axi_wready;

    logic        m_axi_bvalid;
    logic        m_axi_bready;

    logic [31:0] m_axi_araddr;
    logic [7:0]  m_axi_arlen;
    logic [2:0]  m_axi_arsize;
    logic [1:0]  m_axi_arburst;
    logic        m_axi_arvalid;
    logic        m_axi_arready;

    logic [AXI_DATA_BITS-1:0] m_axi_rdata;
    logic        m_axi_rvalid;
    logic        m_axi_rlast;
    logic        m_axi_rready;

    logic [31:0] rdata0;
    logic [31:0] rdata1;

    // Clock
    always #10 clk = ~clk;

    // Reset
    initial begin
        clk = 1'b0;
        rst_n = 1'b0;
        repeat (5) @(posedge clk);
        rst_n = 1'b1;
    end

    // L1 cache control
    logic l1_0_flush;
    logic l1_1_flush;
    logic l1_0_invalidate;
    logic l1_1_invalidate;

    // L1 caches
    l1_data_cache #(
        .L1_SIZE_BYTES(L1_SIZE_BYTES),
        .LINE_BYTES(LINE_BYTES),
        .AXI_DATA_BITS(AXI_DATA_BITS),
        .WRITEBACK(CLUSTER_WRITEBACK)
    ) u_l1_0 (
        .clk(clk),
        .rst_n(rst_n),
        .lsu0_req_valid(l1_0_req_valid),
        .lsu0_req_type(l1_0_req_type),
        .lsu0_req_addr(l1_0_req_addr),
        .lsu0_req_wdata(l1_0_req_wdata),
        .lsu0_req_wstrb(l1_0_req_wstrb),
        .lsu0_req_is_vector(l1_0_req_is_vector),
        .lsu0_req_vec_wmask(l1_0_req_vec_wmask),
        .lsu0_req_id(l1_0_req_id),
        .lsu0_req_ready(l1_0_req_ready),
        .lsu0_resp_valid(l1_0_resp_valid),
        .lsu0_resp_data(l1_0_resp_data),
        .lsu0_resp_id(l1_0_resp_id),
        .lsu0_resp_err(),
        .lsu1_req_valid(1'b0),
        .lsu1_req_type(2'b00),
        .lsu1_req_addr(32'h0),
        .lsu1_req_wdata('0),
        .lsu1_req_wstrb('0),
        .lsu1_req_is_vector(1'b0),
        .lsu1_req_vec_wmask('0),
        .lsu1_req_id(8'h0),
        .lsu1_req_ready(),
        .lsu1_resp_valid(),
        .lsu1_resp_data(),
        .lsu1_resp_id(),
        .lsu1_resp_err(),
        .lsu_tex_req_valid(1'b0),
        .lsu_tex_req_type(2'b00),
        .lsu_tex_req_addr(32'h0),
        .lsu_tex_req_wdata('0),
        .lsu_tex_req_wstrb('0),
        .lsu_tex_req_id(8'h0),
        .lsu_tex_req_ready(),
        .lsu_tex_resp_valid(),
        .lsu_tex_resp_data(),
        .lsu_tex_resp_id(),
        .lsu_tex_resp_err(),
        .cfg_flush(l1_0_flush),
        .cfg_invalidate(l1_0_invalidate),
        .mem_req_valid(l2_req_valid[0]),
        .mem_req_rw(l2_req_rw[0]),
        .mem_req_addr(l2_req_addr[0]),
        .mem_req_size(l2_req_size[0]),
        .mem_req_qos(l2_req_qos[0]),
        .mem_req_id(l2_req_id[0]),
        .mem_req_wdata(l2_req_wdata[0]),
        .mem_req_wstrb(l2_req_wstrb[0]),
        .mem_req_ready(l2_req_ready[0]),
        .mem_resp_valid(l2_resp_valid[0]),
        .mem_resp_data(l2_resp_data[0]),
        .mem_resp_id(l2_resp_id[0])
    );

    l1_data_cache #(
        .L1_SIZE_BYTES(L1_SIZE_BYTES),
        .LINE_BYTES(LINE_BYTES),
        .AXI_DATA_BITS(AXI_DATA_BITS),
        .WRITEBACK(CLUSTER_WRITEBACK)
    ) u_l1_1 (
        .clk(clk),
        .rst_n(rst_n),
        .lsu0_req_valid(l1_1_req_valid),
        .lsu0_req_type(l1_1_req_type),
        .lsu0_req_addr(l1_1_req_addr),
        .lsu0_req_wdata(l1_1_req_wdata),
        .lsu0_req_wstrb(l1_1_req_wstrb),
        .lsu0_req_is_vector(l1_1_req_is_vector),
        .lsu0_req_vec_wmask(l1_1_req_vec_wmask),
        .lsu0_req_id(l1_1_req_id),
        .lsu0_req_ready(l1_1_req_ready),
        .lsu0_resp_valid(l1_1_resp_valid),
        .lsu0_resp_data(l1_1_resp_data),
        .lsu0_resp_id(l1_1_resp_id),
        .lsu0_resp_err(),
        .lsu1_req_valid(1'b0),
        .lsu1_req_type(2'b00),
        .lsu1_req_addr(32'h0),
        .lsu1_req_wdata('0),
        .lsu1_req_wstrb('0),
        .lsu1_req_is_vector(1'b0),
        .lsu1_req_vec_wmask('0),
        .lsu1_req_id(8'h0),
        .lsu1_req_ready(),
        .lsu1_resp_valid(),
        .lsu1_resp_data(),
        .lsu1_resp_id(),
        .lsu1_resp_err(),
        .lsu_tex_req_valid(1'b0),
        .lsu_tex_req_type(2'b00),
        .lsu_tex_req_addr(32'h0),
        .lsu_tex_req_wdata('0),
        .lsu_tex_req_wstrb('0),
        .lsu_tex_req_id(8'h0),
        .lsu_tex_req_ready(),
        .lsu_tex_resp_valid(),
        .lsu_tex_resp_data(),
        .lsu_tex_resp_id(),
        .lsu_tex_resp_err(),
        .cfg_flush(l1_1_flush),
        .cfg_invalidate(l1_1_invalidate),
        .mem_req_valid(l2_req_valid[1]),
        .mem_req_rw(l2_req_rw[1]),
        .mem_req_addr(l2_req_addr[1]),
        .mem_req_size(l2_req_size[1]),
        .mem_req_qos(l2_req_qos[1]),
        .mem_req_id(l2_req_id[1]),
        .mem_req_wdata(l2_req_wdata[1]),
        .mem_req_wstrb(l2_req_wstrb[1]),
        .mem_req_ready(l2_req_ready[1]),
        .mem_resp_valid(l2_resp_valid[1]),
        .mem_resp_data(l2_resp_data[1]),
        .mem_resp_id(l2_resp_id[1])
    );

    // MailboxFabric control (unused in TB)
    logic mb_s_awvalid;
    logic mb_s_awready;
    logic [15:0] mb_s_awaddr;
    logic mb_s_wvalid;
    logic mb_s_wready;
    logic [31:0] mb_s_wdata;
    logic [3:0] mb_s_wstrb;
    mailbox_pkg::mailbox_tag_t mb_s_tag;
    logic mb_s_bready;
    logic mb_s_bvalid;
    logic mb_s_arvalid;
    logic mb_s_arready;
    logic [15:0] mb_s_araddr;
    logic mb_s_rvalid;
    logic mb_s_rready;
    logic [31:0] mb_s_rdata;

    // L2 cache
    l2_data_cache #(
        .L2_SIZE_BYTES(L2_SIZE_BYTES),
        .LINE_BYTES(LINE_BYTES),
        .ASSOCIATIVITY(4),
        .NUM_PORTS(NUM_PORTS),
        .AXI_ADDR_BITS(32),
        .AXI_DATA_BITS(AXI_DATA_BITS),
        .WRITEBACK(CLUSTER_WRITEBACK),
        .PARTITION_ENABLE(1'b1),
        .USE_MAILBOX_CTRL(1'b1)
    ) u_l2 (
        .clk(clk),
        .rst_n(rst_n),
        .l1_req_valid(l2_req_valid),
        .l1_req_rw(l2_req_rw),
        .l1_req_addr(l2_req_addr),
        .l1_req_size(l2_req_size),
        .l1_req_qos(l2_req_qos),
        .l1_req_id(l2_req_id),
        .l1_req_wdata(l2_req_wdata),
        .l1_req_wstrb(l2_req_wstrb),
        .l1_req_ready(l2_req_ready),
        .l1_resp_valid(l2_resp_valid),
        .l1_resp_data(l2_resp_data),
        .l1_resp_id(l2_resp_id),
        .mb_s_awvalid(mb_s_awvalid),
        .mb_s_awready(mb_s_awready),
        .mb_s_awaddr(mb_s_awaddr),
        .mb_s_wvalid(mb_s_wvalid),
        .mb_s_wready(mb_s_wready),
        .mb_s_wdata(mb_s_wdata),
        .mb_s_wstrb(mb_s_wstrb),
        .mb_s_tag(mb_s_tag),
        .mb_s_bready(mb_s_bready),
        .mb_s_bvalid(mb_s_bvalid),
        .mb_s_arvalid(mb_s_arvalid),
        .mb_s_arready(mb_s_arready),
        .mb_s_araddr(mb_s_araddr),
        .mb_s_rvalid(mb_s_rvalid),
        .mb_s_rready(mb_s_rready),
        .mb_s_rdata(mb_s_rdata),
        .m_axi_awaddr(m_axi_awaddr),
        .m_axi_awlen(m_axi_awlen),
        .m_axi_awsize(m_axi_awsize),
        .m_axi_awburst(m_axi_awburst),
        .m_axi_awvalid(m_axi_awvalid),
        .m_axi_awready(m_axi_awready),
        .m_axi_wdata(m_axi_wdata),
        .m_axi_wstrb(m_axi_wstrb),
        .m_axi_wlast(m_axi_wlast),
        .m_axi_wvalid(m_axi_wvalid),
        .m_axi_wready(m_axi_wready),
        .m_axi_bvalid(m_axi_bvalid),
        .m_axi_bready(m_axi_bready),
        .m_axi_araddr(m_axi_araddr),
        .m_axi_arlen(m_axi_arlen),
        .m_axi_arsize(m_axi_arsize),
        .m_axi_arburst(m_axi_arburst),
        .m_axi_arvalid(m_axi_arvalid),
        .m_axi_arready(m_axi_arready),
        .m_axi_rdata(m_axi_rdata),
        .m_axi_rvalid(m_axi_rvalid),
        .m_axi_rlast(m_axi_rlast),
        .m_axi_rready(m_axi_rready)
    );

    // Simple AXI memory model
    localparam int MEM_BYTES = 256 * 1024;
    logic [7:0] mem [0:MEM_BYTES-1];

    logic        aw_pending;
    logic [31:0] aw_addr;
    logic [7:0]  aw_len;
    logic [7:0]  aw_beat;

    logic        ar_pending;
    logic [31:0] ar_addr;
    logic [7:0]  ar_len;
    logic [7:0]  ar_beat;

    assign m_axi_awready = !aw_pending;
    assign m_axi_wready  = aw_pending;
    assign m_axi_bready  = 1'b1;

    assign m_axi_arready = !ar_pending;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            aw_pending <= 1'b0;
            aw_addr    <= 32'h0;
            aw_len     <= 8'h0;
            aw_beat    <= 8'h0;
            m_axi_bvalid <= 1'b0;
        end else begin
            if (m_axi_awvalid && m_axi_awready) begin
                aw_pending <= 1'b1;
                aw_addr    <= m_axi_awaddr;
                aw_len     <= m_axi_awlen;
                aw_beat    <= 8'h0;
            end

            if (m_axi_wvalid && m_axi_wready) begin
                for (int b = 0; b < (AXI_DATA_BITS/8); b = b + 1) begin
                    if (m_axi_wstrb[b]) begin
                        mem[aw_addr + (aw_beat * (AXI_DATA_BITS/8)) + b] <= m_axi_wdata[b*8 +: 8];
                    end
                end
                if (m_axi_wlast) begin
                    aw_pending  <= 1'b0;
                    m_axi_bvalid <= 1'b1;
                end else begin
                    aw_beat <= aw_beat + 1'b1;
                end
            end

            if (m_axi_bvalid) begin
                m_axi_bvalid <= 1'b0;
            end
        end
    end

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            ar_pending <= 1'b0;
            ar_addr    <= 32'h0;
            ar_len     <= 8'h0;
            ar_beat    <= 8'h0;
            m_axi_rvalid <= 1'b0;
            m_axi_rdata  <= '0;
        end else begin
            if (m_axi_arvalid && m_axi_arready) begin
                ar_pending <= 1'b1;
                ar_addr    <= m_axi_araddr;
                ar_len     <= m_axi_arlen;
                ar_beat    <= 8'h0;
                m_axi_rvalid <= 1'b0;
            end

            if (ar_pending && (!m_axi_rvalid || (m_axi_rvalid && m_axi_rready))) begin
                m_axi_rvalid <= 1'b1;
                for (int b = 0; b < (AXI_DATA_BITS/8); b = b + 1) begin
                    m_axi_rdata[b*8 +: 8] <= mem[ar_addr + (ar_beat * (AXI_DATA_BITS/8)) + b];
                end
                $display("%0t MEM R beat=%0d/%0d rlast=%0b", $time, ar_beat, ar_len, (ar_beat == ar_len));
                if (m_axi_rvalid && m_axi_rready) begin
                    if (ar_beat == ar_len) begin
                        ar_pending  <= 1'b0;
                        m_axi_rvalid <= 1'b0;
                    end else begin
                        ar_beat <= ar_beat + 1'b1;
                    end
                end
            end
        end
    end

    assign m_axi_rlast = ar_pending && m_axi_rvalid && (ar_beat == ar_len);

    // Simple LSU tasks
    task automatic l1_load(
        input int core,
        input [31:0] addr,
        output [31:0] data
    );
        int timeout;
        begin
            if (core == 0) begin
                l1_0_req_valid = 1'b1;
                l1_0_req_type  = 2'b00;
                l1_0_req_addr  = addr;
                l1_0_req_wdata = '0;
                l1_0_req_wstrb = '0;
                l1_0_req_is_vector = 1'b0;
                l1_0_req_vec_wmask = 4'b0001;
                l1_0_req_id    = 8'h01;
                timeout = 2000;
                while (!l1_0_req_ready && timeout > 0) begin
                    @(posedge clk); timeout--;
                end
                if (timeout == 0) begin
                    $error("Timeout waiting for l1_0_req_ready (load)");
                    $finish;
                end
                @(posedge clk);
                l1_0_req_valid = 1'b0;
                timeout = 2000;
                while (!l1_0_resp_valid && timeout > 0) begin
                    @(posedge clk); timeout--;
                end
                if (timeout == 0) begin
                    $error("Timeout waiting for l1_0_resp_valid (load)");
                    $finish;
                end
                data = l1_0_resp_data[31:0];
            end else begin
                l1_1_req_valid = 1'b1;
                l1_1_req_type  = 2'b00;
                l1_1_req_addr  = addr;
                l1_1_req_wdata = '0;
                l1_1_req_wstrb = '0;
                l1_1_req_is_vector = 1'b0;
                l1_1_req_vec_wmask = 4'b0001;
                l1_1_req_id    = 8'h02;
                timeout = 2000;
                while (!l1_1_req_ready && timeout > 0) begin
                    @(posedge clk); timeout--;
                end
                if (timeout == 0) begin
                    $error("Timeout waiting for l1_1_req_ready (load)");
                    $finish;
                end
                @(posedge clk);
                l1_1_req_valid = 1'b0;
                timeout = 2000;
                while (!l1_1_resp_valid && timeout > 0) begin
                    @(posedge clk); timeout--;
                end
                if (timeout == 0) begin
                    $error("Timeout waiting for l1_1_resp_valid (load)");
                    $finish;
                end
                data = l1_1_resp_data[31:0];
            end
        end
    endtask

    task automatic l1_store(
        input int core,
        input [31:0] addr,
        input [31:0] data
    );
        int timeout;
        begin
            if (core == 0) begin
                l1_0_req_valid = 1'b1;
                l1_0_req_type  = 2'b01;
                l1_0_req_addr  = addr;
                l1_0_req_wdata = {96'h0, data};
                l1_0_req_wstrb = {LINE_BYTES/8{1'b1}};
                l1_0_req_is_vector = 1'b0;
                l1_0_req_vec_wmask = 4'b0001;
                l1_0_req_id    = 8'h11;
                timeout = 2000;
                while (!l1_0_req_ready && timeout > 0) begin
                    @(posedge clk); timeout--;
                end
                if (timeout == 0) begin
                    $error("Timeout waiting for l1_0_req_ready (store)");
                    $finish;
                end
                @(posedge clk);
                l1_0_req_valid = 1'b0;
                if (!CLUSTER_WRITEBACK) begin
                    timeout = 2000;
                    while (!l2_resp_valid[0] && timeout > 0) begin
                        @(posedge clk); timeout--;
                    end
                    if (timeout == 0) begin
                        $error("Timeout waiting for l2_resp_valid[0] (store)");
                        $finish;
                    end
                end
            end else begin
                l1_1_req_valid = 1'b1;
                l1_1_req_type  = 2'b01;
                l1_1_req_addr  = addr;
                l1_1_req_wdata = {96'h0, data};
                l1_1_req_wstrb = {LINE_BYTES/8{1'b1}};
                l1_1_req_is_vector = 1'b0;
                l1_1_req_vec_wmask = 4'b0001;
                l1_1_req_id    = 8'h12;
                timeout = 2000;
                while (!l1_1_req_ready && timeout > 0) begin
                    @(posedge clk); timeout--;
                end
                if (timeout == 0) begin
                    $error("Timeout waiting for l1_1_req_ready (store)");
                    $finish;
                end
                @(posedge clk);
                l1_1_req_valid = 1'b0;
                if (!CLUSTER_WRITEBACK) begin
                    timeout = 2000;
                    while (!l2_resp_valid[1] && timeout > 0) begin
                        @(posedge clk); timeout--;
                    end
                    if (timeout == 0) begin
                        $error("Timeout waiting for l2_resp_valid[1] (store)");
                        $finish;
                    end
                end
            end
        end
    endtask

    task automatic l1_flush_all;
        begin
            l1_0_flush = 1'b1;
            l1_1_flush = 1'b1;
            @(posedge clk);
            l1_0_flush = 1'b0;
            l1_1_flush = 1'b0;
        end
    endtask

    task automatic l1_invalidate_all;
        begin
            l1_0_invalidate = 1'b1;
            l1_1_invalidate = 1'b1;
            @(posedge clk);
            l1_0_invalidate = 1'b0;
            l1_1_invalidate = 1'b0;
        end
    endtask

    task automatic wait_l1_wb_drain;
        int timeout;
        begin
            timeout = 5000;
            while (timeout > 0) begin
                @(posedge clk);
                if (l2_resp_valid[0] && l2_resp_id[0] == 8'hFF) begin
                    break;
                end
                if (l2_resp_valid[1] && l2_resp_id[1] == 8'hFF) begin
                    break;
                end
                timeout--;
            end
            if (timeout == 0) begin
                $error("Timeout waiting for L1 WB drain");
            end
        end
    endtask

    task automatic l2_read_status(output logic [31:0] status);
        int timeout;
        begin
            mb_s_araddr  = 16'h0000;
            mb_s_arvalid = 1'b1;
            timeout = 2000;
            while (!mb_s_arready && timeout > 0) begin
                @(posedge clk); timeout--;
            end
            @(posedge clk);
            mb_s_arvalid = 1'b0;
            timeout = 2000;
            while (!mb_s_rvalid && timeout > 0) begin
                @(posedge clk); timeout--;
            end
            status = mb_s_rdata;
        end
    endtask

    task automatic wait_l2_flush_done;
        logic [31:0] status;
        int timeout;
        begin
            timeout = 20000;
            while (timeout > 0) begin
                l2_read_status(status);
                if (status[4] == 1'b1) begin
                    break;
                end
                timeout--;
            end
            if (timeout == 0) begin
                $error("Timeout waiting for L2 flush start");
            end

            timeout = 20000;
            while (timeout > 0) begin
                l2_read_status(status);
                if (status[4] == 1'b0) begin
                    break;
                end
                timeout--;
            end
            if (timeout == 0) begin
                $error("Timeout waiting for L2 flush done");
            end
        end
    endtask

    task automatic l2_flush_mailbox;
        begin
            mb_s_awaddr  = 16'h0000;
            mb_s_wdata   = 32'h0000_0005; // bit0 enable + bit2 flush
            mb_s_wstrb   = 4'hF;
            mb_s_awvalid = 1'b1;
            mb_s_wvalid  = 1'b1;
            wait (mb_s_awready && mb_s_wready);
            @(posedge clk);
            mb_s_awvalid = 1'b0;
            mb_s_wvalid  = 1'b0;
        end
    endtask

    // Test sequence
    initial begin
        mb_s_awvalid = 1'b0;
        mb_s_awaddr  = 16'h0;
        mb_s_wvalid  = 1'b0;
        mb_s_wdata   = 32'h0;
        mb_s_wstrb   = 4'h0;
        mb_s_tag     = '0;
        mb_s_bready  = 1'b1;
        mb_s_arvalid = 1'b0;
        mb_s_araddr  = 16'h0;
        mb_s_rready  = 1'b1;
        l1_0_flush = 1'b0;
        l1_1_flush = 1'b0;
        l1_0_invalidate = 1'b0;
        l1_1_invalidate = 1'b0;
        l1_0_req_valid = 1'b0;
        l1_1_req_valid = 1'b0;
        l1_0_req_type  = 2'b00;
        l1_1_req_type  = 2'b00;
        l1_0_req_addr  = 32'h0;
        l1_1_req_addr  = 32'h0;
        l1_0_req_wdata = '0;
        l1_1_req_wdata = '0;
        l1_0_req_wstrb = '0;
        l1_1_req_wstrb = '0;
        l1_0_req_is_vector = 1'b0;
        l1_1_req_is_vector = 1'b0;
        l1_0_req_vec_wmask = 4'b0000;
        l1_1_req_vec_wmask = 4'b0000;
        l1_0_req_id    = 8'h0;
        l1_1_req_id    = 8'h0;

        wait (rst_n == 1'b1);
        repeat (5) @(posedge clk);

        begin
            logic [31:0] status;
            l2_read_status(status);
            $display("L2 status: flush_active=%0b reg_enable=%0b l2_enable=%0b", status[4], status[3], status[2]);
        end

        // Initialize memory word at 0x0000_0100
        mem[32'h100] = 8'hAA;
        mem[32'h101] = 8'hBB;
        mem[32'h102] = 8'hCC;
        mem[32'h103] = 8'hDD;

        // Core0 load, then store, Core1 load to verify
        l1_load(0, 32'h0000_0100, rdata0);
        l1_store(0, 32'h0000_0100, 32'h11223344);
        l1_load(0, 32'h0000_0100, rdata0);
        $display("Core0 read after store = %08h", rdata0);

        if (CLUSTER_WRITEBACK) begin
            // In WB, other core may see old data until flush
            l1_load(1, 32'h0000_0100, rdata1);
            $display("WB mode: core1 read before flush = %08h", rdata1);
            l1_flush_all();
            wait_l1_wb_drain();
            l2_flush_mailbox();
            begin
                logic [31:0] status;
                l2_read_status(status);
                $display("L2 status after flush cmd: flush_active=%0b", status[4]);
            end
            wait_l2_flush_done();
            l1_invalidate_all();
            l1_load(1, 32'h0000_0100, rdata1);
            if (rdata1 !== 32'h11223344) begin
                $error("WB mode: expected 11223344 after flush, got %08h", rdata1);
            end else begin
                $display("WB mode: core1 read after flush = %08h", rdata1);
            end
        end else begin
            l1_load(1, 32'h0000_0100, rdata1);
            if (rdata1 !== 32'h11223344) begin
                $error("WT mode: expected 11223344, got %08h", rdata1);
            end else begin
                $display("WT mode: core1 read after store = %08h", rdata1);
            end
        end

        $display("L1-L2 cache TB finished.");
        $finish;
    end

    // Debug: trace AXI transactions
    always_ff @(posedge clk) begin
        if (l2_req_valid[0] && l2_req_ready[0]) begin
            $display("%0t L2 req0 rw=%0b addr=%08h id=%0d qos=%0d data[31:0]=%08h", $time, l2_req_rw[0], l2_req_addr[0], l2_req_id[0], l2_req_qos[0], l2_req_wdata[0][31:0]);
        end
        if (l2_req_valid[1] && l2_req_ready[1]) begin
            $display("%0t L2 req1 rw=%0b addr=%08h id=%0d qos=%0d", $time, l2_req_rw[1], l2_req_addr[1], l2_req_id[1], l2_req_qos[1]);
        end
        if (l2_resp_valid[0]) begin
            $display("%0t L2 resp0 id=%0d", $time, l2_resp_id[0]);
        end
        if (l2_resp_valid[1]) begin
            $display("%0t L2 resp1 id=%0d", $time, l2_resp_id[1]);
        end
        if (m_axi_awvalid && m_axi_awready) begin
            $display("%0t AXI AW addr=%08h len=%0d", $time, m_axi_awaddr, m_axi_awlen);
        end
        if (m_axi_wvalid && m_axi_wready) begin
            $display("%0t AXI W last=%0b data[31:0]=%08h", $time, m_axi_wlast, m_axi_wdata[31:0]);
        end
        if (m_axi_arvalid && m_axi_arready) begin
            $display("%0t AXI AR addr=%08h len=%0d", $time, m_axi_araddr, m_axi_arlen);
        end
        if (m_axi_rvalid && m_axi_rready) begin
            $display("%0t AXI R last=%0b", $time, m_axi_rlast);
        end
        if (mb_s_bvalid) begin
            $display("%0t L2 mailbox write ack", $time);
        end
    end

    // Timeout watchdog
    integer sim_cycles;
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            sim_cycles <= 0;
        end else begin
            sim_cycles <= sim_cycles + 1;
            if (sim_cycles > 50000) begin
                $error("Simulation timeout");
                $finish;
            end
        end
    end

endmodule
