// Cluster-shared L2 Data Cache (4-way set associative, multi-port)
// - Configurable size: 16KB-128KB
// - Cluster-wide WT/WB policy (no mixing)
// - AXI4 master interface to system memory
// - QoS-aware arbitration + optional partitioning by core/port
// - Optional MailboxFabric control/status interface (AXI-Lite slave)

module l2_data_cache #(
    parameter int L2_ENABLED       = 1,
    parameter int L2_SIZE_BYTES    = 16384,
    parameter int LINE_BYTES       = 64,
    parameter int ASSOCIATIVITY    = 4,
    parameter int NUM_PORTS        = 2,
    parameter int AXI_ADDR_BITS    = 32,
    parameter int AXI_DATA_BITS    = 64,
    parameter bit WRITEBACK        = 0,
    parameter bit PARTITION_ENABLE = 1'b1,
    parameter bit USE_MAILBOX_CTRL = 1'b0
) (
    input  logic clk,
    input  logic rst_n,

    // L1->L2 request ports (mem_req style)
    input  logic [NUM_PORTS-1:0]                    l1_req_valid,
    input  logic [NUM_PORTS-1:0]                    l1_req_rw,
    input  logic [NUM_PORTS-1:0][31:0]              l1_req_addr,
    input  logic [NUM_PORTS-1:0][7:0]               l1_req_size,
    input  logic [NUM_PORTS-1:0][3:0]               l1_req_qos,
    input  logic [NUM_PORTS-1:0][7:0]               l1_req_id,
    input  logic [NUM_PORTS-1:0][LINE_BYTES*8-1:0]  l1_req_wdata,
    input  logic [NUM_PORTS-1:0][LINE_BYTES/8-1:0]  l1_req_wstrb,
    output logic [NUM_PORTS-1:0]                    l1_req_ready,

    output logic [NUM_PORTS-1:0]                    l1_resp_valid,
    output logic [NUM_PORTS-1:0][AXI_DATA_BITS-1:0] l1_resp_data,
    output logic [NUM_PORTS-1:0][7:0]               l1_resp_id,

    // MailboxFabric control interface (AXI-Lite slave)
    input  logic                                    mb_s_awvalid,
    output logic                                    mb_s_awready,
    input  logic [15:0]                             mb_s_awaddr,
    input  logic                                    mb_s_wvalid,
    output logic                                    mb_s_wready,
    input  logic [31:0]                             mb_s_wdata,
    input  logic [3:0]                              mb_s_wstrb,
    input  mailbox_pkg::mailbox_tag_t              mb_s_tag,
    input  logic                                    mb_s_bready,
    output logic                                    mb_s_bvalid,

    input  logic                                    mb_s_arvalid,
    output logic                                    mb_s_arready,
    input  logic [15:0]                             mb_s_araddr,
    output logic                                    mb_s_rvalid,
    input  logic                                    mb_s_rready,
    output logic [31:0]                             mb_s_rdata,

    // AXI4 master (read+write)
    output logic [AXI_ADDR_BITS-1:0]                m_axi_awaddr,
    output logic [7:0]                              m_axi_awlen,
    output logic [2:0]                              m_axi_awsize,
    output logic [1:0]                              m_axi_awburst,
    output logic                                    m_axi_awvalid,
    input  logic                                    m_axi_awready,

    output logic [AXI_DATA_BITS-1:0]                m_axi_wdata,
    output logic [AXI_DATA_BITS/8-1:0]              m_axi_wstrb,
    output logic                                    m_axi_wlast,
    output logic                                    m_axi_wvalid,
    input  logic                                    m_axi_wready,

    input  logic                                    m_axi_bvalid,
    output logic                                    m_axi_bready,

    output logic [AXI_ADDR_BITS-1:0]                m_axi_araddr,
    output logic [7:0]                              m_axi_arlen,
    output logic [2:0]                              m_axi_arsize,
    output logic [1:0]                              m_axi_arburst,
    output logic                                    m_axi_arvalid,
    input  logic                                    m_axi_arready,

    input  logic [AXI_DATA_BITS-1:0]                m_axi_rdata,
    input  logic                                    m_axi_rvalid,
    input  logic                                    m_axi_rlast,
    output logic                                    m_axi_rready
);
    import mailbox_pkg::*;

    localparam int NUM_SETS    = L2_SIZE_BYTES / (LINE_BYTES * ASSOCIATIVITY);
    localparam int OFFSET_BITS = $clog2(LINE_BYTES);
    localparam int INDEX_BITS  = $clog2(NUM_SETS);
    localparam int TAG_BITS    = 32 - INDEX_BITS - OFFSET_BITS;

    localparam int BEAT_BYTES  = AXI_DATA_BITS / 8;
    localparam int LINE_BEATS  = (LINE_BYTES + BEAT_BYTES - 1) / BEAT_BYTES;
    localparam int BEAT_CNT_W  = (LINE_BEATS <= 1) ? 1 : $clog2(LINE_BEATS);

    localparam int WAYS_PER_PORT_RAW = (NUM_PORTS == 0) ? ASSOCIATIVITY : (ASSOCIATIVITY / NUM_PORTS);
    localparam bit PARTITION_ACTIVE = PARTITION_ENABLE && (ASSOCIATIVITY % NUM_PORTS == 0);
    localparam int WAYS_PER_PORT = PARTITION_ACTIVE ? WAYS_PER_PORT_RAW : ASSOCIATIVITY;

    typedef struct packed {
        logic               valid;
        logic               dirty;
        logic [TAG_BITS-1:0] tag;
    } l2_tag_t;

    l2_tag_t tag_ram [0:NUM_SETS-1][0:ASSOCIATIVITY-1];
    logic [LINE_BYTES*8-1:0] data_ram [0:NUM_SETS-1][0:ASSOCIATIVITY-1];

    // Replacement (round-robin per set, per port partition)
    logic [$clog2(WAYS_PER_PORT)-1:0] rr_way [0:NUM_SETS-1][0:NUM_PORTS-1];

    // Arbitration
    logic [$clog2(NUM_PORTS)-1:0] rr_ptr;
    logic [$clog2(NUM_PORTS)-1:0] sel_port;
    logic                        sel_valid;
    logic [3:0]                  best_qos;

    // Pending request
    logic                        pend_rw;
    logic [31:0]                 pend_addr;
    logic [7:0]                  pend_id;
    logic [$clog2(NUM_PORTS)-1:0] pend_port;
    logic [LINE_BYTES*8-1:0]     pend_wdata;
    logic [LINE_BYTES/8-1:0]     pend_wstrb;
    logic [$clog2(ASSOCIATIVITY)-1:0] pend_way;

    wire [INDEX_BITS-1:0] pend_set = pend_addr[OFFSET_BITS +: INDEX_BITS];
    wire [TAG_BITS-1:0]   pend_tag = pend_addr[31 -: TAG_BITS];

    // Response sequencing
    logic [LINE_BYTES*8-1:0] resp_line;
    logic [BEAT_CNT_W-1:0]   resp_beat;
    logic [BEAT_CNT_W-1:0]   resp_total_beats;
    logic                    resp_active;

    // AXI transaction controls
    logic [BEAT_CNT_W-1:0]   axi_beat_cnt;
    logic [LINE_BYTES*8-1:0] axi_line_buf;
    logic [AXI_ADDR_BITS-1:0] axi_addr;

    // Mailbox control regs
    logic reg_enable;
    logic reg_invalidate_pulse;
    logic reg_flush_pulse;
    logic mb_bvalid_r;
    logic mb_rvalid_r;
    logic [31:0] mb_rdata_r;

    // Counters
    logic [31:0] cnt_hits, cnt_misses, cnt_evictions, cnt_refills, cnt_wb;

    // Flush state
    logic flush_active;
    logic [INDEX_BITS-1:0] flush_set;
    logic [$clog2(ASSOCIATIVITY)-1:0] flush_way;

    wire l2_enable = (L2_ENABLED != 0) && (!USE_MAILBOX_CTRL || reg_enable);

    // Request selection with QoS priority
    integer p_sel;
    integer idx_sel;
    always_comb begin
        sel_valid = 1'b0;
        sel_port  = rr_ptr;
        best_qos  = 4'h0;

        for (p_sel = 0; p_sel < NUM_PORTS; p_sel = p_sel + 1) begin
            if (l1_req_valid[p_sel] && (l1_req_qos[p_sel] > best_qos)) begin
                best_qos = l1_req_qos[p_sel];
            end
        end

        for (p_sel = 0; p_sel < NUM_PORTS; p_sel = p_sel + 1) begin
            idx_sel = (int'(rr_ptr) + p_sel) % NUM_PORTS;
            if (!sel_valid && l1_req_valid[idx_sel] && (l1_req_qos[idx_sel] == best_qos)) begin
                sel_valid = 1'b1;
                sel_port  = idx_sel[$clog2(NUM_PORTS)-1:0];
            end
        end
    end

    // Hit detection within partition
    function automatic logic [ASSOCIATIVITY-1:0] way_mask_for_port(input int port);
        logic [ASSOCIATIVITY-1:0] mask;
        int w;
        int base;
        begin
            mask = '0;
            if (PARTITION_ACTIVE) begin
                base = port * WAYS_PER_PORT;
                for (w = 0; w < WAYS_PER_PORT; w = w + 1) begin
                    mask[base + w] = 1'b1;
                end
            end else begin
                mask = {ASSOCIATIVITY{1'b1}};
            end
            way_mask_for_port = mask;
        end
    endfunction

    integer p_resp;
    always_comb begin
        for (p_resp = 0; p_resp < NUM_PORTS; p_resp = p_resp + 1) begin
            l1_req_ready[p_resp]  = 1'b0;
            l1_resp_valid[p_resp] = 1'b0;
            l1_resp_data[p_resp]  = '0;
            l1_resp_id[p_resp]    = '0;
        end

        if (resp_active) begin
            l1_resp_valid[pend_port] = 1'b1;
            l1_resp_data[pend_port]  = resp_line[resp_beat*AXI_DATA_BITS +: AXI_DATA_BITS];
            l1_resp_id[pend_port]    = pend_id;
        end

        if (!resp_active && sel_valid && l2_enable && !flush_active) begin
            l1_req_ready[sel_port] = 1'b1;
        end
    end

    // AXI defaults
    assign m_axi_awburst = 2'b01;
    assign m_axi_arburst = 2'b01;
    assign m_axi_awsize  = (AXI_DATA_BITS == 64) ? 3'd3 : 3'd2;
    assign m_axi_arsize  = (AXI_DATA_BITS == 64) ? 3'd3 : 3'd2;
    assign m_axi_awlen   = LINE_BEATS[7:0] - 1'b1;
    assign m_axi_arlen   = LINE_BEATS[7:0] - 1'b1;
    assign m_axi_bready  = 1'b1;

    // State machine
    typedef enum logic [3:0] {
        ST_IDLE,
        ST_FLUSH_EVICT_AW,
        ST_FLUSH_EVICT_W,
        ST_FLUSH_EVICT_B,
        ST_FLUSH_ADV,
        ST_EVICT_AW,
        ST_EVICT_W,
        ST_EVICT_B,
        ST_READ_AR,
        ST_READ_R,
        ST_REFILL_DONE,
        ST_WT_AW,
        ST_WT_W,
        ST_WT_B,
        ST_RESPOND
    } state_t;

    state_t state;

    // Helpers
    function automatic [LINE_BYTES*8-1:0] merge_line(
        input [LINE_BYTES*8-1:0] old_line,
        input [LINE_BYTES*8-1:0] new_line,
        input [LINE_BYTES/8-1:0] wstrb
    );
        integer b;
        begin
            merge_line = old_line;
            for (b = 0; b < LINE_BYTES; b = b + 1) begin
                if (wstrb[b]) begin
                    merge_line[b*8 +: 8] = new_line[b*8 +: 8];
                end
            end
        end
    endfunction

    function automatic logic [$clog2(ASSOCIATIVITY)-1:0] select_victim_way(
        input int port,
        input int set_idx
    );
        int base;
        int tmp;
        logic [$clog2(ASSOCIATIVITY)-1:0] way;
        begin
            if (PARTITION_ACTIVE) begin
                base = port * WAYS_PER_PORT;
                tmp = base + int'(rr_way[set_idx][port]);
                way = tmp[$clog2(ASSOCIATIVITY)-1:0];
            end else begin
                tmp = int'(rr_way[set_idx][0]);
                way = tmp[$clog2(ASSOCIATIVITY)-1:0];
            end
            select_victim_way = way;
        end
    endfunction

    // Mailbox control (simple CSR)
    assign mb_s_awready = !mb_bvalid_r;
    assign mb_s_wready  = !mb_bvalid_r;
    assign mb_s_bvalid  = mb_bvalid_r;
    assign mb_s_arready = !mb_rvalid_r;
    assign mb_s_rvalid  = mb_rvalid_r;
    assign mb_s_rdata   = mb_rdata_r;

    // Tag input unused (compat)
    wire unused_mb_tag = ^mb_s_tag;

    // Main FSM + control
    integer i, w;
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state        <= ST_IDLE;
            rr_ptr       <= '0;
            resp_active  <= 1'b0;
            resp_beat    <= '0;
            resp_total_beats <= '0;
            pend_port    <= '0;
            pend_rw      <= 1'b0;
            pend_addr    <= 32'h0;
            pend_id      <= 8'h0;
            pend_wdata   <= '0;
            pend_wstrb   <= '0;
            pend_way     <= '0;
            axi_beat_cnt  <= '0;
            axi_line_buf  <= '0;
            axi_addr      <= '0;
            m_axi_awvalid <= 1'b0;
            m_axi_wvalid  <= 1'b0;
            m_axi_wlast   <= 1'b0;
            m_axi_arvalid <= 1'b0;
            m_axi_rready  <= 1'b0;
            reg_enable    <= 1'b1;
            reg_invalidate_pulse <= 1'b0;
            reg_flush_pulse <= 1'b0;
            mb_bvalid_r   <= 1'b0;
            mb_rvalid_r   <= 1'b0;
            mb_rdata_r    <= 32'h0;
            cnt_hits      <= 32'h0;
            cnt_misses    <= 32'h0;
            cnt_evictions <= 32'h0;
            cnt_refills   <= 32'h0;
            cnt_wb        <= 32'h0;
            flush_active  <= 1'b0;
            flush_set     <= '0;
            flush_way     <= '0;
            for (i = 0; i < NUM_SETS; i = i + 1) begin
                for (w = 0; w < ASSOCIATIVITY; w = w + 1) begin
                    tag_ram[i][w].valid <= 1'b0;
                    tag_ram[i][w].dirty <= 1'b0;
                    tag_ram[i][w].tag   <= '0;
                    data_ram[i][w]      <= '0;
                end
                for (w = 0; w < NUM_PORTS; w = w + 1) begin
                    rr_way[i][w] <= '0;
                end
            end
        end else begin
            // Default strobes
            m_axi_awvalid <= m_axi_awvalid;
            m_axi_wvalid  <= m_axi_wvalid;
            m_axi_wlast   <= m_axi_wlast;
            m_axi_arvalid <= m_axi_arvalid;
            m_axi_rready  <= m_axi_rready;
            reg_invalidate_pulse <= 1'b0;
            reg_flush_pulse <= 1'b0;

            if (mb_bvalid_r && mb_s_bready) mb_bvalid_r <= 1'b0;
            if (mb_rvalid_r && mb_s_rready) mb_rvalid_r <= 1'b0;

            if (USE_MAILBOX_CTRL && mb_s_awvalid && mb_s_wvalid && mb_s_awready) begin
                mb_bvalid_r <= 1'b1;
                if (mb_s_awaddr[5:2] == 4'h0) begin
                    if (mb_s_wstrb[0]) begin
                        reg_enable <= mb_s_wdata[0];
                        if (mb_s_wdata[1]) reg_invalidate_pulse <= 1'b1;
                        if (mb_s_wdata[2]) reg_flush_pulse <= 1'b1;
                    end
                end
            end

            if (USE_MAILBOX_CTRL && mb_s_arvalid && mb_s_arready) begin
                mb_rvalid_r <= 1'b1;
                case (mb_s_araddr[5:2])
                    4'h0: mb_rdata_r <= {27'h0, flush_active, reg_enable, l2_enable, 2'b00};
                    4'h1: mb_rdata_r <= cnt_hits;
                    4'h2: mb_rdata_r <= cnt_misses;
                    4'h3: mb_rdata_r <= cnt_evictions;
                    4'h4: mb_rdata_r <= cnt_refills;
                    4'h5: mb_rdata_r <= cnt_wb;
                    default: mb_rdata_r <= 32'h0;
                endcase
            end

            if (reg_invalidate_pulse) begin
                for (i = 0; i < NUM_SETS; i = i + 1) begin
                    for (w = 0; w < ASSOCIATIVITY; w = w + 1) begin
                        tag_ram[i][w].valid <= 1'b0;
                        tag_ram[i][w].dirty <= 1'b0;
                    end
                end
            end

            if (reg_flush_pulse && WRITEBACK) begin
                flush_active <= 1'b1;
                flush_set <= '0;
                flush_way <= '0;
            end else if (reg_flush_pulse && !WRITEBACK) begin
                for (i = 0; i < NUM_SETS; i = i + 1) begin
                    for (w = 0; w < ASSOCIATIVITY; w = w + 1) begin
                        tag_ram[i][w].valid <= 1'b0;
                        tag_ram[i][w].dirty <= 1'b0;
                    end
                end
            end

            case (state)
                ST_IDLE: begin
                    resp_active <= 1'b0;
                    resp_beat   <= '0;
                    m_axi_awvalid <= 1'b0;
                    m_axi_wvalid  <= 1'b0;
                    m_axi_wlast   <= 1'b0;
                    m_axi_arvalid <= 1'b0;
                    m_axi_rready  <= 1'b0;

                    if (flush_active) begin
                        if (tag_ram[flush_set][flush_way].valid && tag_ram[flush_set][flush_way].dirty) begin
                            axi_line_buf <= data_ram[flush_set][flush_way];
                            axi_addr     <= {tag_ram[flush_set][flush_way].tag, flush_set, {OFFSET_BITS{1'b0}}};
                            axi_beat_cnt <= '0;
                            state        <= ST_FLUSH_EVICT_AW;
                                $display("%0t L2 flush dirty set=%0d way=%0d", $time, flush_set, flush_way);
                        end else begin
                            tag_ram[flush_set][flush_way].valid <= 1'b0;
                            tag_ram[flush_set][flush_way].dirty <= 1'b0;
                            state <= ST_FLUSH_ADV;
                        end
                    end else if (sel_valid && l2_enable) begin
                        logic [31:0] sel_addr;
                        logic [INDEX_BITS-1:0] sel_set;
                        logic [TAG_BITS-1:0]   sel_tag;
                        logic [ASSOCIATIVITY-1:0] sel_way_mask;
                        logic hit;
                        logic [$clog2(ASSOCIATIVITY)-1:0] hit_way;
                        int vway;

                        sel_addr = l1_req_addr[sel_port];
                        sel_set  = sel_addr[OFFSET_BITS +: INDEX_BITS];
                        sel_tag  = sel_addr[31 -: TAG_BITS];
                        sel_way_mask = way_mask_for_port(int'(sel_port));
                        hit = 1'b0;
                        hit_way = '0;

                        for (vway = 0; vway < ASSOCIATIVITY; vway = vway + 1) begin
                            if (sel_way_mask[vway] && tag_ram[sel_set][vway].valid && (tag_ram[sel_set][vway].tag == sel_tag)) begin
                                hit = 1'b1;
                                hit_way = vway[$clog2(ASSOCIATIVITY)-1:0];
                            end
                        end

                        pend_port  <= sel_port;
                        pend_rw    <= l1_req_rw[sel_port];
                        pend_addr  <= sel_addr;
                        pend_id    <= l1_req_id[sel_port];
                        pend_wdata <= l1_req_wdata[sel_port];
                        pend_wstrb <= l1_req_wstrb[sel_port];
                        rr_ptr     <= sel_port + 1'b1;

                        if (hit) begin
                            cnt_hits <= cnt_hits + 1'b1;
                            pend_way <= hit_way;
                            if (l1_req_rw[sel_port] == 1'b0) begin
                                resp_line        <= data_ram[sel_set][hit_way];
                                resp_total_beats <= BEAT_CNT_W'(LINE_BEATS);
                                resp_active <= 1'b1;
                                state       <= ST_RESPOND;
                            end else begin
                                data_ram[sel_set][hit_way] <= merge_line(data_ram[sel_set][hit_way], l1_req_wdata[sel_port], l1_req_wstrb[sel_port]);
                                if (WRITEBACK) begin
                                    tag_ram[sel_set][hit_way].dirty <= 1'b1;
                                    $display("%0t L2 write hit set=%0d way=%0d", $time, sel_set, hit_way);
                                    resp_active       <= 1'b1;
                                    resp_line         <= '0;
                                    resp_total_beats  <= BEAT_CNT_W'(1);
                                    state       <= ST_RESPOND;
                                end else begin
                                    axi_line_buf <= merge_line(data_ram[sel_set][hit_way], l1_req_wdata[sel_port], l1_req_wstrb[sel_port]);
                                    axi_addr     <= {sel_addr[31:OFFSET_BITS], {OFFSET_BITS{1'b0}}};
                                    axi_beat_cnt <= '0;
                                    state        <= ST_WT_AW;
                                end
                            end
                        end else begin
                            cnt_misses <= cnt_misses + 1'b1;
                            pend_way <= select_victim_way(int'(sel_port), int'(sel_set));
                            if (WRITEBACK && tag_ram[sel_set][pend_way].valid && tag_ram[sel_set][pend_way].dirty) begin
                                axi_line_buf <= data_ram[sel_set][pend_way];
                                axi_addr     <= {tag_ram[sel_set][pend_way].tag, sel_set, {OFFSET_BITS{1'b0}}};
                                axi_beat_cnt <= '0;
                                state        <= ST_EVICT_AW;
                                cnt_evictions <= cnt_evictions + 1'b1;
                            end else if (l1_req_rw[sel_port] && (&l1_req_wstrb[sel_port])) begin
                                // Write-allocate without read (full line provided)
                                data_ram[sel_set][pend_way] <= l1_req_wdata[sel_port];
                                tag_ram[sel_set][pend_way].valid <= 1'b1;
                                tag_ram[sel_set][pend_way].tag   <= sel_tag;
                                tag_ram[sel_set][pend_way].dirty <= WRITEBACK ? 1'b1 : 1'b0;

                                if (WRITEBACK) begin
                                    $display("%0t L2 write allocate set=%0d way=%0d", $time, sel_set, pend_way);
                                    resp_active      <= 1'b1;
                                    resp_line        <= '0;
                                    resp_total_beats <= BEAT_CNT_W'(1);
                                    state       <= ST_RESPOND;
                                end else begin
                                    axi_line_buf <= l1_req_wdata[sel_port];
                                    axi_addr     <= {sel_addr[31:OFFSET_BITS], {OFFSET_BITS{1'b0}}};
                                    axi_beat_cnt <= '0;
                                    state        <= ST_WT_AW;
                                end
                            end else begin
                                axi_addr     <= {sel_addr[31:OFFSET_BITS], {OFFSET_BITS{1'b0}}};
                                axi_beat_cnt <= '0;
                                state        <= ST_READ_AR;
                            end
                        end
                    end
                end

                ST_FLUSH_EVICT_AW: begin
                    m_axi_awvalid <= 1'b1;
                    m_axi_awaddr  <= axi_addr;
                    if (m_axi_awvalid && m_axi_awready) begin
                        m_axi_awvalid <= 1'b0;
                        m_axi_wvalid  <= 1'b1;
                        m_axi_wlast   <= (LINE_BEATS == 1);
                        state         <= ST_FLUSH_EVICT_W;
                    end
                end

                ST_FLUSH_EVICT_W: begin
                    if (m_axi_wvalid && m_axi_wready) begin
                        if (axi_beat_cnt == BEAT_CNT_W'(LINE_BEATS-1)) begin
                            m_axi_wvalid <= 1'b0;
                            m_axi_wlast  <= 1'b0;
                            state        <= ST_FLUSH_EVICT_B;
                        end else begin
                            axi_beat_cnt <= axi_beat_cnt + 1'b1;
                            m_axi_wlast  <= (axi_beat_cnt == BEAT_CNT_W'(LINE_BEATS-2));
                        end
                    end
                end

                ST_FLUSH_EVICT_B: begin
                    if (m_axi_bvalid) begin
                        tag_ram[flush_set][flush_way].dirty <= 1'b0;
                        tag_ram[flush_set][flush_way].valid <= 1'b0;
                        cnt_wb <= cnt_wb + 1'b1;
                        state <= ST_FLUSH_ADV;
                    end
                end

                ST_FLUSH_ADV: begin
                    if (int'(flush_way) == (ASSOCIATIVITY-1)) begin
                        flush_way <= '0;
                        if (int'(flush_set) == (NUM_SETS-1)) begin
                            flush_set <= '0;
                            flush_active <= 1'b0;
                            state <= ST_IDLE;
                        end else begin
                            flush_set <= flush_set + 1'b1;
                            state <= ST_IDLE;
                        end
                    end else begin
                        flush_way <= flush_way + 1'b1;
                        state <= ST_IDLE;
                    end
                end

                ST_EVICT_AW: begin
                    m_axi_awvalid <= 1'b1;
                    m_axi_awaddr  <= axi_addr;
                    if (m_axi_awvalid && m_axi_awready) begin
                        m_axi_awvalid <= 1'b0;
                        m_axi_wvalid  <= 1'b1;
                        m_axi_wlast   <= (LINE_BEATS == 1);
                        state         <= ST_EVICT_W;
                    end
                end

                ST_EVICT_W: begin
                    if (m_axi_wvalid && m_axi_wready) begin
                        if (axi_beat_cnt == BEAT_CNT_W'(LINE_BEATS-1)) begin
                            m_axi_wvalid <= 1'b0;
                            m_axi_wlast  <= 1'b0;
                            state        <= ST_EVICT_B;
                        end else begin
                            axi_beat_cnt <= axi_beat_cnt + 1'b1;
                            m_axi_wlast  <= (axi_beat_cnt == BEAT_CNT_W'(LINE_BEATS-2));
                        end
                    end
                end

                ST_EVICT_B: begin
                    if (m_axi_bvalid) begin
                        tag_ram[pend_set][pend_way].dirty <= 1'b0;
                        cnt_wb <= cnt_wb + 1'b1;
                        axi_addr     <= {pend_addr[31:OFFSET_BITS], {OFFSET_BITS{1'b0}}};
                        axi_beat_cnt <= '0;
                        state        <= ST_READ_AR;
                    end
                end

                ST_READ_AR: begin
                    m_axi_arvalid <= 1'b1;
                    m_axi_araddr  <= axi_addr;
                    if (m_axi_arvalid && m_axi_arready) begin
                        m_axi_arvalid <= 1'b0;
                        m_axi_rready  <= 1'b1;
                        axi_beat_cnt  <= '0;
                        state         <= ST_READ_R;
                    end
                end

                ST_READ_R: begin
                    if (m_axi_rvalid && m_axi_rready) begin
                        axi_line_buf[axi_beat_cnt*AXI_DATA_BITS +: AXI_DATA_BITS] <= m_axi_rdata;
                        if (m_axi_rlast) begin
                            m_axi_rready <= 1'b0;
                            state <= ST_REFILL_DONE;
                        end else begin
                            axi_beat_cnt <= axi_beat_cnt + 1'b1;
                        end
                    end
                end

                ST_REFILL_DONE: begin
                    // Fill line
                    tag_ram[pend_set][pend_way].valid <= 1'b1;
                    tag_ram[pend_set][pend_way].tag   <= pend_tag;
                    data_ram[pend_set][pend_way]      <= axi_line_buf;
                    cnt_refills <= cnt_refills + 1'b1;

                    if (pend_rw) begin
                        logic [LINE_BYTES*8-1:0] merged;
                        merged = merge_line(axi_line_buf, pend_wdata, pend_wstrb);
                        data_ram[pend_set][pend_way] <= merged;
                        if (WRITEBACK) begin
                            tag_ram[pend_set][pend_way].dirty <= 1'b1;
                            resp_line        <= '0;
                            resp_total_beats <= BEAT_CNT_W'(1);
                            resp_active      <= 1'b1;
                            state       <= ST_RESPOND;
                        end else begin
                            axi_line_buf <= merged;
                            axi_addr     <= {pend_addr[31:OFFSET_BITS], {OFFSET_BITS{1'b0}}};
                            axi_beat_cnt <= '0;
                            state        <= ST_WT_AW;
                        end
                    end else begin
                        tag_ram[pend_set][pend_way].dirty <= 1'b0;
                        resp_line   <= axi_line_buf;
                        resp_total_beats <= BEAT_CNT_W'(LINE_BEATS);
                        resp_active <= 1'b1;
                        state       <= ST_RESPOND;
                    end

                    // Update replacement pointer
                    if (PARTITION_ACTIVE) begin
                        rr_way[pend_set][pend_port] <= rr_way[pend_set][pend_port] + 1'b1;
                    end else begin
                        rr_way[pend_set][0] <= rr_way[pend_set][0] + 1'b1;
                    end
                end

                ST_WT_AW: begin
                    m_axi_awvalid <= 1'b1;
                    m_axi_awaddr  <= axi_addr;
                    if (m_axi_awvalid && m_axi_awready) begin
                        m_axi_awvalid <= 1'b0;
                        m_axi_wvalid  <= 1'b1;
                        m_axi_wlast   <= (LINE_BEATS == 1);
                        state         <= ST_WT_W;
                    end
                end

                ST_WT_W: begin
                    if (m_axi_wvalid && m_axi_wready) begin
                        if (axi_beat_cnt == BEAT_CNT_W'(LINE_BEATS-1)) begin
                            m_axi_wvalid <= 1'b0;
                            m_axi_wlast  <= 1'b0;
                            state        <= ST_WT_B;
                        end else begin
                            axi_beat_cnt <= axi_beat_cnt + 1'b1;
                            m_axi_wlast  <= (axi_beat_cnt == BEAT_CNT_W'(LINE_BEATS-2));
                        end
                    end
                end

                ST_WT_B: begin
                    if (m_axi_bvalid) begin
                        resp_line        <= '0;
                        resp_total_beats <= BEAT_CNT_W'(1);
                        resp_active      <= 1'b1;
                        state       <= ST_RESPOND;
                    end
                end

                ST_RESPOND: begin
                    if (resp_active) begin
                        if (resp_beat == (resp_total_beats - 1'b1)) begin
                            resp_active <= 1'b0;
                            resp_beat   <= '0;
                            state       <= ST_IDLE;
                        end else begin
                            resp_beat <= resp_beat + 1'b1;
                        end
                    end
                end

                default: state <= ST_IDLE;
            endcase
        end
    end

    // Drive AXI write data
    always_comb begin
        m_axi_wdata = axi_line_buf[axi_beat_cnt*AXI_DATA_BITS +: AXI_DATA_BITS];
        m_axi_wstrb = {AXI_DATA_BITS/8{1'b1}};
    end

    initial begin
        if ((L2_SIZE_BYTES < 16384) || (L2_SIZE_BYTES > 131072)) begin
            $display("L2_SIZE_BYTES out of recommended range (16KB-128KB)");
        end
        if (NUM_PORTS < 1) begin
            $error("NUM_PORTS must be >= 1");
        end
        if (ASSOCIATIVITY != 4) begin
            $display("L2 associativity set to %0d (planned 4-way default)", ASSOCIATIVITY);
        end
    end

endmodule
