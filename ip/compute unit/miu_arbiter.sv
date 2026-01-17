module miu_arbiter (
    input  logic         clk,
    input  logic         rst_n,

    // Texture Cache Miss Interface (Refill Request)
    input  logic         tex_req_valid,
    input  logic [31:0]  tex_req_addr,
    output logic         tex_req_ready,
    output logic         tex_resp_valid,
    output logic [127:0] tex_resp_data, // Always 128-bit vector

    // GFX Descriptor Cache Miss Interface (Refill Request)
    input  logic         gfx_req_valid,
    input  logic [31:0]  gfx_req_addr,
    output logic         gfx_req_ready,
    output logic         gfx_resp_valid,
    output logic [127:0] gfx_resp_data, // Always 128-bit vector

    // Write-merge-buffer outbound (global scalar stores)
    input  logic         wmb_out_valid,
    input  logic [31:0]  wmb_out_addr,
    input  logic [31:0]  wmb_out_wdata,
    input  logic [3:0]   wmb_out_wstrb,
    output logic         wmb_out_ready,

    // LSU internal request (global)
    input  logic         lsu_req_valid,
    input  logic         lsu_req_is_load,
    input  logic         lsu_req_is_vector,
    input  logic [31:0]  lsu_req_addr,
    input  logic [127:0] lsu_req_wdata,
    input  logic [4:0]   lsu_req_rd,
    input  logic         lsu_req_is_rmw_load,
    output logic         lsu_req_ready,

    // Vector addressing info for non-texture vector ops
    input  logic [1:0]   vec_mode,
    input  logic [31:0]  vec_stride,
    input  logic [127:0] vec_index,

    // Global memory (OSPI) Unified 32-bit Interface
    output logic         global_req_valid,
    output logic         global_req_is_load,
    output logic [31:0]  global_req_addr,
    output logic [31:0]  global_req_wdata,
    output logic [4:0]   global_req_rd,
    input  logic         global_req_ready,

    input  logic         global_resp_valid,
    input  logic [4:0]   global_resp_rd,
    input  logic [31:0]  global_resp_data,

    // Response (to LSU / texture)
    output logic         arb_resp_valid,
    output logic [4:0]   arb_resp_rd,
    output logic [31:0]  arb_resp_data_scalar,
    output logic [127:0] arb_resp_data_vector,
    output logic         arb_resp_is_vector,
    output logic         arb_resp_is_rmw,
    output logic         arb_resp_is_tex,
    output logic         arb_resp_is_gfx,

    output logic         busy
);

    // ------------------------------------------------------------------------
    // INTERNAL STANDARDIZED REQUEST BUS (128-bit)
    // ------------------------------------------------------------------------
    logic        arb_req_valid;
    logic        arb_req_is_load;
    logic        arb_req_is_vector;
    logic [31:0] arb_req_addr;
    logic [127:0] arb_req_wdata;
    logic [4:0]  arb_req_rd;
    logic        arb_req_ready;

    // ------------------------------------------------------------------------
    // ARBITER (LSU vs WMB vs cache refills)
    // Policy: LSU > WMB > TEX > GFX
    // ------------------------------------------------------------------------
    logic arb_grant_tex;
    logic arb_grant_gfx;
    logic arb_grant_wmb;
    logic arb_grant_lsu;
    logic tex_inflight;
    logic gfx_inflight;

    // Mask unknowns on texture valid to avoid propagating X into stall/busy
    wire tex_req_valid_masked = (tex_req_valid === 1'b1);

    // Mask unknowns on gfx valid to avoid propagating X into stall/busy
    wire gfx_req_valid_masked = (gfx_req_valid === 1'b1);

    // Priority selection
    assign arb_grant_lsu = lsu_req_valid;
    assign arb_grant_wmb = (!arb_grant_lsu) && wmb_out_valid;
    assign arb_grant_tex = (!arb_grant_lsu) && (!arb_grant_wmb) && tex_req_valid_masked && !tex_inflight;
    assign arb_grant_gfx = (!arb_grant_lsu) && (!arb_grant_wmb) && (!arb_grant_tex) && gfx_req_valid_masked && !gfx_inflight;

    assign arb_req_valid     = arb_grant_lsu || arb_grant_wmb || arb_grant_tex || arb_grant_gfx;
    assign arb_req_is_load   = (arb_grant_tex || arb_grant_gfx) ? 1'b1 : (arb_grant_wmb ? 1'b0 : lsu_req_is_load);
    assign arb_req_is_vector = (arb_grant_tex || arb_grant_gfx) ? 1'b1 : (arb_grant_wmb ? 1'b0 : lsu_req_is_vector);
    assign arb_req_addr      = arb_grant_tex ? tex_req_addr
                            : arb_grant_gfx ? gfx_req_addr
                            : arb_grant_wmb ? wmb_out_addr
                            : lsu_req_addr;
    assign arb_req_wdata     = (arb_grant_tex || arb_grant_gfx) ? 128'h0
                            : (arb_grant_wmb ? {96'h0, wmb_out_wdata} : lsu_req_wdata);
    assign arb_req_rd        = (arb_grant_tex || arb_grant_gfx || arb_grant_wmb) ? 5'd0 : lsu_req_rd;

    assign tex_req_ready = arb_grant_tex && arb_req_ready;
    assign gfx_req_ready = arb_grant_gfx && arb_req_ready;
    assign wmb_out_ready = arb_grant_wmb && arb_req_ready;
    assign lsu_req_ready = arb_grant_lsu && arb_req_ready;

    // WMB write strobe currently unused by the unified global interface.
    // Keep it for forward-compat.
    /* verilator lint_off UNUSED */
    wire unused_wmb_wstrb = ^wmb_out_wstrb;
    /* verilator lint_on UNUSED */

    // ------------------------------------------------------------------------
    // MEMORY ADAPTER (128-bit Internal <-> 32-bit External)
    // ------------------------------------------------------------------------
    typedef enum logic [2:0] {
        ADAPT_IDLE,
        ADAPT_VWR_1, ADAPT_VWR_2, ADAPT_VWR_3,
        ADAPT_VRD_1, ADAPT_VRD_2, ADAPT_VRD_3
    } adapt_state_t;

    adapt_state_t adapt_state;
    logic [127:0] req_wdata_hold;
    logic [31:0]  req_addr_hold;
    logic [4:0]   req_rd_hold;
    logic         req_is_cache_hold;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            adapt_state     <= ADAPT_IDLE;
            req_wdata_hold  <= '0;
            req_addr_hold   <= '0;
            req_rd_hold     <= '0;
            req_is_cache_hold <= 1'b0;
        end else begin
            case (adapt_state)
                ADAPT_IDLE: begin
                    if (arb_req_valid) begin
                        if (arb_req_is_vector) begin
                            if (global_req_ready) begin
                                req_addr_hold   <= arb_req_addr;
                                req_wdata_hold  <= arb_req_wdata;
                                req_rd_hold     <= arb_req_rd;
                                req_is_cache_hold <= (arb_grant_tex || arb_grant_gfx);
                                adapt_state     <= arb_req_is_load ? ADAPT_VRD_1 : ADAPT_VWR_1;
                            end
                        end else if (global_req_ready) begin
                            adapt_state <= ADAPT_IDLE;
                        end
                    end
                end
                ADAPT_VWR_1: if (global_req_ready) adapt_state <= ADAPT_VWR_2;
                ADAPT_VWR_2: if (global_req_ready) adapt_state <= ADAPT_VWR_3;
                ADAPT_VWR_3: if (global_req_ready) adapt_state <= ADAPT_IDLE;
                ADAPT_VRD_1: if (global_req_ready) adapt_state <= ADAPT_VRD_2;
                ADAPT_VRD_2: if (global_req_ready) adapt_state <= ADAPT_VRD_3;
                ADAPT_VRD_3: if (global_req_ready) adapt_state <= ADAPT_IDLE;
                default: adapt_state <= ADAPT_IDLE;
            endcase
        end
    end

    function automatic [31:0] lane_addr(
        input logic [31:0] base,
        input logic [1:0]  mode,
        input logic [31:0] stride,
        input logic [127:0] idx_vec,
        input int          lane
    );
        case (mode)
            2'b01: lane_addr = base + stride * lane;
            2'b10: lane_addr = base + idx_vec[lane*32 +: 32];
            default: lane_addr = base + lane*4;
        endcase
    endfunction

    function automatic logic [31:0] tex_lane_addr(
        input logic [31:0] base,
        input int lane
    );
        tex_lane_addr = base + lane * 4;
    endfunction

    // External Request Drive
    always_comb begin
        global_req_valid   = 1'b0;
        global_req_is_load = arb_req_is_load;
        global_req_addr    = arb_req_addr;
        global_req_wdata   = arb_req_wdata[31:0];
        global_req_rd      = (adapt_state == ADAPT_IDLE) ? arb_req_rd : req_rd_hold;
        arb_req_ready      = 1'b0;

        /* verilator lint_off CASEINCOMPLETE */
        case (adapt_state)
            ADAPT_IDLE: begin
                if (arb_req_valid) begin
                    if (arb_req_is_vector) begin
                        global_req_valid = 1'b1;
                        global_req_addr  = (arb_grant_tex || arb_grant_gfx) ? tex_lane_addr(arb_req_addr, 0)
                                                        : lane_addr(arb_req_addr, vec_mode, vec_stride, vec_index, 0);
                        global_req_wdata = arb_req_wdata[31:0];
                        if (global_req_ready) arb_req_ready = 1'b1;
                    end else begin
                        global_req_valid = 1'b1;
                        global_req_addr  = arb_req_addr;
                        global_req_wdata = arb_req_wdata[31:0];
                        if (global_req_ready) arb_req_ready = 1'b1;
                    end
                end
            end
            ADAPT_VWR_1: begin
                global_req_valid   = 1'b1;
                global_req_is_load = 1'b0;
                global_req_addr    = req_is_cache_hold ? tex_lane_addr(req_addr_hold, 1)
                                                    : lane_addr(req_addr_hold, vec_mode, vec_stride, vec_index, 1);
                global_req_wdata   = req_wdata_hold[63:32];
            end
            ADAPT_VWR_2: begin
                global_req_valid   = 1'b1;
                global_req_is_load = 1'b0;
                global_req_addr    = req_is_cache_hold ? tex_lane_addr(req_addr_hold, 2)
                                                    : lane_addr(req_addr_hold, vec_mode, vec_stride, vec_index, 2);
                global_req_wdata   = req_wdata_hold[95:64];
            end
            ADAPT_VWR_3: begin
                global_req_valid   = 1'b1;
                global_req_is_load = 1'b0;
                global_req_addr    = req_is_cache_hold ? tex_lane_addr(req_addr_hold, 3)
                                                    : lane_addr(req_addr_hold, vec_mode, vec_stride, vec_index, 3);
                global_req_wdata   = req_wdata_hold[127:96];
            end
            default: begin
                global_req_valid   = 1'b0;
                global_req_is_load = arb_req_is_load;
                global_req_addr    = arb_req_addr;
                global_req_wdata   = arb_req_wdata[31:0];
                arb_req_ready      = 1'b0;
            end
            ADAPT_VRD_1: begin
        /* verilator lint_on CASEINCOMPLETE */
                global_req_valid   = 1'b1;
                global_req_is_load = 1'b1;
                global_req_addr    = req_is_cache_hold ? tex_lane_addr(req_addr_hold, 1)
                                                    : lane_addr(req_addr_hold, vec_mode, vec_stride, vec_index, 1);
            end
            ADAPT_VRD_2: begin
                global_req_valid   = 1'b1;
                global_req_is_load = 1'b1;
                global_req_addr    = req_is_cache_hold ? tex_lane_addr(req_addr_hold, 2)
                                                    : lane_addr(req_addr_hold, vec_mode, vec_stride, vec_index, 2);
            end
            ADAPT_VRD_3: begin
                global_req_valid   = 1'b1;
                global_req_is_load = 1'b1;
                global_req_addr    = req_is_cache_hold ? tex_lane_addr(req_addr_hold, 3)
                                                    : lane_addr(req_addr_hold, vec_mode, vec_stride, vec_index, 3);
            end
        endcase
    end

    // ------------------------------------------------------------------------
    // RESPONSE RECONSTRUCTION & ROUTING
    // ------------------------------------------------------------------------
    logic [1:0]   rx_collect_cnt;
    logic [127:0] rx_collect_buf;
    logic [4:0]   rx_pending_rd;
    logic         rx_fifo_pop;

    logic [3:0] expect_fifo_type [0:7];
    logic [1:0] expect_fifo_src  [0:7];
    logic [3:0] expect_fifo_rmw  [0:7];
    logic [2:0] expect_wr, expect_rd;
    logic       expect_empty;

    assign expect_empty = (expect_wr == expect_rd);

    localparam logic [1:0] SRC_LSU = 2'd0;
    localparam logic [1:0] SRC_TEX = 2'd1;
    localparam logic [1:0] SRC_GFX = 2'd2;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            expect_wr      <= 0;
            expect_rd      <= 0;
            rx_collect_cnt <= 0;
            rx_collect_buf <= 0;
            rx_pending_rd  <= 0;
        end else begin
            if (arb_req_valid && arb_req_ready && arb_req_is_load) begin
                expect_fifo_type[expect_wr] <= arb_req_is_vector;
                expect_fifo_src[expect_wr]  <= arb_grant_tex ? SRC_TEX : (arb_grant_gfx ? SRC_GFX : SRC_LSU);
                expect_fifo_rmw[expect_wr]  <= (!arb_req_is_vector)
                                           && (!arb_grant_tex)
                                           && (!arb_grant_gfx)
                                           && (!arb_grant_wmb)
                                           && lsu_req_is_rmw_load;
                expect_wr <= expect_wr + 1;
            end

            if (rx_fifo_pop) expect_rd <= expect_rd + 1;

            if (global_resp_valid && !expect_empty) begin
                if (expect_fifo_type[expect_rd]) begin
                    case (rx_collect_cnt)
                        0: begin
                            rx_collect_buf[31:0] <= global_resp_data;
                            rx_pending_rd <= global_resp_rd;
                        end
                        1: rx_collect_buf[63:32]  <= global_resp_data;
                        2: rx_collect_buf[95:64]  <= global_resp_data;
                        3: rx_collect_buf[127:96] <= global_resp_data;
                    endcase
                    rx_collect_cnt <= rx_collect_cnt + 1;
                end
            end

            if (rx_fifo_pop && expect_fifo_type[expect_rd]) rx_collect_cnt <= 0;
        end
    end

    wire current_is_tex = (expect_fifo_src[expect_rd] == SRC_TEX);
    wire current_is_gfx = (expect_fifo_src[expect_rd] == SRC_GFX);
    wire current_is_rmw = expect_fifo_rmw[expect_rd];

    always_comb begin
        arb_resp_valid       = 1'b0;
        arb_resp_data_scalar = 32'b0;
        arb_resp_data_vector = 128'b0;
        arb_resp_rd          = 5'b0;
        rx_fifo_pop          = 1'b0;
        arb_resp_is_vector   = 1'b0;
        arb_resp_is_rmw      = 1'b0;
        arb_resp_is_tex      = 1'b0;
        arb_resp_is_gfx      = 1'b0;

        if (global_resp_valid && !expect_empty) begin
            if (expect_fifo_type[expect_rd] == 0) begin
                arb_resp_valid       = 1'b1;
                arb_resp_data_scalar = global_resp_data;
                arb_resp_rd          = global_resp_rd;
                rx_fifo_pop          = 1'b1;
                arb_resp_is_vector   = 1'b0;
                arb_resp_is_rmw      = current_is_rmw;
                arb_resp_is_tex      = current_is_tex;
                arb_resp_is_gfx      = current_is_gfx;
            end else begin
                if (rx_collect_cnt == 2'd3) begin
                    arb_resp_valid       = 1'b1;
                    arb_resp_data_vector = {global_resp_data, rx_collect_buf[95:0]};
                    arb_resp_rd          = rx_pending_rd;
                    rx_fifo_pop          = 1'b1;
                    arb_resp_is_vector   = 1'b1;
                    arb_resp_is_rmw      = 1'b0;
                    arb_resp_is_tex      = current_is_tex;
                    arb_resp_is_gfx      = current_is_gfx;
                end
            end
        end
    end

    assign tex_resp_valid = arb_resp_valid && arb_resp_is_tex;
    assign tex_resp_data  = arb_resp_data_vector;

    assign gfx_resp_valid = arb_resp_valid && arb_resp_is_gfx;
    assign gfx_resp_data  = arb_resp_data_vector;

    // Texture inflight tracking
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tex_inflight <= 1'b0;
            gfx_inflight <= 1'b0;
        end else begin
            if (tex_req_ready && tex_req_valid_masked) begin
                tex_inflight <= 1'b1;
            end
            if (tex_resp_valid) begin
                tex_inflight <= 1'b0;
            end

            if (gfx_req_ready && gfx_req_valid_masked) begin
                gfx_inflight <= 1'b1;
            end
            if (gfx_resp_valid) begin
                gfx_inflight <= 1'b0;
            end
        end
    end

    assign busy = (adapt_state != ADAPT_IDLE)
               || (arb_req_valid && !arb_req_ready)
               || !expect_empty;

endmodule
