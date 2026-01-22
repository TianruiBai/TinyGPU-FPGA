// Framebuffer-driven LCD SPI controller (ST7735S, 0.96" 160x80)
// AXI4 read master for framebuffer fetch, 4-wire SPI output
module lcd_spi_axi #(
    parameter int CLK_HZ       = 50_000_000,
    parameter int SPI_HZ       = 12_000_000,
    parameter int FB_WIDTH     = 160,
    parameter int FB_HEIGHT    = 80,
    parameter int AXI_ADDR_BITS = 32,
    parameter int AXI_DATA_BITS = 32,
    parameter logic [AXI_ADDR_BITS-1:0] FB_BASE_DEFAULT = '0,
    parameter bit USE_MAILBOX_CTRL = 1'b1
) (
    input  logic                   clk,
    input  logic                   rst_n,

    // Control
    input  logic                   enable,
    input  logic                   frame_start,
    input  logic [AXI_ADDR_BITS-1:0] fb_base,
    output logic                   busy,
    output logic                   frame_done,

    // Mailbox control register interface (AXI-Lite-like)
    input  logic                   mb_s_awvalid,
    output logic                   mb_s_awready,
    input  logic [15:0]            mb_s_awaddr,
    input  logic                   mb_s_wvalid,
    output logic                   mb_s_wready,
    input  logic [31:0]            mb_s_wdata,
    input  logic [3:0]             mb_s_wstrb,
    input  mailbox_pkg::mailbox_tag_t mb_s_tag,
    input  logic                   mb_s_bready,
    output logic                   mb_s_bvalid,

    input  logic                   mb_s_arvalid,
    output logic                   mb_s_arready,
    input  logic [15:0]            mb_s_araddr,
    output logic                   mb_s_rvalid,
    input  logic                   mb_s_rready,
    output logic [31:0]            mb_s_rdata,

    // AXI4 Read Address Channel
    output logic [AXI_ADDR_BITS-1:0] m_axi_araddr,
    output logic [7:0]             m_axi_arlen,
    output logic [2:0]             m_axi_arsize,
    output logic [1:0]             m_axi_arburst,
    output logic                   m_axi_arvalid,
    input  logic                   m_axi_arready,

    // AXI4 Read Data Channel
    input  logic [AXI_DATA_BITS-1:0] m_axi_rdata,
    input  logic                   m_axi_rvalid,
    input  logic                   m_axi_rlast,
    output logic                   m_axi_rready,
    input  logic [1:0]             m_axi_rresp,

    // LCD SPI pins
    output logic                   lcd_cs_n,
    output logic                   lcd_sclk,
    output logic                   lcd_mosi,
    output logic                   lcd_dc,
    output logic                   lcd_rst_n,
    output logic                   lcd_bl
);
    import mailbox_pkg::*;
    // -----------------------
    // Constants / parameters
    // -----------------------
    localparam int SPI_DIV_RAW = (SPI_HZ == 0) ? 1 : (CLK_HZ / (SPI_HZ * 2));
    localparam int SPI_DIV = (SPI_DIV_RAW < 1) ? 1 : SPI_DIV_RAW;
    localparam int SPI_DIV_W = (SPI_DIV <= 1) ? 1 : $clog2(SPI_DIV);
    localparam int INIT_DELAY_200MS = (CLK_HZ / 5);
    localparam int INIT_DELAY_120MS = (CLK_HZ * 120) / 1000;

    localparam int FB_PIXELS = FB_WIDTH * FB_HEIGHT;
    localparam int FB_BYTES  = FB_PIXELS * 2; // RGB565
    localparam int FB_WORDS  = FB_BYTES / 4;  // 32-bit words

    // -----------------------
    // Init sequence ROM (ST7735S)
    // -----------------------
    localparam int INIT_LEN = 76;

    function automatic logic init_is_data(input int idx);
        case (idx)
            0:  init_is_data = 1'b0; // 0x11
            1:  init_is_data = 1'b0; // 0x21
            2:  init_is_data = 1'b0; // 0x21
            3:  init_is_data = 1'b0; // 0xB1
            4:  init_is_data = 1'b1;
            5:  init_is_data = 1'b1;
            6:  init_is_data = 1'b1;
            7:  init_is_data = 1'b0; // 0xB2
            8:  init_is_data = 1'b1;
            9:  init_is_data = 1'b1;
            10: init_is_data = 1'b1;
            11: init_is_data = 1'b0; // 0xB3
            12: init_is_data = 1'b1;
            13: init_is_data = 1'b1;
            14: init_is_data = 1'b1;
            15: init_is_data = 1'b1;
            16: init_is_data = 1'b1;
            17: init_is_data = 1'b1;
            18: init_is_data = 1'b0; // 0xB4
            19: init_is_data = 1'b1;
            20: init_is_data = 1'b0; // 0xC0
            21: init_is_data = 1'b1;
            22: init_is_data = 1'b1;
            23: init_is_data = 1'b1;
            24: init_is_data = 1'b0; // 0xC1
            25: init_is_data = 1'b1;
            26: init_is_data = 1'b0; // 0xC2
            27: init_is_data = 1'b1;
            28: init_is_data = 1'b1;
            29: init_is_data = 1'b0; // 0xC3
            30: init_is_data = 1'b1;
            31: init_is_data = 1'b1;
            32: init_is_data = 1'b0; // 0xC4
            33: init_is_data = 1'b1;
            34: init_is_data = 1'b1;
            35: init_is_data = 1'b0; // 0xC5
            36: init_is_data = 1'b1;
            37: init_is_data = 1'b0; // 0xE0
            38: init_is_data = 1'b1;
            39: init_is_data = 1'b1;
            40: init_is_data = 1'b1;
            41: init_is_data = 1'b1;
            42: init_is_data = 1'b1;
            43: init_is_data = 1'b1;
            44: init_is_data = 1'b1;
            45: init_is_data = 1'b1;
            46: init_is_data = 1'b1;
            47: init_is_data = 1'b1;
            48: init_is_data = 1'b1;
            49: init_is_data = 1'b1;
            50: init_is_data = 1'b1;
            51: init_is_data = 1'b1;
            52: init_is_data = 1'b1;
            53: init_is_data = 1'b1;
            54: init_is_data = 1'b0; // 0xE1
            55: init_is_data = 1'b1;
            56: init_is_data = 1'b1;
            57: init_is_data = 1'b1;
            58: init_is_data = 1'b1;
            59: init_is_data = 1'b1;
            60: init_is_data = 1'b1;
            61: init_is_data = 1'b1;
            62: init_is_data = 1'b1;
            63: init_is_data = 1'b1;
            64: init_is_data = 1'b1;
            65: init_is_data = 1'b1;
            66: init_is_data = 1'b1;
            67: init_is_data = 1'b1;
            68: init_is_data = 1'b1;
            69: init_is_data = 1'b1;
            70: init_is_data = 1'b1;
            71: init_is_data = 1'b0; // 0x3A
            72: init_is_data = 1'b1;
            73: init_is_data = 1'b0; // 0x36
            74: init_is_data = 1'b1;
            75: init_is_data = 1'b0; // 0x29
            default: init_is_data = 1'b0;
        endcase
    endfunction

    function automatic [7:0] init_byte(input int idx);
        case (idx)
            0:  init_byte = 8'h11;
            1:  init_byte = 8'h21;
            2:  init_byte = 8'h21;
            3:  init_byte = 8'hB1;
            4:  init_byte = 8'h05;
            5:  init_byte = 8'h3A;
            6:  init_byte = 8'h3A;
            7:  init_byte = 8'hB2;
            8:  init_byte = 8'h05;
            9:  init_byte = 8'h3A;
            10: init_byte = 8'h3A;
            11: init_byte = 8'hB3;
            12: init_byte = 8'h05;
            13: init_byte = 8'h3A;
            14: init_byte = 8'h3A;
            15: init_byte = 8'h05;
            16: init_byte = 8'h3A;
            17: init_byte = 8'h3A;
            18: init_byte = 8'hB4;
            19: init_byte = 8'h03;
            20: init_byte = 8'hC0;
            21: init_byte = 8'h62;
            22: init_byte = 8'h02;
            23: init_byte = 8'h04;
            24: init_byte = 8'hC1;
            25: init_byte = 8'hC0;
            26: init_byte = 8'hC2;
            27: init_byte = 8'h0D;
            28: init_byte = 8'h00;
            29: init_byte = 8'hC3;
            30: init_byte = 8'h8D;
            31: init_byte = 8'h6A;
            32: init_byte = 8'hC4;
            33: init_byte = 8'h8D;
            34: init_byte = 8'hEE;
            35: init_byte = 8'hC5;
            36: init_byte = 8'h0E;
            37: init_byte = 8'hE0;
            38: init_byte = 8'h10;
            39: init_byte = 8'h0E;
            40: init_byte = 8'h02;
            41: init_byte = 8'h03;
            42: init_byte = 8'h0E;
            43: init_byte = 8'h07;
            44: init_byte = 8'h02;
            45: init_byte = 8'h07;
            46: init_byte = 8'h0A;
            47: init_byte = 8'h12;
            48: init_byte = 8'h27;
            49: init_byte = 8'h37;
            50: init_byte = 8'h00;
            51: init_byte = 8'h0D;
            52: init_byte = 8'h0E;
            53: init_byte = 8'h10;
            54: init_byte = 8'hE1;
            55: init_byte = 8'h10;
            56: init_byte = 8'h0E;
            57: init_byte = 8'h03;
            58: init_byte = 8'h03;
            59: init_byte = 8'h0F;
            60: init_byte = 8'h06;
            61: init_byte = 8'h02;
            62: init_byte = 8'h08;
            63: init_byte = 8'h0A;
            64: init_byte = 8'h13;
            65: init_byte = 8'h26;
            66: init_byte = 8'h36;
            67: init_byte = 8'h00;
            68: init_byte = 8'h0D;
            69: init_byte = 8'h0E;
            70: init_byte = 8'h10;
            71: init_byte = 8'h3A;
            72: init_byte = 8'h05;
            73: init_byte = 8'h36;
            74: init_byte = 8'hA8;
            75: init_byte = 8'h29;
            default: init_byte = 8'h00;
        endcase
    endfunction

    // -----------------------
    // SPI byte transmitter
    // -----------------------
    logic [7:0]  tx_shift;
    logic [2:0]  tx_bit_cnt;
    logic        tx_busy;
    logic        tx_start;
    logic [7:0]  tx_byte;
    logic        tx_is_data;
    logic [SPI_DIV_W-1:0] spi_div_cnt;
    logic        sclk_int;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            tx_busy     <= 1'b0;
            tx_shift    <= 8'h00;
            tx_bit_cnt  <= 3'd0;
            spi_div_cnt <= '0;
            sclk_int    <= 1'b0;
            lcd_mosi    <= 1'b0;
        end else begin
            if (tx_start && !tx_busy) begin
                tx_busy     <= 1'b1;
                tx_shift    <= tx_byte;
                tx_bit_cnt  <= 3'd7;
                spi_div_cnt <= '0;
                sclk_int    <= 1'b0;
                lcd_mosi    <= tx_byte[7];
            end else if (tx_busy) begin
                if (spi_div_cnt == SPI_DIV_W'(SPI_DIV-1)) begin
                    spi_div_cnt <= '0;
                    sclk_int <= ~sclk_int;
                    if (sclk_int) begin
                        // Falling edge: shift next bit
                        if (tx_bit_cnt == 0) begin
                            tx_busy <= 1'b0;
                            sclk_int <= 1'b0;
                        end else begin
                            tx_shift <= {tx_shift[6:0], 1'b0};
                            tx_bit_cnt <= tx_bit_cnt - 1'b1;
                            lcd_mosi <= tx_shift[6];
                        end
                    end
                end else begin
                    spi_div_cnt <= spi_div_cnt + 1'b1;
                end
            end
        end
    end

    assign lcd_sclk = tx_busy ? sclk_int : 1'b0;

    // -----------------------
    // Mailbox control registers
    // -----------------------
    logic        reg_enable;
    logic        reg_frame_start_pulse;
    logic [31:0] reg_fb_base;
    logic        mb_bvalid_r;
    logic        mb_rvalid_r;
    logic [31:0] mb_rdata_r;

    wire        enable_eff = USE_MAILBOX_CTRL ? reg_enable : enable;
    wire        frame_start_eff = USE_MAILBOX_CTRL ? reg_frame_start_pulse : frame_start;
    wire [31:0] fb_base_eff = USE_MAILBOX_CTRL ? reg_fb_base : fb_base;

    // Mailbox write handshake
    assign mb_s_awready = !mb_bvalid_r;
    assign mb_s_wready  = !mb_bvalid_r;
    assign mb_s_bvalid  = mb_bvalid_r;

    // Mailbox read handshake
    assign mb_s_arready = !mb_rvalid_r;
    assign mb_s_rvalid  = mb_rvalid_r;
    assign mb_s_rdata   = mb_rdata_r;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            reg_enable            <= 1'b0;
            reg_frame_start_pulse <= 1'b0;
            reg_fb_base           <= FB_BASE_DEFAULT;
            mb_bvalid_r           <= 1'b0;
            mb_rvalid_r           <= 1'b0;
            mb_rdata_r            <= 32'h0;
        end else begin
            reg_frame_start_pulse <= 1'b0;

            if (mb_bvalid_r && mb_s_bready) mb_bvalid_r <= 1'b0;
            if (mb_rvalid_r && mb_s_rready) mb_rvalid_r <= 1'b0;

            if (mb_s_awvalid && mb_s_wvalid && mb_s_awready) begin
                mb_bvalid_r <= 1'b1;
                case (mb_s_awaddr[5:2])
                    4'h0: begin
                        if (mb_s_wstrb[0]) begin
                            reg_enable <= mb_s_wdata[0];
                            if (mb_s_wdata[1]) reg_frame_start_pulse <= 1'b1;
                        end
                    end
                    4'h1: begin
                        if (mb_s_wstrb[0]) reg_fb_base[7:0]   <= mb_s_wdata[7:0];
                        if (mb_s_wstrb[1]) reg_fb_base[15:8]  <= mb_s_wdata[15:8];
                        if (mb_s_wstrb[2]) reg_fb_base[23:16] <= mb_s_wdata[23:16];
                        if (mb_s_wstrb[3]) reg_fb_base[31:24] <= mb_s_wdata[31:24];
                    end
                    default: ;
                endcase
            end

            if (mb_s_arvalid && mb_s_arready) begin
                mb_rvalid_r <= 1'b1;
                case (mb_s_araddr[5:2])
                    4'h0: mb_rdata_r <= {28'h0, frame_done, busy, 2'b00, reg_enable};
                    4'h1: mb_rdata_r <= reg_fb_base;
                    default: mb_rdata_r <= 32'h0;
                endcase
            end
        end
    end

    // -----------------------
    // AXI read control
    // -----------------------
    logic [AXI_ADDR_BITS-1:0] rd_addr;
    logic                     rd_pending;
    logic [AXI_DATA_BITS-1:0]  rd_data;
    logic                     rd_data_valid;
    logic                     ar_req;
    logic [AXI_ADDR_BITS-1:0]  ar_req_addr;
    logic                     rd_consume;

    assign m_axi_arlen   = 8'd0; // single beat
    assign m_axi_arsize  = (AXI_DATA_BITS == 64) ? 3'd3 : 3'd2; // 4 or 8 bytes
    assign m_axi_arburst = 2'b01; // INCR
    assign m_axi_rready  = rd_pending;

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            m_axi_arvalid <= 1'b0;
            m_axi_araddr  <= '0;
            rd_pending    <= 1'b0;
            rd_data_valid <= 1'b0;
            rd_data       <= '0;
        end else begin
            if (ar_req && !m_axi_arvalid && !rd_pending) begin
                m_axi_arvalid <= 1'b1;
                m_axi_araddr  <= ar_req_addr;
            end

            if (m_axi_arvalid && m_axi_arready) begin
                m_axi_arvalid <= 1'b0;
                rd_pending    <= 1'b1;
            end

            if (m_axi_rvalid && rd_pending) begin
                rd_data       <= m_axi_rdata;
                rd_data_valid <= 1'b1;
                rd_pending    <= 1'b0;
            end

            if (rd_consume) begin
                rd_data_valid <= 1'b0;
            end
        end
    end

    // -----------------------
    // Main FSM
    // -----------------------
    typedef enum logic [3:0] {
        ST_RESET_LOW,
        ST_RESET_HIGH,
        ST_RESET_WAIT,
        ST_INIT,
        ST_INIT_DELAY,
        ST_SET_WINDOW,
        ST_FRAME_CMD,
        ST_FRAME_STREAM,
        ST_FRAME_DONE
    } state_t;

    state_t state;
    logic [$clog2(INIT_LEN):0] init_idx;
    logic [31:0] delay_cnt;

    // Frame streaming
    logic [31:0] word_idx;
    logic [1:0]  byte_sel;
    logic        have_word;

    // Window command sequence
    logic [4:0]  win_idx;

    // Default outputs
    assign lcd_bl  = 1'b1;
    assign lcd_cs_n = !(tx_busy || (state == ST_INIT) || (state == ST_SET_WINDOW) || (state == ST_FRAME_STREAM));

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state       <= ST_RESET_LOW;
            init_idx    <= '0;
            delay_cnt   <= 32'd0;
            tx_start    <= 1'b0;
            tx_byte     <= 8'h00;
            tx_is_data  <= 1'b0;
            lcd_dc      <= 1'b0;
            lcd_rst_n   <= 1'b0;
            busy        <= 1'b1;
            frame_done  <= 1'b0;
            word_idx    <= 32'd0;
            byte_sel    <= 2'd0;
            have_word   <= 1'b0;
            win_idx     <= 5'd0;
            rd_addr     <= '0;
            ar_req      <= 1'b0;
            ar_req_addr <= '0;
            rd_consume  <= 1'b0;
        end else begin
            tx_start   <= 1'b0;
            frame_done <= 1'b0;
            ar_req      <= 1'b0;
            rd_consume  <= 1'b0;

            case (state)
                ST_RESET_LOW: begin
                    busy      <= 1'b1;
                    lcd_rst_n <= 1'b0;
                    delay_cnt <= INIT_DELAY_200MS;
                    state     <= ST_RESET_HIGH;
                end

                ST_RESET_HIGH: begin
                    if (delay_cnt != 0) begin
                        delay_cnt <= delay_cnt - 1'b1;
                    end else begin
                        lcd_rst_n <= 1'b1;
                        delay_cnt <= INIT_DELAY_200MS;
                        state     <= ST_RESET_WAIT;
                    end
                end

                ST_RESET_WAIT: begin
                    if (delay_cnt != 0) begin
                        delay_cnt <= delay_cnt - 1'b1;
                    end else begin
                        init_idx <= '0;
                        state    <= ST_INIT;
                    end
                end

                ST_INIT: begin
                    if (!tx_busy) begin
                        if (init_idx < INIT_LEN) begin
                            tx_byte    <= init_byte(init_idx);
                            tx_is_data <= init_is_data(init_idx);
                            lcd_dc     <= init_is_data(init_idx);
                            tx_start   <= 1'b1;
                            if (init_idx == 0) begin
                                state <= ST_INIT_DELAY;
                                delay_cnt <= INIT_DELAY_120MS;
                            end
                            init_idx <= init_idx + 1'b1;
                        end else begin
                            win_idx <= 5'd0;
                            state   <= ST_SET_WINDOW;
                        end
                    end
                end

                ST_INIT_DELAY: begin
                    if (delay_cnt != 0) begin
                        delay_cnt <= delay_cnt - 1'b1;
                    end else begin
                        state <= ST_INIT;
                    end
                end

                ST_SET_WINDOW: begin
                    if (!tx_busy) begin
                        case (win_idx)
                            5'd0: begin // 0x2A
                                tx_byte   <= 8'h2A;
                                tx_is_data <= 1'b0;
                                lcd_dc    <= 1'b0;
                            end
                            5'd1: begin // Xstart MSB
                                tx_byte   <= 8'h00;
                                tx_is_data <= 1'b1;
                                lcd_dc    <= 1'b1;
                            end
                            5'd2: begin // Xstart LSB (1)
                                tx_byte   <= 8'h01;
                                tx_is_data <= 1'b1;
                                lcd_dc    <= 1'b1;
                            end
                            5'd3: begin // Xend MSB
                                tx_byte   <= ((FB_WIDTH) >> 8) & 8'hFF;
                                tx_is_data <= 1'b1;
                                lcd_dc    <= 1'b1;
                            end
                            5'd4: begin // Xend LSB
                                tx_byte   <= (FB_WIDTH[7:0]);
                                tx_is_data <= 1'b1;
                                lcd_dc    <= 1'b1;
                            end
                            5'd5: begin // 0x2B
                                tx_byte   <= 8'h2B;
                                tx_is_data <= 1'b0;
                                lcd_dc    <= 1'b0;
                            end
                            5'd6: begin // Ystart MSB
                                tx_byte   <= 8'h00;
                                tx_is_data <= 1'b1;
                                lcd_dc    <= 1'b1;
                            end
                            5'd7: begin // Ystart LSB (26)
                                tx_byte   <= 8'd26;
                                tx_is_data <= 1'b1;
                                lcd_dc    <= 1'b1;
                            end
                            5'd8: begin // Yend MSB
                                tx_byte   <= ((FB_HEIGHT + 25) >> 8) & 8'hFF;
                                tx_is_data <= 1'b1;
                                lcd_dc    <= 1'b1;
                            end
                            5'd9: begin // Yend LSB
                                tx_byte   <= (FB_HEIGHT + 25)[7:0];
                                tx_is_data <= 1'b1;
                                lcd_dc    <= 1'b1;
                            end
                            5'd10: begin // 0x2C
                                tx_byte   <= 8'h2C;
                                tx_is_data <= 1'b0;
                                lcd_dc    <= 1'b0;
                            end
                            default: begin
                                tx_byte   <= 8'h00;
                                tx_is_data <= 1'b0;
                                lcd_dc    <= 1'b0;
                            end
                        endcase
                        tx_start <= 1'b1;
                        if (win_idx == 5'd10) begin
                            state    <= ST_FRAME_CMD;
                            word_idx <= 32'd0;
                            byte_sel <= 2'd0;
                            have_word <= 1'b0;
                        end
                        win_idx <= win_idx + 1'b1;
                    end
                end

                ST_FRAME_CMD: begin
                    if (!enable_eff) begin
                        state <= ST_FRAME_DONE;
                    end else if (frame_start_eff) begin
                        state <= ST_FRAME_STREAM;
                    end
                end

                ST_FRAME_STREAM: begin
                    if (!enable_eff) begin
                        state <= ST_FRAME_DONE;
                    end else begin
                        // Request next word if needed
                        if (!have_word && !rd_pending && !m_axi_arvalid && (word_idx < FB_WORDS)) begin
                            rd_addr     <= fb_base_eff + (word_idx << 2);
                            ar_req_addr <= fb_base_eff + (word_idx << 2);
                            ar_req      <= 1'b1;
                        end

                        if (rd_data_valid) begin
                            have_word <= 1'b1;
                        end

                        if (have_word && !tx_busy) begin
                            // Byte order for RGB565: send high byte then low byte per pixel
                            case (byte_sel)
                                2'd0: tx_byte <= rd_data[15:8];
                                2'd1: tx_byte <= rd_data[7:0];
                                2'd2: tx_byte <= rd_data[31:24];
                                2'd3: tx_byte <= rd_data[23:16];
                            endcase
                            tx_is_data <= 1'b1;
                            lcd_dc     <= 1'b1;
                            tx_start   <= 1'b1;

                            if (byte_sel == 2'd3) begin
                                byte_sel   <= 2'd0;
                                word_idx   <= word_idx + 1'b1;
                                have_word  <= 1'b0;
                                rd_consume <= 1'b1;
                            end else begin
                                byte_sel <= byte_sel + 1'b1;
                            end
                        end

                        if (word_idx == FB_WORDS && !have_word && !tx_busy) begin
                            state <= ST_FRAME_DONE;
                        end
                    end
                end

                ST_FRAME_DONE: begin
                    frame_done <= 1'b1;
                    busy <= 1'b0;
                    if (frame_start && enable) begin
                        win_idx <= 5'd0;
                        state   <= ST_SET_WINDOW;
                        busy    <= 1'b1;
                    end
                end

                default: state <= ST_RESET_LOW;
            endcase

            // Busy flag
            if (state == ST_RESET_LOW || state == ST_RESET_HIGH || state == ST_RESET_WAIT || state == ST_INIT || state == ST_INIT_DELAY || state == ST_SET_WINDOW || state == ST_FRAME_STREAM) begin
                busy <= 1'b1;
            end
        end
    end

endmodule
