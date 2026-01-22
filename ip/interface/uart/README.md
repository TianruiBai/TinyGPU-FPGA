## This module is a generic UART sat-ip
Configurable baud rate:1200-2048000
With basic 1byte TX/ 1byte RX
Optional odd/even bit(LOCALPARAM)
With support of CTS/RTS

Bus interface: Direct connect.
clk
rst_n
[7:0] tx_data
trmt
[7:0] rx_data
rdy
clr_rdy

UART_TX
UART_RX
UART_CTS
UART_RTS

Due to simplicity, this sat-ip provide a AXI-MailboxFabric varian, design to be direct control and access.