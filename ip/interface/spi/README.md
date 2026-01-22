## This module design for a simple SPI sat-ip
Configurable SPI sat-ip
TX/RX length: 1byte - 8bytes
support multiple CS(Chip-select)
configurable SCLK frequency: 4-24MHz

Bus interface: Direct connect.
clk
rst_n
[TX_length*8 - 1:0] cmd (TX)
[RX_length*8 - 1:0] resp (RX)
trmt (TX transmit)
rx_rdy (RX data payload valid and ready)
clr_rdy

SPI_SCLK
SPI_MOSI
SPI_MISO
[CS_num - 1: 0]SPI_CS

Auto-clear resp register when trmt assert.
Due to simplicity, this sat-ip provide a AXI-MailboxFabric varian, design to be direct control and access.