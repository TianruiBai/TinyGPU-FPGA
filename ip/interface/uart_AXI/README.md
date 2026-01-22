## This module is AXI-Lite/AXI-4 UART IP
Using AXI-Lite/AXI-4 to provide flexible UART interface with configurable following:
1. Configurable baud rate
2. Support for various UART data frame(B), parity bits(Odd/Even), stop bits(S). (7B1E1S, 8B1S, 8B1E1S, etc)
3. Support for CTS(clear to send)/RTS(ready to send)
4. Support configurable depth Data FIFO
5. Support DMA
6. Configuration could be configured through: AXI-4/AXI-Lite/AXI-MailboxFabric

Different version:
1. Full AXI-4
2. AXI-4 + AXI-MailboxFabric
3. AXI-Lite
4. AXI-Lite + AXI-MailboxFabric