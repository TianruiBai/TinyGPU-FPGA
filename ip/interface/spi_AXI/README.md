## This module is AXI-Lite/AXI-4 interface SPI core
Using AXI-Lite/AXI-4 to provide flexible SPI interface with configurable following:
1. payload size, vary from 1B to 16B
2. Configurable slave number, CHip-select
3. Configurable SPI SCLK frequency.
4. Use AXI-4 bus to control send and receive
5. internal FIFO, and byte-addressable, and very long word memory access write back(more that 4B)
6. Initial support for DMA
7. Configuration could be configured through: AXI-4/AXI-Lite/AXI-MailboxFabric

Different version:
1. Full AXI-4
2. AXI-4 + AXI-MailboxFabric
3. AXI-Lite
4. AXI-Lite + AXI-MailboxFabric