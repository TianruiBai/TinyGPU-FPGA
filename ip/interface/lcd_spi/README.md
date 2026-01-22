## This module design to be drive the ST7735S with SPI interface
Target OLED chipset: ST7735S
Screen resoltuion: 160x80
Display Interface: 4-Wire SPI
Speed: 16-24MHz
Color: RGB565

Bus Interface: AXI-4
Perform Read operation directly from framebuffer memory location.
Draw everything render to framebuffer
double framebuffer, the ouput triggered automaticaly when flip the framebuffer.