rem Put SMS build tools on the path
rem https://github.com/maxim-zhao/sms-build-tools

bmp2tile font.1bpp.png -nomirror -savetiles font.1bpp
bmp2tile mastersystem.1bpp.png -savetiles mastersystem.1bpp
bmp2tile segalogo.1bpp.png -noremovedupes -savetiles segalogo.1bpp
bmp2tile segalogo.4bpp.png -noremovedupes -savetiles segalogo.bin
call Compile "SMS Prototype (M404) [BIOS].sms.asm"