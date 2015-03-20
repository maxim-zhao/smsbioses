;--------------------------------------------------------------
; Sega Master System BIOS 1.0
; Dumped by Bock in 2011
; Sources commented by segmtfault in 2015
; orig. ROM detection comments and PSG tones table by Maxim:
; http://www.smspower.org/maxim/forumstuff/psgtable.html
; http://www.smspower.org/forums/6706-USBIOSDisassembly
; * Thanks to Calindro for info about Alex Kidd in M. World
; * Thanks to Calindro & Maxim for info about sound engines
;--------------------------------------------------------------

.MEMORYMAP
DEFAULTSLOT 0
SLOT 0 $0000 $2000
.ENDME

.ROMBANKMAP
BANKSTOTAL 1
BANKSIZE $2000
BANKS 1
.ENDRO

.EMPTYFILL $ff
.DEFINE RAM_Base        $c000
.DEFINE RAM_Size        $2000
.DEFINE ROM_RAM_Offset  $c200 - $0208   ; offset between the DetectCode in ROM and the copy in RAM

.DEFINE MemCtrlPort     $3e
.DEFINE PSGPort         $7f
.DEFINE VDPDataPort     $be
.DEFINE VDPCtrlPort     $bf

.DEFINE VDP_WriteVRAM   $4000
.DEFINE VDP_WriteReg    $8000
.DEFINE VDP_WriteCRAM   $c000
.DEFINE TileMap_Base    $3800
.DEFINE TileMapVRAM     TileMap_Base | VDP_WriteVRAM
.DEFINE SpriteVRAM      $3f00 | VDP_WriteVRAM

.MACRO LD_DE_TILEMAP_XY ARGS x, y
    ld de, TileMapVRAM + (y*32 + x)*2
.ENDM

.MACRO LD_DE_PATTERN_ADDR ARGS idx
    ld de, VDP_WriteVRAM | (idx*32)
.ENDM

.MACRO LD_DE_VDP_REG_VAL ARGS reg, val
    ld de,  VDP_WriteReg | (reg << 8) | val
.ENDM

.MACRO LD_D_VDP_REG_NUM ARGS reg
    ld d, >VDP_WriteReg | reg
.ENDM

.MACRO VDP_REG_VAL ARGS reg, val
.dw VDP_WriteReg | (reg << 8) | val
.ENDM

;-------------------------------------------------------------
;  RAM USAGE
;-------------------------------------------------------------

.ENUM $c000
ROM_HardwareSlot   db                   ; Port $3E value for which we found a ROM header
.ENDE

.ENUM $c100 ASC
Unused             dsb 1
TempVar            db
TileMapHighByte    db
.ENDE

.ENUM $c120 ASC  ; these vars unused
ResetScrollRegFlag db
Unused_C121        dsb 5
ResetScrollX_Val   db
Unused_C127        dsb 3
ResetScrollY_Val   db
.ENDE

.ENUM $c160 ASC
ROM_HeaderCopy     dsb 16
ComputedChecksum   dw
QuestionMarkCoords dw
QuestionMarkBlink  db
.ENDE

.ENUM $c200
DetectCodeRAM      ds 216
.ENDE

.ENUM $c700
UnusedSpriteRAM    dsb 256
.ENDE

; VARIABLES FOR THE SOUND ENGINE THAT IS UNUSED IN THIS BIOS
.STRUCT Voice
flags              db
PSGChannel         db
durationMult       db
dataPtr            dw
toneOffset         db
vibrato            db
envelope           db
volume             db
unused             dsb 1
duration           dw
durCounter         dw
envCounter         db
vibCounter         db
baseTone           dw
PSGTone            dw
deltaTone          dw
PSGVolume          db
unused2            dsb 9
.ENDST

.ENUM $d000 ASC
SongRequest        db
Voice0             INSTANCEOF Voice
Voice1             INSTANCEOF Voice
Voice2             INSTANCEOF Voice
Voice3             INSTANCEOF Voice
.ENDE

;-------------------------------------------------------------

.BANK 0 SLOT 0
.ORG $0000
    di                              ; disable VDP interrupt
    ld hl, PSG_ResetData            ; write 4 bytes from PSG_ResetData to port 7F (PSG)
    ld c, PSGPort                   ; which set the four PSG channels to "volume off"
    ld b, $04
    otir
    im 1                            ; Interrupt Mode 1: executes a RST 38h on interrupts
    ld sp, RAM_Base+RAM_Size-$10    ; set stack near top of RAM
    jr BiosColdBoot

.ORG $0038
MaskableInterrupt:
    jp MaskableInterruptHandler

.ORG $0066
NMI_Pause:
    retn                            ; disable pause button

BiosColdBoot:
    ld hl, VDP_ResetData            ; write 24 bytes from VDP_ResetData to port BF (VDP Control)
    ld bc, $1800 | VDPCtrlPort      ; which initializes VDP registers to their initial values,
    otir                            ; and writes to the VDP data port go to CRAM, 2nd/sprite palette
    xor a
    out (VDPDataPort), a            ; writes 0 to VDP data port => color 0 of sprite palette = black
    ld hl, RAM_Base
    ld de, RAM_Base + 1
    ld bc, RAM_Size - 1
    ld (hl), $00
    ldir                            ; zero out the entire RAM
    ld hl, DetectCode
    ld de, DetectCodeRAM
    ld bc, DetectEnd - DetectCode   ; copy the ROM detection code in RAM
    ldir
    call DetectCodeRAM              ; run detection code from RAM
    ld c, $00
    ld a, (ROM_HardwareSlot)
    or a
    jr z, BiosWait                  ; if no ROM detected, we are going to wait ~1.67 s
    ld a, (ROM_HeaderCopy + $0f)    ; A = last byte of copied potential ROM header
    sub $09
    and $0f
    cp $05
    jr nc, BiosWaitDone             ; jump if ROM size nibble indicates ROM is 64K or bigger
    ld c, a                         ; else: 8KB => BC=1,  16KB => BC=2,  32KB => BC = 3,  48KB => BC = 4
BiosWait:
    ld b, $00
    ld hl, WaitRoutineTable         ; use BC as an index in this table to determine how long to wait:
    add hl, bc                      ; 8KB => 1.55 s, 16KB => 1.43 s, 32KB => 1.19 s, 48KB => 0.95 s
    ld b, (hl)
    call WaitRoutine
BiosWaitDone:
    ld hl, VDP_ResetData            ; reset VDP registers again (because ResetButtonHandler jumps here?)
    ld bc, $1800 | VDPCtrlPort
    otir
    xor a
    out (VDPDataPort), a            ; writes 0 to VDP data port => color 0 of sprite palette = black
    ld hl, InitPaletteData
    ld de, VDP_WriteCRAM
    ld bc, $000d
    call WriteVRAMBlockFromHL       ; initializes first 13 colors of first palette
    ld l, b                         ; L = 0
    LD_DE_PATTERN_ADDR $000
    ld bc, $0020
    call WriteVRAMBlockWithL        ; write 0 in the first 32 bytes of VRAM, so pattern 0 is all 0s (black)
    ld de, TileMapVRAM              ; write in VRAM at address $3800 (tilemap)
    ld bc, $0700                    ; $700 bytes (= length of tilemap: 32 x 28 x 2 bytes)
    call WriteVRAMBlockWithL        ; all zeros (reset tilemap)
    LD_DE_PATTERN_ADDR $3f
    ld bc, $0020                    ; write 0 in 32 bytes of VRAM so that pattern 63 is all black
    call WriteVRAMBlockWithL
    ld a, $d0                       ; init first byte of sprite VRAM (Y for sprite #0) to $d0,
    ld de, SpriteVRAM               ; so sprite #0 (and all remaining sprites) will not be drawn.
    call WriteVRAMByte
    ld hl, FontData
    LD_DE_PATTERN_ADDR $11b
    ld bc, $0228                    ; load $228 = 552 lines of pattern data (69 patterns)
    ld a, $03                       ; with color 3 (white)
    call LoadMonochromePatternData  ; so the pattern for ASCII codes 32-95 is $100 + ASCII code ("A" => pattern $141)
    ld hl, MasterSystemPatternData
    LD_DE_PATTERN_ADDR $088
    ld bc, $06c0                    ; write the 54 patterns for the "MASTER SYSTEM" part of the bumper screen
    call WriteVRAMBlockFromHL       ; stored in uncompressed format...
    ld hl, SegaLogoPatternData
    LD_DE_PATTERN_ADDR $060
    ld bc, $0500                    ; write the 40 patterns of the white and blue SEGA logo
    call WriteVRAMBlockFromHL       ; also stored in uncompressed format
    xor a
    ld (TileMapHighByte), a         ; (write 0 in the high byte of tilemap entries)
    LD_DE_TILEMAP_XY $0b $08        ; write a 4 rows, 10 columns block of tilemap entries
    ld hl, SegaLogoTileMap          ; (X=11,Y=8)-(X=20,Y=11), that show the tiles of the SEGA logo
    ld bc, $040a
    call UpdateTileMapBlock
    LD_DE_TILEMAP_XY $05, $0e       ; write a 3 rows, 23 columns block of tilemap entries
    ld hl, MasterSystemTileMap      ; (X=5,Y=14)-(X=27,Y=16), that show the tiles of the MASTER SYSTEM logo
    ld bc, $0317
    call UpdateTileMapBlock
    call ShowSegaCopyrightString
    call TurnOnDisplay              ; turn on display, but keep frame interrupt disabled
    ld a, (ROM_HardwareSlot)
    or a                            ; if we couldn't find any "TMR SEGA" string at any location for
    jp z, NoRomFound                ; any configuration, go show the instruction screen, else:
    ld hl, (ComputedChecksum)       ; HL = checksum computed
    ld de, (ROM_HeaderCopy + $0a)   ; DE = checksum from potential ROM header
    or a
    sbc hl, de
    jp z, BootGame + ROM_RAM_Offset ; if the checksums match, boot the ROM!
ChecksumMismatch:
    call StartErrorScreen           ; wait ~3.3 s, blank screen, write "MASTER SYSTEM (c) SEGA 1986"
    LD_DE_TILEMAP_XY $02, $06       ; and  "S/R  V1.0" strings
    ld hl, ChecksumErrorString
    ld b, $0f
    call WriteString                ; write the string "CHECK SUM ERROR" at (X=2, Y=6)
    LD_DE_TILEMAP_XY $02, $08       ; infinite loop with '?' blinking at (X=2, Y=8)

ShowQuestionMark:
    ld (QuestionMarkCoords), de
    ld a, '?'
    call WriteVRAMByte              ; write first byte of tilemap entry
    ld a, $01
    out (VDPDataPort), a            ; write second byte of tilemap entry (add $100 to tile # to get "?" tile)
    ld (QuestionMarkBlink), a       ; save the 1 as blinking status ( = on)
InfiniteLoop:
    ld b, $06
    call WaitRoutine                ; wait ~0.7 second
    ld hl, QuestionMarkBlink
    ld a, (hl)
    xor $01                         ; flip blinking status
    ld (hl), a
    ld de, (QuestionMarkCoords)
    inc de                          ; DE = 2nd byte of tilemap entry for "?"
    call WriteVRAMByte              ; => write new blinking status
    jr InfiniteLoop                 ; and keep blinking forever...

ChecksumErrorString:
.db "CHECK SUM ERROR"

NoRomFound:                         ;  blank screen, then at (X=2, Y=2)
    call StartErrorScreen           ; wait ~3.3 s, blank screen, write "MASTER SYSTEM (c) SEGA 1986"
    LD_DE_TILEMAP_XY $02, $06       ; and  "S/R  V1.0" strings
    jr ShowQuestionMark             ; and end in the infinite loop with the blinking question mark

InsertCardOrCartString:             ; UNUSED
.db "INSERT CARD OR CARTRIDGE"

ShowSegaCopyrightString:
    LD_DE_TILEMAP_XY $0a, $15       ; write 11 entries starting at tile coords X=10, Y=21
    ld b, $0b
    ld hl, SegaCopyrightTileMap
WriteString:
    call WriteVDPControlPort
WriteCharacter:
    ld a, (hl)
    out (VDPDataPort), a            ; write first byte of tilemap entry
    push af
    pop af
    inc hl
    ld a, $01                       ; and write $1 for the second byte to add $100 to pattern number
    out (VDPDataPort), a            ; so that the pattern matches the ASCII char
    djnz WriteCharacter
    ret

MasterSystemStringTileMap:
.db "MASTER SYSTEM "
SegaCopyrightTileMap:
.db $1b, " ", $1c, $1d, $1e, $1f, " 1986"  ; ($11b => (c) tile, $11d-$11f = SEGA tiles in the special font)

StartErrorScreen:
    ld b, $1c
    call WaitRoutine                ; wait ~3.3 seconds
    ld l, $00
    ld de, TileMapVRAM              ; reset entire tilemap to 0 (black screen)
    ld bc, $0700
    call WriteVRAMBlockWithL
    ld hl, MasterSystemStringTileMap
    LD_DE_TILEMAP_XY $02, $02       ; at (X=2, Y=2) write the string "MASTER SYSTEM (c) SEGA 1986"
    ld b, $19
    call WriteString
    ld hl, BiosVersionString
    LD_DE_TILEMAP_XY $02, $04      ; at (X=2, Y=4) write the string "S/R  V1.0"
    ld b, $09
    jr WriteString

BiosVersionString:
.db "S/R  V1.0"

.ORG $0208
DetectCode:
    ld hl, Port3E_Values + ROM_RAM_Offset      ; HL points to the array of the 3 port 3E's configs we are going to test
    ld b, $03                                  ; (RAM & card, RAM & cartridge, RAM & expansion in that order)
TestPort3E_ConfigLoop:
    ld a, %11101011
    out (MemCtrlPort), a                       ; enable RAM & I/O only
    ld a, (hl)
    ld (ROM_HardwareSlot), a                   ; enable the configuration to be tested and save it
    out (MemCtrlPort), a
    exx
    call CheckHeaderAt7ff0 + ROM_RAM_Offset
    call CheckHeaderAt3ff0 + ROM_RAM_Offset
    call CheckHeaderAt1ff0 + ROM_RAM_Offset
    exx
    inc hl
    djnz TestPort3E_ConfigLoop
    xor a                                      ; if we arrive  here we didn't find any card, cartridge or expansion
    ld (ROM_HardwareSlot), a                   ; so set ROM_HardwareSlot to zero and go back to BIOS
ReturnToBIOS:
    ld a, %11101011
    out (MemCtrlPort), a                       ; enable RAM and I/O only
    ld a, %11100011
    out (MemCtrlPort), a                       ; enable RAM,I/O and BIOS ROM
    ret                                        ; and return to BIOS!
CheckHeaderAt1ff0:
    ld hl, $1ff0                               ; we are going to check for a valid ROM header at each of
    jr CheckHeader                             ; the three possible locations ($1ff0, $3ff0, $7ff0)
CheckHeaderAt3ff0:
    ld hl, $3ff0
    jr CheckHeader
CheckHeaderAt7ff0:
    ld hl, $7ff0
CheckHeader:
    ld de, ROM_HeaderCopy
    ld bc, $0010                               ; copy 16 bytes from the potential ROM header location in RAM
    ldir
    ld hl, TMR_Sega + ROM_RAM_Offset
    ld de, ROM_HeaderCopy
    ld b, $08
CheckTMR_SegaLoop:                             ; check the presence of the "TMR SEGA" string in the first 8 bytes
    ld a, (de)                                 ; and return if it's not there
    cp (hl)
    ret nz
    inc hl
    inc de
    djnz CheckTMR_SegaLoop
    call CheckRegion + ROM_RAM_Offset          ; passed "TMR SEGA" test, now check region then checksum
    pop af                                     ; (pop the return address so that the ret in ReturnToBIOS
    jr ReturnToBIOS                            ; actually returns to the caller of DetectCode, instead of
                                               ; right after the call to CheckHeaderAt[7/3/1]ff0 if fail)
TMR_Sega:
.db "TMR SEGA"

Port3E_Values:
.db %11001011     ; enable RAM, I/O and card slot only
.db %10101011     ; enable RAM, I/O and cartridge slot only
.db %01101011     ; enable RAM, I/O and expansion slot only

CheckRegion:
    ld de, $0000
    ld a, (ROM_HeaderCopy + $0f)               ; A = last byte of copied potential ROM header
    ld c, a                                    ; save a copy in C (needed later for checksum test)
    and $f0                                    ; extract high nibble which contains region code
    cp $40                                     ; if region code = 4 (SMS Export), we are good
    jr z, DoChecksum                           ; so perform the final test: the checksum test!
    xor a
    ld (ROM_HardwareSlot), a                   ; if region check fails, go back to BIOS with ROM_HardwareSlot = 0
    ret

DoChecksum:
    ld a, c                                    ; A = last byte of potential ROM header
    sub $0a
    and $0f                                    ; extract low nibble (ROM size) and transforms it into
    push af                                    ; an index into the ChecksumRanges table
    ld hl, ChecksumRanges + ROM_RAM_Offset
    ld c, a
    ld b, $00
    add hl, bc                                 ; B = value looked up in ChecksumRanges table
    ld b, (hl)                                 ;   = high byte of upper limit for first 32KB checksum range
    ld c, $f0                                  ; so now BC = upper limit for checksum range (if B = $F, BC = $3FF0)
    ld hl, $0000                               ; (the ROM header is excluded from the checksum)
    call Checksum + ROM_RAM_Offset
    ld (ComputedChecksum), de                  ; store computed checksum so far
    pop af
    sub $04                                    ; if index into the ChecksumRanges table < 4
    ret c                                      ; (i.e. if ROM size is 8, 16, 32 or 48 KB) we are done
    ld c, a
    ld b, $00                                  ; else use the index into the ChecksumRanges table - 4
    ld hl, ChecksumPages + ROM_RAM_Offset      ; as an index into the ChecksumPages table
    add hl, bc
    ld b, (hl)                                 ; so now B contains the number of 16KB pages left to checksum
    ld a, $02                                  ; A = 2 (we are going to start by checksumming ROM page 2)
ChecksumROMPagesLoop:
    push bc
    ld ($ffff), a                              ; maps slot 2 ($8000-$bfff) to ROM bank number in A
    inc a                                      ; (increment bank number for next iteration)
    push af
    call Checksum16KB + ROM_RAM_Offset
    pop af
    pop bc
    djnz ChecksumROMPagesLoop                  ; repeat until we have checksummed all the pages
    ld (ComputedChecksum), de                  ; and save the final checksum
    ret

Checksum16KB:
    ld bc, $4000                               ; we are going to checksum 16KB...
    ld hl, $8000                               ; starting at address $8000

Checksum:                                      ; (note: DE is init. to 0 at the start of CheckRegion)
    ld a, e
    add a, (hl)
    ld e, a                                    ; add byte pointed by HL to current sum in DE
    ld a, d
    adc a, $00
    ld d, a
    inc hl                                     ; move HL to point to next byte
    dec bc                                     ; and decrement number of bytes to sum
    ld a, b
    or c
    jr nz, Checksum                            ; repeat until we have summed all bytes in the range
    ret

ChecksumRanges:      ; high byte of upper limit for first 32KB checksum range
.db $1f              ; ROM size 8KB    ($a) : check 8KB - header
.db $3f              ; ROM size 16KB   ($b) : check 16KB - header
.db $7f              ; ROM size 32KB   ($c) : check 32KB - header
.db $bf              ; ROM size 48KB   ($d) : check 48KB - header, strange since no header is checked for
.db $7f              ; ROM size 64KB   ($e) : check 32KB - header
.db $7f              ; ROM size 128KB  ($f) : check 32KB - header
.db $7f              ; ROM size 256KB  ($0) : check 32KB - header
.db $7f              ; ROM size 512KB  ($1) : check 32KB - header
.db $7f              ; ROM size 1024KB ($2) : check 32KB - header
; range $3 and above will get weird results!

ChecksumPages:       ; how many pages to check for non-first 32KB checksumming
.db $02              ; ROM size 64KB  ($e) : (64KB  - 32KB) / 16KB = 2 pages left to checksum
.db $06              ; ROM size 128KB ($f) : (128KB - 32KB) / 16KB = 6 pages left to checksum
.db $0e              ; ROM size 256KB ($0) : (256KB - 32KB) / 16KB = 14 pages left to checksum
.db $1e              ; ROM size 512KB ($1) : (512KB - 32KB) / 16KB = 30 pages left to checksum
                     ; ROM size 1MB ($2) will grab the first byte of the next opcode which is $3e = 62,
                     ; which is what is wanted ((1024KB - 32KB) / 16KB = 62) This is insane optimisation!

BootGame:
    ld a, %11101011
    out (MemCtrlPort), a                       ; enables RAM and I/O only
    ld a, (ROM_HardwareSlot)                   ; set the port 3E's config where we found a valid ROM to run
    out (MemCtrlPort), a
    jp $0000                                   ; and boot the game!
DetectEnd

WaitRoutine:                            ; runs B iterations of a wait loop that decrements DE from 16K to 0
    ld de, $4000                        ; about 120 ms per loop
WaitLoop:
    dec de
    ld a, d
    or e
    jr nz, WaitLoop
    djnz WaitRoutine
    ret

WaitRoutineTable:                           ; a table of values of B to pass to WaitRoutine
.db $0e, $0d, $0c, $0a, $08, $06

InitPaletteData:
.db $00, $3f, $3e, $3f, $00, $00, $00, $00  ; black, white, pale turquoise, white, black, black, black, black
.db $00, $00, $00, $3f, $30                 ; black, black, black, white, blue

MaskableInterruptHandler: ; UNUSED, the interrupt is never enabled!
    push af
    in a, (VDPCtrlPort)             ; read VDP control port (which also disables int. req. line)
    rlca                            ; bit 7 (1 = VBLANK, 0 = H-Line) in Carry
    jp nc, NotVBlank                ; jump if not VBLANK
    push bc
    push de                         ; save registers
    push hl
    call UpdateSound                ; update sound engine
    pop hl
    pop de                          ; restore registers
    pop bc
    pop af
    ei                              ; enable interrupt and return
    ret
NotVBlank:
    pop af
    ei                              ; enable interrupt and return
    ret

WriteVRAMByte:                      ; write A at VRAM address in DE
    push af
    call WriteVDPControlPort        ; first write the VRAM address
    pop af
    out (VDPDataPort), a            ; then the data!
    ret

TurnOffDisplay:                     ; (UNUSED)
    LD_DE_VDP_REG_VAL $01, $a0      ; write $a0 in VDP reg.1 => turn off display, frame int. enable
    jr WriteVDPControlPort

TurnOnDisplay:
    LD_DE_VDP_REG_VAL $01, $c0      ; write $c0 in VDP reg.1 => turn on display, frame int. disable

WriteVDPControlPort:                ; write register DE to the VDP control port (E, then D)
    ld a, e
    out (VDPCtrlPort), a
    ld a, d
    out (VDPCtrlPort), a
    ret

WriteVRAMBlockFromHL:               ; copy a block of BC bytes of data at HL to VRAM pointed by DE
    call WriteVDPControlPort
    ld a, c
    or a
    jr z, WriteVRAMNoExtraBlock
    inc b
WriteVRAMNoExtraBlock:
    ld a, b                         ; A = number of blocks to write
    ld b, c                         ; B = number of bytes left in this block
    ld c, VDPDataPort
WriteVRAMLoop:
    outi                            ; (VDPDataPort) = (HL);  = B - 1; HL = HL + 1
    jp nz, WriteVRAMLoop
    dec a                           ; finished a block, decrement blocks counter
    jp nz, WriteVRAMLoop
    ret                             ; finished all blocks

WriteVRAMBlockWithL:                ; write value in L to a block of BC bytes of VRAM pointed by DE
    call WriteVDPControlPort        ; write DE to VDP control port to set the address
    ld a, c
    or a                            ; we are going to write BC bytes by blocks of 256 bytes
    jr z, WriteVRAMNoExtraBlock2    ; if BC is not a multiple of 256, we need to count an extra block
    inc b                           ; to transfer the remainder
WriteVRAMNoExtraBlock2:
    ld a, l                         ; write the value in L
WriteVRAMLoop2:
    out (VDPDataPort), a
    dec c
    jr nz, WriteVRAMLoop2
    djnz WriteVRAMLoop2
    ret

; (UNUSED)
; Writes a rectangular block of tilemap entries.
; Height in B, width x2 in C, (start address in VRAM | VDP_WriteVRAM) in DE, and source data in HL
; source data is two bytes per tilemap entry.
; source data layout is width bytes for the first "row", width bytes for the second "row" etc.
UpdateTileMapBlock2:
    push bc
    call WriteVDPControlPort
    ld b, c                         ; width (x2) in B
    ld c, VDPDataPort
WriteTileMapEntry2:                 ; write one row of tilemap entries from data pointed by HL
    outi
    nop
    jr nz, WriteTileMapEntry2
    ex de, hl
    ld bc, $0040
    add hl, bc                      ; advance DE by 64 bytes = 32 tilemap entries
    ex de, hl                       ; so that DE points to the start of the next row in the block
    pop bc
    djnz UpdateTileMapBlock2        ; and keep going until we have written all rows
    ret

; (UNUSED) Same as UpdateTileMapBlock, except the value to write for the second (high) byte of
; each is tilemap entry is passed in A
UpdateTileMapBlock3:
    ld (TileMapHighByte), a         ; save A in TileMapHighByte and fall through to UpdateTileMapBlock

; Writes a rectangular block of tilemap entries.
; Height in B, width in C, (start address in VRAM | VDP_WriteVRAM) in DE, and source data in HL
; source data is one byte per tilemap entry: the pattern number byte.
; The second byte is set for all entries with the value of the TileMapHighByte variable
; source data layout is width bytes for the first "row", width bytes for the second "row" etc.
UpdateTileMapBlock:
    push bc
    call WriteVDPControlPort
    ld b, c                         ; width in B
    ld c, VDPDataPort
WriteTileMapEntry:
    outi                            ; write first byte of tilemap entry from data pointed by HL
    ld a, (TileMapHighByte)
    nop
    out (c), a                      ; write second byte of tilemap entry from value of TileMapHighByte
    nop
    jp nz, WriteTileMapEntry
    ex de, hl
    ld bc, $0040
    add hl, bc                      ; advance DE by 64 bytes = 32 tilemap entries
    ex de, hl                       ; so that DE points to the start of the next row in the block
    pop bc
    djnz UpdateTileMapBlock         ; and keep going until we have written all rows
    ret

; 1BPP Tile Loader (also used in Alex Kidd in Miracle World...)
; Load pattern lines that are all entirely of the same color.
; BC = number of consecutive pattern lines, A = color, HL = start source address in ROM
; DE = start dest. address in VRAM | VDP_WriteVRAM
; Because all of the pixels of a given line have the same color, we need only one byte (instead of 4)
; to describe the 4 bitplanes, achieving a compression ratio of 4x
; e.g: A = color = 9 (1001b) and byte = $3C, the color is a bitplane mask, and we write:
; $3C (bitplane 0), $00 (bitplane 1), $00 (bitplane 2), $3C (bitplane 3)
LoadMonochromePatternData:
    ld (TempVar), a                 ; temp. save color
    call WriteVDPControlPort        ; set VRAM start address from DE
CompressedLineDataLoop:
    ld a, (hl)                      ; fetch a line
    exx                             ; save BC, DE, HL
    ld c, VDPDataPort               ; we will write to VDP data port
    ld b, $04                       ; 4 bitplanes
    ld h, a                         ; H = line
    ld a, (TempVar)                 ; A = color = bitplane mask
BitplanesLoop:
    rra                             ; shift A to test bitplane bit
    ld d, h                         ; if bitplane bit set, write data line to VDP data port
    jr c, WriteBitplaneLine         ; else we write 0
    ld d, $00
WriteBitplaneLine:
    out (c), d
    djnz BitplanesLoop              ; loop to next bitplane
    exx                             ; done with this line, restore BC, DE, HL
    inc hl                          ; increment data source address
    dec bc                          ; decrement source length counter
    ld a, b
    or c
    jp nz, CompressedLineDataLoop   ; and loop until counter is 0
    ret

UpdateSpriteAttrTable:              ; (UNUSED)
    ld hl, UnusedSpriteRAM
    ld de, SpriteVRAM
    ld bc, $6400 | VDPDataPort
    call WriteVDPControlPort        ; writes to data port go to VRAM, at $3f00 = sprite attribute table
    otir                            ; writes 100 bytes from UnusedSpriteRAM to VDP's $3f00
    ld hl, UnusedSpriteRAM + $80
    ld de, SpriteVRAM + $80
    ld b, $80                       ; write to data port go to VRAM, at $3f80 =
    call WriteVDPControlPort        ; 2nd half of sprite attr. table (X + pattern #)
    otir                            ; writes 128 bytes from UnusedSpriteRAM + $80 to VDP's $3f80
    ret

VDP_ResetData:
 VDP_REG_VAL $00, $36   ; reg.0 (Mode Control 1) : don't disable scrolling for cols 24-31 and rows 0-1, mask col 0
                        ; with overscan color from reg. 7, enable line interrupt, don't shift sprites left by 8
                        ; use Mode 4, normal display
 VDP_REG_VAL $01, $80   ; reg.1 (Mode Control 2) : display blanked, disable frame interrupt, sprites are 8x8
 VDP_REG_VAL $02, $ff   ; reg.2 (Name Table Base Address) : tilemap at $3800 (highest 2KB of the 16KB VRAM)
 VDP_REG_VAL $03, $ff   ; reg.3 (Color Table Base Address) : needs to be set to FF or pattern and tilemap data
                        ; will be fetched incorrectly
 VDP_REG_VAL $04, $ff   ; reg.4 (Background Pattern Generator Base Address) : bits 2-0 must be set or pattern data
                        ; and tilemap data will be fetched incorrectly
 VDP_REG_VAL $05, $ff   ; reg.5 (Sprite Attr. Table Base Address) : $3f00 (unused top 256 bytes of tilemap memory)
 VDP_REG_VAL $06, $fb   ; reg.6 (Sprite Pattern Generator Base Address) : sprite patterns in first 8 KB of VRAM
 VDP_REG_VAL $08, $00   ; reg.8 (Background X Scroll) : no scrolling
 VDP_REG_VAL $09, $00   ; reg.9 (Background Y Scroll) : no scrolling
 VDP_REG_VAL $0a, $00   ; reg.A (Line Counter) line counter value = 0
 VDP_REG_VAL $07, $00   ; reg.7  (Overscan/Backdrop Color) : color 0 (from sprite palette)
.dw VDP_WriteCRAM + $10 ; Writes to the data port goes to CRAM starting at byte 16 (start of 2nd/sprite palette)

ResetScrollRegisters: ; (UNUSED)
    xor a
    ld (ResetScrollY_Val), a        ; zeroes these two variables
    ld (ResetScrollX_Val), a
    di
    ld hl, ResetScrollRegFlag
    ld a, (hl)                      ; if ResetScrollRegFlag = 0, return
    or a
    ret z
    ld (hl), $00                    ; else set the ResetScrollRegFlag to 0
    ld a, (ResetScrollY_Val)
    ld e, a
    LD_D_VDP_REG_NUM $09
    call WriteVDPControlPort        ; write 0 to VDP register #9 (Y scroll)
    ld a, (ResetScrollX_Val)
    ld e, a
    dec d
    jp WriteVDPControlPort          ; write 0 to VDP register #8 (X scroll)

FontData:
; Monochrome, 4x compression. Contains SEGA copyright, and font for ASCII codes 32-95
.db $3c, $42, $99, $a1, $a1, $99, $42, $3c      ; copyright sign
.db $7c, $fc, $e0, $f8, $7c, $1c, $fc, $f8      ; "Sega" S
.db $7c, $fc, $c0, $f8, $f8, $c0, $fc, $7c      ; "Sega" E
.db $7c, $fc, $c0, $dc, $dc, $cc, $fc, $7c      ; "Sega" G
.db $38, $38, $7c, $5c, $5c, $ce, $be, $be      ; "Sega" A
.db $00, $00, $00, $00, $00, $00, $00, $00      ; empty/space
.db $18, $3c, $3c, $3c, $18, $00, $18, $00      ; !
.db $6c, $6c, $48, $00, $00, $00, $00, $00      ; "
.db $6c, $fe, $6c, $6c, $6c, $fe, $6c, $00      ; #
.db $18, $3e, $58, $3c, $1a, $7c, $18, $00      ; $
.db $00, $c6, $cc, $18, $30, $66, $c6, $00      ; %
.db $70, $c8, $c8, $70, $9a, $8c, $76, $00      ; &
.db $18, $18, $10, $00, $00, $00, $00, $00      ; '
.db $0c, $18, $30, $30, $30, $18, $0c, $00      ; (
.db $30, $18, $0c, $0c, $0c, $18, $30, $00      ; )
.db $00, $18, $5a, $3c, $3c, $5a, $18, $00      ; *
.db $00, $18, $18, $7e, $18, $18, $00, $00      ; +
.db $00, $00, $00, $00, $18, $18, $08, $10      ; ,
.db $00, $00, $00, $7c, $00, $00, $00, $00      ; -
.db $00, $00, $00, $00, $00, $18, $18, $00      ; .
.db $00, $06, $0c, $18, $30, $60, $c0, $00      ; /
.db $38, $4c, $c6, $c6, $c6, $64, $38, $00      ; 0
.db $18, $38, $18, $18, $18, $18, $7e, $00      ; 1
.db $7c, $c6, $0e, $3c, $78, $e0, $fe, $00      ; 2
.db $7e, $0c, $18, $3c, $06, $c6, $7c, $00      ; 3
.db $1c, $3c, $6c, $cc, $fe, $0c, $0c, $00      ; 4
.db $fc, $c0, $fc, $06, $06, $c6, $7c, $00      ; 5
.db $3c, $60, $c0, $fc, $c6, $c6, $7c, $00      ; 6
.db $fe, $c6, $0c, $18, $30, $30, $30, $00      ; 7
.db $7c, $c6, $c6, $7c, $c6, $c6, $7c, $00      ; 8
.db $7c, $c6, $c6, $7e, $06, $0c, $78, $00      ; 9
.db $00, $18, $18, $00, $18, $18, $00, $00      ; :
.db $00, $18, $18, $00, $18, $18, $08, $10      ; ;
.db $0c, $18, $30, $60, $30, $18, $0c, $00      ; <
.db $00, $00, $7c, $00, $7c, $00, $00, $00      ; =
.db $60, $30, $18, $0c, $18, $30, $60, $00      ; >
.db $7c, $c6, $06, $1c, $30, $00, $30, $00      ; ?
.db $7c, $c6, $06, $66, $d6, $d6, $7c, $00      ; @
.db $38, $6c, $c6, $c6, $fe, $c6, $c6, $00      ; A
.db $fc, $c6, $c6, $fc, $c6, $c6, $fc, $00      ; B
.db $3c, $66, $c0, $c0, $c0, $66, $3c, $00      ; C
.db $f8, $cc, $c6, $c6, $c6, $cc, $f8, $00      ; D
.db $fe, $c0, $c0, $f8, $c0, $c0, $fe, $00      ; E
.db $fe, $c0, $c0, $f8, $c0, $c0, $c0, $00      ; F
.db $3e, $60, $c0, $ce, $c6, $66, $3e, $00      ; G
.db $c6, $c6, $c6, $fe, $c6, $c6, $c6, $00      ; H
.db $7e, $18, $18, $18, $18, $18, $7e, $00      ; I
.db $06, $06, $06, $06, $06, $c6, $7c, $00      ; J
.db $c6, $cc, $d8, $f0, $f8, $dc, $ce, $00      ; K
.db $c0, $c0, $c0, $c0, $c0, $c0, $fe, $00      ; L
.db $c6, $ee, $fe, $fe, $d6, $c6, $c6, $00      ; M
.db $c6, $e6, $f6, $fe, $de, $ce, $c6, $00      ; N
.db $7c, $c6, $c6, $c6, $c6, $c6, $7c, $00      ; O
.db $fc, $c6, $c6, $c6, $fc, $c0, $c0, $00      ; P
.db $7c, $c6, $c6, $c6, $de, $cc, $76, $00      ; Q
.db $fc, $c6, $c6, $ce, $f8, $dc, $ce, $00      ; R
.db $78, $cc, $c0, $7c, $06, $c6, $7c, $00      ; S
.db $7e, $18, $18, $18, $18, $18, $18, $00      ; T
.db $c6, $c6, $c6, $c6, $c6, $c6, $7c, $00      ; U
.db $c6, $c6, $c6, $ee, $7c, $38, $10, $00      ; V
.db $c6, $c6, $d6, $fe, $fe, $6c, $44, $00      ; W
.db $c6, $ee, $7c, $38, $7c, $ee, $c6, $00      ; X
.db $66, $66, $66, $3c, $18, $18, $18, $00      ; Y
.db $fe, $0e, $1c, $38, $70, $e0, $fe, $00      ; Z
.db $f8, $c0, $c0, $c0, $c0, $c0, $f8, $00      ; [
.db $00, $80, $40, $20, $10, $08, $00, $00      ; \
.db $f8, $18, $18, $18, $18, $18, $f8, $00      ; ]
.db $00, $00, $20, $50, $88, $00, $00, $00      ; ^
.db $00, $00, $00, $00, $00, $00, $00, $f8      ; _

SegaLogoPatternData: ; uncompressed format
.db $07, $07, $00, $07, $1c, $1c, $03, $1f, $30, $30, $0f, $3f, $60, $60, $1f, $7f
.db $41, $41, $3e, $7f, $c6, $c6, $39, $ff, $84, $84, $7b, $ff, $88, $88, $77, $ff
.db $ff, $ff, $00, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $ff, $ff, $00, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $e1, $e1, $00, $e1, $27, $27, $c0, $e7, $2c, $2c, $c3, $ef, $38, $38, $c7, $ff
.db $f0, $f0, $0f, $ff, $31, $31, $ce, $ff, $21, $21, $de, $ff, $22, $22, $dd, $ff
.db $ff, $ff, $00, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $7f, $7f, $80, $ff, $80, $80, $7f, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $f8, $f8, $00, $f8, $09, $09, $f0, $f9, $0b, $0b, $f0, $fb, $0e, $0e, $f1, $ff
.db $fc, $fc, $03, $ff, $0c, $0c, $f3, $ff, $08, $08, $f7, $ff, $08, $08, $f7, $ff
.db $7f, $7f, $00, $7f, $c0, $c0, $3f, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $1f, $1f, $e0, $ff, $60, $60, $9f, $ff, $40, $40, $bf, $ff, $80, $80, $7f, $ff
.db $fe, $fe, $00, $fe, $02, $02, $fc, $fe, $02, $02, $fc, $fe, $02, $02, $fc, $fe
.db $fe, $fe, $00, $fe, $02, $02, $fc, $fe, $02, $02, $fc, $fe, $02, $02, $fc, $fe
.db $03, $03, $00, $03, $0e, $0e, $01, $0f, $08, $08, $07, $0f, $18, $18, $07, $1f
.db $11, $11, $0e, $1f, $31, $31, $0e, $3f, $22, $22, $1d, $3f, $22, $22, $1d, $3f
.db $e0, $e0, $00, $e0, $39, $39, $c0, $f9, $0f, $0f, $f0, $ff, $0e, $0e, $f1, $ff
.db $c6, $c6, $39, $ff, $46, $46, $b9, $ff, $22, $22, $dd, $ff, $23, $23, $dc, $ff
.db $fc, $fc, $00, $fc, $86, $86, $78, $fe, $7b, $7b, $84, $ff, $8d, $8d, $72, $ff
.db $b5, $b5, $4a, $ff, $8d, $8d, $72, $ff, $b5, $b5, $4a, $ff, $7b, $7b, $84, $ff
.db $88, $88, $77, $ff, $88, $88, $77, $ff, $84, $84, $7b, $ff, $c6, $c6, $39, $ff
.db $41, $41, $3e, $7f, $60, $60, $1f, $7f, $30, $30, $0f, $3f, $1c, $1c, $03, $1f
.db $ff, $ff, $00, $ff, $07, $07, $f8, $ff, $01, $01, $fe, $ff, $00, $00, $ff, $ff
.db $f0, $f0, $0f, $ff, $0c, $0c, $f3, $ff, $04, $04, $fb, $ff, $02, $02, $fd, $ff
.db $e2, $e2, $1d, $ff, $e2, $e2, $1d, $ff, $e2, $e2, $1d, $ff, $e2, $e2, $1d, $ff
.db $63, $63, $9c, $ff, $62, $62, $9d, $ff, $22, $22, $dd, $ff, $22, $22, $dd, $ff
.db $3f, $3f, $c0, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $ff, $ff, $00, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $f8, $f8, $07, $ff, $18, $18, $e7, $ff, $18, $18, $e7, $ff, $18, $18, $e7, $ff
.db $f8, $f8, $07, $ff, $18, $18, $e7, $ff, $18, $18, $e7, $ff, $18, $18, $e7, $ff
.db $8f, $8f, $70, $ff, $88, $88, $77, $ff, $88, $88, $77, $ff, $88, $88, $77, $ff
.db $8f, $8f, $70, $ff, $88, $88, $77, $ff, $88, $88, $77, $ff, $88, $88, $77, $ff
.db $fe, $fe, $00, $fe, $02, $02, $fc, $fe, $02, $02, $fc, $fe, $02, $02, $fc, $fe
.db $e2, $e2, $1c, $fe, $22, $22, $dc, $fe, $23, $23, $dc, $ff, $23, $23, $dc, $ff
.db $62, $62, $1d, $7f, $44, $44, $3b, $7f, $44, $44, $3b, $7f, $c4, $c4, $3b, $ff
.db $88, $88, $77, $ff, $88, $88, $77, $ff, $88, $88, $77, $ff, $11, $11, $ee, $ff
.db $23, $23, $dc, $ff, $11, $11, $ee, $ff, $11, $11, $ee, $ff, $11, $11, $ee, $ff
.db $88, $88, $77, $ff, $88, $88, $77, $ff, $88, $88, $77, $ff, $c4, $c4, $3b, $ff
.db $86, $86, $78, $fe, $fc, $fc, $00, $fc, $00, $00, $00, $00, $80, $80, $00, $80
.db $80, $80, $00, $80, $80, $80, $00, $80, $c0, $c0, $00, $c0, $40, $40, $80, $c0
.db $ff, $ff, $00, $ff, $80, $80, $7f, $ff, $80, $80, $7f, $ff, $80, $80, $7f, $ff
.db $ff, $ff, $00, $ff, $80, $80, $7f, $ff, $80, $80, $7f, $ff, $80, $80, $7f, $ff
.db $e2, $e2, $1d, $ff, $02, $02, $fd, $ff, $04, $04, $fb, $ff, $0c, $0c, $f3, $ff
.db $f0, $f0, $0f, $ff, $00, $00, $ff, $ff, $01, $01, $fe, $ff, $07, $07, $f8, $ff
.db $22, $22, $dd, $ff, $22, $22, $dd, $ff, $21, $21, $de, $ff, $71, $71, $8e, $ff
.db $70, $70, $8f, $ff, $d8, $d8, $07, $df, $8c, $8c, $03, $8f, $07, $07, $00, $07
.db $3f, $3f, $c0, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $80, $80, $7f, $ff
.db $7f, $7f, $80, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $f8, $f8, $07, $ff, $08, $08, $f7, $ff, $08, $08, $f7, $ff, $0c, $0c, $f3, $ff
.db $fc, $fc, $03, $ff, $0e, $0e, $f1, $ff, $0b, $0b, $f0, $fb, $09, $09, $f0, $f9
.db $8e, $8e, $71, $ff, $80, $80, $7f, $ff, $40, $40, $bf, $ff, $60, $60, $9f, $ff
.db $1f, $1f, $e0, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $c0, $c0, $3f, $ff
.db $23, $23, $dc, $ff, $23, $23, $dc, $ff, $22, $22, $dd, $ff, $22, $22, $dd, $ff
.db $e2, $e2, $1d, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $11, $11, $ee, $ff, $10, $10, $ef, $ff, $22, $22, $dd, $ff, $22, $22, $dd, $ff
.db $23, $23, $dc, $ff, $46, $46, $b9, $ff, $46, $46, $b9, $ff, $46, $46, $b9, $ff
.db $c4, $c4, $3b, $ff, $04, $04, $fb, $ff, $02, $02, $fd, $ff, $02, $02, $fd, $ff
.db $fe, $fe, $01, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff, $00, $00, $ff, $ff
.db $40, $40, $80, $c0, $60, $60, $80, $e0, $20, $20, $c0, $e0, $20, $20, $c0, $e0
.db $30, $30, $c0, $f0, $10, $10, $e0, $f0, $10, $10, $e0, $f0, $10, $10, $e0, $f0
.db $ff, $ff, $00, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $fc, $fc, $00, $fc, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $01, $01, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $ff, $ff, $00, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $f8, $f8, $00, $f8, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $7f, $7f, $00, $7f, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $ff, $ff, $00, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $ff, $ff, $00, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $ff, $ff, $00, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $f0, $f0, $00, $f0, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

SegaLogoTileMap:
.db $60, $61, $62, $63, $64, $65, $66, $67, $68, $69
.db $6a, $6b, $6c, $6d, $6e, $6f, $70, $71, $72, $73
.db $74, $75, $76, $77, $78, $79, $7a, $7b, $7c, $7d
.db $7e, $7f, $80, $81, $82, $83, $84, $85, $86, $87

MasterSystemPatternData: ; uncompressed format
.db $7c, $80, $00, $00, $3c, $40, $00, $00, $3c, $00, $00, $00, $3c, $00, $00, $00
.db $3c, $02, $00, $00, $3e, $00, $00, $00, $3e, $00, $00, $00, $36, $08, $00, $00
.db $1f, $00, $00, $00, $1e, $01, $00, $00, $1e, $00, $00, $00, $1e, $00, $00, $00
.db $1e, $20, $00, $00, $3e, $00, $00, $00, $3e, $00, $00, $00, $2e, $10, $00, $00
.db $01, $80, $00, $00, $01, $00, $00, $00, $01, $02, $00, $00, $03, $00, $00, $00
.db $03, $00, $00, $00, $03, $04, $00, $00, $06, $01, $00, $00, $06, $00, $00, $00
.db $e0, $00, $00, $00, $e0, $00, $00, $00, $e0, $10, $00, $00, $70, $80, $00, $00
.db $70, $08, $00, $00, $78, $00, $00, $00, $38, $40, $00, $00, $38, $00, $00, $00
.db $03, $04, $00, $00, $0f, $10, $00, $00, $1c, $02, $00, $00, $18, $24, $00, $00
.db $38, $04, $00, $00, $38, $04, $00, $00, $3c, $02, $00, $00, $3f, $00, $00, $00
.db $90, $41, $00, $00, $f1, $00, $00, $00, $31, $4a, $00, $00, $1b, $20, $00, $00
.db $0a, $11, $00, $00, $08, $10, $00, $00, $00, $00, $00, $00, $80, $00, $00, $00
.db $ff, $00, $00, $00, $ff, $00, $00, $00, $dd, $22, $00, $00, $1c, $80, $00, $00
.db $1c, $00, $00, $00, $1c, $00, $00, $00, $1c, $00, $00, $00, $1c, $00, $00, $00
.db $87, $48, $00, $00, $c3, $04, $00, $00, $c3, $20, $00, $00, $63, $80, $00, $00
.db $23, $40, $00, $00, $03, $00, $00, $00, $03, $00, $00, $00, $03, $00, $00, $00
.db $ff, $00, $00, $00, $ff, $00, $00, $00, $83, $04, $00, $00, $81, $02, $00, $00
.db $80, $01, $00, $00, $80, $00, $00, $00, $84, $00, $00, $00, $84, $08, $00, $00
.db $1f, $20, $00, $00, $0f, $90, $00, $00, $8e, $00, $00, $00, $8e, $00, $00, $00
.db $8e, $00, $00, $00, $0e, $00, $00, $00, $0e, $00, $00, $00, $0e, $00, $00, $00
.db $f0, $08, $00, $00, $f8, $04, $00, $00, $3c, $40, $00, $00, $0c, $12, $00, $00
.db $0e, $10, $00, $00, $0e, $00, $00, $00, $0e, $10, $00, $00, $0c, $12, $00, $00
.db $07, $08, $00, $00, $1f, $20, $00, $00, $38, $04, $00, $00, $30, $48, $00, $00
.db $70, $08, $00, $00, $70, $08, $00, $00, $78, $04, $00, $00, $7f, $00, $00, $00
.db $27, $88, $00, $00, $e3, $04, $00, $00, $61, $92, $00, $00, $31, $40, $00, $00
.db $10, $21, $00, $00, $10, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $c3, $24, $00, $00, $81, $42, $00, $00, $c3, $00, $00, $00, $c3, $24, $00, $00
.db $e6, $01, $00, $00, $e6, $18, $00, $00, $7c, $82, $00, $00, $7c, $00, $00, $00
.db $c3, $24, $00, $00, $8f, $50, $00, $00, $1c, $82, $00, $00, $18, $24, $00, $00
.db $38, $04, $00, $00, $38, $04, $00, $00, $3c, $02, $00, $00, $3f, $00, $00, $00
.db $1f, $20, $00, $00, $0f, $90, $00, $00, $8f, $00, $00, $00, $8f, $00, $00, $00
.db $8f, $00, $00, $00, $0f, $00, $00, $00, $0f, $00, $00, $00, $0d, $02, $00, $00
.db $07, $00, $00, $00, $07, $00, $00, $00, $07, $00, $00, $00, $07, $00, $00, $00
.db $07, $88, $00, $00, $8f, $00, $00, $00, $8f, $00, $00, $00, $8b, $04, $00, $00
.db $c0, $20, $00, $00, $80, $40, $00, $00, $80, $00, $00, $00, $80, $00, $00, $00
.db $80, $00, $00, $00, $80, $00, $00, $00, $80, $00, $00, $00, $80, $00, $00, $00
.db $36, $01, $00, $00, $37, $00, $00, $00, $37, $00, $00, $00, $33, $04, $00, $00
.db $33, $00, $00, $00, $33, $00, $00, $00, $33, $00, $00, $00, $31, $02, $00, $00
.db $2e, $40, $00, $00, $6e, $00, $00, $00, $6e, $00, $00, $00, $4e, $a0, $00, $00
.db $ce, $00, $00, $00, $ce, $00, $00, $00, $ce, $00, $00, $00, $8e, $40, $00, $00
.db $06, $00, $00, $00, $06, $08, $00, $00, $0c, $02, $00, $00, $0f, $00, $00, $00
.db $0f, $00, $00, $00, $0c, $10, $00, $00, $18, $04, $00, $00, $18, $00, $00, $00
.db $38, $04, $00, $00, $3c, $00, $00, $00, $1c, $20, $00, $00, $fc, $00, $00, $00
.db $fc, $02, $00, $00, $1e, $00, $00, $00, $0e, $10, $00, $00, $0e, $00, $00, $00
.db $1f, $00, $00, $00, $0f, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00
.db $20, $10, $00, $00, $20, $10, $00, $00, $30, $08, $00, $00, $18, $20, $00, $00
.db $e0, $00, $00, $00, $f0, $00, $00, $00, $f0, $08, $00, $00, $78, $80, $00, $00
.db $38, $40, $00, $00, $38, $40, $00, $00, $38, $40, $00, $00, $70, $88, $00, $00
.db $1c, $00, $00, $00, $1c, $00, $00, $00, $1c, $00, $00, $00, $1c, $00, $00, $00
.db $1c, $00, $00, $00, $1c, $00, $00, $00, $1c, $00, $00, $00, $1c, $00, $00, $00
.db $03, $00, $00, $00, $03, $00, $00, $00, $03, $00, $00, $00, $03, $00, $00, $00
.db $03, $00, $00, $00, $03, $00, $00, $00, $03, $00, $00, $00, $03, $00, $00, $00
.db $fc, $00, $00, $00, $fc, $00, $00, $00, $84, $08, $00, $00, $84, $00, $00, $00
.db $80, $00, $00, $00, $80, $00, $00, $00, $81, $00, $00, $00, $83, $00, $00, $00
.db $0e, $00, $00, $00, $0f, $00, $00, $00, $0f, $00, $00, $00, $0e, $00, $00, $00
.db $0e, $00, $00, $00, $ce, $00, $00, $00, $8e, $40, $00, $00, $8e, $00, $00, $00
.db $3c, $40, $00, $00, $f8, $04, $00, $00, $f0, $08, $00, $00, $38, $40, $00, $00
.db $38, $44, $00, $00, $1c, $20, $00, $00, $1c, $02, $00, $00, $0e, $10, $00, $00
.db $3f, $00, $00, $00, $1f, $00, $00, $00, $07, $00, $00, $00, $00, $01, $00, $00
.db $40, $20, $00, $00, $40, $20, $00, $00, $60, $10, $00, $00, $30, $41, $00, $00
.db $c0, $00, $00, $00, $e0, $00, $00, $00, $e0, $10, $00, $00, $f0, $00, $00, $00
.db $70, $80, $00, $00, $70, $80, $00, $00, $70, $80, $00, $00, $e0, $10, $00, $00
.db $38, $44, $00, $00, $38, $00, $00, $00, $38, $00, $00, $00, $38, $00, $00, $00
.db $38, $00, $00, $00, $38, $00, $00, $00, $38, $00, $00, $00, $38, $00, $00, $00
.db $0d, $00, $00, $00, $0d, $00, $00, $00, $0d, $00, $00, $00, $0c, $01, $00, $00
.db $0c, $00, $00, $00, $cc, $00, $00, $00, $8c, $40, $00, $00, $8c, $00, $00, $00
.db $8b, $50, $00, $00, $db, $00, $00, $00, $db, $00, $00, $00, $d3, $28, $00, $00
.db $f3, $00, $00, $00, $f3, $00, $00, $00, $f3, $00, $00, $00, $63, $90, $00, $00
.db $80, $00, $00, $00, $80, $00, $00, $00, $80, $00, $00, $00, $80, $00, $00, $00
.db $80, $00, $00, $00, $80, $00, $00, $00, $80, $00, $00, $00, $80, $00, $00, $00
.db $31, $48, $00, $00, $79, $84, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $8e, $11, $00, $00, $9f, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $18, $24, $00, $00, $3c, $c2, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $0e, $11, $00, $00, $1f, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $1f, $00, $00, $00, $13, $84, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $e0, $10, $00, $00, $c0, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $1c, $22, $00, $00, $3e, $41, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $03, $04, $00, $00, $07, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $ff, $00, $00, $00, $ff, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $0e, $91, $00, $00, $1f, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $0e, $01, $00, $00, $07, $88, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $3f, $00, $00, $00, $27, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $c0, $20, $00, $00, $80, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $38, $44, $00, $00, $fe, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $1f, $00, $00, $00, $13, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $0c, $92, $00, $00, $1e, $21, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $63, $04, $00, $00, $67, $08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $80, $40, $00, $00, $c0, $20, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

MasterSystemTileMap:
.db $88, $89, $8a, $8b, $8c, $8d, $8e, $8f, $90, $91, $92, $00, $93, $94, $95, $96, $8d, $8e, $8f, $90, $97, $98, $99
.db $9a, $9b, $9c, $9d, $9e, $9f, $a0, $a1, $a2, $a3, $a4, $00, $a5, $a6, $a7, $9e, $9f, $a0, $a1, $a2, $a8, $a9, $aa
.db $ab, $ac, $ad, $ae, $af, $b0, $b1, $b2, $b3, $b4, $b5, $b6, $b7, $b8, $b9, $ba, $b0, $b1, $b2, $b3, $bb, $bc, $bd

;---------------------------------------------------------------------------------------------------------
; SOUND CODE AND DATA START HERE, ALL OF IT UNUSED IN THIS BIOS, LATER USED IN 1.3 AND LATER
;
; This looks like a predecessor to sound engines used in Choplifter, Pro Wrestling, Alex Kidd in M. World
; Song format is as follows:
; first byte is the number of voices (always 4 in this BIOS)
; then 9 bytes per voice:
; byte 0: flags byte:
;         * bit 7: if set, update the voice
;         * bit 6: if set, don't write tones to PSG (debug/dev flag?)
;         * bit 5: if set, we are in "tone ramp" mode (cf. explanation below)
;         * bit 3: if set, tones are encoded directly in the sound data (instead of using the tone table)
;         * bit 2: if set, disable all writes to PSG (debug/dev flag?)
; byte 1: low nibble = PSG channel mapping (e.g. if byte = $21, this voice is mapped to PSG channel 1)
; byte 2: duration multiplier; durations in the sound data for this voice are multiplied by this value
; bytes 3-4: pointer to sound data for this voice
; byte 5: offset into the tone table; added to tone indices in the sound data for this voice
; byte 6: index of vibrato curve to use +1; in this BIOS always 0 (no vibrato) or 1, as there is only
;         one vibrato curve defined in the BIOS data
; byte 7: index of envelope to use +1; in this BIOS 4 envelopes are defined
; byte 8: base volume of this voice
;
; Format for the sound data of each voice is a sequence of bytes with the possible following values:
; * $00-$7f: a duration value (in VBlanks)
; * $80: silence/rest (treated like a tone index)
; * $81-$df: an index in the tone table ($81 is tone 1, $82 is tone 2,...)
; * $e0-$ff: index to a sound routine/control flag.
;   Only $e0-$e5 and $e7 are used in this BIOS. There is code for $e6 and $e8 but seems unreferenced.
;   They are (with $nn representing an argument byte):
;   $e0 $nn: Set base volume to $nn
;   $e1: end of sound voice
;   $e2 $nn: write to PSG noise "tone" reg., and if %11 allow write to tone2 from noise voice
;   $e3 $nn: use envelope $nn
;   $e4 $nn $nn: jump current pointer in voice to the address provided. Used to loop the snail maze song
;   $e5: deactivate tone ramp mode
;   $e6 $nn $nn $nn: some kind of for loop using $e4 above
;   $e7 $nn: Set base volume to min(base volume + $nn, $0e)
;   $e8 $nn: add $nn to offset into the tone table
;
;   To trigger a song, write the song index + $81 at address $d000 (SongRequest)
;
; About the tone ramp mode: it takes as inputs two tone values (base tone and delta tone) and a duration
; The sign of difference between the two tones indicates the direction of the ramp (if the difference is
; negative, the output tone value will go down over the duration, so the pitch will go up)
; Over the duration, the output tone will be interpolated linearly from the base tone to a new tone
; which has the same low byte as the delta tone, and the closest high byte that matches the direction of
; the ramp. For example:
; * if base tone = 0261 and delta tone = 004C, the output tone will go down linearly from 0261 to 024C
; * if base tone = 0261 and delta tone = 044C, the output tone will go up linearly from 0261 to 034C
;---------------------------------------------------------------------------------------------------------

UpdateSound:
    call LoadSongData               ; Load song/queue deta if a play request is pending
    ld ix, Voice0                   ; base address of the voice structs (32 bytes / voice)
    ld b, $04
UpdateSoundLoop:                    ; for each of the 4 voices:
    push bc
    bit 7,(ix + Voice.flags)        ; update voice if bit 7 of flag byte set
    call nz, UpdateVoice
    ld de, $20
    add ix, de                      ; move to next voice buffer
    pop bc
    djnz UpdateSoundLoop
    ret

LoadSongData:
    ld a, (SongRequest)
    cp $80
    jp c, ClearSound                ; if bit 7 not set, clear voice structs, silence PSG and we are done
    jp z, NoSongDataToLoad          ; if byte = $80, nothing to load, keep playing
    ld de, Voice0                   ; if byte > $80, load song data. Set DE to address of first voice buffer
    sub $81                         ; Calculate song index in song table  (A = $81 => song #0, $82 => song #1, etc.)
    ld hl, SongTable
    ld b, $00
    add a, a
    ld c, a
    add hl, bc                      ; HL points to SongTable[songIndex]
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a                         ; HL = SongTable[songIndex] = adress of song data
    ld b, (hl)                      ; B = 1st byte song data = number of voices for this song
    inc hl                          ; and now HL points to the data for the first voice in the song data structure
LoadVoiceData:
    push bc                         ; (save num. voices/loop counter)
    ld bc, $0009                    ; copy 9 bytes of voice data to the start of the voice buffer
    ldir
    ld a, $20
    ld (de), a                      ; append the value $20 to the voice buffer (byte 9) ????
    inc de
    ld a, $01                       ; append the word $0001 to the voice buffer
    ld (de), a                      ; this is the original value for the note duration, setting it to 1
    inc de                          ; ensures we will fetch the first sound data byte on this update
    xor a
    ld (de), a
    inc de
    ld (de), a                      ; append the word $0000 to the voice buffer, to initialize the voice's duration counter
    inc de
    ld (de), a
    push hl                         ; so 14 bytes written in all
    ld hl, $0012                    ; (the 18 remaining bytes were initialized to 0 when we zeroed all the RAM)
    add hl, de
    ex de, hl
    pop hl
    inc de                          ; move DE forward 19 bytes to move it to next voice buffer
    pop bc
    djnz LoadVoiceData              ; and load data for next voice
NoSongDataToLoad:
    ld a, $80
    ld (SongRequest), a             ; we are done buffering song data for the 4 channels, so set request byte to $80
    ret                             ; so we don't reload the song each frame

UpdateVoice:
    ld e, (ix + Voice.durCounter)
    ld d, (ix + Voice.durCounter+1)
    inc de
    ld (ix + Voice.durCounter), e   ; increment and update voice's duration counter
    ld (ix + Voice.durCounter+1), d
    ld l, (ix + Voice.duration)     ; HL = currently stored note duration
    ld h, (ix + Voice.duration+1)
    or a                            ; (to set Carry to 0 as there is no sub hl, de instruction)
    sbc hl, de
    call z, GetNextSoundDataByte    ; if duration counter equals note duration, time to grab next sound data byte!
    ld e, (ix + Voice.baseTone)
    ld d, (ix + Voice.baseTone+1)   ; DE = base tone value
    ld a, e
    or d
    jr nz, NonZeroBaseTone
    ld (ix + Voice.PSGVolume), $0f  ; if base tone = 0 (i.e. sound data byte was $80), silence the corresponding PSG channel
    jp PSGWriteVolume               ; by writing $f for its volume, and we are done for this voice's update
NonZeroBaseTone:
    bit 5, (ix + Voice.flags)       ; if we are in tone ramp mode, update position on the ramp, derive new tone
    jr nz, UpdateToneRamp
    ld a, (ix + Voice.vibrato)      ; A = index vibrato curve (+1)
    or a
    jr nz, VibratoStage             ; if index valid, apply vibrato
    ld (ix + Voice.PSGTone), e      ; else we set the base tone value as the final tone value to send to the PSG
    ld (ix + Voice.PSGTone+1), d
    jp VolumeStage

ReadFromWordTable:                  ; HL = word at offset A-1 in the word table at address HL
    dec a
    ld c, a
    ld b, $00
    add hl, bc
    add hl, bc
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ret

VibratoStage:
    ld hl, VibratoCurvesTable
    call ReadFromWordTable          ; HL = adress of vibrato curve to apply
    call ApplyVibrato               ; (always VibratoCurve since VibratoCuvesTable has only one entry here)
    jr VolumeStage

UpdateToneRamp:
    push de                         ; (save base tone for AddRampValToBaseTone)
    ld l, (ix + Voice.deltaTone)
    ld h, (ix + Voice.deltaTone+1)  ; HL = delta tone value
    or a                            ; (reset Carry as there is no sub hl, de instruction)
    sbc hl, de                      ; HL = delta tone - base tone (let's call this value "diff")
    push af                         ; save flags
    ld a, l                         ; A = low byte diff (= diff % 256)
    jp p, ToneDiffPositive          ; jump if diff positive
    neg                             ; A = -A
ToneDiffPositive:
    ld h, a                         ; H = abs(diff) mod 256
    ld e, (ix + Voice.durCounter)   ; E = low byte of voice's duration counter
    call MultiplyHbyE               ; HL = H * E
    ld e, (ix + Voice.duration)     ; E = low byte of voice's note duration
    call DivideHLbyE
    ld e, a
    ld d, $00                       ; so the value in DE is (abs(diff) mod 256) interpolated linearly over the tone duration
    pop af                          ; restore flags from the computation of diff above
    ld a, e
    jp p, AddRampValToBaseTone      ; if the result of the subtraction was positive, jump
    neg
    jr z, AddRampValToBaseTone      ; if DE is zero, nothing to do
    dec d                           ; else DE = -DE
    ld e, a
AddRampValToBaseTone:
    pop hl                          ; HL = base tone
    add hl, de                      ; add value computed above = interpolated(abs(diff) mod 256) * sign(diff)
    ex de, hl
    ld (ix + Voice.PSGTone), e
    ld (ix + Voice.PSGTone+1), d    ; and this is the final tone to send to PSG
    ld a, (ix + Voice.vibrato)      ; A = index vibrato curve
    or a
    jp nz, VibratoStage             ; if valid, apply vibrato, else go straight to envelope code
VolumeStage:
    ld a, (ix + Voice.envelope)     ; A = index envelope
    or a
    jr nz, Envelope                 ; if valid, apply envelope
    ld a, (ix + Voice.volume)
    cpl                             ; else we will just output the base volume from the song data
    and $0f
    ld (ix + Voice.PSGVolume), a
    jr PSGWriteStage
Envelope:
    res 7, a
    ld hl, EnvelopesTable
    call ReadFromWordTable          ; HL = address envelope to apply
    call ApplyEnvelope
PSGWriteStage:
    bit 6, (ix + Voice.flags)       ; write tone value to PSG unless this flag is set
    jr nz, PSGWriteVolume
PSGWriteTone:
    ld a, (ix + Voice.PSGChannel)   ; retrieve PSG channel index for this channel in BC
    and $0f
    ld c, a
    ld b, $00
    ld hl, PSGLatchTone
    add hl, bc
    ld c, (hl)                      ; get corresponding PSG tone latch byte
    ld a, (ix + Voice.PSGTone)
    and $0f
    or c                            ; add low nibble of tone value to write
    call WriteToPSG                 ; and send to PSG
    ld a, (ix + Voice.PSGTone)
    and $f0
    or (ix + Voice.PSGTone+1)
    rrca                            ; swap nibbles
    rrca
    rrca
    rrca                            ; so A now contains the high byte of the tone value
    call WriteToPSG                 ; and send to PSG
PSGWriteVolume:
    ld a, (ix + Voice.PSGChannel)   ; retrieve PSG channel index for this channel in BC
    and $0f
    ld c, a
    ld b, $00
    ld hl, PSGLatchVolume           ; get corresponding PSG volume latch byte
    add hl, bc
    ld a, (hl)
    or (ix + Voice.PSGVolume)       ; and add volume value
    jp WriteToPSG                   ; send it to the PSG, and return!

PSGLatchTone:
.db $80                                 ; write to tone0 channel, tone
.db $a0                                 ; write to tone1 channel, tone
.db $c0                                 ; write to tone2 channel, tone
.db $c0                                 ; write to tone2 channel, tone (to control noise channel when we write %11 to shift rate)
PSGLatchVolume:
.db $90                                 ; write to tone0 channel, volume
.db $b0                                 ; write to tone1 channel, volume
.db $d0                                 ; write to tone2 channel, volume
.db $f0                                 ; write to noise channel, volume

ResetEnvelopeCounter:
    ld (ix + Voice.envCounter), a   ; used to restart at the beginning of the envelope if envelope byte read is 0

ApplyEnvelope:
    push hl                         ; HL = address envelope data
    ld a, (ix + Voice.envCounter)   ; A = envelope counter
    srl a
    push af                         ; save flags (Carry = 1 if counter is odd)
    ld c, a
    ld b, $00                       ; envelope data is one nibble/frame, so the envelope data for this frame
    add hl, bc                      ; is stored in byte (envelope counter)/2 of the envelope byte array
    pop af
    ld a, (hl)                      ; a = envelope data byte
    pop hl
    jr c, IncEnvelopeCounter
    rrca                            ; if envelope counter was even, swap low and high nibbles of byte read
    rrca
    rrca
    rrca
    or a
    jr z, ResetEnvelopeCounter      ; if envelope byte read is 0, restart envelope
    cp $10
    jr nz, EnvelopeByteNotOne
    dec (ix + Voice.envCounter)     ; if envelope byte is $01 ($10 after swap), decrement envelope counter and rerun
    jr ApplyEnvelope                ; (iow, infinite loop on the envelope byte preceding the $01)
EnvelopeByteNotOne:
    cp $20                          ; if envelope byte is $02 ($20 after swap), set voice volume to silence
    jr z, EnvelopeSilence           ; and don't increment envelope counter so keep emitting silence each frame
IncEnvelopeCounter:
    inc (ix + Voice.envCounter)     ; increment envelope counter
    or $f0                          ; A = $f in high nibble, envelope nibble in low nibble
    add a, (ix + Voice.volume)      ; add base channel volume nibble (from song data)
    inc a                           ; and add 1
    jr c, SetFinalVolume            ; if no carry, final volume is to small so set channel volume to silence
EnvelopeSilence:
    xor a
SetFinalVolume:
    cpl                             ; set final volume by converting internal representation (higher is louder)
    and $0f                         ; to the attenuation format expected by PSG (0 = loudest, $f = silence)
    ld (ix + Voice.PSGVolume), a
    ret

ResetVibratoCounter:
    ld (ix + Voice.vibCounter), a   ;  used to restart at the beginning of the curve if curve byte read is 0

ApplyVibrato:
    push hl                         ; HL = @ envelope data
    ld a, (ix + Voice.vibCounter)   ; A = vibrato counter
    srl a
    push af                         ; save flags (Carry = 1 if counter is odd)
    ld c, a
    ld b, $00                       ; vibrato curve data is one nibble/frame, so the data for this frame
    add hl, bc                      ; is stored in byte (vibrato counter)/2 of the curve's byte array
    pop af
    ld a, (hl)                      ; a = vibrato data byte
    pop hl
    jr c, IncVibratoCounter
    rrca                            ; if vibrato counter was even, swap low and high nibbles of byte read
    rrca
    rrca
    rrca
    or a
    jp z, ResetVibratoCounter       ; if byte read was 0, restart vibrato curve
    cp $10                          ; if vibrato curve byte is $01 ($10 after swap), decrement vibrato counter and rerun
    jr nz, IncVibratoCounter        ; (iow, infinite loop on the vibrato byte preceding the $01)
    dec (ix + Voice.vibCounter)
    jr ApplyVibrato
    cp $20                          ; (this instruction and next bypassed/unreachable)
    ret z
IncVibratoCounter:
    inc (ix + Voice.vibCounter)     ; increment vibrato counter
    cpl
    and $0f
    ld l, a
    ld h, $00
    ex de, hl                       ; de = nibble from vibrato curve data
    add hl, de                      ; added to the current tone value
    ld (ix + Voice.PSGTone), l
    ld (ix + Voice.PSGTone+1), h    ; to give us the final tone counter to send to the PSG!
    ret

GetNextSoundDataByte:
    ld e, (ix + Voice.dataPtr)
    ld d, (ix + Voice.dataPtr+1)    ; DE = current pointer in sound data for that voice
GetNextSoundDataByte2:
    ld a, (de)                      ; consume a byte of sound data
    inc de
    cp $e0
    jp nc, JumpToSoundRoutine       ; if read byte >= $e0: it is a sound routine, so go execute it
    bit 3, (ix + Voice.flags)       ; if this flag is set, tones are encoded directly in the sound data,
    jr nz, NoToneTableMode          ; instead of going through the tone table indirection
    or a                            ; (reset carry and P flag (re)set based on value of sound data byte)
    jp p, ProcessDurationValue      ; if byte < $80, it is a duration value
    sub $80                         ; else byte is in the [$80-$df] range i.e. a tone index + $80
    jr z, ProcessToneIndex          ; add tone index offset for this voice, unless the byte is $80 (= rest)
    add a, (ix + Voice.toneOffset)
ProcessToneIndex:
    ld hl, ToneTable
    ld c, a
    ld b, $00
    add hl, bc
    add hl, bc                      ; hl = address of ToneTable[tone index] = address base tone
    ld a, (hl)
    ld (ix + Voice.baseTone), a
    inc hl
    ld a, (hl)
    ld (ix + Voice.baseTone+1), a   ; write base tone value in voice buffer
    bit 5, (ix + Voice.flags)
    jr z, CheckForDurationByte      ; if we are not in "tone ramp mode", go check if next byte is a duration value, etc.
    ld a, (de)                      ; else consume next byte assuming it is a tone value
    inc de
    sub $80
    add a, (ix + Voice.toneOffset)  ; convert byte to tone index by subtracting $80 and adding voice's tone index offset
    ld hl, ToneTable
    ld c, a
    ld b, $00
    add hl, bc
    add hl, bc
    ld a, (hl)                      ; lookup delta tone in tone table
    ld (ix + Voice.deltaTone), a    ; and save it in voice buffer
    inc hl
    ld a, (hl)
    ld (ix + Voice.deltaTone+1), a
ReadDurationValue:
    ld a, (de)                      ; consume next byte, assuming it is a duration value
    inc de
ProcessDurationValue:
    push de
    ld h, a                         ; H = duration byte read from voice data
    ld e, (ix + Voice.durationMult) ; E = duration multiplier for this voice read from song data
    call MultiplyHbyE               ; multiply both, result in HL
    pop de
    ld (ix + Voice.duration), l
    ld (ix + Voice.duration+1), h   ; store result as duration for this note
ResetVoiceCounters:
    xor a
    ld (ix + Voice.envCounter), a   ; reset envelope counter
    ld (ix + Voice.vibCounter), a   ; reset vibrato counter
    ld (ix + Voice.dataPtr), e
    ld (ix + Voice.dataPtr+1), d    ; update stored current pointer in sound data for that voice
    xor a
    ld (ix + Voice.durCounter), a   ; reset voice's duration counter
    ld (ix + Voice.durCounter+1), a
    ret

NoToneTableMode:                    ; the tone values are written directly in the sound data, instead of going through
    ld (ix + Voice.baseTone+1), a   ; the tone index / tone table indirection. Not used in the final BIOS data so probably
    ld a, (de)                      ; a development / debug option
    inc de
    ld (ix + Voice.baseTone), a     ; read and save base tone value directly from the sound data bytes
    bit 5, (ix + Voice.flags)
    jr z, ReadDurationValue
    ld a, (de)                      ; if we are in "tone ramp" mode, read and save delta tone directly too
    inc de
    ld (ix + Voice.deltaTone+1), a
    ld a, (de)
    inc de
    ld (ix + Voice.deltaTone), a
    jr ReadDurationValue

CheckForDurationByte:
    ld a, (de)                      ; byte after tone
    or a
    jp p, ReadDurationValue+1       ; if it's a duration value, read it, else skip directly to reset voice counters part
    jr ResetVoiceCounters

JumpToSoundRoutine:
    ld hl, SoundRoutineRetsHere     ; push that adress on the stack so that will be where the
    push hl                         ; sound routine returns when it executes the ret instruction
    and $1f                         ; extract routine number from command byte ($e5 -> 5)
    ld hl, SoundRoutineTable
    ld c, a
    ld b, $00
    add hl, bc                      ; lookup the corresponding routine address in the table...
    add hl, bc
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    jp (hl)                         ; and jump to it!

SoundRoutineRetsHere:
    inc de
    jp GetNextSoundDataByte2        ; fetch and process next sound data byte

SoundRoutineTable:
.dw SoundRoutineE0
.dw SoundRoutineE1
.dw SoundRoutineE2
.dw SoundRoutineE3
.dw SoundRoutineE4
.dw SoundRoutineE5
.dw SoundRoutineE6
.dw SoundRoutineE7
.dw SoundRoutineE8

SoundRoutineE4:                     ; move current voice data pointer to address in the next two bytes of voice data
    ex de, hl
    ld e, (hl)
    inc hl
    ld d, (hl)
    dec de
    ret

SoundRoutineE8:                     ; add provided value to the offset added to tone table indices
    ld a, (de)                      ; apparently unused in this BIOS's data
    add a, (ix + Voice.toneOffset)
    ld (ix + Voice.toneOffset), a
    ret

SoundRoutineE5:
    res 5, (ix + Voice.flags)       ; deactivate tone ramp mode
    dec de
    ret

SoundRoutineE6:                     ; like SoundRoutineE4, but instead of a "goto" it is some kind of "for" loop
    ld a, (de)
    inc de
    add a, $17
    ld c, a
    ld b, $00
    push ix
    pop hl
    add hl, bc
    ld a, (hl)                      ; A = byte at offset $nn+$17 in the voice buffer = current loop counter
    or a
    jr nz, SoundE6CounterNotZero
    ld a, (de)
    ld (hl), a                      ; if the counter is zero,reset it with value in with next byte
SoundE6CounterNotZero:
    inc de
    dec (hl)
    jp nz, SoundRoutineE4           ; decrement counter, if not zero, jump to address provided
    inc de
    ret

SoundRoutineE2:
    ld a, (de)
    or $e0                          ; write to PSG noise channel, tone/noise value
    push af
    call WriteToPSG
    pop af
    or $fc                          ; if the shift rate value is %11, the noise channel counter is reset to Tone2;
    inc a                           ; so we have to enable writing of tone values to the PSG for the noise voice,
    jr nz, NoWriteFromNoiseToChan2  ; as writing a tone to the noise voice writes to the PSG's channel 2 tone
    res 6, (ix + Voice.flags)       ; (cf. PSGLatchTone). In the other cases, the channel counter is reset to some
    ret                             ; constants (0x10, 0x20 or 0x40), so we want to *disable* the tone writes for the
NoWriteFromNoiseToChan2:            ; noise voice. Thus the sound data can keep the "tone byte first, then duration byte"
    set 6, (ix + Voice.flags)       ; syntax and everything works fine (tone value in noise voice is simply ignored
    ret                             ; in the "non %11" case)

SoundRoutineE3:                     ; changes index of envelope to use for this voice
    ld a, (de)
    ld (ix + Voice.envelope), a
    ret

SoundRoutineE7:                     ; add provided value to voice's based volume, but cap new volume to $0e
    ld a, (de)                      ; ( iow newVolume = min(oldVolume + value, $0e) )
    add a, (ix + Voice.volume)
    cp $0e
    jr c, SoundRoutineSetVolume
    ld a, $0e
    jr SoundRoutineSetVolume

SoundRoutineE0:                     ; set voice's base volume to value passed in next sound byte
    ld a, (de)
SoundRoutineSetVolume:
    ld (ix + Voice.volume), a
    ret

SoundRoutineE1:
    xor a
    ld (ix + Voice.flags), a        ; disable voice update
    call SilenceChannel             ; and silence corresponding PSG channel
    pop hl
    pop hl                          ; skip 2 levels of callstack so instead of returning to SoundRoutineRetsHere,
    ret                             ; we return to the ld de, $20 instruction in UpdateSoundLoop

SilenceChannel:
    ld a, (ix + Voice.PSGChannel)
    and $0f                         ; retrieve channel # from low nibble of byte 1 of voice buffer
    ld c, a
    ld b, $00
    ld hl, PSGLatchVolume           ; and use it as index in PSGLatchVolume array
    add hl, bc
    ld a, (hl)
    or $0f                          ; Set volume value to $f (silence) and finish with WriteToPSG

WriteToPSG:
    bit 2, (ix + Voice.flags)       ; only go through with the write if bit2 (ix+00) is not set
    ret nz
    out (PSGPort), a
    ret

ClearSound:
    exx
    ld hl, Voice0                    ; memzero sound state + voice structs
    ld de, Voice0+1
    ld bc, $7f
    ld (hl), $00
    ldir
    exx
    exx
    ld hl, PSG_ResetData             ; turn off all sound channels
    ld c, PSGPort
    ld b, $04
    otir
    xor a
    exx
    ret

PSG_ResetData:
.db $9f                    ; tone 0, volume off
.db $bf                    ; tone 1, volume off
.db $df                    ; tone 2, volume off
.db $ff                    ; noise,  volume off

ToneTable:
.dw $0000                ; rest
.dw $03ff                ; ~A2
.dw $03c7                ; ~A#2
.dw $0390                ; ~B2
.dw $035d                ; ~C3
.dw $032d                ; ~C#3
.dw $02ff                ; ~D3
.dw $02d4                ; ~D#3
.dw $02ab                ; ~E3
.dw $0285                ; ~F3
.dw $0261                ; ~F#3
.dw $023f                ; G3
.dw $021e                ; ~G#3
.dw $0200                ; ~A3
.dw $01e3                ; ~A#3
.dw $01c8                ; ~B3
.dw $01af                ; ~C4
.dw $0196                ; ~C#4
.dw $0180                ; ~D4
.dw $016a                ; ~D#4
.dw $0156                ; ~E4
.dw $0143                ; ~F4
.dw $0130                ; ~F#4
.dw $011f                ; ~G4
.dw $010f                ; ~G#4
.dw $0100                ; ~A4
.dw $00f2                ; ~A#4
.dw $00e4                ; ~B4
.dw $00d7                ; ~C5
.dw $00cb                ; ~C#5
.dw $00c0                ; ~D5
.dw $00b5                ; ~D#5
.dw $00ab                ; ~E5
.dw $00a1                ; ~F5
.dw $0098                ; ~F#5
.dw $0090                ; ~G5
.dw $0088                ; ~G#5
.dw $0080                ; ~A5
.dw $0079                ; ~A#5
.dw $0072                ; ~B5
.dw $006c                ; ~C6
.dw $0066                ; ~C#6
.dw $0060                ; ~D6
.dw $005b                ; ~D#6
.dw $0055                ; ~E6
.dw $0051                ; ~F6
.dw $004c                ; ~F#6
.dw $0048                ; ~G6
.dw $0044                ; ~G#6
.dw $0040                ; ~A6
.dw $003c                ; ~A#6
.dw $0039                ; ~B6
.dw $0036                ; ~C7
.dw $0033                ; ~C#7
.dw $0030                ; ~D7
.dw $002d                ; ~D#7
.dw $002b                ; ~E7
.dw $0028                ; ~F7
.dw $0026                ; ~F#7
.dw $0024                ; ~G7
.dw $0022                ; ~G#7
.dw $0020                ; ~A7
.dw $001e                ; ~A#7
.dw $001c                ; ~B7
.dw $001b                ; ~C8
.dW $0019                ; ~C#8
.dw $0018                ; ~D8
.dw $0016                ; ~D#8
.dw $0015                ; ~E8
.dw $0014                ; ~F8
.dw $0013                ; ~F#8
.dw $0012                ; ~G8
.dw $0011                ; ~G#8

MultiplyHbyE:                       ; Multiply H by E, result in HL
    ld d, $00
    ld l, d                         ; D = L = 0
    ld b, 8
ProcessOneBitOfH:
    add hl, hl                      ; shift H (and partial result) one bit left
    jr nc, DontAddE
    add hl, de                      ; if bit of H was set, add e to the result
DontAddE:
    djnz ProcessOneBitOfH
    ret

DivideHLbyE:                        ; Unsigned integer division. Divide HL by E, quotient in A, remainder in H
    ld b, 8
DividendLoop:
    adc hl, hl                      ; Shift HL, and bring Carry from previous iteration in bit0 of l
    ld a, h                         ; if Carry, current 9 left bits of HL were of the form 1........
    jr c, GreaterThanE              ; so guaranteed to be > E. Subtract E from rightmost 8 bits and clear Carry
    cp e                            ; If Carry was not set, we have to compare with E
    jr c, LesserThanE               ; if smaller than E, end this iteration with Carry set
GreaterThanE:
    sub e                           ; else subtract E, and update HL for next iteration
    ld h, a
    or a                            ; set Carry to 0, this iteration the divisor was <= the 9 left bits of HL
LesserThanE:
    djnz DividendLoop
    ld a, l
    rla                             ; do the final rotation bringing Carry in bit0 performed by adc in the loop
    cpl                             ; and now each bit of a is 0 if the division step succeeded, 1 if it failed
    ret                             ; so by inverting the bits of A we obtain the quotient in A!

SegaProtoSong:
.db $04                                    ; 4 channels
.db $a0, $20, $05, <SegaProtoSongChan0, >SegaProtoSongChan0, $05, $01, $04, $05
.db $a0, $21, $05, <SegaProtoSongChan1, >SegaProtoSongChan1, $05, $01, $04, $05
.db $a0, $22, $05, <SegaProtoSongChan2, >SegaProtoSongChan2, $05, $01, $04, $05
.db $80, $23, $05, <SegaProtoSongChan3, >SegaProtoSongChan3, $00, $00, $00, $00

SegaProtoSongChan3:
.db $80, $32, $e2, $04, $e3, $01, $e0, $0d, $8d, $08, $e3, $02, $e0, $0f, $10, $e1

SegaProtoSongChan1:
.db $8d, $91, $04, $e7, $01, $e8, $01, $e6, $00, $0a, $55, $16, $91, $9d, $0a, $e5, $8d, $08, $81, $10, $e1

SegaProtoSongChan0:
.db $8c, $8f, $04, $e7, $01, $e8, $01, $e6, $00, $0a, $6a, $16, $8f, $9b, $0a, $e5, $8a, $08, $8a, $10, $e1

SegaProtoSongChan2:
.db $88, $8c, $04, $e7, $01, $e8, $01, $e6, $00, $0a, $7f, $16, $8c, $98, $0a, $e5, $86, $08, $86, $10, $e1

SnailMazeGameEndSong:
.db $04
.db $80, $20, $05, <SnailMazeGameEndSongChan0, >SnailMazeGameEndSongChan0, $00, $01, $02, $0e
.db $80, $21, $05, <SnailMazeGameEndSongChan1, >SnailMazeGameEndSongChan1, $00, $01, $02, $0f
.db $80, $22, $05, <SnailMazeGameEndSongChan2, >SnailMazeGameEndSongChan2, $00, $01, $02, $0e
.db $80, $23, $05, <SnailMazeGameEndSongChan3, >SnailMazeGameEndSongChan3, $00, $00, $00, $00

SnailMazeGameEndSongChan0:
.db $8f, $02, $02, $02, $e3, $04, $0f, $e3, $02, $8d, $03, $03, $8f, $e3, $04, $91, $18, $e1

SnailMazeGameEndSongChan1:
.db $9f, $02, $02, $02, $e3, $04, $a0, $0f, $e3, $02, $9d, $03, $03, $9f, $e3, $04, $a1, $18, $e1

SnailMazeGameEndSongChan2:
.db $a2, $02, $02, $02, $e3, $04, $a4, $0f, $e3, $02, $a0, $03, $03, $a2, $e3, $04, $a4, $18, $e1

SnailMazeGameEndSongChan3:
.db $e2, $04, $e3, $01, $e0, $0d, $8d, $02, $02, $02, $e3, $02, $e0, $0f, $0f, $e3, $01, $e0, $0d
.db $03, $03, $03, $e3, $02, $e0, $0f, $12, $e1

SnailMazeLoopSong:
.db $04
.db $80, $20, $02, <SnailMazeLoopSongChan0, >SnailMazeLoopSongChan0, $00, $01, $03, $0b
.db $80, $21, $02, <SnailMazeLoopSongChan1, >SnailMazeLoopSongChan1, $00, $01, $03, $0c
.db $80, $22, $02, <SnailMazeLoopSongChan2, >SnailMazeLoopSongChan2, $00, $01, $03, $0b
.db $80, $23, $02, <SnailMazeLoopSongChan3, >SnailMazeLoopSongChan3, $00, $00, $00, $00

SnailMazeLoopSongChan0:
.db $80, $01               ; same as channel 1 with a short silence before

SnailMazeLoopSongChan1:
.db $99, $08, $a0, $9d, $a0, $9e, $9b, $9d, $9b, $99, $9b, $9d, $9b, $94, $96, $94, $80
.db $99, $08, $a0, $9d, $a0, $9e, $9b, $9d, $9b, $9d, $9b, $99, $98, $99, $99, $99, $80
.db $e4, <SnailMazeLoopSongChan1, >SnailMazeLoopSongChan1

SnailMazeLoopSongChan2:
.db $8d, $08, $99, $8d, $99, $92, $9e, $94, $a0, $8d, $94, $8d, $99, $88, $8f, $88, $94
.db $8d, $08, $99, $8d, $99, $92, $9e, $94, $a0, $8d, $94, $88, $94, $8d, $99, $8d, $80
.db $e4, <SnailMazeLoopSongChan2, >SnailMazeLoopSongChan2

SnailMazeLoopSongChan3:
.db $e2, $04, $e3, $01, $e0, $0d, $8d, $08, $e3, $02, $e0, $0f, $08, $e3, $01, $e0, $0d
.db $04, $04, $e3, $02, $e0, $0f, $08
.db $e4, <SnailMazeLoopSongChan3+2, >SnailMazeLoopSongChan3

SnailMazeEndRoundSong:
.db $04
.db $80, $20, $04, <SnailMazeEndRoundSongChan0, >SnailMazeEndRoundSongChan0, $00, $01, $02, $0e
.db $80, $21, $04, <SnailMazeEndRoundSongChan1, >SnailMazeEndRoundSongChan1, $00, $01, $02, $0d
.db $80, $22, $04, <SnailMazeEndRoundSongChan2, >SnailMazeEndRoundSongChan2, $00, $01, $02, $0e
.db $80, $23, $04, <SnailMazeEndRoundSongChan3, >SnailMazeEndRoundSongChan3, $00, $00, $00, $00

SnailMazeEndRoundSongChan0:
.db $9d, $02, $9b, $9d, $a0, $9d, $a0, $e3, $04, $a5, $10, $e1

SnailMazeEndRoundSongChan1:
.db $99, $02, $98, $99, $9d, $99, $9d, $e3, $04, $a2, $10, $e1

SnailMazeEndRoundSongChan2:
.db $8d, $02, $02, $02, $88, $88, $88, $e3, $04, $91, $10, $e1

SnailMazeEndRoundSongChan3:
.db $e2, $04, $e3, $01, $e0, $0d, $8d, $04, $e3, $02, $e0, $0f, $04, $e3, $01, $e0, $0d
.db $02, $02, $e3, $02, $e0, $0f, $04, $e1

Envelope3:
.db $af, $ec, $ee, $01                    ; $01 at the end means we sustain the $e
;  ++                       f
;  ++++  ++++...++++++++... e
;  ++++  ++++...++++++++... d
;  ++++++++++...++++++++... c
;  ++++++++++...++++++++... b
;++++++++++++...++++++++... a

VibratoCurve:
.db $bc, $dd, $ee, $ff, $ee, $ff, $dd, $cc, $bb, $dd, $ee, $ff, $ee, $dd, $cc, $bb
.db $cc, $dd, $ee, $ff, $fe, $dc, $bc, $de, $fe, $db, $ab, $cd, $ef, $ed, $cb, $ab
.db $cd, $ef, $ee, $01                    ; $01 at the end means we sustain the $e
.db $ff, $ee, $00                         ; leftover data from a previous version of the curve?

; The top part of the vibrato curve looks like this (left out [0-9] bottom part for brevity)
;      ++  ++          ++              +++       +        +         +   f
;    ++++++++        ++++++          ++++++     +++      +++       ++++ e
;  ++++++++++++    ++++++++++      +++++++++   +++++    +++++     +++++ d
; +++++++++++++++  ++++++++++++  ++++++++++++ ++++++   +++++++   ++++++ c
;++++++++++++++++++++++++++++++++++++++++++++++++++++ +++++++++ +++++++ b
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ a

EnvelopesTable:
.dw Envelope0
.dw Envelope1
.dw Envelope2
.dw Envelope3

Envelope0:    ; Max volume for 2 frames, then silence
.db $ff, $02

Envelope1:    ; Release only
.db $ff, $ed, $dc, $cb, $ba, $a9, $02
;++++                     f
;++++++                   e
;++++++++++               d
;++++++++++++++           c
;++++++++++++++++++       b
;++++++++++++++++++++++   a
;++++++++++++++++++++++++ 9

Envelope2:
.db $ef, $ff, $ff, $ee, $02
;  ++++++++++     f
;++++++++++++++++ e

VibratoCurvesTable:
.dw VibratoCurve

SongTable:
.dw SegaProtoSong
.dw SnailMazeGameEndSong
.dw SnailMazeLoopSong
.dw SnailMazeEndRoundSong
