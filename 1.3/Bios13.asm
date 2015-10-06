;-------------------------------------------------------------
; Sega Master System BIOS 1.3
; Dumped by Image/Helsinki/Finland, long time ago
; Sources commented by segmtfault in 2015
; Based on:
; * Previous commenting work by Omar Cornut / Zoop in 1999
; * ROM detection comments and PSG tones table by Maxim:
; http://www.smspower.org/maxim/forumstuff/psgtable.html
; http://www.smspower.org/forums/6706-USBIOSDisassembly
; * Thanks to Calindro for info about Alex Kidd in M. World
; * Thanks to Calindro & Maxim for info about sound engines
;-------------------------------------------------------------

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
.DEFINE ROM_RAM_Offset  $c700 - $0289   ; offset between the DetectCode in ROM and the copy in RAM

.DEFINE MemCtrlPort     $3e
.DEFINE PSGPort         $7f
.DEFINE VDPDataPort     $be
.DEFINE VDPCtrlPort     $bf
.DEFINE IO_1_Port       $dc
.DEFINE IO_2_Port       $dd

; Flags passed to interrupt handler
.DEFINE UpdateSAT         $01
.DEFINE UpdateGraphics    $02
.DEFINE UpdateLogoScroll  $04
.DEFINE UpdateMazeCol     $08
.DEFINE UpdateMazeTime    $10

.DEFINE VDP_WriteVRAM   $4000
.DEFINE VDP_WriteReg    $8000
.DEFINE VDP_WriteCRAM   $c000
.DEFINE TileMap_Base    $3800
.DEFINE TileMapVRAM     TileMap_Base | VDP_WriteVRAM
.DEFINE SpriteVRAM      $3f00 | VDP_WriteVRAM

.MACRO TILEMAP_XY ARGS x, y
.dw TileMapVRAM + (y*32 + x)*2
.ENDM

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

.ENUM $c200 ASC
InterruptArg       db
TempVar            db                   ; used by LoadMonochromePatternData and SEGA logo scrolling code
SegaLogoScrollH    db
GfxDataBlockAddr   dw
Unused_C205        dsb 1
ResetButtonState   db                   ; $10 = not pressed, $00 = pressed
ControllerState    db                   ; NOT version of port $DC so a bit to 1 means button is pressed
ControllerState2   db                   ; a bit to 1 means the button was pressed this frame and not last frame
SpriteRamPointer   dw
DoResetCheck       db                   ; enable RESET button test in interrupt handler
Unused_C20C        dsb 20
ResetScrollRegFlag db
Unused_C221        dsb 4
MazeScrollH        db
ResetScrollX_Val   db
Unused_C227        dsb 1
MazeDebugVar       db
Unused_C229        dsb 1
ResetScrollY_Val   db
Unused_C22B        dsb 5
RoundNumberDCB     db
RoundNumber        db
RoundTimeUpdCtr    db                  ; VBlanks counter for UpdateSnailMazeGameTime
RoundTimeLeft      db
GoalReachedFlag    db
OutOfTimeFlag      db
Unused_C236        dsb 14
ScrollMazeFlag     db
Unused_C245        dsb 27
ROM_HeaderCopy     dsb 16
ComputedChecksum   dw
Unused_C272        dsb 2
BlinkTimeUpCouter  db
.ENDE

.STRUCT Entity
entityId           db                   ; 1 for snail, 2 for goal, 3 for start point
flag               db                   ; "moving" flag for snail "initialized" flag for goal & start point
animStep           db
animVBlanksCounter db
animVBlanksCtrRst  db
animDataPointer    dw
x                  db
y                  db
xChange            db                   ; +1 if moving right, -1 if moving left
yChange            db                   ; +1 if moving down, -1 if moving up
unused             dsb 1
pattern            db
unused2            dsb 18
controllerChkCtr   db
.ENDST

.ENUM $c300 ASC
SpriteRAM          dsb 256
Snail              INSTANCEOF Entity
StartPoint         INSTANCEOF Entity
Goal               INSTANCEOF Entity
.ENDE

.ENUM $c700
DetectCodeRAM      ds 221
.ENDE

.ENUM $c9c2 ASC
MazeTopTilemap     dsw 30              ; tilemap for the upper wall tiles of the maze
Unused             dsw 1
MazeTilemap        dsw 512             ; tilemap for the maze. 32 x 16, starting at $ca00
.ENDE

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

.ENUM $cf00 ASC
SongRequest        db
VoiceEndedFlag     db
VoiceSyncFlag      db                  ; used to synchronize code execution with sound voice
Voice0             INSTANCEOF Voice
Voice1             INSTANCEOF Voice
Voice2             INSTANCEOF Voice
Voice3             INSTANCEOF Voice
.ENDE

;-------------------------------------------------------------

.BANK 0 SLOT 0
.ORG $0000
    di                              ; disable VDP interrupt
    im 1                            ; Interrupt Mode 1: executes a RST 38h on interrupts
    ld sp, RAM_Base+RAM_Size-$10    ; stack near top of RAM
    jr BiosColdBoot

.ORG $0008
WriteVRAMByte:                      ; rst $8 : write A at VRAM address in DE
    push af
    rst $20                         ; first write the VRAM address
    pop af
    out (VDPDataPort), a            ; then the data!
    ret

.ORG $0010                          ; rst $10 : write B tilemap values at VRAM address in DE,
WriteTileMapEntries:                ; with source data at address HL
    rst $20                         ; first, write VRAM address
    xor a                           ;
    ld c, VDPDataPort               ; A = flags byte = 0 -> no flipping, bg priority, use palette #0
                                    ; and bit #9 pattern number = 0 (so only use bottom 256)
WriteTileMapEntry:
    outi                            ; writes pattern number byte from data pointed by HL, and inc. HL
    push af                         ; wait a bit...
    pop af
    out (c), a                      ; then write flags byte (= 0)
    jr nz, WriteTileMapEntry        ; and continue until we have written B entries
    ret

TurnOnDisplay:
    LD_DE_VDP_REG_VAL $01, $e0      ; write $e0 in VDP reg.1 => turn on display, frame int. enable

.ORG $0020
WriteVDPControlPort:                ; rst $20
    ld a, e                         ; write register DE to the VDP control port (E, then D)
    out (VDPCtrlPort), a
    ld a, d
    out (VDPCtrlPort), a
    ret

.ORG $0028
WriteVRAMBlockWithL:                ; $rst28: write BC bytes of VRAM with the value in L
    rst $20                         ; write DE to VDP control port to set the address
    ld a, c
    or a                            ; we are going to write BC bytes by blocks of 256 bytes
    jr z, WriteVRAMNoExtraBlock     ; if BC is not a multiple of 256, we need to count an extra block
    inc b                           ; to transfer the remainder
WriteVRAMNoExtraBlock:
    ld a, l                         ; write the value in L
WriteVRAMLoop:
    out (VDPDataPort), a
    dec c
    jr nz, WriteVRAMLoop
    djnz WriteVRAMLoop
    ret

.ORG $0038
MaskableInterrupt:
    jp MaskableInterruptHandler

WriteVRAMBlockFromHL:               ; (UNUSED IN THIS BIOS) copy a block of BC bytes of data
    rst $20                         ; pointed to by HL to VRAM address pointed by DE
    ld a, c
    or a                            ; we are going to write BC bytes by blocks of 256 bytes
    jr z, WriteVRAMNoExtraBlock2
    inc b
WriteVRAMNoExtraBlock2:
    ld a, b                         ; A = number of blocks to write
    ld b, c                         ; B = number of bytes left in this block
    ld c, VDPDataPort
WriteVRAMLoop2:
    outi                            ; (VDPDataPort) = (HL); B = B - 1; HL = HL + 1
    jr nz, WriteVRAMLoop2
    dec a                           ; finished a block, decrement blocks counter
    jr nz, WriteVRAMLoop2
    ret                             ; finished all blocks

UpdateSpriteAttrTable:
    ld hl, SpriteRAM
    ld de, SpriteVRAM
    ld bc, $6400 | VDPDataPort
    rst $20                         ; writes to data port go to VRAM, at $3f00 = sprite attribute table
    otir                            ; writes 100 bytes from SpriteRAM to VDP's $3f00
    ld hl, SpriteRAM + $80          ; (64 bytes of Y coords + 36 bytes in "empty zone" of SAT)
    ld de, SpriteVRAM + $80
    ld b, $80                       ; write to data port go to VRAM, at $3f80 =
    rst $20                         ; 2nd half of sprite attr. table (X + pattern #)
    otir                            ; writes 128 bytes from SpriteRAM + $80 to VDP's $3f80
    ret

.ORG $0066
NMI_Pause:
    retn                            ; disable pause button

TurnOffDisplay:                     ; (UNUSED IN THIS BIOS)
    LD_DE_VDP_REG_VAL $01, $a0      ; write $a0 in VDP reg.1 => turn off display, frame int. enable
    jr WriteVDPControlPort

BiosColdBoot:
    ld hl, PSG_ResetData            ; write 4 bytes from PSG_ResetData to port 7F (PSG)
    ld c, PSGPort                   ; which set the four channels to "volume off"
    ld b, $04
    otir
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
    ld b, $0e
    call WaitRoutine                ; wait 5.96 million cycles (near 1.7 s) for HW to warm up/init.
BiosWarmBoot:
    ld hl, VDP_ResetData            ; reset VDP registers again (because ResetButtonHandler jumps here)
    ld bc, $1800 | VDPCtrlPort
    otir
    xor a
    out (VDPDataPort), a            ; writes 0 to VDP data port => color 0 of sprite palette = black
    ld hl, InitPaletteData
    ld (GfxDataBlockAddr), hl       ; init. addr. of block of graphics data to write to VRAM to InitPaletteData
    ld a, $d0                       ; init first byte of sprite RAM (Y for sprite #0) to $d0,
    ld (SpriteRAM), a               ; so sprite #0 (and all remaining sprites) will not be drawn.
    ei                              ; enable interrupts
    ld a, UpdateGraphics | UpdateSAT; wait for interrupt handler to run, requesting it to call
    call WaitForIntHandlerDone      ; UpdateSpriteAttrTable and UpdateGraphicsData
    di                              ; disable interrupts
    LD_DE_PATTERN_ADDR $000         ; note: here HL points to InterruptArg (from call above to WaitForIntHandlerDone) so L=0
    ld bc, $0020
    rst $28                         ; write 0 in the first 32 bytes of VRAM, so pattern 0 is all 0s
    call ResetTileMap               ; resets tilemap with all zeros
    LD_DE_PATTERN_ADDR $13f
    ld bc, $0020                    ; write 0 in 32 bytes of VRAM at $27e0
    rst $28                         ; so pattern $13f (319) is all zeros
    ld hl, FontData                 ; load $228 = 552 lines of pattern data (69 patterns) with color 9 (white)
    LD_DE_PATTERN_ADDR $01b         ; from ROM address FontData, to VRAM address $360 (pattern 27 and following)
    ld bc, $0228                    ; => upload (c)SEGA patterns, then font patterns for ASCII code 32 (space) to 95 (_)
    ld a, $09                       ; because we start at pattern 27, the patterns 32-95 match their ASCII code!
    call LoadMonochromePatternData  ; (e.g. the pattern for "A" is pattern 65, which is the ASCII code for "A")
    ld hl, RLEPatternDataEnjoy
    LD_DE_PATTERN_ADDR $060         ; load patterns 96-116, the "ENJOY!" pink string, and solid blue square
    call DecodeRLECompPatterns      ; stored in ROM in RLE compressed format
    xor a
    ld (SegaLogoScrollH), a         ; init. SegaLogoScrollH to 0
    LD_DE_TILEMAP_XY $0b $08        ; write a 4 rows, 10 columns block of tilemap entries
    ld hl, SegaLogoTileMap          ; (X=11,Y=8)-(X=20,Y=11), area of the Sega logo
    ld bc, $040a                    ; with incrementing pattern numbers (118-153) (last row has repeats of pattern #148
    call UpdateTileMapBlock         ;  for X=11,14,17,18,19, this is the horizontal white line at the bottom of each letter)
    ld a, $81
    ld (SongRequest), a             ; trigger playback of SegaMasterSystemSong
    LD_DE_PATTERN_ADDR $076
    ld hl, RLEPatternDataSEGA       ; load patterns 118-207 (118-153: Sega logo, 154-207: "MASTER SYSTEM")
    call DecodeRLECompPatterns
    ld hl, RLEPatternDataSnailMaze
    LD_DE_PATTERN_ADDR $003         ; load patterns 3-20 (snail maze game)
    call DecodeRLECompPatterns
    call InitSpritesAndScrolling    ; init grid of black sprites and horizontal scrolling
    call TurnOnDisplay
    call ShowBumperScreen
    di                              ; disable VDP interrupt
    ld hl, DetectCode
    ld de, DetectCodeRAM
    ld bc, DetectEnd - DetectCode   ; copy the cart etc., detection routine in RAM at C700
    ldir
    call DetectCodeRAM              ; run detection code for RAM
    di                              ; disable VDP interrupt (unnecessary, instruction removed in BIOS 2.0)
    ld a, (ROM_HardwareSlot)
    or a                            ; if we couldn't find any "TMR SEGA" string at any location for
    jr z, NoRomFound                ; any configuration, go show the instruction screen, else:
    ld hl, (ComputedChecksum)       ; HL = checksum computed
    ld de, (ROM_HeaderCopy + $0a)   ; DE = checksum from potential ROM header
    or a
    sbc hl, de
    jp z, BootGame + ROM_RAM_Offset ; if the checksums match, boot the ROM!
    call ResetTileMap               ; else we failed the checksum or region test, so clear the screen
    ld hl, SoftwareErrorTileMap
    ld b, $03                       ; update the tilemap to show the 3 strings of the
    call PrintStrings               ; software error string
SoftwareErrorScreenInfLoop:
    jr SoftwareErrorScreenInfLoop   ; and end in infinite loop
NoRomFound:
    call ShowInstructionsScreen     ; first show the instructions screen
NoRomFoundInfLoop:
    ei                              ; then wait in infinite loop until player triggers the secret game
    ld a, $80                       ; enable interrupt handler, but only sound and controller updates
    call WaitForIntHandlerDone
    ld a, (ControllerState)         ; read input state controller 1
    and $31
    cp $31
    jp z, SnailMazeGame             ; if up + button 1 + button 2 pressed, start the Snail Maze game!
    jr NoRomFoundInfLoop

ShowSegaCopyrightString:
    LD_DE_TILEMAP_XY $0a, $15
    ld b, $0b
    ld hl, SegaCopyrightTileMap
    rst $10                         ; write 11 entries starting at tile coords X=10, Y=21
    ret

SegaCopyrightTileMap:
.db $1b, " ", $1c, $1d, $1e, $1f, " 1986"  ; ($1b = (c) tile, $1d-$1f = SEGA tiles in the special font)

ShowInstructionsScreen:
    call ResetTileMap
    ld b, $0c                       ; the instructional screen has 11 lines of text to display
    ld hl, InstrScreenTileMap       ; plus the two "strings" of the "ENJOY!" logo

PrintStrings:                       ; number of strings to print in B, text data pointed by HL
    push bc
    ld e, (hl)
    inc hl
    ld d, (hl)                      ; DE = VRAM address of first tilemap entry to write
    inc hl
    ld b, (hl)                      ; B = string length
    inc hl                          ; then the string stored in ASCII
    rst $10                         ; since the ASCII  code and the tile # match, all we have to do
    pop bc                          ; is write the ASCII values in the tilemap
    djnz PrintStrings               ; repeat until we have written all the strings
    ret

SoftwareErrorTileMap:
 TILEMAP_XY $02, $02                ; write at tile coords (X=2, Y=2) ($1b = (c) tile, $1d-$1f = SEGA tiles in the special font)
.db $19, "MASTER SYSTEM ", $1b, " ", $1c, $1d, $1e, $1f, " 1986"
 TILEMAP_XY $02, $04
.db $0e, "SOFTWARE ERROR"

InstrScreenTileMap:
 TILEMAP_XY $1a, $00                ; write at tile coords (X=26, Y=0) (this version string shared with Software Error screen)
.db $04, "V1.3"
 TILEMAP_XY $02, $02
.db $13, "WELCOME TO THE SEGA"
 TILEMAP_XY $02, $04
.db $0e, "MASTER SYSTEM."
 TILEMAP_XY $02, $07
.db $19, "TO PLAY,JUST FOLLOW THESE"
 TILEMAP_XY $02, $09
.db $0d, "INSTRUCTIONS:"
 TILEMAP_XY $02, $0c
.db $19, "1.TURN OFF POWER ON POWER"
 TILEMAP_XY $04, $0e
.db $05, "BASE."
 TILEMAP_XY $02, $10
.db $1c, "2.INSERT CARD/CARTRIDGE INTO"
 TILEMAP_XY $04, $12
.db $07, "SYSTEM."
 TILEMAP_XY $02, $14
.db $1c, "3.TURN POWER BACK ON,AND...."
 TILEMAP_XY $09, $16                ; write top part of "ENJOY!!!" logo
.db $0d, $60, $61, $62, $63, $64, $65, $66, $67, $68, $69, $63, $63, $63
 TILEMAP_XY $09, $17                ; write bottom part of "ENJOY!!!" logo
.db $0d, $6a, $6b, $6c, $6d, $6e, $6f, $70, $71, $72, $00, $73, $73, $73

.ORG $0289
DetectCode:
    in a, (IO_1_Port)                       ; A = input port A + up/down port B (why?)
    ld hl, Port3E_Values + ROM_RAM_Offset   ; HL points to the array of the 3 port 3E's configs we are going to test
    ld b, $03                               ; (RAM & card, RAM & cartridge, RAM & expansion in that order)
TestPort3E_ConfigLoop:
    ld a, %11101011
    out (MemCtrlPort), a                    ; enable RAM & I/O only
    ld a, (hl)
    ld (ROM_HardwareSlot), a                ; enable the configuration to be tested and save it
    out (MemCtrlPort), a
    exx
    call CheckHeaderAt7ff0 + ROM_RAM_Offset
    call CheckHeaderAt3ff0 + ROM_RAM_Offset
    call CheckHeaderAt1ff0 + ROM_RAM_Offset
    exx
    inc hl
    djnz TestPort3E_ConfigLoop
    xor a                                   ; if we arrive  here we didn't find any card, cartridge or expansion
    ld (ROM_HardwareSlot), a                ;  so set ROM_HardwareSlot to zero and go back to BIOS
ReturnToBIOS:
    ld a, %11101011
    out (MemCtrlPort), a                    ; enable RAM and I/O only
    ld a, %11100011
    out (MemCtrlPort), a                    ; enable RAM,I/O and BIOS ROM
    ret                                     ; and return to BIOS!
CheckHeaderAt1ff0:
    ld hl, $1ff0                            ; we are going to check for a valid ROM header at each of
    jr CheckHeader                          ; the three possible locations ($1ff0, $3ff0, $7ff0)
CheckHeaderAt3ff0:
    ld hl, $3ff0
    jr CheckHeader
CheckHeaderAt7ff0:
    ld hl, $7ff0
CheckHeader:
    ld de, ROM_HeaderCopy
    ld bc, $0010                            ; copy 16 bytes from the potential ROM header location in RAM
    ldir
    ld hl, TMR_Sega + ROM_RAM_Offset
    ld de, ROM_HeaderCopy
    ld b, $08
CheckTMR_SegaLoop:                          ; check the presence of the "TMR SEGA" string in the first 8 bytes
    ld a, (de)                              ; and return if it's not there
    cp (hl)
    ret nz
    inc hl
    inc de
    djnz CheckTMR_SegaLoop
    call CheckRegion + ROM_RAM_Offset       ; passed "TMR SEGA" test, now check region then checksum
    pop af                                  ; (pop the return address so that the ret in ReturnToBIOS
    jr ReturnToBIOS                         ; actually returns to the caller of DetectCode, instead of
                                            ; right after the call to CheckHeaderAt[7/3/1]ff0 if fail)
TMR_Sega:
.db "TMR SEGA"

Port3E_Values:
.db %11001011     ; enable RAM, I/O and card slot only
.db %10101011     ; enable RAM, I/O and cartridge slot only
.db %01101011     ; enable RAM, I/O and expansion slot only

CheckRegion:
    ld de, $0000
    ld a, (ROM_HeaderCopy + $0f)           ; A = last byte of copied potential ROM header
    ld c, a                                ; save a copy in C (needed later for checksum test)
    and $f0                                ; extract high nibble which contains region code
    cp $40                                 ; if region code = 4 (SMS Export), we are good
    jr z, DoChecksum                       ; so perform the final test: the checksum test!
    ld hl, (ComputedChecksum)              ; if the region test fails, overwrite the copy of the
    inc hl                                 ; checksum read in the potential ROM header to be
    ld (ROM_HeaderCopy + $0a), hl          ; checksum computed + 1, so they mismatch even if
    ret                                    ; the ROM header contains a zero checksum

DoChecksum:
    ld a, c                                ; A = last byte of potential ROM header
    sub $0a
    and $0f                                ; extract low nibble (ROM size) and transforms it into
    push af                                ; an index into the ChecksumRanges table
    ld hl, ChecksumRanges + ROM_RAM_Offset
    ld c, a
    ld b, $00
    add hl, bc                             ; B = value looked up in ChecksumRanges table
    ld b, (hl)                             ;   = high byte of upper limit for first 32KB checksum range
    ld c, $f0                              ; so now BC = upper limit for checksum range (if B = $F, BC = $3FF0)
    ld hl, $0000                           ; (the ROM header is excluded from the checksum)
    call Checksum + ROM_RAM_Offset
    ld (ComputedChecksum), de              ; store computed checksum so far
    pop af
    sub $04                                ; if index into the ChecksumRanges table < 4
    ret c                                  ; (i.e. if ROM size is 8, 16, 32 or 48 KB) we are done
    ld c, a
    ld b, $00                              ; else use the index into the ChecksumRanges table - 4
    ld hl, ChecksumPages + ROM_RAM_Offset  ; as an index into the ChecksumPages table
    add hl, bc
    ld b, (hl)                             ; so now B contains the number of 16KB pages left to checksum
    ld a, $02                              ; A = 2 (we are going to start by checksumming ROM page 2)
ChecksumROMPagesLoop:
    push bc
    ld ($ffff), a                          ; maps slot 2 ($8000-$bfff) to ROM bank number in A
    inc a                                  ; (increment bank number for next iteration)
    push af
    call Checksum16KB + ROM_RAM_Offset
    pop af
    pop bc
    djnz ChecksumROMPagesLoop              ; repeat until we have checksummed all the pages
    ld (ComputedChecksum), de              ; and save the final checksum
    ret

Checksum16KB:
    ld bc, $4000                           ; we are going to checksum 16KB...
    ld hl, $8000                           ; starting at address $8000

Checksum:                                  ; (note: DE is init. to 0 at the start of CheckRegion)
    ld a, e
    add a, (hl)
    ld e, a                                ; add byte pointed by HL to current sum in DE
    ld a, d
    adc a, $00
    ld d, a
    inc hl                                 ; move HL to point to next byte
    dec bc                                 ; and decrement number of bytes to sum
    ld a, b
    or c
    jr nz, Checksum                        ; repeat until we have summed all bytes in the range
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
    out (MemCtrlPort), a                   ; enables RAM and I/O only
    ld a, (ROM_HardwareSlot)               ; set the port 3E's config where we found a valid ROM to run
    out (MemCtrlPort), a
    jp $0000                               ; and boot the game!
DetectEnd

WaitRoutine:                        ; runs B iterations of a wait loop that decrements DE from 16K to 0
    ld de, $4000                    ; about 120 ms per loop
WaitLoop:
    dec de
    ld a, d
    or e
    jr nz, WaitLoop
    djnz WaitRoutine
    ret

WaitRoutineTable:                           ; a table of values of B to pass to WaitRoutine, used in BIOS 1.0
.db $0e, $0d, $0c, $0a, $08, $06            ; unused in this BIOS, and removed in BIOS 2.0

ResetButtonHandler:
    di                              ; disable interrupts
    ld sp, RAM_Base+RAM_Size-$10    ; reset stack near top of RAM
    ld hl, RAM_Base
    ld de, RAM_Base+1
    ld bc, RAM_Size-1
    ld (hl), $00
    ldir                            ; zero out entire RAM
    ld hl, PSG_ResetData            ; write 4 bytes from PSG_ResetData to port 7F (PSG)
    ld c, PSGPort                   ; which set the four channels to "volume off"
    ld b, $04
    otir
    jp BiosWarmBoot                 ; and jump to rest of BIOS boot routine

InitPaletteData:
.db $20                                     ; 32 bytes to write
.dw VDP_WriteCRAM                           ; writes to the data port go to CRAM
.db $00, $3f, $3e, $3f, $30, $30, $38, $3f  ; black, white, pale turquoise, white, blue, blue, sky blue, white
.db $37, $3f, $00, $00, $00, $00, $00, $00  ; pink, white, blacks
.db $00, $03, $30, $0f, $07, $16, $3f, $02  ; black, red, blue, yellow, orange, brown, white, dark red
.db $00, $0f, $00, $00, $00, $00, $00, $00  ; black, yellow, blacks

MaskableInterruptHandler:
    push af
    in a, (VDPCtrlPort)             ; read VDP control port (which also disables int. req. line)
    rlca                            ; bit 7 (1 = VBLANK, 0 = H-Line) in Carry
    jp nc, NotVBlank                ; jump if not VBLANK
    push bc                         ; save both register banks
    push de
    push hl
    push ix
    push iy
    ex af, af'
    exx
    push af
    push bc
    push de
    push hl
    ld a, (DoResetCheck)            ; if DoResetCheck = 0, skip reset check
    or a
    jr z, SkipResetCheck
    in a, (IO_2_Port)
    and $10                         ; if reset check not skipped, check value of reset button (0 = pressed)
    ld hl, ResetButtonState
    ld c, (hl)                      ; C = value of reset button last frame
    ld (hl), a                      ; (save current value as previous value for next frame)
    xor c                           ; if the state of the reset button is different from the one last frame
    and c                           ; and last frame was not pressed (=> we just pressed the button)
    jp nz, ResetButtonHandler       ; execute RESET
SkipResetCheck:
    ld a, (InterruptArg)            ; check "interrupt argument byte"
    rrca
    push af
    call c, UpdateSpriteAttrTable   ; if bit 0 InterruptArg set, update VDP's SAT from SpriteRAM
    pop af
    rrca
    call c, UpdateGraphicsData      ; if bit 1 InterruptArg set, write graphics data to VDP
    rrca
    push af
    call c, UpdateSegaLogoScrolling ; if bit 2 InterruptArg set, update the scrolling & uncovering of the SEGA logo
    call CheckController1           ; always: check portA input, update ControllerState/ControllerState2 input state vars
    pop af
    rrca
    push af
    call c, UpdateWriteMazeColumn   ; if bit 3 InterruptArg set, we are scrolling in a Snail Game maze,
    pop af                          ; and we call this function to write a new column to the tilemap as needed
    rrca
    call c, UpdateSnailMazeGameTime ; if bit 4 InterruptArg set, update state and display of snail maze game time
    call UpdateSound                ; always: update sound
    xor a
    ld (InterruptArg), a            ; reset InterruptArg to 0
    pop hl                          ; then restore both register banks
    pop de
    pop bc
    pop af
    ex af, af'
    exx
    pop iy
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ei                              ; enable interrupts and return
    ret
NotVBlank:                          ; so this is a line interrupt
    push de
    ld de, (MazeScrollH)
    LD_D_VDP_REG_NUM $08            ; we are scrolling in a Snail Game maze, so
    rst $20                         ; update X scroll VDP register with the value in MazeScrollH
    LD_DE_VDP_REG_VAL $0a, $ff
    rst $20                         ; and set Line Counter to 255 (to prevent unwanted HBlank interrupts)
    pop de
    pop af
    ei
    ret

WaitForIntHandlerDone:
    ld hl, InterruptArg             ; set InterruptArg to value passed in A
    ld (hl), a
WaitForIntHandlerLoop:              ; then wait for the interrupt handler to reset InterruptArg to 0
    ld a, (hl)
    or a
    jr nz, WaitForIntHandlerLoop
    ret

; GfxDataBlockAddr contains the address of a data block with the following format:
; byte 0: how many data bytes to write to VRAM/CRAM
; bytes 1-2: word for VDP control register that defines where to write the data
; bytes 3-n: the data to write
UpdateGraphicsData:
    ld hl, (GfxDataBlockAddr)
    ld b, (hl)                      ; B = how many bytes to write
    inc hl
    ld e, (hl)
    inc hl
    ld d, (hl)                      ; de = where to write
    inc hl
    rst $20                         ; write VDP control register
    ld c, VDPDataPort
    otir                            ; then writes data to VDP data port
    ret

Read_VRAM_Byte:                     ; (UNUSED IN THIS BIOS) read in A VRAM byte at address in DE
    rst $20
    in a, (VDPDataPort)
    ret

ReadTileMapEntries:                 ; (UNUSED IN THIS BIOS) read BC tilemap entries (first byte)
    rst $20                         ; at VRAM address in DE, to RAM address in HL
ReadTileMapEntry:
    in a, (VDPDataPort)             ; write VRAM address
    ld (hl), a                      ; read and copy tilemap entry 1st byte
    ex (sp), hl                     ; wait a bit...
    ex (sp), hl
    in a, (VDPDataPort)             ; read tilemap entry 2nd byte to advance internal VDP pointer
    inc hl                          ; increment RAM address
    dec bc
    ld a, b
    or c
    jr nz, ReadTileMapEntry         ; and repeat until we have read BC entries
    ret

; Writes a rectangular block of tilemap entries.
; Height in B, width in C, (start address in VRAM | VDP_WriteVRAM) in DE, and source data in HL
; source data is one byte per tilemap entry (the pattern number byte, the other one is always set to 0)
; source data layout is width bytes for the first "row", width bytes for the second "row" etc.
UpdateTileMapBlock:
    ld a, c
    ld c, b                         ; swap B & C (so now B is the width and can be used as the
    ld b, a                         ; number of consecutive tilemap entries to write for rst $10)
UpdateTileMapRow:
    push bc
    rst $10                         ; write one row of tilemap entries
    ex de, hl
    ld bc, $0040
    add hl, bc                      ; advance DE by 64 bytes = 32 tilemap entries
    ex de, hl                       ; so that DE points to the start of the next row in the block
    pop bc
    dec c                           ; decrement C = row counter
    jr nz, UpdateTileMapRow         ; and keep going until we have written all rows
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
    rst $20                         ; set VRAM start address from DE
CompressedLineDataLoop:
    ld a, (hl)                      ; fetch a line
    exx                             ; save BC, DE, HL
    ld bc, $0400 | VDPDataPort      ; 4 bitplanes, and we will write to VDP data port
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
    jr nz, CompressedLineDataLoop   ; and loop until counter is 0
    ret

VDP_ResetData:
 VDP_REG_VAL $00, $36   ; reg.0 (Mode Control 1) : don't disable scrolling for cols 24-31 and rows 0-1, mask col 0
                        ; with overscan color from reg. 7, enable line interrupt, don't shift sprites left by 8
                        ; use Mode 4, normal display
 VDP_REG_VAL $01, $a0   ; reg.1 (Mode Control 2) : display blanked, enable frame interrupt, sprites are 8x8
 VDP_REG_VAL $02, $ff   ; reg.2 (Name Table Base Address) : tilemap at $3800 (highest 2KB of the 16KB VRAM)
 VDP_REG_VAL $03, $ff   ; reg.3 (Color Table Base Address) : needs to be set to FF or pattern and tilemap data
                        ; will be fetched incorrectly
 VDP_REG_VAL $04, $ff   ; reg.4 (Background Pattern Generator Base Address) : bits 2-0 must be set or pattern data
                        ; and tilemap data will be fetched incorrectly
 VDP_REG_VAL $05, $ff   ; reg.5 (Sprite Attr. Table Base Address) : $3f00 (unused top 256 bytes of tilemap memory)
 VDP_REG_VAL $06, $fb   ; reg.6 (Sprite Pattern Generator Base Address) : sprite patterns in first 8 KB of VRAM
 VDP_REG_VAL $08, $00   ; reg.8 (Background X Scroll) : no scrolling
 VDP_REG_VAL $09, $00   ; reg.9 (Background Y Scroll) : no scrolling
 VDP_REG_VAL $0a, $ff   ; reg.A (Line Counter) turn off HBLANK interrupt requests
 VDP_REG_VAL $07, $00   ; reg.7  (Overscan/Backdrop Color) : color 0 (from sprite palette)
.dw VDP_WriteCRAM + $10 ; Writes to the data port goes to CRAM starting at byte 16 (start of 2nd/sprite palette)

; Phantasy Star Tile Decoder
; Decode and write to VRAM a list of consecutive pattern lines stored in RLE compressed format.
; HL = address of compressed data, DE = where to write in VRAM | VDP_WriteVRAM 
; The compressed format is the "Phantasy Star tile" format: 
; 4 compressed byte streams in sequence, one per bitplane.
; for each byte stream: read the "counter" byte (let's call its value n)
; if the counter byte's bit7 is *not* set, the next byte is to be written n times ("repeat mode")
; if the counter byte's bit7 is set, it is followed in the stream by n bytes of data ("non-repeat mode")
; if the counter byte is 0, we have reached the end of the stream; else after writing the data
; following this counter byte, fetch next counter byte etc.
DecodeRLECompPatterns:
    ld b, $04                       ; for each of the 4 bitplanes...
DecodeRLECompBitPlaneLoop:
    push bc
    push de
    call DecodeRLECompBitPlane      ; decode the compressed stream for that bitplane
    pop de
    inc de                          ; then reposition VRAM address to the start adress we used
    pop bc                          ; plus one to write to the next bitplane
    djnz DecodeRLECompBitPlaneLoop
    ret                             ; all 4 bitplanes decoded, we are done!

DecodeRLECompBitPlane:
    ld a, (hl)                      ; read counter byte (with repeat flag)
    inc hl
    or a
    ret z                           ; counter byte is 0, we are done with this bitplane!
    ld b, a
    ld c, a                         ; c = counter byte (with repeat flag)
    res 7, b                        ; b = counter byte (without repeat flag; only data bytes count)
WritePatternLineBitplane:
    ld a, e
    di
    out (VDPCtrlPort), a
    ld a, d
    out (VDPCtrlPort), a            ; write VRAM address of bitplane of this line (from de)
    ld a, (hl)
    out (VDPDataPort), a            ; load bitplane data, and write VRAM
    ei
    bit 7, c
    jr z, RepeatMode                ; if bit7 set (non-repeat mode), increment source address
    inc hl
RepeatMode:
    inc de                          ; increment de to point to next pattern line
    inc de
    inc de
    inc de
    djnz WritePatternLineBitplane   ; decrement data bytes count and loop if non-zero
    jr nz, DecodeRLECompBitPlane    ; done with this counter byte. If bit7 wasn't set,
    inc hl                          ; we were in repeat mode and so we must advance source address
    jr DecodeRLECompBitPlane        ; so we aren't stuck on (repeated) data byte.
                                    ; Then fetch next counter byte.

ResetScrollRegisters:               ; (UNUSED IN THIS BIOS)
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
    rst $20                         ; write 0 to VDP register #9 (Y scroll)
    ld a, (ResetScrollX_Val)
    ld e, a
    dec d
    rst $20                         ; write 0 to VDP register #8 (X scroll)
    ret

ResetTileMap:
    ld de, TileMapVRAM              ; write in VRAM at address $3800 (tilemap)
    ld bc, $0700                    ; $700 bytes (= length of tilemap: 32 x 28 x 2 bytes)
    ld l, $00                       ; the value 0
    rst $28
    ret

CheckController1:
    in a, (IO_1_Port)               ; A = input port A + up/down port B
    ld hl, ControllerState
    cpl                             ; A = not A (so now a bit to 1 = button pressed)
    ld c, a
    xor (hl)                        ; XOR current state with previous state
    ld (hl), c                      ; save current input state (to become previous state for next call)
    inc hl
    and c                           ; AND the result of XOR above with the current state so now a bit
    ld (hl), a                      ; to 1 = button pressed this frame & not last frame, save the result
    ret

ShowBumperScreen:
    ld a, UpdateLogoScroll | UpdateSAT
    call WaitForIntHandlerDone      ; Int handler will call UpdateSpriteAttrTable and UpdateSegaLogoScrolling
    ld a, (VoiceSyncFlag)           ; wait for sound sync flag, initially 0, set to $80 by soundRoutine7
    or a                            ; called from voice 1 right after the rest after the starting tone ramp
    jr z, ShowBumperScreen
    LD_DE_TILEMAP_XY $05, $0e       ; Make the "MASTER SYSTEM" logo appear
    ld hl, MasterSystemTileMap      ; by writing a 3 high, 23 wide rectangular block of tilemap entries
    ld bc, $0317                    ; at tile coordinates (X=5,Y=14)-(X=27,Y=16)
    di
    call UpdateTileMapBlock
    call ShowSegaCopyrightString
    ei
WaitForEndOfSegaMasterSystemSong:
    ld a, (VoiceEndedFlag)
    or a
    jr z, WaitForEndOfSegaMasterSystemSong
    ret

InitSpritesAndScrolling:
    ld a, $03
    ld (TempVar), a                 ; Initially wait 3 VBlanks before the first call to UpdateSegaLogoScrolling
    ld a, $f0                       ; and actually starting to scroll the Sega logo
    ld (SegaLogoScrollH), a         ; Initial H scroll value: $f0
    ld e, a
    LD_D_VDP_REG_NUM $08            ; VDP register 8 (horizontal scrolling) = $f0
    rst $20
    ld l, $ff
    LD_DE_PATTERN_ADDR $fe          ; write $ff at VRAM addresses $1fc0-$1fff (patterns #254 & #255)
    ld bc, $0040                    ; (makes them black with current palettes)
    rst $28
    ld de, SpriteRAM
    ld a, $08
Init4SpritesY:
    ld hl, InitSpritesYData         ; initialize Y of first 32 sprites in 8 repeats of the 4 values group
    ld bc, $0004                    ; in InitSpritesYData
    ldir
    dec a
    jr nz, Init4SpritesY
    ld a, $d0
    ld (de), a                      ; Y of 33rd sprite to $d0 so that it (and all remaining sprites) will not be drawn
    ld e, $80                       ; de = SpriteRAM + $80
    ld bc, $08ff                    ; HL now points to InitSpritesXandPatternData. We are going to again initialize
InitSpriteGroupXandPattern:
    ld a, $04                       ; the first 32 sprites in B=8 groups of A=4.
InitSpriteXandPattern:
    ldi                             ; for each group: write the X byte, then the pattern # byte
    ldi
    dec a
    jr z, InitSpriteGroupDone
    dec hl                          ; if we are not done with this group yet, "rewind" HL 2 bytes
    dec hl                          ; (to compensate for the auto-increments by ldi)
    jr InitSpriteXandPattern
InitSpriteGroupDone:
    djnz InitSpriteGroupXandPattern
    ld a, UpdateSAT
    jp WaitForIntHandlerDone        ; A = 1 => Int handler will call UpdateSpriteAttrTable

InitSpritesYData:
.db $38, $40, $48, $50

InitSpritesXandPatternData:
.db $47, $fe, $5a, $fe, $6c, $fe, $7e, $fe
.db $4f, $ff, $62, $ff, $74, $ff, $86, $ff

; At the end of InitSpritesAndScrolling, we end up with the following grid of sprites:
;
;        71   79      90  98     108  116    126  134
;    57   +----+----+-+----+----+-+----+----+-+----+----+
;         | fe | ff | | fe | ff | | fe | ff | | fe | ff |
;    65   +----+----+-+----+----+-+----+----+-+----+----+
;         | fe | ff | | fe | ff | | fe | ff | | fe | ff |
;    73   +----+----+-+----+----+-+----+----+-+----+----+
;         | fe | ff | | fe | ff | | fe | ff | | fe | ff |
;    81   +----+----+-+----+----+-+----+----+-+----+----+
;         | fe | ff | | fe | ff | | fe | ff | | fe | ff |
;         +----+----+-+----+----+-+----+----+-+----+----+
;
; The black sprites cover up the SEGA logo, except for the little spaces between blocks of sprites

UpdateSegaLogoScrolling:
    ld hl, TempVar                  ; wait for 3 VBlanks the first time (cf. InitSpritesAndScrolling)
    dec (hl)                        ; then every other frame afterwards (cf. below)
    ret nz
    ld (hl), $02                    ; reinitialize VBlanks wait counter
    ld hl, SegaLogoScrollH
    ld a, (hl)
    cp $01
    jr nz, UpdateScrollAndSprites
    ld a, $80
    ld (InterruptArg), a            ; if we are finished with the logo scrolling, write $80 in InterruptArg
    ret                             ; (which will disable the optional update calls in the interrupt)
UpdateScrollAndSprites:
    ld e, a
    LD_D_VDP_REG_NUM $08
    rst $20                         ; write value of SegaLogoScrollH in VDP HScroll register
    inc (hl)                        ; then increment it
    ld a, (hl)
    neg                             ; A = -SegaLogoScrollH so as SegaLogoScrollH goes $f1, $f2...
    bit 7, a                        ; A goes $0f, $0e... so a counter of many scrolling updates left to do
    ret nz                          ; when A goes from $00 to $ff, we are done with the scrolling!
    cp $08
    LD_DE_PATTERN_ADDR $ff          ; if A >= 8, we are going to update tile data for sprite $ff
    jr c, UpdateMaskingSpriteTile
    sub $08
    LD_DE_PATTERN_ADDR $fe          ; else update tile date for sprite $fe
UpdateMaskingSpriteTile: 
    ld hl, MaskingSpriteTiles       ; we are going to "shrink" the $ff sprite, then the $fe sprite, one column at a time:
    ld b, $00                       ; first update we will write $7f in all of the tile's bytes, so the leftmost column
    ld c, a                         ; will be color #0 (transparent, see the logo through), and the others columns are 
    add hl, bc                      ; color #15 (masking black). Next time, we write $3f (so 2 transparent columns) etc.
    ld l, (hl)
    ld c, $20
    rst $28                         ; write the 32 bytes of sprite tile VRAM with the value read from MaskingSpriteTiles
    ret

MaskingSpriteTiles:
.db $00, $01, $03, $07, $0f, $1f, $3f, $7f

;---------------------------------------------------------------------------------------------------------
; SOUND CODE AND DATA START HERE
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
;   $e6 $nn $nn $nn: some kind of for loop using $e4 above (UNUSED IN THIS BIOS)
;   $e7: set VoiceSyncFlag to $80 (used to synch. sound with display of "MASTER SYSTEM")
;   $e8 $nn: add $nn to offset into the tone table (UNUSED IN THIS BIOS)
;
;   To trigger a song, write the song index + $81 at address $cf00 (SongRequest)
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

.ORG $05B8
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
    call ClearSound
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

SoundRoutineE7:
    ld a, $80
    ld (VoiceSyncFlag), a           ; set synchronization flag used by SEGA logo display code
    dec de                          ; to know when to display the "MASTER SYSTEM" part
    ret

SoundRoutineE0:                     ; set voice's base volume to value passed in next sound byte
    ld a, (de)
    ld (ix + Voice.volume), a
    ret

SoundRoutineE1:                     ; end of voice
    ld a, $80
    ld (VoiceEndedFlag), a          ; set end of voice flag
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
    ld hl, VoiceEndedFlag           ; memzero sound state + voice structs
    ld de, VoiceEndedFlag+1
    ld bc, $7f
    ld (hl), $00
    ldir
    exx
    exx
    ld hl, PSG_ResetData            ; turn off all sound channels
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
.dw $0060                ; ~D6 (used as delta tone)
.dw $005b                ; ~D#6
.dw $0055                ; ~E6
.dw $0051                ; ~F6
.dw $004c                ; ~F#6 (used as delta tone)
.dw $0048                ; ~G6
.dw $0044                ; ~G#6
.dw $0040                ; ~A6 (used as delta tone)
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

SegaMasterSystemSong:
.db $04                                    ; 4 channels
.db $a0, $20, $05, <SegaMasterSystemSongChan0, >SegaMasterSystemSongChan0, $05, $01, $04, $0d
.db $a0, $21, $05, <SegaMasterSystemSongChan1, >SegaMasterSystemSongChan1, $05, $01, $04, $0d
.db $a0, $22, $05, <SegaMasterSystemSongChan2, >SegaMasterSystemSongChan2, $05, $01, $04, $0d
.db $80, $23, $05, <SegaMasterSystemSongChan3, >SegaMasterSystemSongChan3, $00, $00, $00, $00

SegaMasterSystemSongChan3:
.db $80, $10, $e2, $04, $e3, $01, $e0, $0d, $8d, $08, $e3, $02, $e0, $0f, $1c, $e1

SegaMasterSystemSongChan1:
.db $81, $a5, $08, $e5, $80, $e7, $8d, $81, $10, $80, $0c, $e1

SegaMasterSystemSongChan0:
.db $85, $a9, $08, $e5, $80, $8a, $8a, $10, $80, $08, $e1

SegaMasterSystemSongChan2:
.db $88, $ac, $08, $e5, $80, $86, $86, $10, $80, $08, $e1

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
.db $03, $03, $03, $e3, $02, $e0, $0f, $18, $e1

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
.db $02, $02, $e3, $02, $e0, $0f, $10, $e1

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
.dw SegaMasterSystemSong                ; SEGA Master System logo
.dw SnailMazeGameEndSong                ; Snail Maze Game End (finished 12th round!)
.dw SnailMazeLoopSong                   ; Snail Maze Game Song (loop)
.dw SnailMazeEndRoundSong               ; Snail Maze Game Round Victory

;-----------------------------------------------------------------------
; SNAIL MAZE GAME'S CODE STARTS HERE
; (Note: Sprite & Entity code  also used in Alex Kidd in Miracle World)
;-----------------------------------------------------------------------
.ORG $0B90
SnailMazeGame:
    ld a, $ff
    ld (DoResetCheck), a            ; enable RESET test in interrupt handler
    ld hl, InitRoundAndTimeValues
    ld de, RoundNumberDCB
    ld bc, $0004
    ldir                            ; initialize round number and time left values
    ld hl, MazeTopTilemap
    ld b, $1e
InitTopBorderLoop:
    ld (hl), $03                    ; init 30 tilemap entries with tile #3 to form
    inc hl                          ; the upper wall of the maze.
    inc hl                          ; (the top left and right corners are just black, using tile 0)
    djnz InitTopBorderLoop
    di
    ld hl, RoundTimeString
    LD_DE_TILEMAP_XY $12, $05
    ld b, $0a                       ; write the "RD    TIME" string at the top right of the screen
    rst $10                         ; (write 10 tilemap entries starting at tile coords (X=18, Y=5))
StartNewRound:
    di
    ld hl, RoundNumberDCB
    LD_DE_TILEMAP_XY $15, $05
    call PrintYellowNumber          ; display round number (DCB) at tile coords (X=21, Y=5)
    ei
    ld a, UpdateMazeTime
    call WaitForIntHandlerDone      ; Int handler will call UpdateSnailMazeGameTime
    ld ix, Snail
    call ResetEntityStruct          ; rest snail's entity struct
    ld (ix + Entity.x), $08         ; snail always starts at X = 8
    ld (ix + Entity.pattern), $0d   ; pattern index: snail points right
    ld (ix + Entity.animVBlanksCtrRst), $04  ; snail updates its animation every 4 VBlanks
    ld ix, Goal
    call ResetEntityStruct          ; reset goal's entity struct
    ld ix, StartPoint
    call ResetEntityStruct          ; reset start points' entity struct
    call LoadAndScrollMaze          ; returns once the maze for this round is done scrolling in
    ld a, $01
    ld (Snail.entityId), a          ; assigns entity id = 1 to snail
    inc a
    ld (Goal.entityId), a           ; assigns entity id = 2 to goal
    inc a
    ld (StartPoint.entityId), a     ; assigns entity id = 3 to start point
    call UpdateEntities             ; do a first update of entities to finish their initialization
    ld bc, $1400 | UpdateSAT
    call Wait_B_Vblanks             ; wait 20 VBlanks (which will run UpdateSpriteAttrTable)
    ld a, $83
    ld (SongRequest), a             ; triggers playback of SnailMazeLoopSong
SnailMazeGameLoop:
    ld a, UpdateMazeTime | UpdateSAT
    call WaitForIntHandlerDone      ; wait 1 VBlank (UpdateSpriteAttrTable and UpdateSnailMazeGameTime)
    call UpdateEntities
    ld hl, GoalReachedFlag          ; goal reached flag (set to Y snail = Y goal when goal reached)
    ld a, (hl)
    or a
    jr nz, WonRound                 ; if goal reached, jump
    inc hl
    ld a, (hl)                      ; else check "ran out of time" flag (set to $ff if have)
    or a
    jr z, SnailMazeGameLoop         ; if we haven't yet run out of time, loop
    di                              ; oops we are out of time
    ld hl, TimeUpString
    LD_DE_TILEMAP_XY $0d, $07       ; at tile coords (X=13, Y=7)
    ld b, $07                       ; write tilemap entries for "TIME UP" string
    rst $10
    ei
    ld b, $00                       ; BC = $00be (C was set to VDPDataPort by rst $10)
BlinkTimeUpString:                      ; we are going to have the "TIME UP" string blink for 180 VBlanks
    push bc
    ld bc, $0180                    ; calls interrupt handler once, with no optional flags
    call Wait_B_Vblanks
    ld de, $79db                    ; 2nd byte in tilemap for tile coords (X=13, Y=7)
    ld hl, BlinkTimeUpCouter
    inc (hl)                        ; increment counter used to time change of palette
    di
    rst $20
    ld b, $07
WriteTimeUpPalette:
    ld a, (BlinkTimeUpCouter)
    and $08                         ; for 8 frames, write 0 in 2nd byte tilemap entry (palette = 0)
    out (VDPDataPort), a            ; and for the next 8 frames, we will write $08 (palette = 1)
    push af                         ; and so the "TIME UP" text will blink white and yellow
    pop af                          ; wait a bit before writing for the next character in the string
    in a, (VDPDataPort)
    djnz WriteTimeUpPalette         ; repeat until we write 2nd byte tilemap entry for each char in "TIME UP"
    ei
    pop bc
    djnz BlinkTimeUpString
    jp ResetButtonHandler           ; and finally reset the machine!
WonRound:
    ld (hl), $00                    ; first reset goal reached flag to 0
    ld a, $84
    ld (SongRequest), a             ; trigger playback of SnailMazeEndRoundSong
    ld bc, $6000 | UpdateSAT
    call Wait_B_Vblanks             ; wait 96 VBlanks (which run UpdateSpriteAttrTable)
    ld a, $d0
    ld (SpriteRAM), a               ; stop displaying sprites (writes $d0 as Y of sprite #0)
    ld bc, $3000 | UpdateSAT        ; wait 48 VBlanks (which run UpdateSpriteAttrTable)
    call Wait_B_Vblanks
    ld hl, RoundNumberDCB
    ld a, (hl)
    add a, $01                      ; increment and save round number
    daa
    ld (hl), a                      ; the DCB one...
    inc hl
    inc (hl)                        ; and the binary one
    ld a, (hl)
    cp $0d
    jr nc, WonGame                  ; jump if we just completed round 12
    ld e, a
    ld d, $00
    ld hl, RoundTimesTable-2
    add hl, de
    ld a, (hl)                      ; else look up table of round times (indexed by round number)
    ld hl, (RoundTimeUpdCtr)        ; H = RoundTimeLeft, L = RoundTimeUpdCtr
    add a, h                        ; add remaining seconds
    daa                             ; convert to DCB
    jr nc, RoundTimeLessThan99s
    ld a, $99                       ; sum is capped to 99 seconds
RoundTimeLessThan99s:
    ld h, a
    ld l, d
    ld (RoundTimeUpdCtr), hl        ; set timer to this new value, and VBlank counter to 0
    jp StartNewRound
WonGame:
    ld a, $82
    ld (SongRequest), a             ; trigger playback of SnailMazeGameEndSong!
    ld hl, VictoryString
    ld de, $79d0
    ld b, $10
    di
    rst $10                         ; display "CONGRATULATIONS!" string at tile (X=8, Y=7)
    ei
    ld bc, $0080
    call Wait_B_Vblanks             ; wait 256 VBlanks...
    ld bc, $0080
    call Wait_B_Vblanks             ; and 256 more...
    jp ResetButtonHandler           ; then reset the machine!

VictoryString:
.db "CONGRATULATIONS!"

Wait_B_Vblanks:                     ; calls B times interrupt handler with InterruptArg = C
    ld a, c
    call WaitForIntHandlerDone
    djnz Wait_B_Vblanks
    ret

LoadAndScrollMaze:
    ld a, (RoundNumber)             ; round number (coded in binary)
    dec a                           ; mazes are stored in sequence, and each maze is $83 bytes of data
    ld d, a                         ; so the offset from the first maze to the one we want is:
    add a, a                        ; offset = $83 * (round number - 1)
    add a, d
    ld e, a                         ; E = 3*(round - 1) and D = (round - 1) so DE = $103 * (round - 1)
    xor a                           ; A = 0 so DA = $100 * (round - 1)
    rr d
    rra                             ; divide DA by 2 so DA = $80 * (round - 1) (A's bits 0-6 are 0)
    add a, e                        ; since round <= 12, E fits in bits 0-6 of A, and so
    ld e, a                         ; we now have DA = DE = $83 * (round - 1) !
    ld hl, SnailMaze1
    add hl, de                      ; HL = address of maze data for this round
    ld a, (hl)
    ld (Snail.y), a                 ; 1st byte of maze data is snail's initial Y position
    inc hl
    ld a, (hl)
    ld (Goal.y), a                  ; 2nd byte of maze data is goal's Y position
    inc hl
    ld a, (hl)
    ld (Goal.x), a                  ; 3rd byte of maze data is goal's X position
    inc hl
    ld de, MazeTilemap              ; here we store the RAM copy of the tilemap for the maze
    ld b, $80                       ; 32 x 16 = 512 entries, so 128 bytes, each entry coded with 2 bits
DecodeMazeByte:                         ; as we have only 4 possible tiles (4, 5, 6, 7)
    push bc
    ld b, $04                       ; 4 entries per byte
    ld c, (hl)                      ; fetch one maze data byte and process 2 bits at a time
DecodeMaze2Bits:
    ld a, c                         ; mapping table is: %00 -> tile #5 = %101 (wall to the bottom)
    inc a                           ;                   %01 -> tile #6 = %110 (wall to the right)
    and $03                         ;                   %10 -> tile #7 = %111 (walls to right & bottom)
    add a, $04                      ;                   %11 -> tile #4 = %100 (no walls)
    ld (de), a                      ; so $42 = %01000010 gives the tiles: 6, 5, 5, 7: .|__|_
    inc de                          ; note that tthe tile index also gives us the collision info:
    xor a                           ; bit1 is set if wall to the right, bit0 is set if wall to the bottom
    ld (de), a                      ; sets 0 for 2nd byte of tilemap entry
    inc de
    rr c                            ; shift 2 next bits to process to bits 0-1 of C
    rr c
    djnz DecodeMaze2Bits
    inc hl
    pop bc
    djnz DecodeMazeByte
    ld hl, MazeTilemap + $3e        ; address of (X=31, Y=0) in maze's RAM tilemap
    ld bc, $1074
    ld de, $0040                    ; offset between two rows in the tilemap
OverwiteLastMazeColumn:
    ld (hl), c                      ; replace all 16 RAM tilemap entries in column 31
    add hl, de                      ; with tile #74 (solid blue, no wall)
    djnz OverwiteLastMazeColumn     ; (this is the column of solid blue you see on the right)
    ld a, $ff
    ld (ScrollMazeFlag), a          ; set flag to mark the beginning of scrolling the maze
    LD_DE_VDP_REG_VAL $00, $36      ; write $36 to VDP register 0, same value that is written
    rst $20                         ; during BIOS init (cf. VDP_ResetData)
WaitForMazeScrollingDone:
    ld a, UpdateMazeCol
    call WaitForIntHandlerDone      ; Int handler will call UpdateWriteMazeColumn
    ld a, (ScrollMazeFlag)
    or a
    jr nz, WaitForMazeScrollingDone ; loop until scrolling done
    LD_DE_VDP_REG_VAL $00, $06      ; write $06 to VDP register 0 => don't mask column 0,
    rst $20                         ; and disable line interrupts
    LD_DE_VDP_REG_VAL $0a, $ff
    rst $20                         ; write $ff to VDP register $A (Line Counter)
    ret

UpdateWriteMazeColumn:
    LD_DE_VDP_REG_VAL $08, $00
    rst $20                         ; write 0 in VDP register 8 (Background X Scroll) : no scrolling
    LD_DE_VDP_REG_VAL $0a, $37      ; write $37 in VDP register $A (Line Counter = $37 = 55)
    rst $20                         ; which is the line where we start the scrolling of the maze
    ld hl, MazeScrollH
    ld a, (hl)                      ; read current X scroll RAM value
    dec (hl)                        ; and decrement it, if it reaches 0 we are done scrolling
    jr nz, WriteMazeColumnToVDP     ; if send a column of maze to the VDP if needed
    xor a
    ld (ScrollMazeFlag), a          ; done scrolling so reset the flag to signal
    ret
WriteMazeColumnToVDP:
    neg                             ; before neg, a = $00, $ff, $fe so after 0, 1, 2
    ld e, a
    and $07                         ;  we only run the code below every 8 VBlanks =
    ret nz                          ;  = every time we have scrolled in one maze column
    ld a, e
    rrca
    rrca
    add a, $c0                      ; A increments by 8, so E increments by 2 = one tile map entry
    ld e, a
    ld d, $79                       ; so DE = $79c0 + A/4 = tilemap entry (X=A/8, Y=7)
    ld l, a
    ld h, >MazeTopTilemap           ; HL = $c9c0 + A/4 = RAM tilemap entry (X=A/8, Y=-1)
    ld b, $11                       ; we are going to write 17 tile entries
WriteMazeColumnLoop:                ; the top border ones: tile #0 at the corners, else #3 (top wall)
    push bc                         ; and 16 for maze column A/8
    ld a, (hl)
    rst $08                         ; write VDP tilemap entry (1st byte) from the one in RAM
    ld bc, $0040
    add hl, bc                      ; then go down one row in both tilemaps
    ex de, hl
    add hl, bc
    ex de, hl
    pop bc
    djnz WriteMazeColumnLoop        ; until we have written the column (17 entries)
    ret

UpdateSnailMazeGameTime:
    ld hl, RoundTimeUpdCtr          ; decrement VBlanks counter
    dec (hl)
    ret p                           ; if it is above -1, return, else reload it with 59
    ld (hl), $3b                    ; this way the code below only runs every 60 VBlanks
    inc hl                          ; that is every second
    ld a, (hl)                      ; A = time left
    sub $01
    daa                             ; subtract one second, and save (after reconverting to DCB)
    ld (hl), a
    LD_DE_TILEMAP_XY $1d, $05       ; we are going to display time at (X=29, Y=5)
    jr nz, PrintYellowNumber        ; if time is now greater or equal to zero, display it
    dec a
    ld (OutOfTimeFlag), a           ; else first set OutOfTimeFlag to $ff to track we ran out of time!
    inc a                           ; and set A back to 0 so it is displayed as a tranparent tile

PrintYellowNumber:                  ; number is 2 digits stored in DCB at address pointed by HL
    rst $20                         ; send VRAM address to VDP
    ld a, (hl)                      ; A = 2 digit number (BCD)
    rra
    rra
    rra
    rra
    and $0f                         ; extract high nibble = first digit
    jr z, FirstDigitIsZero          ; if zero, we won't display it (tile #0)
    or $30                          ; else convert to ASCII code which is also the tile number
FirstDigitIsZero:
    call PrintYellowDigit
    ld a, (hl)
    and $0f                         ; extract low nibble = second digit
    or $30                          ; and convert to ASCII code/tile number
PrintYellowDigit:
    out (VDPDataPort), a            ; output ASCII code = tile number as first tilemap byte
    push af
    pop af                          ; wait a bit...
    ld a, $08
    out (VDPDataPort), a            ; output $8 as second byte => use second palette
    ret                             ; (since the font data uses color 9, it appears in yellow then)

InitRoundAndTimeValues:
.db $01                                 ; initial round number (DCB)
.db $01                                 ; initial round number (binary)
.db $00                                 ; initial value for VBlank counter for time update
.db $61                                 ; time left (DCB) (really 60 secs as it is decremented on the
                                        ; first call to UpdateSnailMazeGameTime)

TimeUpString:
.db "TIME UP"

RoundTimeString:
.db "RD    TIME"

RoundTimesTable:    ; DCB encoding!
.db $30, $35, $25, $35, $35, $30, $35, $35, $35, $10, $15

EntityUpdateFuncsTable:
.dw UpdateSnail
.dw UpdateGoal
.dw UpdateStartPoint

UpdateSnail:
    bit 7, (ix + Entity.flag)       ; for the snail, this bit is set if the snail is moving
    jr z, SnailMoveFlagNotSet
    dec (ix + Entity.controllerChkCtr)    ; decrement this counter each frame, if not zero we keep moving
    jr z, ResetSnailMovement        ; in the current direction, else it's time to re-check the controller
    ld hl, (Snail.animDataPointer)
    jp UpdateEntityAnimation
ResetSnailMovement:
    res 7, (ix + Entity.flag)       ; reset snail move flag, then
    ld hl, $0000
    ld (Snail.xChange), hl          ; reset both X and Y movement values to 0
SnailMoveFlagNotSet:
    call GetSnailTileMapEntryPtr    ; HL points to RAM tilemap entry containing snail's position
    ld a, (ControllerState)         ; read input state controller 1
    rrca
    call c, UpPressedHandler
    rrca
    call c, DownPressedHandler
    rrca
    jr c, LeftPressedHandler
    rrca
    ret nc                          ; return if we have not pressed any direction on controller

RightPressedHandler:
    bit 1, (hl)                     ; return if there is a wall to the right of our maze tile
    ret nz
    ld (ix + Entity.xChange), $01   ; else we can move 1 pixel in the right direction
    ld hl, AnimDataSnailRight
    jr SnailMoved

UpPressedHandler:
    push hl
    ld de, $ffc0                    ; = -$40 = offset to next row up in maze tilemap
    add hl, de
    bit 0, (hl)                     ; return if there is a wall at the bottom of the maze tile above
    pop hl
    ret nz
    ld (ix + Entity.yChange), $-1   ; else we can move 1 pixel in the up direction
    ld hl, AnimDataSnailUp
    jr PopUpDownHandlerRetAddr

DownPressedHandler:
    bit 0, (hl)                     ; return if there is a wall at the bottom of our maze tile
    ret nz
    ld (ix + Entity.yChange), $01   ; else we can move 1 pixel in the right direction
    ld hl, AnimDataSnailDown
PopUpDownHandlerRetAddr:            ; pop return address of Up/DownPressedHandler, so that ret
    pop af                          ; returns to caller of UpdateSnail instead of rrca statement above

SnailMoved:
    ld (Snail.animDataPointer), hl  ; set address of anim data to play depending on snsil's direction
    ld (ix + Entity.controllerChkCtr), $08    ; keep moving for 8 frames, don't re-check the controller until then
    set 7, (ix + Entity.flag)       ; set snail moving flag
    jp UpdateEntityAnimation

LeftPressedHandler:
    dec hl
    dec hl
    bit 1, (hl)                     ; return if there is a wall to the right of the maze tile on the left
    ret nz
    ld (ix + Entity.xChange), $-1   ; else we can move 1 pixel in the left direction
    ld hl, AnimDataSnailLeft
    jr SnailMoved

GetSnailTileMapEntryPtr:
    ld a, (Snail.x)                 ; A = snail's X position
    rrca                            ; a tile is 8x8 pixels, and a tilemap entry is 2 byte, so the
    rrca                            ; offset in the tilemap row is (X/8)*2, i.e shift A 3 bits right,
    and $3e                         ; then 1 bit left, which is the same as rotating A 2 bits right,
    ld e, a                         ; then force bits 0, 6 & 7 to zero, which is what we do here
    ld a, (Snail.y)                 ; A = snail's Y position
    and $f8
    ld l, a
    ld h, $00                       ; => HL = Y position rounded down to multiple of 8
    add hl, hl
    add hl, hl                      ; HL = 8 * HL = offset in the maze tilemap of the
    add hl, hl                      ; first entry in the tilemap row that contains snail's Y position
    ld d, >MazeTilemap-2            ; DE = base address RAM tilemap + offset in the tilemap row
    add hl, de                      ; so HL = tilemap entry that contains the snail's position!
    ret

UpdateGoal:
    bit 0, (ix + Entity.flag)
    jr z, GoalFirstUpdate           ; jump if first update since entity struct reset
    ld hl, (Snail.x)                ; HL = X,Y coordinates of snail's sprite
    ld a, l
    cp (ix + Entity.x)              ; compare X snail with X goal
    jr nz, GoalNotReached
    ld a, h
    cp (ix + Entity.y)              ; compare Y snail with Y goal
    jr nz, GoalNotReached
    ld (GoalReachedFlag), a         ; goal reached! Set flag to snail's Y
    ret
GoalNotReached:
    ld hl, AnimDataGoal
    jp UpdateEntityAnimation
GoalFirstUpdate:
    ld (ix + Entity.pattern), $0c   ; set pattern index: bottom point of GOAL arrow

FinishEntityInit:
    set 0, (ix + Entity.flag)       ; set "initialized" flag
    ld (ix + Entity.animVBlanksCtrRst), $1e    ; animation step will be incremented every 30 frames
    ret

UpdateStartPoint:
    bit 0, (ix + Entity.flag)
    jr z, StartFirstUpdate          ; jump if first update since entity struct reset
    ld hl, AnimDataStart
    jp UpdateEntityAnimation
StartFirstUpdate:
    ld hl, (Snail.x)                ; HL = X, Y coordinates of snail's sprite
    ld (StartPoint.x), hl           ; set Start's coordinates to the same values
    ld (ix + Entity.pattern), $53   ; set pattern index to "S" pattern
    jr FinishEntityInit

AnimDataGoal:
.db $02       ; 2 animation frames
.db $47, $0c  ; blink between "G" and arrow??

AnimDataStart:
.db $02       ; 2 animation frames
.db $20, $53  ; blink between "S" and " " tiles

AnimDataSnailRight:
.db $02       ; 2 animation frames
.db $0d, $0e  ; alternate between the two "snail going right" tiles

AnimDataSnailLeft:
.db $02       ; 2 animation frames
.db $0f, $10  ; alternate between the two "snail going left" tiles

AnimDataSnailDown:
.db $02       ; 2 animation frames
.db $11, $12  ; alternate between the two "snail going down" tiles

AnimDataSnailUp:
.db $02       ; 2 animation frames
.db $13, $14  ; alternate between the two "snail going up" tiles

UpdateEntities:
    ld hl, SpriteRAM
    ld (SpriteRamPointer), hl       ; reset sprite RAM pointer to start of sprite RAM
    ld ix, Snail                    ; IX points to first entity (snail)
    ld b, $04                       ; shouldn't it be 3? bug?
EntitiesLoop:
    ld a, (ix + Entity.entityId)    ; read entity id byte
    and $7f                         ; ignore bit 7
    jp z, NextEntity                ; if id = 0, not a valid entity, go to next one
    push bc
    ld hl, EntityUpdateFuncsTable-2 ; else call the appropriate entity update code
    call UpdateEntity               ; based on entity id
    ld a, (ix + Entity.entityId)    ; check entity id byte
    or a                            ; if id = 0, not a valid entity, go to next one
    jp z, NextEntity-1
    call UpdateEntityX
    call UpdateEntityY
    call UpdateEntitySpriteData
    pop bc
NextEntity:
    ld de, $0020
    add ix, de                      ; IX points to next entity
    djnz EntitiesLoop
    ld hl, (SpriteRamPointer)       ; write Y=$d0 for 1st sprite after the active entities' sprites
    ld (hl), $d0                    ; so that this sprite and all remaining ones are not drawn
    ret

UpdateEntitySpriteData:
    ld a, (ix + Entity.entityId)
    or a
    ret z                           ; if entity id = 0 (invalid), return
    ld a, (ix + Entity.y)
    cp $c0
    ret nc                          ; if entity's Y position >= 192, return
    ld hl, (SpriteRamPointer)       ; HL = current pointer in sprite RAM
    ld (hl), a                      ; set sprite's Y coordinate to entity's Y position
    inc hl
    ex af, af'
    ld a, (ix + $0c)                ; A = entity's pattern index
    cp $0c
    jr z, WriteGoalSpritesData      ; if pattern index is $0c (bottom point GOAL arrow), jump
    ld (SpriteRamPointer), hl       ; increment current pointer in sprite RAM
    dec hl
    sla l
    set 7, l                        ; L = $80  + 2 * L => HL points to X coord of sprite in sprite RAM
    ld a, (ix + Entity.x)           ; set it to entity's X position
    ld (hl), a
    inc hl                          ; HL points to tile pattern index
    ld a, (ix + Entity.pattern)     ; write entity's pattern index
    ld (hl), a
    ret
WriteGoalSpritesData:
    ex af, af'                      ; A = Y coord. of the sprite bottom point of GOAL arrow
    ld b, $04
    push hl                         ; for the next 4 sprites composing the arrow:
WriteGoalSpritesYs:
    sub $08                         ; Y = Y - 8
    ld (hl), a                      ; sprite Y = Y
    inc hl                          ; advance sprite RAM pointer
    djnz WriteGoalSpritesYs
    ld (SpriteRamPointer), hl
    pop hl
    dec hl
    sla l
    set 7, l                        ; L = $80  + 2 * L => HL points to X coord of the sprite
    ld b, $05                       ; for the bottom point of the GOAL arrow
    ld c, (ix + Entity.x)           ; X for sprites of GOAL arrow
    ex af, af'                      ; A = pattern index for bottom point of GOAL arrow
WriteGoalSpritesXsAndPats:
    ld (hl), c                      ; for the 5 sprites composing the arrow, write the same X
    inc hl                          ; and decreasing pattern indices
    ld (hl), a
    inc hl
    dec a
    djnz WriteGoalSpritesXsAndPats
    ret

UpdateEntityY:
    ld a, (ix + Entity.yChange)     ; Y move value = 1 if moving down, -1 if moving up
    add a, (ix + Entity.y)          ; entity's Y position = Y position + Y move value
    ld (ix + Entity.y), a
    ret

ResetEntityStruct:                  ; sets all the bytes of the entity struct to 0
    xor a                           ; except for the animation VBlank counter set to 1
    push ix
    pop hl
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), $01
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    inc l
    ld (hl), a
    ld c, a
    ret

UpdateEntityX:
    ld a, (MazeDebugVar)            ; ?? debug variable? never written in this BIOS
    ld d, (ix + Entity.xChange)     ; X move value = 1 if moving right, -1 if moving left
    add a, d
    ld d, a
    add a, (ix + Entity.x)          ; entity's X position = X position + X move value
    ld (ix + Entity.x), a
    bit 7, d
    jr nz, CheckEntityXPositive     ; if moving left, make sure new X position >= 0
    jr c, ResetEntityStruct         ; else if new X pos >= 256, reset entity
    ret
CheckEntityXPositive:
    jr nc, ResetEntityStruct        ; if new X pos < 0, reset entity
    ret


; The animated sprites data format for the 3 entities (snail, start, goal) is very simple
; First byte is indicating how many animation steps there are in total (always 2 in this game)
; Each of the following byte is the pattern index of the tile to show for this step

UpdateEntityAnimation:
    ld b, (hl)                              ; B = 1st byte of anim. data = how many anim. steps
    inc hl                                  ; HL now points to first animation step
    ld d, b
    dec (ix + Entity.animVBlanksCounter)    ; decrement VBlanks counter
    ld a, (ix + Entity.animStep)            ; A = current animation step
    jr nz, WriteAnimPatternIndex            ; if VBlank counter didn't reach zero, stay on this step
    ld e, (ix + Entity.animVBlanksCtrRst)
    ld (ix + Entity.animVBlanksCounter), e  ; else reload the counter with reset value
    inc a                                   ; increment animation step
    cp d                                    ; wrap around: if step >= number of steps for this anim,
    jr c, WriteAnimationStep                ; then set step = 0
    xor a
WriteAnimationStep:
    ld (ix + Entity.animStep), a    ; write new animation step value in entity's struct
WriteAnimPatternIndex:
    ld c, a
    ld b, $00
    add hl, bc                      ; use frame number as index into pattern indices' array
    ld a, (hl)                      ; read pattern index for this step
    ld (ix + Entity.pattern), a     ; and write it in the entity's struct
    ret

DeadCode:                               ; bug? leftover from earlier version of UpdateEntityAnimation?
    ld b, (hl)
    inc hl
    ld d, b
    ld a, (ix + Entity.animStep)
    jr WriteAnimPatternIndex
    and $0f

UpdateEntity:                           ; HL = routine addresses' table, A = entity id
    add a, a
    ld e, a
    ld d, $00                       ; DE = 2*A = offset in the table of (16-bit) addresses
    add hl, de
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a                         ; HL = address of entity update code to call
    jp (hl)                         ; jump there

;-----------------------------------
; SNAIL MAZE GAME'S CODE ENDS HERE
;-----------------------------------

SegaLogoTileMap:
.db $76, $77, $78, $79, $7a, $7b, $7c, $7d, $7e, $7f
.db $80, $81, $82, $83, $84, $85, $86, $87, $88, $89
.db $8a, $8b, $8c, $8d, $8e, $8f, $90, $91, $92, $93
.db $94, $95, $96, $94, $97, $98, $94, $94, $94, $99

MasterSystemTileMap:
.db $9a, $9b, $9c, $9d, $9e, $9f, $a0, $a1, $a2, $a3, $a4, $00, $a5, $a6, $a7, $a8, $9f, $a0, $a1, $a2, $a9, $aa, $ab
.db $ac, $ad, $ae, $af, $b0, $b1, $b2, $b3, $b4, $b5, $b6, $00, $b7, $b8, $b9, $b0, $b1, $b2, $b3, $b4, $ba, $bb, $bc
.db $bd, $be, $bf, $c0, $c1, $c2, $c3, $c4, $c5, $c6, $c7, $c8, $c9, $ca, $cb, $cc, $c2, $c3, $c4, $c5, $cd, $ce, $cf

; Patterns 118-153 = SEGA logo. 154-207 = MASTER SYSTEM
RLEPatternDataSEGA:
.db $89, $07, $1c, $30, $60, $41, $c6, $84, $88 ; plane 0 pat. 118
.db $ff, $03, $00, $81, $ff, $03, $00           ; plane 0 pat. 119
.db $89, $e1, $27, $2c, $38, $f0, $31, $21, $22 ; plane 0 pat. 120
.db $ff, $03, $00, $95, $7f, $80, $00, $00      ; plane 0 pat. 121
.db $f8, $09, $0b, $0e, $fc, $0c, $08, $08      ; plane 0 pat. 122
.db $7f, $c0, $00, $00, $1f, $60, $40, $80      ; plane 0 pat. 123
.db $fe, $03, $02, $81, $fe, $03, $02           ; plane 0 pat. 124
.db $a8, $03, $0e, $08, $18, $11, $31, $22, $22 ; plane 0 pat. 125
.db $e0, $39, $0f, $0e, $c6, $46, $22, $23      ; plane 0 pat. 126
.db $fc, $86, $7b, $8d, $b5, $8d, $b5, $7b      ; plane 0 pat. 127
.db $88, $88, $84, $c6, $41, $60, $30, $1c      ; plane 0 pat. 128
.db $ff, $07, $01, $00, $f0, $0c, $04, $02      ; plane 0 pat. 129
.db $04, $e2, $85, $63, $62, $22, $22           ; plane 0 pat. 130
.db $3f, $03, $00, $81, $ff, $03, $00           ; plane 0 pat. 131
.db $81, $f8, $03, $18, $81, $f8, $03, $18      ; plane 0 pat. 132
.db $81, $8f, $03, $88, $81, $8f, $03, $88      ; plane 0 pat. 133
.db $81, $fe, $03, $02, $88, $e2, $22, $23, $23 ; plane 0 pat. 134
.db $62, $44, $44, $c4, $03, $88, $82, $11      ; plane 0 pat. 135
.db $23, $03, $11, $03, $88, $84, $c4           ; plane 0 pat. 136
.db $86, $fc, $00, $03, $80, $83, $c0, $40      ; plane 0 pat. 137
.db $ff, $03, $80, $81, $ff, $03, $80           ; plane 0 pat. 138
.db $95, $e2, $02, $04, $0c, $f0, $00, $01, $07 ; plane 0 pat. 139
.db $22, $22, $21, $71, $70, $d8, $8c, $07      ; plane 0 pat. 140
.db $3f, $00, $00, $80, $7f, $03, $00           ; plane 0 pat. 141
.db $90, $f8, $08, $08, $0c, $fc, $0e, $0b, $09 ; plane 0 pat. 142
.db $8e, $80, $40, $60, $1f, $00, $00, $c0      ; plane 0 pat. 143
.db $02, $23, $02, $22, $81, $e2, $03, $00      ; plane 0 pat. 144
.db $85, $11, $10, $22, $22, $23, $03, $46      ; plane 0 pat. 145
.db $85, $c4, $04, $02, $02, $fe, $03, $00      ; plane 0 pat. 146
.db $85, $40, $60, $20, $20, $30, $03, $10      ; plane 0 pat. 147
.db $81, $ff, $07, $00                          ; plane 0 pat. 148
.db $81, $fc, $07, $00                          ; plane 0 pat. 149
.db $81, $01, $07, $00                          ; plane 0 pat. 150
.db $81, $f8, $07, $00                          ; plane 0 pat. 151
.db $81, $7f, $07, $00                          ; plane 0 pat. 152
.db $81, $f0, $07, $00                          ; plane 0 pat. 153
.db $81, $7c, $04, $3c, $02, $3e, $82, $36      ; plane 0 pat. 154
.db $1f, $04, $1e, $02, $3e, $81, $2e           ; plane 0 pat. 155
.db $03, $01, $03, $03, $02, $06                ; plane 0 pat. 156
.db $03, $e0, $02, $70, $96, $78, $38, $38      ; plane 0 pat. 157
.db $03, $0f, $1c, $18, $38, $38, $3c, $3f      ; plane 0 pat. 158
.db $90, $f1, $31, $1b, $0a, $08, $00, $80      ; plane 0 pat. 159
.db $ff, $ff, $dd, $05, $1c                     ; plane 0 pat. 160
.db $85, $87, $c3, $c3, $63, $23, $03, $03      ; plane 0 pat. 161
.db $02, $ff, $82, $83, $81, $02, $80, $02, $84 ; plane 0 pat. 162
.db $82, $1f, $0f, $03, $8e, $03, $0e           ; plane 0 pat. 163
.db $84, $f0, $f8, $3c, $0c, $03, $0e, $8d, $0c ; plane 0 pat. 164
.db $07, $1f, $38, $30, $70, $70, $78, $7f      ; plane 0 pat. 165
.db $27, $e3, $61, $31, $02, $10, $02, $00      ; plane 0 pat. 166
.db $82, $c3, $81, $02, $c3, $02, $e6, $02, $7c ; plane 0 pat. 167
.db $8a, $c3, $8f, $1c, $18, $38, $38, $3c, $3f ; plane 0 pat. 168
.db $1f, $0f, $03, $8f, $02, $0f, $81, $0d      ; plane 0 pat. 169
.db $05, $07, $02, $8f, $82, $8b                ; plane 0 pat. 170
.db $c0, $07, $80                               ; plane 0 pat. 171
.db $81, $36, $02, $37, $04, $33, $85, $31      ; plane 0 pat. 172
.db $2e, $6e, $6e, $4e, $03, $ce, $9d, $8e      ; plane 0 pat. 173
.db $06, $06, $0c, $0f, $0f, $0c, $18, $18      ; plane 0 pat. 174
.db $38, $3c, $1c, $fc, $fc, $1e, $0e, $0e      ; plane 0 pat. 175
.db $1f, $0f, $03, $00, $20, $20, $30, $18      ; plane 0 pat. 176
.db $e0, $f0, $f0, $78, $03, $38, $81, $70      ; plane 0 pat. 177
.db $08, $1c                                    ; plane 0 pat. 178
.db $08, $03                                    ; plane 0 pat. 179
.db $02, $fc, $02, $84, $02, $80, $83, $81, $83 ; plane 0 pat. 180
.db $0e, $02, $0f, $02, $0e, $86, $ce, $8e, $8e ; plane 0 pat. 181
.db $3c, $f8, $f0, $02, $38, $02, $1c, $8d, $0e ; plane 0 pat. 182
.db $3f, $1f, $07, $00, $40, $40, $60, $30      ; plane 0 pat. 183
.db $c0, $e0, $e0, $f0, $03, $70, $81, $e0      ; plane 0 pat. 184
.db $08, $38                                    ; plane 0 pat. 185
.db $03, $0d, $02, $0c, $87, $cc, $8c, $8c      ; plane 0 pat. 186
.db $8b, $db, $db, $d3, $03, $f3, $81, $63      ; plane 0 pat. 187
.db $08, $80                                    ; plane 0 pat. 188
.db $82, $31, $79, $06, $00                     ; plane 0 pat. 189
.db $82, $8e, $9f, $06, $00                     ; plane 0 pat. 190
.db $82, $18, $3c, $06, $00                     ; plane 0 pat. 191
.db $82, $0e, $1f, $06, $00                     ; plane 0 pat. 192
.db $82, $1f, $13, $06, $00                     ; plane 0 pat. 193
.db $82, $e0, $c0, $06, $00                     ; plane 0 pat. 194
.db $82, $1c, $3e, $06, $00                     ; plane 0 pat. 195
.db $82, $03, $07, $06, $00                     ; plane 0 pat. 196
.db $02, $ff, $06, $00                          ; plane 0 pat. 197
.db $82, $0e, $1f, $06, $00                     ; plane 0 pat. 198
.db $82, $0e, $07, $0e, $00                     ; plane 0 pat. 199
.db $82, $3f, $27, $06, $00                     ; plane 0 pat. 201
.db $82, $c0, $80, $06, $00                     ; plane 0 pat. 202
.db $82, $38, $fe, $06, $00                     ; plane 0 pat. 203
.db $82, $1f, $13, $06, $00                     ; plane 0 pat. 204
.db $82, $0c, $1e, $06, $00                     ; plane 0 pat. 205
.db $82, $63, $67, $06, $00                     ; plane 0 pat. 206
.db $82, $80, $c0, $06, $00, $00                ; plane 0 pat. 207
.db $89, $07, $1c, $30, $60, $41, $c6, $84, $88 ; plane 1 pat. 118
.db $ff, $03, $00, $81, $ff, $03, $00           ; plane 1 pat. 119
.db $89, $e1, $27, $2c, $38, $f0, $31, $21, $22 ; plane 1 pat. 120
.db $ff, $03, $00, $95, $7f, $80, $00, $00      ; plane 1 pat. 121
.db $f8, $09, $0b, $0e, $fc, $0c, $08, $08      ; plane 1 pat. 122
.db $7f, $c0, $00, $00, $1f, $60, $40, $80      ; plane 1 pat. 123
.db $fe, $03, $02, $81, $fe, $03, $02           ; plane 1 pat. 124
.db $a8, $03, $0e, $08, $18, $11, $31, $22, $22 ; plane 1 pat. 125
.db $e0, $39, $0f, $0e, $c6, $46, $22, $23      ; plane 1 pat. 126
.db $fc, $86, $7b, $8d, $b5, $8d, $b5, $7b      ; plane 1 pat. 127
.db $88, $88, $84, $c6, $41, $60, $30, $1c      ; plane 1 pat. 128
.db $ff, $07, $01, $00, $f0, $0c, $04, $02      ; plane 1 pat. 129
.db $04, $e2, $85, $63, $62, $22, $22           ; plane 1 pat. 130
.db $3f, $03, $00, $81, $ff, $03, $00           ; plane 1 pat. 131
.db $81, $f8, $03, $18, $81, $f8, $03, $18      ; plane 1 pat. 132
.db $81, $8f, $03, $88, $81, $8f, $03, $88      ; plane 1 pat. 133
.db $81, $fe, $03, $02, $88, $e2, $22, $23, $23 ; plane 1 pat. 134
.db $62, $44, $44, $c4, $03, $88, $82, $11      ; plane 1 pat. 135
.db $23, $03, $11, $03, $88, $84, $c4           ; plane 1 pat. 136
.db $86, $fc, $00, $03, $80, $83, $c0, $40      ; plane 1 pat. 137
.db $ff, $03, $80, $81, $ff, $03, $80           ; plane 1 pat. 138
.db $95, $e2, $02, $04, $0c, $f0, $00, $01, $07 ; plane 1 pat. 139
.db $22, $22, $21, $71, $70, $d8, $8c, $07      ; plane 1 pat. 140
.db $3f, $00, $00, $80, $7f, $03, $00           ; plane 1 pat. 141
.db $90, $f8, $08, $08, $0c, $fc, $0e, $0b, $09 ; plane 1 pat. 142
.db $8e, $80, $40, $60, $1f, $00, $00, $c0      ; plane 1 pat. 143
.db $02, $23, $02, $22, $81, $e2, $03, $00      ; plane 1 pat. 144
.db $85, $11, $10, $22, $22, $23, $03, $46      ; plane 1 pat. 145
.db $85, $c4, $04, $02, $02, $fe, $03, $00      ; plane 1 pat. 146
.db $85, $40, $60, $20, $20, $30, $03, $10      ; plane 1 pat. 147
.db $81, $ff, $07, $00                          ; plane 1 pat. 148
.db $81, $fc, $07, $00                          ; plane 1 pat. 149
.db $81, $01, $07, $00                          ; plane 1 pat. 150
.db $81, $f8, $07, $00                          ; plane 1 pat. 151
.db $81, $7f, $07, $00                          ; plane 1 pat. 152
.db $81, $f0, $07, $00                          ; plane 1 pat. 153
.db $97, $80, $40, $00, $00, $02, $00, $00, $08 ; plane 1 pat. 154
.db $00, $01, $00, $00, $20, $00, $00, $10      ; plane 1 pat. 155
.db $80, $00, $02, $00, $00, $04, $01, $03, $00 ; plane 1 pat. 156 + 2 zero lines for pat. 157
.db $94, $10, $80, $08, $00, $40, $00           ; plane 1 pat. 157 (6 lines)
.db $04, $10, $02, $24, $04, $04, $02, $00      ; plane 1 pat. 158
.db $41, $00, $4a, $20, $11, $10, $04, $00      ; plane 1 pat. 159 + 2 zero lines for pat. 160
.db $82, $22, $80, $04, $00                     ; plane 1 pat. 160 (6 lines)
.db $85, $48, $04, $20, $80, $40, $05, $00      ; plane 1 pat. 161 + 2 zero lines for pat. 162
.db $88, $04, $02, $01, $00, $00, $08           ; plane 1 pat. 162 (6 lines)
.db $20, $90, $06, $00                          ; plane 1 pat. 163
.db $aa, $08, $04, $40, $12, $10, $00, $10, $12 ; plane 1 pat. 164
.db $08, $20, $04, $48, $08, $08, $04, $00      ; plane 1 pat. 165
.db $88, $04, $92, $40, $21, $20, $00, $00      ; plane 1 pat. 166
.db $24, $42, $00, $24, $01, $18, $82, $00      ; plane 1 pat. 167
.db $24, $50, $82, $24, $04, $04, $02, $00      ; plane 1 pat. 168
.db $20, $90, $05, $00, $81, $02                ; plane 1 pat. 169
.db $04, $00, $86, $88, $00, $00, $04           ; plane 1 pat. 170
.db $20, $40, $06, $00                          ; plane 1 pat. 171
.db $84, $01, $00, $00, $04, $03, $00, $85, $02 ; plane 1 pat. 172
.db $40, $00, $00, $a0, $03, $00, $90, $40      ; plane 1 pat. 173
.db $00, $08, $02, $00, $00, $10, $04, $00      ; plane 1 pat. 174
.db $04, $00, $20, $00, $02, $00, $10, $05, $00 ; plane 1 pat. 175 + 4 zero lines for pat. 176
.db $02, $10, $86, $08, $20                     ; plane 1 pat. 176 (4 lines)
.db $00, $00, $08, $80, $03, $40, $81, $88      ; plane 1 pat. 177
.db $12, $00                                    ; plane 1 pat. 178 + 179 (all 0s) + 2 zero lines for pat. 179
.db $81, $08, $0b, $00                          ; plane 1 pat. 180 (6 lines) + 6 zero lines for pat. 180
.db $8a, $40, $00                               ; plane 1 pat. 181 (2 lines)
.db $40, $04, $08, $40, $44, $20, $02, $10      ; plane 1 pat. 182
.db $03, $00, $89, $01, $20, $20, $10, $41      ; plane 1 pat. 183
.db $00, $00, $10, $00, $03, $80, $82, $10      ; plane 1 pat. 184
.db $44, $0a, $00                               ; plane 1 pat. 185 + 3 zero lines for pat. 186
.db $89, $01, $00, $00, $40, $00                ; plane 1 pat. 186 (5 lines)
.db $50, $00, $00, $28, $03, $00, $81, $90      ; plane 1 pat. 187
.db $08, $00                                    ; plane 1 pat. 188
.db $82, $48, $84, $06, $00                     ; plane 1 pat. 189
.db $82, $11, $20, $06, $00                     ; plane 1 pat. 190
.db $82, $24, $c2, $06, $00                     ; plane 1 pat. 191
.db $82, $11, $20, $07, $00                     ; plane 1 pat. 192 + 1 zero line for pat. 193
.db $81, $84, $06, $00                          ; plane 1 pat. 193 (7 lines)
.db $82, $10, $20, $06, $00                     ; plane 1 pat. 194
.db $82, $22, $41, $06, $00                     ; plane 1 pat. 195
.db $82, $04, $08, $0e, $00                     ; plane 1 pat. 196 + 197 (all zeros)
.db $82, $91, $20, $06, $00                     ; plane 1 pat. 198
.db $82, $01, $88, $07, $00                     ; plane 1 pat. 199 + 1 zero line for pat. 200
.db $81, $80, $07, $00                          ; plane 1 pat. 200 (7 lines) + 1 zero line for pat. 201
.db $81, $08, $06, $00                          ; plane 1 pat. 201 (7 lines)
.db $82, $20, $40, $06, $00                     ; plane 1 pat. 202
.db $81, $44, $08, $00                          ; plane 1 pat. 203 + 1 zero line for pat. 204
.db $81, $04, $06, $00                          ; plane 1 pat. 204 (7 lines)
.db $82, $92, $21, $06, $00                     ; plane 1 pat. 205
.db $82, $04, $08, $06, $00                     ; plane 1 pat. 206
.db $82, $40, $20, $06, $00, $00                ; plane 1 pat. 207
.db $89, $00, $03, $0f, $1f, $3e, $39, $7b, $77 ; plane 2 pat. 118
.db $00, $03, $ff, $81, $00, $03, $ff           ; plane 2 pat. 119
.db $89, $00, $c0, $c3, $c7, $0f, $ce, $de, $dd ; plane 2 pat. 120
.db $00, $03, $ff, $95, $80, $7f, $ff, $ff      ; plane 2 pat. 121
.db $00, $f0, $f0, $f1, $03, $f3, $f7, $f7      ; plane 2 pat. 122
.db $00, $3f, $ff, $ff, $e0, $9f, $bf, $7f      ; plane 2 pat. 123
.db $00, $03, $fc, $81, $00, $03, $fc           ; plane 2 pat. 124
.db $82, $00, $01, $02, $07, $02, $0e, $02, $1d ; plane 2 pat. 125
.db $a0, $00, $c0, $f0, $f1, $39, $b9, $dd, $dc ; plane 2 pat. 126
.db $00, $78, $84, $72, $4a, $72, $4a, $84      ; plane 2 pat. 127
.db $77, $77, $7b, $39, $3e, $1f, $0f, $03      ; plane 2 pat. 128
.db $00, $f8, $fe, $ff, $0f, $f3, $fb, $fd      ; plane 2 pat. 129
.db $04, $1d, $85, $9c, $9d, $dd, $dd           ; plane 2 pat. 130
.db $c0, $03, $ff, $81, $00, $03, $ff           ; plane 2 pat. 131
.db $81, $07, $03, $e7, $81, $07, $03, $e7      ; plane 2 pat. 132
.db $81, $70, $03, $77, $81, $70, $03, $77      ; plane 2 pat. 133
.db $81, $00, $03, $fc, $81, $1c, $03, $dc      ; plane 2 pat. 134
.db $81, $1d, $03, $3b, $03, $77, $82, $ee      ; plane 2 pat. 135
.db $dc, $03, $ee, $03, $77, $82, $3b           ; plane 2 pat. 136
.db $78, $06, $00, $82, $80                     ; plane 2 pat. 137
.db $00, $03, $7f, $81, $00, $03, $7f           ; plane 2 pat. 138
.db $95, $1d, $fd, $fb, $f3, $0f, $ff, $fe, $f8 ; plane 2 pat. 139
.db $dd, $dd, $de, $8e, $8f, $07, $03, $00      ; plane 2 pat. 140
.db $c0, $ff, $ff, $7f, $80, $03, $ff           ; plane 2 pat. 141
.db $90, $07, $f7, $f7, $f3, $03, $f1, $f0, $f0 ; plane 2 pat. 142
.db $71, $7f, $bf, $9f, $e0, $ff, $ff, $3f      ; plane 2 pat. 143
.db $02, $dc, $02, $dd, $81, $1d, $03, $ff      ; plane 2 pat. 144
.db $85, $ee, $ef, $dd, $dd, $dc, $03, $b9      ; plane 2 pat. 145
.db $85, $3b, $fb, $fd, $fd, $01, $03, $ff      ; plane 2 pat. 146
.db $02, $80, $03, $c0, $03, $e0                ; plane 2 pat. 147
.db $7f, $00, $7f, $00, $7f, $00, $63, $00, $00 ; plane 2 pat. 148-207 all zeros
.db $7f, $00, $7f, $00, $7f, $00, $7f, $00      ; plane 3 pat. 118-207 all zeros
.db $7f, $00, $55, $00, $00

; Patterns 3-20: Snail Maze Game (3-7: walls, 8-12: Goal arrow, 13-20: snail, 
; snail has 4 directions, 2 frames animation/direction: 13-14: right, 15-16: left, 17-18: down, 19-20 : up)
RLEPatternDataSnailMaze:
.db $08, $00                                    ; plane 0 pat. 3
.db $07, $ff, $81, $fe                          ; plane 0 pat. 4
.db $07, $ff, $81, $00                          ; plane 0 pat. 5
.db $0f, $fe                                    ; plane 0 pat. 6 + pat. 7 (7 lines)
.db $8f, $00                                    ; plane 0 pat. 7 (1 line)
.db $81, $c3, $e7, $ff, $ff, $c1, $81, $91      ; plane 0 pat. 8
.db $91, $99, $81, $c3, $c3, $81, $03, $99      ; plane 0 pat. 9 + pat. 10 (1 line)
.db $85, $81, $c3, $c3, $81, $99, $02, $81      ; plane 0 pat. 10 (7 lines)
.db $02, $99, $04, $8f, $02, $81                ; plane 0 pat. 11
.db $84, $ff, $7e, $3c, $18, $04, $00           ; plane 0 pat. 12
.db $81, $78, $04, $fc, $84, $78, $2a, $00      ; plane 0 pat. 13
.db $78, $04, $fc, $84, $78, $54, $00           ; plane 0 pat. 14
.db $1e, $04, $3f, $84, $1e, $54, $00           ; plane 0 pat. 15
.db $1e, $04, $3f, $a3, $1e, $2a, $00           ; plane 0 pat. 16
.db $1e, $7f, $3f, $7f, $3f, $5e, $00, $00      ; plane 0 pat. 17
.db $1e, $3f, $7f, $3f, $7f, $1e, $40, $00      ; plane 0 pat. 18
.db $00, $02, $78, $fe, $fc, $fe, $fc, $78      ; plane 0 pat. 19
.db $00, $00, $7a, $fc, $fe, $fc, $fe, $78, $00 ; plane 0 pat. 20
.db $07, $00, $81, $ff                          ; plane 1 pat. 3
.db $07, $00, $81, $01                          ; plane 1 pat. 4
.db $07, $00, $81, $ff                          ; plane 1 pat. 5
.db $0f, $01                                    ; plane 1 pat. 6 + pat.7 (7 lines)
.db $84, $ff                                    ; plane 1 pat. 7 (1 line)
.db $81, $c3, $e7, $1e, $ff                     ; plane 1 pat. 8 + pat. 9-11 + pat. 12 (1 line)
.db $83, $7e, $3c, $18, $04, $00                ; plane 1 pat. 12 (7 lines)
.db $c0, $78, $cc, $b5, $ac, $c4, $78, $00, $00 ; plane 1 pat. 13
.db $78, $cc, $b4, $ad, $c4, $78, $00, $00      ; plane 1 pat. 14
.db $1e, $33, $ad, $35, $23, $1e, $00, $00      ; plane 1 pat. 15
.db $1e, $33, $2d, $b5, $23, $1e, $00, $00      ; plane 1 pat. 16
.db $1e, $33, $2d, $25, $2b, $1e, $00, $08      ; plane 1 pat. 17
.db $1e, $33, $2d, $25, $2b, $1e, $00, $04      ; plane 1 pat. 18
.db $20, $00, $78, $d4, $a4, $b4, $cc, $78      ; plane 1 pat. 19
.db $10, $00, $78, $d4, $a4, $b4, $cc, $78, $00 ; plane 1 pat. 20
.db $07, $00, $21, $ff                          ; plane 2 pat. 3-7
.db $8e, $81, $c3, $e7, $ff, $ff, $c1, $bf, $f1 ; plane 2 pat. 8
.db $ff, $ff, $e7, $ff, $c3, $bd, $03, $ff      ; plane 2 pat. 9 + pat. 10 (1 line)
.db $86, $e7, $ff, $c3, $bd, $ff, $e7, $03, $ff ; plane 2 pat. 10 (7 lines) + pat. 11 (2 lines)
.db $81, $8f, $03, $ff, $86, $f1, $ff           ; plane 2 pat. 11 (6 lines)
.db $ff, $7e, $3c, $18, $05, $00                ; plane 2 pat. 12 + pat. 13 (1 line)
.db $bf, $30, $49, $51, $3b, $07, $fe, $00      ; plane 2 pat. 13 (7 lines)
.db $00, $30, $48, $51, $3b, $07, $fe, $00      ; plane 2 pat. 14
.db $00, $0c, $92, $8a, $dc, $e0, $7f, $00      ; plane 2 pat. 15
.db $00, $0c, $12, $8a, $dc, $e0, $7f, $00      ; plane 2 pat. 16
.db $40, $4c, $52, $5a, $54, $60, $70, $38      ; plane 2 pat. 17
.db $40, $4c, $52, $5a, $54, $60, $70, $3c      ; plane 2 pat. 18
.db $3c, $0e, $06, $2a, $5a, $4a, $32, $02      ; plane 2 pat. 19
.db $1c, $0e, $06, $2a, $5a, $4a, $32, $02, $00 ; plane 2 pat. 20
.db $7f, $00, $11, $00, $00                     ; plane 3 pat.3-20 all zeros

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

RLEPatternDataEnjoy:
.db $7f, $00, $21, $00, $08, $ff, $00           ; plane 0: 7f+21 = 160 lines = 20 pat. with 0, then all pixels on for 21st pat.
.db $7f, $00, $29, $00, $00                     ; 1933 plane 1: 168 lines = 21 pat. with 0
.db $7f, $00, $21, $00, $08, $ff, $00           ; 1938 plane 2: 20 pat. with 0, all pixels on for 21st pat.
.db $02, $ff, $05, $f0, $81, $ff                ; 193F plane 3 pat. 96  (pink, top left "E")
.db $02, $f8, $05, $00, $81, $e0                ; 1945 plane 3 pat. 97  (pink, top right "E")
.db $02, $f8, $03, $fc, $03, $f6                ; 194B plane 3 pat. 98  (pink, top left  "N")
.db $08, $78                                    ; 1951 plane 3 pat. 99  (pink, top right "N" / top "!")
.db $02, $1f, $06, $03                          ; 1953 plane 3 pat. 100 (pink, top left "J")
.db $82, $f1, $f3, $06, $c3                     ; 1957 plane 3 pat. 101 (pink, top right "J" + bit of top left "O")
.db $02, $ff, $06, $c0                          ; 195C plane 3 pat. 102 (pink, top left "O")
.db $81, $e7, $02, $f3, $02, $f1, $03, $f0      ; 1960 plane 3 pat. 103 (pink, top right "O" + bit of top left "Y")
.db $02, $80, $02, $c1, $02, $e3, $83, $f7, $7f ; 1968 plane 3 pat. 104 (pink, top left "Y")
.db $f0, $02, $e0, $02, $c0, $02, $80, $82, $00 ; 1971 plane 3 pat. 105 (pink, top right "Y")
.db $ff, $05, $f0, $02, $ff                     ; 197A plane 3 pat. 106 (pink, bottom left "E")
.db $81, $e0, $05, $00, $02, $f8                ; 197F plane 3 pat. 107 (pink, bottom right "E")
.db $03, $f3, $03, $f1, $02, $f0                ; 1985 plane 3 pat. 108 (pink, bottom left "N")
.db $02, $78, $81, $79, $04, $f9, $81, $f8      ; 198B plane 3 pat. 109 (pink, bottom right "N" + bit of bottom left "J")
.db $02, $03, $04, $e3, $02, $ff                ; 1993 plane 3 pat. 110 (pink, bottom left "J")
.db $07, $c3, $81, $81                          ; 1999 plane 3 pat. 111 (pink, bottom right "J" + bit of bottom left "O")
.db $06, $c0, $02, $ff                          ; 199d plane 3 pat. 112 (pink, bottom left "O")
.db $07, $f0, $82, $e0                          ; 19a1 plane 3 pat. 113 (pink, bottom right "O")
.db $7f, $07, $3e                               ; 19a5 plane 3 pat. 114 (pink, bottom "Y")
.db $04, $78, $02, $00, $02, $78                ; 19a8 plane 3 pat. 115 (pink, bottom "!")
.db $08, $00, $00                               ; 19ae plane 3 pat. 116 (square of solid blue)

SnailMaze1:
.db $b8, $60, $f0                               ; maze 1 snail's Y position, goal's Y position, goal's X position
.db $41, $03, $0d, $00, $0d, $c3, $00, $50      ; maze 1 row 1
.db $8d, $43, $78, $77, $79, $5d, $03, $ad      ; maze 1 row 2
.db $d1, $10, $50, $55, $64, $65, $40, $51      ; maze 1 row 3
.db $2d, $35, $24, $22, $81, $81, $43, $55      ; maze 1 row 4
.db $41, $05, $75, $fb, $40, $00, $52, $68      ; maze 1 row 5
.db $8d, $c5, $22, $2e, $cd, $0c, $14, $94      ; maze 1 row 6
.db $35, $d1, $43, $7b, $51, $01, $e1, $56      ; maze 1 row 7
.db $45, $63, $50, $6d, $18, $41, $8e, $5b      ; maze 1 row 8
.db $5d, $1e, $98, $10, $d4, $12, $34, $6e      ; maze 1 row 9
.db $55, $eb, $48, $47, $5b, $04, $74, $5b      ; maze 1 row 10
.db $85, $4d, $83, $b6, $2d, $d1, $58, $6d      ; maze 1 row 11
.db $35, $92, $0e, $d8, $42, $04, $2d, $92      ; maze 1 row 12
.db $45, $d7, $11, $20, $06, $41, $76, $63      ; maze 1 row 13
.db $8d, $61, $d4, $00, $d8, $36, $98, $5d      ; maze 1 row 14
.db $dd, $b4, $bb, $77, $a3, $77, $0f, $56      ; maze 1 row 15
.db $89, $20, $00, $22, $02, $22, $02, $a8      ; maze 1 row 16

SnailMaze2:
.db $a0, $58, $f0                               ; maze 2 snail's Y position, goal's Y position, goal's X position
.db $01, $4d, $37, $04, $1d, $30, $1e, $ad      ; maze 2 row 1
.db $4d, $11, $06, $75, $08, $4d, $7b, $57      ; maze 2 row 2
.db $51, $d4, $75, $61, $d3, $ba, $2e, $ae      ; maze 2 row 3
.db $ad, $83, $69, $4d, $2e, $d4, $75, $5b      ; maze 2 row 4
.db $41, $50, $1d, $d2, $78, $4b, $18, $56      ; maze 2 row 5
.db $0d, $6e, $08, $ad, $23, $be, $07, $55      ; maze 2 row 6
.db $35, $35, $34, $d2, $4e, $d8, $d2, $56      ; maze 2 row 7
.db $41, $c5, $06, $63, $3b, $11, $8d, $5b      ; maze 2 row 8
.db $4d, $b8, $7b, $bd, $48, $44, $01, $54      ; maze 2 row 9
.db $11, $d4, $6e, $d5, $8e, $55, $00, $55      ; maze 2 row 10
.db $ed, $2d, $b8, $18, $35, $d6, $d0, $56      ; maze 2 row 11
.db $61, $c8, $ed, $ed, $d6, $38, $15, $55      ; maze 2 row 12
.db $81, $4d, $51, $51, $63, $e0, $82, $59      ; maze 2 row 13
.db $75, $51, $6c, $60, $14, $53, $03, $ae      ; maze 2 row 14
.db $55, $23, $11, $10, $05, $56, $0d, $58      ; maze 2 row 15
.db $21, $02, $08, $08, $00, $88, $00, $a0      ; maze 2 row 16

SnailMaze3:
.db $88, $b8, $50                               ; maze 3 snail's Y position, goal's Y position, goal's X position
.db $7d, $37, $30, $80, $77, $77, $77, $53      ; maze 3 row 1
.db $15, $e2, $e0, $74, $55, $55, $61, $ad      ; maze 3 row 2
.db $d5, $5d, $83, $55, $55, $55, $b4, $51      ; maze 3 row 3
.db $55, $55, $40, $55, $6d, $e1, $d5, $55      ; maze 3 row 4
.db $55, $d5, $6d, $55, $35, $82, $58, $55      ; maze 3 row 5
.db $55, $55, $51, $55, $d9, $00, $54, $55      ; maze 3 row 6
.db $61, $55, $6d, $55, $14, $00, $85, $54      ; maze 3 row 7
.db $8d, $55, $51, $21, $d8, $00, $02, $94      ; maze 3 row 8
.db $31, $88, $63, $0d, $2e, $00, $35, $58      ; maze 3 row 9
.db $ed, $00, $5e, $01, $d5, $00, $06, $54      ; maze 3 row 10
.db $11, $00, $55, $73, $38, $00, $34, $58      ; maze 3 row 11
.db $6d, $03, $56, $55, $57, $03, $08, $55      ; maze 3 row 12
.db $51, $00, $88, $55, $55, $00, $34, $a2      ; maze 3 row 13
.db $2d, $0d, $00, $56, $45, $03, $0b, $50      ; maze 3 row 14
.db $75, $05, $0d, $21, $82, $8d, $0d, $a0      ; maze 3 row 15
.db $21, $02, $08, $08, $00, $02, $02, $a0      ; maze 3 row 16

SnailMaze4:
.db $a8, $58, $38                               ; maze 4 snail's Y position, goal's Y position, goal's X position
.db $dd, $c0, $02, $00, $00, $00, $31, $50      ; maze 4 row 1
.db $55, $83, $c3, $00, $02, $40, $04, $58      ; maze 4 row 2
.db $55, $d0, $5e, $03, $00, $10, $01, $50      ; maze 4 row 3
.db $55, $a3, $65, $cd, $00, $d5, $d0, $6d      ; maze 4 row 4
.db $85, $0c, $8e, $56, $03, $46, $8b, $98      ; maze 4 row 5
.db $d5, $36, $01, $88, $d0, $00, $0d, $54      ; maze 4 row 6
.db $15, $d8, $0d, $d0, $60, $83, $05, $51      ; maze 4 row 7
.db $d5, $6d, $35, $64, $53, $74, $b5, $54      ; maze 4 row 8
.db $55, $b5, $d5, $b5, $14, $55, $d5, $55      ; maze 4 row 9
.db $55, $d5, $56, $45, $45, $6d, $55, $55      ; maze 4 row 10
.db $55, $55, $5b, $81, $21, $16, $88, $55      ; maze 4 row 11
.db $45, $65, $55, $41, $0d, $08, $74, $55      ; maze 4 row 12
.db $71, $15, $52, $80, $f5, $00, $55, $55      ; maze 4 row 13
.db $6d, $48, $07, $00, $58, $43, $55, $57      ; maze 4 row 14
.db $85, $8f, $35, $00, $60, $8d, $55, $55      ; maze 4 row 15
.db $01, $02, $22, $00, $00, $0a, $20, $a2      ; maze 4 row 16

SnailMaze5:
.db $40, $b0, $f0                               ; maze 5 snail's Y position, goal's Y position, goal's X position
.db $31, $f4, $3d, $d0, $d0, $1e, $d1, $94      ; maze 5 row 1
.db $4d, $55, $d8, $45, $87, $7b, $2d, $59      ; maze 5 row 2
.db $b5, $61, $87, $11, $02, $12, $d2, $50      ; maze 5 row 3
.db $05, $1e, $36, $4e, $43, $e0, $84, $57      ; maze 5 row 4
.db $45, $0b, $0b, $c5, $1e, $44, $fb, $5a      ; maze 5 row 5
.db $41, $53, $80, $12, $45, $bb, $60, $97      ; maze 5 row 6
.db $8d, $ee, $d0, $d3, $52, $e0, $8d, $55      ; maze 5 row 7
.db $4d, $3b, $6e, $2e, $2d, $b4, $f4, $a2      ; maze 5 row 8
.db $85, $ee, $13, $78, $46, $db, $18, $5e      ; maze 5 row 9
.db $35, $85, $ed, $2d, $81, $63, $e4, $98      ; maze 5 row 10
.db $8d, $d1, $92, $10, $8d, $1d, $b4, $6d      ; maze 5 row 11
.db $51, $60, $e3, $4d, $d1, $06, $08, $91      ; maze 5 row 12
.db $6d, $93, $50, $b8, $87, $7b, $37, $56      ; maze 5 row 13
.db $7d, $45, $a4, $d7, $76, $ac, $45, $af      ; maze 5 row 14
.db $15, $12, $cd, $16, $15, $d1, $82, $55      ; maze 5 row 15
.db $09, $20, $88, $20, $82, $00, $20, $a2      ; maze 5 row 16

SnailMaze6:
.db $b0, $80, $68                               ; maze 6 snail's Y position, goal's Y position, goal's X position
.db $0d, $40, $0f, $0d, $11, $74, $03, $50      ; maze 6 row 1
.db $35, $5d, $75, $75, $35, $60, $01, $a0      ; maze 6 row 2
.db $55, $55, $55, $55, $75, $80, $34, $50      ; maze 6 row 3
.db $55, $55, $56, $55, $18, $d0, $40, $97      ; maze 6 row 4
.db $55, $15, $58, $18, $d0, $21, $50, $55      ; maze 6 row 5
.db $55, $08, $20, $00, $58, $dd, $54, $95      ; maze 6 row 6
.db $05, $00, $f4, $00, $54, $55, $65, $55      ; maze 6 row 7
.db $75, $f7, $55, $03, $55, $55, $8d, $55      ; maze 6 row 8
.db $21, $62, $55, $41, $55, $15, $75, $55      ; maze 6 row 9
.db $0d, $80, $55, $80, $b5, $48, $55, $95      ; maze 6 row 10
.db $05, $70, $15, $00, $06, $13, $5a, $65      ; maze 6 row 11
.db $01, $54, $05, $00, $41, $05, $6c, $5d      ; maze 6 row 12
.db $0d, $55, $01, $d0, $5e, $08, $78, $55      ; maze 6 row 13
.db $05, $66, $0d, $54, $1b, $00, $54, $55      ; maze 6 row 14
.db $71, $83, $01, $56, $ed, $00, $25, $5a      ; maze 6 row 15
.db $21, $02, $00, $20, $82, $00, $02, $a0      ; maze 6 row 16

SnailMaze7:
.db $78, $70, $78                               ; maze 7 snail's Y position, goal's Y position, goal's X position
.db $0d, $00, $38, $03, $00, $e0, $00, $50      ; maze 7 row 1
.db $35, $00, $44, $00, $00, $00, $34, $ad      ; maze 7 row 2
.db $d5, $00, $b8, $23, $00, $40, $5b, $51      ; maze 7 row 3
.db $15, $00, $44, $0d, $00, $b4, $54, $ad      ; maze 7 row 4
.db $55, $03, $b8, $35, $00, $45, $1b, $52      ; maze 7 row 5
.db $55, $00, $80, $14, $4d, $81, $54, $a3      ; maze 7 row 6
.db $55, $03, $00, $88, $b8, $1d, $18, $5e      ; maze 7 row 7
.db $6d, $0d, $37, $1c, $40, $08, $00, $55      ; maze 7 row 8
.db $b5, $75, $e1, $d8, $80, $0d, $00, $56      ; maze 7 row 9
.db $55, $55, $81, $d7, $00, $02, $00, $55      ; maze 7 row 10
.db $55, $55, $41, $55, $c0, $dd, $8d, $55      ; maze 7 row 11
.db $55, $61, $5d, $d5, $50, $55, $dd, $55      ; maze 7 row 12
.db $55, $80, $55, $59, $63, $55, $55, $58      ; maze 7 row 13
.db $15, $03, $4a, $5d, $50, $89, $55, $a3      ; maze 7 row 14
.db $45, $00, $50, $58, $63, $00, $88, $50      ; maze 7 row 15
.db $01, $00, $00, $88, $00, $20, $00, $a0      ; maze 7 row 16

SnailMaze8:
.db $78, $48, $e0                               ; maze 8 snail's Y position, goal's Y position, goal's X position
.db $dd, $00, $dd, $40, $34, $77, $03, $50      ; maze 8 row 1
.db $55, $43, $55, $03, $d6, $16, $8e, $a4      ; maze 8 row 2
.db $85, $5d, $54, $00, $55, $01, $00, $51      ; maze 8 row 3
.db $41, $15, $56, $03, $56, $d0, $40, $57      ; maze 8 row 4
.db $4d, $45, $88, $00, $18, $60, $53, $55      ; maze 8 row 5
.db $b5, $50, $03, $0c, $d4, $5d, $85, $55      ; maze 8 row 6
.db $05, $d5, $de, $e1, $55, $58, $01, $56      ; maze 8 row 7
.db $71, $15, $88, $5d, $95, $63, $00, $58      ; maze 8 row 8
.db $6d, $d5, $00, $56, $35, $de, $10, $94      ; maze 8 row 9
.db $51, $15, $00, $55, $05, $55, $e3, $55      ; maze 8 row 10
.db $6d, $d8, $8c, $56, $7b, $55, $50, $ad      ; maze 8 row 11
.db $15, $a0, $fb, $55, $60, $15, $54, $55      ; maze 8 row 12
.db $d5, $70, $5d, $88, $80, $d5, $58, $55      ; maze 8 row 13
.db $85, $63, $15, $0e, $00, $5a, $63, $58      ; maze 8 row 14
.db $75, $50, $35, $78, $77, $57, $80, $57      ; maze 8 row 15
.db $21, $a0, $08, $20, $22, $22, $00, $a2      ; maze 8 row 16

SnailMaze9:
.db $58, $b0, $e8                               ; maze 9 snail's Y position, goal's Y position, goal's X position
.db $0d, $34, $b0, $b7, $c7, $e0, $ec, $5d      ; maze 9 row 1
.db $35, $e1, $44, $d1, $12, $34, $51, $55      ; maze 9 row 2
.db $11, $10, $b5, $2e, $ed, $51, $84, $55      ; maze 9 row 3
.db $31, $04, $06, $75, $81, $6d, $cb, $96      ; maze 9 row 4
.db $1d, $d4, $51, $11, $1e, $b8, $10, $55      ; maze 9 row 5
.db $c5, $55, $60, $20, $fb, $40, $63, $98      ; maze 9 row 6
.db $b5, $2e, $b4, $47, $ad, $53, $51, $6f      ; maze 9 row 7
.db $45, $38, $d8, $62, $d1, $96, $20, $52      ; maze 9 row 8
.db $51, $04, $6d, $43, $24, $ed, $dd, $a0      ; maze 9 row 9
.db $15, $d0, $b6, $8d, $3b, $82, $1b, $50      ; maze 9 row 10
.db $05, $11, $d8, $42, $05, $41, $ed, $6d      ; maze 9 row 11
.db $4d, $04, $54, $d3, $be, $8d, $51, $51      ; maze 9 row 12
.db $1d, $38, $61, $2d, $d8, $d2, $6e, $94      ; maze 9 row 13
.db $05, $2d, $1e, $42, $87, $14, $82, $55      ; maze 9 row 14
.db $41, $40, $0b, $77, $35, $7b, $77, $59      ; maze 9 row 15
.db $01, $20, $02, $22, $82, $20, $22, $a8      ; maze 9 row 16

SnailMaze10:
.db $40, $40, $e8                               ; maze 10 snail's Y position, goal's Y position, goal's X position
.db $75, $d3, $04, $c3, $4e, $37, $0d, $92      ; maze 10 row 1
.db $21, $24, $dd, $2e, $11, $61, $38, $55      ; maze 10 row 2
.db $3d, $75, $15, $41, $44, $4e, $47, $58      ; maze 10 row 3
.db $59, $18, $d6, $be, $11, $34, $12, $94      ; maze 10 row 4
.db $2d, $ec, $d5, $d4, $ee, $49, $d1, $5d      ; maze 10 row 5
.db $01, $b8, $98, $1b, $b4, $5d, $84, $68      ; maze 10 row 6
.db $dd, $ed, $60, $d1, $48, $28, $0d, $50      ; maze 10 row 7
.db $85, $51, $83, $14, $5d, $43, $d0, $94      ; maze 10 row 8
.db $71, $a0, $dd, $d9, $b6, $10, $24, $61      ; maze 10 row 9
.db $2d, $3d, $86, $23, $45, $2f, $41, $57      ; maze 10 row 10
.db $71, $08, $41, $d1, $b8, $35, $1e, $56      ; maze 10 row 11
.db $2d, $51, $51, $20, $ed, $49, $4b, $5b      ; maze 10 row 12
.db $75, $54, $14, $70, $b6, $50, $50, $94      ; maze 10 row 13
.db $15, $d4, $d8, $22, $49, $27, $2d, $58      ; maze 10 row 14
.db $49, $1d, $6c, $77, $d0, $d1, $72, $57      ; maze 10 row 15
.db $81, $08, $02, $22, $80, $88, $20, $a2      ; maze 10 row 16

SnailMaze11:
.db $60, $60, $f0                               ; maze 11 snail's Y position, goal's Y position, goal's X position
.db $1d, $f0, $4d, $03, $4d, $74, $8c, $a3      ; maze 11 row 1
.db $09, $6d, $50, $10, $11, $2d, $31, $50      ; maze 11 row 2
.db $cd, $55, $2d, $4d, $5d, $78, $44, $97      ; maze 11 row 3
.db $91, $85, $41, $4b, $44, $23, $35, $66      ; maze 11 row 4
.db $51, $f8, $8e, $1d, $81, $34, $e1, $58      ; maze 11 row 5
.db $2d, $2c, $45, $49, $34, $d5, $8e, $50      ; maze 11 row 6
.db $d1, $39, $52, $38, $49, $24, $0d, $6d      ; maze 11 row 7
.db $85, $dd, $2d, $44, $8d, $3b, $7a, $91      ; maze 11 row 8
.db $c1, $56, $04, $51, $41, $c4, $6e, $6c      ; maze 11 row 9
.db $01, $18, $79, $84, $10, $05, $b8, $51      ; maze 11 row 10
.db $4d, $83, $e0, $40, $43, $31, $0d, $6e      ; maze 11 row 11
.db $11, $ee, $60, $93, $10, $2e, $76, $50      ; maze 11 row 12
.db $ed, $bb, $83, $d7, $8d, $db, $e8, $9d      ; maze 11 row 13
.db $b1, $4c, $43, $85, $41, $20, $8d, $5b      ; maze 11 row 14
.db $ed, $51, $50, $35, $5e, $77, $f8, $56      ; maze 11 row 15
.db $89, $20, $20, $0a, $88, $22, $20, $28      ; maze 11 row 16

SnailMaze12:
.db $40, $88, $c0                               ; maze 12 snail's Y position, goal's Y position, goal's X position
.db $35, $00, $00, $00, $0d, $00, $00, $50      ; maze 12 row 1
.db $d5, $00, $00, $40, $35, $00, $00, $54      ; maze 12 row 2
.db $55, $03, $00, $50, $d5, $00, $00, $55      ; maze 12 row 3
.db $55, $0d, $00, $54, $55, $03, $40, $55      ; maze 12 row 4
.db $55, $35, $00, $55, $55, $0d, $50, $55      ; maze 12 row 5
.db $55, $d5, $40, $55, $55, $35, $54, $55      ; maze 12 row 6
.db $55, $55, $53, $55, $55, $d5, $55, $55      ; maze 12 row 7
.db $55, $95, $55, $55, $55, $55, $55, $55      ; maze 12 row 8
.db $55, $05, $56, $55, $55, $55, $55, $55      ; maze 12 row 9
.db $55, $01, $58, $55, $55, $55, $56, $55      ; maze 12 row 10
.db $55, $00, $60, $55, $55, $55, $5b, $55      ; maze 12 row 11
.db $15, $00, $80, $55, $55, $55, $60, $55      ; maze 12 row 12
.db $05, $00, $00, $56, $55, $15, $80, $55      ; maze 12 row 13
.db $0d, $00, $00, $58, $55, $05, $00, $56      ; maze 12 row 14
.db $0d, $00, $00, $60, $55, $01, $00, $58      ; maze 12 row 15
.db $01, $00, $00, $80, $80, $00, $00, $a0      ; maze 12 row 16