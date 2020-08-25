.memorymap
slotsize $4000
slot 0 $0000
slot 1 $4000
slot 2 $8000
defaultslot 2
.endme

.rombankmap
bankstotal 1
banksize $4000
banks 1
.endro

.enum $C000 export
RAM_Start dsb $80
.ende

.enum $C080 export
PaletteIndexFor1bppTiles db ; Used internally to LoadTiles1bpp, holds the palette index to extend the 1bpp to
TilemapRectHighByte db ; When drawing 8-bit tilemap data, this determines the high byte used.
PaletteRotationTotalCounter db
PaletteRotationFrameCounter db ; Cycles 0-4 to slow the rotation
PaletteRotationPosition db ; Cycles 0-10 to select the palette rotation position
.ende

.enum $C090 export
RAM_ScrollUpdateNeeded db ; Set when the VBlank handler should emit data to the scroll registers
LogoScrollingControl db ; Low bit enables the MASTER SYSTEM logo scrolling, high bit enables the SEGA/MASTER SYSTEM logos scrolling apart
YScrollDeltaCounter db ; Counter for how far the SEGA/MASTER SYSTEM logos scroll apart
.ende

.enum $C096 export
RAM_XScroll db
.ende

.enum $C099 export
_RAM_C099_ db ; Unused?
RAM_YScroll db
.ende

.enum $C0A0 export
RAM_PaletteLoadPending db
RAM_PaletteLoadDataAddress dw
RAM_ClearSegaLogoFlag db
.ende

.enum $C100 export
RAM_SlotCheckCode dsb $66
.ende

.enum $C700 export
RAM_SpriteTable .db
RAM_SpriteTableYs dsb 64
RAM_SpriteTableXNs dsb 64*2
.ende

.enum $D000 export
SegaLogoRightWayUpWithGap dsb 3*8*8*3
SegaLogoUpsideDownWithPadding dsb 3*8*8*3
.ende

.enum $D480 export
RAM_YScrollForSegaLogo dw ; High byte is the real scroll value, 16-bit value is used to allow fractional scrolling. 
.ende

; Ports
.define Port_MemoryControl $3E
.define Port_VDPData $BE
.define Port_VDPAddress $BF

; Input Ports
.define Port_VDPStatus $BF

; VRAM masks
.define VDP_Mask_VRAMWrite $4000
.define VDP_Mask_Palette $c000

; VDP constants
.define SizeOfTile 32
.define SizeOfNameTableRow 64
.define SizeOfNameTable SizeOfNameTableRow*24
.define NameTableAddress $3800
.define SpriteTableAddress $3f00

.bank 0 slot 0
.org $0000

.emptyfill $ff

StartOfROM:
	di
	im 1
	ld sp, $dff0
	jr Boot

.org $38
VBlankInterruptVector:
	jp VBlankHandler

.org $66
PauseInterruptVector:
	; No pause button support
	retn

Boot:
	; Blank RAM
	ld hl, RAM_Start
	ld de, RAM_Start + 1
	ld bc, 8*1024-1 ; All of RAM
	ld (hl), 0
	ldir

	; Initialise sprite table mirrors
	ld de, RAM_SpriteTableYs
	ld hl, SpriteTableData
	ld bc, _sizeof_SpriteTableDataYs
	ldir

	ld de, RAM_SpriteTableXNs
	ld bc, _sizeof_SpriteTableDataXNs
	ldir

	call BootDelay

	; Initialise VDP registers (and set palette write address for the border colour)
	ld hl, VDPRegistersData
	ld bc, _sizeof_VDPRegistersData << 8 | Port_VDPAddress
	otir
	xor a ; Set border to black
	out (Port_VDPData), a

	; Blank tile 0
	ld l, a
	ld de, 0 | VDP_Mask_VRAMWrite
	ld bc, SizeOfTile 
	call FillVRAMWithLAtDE
	; And fill the name table with it
	ld de, $3800 | VDP_Mask_VRAMWrite
	ld bc, SizeOfNameTable
	call FillVRAMWithLAtDE

	ld hl, FontTiles1bpp
	ld de, $2360 | VDP_Mask_VRAMWrite
	ld bc, _sizeof_FontTiles1bpp ; 69*8 ; 69 1bpp tiles
	ld a, 1
	call LoadTiles1bpp

	call MoveWavingSegaLogoToMiddleOfScreen

	ld hl, LogoScrollingControl
	ld (hl), $01
	call WaitForValueAtHLToBeZero ; Waits for MASTER SYSTEM to scroll onto screen

	ld b, $02
	call Delay ; 0.95s
	
	call MasterSystemSetOnTopAndWhite
	
	ld b, $02
	call Delay ; 0.95s
	
	call PrepareForCopyright

	di
	call ShowCopyright

	; Check slots. Code is loaded to RAM as BIOS ROM is disabled. 
	ld hl, SlotCheckRAMCode
	ld de, RAM_SlotCheckCode
	ld bc, SlotCheckRAMCodeEnd - SlotCheckRAMCode
	ldir
	call RAM_SlotCheckCode	; Code is loaded from SlotCheckRAMCode

	; Execution will only continue if no game is detected
	; Loop forever while VBlank does some animation
	ei
-:	jr -

PrepareForCopyright:
	; Signal to set the palette to show the copyright text
	ld a, $FF
	ld (RAM_PaletteLoadPending), a
	ld hl, +
	ld (RAM_PaletteLoadDataAddress), hl
	
	; Scroll up by 16? and wait for it to be done
	ld a, $10
	ld (YScrollDeltaCounter), a
	ld hl, LogoScrollingControl
	ld (hl), $80
	jp WaitForValueAtHLToBeZero ; and ret

+:
.dw 1 | VDP_Mask_Palette ; Address
.db $01 ; Count
.db $3F ; Data

MasterSystemSetOnTopAndWhite:
	; Set Master System logo colours to white
	ld hl, +
	ld (RAM_PaletteLoadDataAddress), hl
	ld hl, RAM_PaletteLoadPending
	ld (hl), $FF
	jp WaitForValueAtHLToBeZero ; and ret

+: 
.dw $1D | VDP_Mask_Palette ; Address
.db $03 ; Count
.db $3F $3F $3F ; Data - white

ShowCopyright:
	ld de, $3BD4 | VDP_Mask_VRAMWrite ; Nametable location to draw it
	call SetVRAMAddressToDE
	ld b, _sizeof_CopyrightTilemap
	ld hl, CopyrightTilemap
-:	ld a, (hl)
	out (Port_VDPData), a
	inc hl
	ld a, $01 ; High tilemap byte for upper tileset
	out (Port_VDPData), a
	djnz -
	ret

CopyrightTilemap:
.db $1B " " $1C $1D $1E $1F " 1986" ; (c) SEGA 1986


; Executed in RAM at 100
SlotCheckRAMCode:

; This can be added to any offset to get the post-relocation address
.define SlotCheckOffset RAM_SlotCheckCode - SlotCheckRAMCode

	ld hl, SlotPort3EValues + SlotCheckOffset
	ld b, 3 ; Number to try

-:	ld a, $EB ; Disable BIOS?
	out (Port_MemoryControl), a
	ld a, (hl) ; Read in value from table
	out (Port_MemoryControl), a
	push hl
	push bc
		call _CheckForText1 + SlotCheckOffset ; $C12E	; Code is loaded from ++
		call _CheckForText2 + SlotCheckOffset ; $C135	; Code is loaded from +++
+:		call _CheckForCode  + SlotCheckOffset ; $C124	; Code is loaded from +
	pop bc
	pop hl
	inc hl
	djnz -

	ld a, $EB
	out (Port_MemoryControl), a
	ld a, (hl)
	out (Port_MemoryControl), a
	ret

; Slot check 3
_CheckForCode:	
	ld b, _sizeof_Code
	ld hl, Code + SlotCheckOffset
	ld de, $38
	jr _BootIfMatches

; Slot check 1
_CheckForText1:
	ld b, _sizeof_Text1
	ld hl, Text1 + SlotCheckOffset 
	jr _BootIfMatchesAt7FE0

; Executed in RAM at 135
_CheckForText2:
	ld b, _sizeof_Text2
	ld hl, Text2 + SlotCheckOffset
_BootIfMatchesAt7FE0:
	ld de, $7FE0 ; Checked address
_BootIfMatches:
--:	ld a, (de)
	cp (hl)
	ret nz ; nop out here to always boot 
	inc hl
	inc de
	djnz -
	; Boot if found
	jp StartOfROM

Text1:
.db "COPYRIGHT SEGA"
; Matches Choplifter, Fantasy Zone [v0]

Text2:
.db "COPYRIGHTSEGA"
; Matches nothing?

Code:
jp $00c8
.db $01 ; first byte of ld bc, nnnn
; Matches nothing?

SlotPort3EValues:
;    ,------- Expansion
;    |,------ Cartridge
;    ||,----- Card
;    |||,---- RAM
;    ||||,--- BIOS
;    |||||,-- IO
.db %11001011 ; Card, RAM, IO
.db %10101011 ; Cart
.db %01101011 ; Expansion
.db %11100011 ; What we do if nothing is found = continue BIOS

SlotCheckRAMCodeEnd:

BootDelay:
	; Delay 2.38s 
	ld b, 5

Delay:
	; Count down 65536*b times
	; Takes roughly 0.476*b seconds
	ld de, 0
-:	dec de
	ld a, d
	or e
	jr nz, -
	djnz Delay
	ret

SpriteTableData:
SpriteTableDataYs:
.db $50 $50 $50 $50 $50 $50 $50 $50
.db $58 $58 $58 $58 $58 $58 $58 $58
.db $60 $60 $60 $60 $60 $60 $60 $60
.db $d0 ; Sprite table terminator

SpriteTableDataXNs:
.db $60 $01 $68 $02 $70 $03 $78 $04 $80 $05 $88 $06 $90 $07 $98 $08
.db $60 $09 $68 $0A $70 $0B $78 $0C $80 $0D $88 $0E $90 $0F $98 $10
.db $60 $11 $68 $12 $70 $13 $78 $14 $80 $15 $88 $16 $90 $17 $98 $18

SetVRAMAddressToDE:
	ld a, e
	out (Port_VDPAddress), a
	ld a, d
	out (Port_VDPAddress), a
	ret

WriteAToVRAMAddressDE:
	push af
		call SetVRAMAddressToDE
	pop af
WriteAToVDP:
	out (Port_VDPData), a
	ret

ScreenOff:
	ld de, $81A0 ; %10100000 = screen off with interrupts
	jr SetVDPRegisterDtoE

ScreenOn:
	ld de, $81E0 ; %11100000 = screen on with interrupts
	; fall through

SetVDPRegisterDtoE:
	; This is a duplicate of SetVRAMAddressToDE 
	ld a, e
	out (Port_VDPAddress), a
	ld a, d
	out (Port_VDPAddress), a
	ret

CopyBCBytesFromHLToVRAMAtDE:
	call SetVRAMAddressToDE
CopyBCBytesFromHLToVRAM:
	ld a, c
	or a
	jr z, +
	inc b
+:	ld a, b
	ld b, c
	ld c, Port_VDPData
-:	outi
	jp nz, -
	dec a
	jp nz, -
	ret

FillVRAMWithLAtDE:
	call SetVRAMAddressToDE
	; fall through

FillVRAMWithL:
	; l = value to write
	; bc = count
	ld a, c
	or a
	jr z, +
	inc b
+:
	ld a, l
-:
	out (Port_VDPData), a
	dec c
	jr nz, -
	djnz -
	ret

--:
	push bc
	call SetVRAMAddressToDE
	ld b, c
	ld c, Port_VDPData
-:
	outi
	nop
	jr nz, -
_LABEL_249_:
	ex de, hl
	ld bc, $0040
	add hl, bc
	ex de, hl
	pop bc
	djnz -21
	ret

DrawTilemapRectWithHighByteA:
	ld (TilemapRectHighByte), a
	; fall through

DrawTilemapRect:
	; de = tilemap address
	; bc = width, height
	; hl = pointer to tile indices
	push bc
		call SetVRAMAddressToDE
		ld b, c
		ld c, Port_VDPData
-:		outi
		ld a, (TilemapRectHighByte)
		nop
		out (c), a
		nop
		jp nz, -
		ex de, hl
		ld bc, SizeOfNameTableRow
		add hl, bc
		ex de, hl
	pop bc
	djnz DrawTilemapRect
	ret

LoadTiles1bpp:
	; de = VRAM address
	; hl = data address
	; a = palette index to use
	ld (PaletteIndexFor1bppTiles), a
	call SetVRAMAddressToDE
--:
	ld a, (hl) ; Get byte of data
	exx
		ld c, Port_VDPData
		ld b, $04
		ld h, a
		ld a, (PaletteIndexFor1bppTiles)
-:		rra
		ld d, h
		jr c, +
		ld d, $00
+:		out (c), d
		djnz -
	exx
	inc hl
	dec bc
	ld a, b
	or c
	jp nz, --
	ret

CopySpriteTableToVRAM:
	ld hl, RAM_SpriteTable
	ld de, $3F00 | VDP_Mask_VRAMWrite
	ld bc, $0040
	call CopyBCBytesFromHLToVRAMAtDE
	ld hl, RAM_SpriteTableXNs
	ld de, $3F80 | VDP_Mask_VRAMWrite
	ld bc, $0080
	jp CopyBCBytesFromHLToVRAMAtDE ; and ret

VDPRegistersData:
.db $26 $80 
.db $A0 $81 
.db $FF $82 
.db $FF $83 
.db $FF $84 
.db $FF $85 
.db $FB $86 
.db $00 $87
.db $00 $88 
.db $00 $89 
.db $00 $8A 
; Last pair is instead setting the VDP to CRAM writing at index 1
.dw $c001

; Unused code
	xor a
	ld (RAM_YScroll), a
	ld (RAM_XScroll), a

MaybeUpdateScrollRegisters:
	; Check flag
	ld hl, RAM_ScrollUpdateNeeded
	ld a, (hl)
	or a
	ret z
	; Clear flag
	ld (hl), 0
	; Update scroll values
	ld a, (RAM_YScroll)
	ld e, a
	ld d, $89
	call SetVDPRegisterDtoE
	ld a, (RAM_XScroll)
	ld e, a
	dec d
	jp SetVDPRegisterDtoE

MoveWavingSegaLogoToMiddleOfScreen:
	; Fill palette
	; Tile palette is black, tile palette is a mix of inline colours and a data block
	ld l, %000000 ; Black
	ld de, 0 | VDP_Mask_Palette
	ld bc, 17
	call FillVRAMWithLAtDE
	; Then some inline colours for the sprites. These are not used in the end.
	ld a, %110000 ; Blue
	out (Port_VDPData), a
	ld a, %110100 ; Cyan
	out (Port_VDPData), a
	ld a, %110100 ; Blue
	out (Port_VDPData), a

	ld hl, SegaLogoPaletteGradientData+15
	ld bc, 9
	call CopyBCBytesFromHLToVRAM

	; Fill the rest of the palette with yellow
	ld l, %001111
	ld bc, 3
	call FillVRAMWithL

	ld de, $2400 | VDP_Mask_VRAMWrite
	ld bc, 16
	ld l, 0
	call FillVRAMWithLAtDE

	ld a, $0E
	ld hl, MasterSystemTiles
	ld de, $2C00 | VDP_Mask_VRAMWrite
	ld bc, _sizeof_MasterSystemTiles
	call LoadTiles1bpp

	; Initialise variables?
	ld hl, $8800 ; Scroll by 88 so the Sega logo is off-screen
	ld (RAM_YScrollForSegaLogo), hl
	ld (_RAM_C099_), hl

	; This will reset the scroll registers to zero
	ld hl, RAM_ScrollUpdateNeeded
	ld (hl), 1
	ei
		call WaitForValueAtHLToBeZero
	di

	; Disable sprites
	ld de, SpriteTableAddress | VDP_Mask_VRAMWrite
	ld a, $D0
	call WriteAToVRAMAddressDE

	ld de, $3B98 | VDP_Mask_VRAMWrite
	ld hl, SegaLogoTilemap
	ld bc, $0308 ; 3x8 size
	ld a, %00001001 ; High byte for tilemap: high tileset and priority flag
	ld (TilemapRectHighByte), a
	call DrawTilemapRect

	ld a, $0A
	ld (PaletteRotationTotalCounter), a
	
	; Load 4bpp (gradient effect) Sega logo tiles at tile index 1
	ld hl, SegaLogoTiles4bpp
	ld de, $0020 | VDP_Mask_VRAMWrite
	ld bc, $0300
	call CopyBCBytesFromHLToVRAMAtDE

	; Load 1bpp tiles to RAM
	; Source data is in column-major tile order, so stored A-X in ROM and displayed as so on-screen:
	; ADGJMPSV
	; BEHKNQTW
	; CFILORUX
	; where each letter is one 1bpp tile (8 bytes) in the usual format.
	; The data is copied to RAM in the form
	; ABC   ABC
	; DEF   DEF
	; GHI   GHI
	; JKL   JKL
	; MNO   MNO
	; PQR   PQR
	; STU   STU
	; VWX   VWX
	; i.e. each set of three tiles is copied, then a gap of the same size, then the three tiles again.
	; This fills d000..d23f.
	ld b, 8 ; Column count
	ld de, SegaLogoRightWayUpWithGap
	ld hl, SegaLogoTiles1bpp
-:	push bc
		push hl
			ld bc, 3*8 ; Copy 3 tiles
			ldir
			ld hl, 3*8 ; Skip ahead 3 tiles in the destination
			add hl, de
			ex de, hl
		pop hl
		ld bc, 3*8 ; Copy same 3 tiles again
		ldir
	pop bc
	djnz -

	; Next, we produce an upside-down version of the tile data, by copying the data in reverse.
	; Each tile column is padded opposite to the above, so we have 24 bytes empty, then 24 bytes
	; for the column upside-down, then another 24 bytes empty.
	ld hl, SegaLogoTiles1bpp
	ld de, SegaLogoUpsideDownWithPadding + 3*8 - 1 ; _RAM_D26F_
	ld c, 8 ; Columns
--:	ld b, 3*8 ; Bytes to reverse per column
-:	ld a, (hl) ; Read top to bottom from source data
	ld (de), a ; Emit bottom to top to destination
	inc hl
	dec de
	djnz -

	push hl
		ld hl, 3*8*4 ; Move to bottom of next column's destination
		add hl, de
		ex de, hl
	pop hl
	dec c
	jr nz, --

	; This layout of data in RAM means that we can draw a "scrolled in tiles" version of each of the logos 
	; (right way up and upside-down) by copying a 24 byte chunk from each column using an appropriate offset.
	; The repetition means the chunk doesn't need to worry about hitting the end of the buffer. 

	; Turn on the screen; there's still nothing showing though
	call ScreenOn

	ld c, 4 ; Loops of the animation
_WavingLogoStartPosition:
	ld hl, SegaLogoRightWayUpWithGap
	ld de, SegaLogoUpsideDownWithPadding + 3*8 - 1
	ld b, $30 ; Number of "positions" before resetting
_WavingLogoNextPosition:
	push bc
	push de
	push hl
		call PointVRAMToSegaLogoArea
		; Emit the logo to the tiles
		ld c, 8 ; Columns
--:		push hl
		push de
			ld b, 3*8 ; Rows per column
-:			ld a, (hl) ; One byte from the right way up for bitplane 1
			call WriteAToVDP
			ld a, (de) ; One from the upside down version for bitplane 2
			call WriteAToVDP
			xor a ; Zero for the other two bitplanes
			call WriteAToVDP
			call WriteAToVDP
			inc hl
			inc de
			djnz -
		pop de
		pop hl
		push bc
			ld bc, 3*8*8 ; Move to next column
			add hl, bc
			ex de, hl
				add hl, bc
			ex de, hl
		pop bc
		dec c
		jr nz, --

		ld hl, (RAM_YScrollForSegaLogo)
		ld de, 0.75 * $100 ; $00C0 ; 0.75 pixels per frame
		add hl, de
		jr nc, +
		ld hl, $FFFF
+:		ld (RAM_YScrollForSegaLogo), hl
		ld a, h
		out (Port_VDPAddress), a
		ld a, $89
		out (Port_VDPAddress), a
	pop hl
	pop de
	pop bc

	inc hl ; Move forward through the right-way-up data
	dec de ; And backwards through the upside-down data
	djnz _WavingLogoNextPosition
	dec c
	jr nz, _WavingLogoStartPosition

	; When all that is done, the scroll position should be 0
	xor a
	ld (RAM_YScroll), a
	ld (RAM_XScroll), a
	inc a
	ld (RAM_ScrollUpdateNeeded), a
	; Signal to clear the Sega logo from the tilemap
	ld (RAM_ClearSegaLogoFlag), a
	ei
	ld hl, PaletteRotationTotalCounter

WaitForValueAtHLToBeZero:
-:	ld a, (hl)
	or a
	jr nz, -
	ret

MaybeClearSegaLogo:
	; Check flag
	ld hl, RAM_ClearSegaLogoFlag
	ld a, (hl)
	or a
	ret z
	; If non-zero, clear tilemap for the Sega logo
	ld (hl), 0 ; clear the flag so we do this only once
	ld l, 0
	ld de, $3B98 | VDP_Flag_VRAMWrite
	ld bc, 16
	call FillVRAMWithLAtDE
	ld de, $3BD8 | VDP_Flag_VRAMWrite
	ld bc, 16
	call FillVRAMWithLAtDE
	ld de, $3C18 | VDP_Flag_VRAMWrite
	ld bc, 16
	jp FillVRAMWithLAtDE ; and ret

RotatePalette:
	; Decrement values
	ld hl, PaletteRotationTotalCounter
	dec (hl)
	ld hl, PaletteRotationFrameCounter
	dec (hl)
	; Exit until the second one goes past 0
	ret p
	; Else set it to 5
	ld (hl), 5
	inc hl
	; Then increment PaletteRotationPosition
	ld a, (hl)
	inc (hl)
	cp 11
	jr nz, +
	ld (hl), 0
+:	; Use that as a reverse index into SegaLogoPaletteGradientData 
	ld e, a
	ld d, 0
	ld hl, SegaLogoPaletteGradientData+13
	or a
	sbc hl, de
	ld de, 11 | VDP_Mask_Palette ; destination
	ld bc, 12 ; count
	jp CopyBCBytesFromHLToVRAMAtDE

PointVRAMToSegaLogoArea:
	push de
		ld de, $2000 | VDP_Flag_VRAWWrite
		ld a, e
		out (Port_VDPAddress), a
		ld a, d
		out (Port_VDPAddress), a
	pop de
	ret

SegaLogoTilemap: ; Low bytes only
.db $00 $03 $06 $09 $0C $0F $12 $15 
.db $01 $04 $07 $0A $0D $10 $13 $16
.db $02 $05 $08 $0B $0E $11 $14 $17

SegaLogoTiles1bpp:
.incbin "Sega logo.1bpp"

SegaLogoPaletteGradientData:
.db $3F $3E $3C $38 $34 $30 $20 $30 $34 $38 $3C $3E 
.db $3F $3E $3C $38 $34 $30 $20 $30 $34 $38 $3C $3E

SegaLogoTiles4bpp:
.incbin "Sega logo.bin"

MasterSystemTilemap: ; Foreground bit is not set 
;   M           A           S           T           E           R           space S           Y           S           T           E           M
.dw $0960 $0961 $0962 $0B62 $0963 $0964 $0965 $0966 $0967 $0968 $0967 $0969 $0000 $0963 $0964 $096A $0B6A $0963 $0964 $0965 $0966 $0967 $0968 $0960 $0961
.dw $096B $096C $096D $0B6D $0F64 $0F63 $096E $096F $0D67 $0D68 $0970 $0971 $0000 $0F64 $0F63 $096E $0B6E $0F64 $0F63 $096E $096F $0D67 $0D68 $096B $096C

MasterSystemTiles:
.incbin "mastersystem.1bpp"

VBlankHandler:
	push af
	push bc
	push de
	push hl
		; Satisfy interrupt
		in a, (Port_VDPStatus)
		call CopySpriteTableToVRAM
		; Some of these only happen when flags enable them. The out-of-interrupt code controls those flags.
		call MaybeLoadPalette
		call RotatePalette
		call MaybeClearSegaLogo
		call MaybeScrollMasterSystemLogo
		call MaybeScrollLogosApart
		call MaybeUpdateScrollRegisters
	pop hl
	pop de
	pop bc
	pop af
	ei
	ret

MaybeLoadPalette:
	; Loads palette data from RAM_PaletteLoadDataAddress but only if RAM_PaletteLoadPending is non-zero

	; Check if the flag is set
	ld hl, RAM_PaletteLoadPending
	ld a, (hl)
	or a
	ret z ; Return early if not
	ld (hl), $00

	inc a
	ret nz ; If it was not $ff, ignore it (doesn't actually happen?)

	ld hl, (RAM_PaletteLoadDataAddress)
	; Pointed data is in the form:
	; dw address
	; db count
	; dsb <count> data
	; Get the address in de and count in b
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ld b, (hl)
	inc hl
	call SetVRAMAddressToDE
	ld c, Port_VDPData
	; Emit data
	otir

	; Set the priority bt on the tilemap for the Master System logo
	ld hl, MasterSystemTilemap + 1 ; Offset 1 to the 
	ld de, $3ac9 | VDP_Mask_VRAMWrite ; Top row of table
	call +
	ld de, $3b09 | VDP_Mask_VRAMWrite
+:
	ld b, 25 ; Tile count
	call SetVRAMAddressToDE
-:	ld a, (hl)
	or $10 ; Set priority bit to show in front of sprites
	out (Port_VDPData), a
	inc hl ; Skip tile index byte
	inc hl
	in a, (Port_VDPData) ; Skip a byte in VRAM
	djnz -
	ret

MaybeScrollMasterSystemLogo:
	; Check for flag low bit
	ld a, (LogoScrollingControl)
	rrca
	ret nc

	; Signal a scroll update
	ld hl, RAM_ScrollUpdateNeeded
	ld (hl), $FF
	; Decrement the X scroll by 4
	ld hl, RAM_XScroll
	ld a, (hl)
	sub 4
	ld (hl), a
	; Check if it got to 0
	or a
	jp z, _scrollDone
	; If not, check where we are 
	neg
	cp $20 ; Outside this range we have nothing to draw yet
	ret c
	cp $E8
	ret nc
	; Inside that range, we do work when it is a multiple of 8px 
	and $07
	ret nz

	; Then we re-load it to decide what needs to be done
	ld a, (hl)
	neg ; Convert to an amount scrolled
	and %11111000 ; Round down by 8
	rrca ; Divide by 4
	rrca
	or %11000000 ; Set high bits
	; That's now the tilemap offset to draw at... which we put into de
	ld e, a
	ld d, $7A

	; Then we index into MasterSystemTilemap for that amount
	ld b, 0
	sub $C8
	ld c, a
	ld hl, MasterSystemTilemap
	add hl, bc
	; Emit one tile
	ld bc, 2
	call CopyBCBytesFromHLToVRAMAtDE

	; Then repeat for row 2
	ld d, $7B
	ld a, e
	and $3E
	ld e, a
	ld bc, $0030
	add hl, bc
	ld bc, 2
	jp CopyBCBytesFromHLToVRAMAtDE ; and ret

_scrollDone:
	; Clear the flag
	xor a
	ld (LogoScrollingControl), a
	ret

MaybeScrollLogosApart:
	; Check high bit of this flag
	ld a, (LogoScrollingControl)
	rlca
	ret nc
	; Signal an update
	ld hl, RAM_ScrollUpdateNeeded
	ld (hl), $FF
	; Decrement the line counter
	ld hl, YScrollDeltaCounter
	dec (hl)
	jr nz, +
	; Clear the flag when done. This will stop this code running on subsequent frames.
	xor a
	ld (LogoScrollingControl), a
+:	; Scroll up by one 
	ld hl, RAM_YScroll
	dec (hl)
	; If it reaches $e0, flip back to $20
	ld a, (hl)
	cp $E0
	jr c, +
	sub $20
	ld (hl), a

	; Also move all the sprites up by one
+:	ld hl, RAM_SpriteTable
	ld b, $18 ; Sprite count
-:	dec (hl)
	inc hl
	djnz -
	ret

FontTiles1bpp:
.incbin "font.1bpp"
