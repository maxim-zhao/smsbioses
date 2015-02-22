; This disassembly was created using Emulicious (http://www.emulicious.net)
.MEMORYMAP
SLOTSIZE $4000
SLOT 0 $0000
SLOT 1 $4000
SLOT 2 $8000
DEFAULTSLOT 2
.ENDME
.ROMBANKMAP
BANKSTOTAL 1
BANKSIZE $4000
BANKS 1
.ENDRO


.BANK 0 SLOT 0
.ORG $0000

_LABEL_0_:
	di
	im 1
	ld sp, $DFF0
	jr _LABEL_6D_

_LABEL_8_:
	push af
	rst $20	; _LABEL_20_
	pop af
	out ($BE), a
	ret

; Data from E to F (2 bytes)
.db $FF $FF

_LABEL_10_:
	rst $20	; _LABEL_20_
	xor a
	ld c, $BE
-:
	outi
	push af
	pop af
	out (c), a
	jr nz, -
	ret

_LABEL_1D_:
	ld de, $81E0
_LABEL_20_:
	ld a, e
	out ($BF), a
	ld a, d
	out ($BF), a
	ret

; Data from 27 to 27 (1 bytes)
.db $FF

_LABEL_28_:
	rst $20	; _LABEL_20_
	ld a, c
	or a
	jr z, +
	inc b
+:
	ld a, l
-:
	out ($BE), a
	dec c
	jr nz, -
	djnz -
	ret

; Data from 37 to 37 (1 bytes)
.db $FF

_LABEL_38_:
	jp _LABEL_3B7_

; Data from 3B to 4C (18 bytes)
.db $E7 $79 $B7 $28 $01 $04 $78 $41 $0E $BE $ED $A3 $20 $FC $3D $20
.db $F9 $C9

_LABEL_4D_:
	ld hl, $C300
	ld de, $7F00
	ld bc, $64BE
	rst $20	; _LABEL_20_
	otir
	ld hl, $C380
	ld de, $7F80
	ld b, $80
	rst $20	; _LABEL_20_
	otir
	ret

; Data from 65 to 65 (1 bytes)
.db $FF

_LABEL_66_:
	retn

; Data from 68 to 6C (5 bytes)
.db $11 $A0 $81 $18 $B3

_LABEL_6D_:
	ld hl, $08D1
	ld c, $7F
	ld b, $04
	otir
	ld hl, $0480
	ld bc, $18BF
	otir
	xor a
	out ($BE), a
	ld hl, $C000
	ld de, $C001
	ld bc, $1FFF
	ld (hl), $00
	ldir
	ld b, $0E
	call _LABEL_366_
_LABEL_93_:
	ld hl, $0480
	ld bc, $18BF
	otir
	xor a
	out ($BE), a
	ld hl, $0394
	ld ($C203), hl
	ld a, $D0
	ld ($C300), a
	ei
	ld a, $03
	call _LABEL_426_
	di
	ld de, $4000
	ld bc, $0020
	rst $28	; _LABEL_28_
	call _LABEL_4E6_
	ld de, $67E0
	ld bc, $0020
	rst $28	; _LABEL_28_
	ld hl, $1704
	ld de, $4360
	ld bc, $0228
	ld a, $09
	call _LABEL_461_
	ld hl, $192C
	ld de, $4C00
	call _LABEL_498_
	xor a
	ld ($C202), a
	ld de, $7A16
	ld hl, $0FBB
	ld bc, $040A
	call _LABEL_451_
	ld a, $81
	ld ($CF00), a
	ld de, $4EC0
	ld hl, $1028
	call _LABEL_498_
	ld hl, $15C5
	ld de, $4060
	call _LABEL_498_
	call _LABEL_520_
	call _LABEL_1D_
	call _LABEL_4FD_
	di
	ld hl, RAMCode
	ld de, $C700
	ld bc, RAMCodeEnd-RAMCode
	ldir
	call $C700	; Possibly invalid
	di
	ld a, ($C000)
	or a
	jr z, +
	ld hl, ($C270)
	ld de, ($C26A)
	or a
	sbc hl, de
	jp z, $C7D1	; Possibly invalid
	call _LABEL_4E6_
	ld hl, $0176
	ld b, $03
	call _LABEL_16A_
-:
	jr -

+:
	call _LABEL_162_
-:
	ei
	ld a, $80
	call _LABEL_426_
	ld a, ($C207)
	and $31
	cp $31
	jp z, _LABEL_B90_
	jr -

_LABEL_14D_:
	ld de, $7D54
	ld b, $0B
	ld hl, $0157
	rst $10	; _LABEL_10_
	ret

; Data from 157 to 161 (11 bytes)
.db $1B $20 $1C $1D $1E $1F $20 $31 $39 $38 $36

_LABEL_162_:
	call _LABEL_4E6_
	ld b, $0C
	ld hl, $01A3
_LABEL_16A_:
	push bc
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	ld b, (hl)
	inc hl
	rst $10	; _LABEL_10_
	pop bc
	djnz _LABEL_16A_
	ret

; Data from 176 to 365 (496 bytes)
.db $84 $78 $19 $4D $41 $53 $54 $45 $52 $20 $53 $59 $53 $54 $45 $4D
.db $20 $1B $20 $1C $1D $1E $1F $20 $31 $39 $38 $36 $04 $79 $0E $53
.db $4F $46 $54 $57 $41 $52 $45 $20 $45 $52 $52 $4F $52 $34 $78 $04
.db $56 $31 $2E $33 $84 $78 $13 $57 $45 $4C $43 $4F $4D $45 $20 $54
.db $4F $20 $54 $48 $45 $20 $53 $45 $47 $41 $04 $79 $0E $4D $41 $53
.db $54 $45 $52 $20 $53 $59 $53 $54 $45 $4D $2E $C4 $79 $19 $54 $4F
.db $20 $50 $4C $41 $59 $2C $4A $55 $53 $54 $20 $46 $4F $4C $4C $4F
.db $57 $20 $54 $48 $45 $53 $45 $44 $7A $0D $49 $4E $53 $54 $52 $55
.db $43 $54 $49 $4F $4E $53 $3A $04 $7B $19 $31 $2E $54 $55 $52 $4E
.db $20 $4F $46 $46 $20 $50 $4F $57 $45 $52 $20 $4F $4E $20 $50 $4F
.db $57 $45 $52 $88 $7B $05 $42 $41 $53 $45 $2E $04 $7C $1C $32 $2E
.db $49 $4E $53 $45 $52 $54 $20 $43 $41 $52 $44 $2F $43 $41 $52 $54
.db $52 $49 $44 $47 $45 $20 $49 $4E $54 $4F $88 $7C $07 $53 $59 $53
.db $54 $45 $4D $2E $04 $7D $1C $33 $2E $54 $55 $52 $4E $20 $50 $4F
.db $57 $45 $52 $20 $42 $41 $43 $4B $20 $4F $4E $2C $41 $4E $44 $2E
.db $2E $2E $2E $92 $7D $0D $60 $61 $62 $63 $64 $65 $66 $67 $68 $69
.db $63 $63 $63 $D2 $7D $0D $6A $6B $6C $6D $6E $6F $70 $71 $72 $00
.db $73 $73 $73 

RAMCode:
.include "export1.3.ram.asm"
RAMCodeEnd:

_LABEL_366_:
	ld de, $4000
-:
	dec de
	ld a, d
	or e
	jr nz, -
	djnz _LABEL_366_
	ret

; Data from 371 to 376 (6 bytes)
.db $0E $0D $0C $0A $08 $06

_LABEL_377_:
	di
	ld sp, $DFF0
	ld hl, $C000
	ld de, $C001
	ld bc, $1FFF
	ld (hl), $00
	ldir
	ld hl, $08D1
	ld c, $7F
	ld b, $04
	otir
	jp _LABEL_93_

; Data from 394 to 3B6 (35 bytes)
.db $20 $00 $C0 $00 $3F $3E $3F $30 $30 $38 $3F $37 $3F $00 $00 $00
.db $00 $00 $00 $00 $03 $30 $0F $07 $16 $3F $02 $00 $0F $00 $00 $00
.db $00 $00 $00

_LABEL_3B7_:
	push af
	in a, ($BF)
	rlca
	jp nc, _LABEL_416_
	push bc
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
	ld a, ($C20B)
	or a
	jr z, +
	in a, ($DD)
	and $10
	ld hl, $C206
	ld c, (hl)
	ld (hl), a
	xor c
	and c
	jp nz, _LABEL_377_
+:
	ld a, ($C200)
	rrca
	push af
	call c, _LABEL_4D_
	pop af
	rrca
	call c, _LABEL_42F_
	rrca
	push af
	call c, _LABEL_577_
	call _LABEL_4F0_
	pop af
	rrca
	push af
	call c, _LABEL_D2A_
	pop af
	rrca
	call c, _LABEL_D5F_
	call _LABEL_5B8_
	xor a
	ld ($C200), a
	pop hl
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
	ei
	ret

_LABEL_416_:
	push de
	ld de, ($C225)
	ld d, $88
	rst $20	; _LABEL_20_
	ld de, $8AFF
	rst $20	; _LABEL_20_
	pop de
	pop af
	ei
	ret

_LABEL_426_:
	ld hl, $C200
	ld (hl), a
-:
	ld a, (hl)
	or a
	jr nz, -
	ret

_LABEL_42F_:
	ld hl, ($C203)
	ld b, (hl)
	inc hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc hl
	rst $20	; _LABEL_20_
	ld c, $BE
	otir
	ret

; Data from 43E to 450 (19 bytes)
.db $E7 $DB $BE $C9 $E7 $DB $BE $77 $E3 $E3 $DB $BE $23 $0B $78 $B1
.db $20 $F3 $C9

_LABEL_451_:
	ld a, c
	ld c, b
	ld b, a
-:
	push bc
	rst $10	; _LABEL_10_
	ex de, hl
	ld bc, $0040
	add hl, bc
	ex de, hl
	pop bc
	dec c
	jr nz, -
	ret

_LABEL_461_:
	ld ($C201), a
	rst $20	; _LABEL_20_
--:
	ld a, (hl)
	exx
	ld bc, $04BE
	ld h, a
	ld a, ($C201)
-:
	rra
	ld d, h
	jr c, +
	ld d, $00
+:
	out (c), d
	djnz -
	exx
	inc hl
	dec bc
	ld a, b
	or c
	jr nz, --
	ret

; Data from 480 to 497 (24 bytes)
.db $36 $80 $A0 $81 $FF $82 $FF $83 $FF $84 $FF $85 $FB $86 $00 $88
.db $00 $89 $FF $8A $00 $87 $10 $C0

_LABEL_498_:
	ld b, $04
-:
	push bc
	push de
	call _LABEL_4A5_
	pop de
	inc de
	pop bc
	djnz -
	ret

_LABEL_4A5_:
	ld a, (hl)
	inc hl
	or a
	ret z
	ld b, a
	ld c, a
	res 7, b
-:
	ld a, e
	di
	out ($BF), a
	ld a, d
	out ($BF), a
	ld a, (hl)
	out ($BE), a
	ei
	bit 7, c
	jr z, +
	inc hl
+:
	inc de
	inc de
	inc de
	inc de
	djnz -
	jr nz, _LABEL_4A5_
	inc hl
	jr _LABEL_4A5_

; Data from 4C8 to 4E5 (30 bytes)
.db $AF $32 $2A $C2 $32 $26 $C2 $F3 $21 $20 $C2 $7E $B7 $C8 $36 $00
.db $3A $2A $C2 $5F $16 $89 $E7 $3A $26 $C2 $5F $15 $E7 $C9

_LABEL_4E6_:
	ld de, $7800
	ld bc, $0700
	ld l, $00
	rst $28	; _LABEL_28_
	ret

_LABEL_4F0_:
	in a, ($DC)
	ld hl, $C207
	cpl
	ld c, a
	xor (hl)
	ld (hl), c
	inc hl
	and c
	ld (hl), a
	ret

_LABEL_4FD_:
	ld a, $05
	call _LABEL_426_
	ld a, ($CF02)
	or a
	jr z, _LABEL_4FD_
	ld de, $7B8A
	ld hl, $0FE3
	ld bc, $0317
	di
	call _LABEL_451_
	call _LABEL_14D_
	ei
-:
	ld a, ($CF01)
	or a
	jr z, -
	ret

_LABEL_520_:
	ld a, $03
	ld ($C201), a
	ld a, $F0
	ld ($C202), a
	ld e, a
	ld d, $88
	rst $20	; _LABEL_20_
	ld l, $FF
	ld de, $5FC0
	ld bc, $0040
	rst $28	; _LABEL_28_
	ld de, $C300
	ld a, $08
-:
	ld hl, $0563
	ld bc, $0004
	ldir
	dec a
	jr nz, -
	ld a, $D0
	ld (de), a
	ld e, $80
	ld bc, $08FF
--:
	ld a, $04
-:
	ldi
	ldi
	dec a
	jr z, +
	dec hl
	dec hl
	jr -

+:
	djnz --
	ld a, $01
	jp _LABEL_426_

; Data from 563 to 576 (20 bytes)
.db $38 $40 $48 $50 $47 $FE $5A $FE $6C $FE $7E $FE $4F $FF $62 $FF
.db $74 $FF $86 $FF

_LABEL_577_:
	ld hl, $C201
	dec (hl)
	ret nz
	ld (hl), $02
	ld hl, $C202
	ld a, (hl)
	cp $01
	jr nz, +
	ld a, $80
	ld ($C200), a
	ret

+:
	ld e, a
	ld d, $88
	rst $20	; _LABEL_20_
	inc (hl)
	ld a, (hl)
	neg
	bit 7, a
	ret nz
	cp $08
	ld de, $5FE0
	jr c, +
	sub $08
	ld de, $5FC0
+:
	ld hl, $05AF
	ld b, $00
	ld c, a
	add hl, bc
	ld l, (hl)
	ld c, $20
	rst $28	; _LABEL_28_
	ret

; Data from 5AF to 5B7 (9 bytes)
.db $00 $01 $03 $07 $0F $1F $3F $7F $FF

_LABEL_5B8_:
	call _LABEL_5D2_
	ld ix, $CF03
	ld b, $04
-:
	push bc
	bit 7, (ix+0)
	call nz, _LABEL_618_
	ld de, $0020
	add ix, de
	pop bc
	djnz -
	ret

_LABEL_5D2_:
	ld a, ($CF00)
	cp $80
	jp c, _LABEL_8B5_
	jp z, +
	ld de, $CF03
	sub $81
	ld hl, $0B87
	ld b, $00
	add a, a
	ld c, a
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	call _LABEL_8B5_
	ld b, (hl)
	inc hl
-:
	push bc
	ld bc, $0009
	ldir
	ld a, $20
	ld (de), a
	inc de
	ld a, $01
	ld (de), a
	inc de
	xor a
	ld (de), a
	inc de
	ld (de), a
	inc de
	ld (de), a
	push hl
	ld hl, $0012
	add hl, de
	ex de, hl
	pop hl
	inc de
	pop bc
	djnz -
+:
	ld a, $80
	ld ($CF00), a
	ret

_LABEL_618_:
	ld e, (ix+12)
	ld d, (ix+13)
	inc de
	ld (ix+12), e
	ld (ix+13), d
	ld l, (ix+10)
	ld h, (ix+11)
	or a
	sbc hl, de
	call z, _LABEL_77E_
	ld e, (ix+16)
	ld d, (ix+17)
	ld a, e
	or d
	jr nz, +
	ld (ix+22), $0F
	jp _LABEL_6F0_

+:
	bit 5, (ix+0)
	jr nz, +
	ld a, (ix+6)
	or a
	jr nz, _LABEL_662_
	ld (ix+18), e
	ld (ix+19), d
	jp _LABEL_6A9_

_LABEL_657_:
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

_LABEL_662_:
	ld hl, $0B85
	call _LABEL_657_
	call _LABEL_748_
	jr _LABEL_6A9_

+:
	push de
	ld l, (ix+20)
	ld h, (ix+21)
	or a
	sbc hl, de
	push af
	ld a, l
	jp p, +
	neg
+:
	ld h, a
	ld e, (ix+12)
	call _LABEL_967_
	ld e, (ix+10)
	call _LABEL_973_
	ld e, a
	ld d, $00
	pop af
	ld a, e
	jp p, +
	neg
	jr z, +
	dec d
	ld e, a
+:
	pop hl
	add hl, de
	ex de, hl
	ld (ix+18), e
	ld (ix+19), d
	ld a, (ix+6)
	or a
	jp nz, _LABEL_662_
_LABEL_6A9_:
	ld a, (ix+7)
	or a
	jr nz, +
	ld a, (ix+8)
	cpl
	and $0F
	ld (ix+22), a
	jr ++

+:
	res 7, a
	ld hl, $0B6F
	call _LABEL_657_
	call _LABEL_70E_
++:
	bit 6, (ix+0)
	jr nz, _LABEL_6F0_
	ld a, (ix+1)
	and $0F
	ld c, a
	ld b, $00
	ld hl, $0703
	add hl, bc
	ld c, (hl)
	ld a, (ix+18)
	and $0F
	or c
	call _LABEL_8AD_
	ld a, (ix+18)
	and $F0
	or (ix+19)
	rrca
	rrca
	rrca
	rrca
	call _LABEL_8AD_
_LABEL_6F0_:
	ld a, (ix+1)
	and $0F
	ld c, a
	ld b, $00
	ld hl, $0707
	add hl, bc
	ld a, (hl)
	or (ix+22)
	jp _LABEL_8AD_

; Data from 703 to 70A (8 bytes)
.db $80 $A0 $C0 $C0 $90 $B0 $D0 $F0

-:
	ld (ix+14), a
_LABEL_70E_:
	push hl
	ld a, (ix+14)
	srl a
	push af
	ld c, a
	ld b, $00
	add hl, bc
	pop af
	ld a, (hl)
	pop hl
	jr c, ++
	rrca
	rrca
	rrca
	rrca
	or a
	jr z, -
	cp $10
	jr nz, +
	dec (ix+14)
	jr _LABEL_70E_

+:
	cp $20
	jr z, +++
++:
	inc (ix+14)
	or $F0
	add a, (ix+8)
	inc a
	jr c, ++++
+++:
	xor a
++++:
	cpl
	and $0F
	ld (ix+22), a
	ret

-:
	ld (ix+15), a
_LABEL_748_:
	push hl
	ld a, (ix+15)
	srl a
	push af
	ld c, a
	ld b, $00
	add hl, bc
	pop af
	ld a, (hl)
	pop hl
	jr c, +
	rrca
	rrca
	rrca
	rrca
	or a
	jp z, -
	cp $10
	jr nz, +
	dec (ix+15)
	jr _LABEL_748_

; Data from 769 to 76B (3 bytes)
.db $FE $20 $C8

+:
	inc (ix+15)
	cpl
	and $0F
	ld l, a
	ld h, $00
	ex de, hl
	add hl, de
	ld (ix+18), l
	ld (ix+19), h
	ret

_LABEL_77E_:
	ld e, (ix+3)
	ld d, (ix+4)
_LABEL_784_:
	ld a, (de)
	inc de
	cp $E0
	jp nc, _LABEL_812_
	bit 3, (ix+0)
	jr nz, _LABEL_7F1_
	or a
	jp p, ++
	sub $80
	jr z, +
	add a, (ix+5)
+:
	ld hl, $08D5
	ld c, a
	ld b, $00
	add hl, bc
	add hl, bc
	ld a, (hl)
	ld (ix+16), a
	inc hl
	ld a, (hl)
	ld (ix+17), a
	bit 5, (ix+0)
	jr z, _LABEL_80B_
	ld a, (de)
	inc de
	sub $80
	add a, (ix+5)
	ld hl, $08D5
	ld c, a
	ld b, $00
	add hl, bc
	add hl, bc
	ld a, (hl)
	ld (ix+20), a
	inc hl
	ld a, (hl)
	ld (ix+21), a
--:
	ld a, (de)
_LABEL_7CC_:
	inc de
++:
	push de
	ld h, a
	ld e, (ix+2)
	call _LABEL_967_
	pop de
	ld (ix+10), l
	ld (ix+11), h
-:
	xor a
	ld (ix+14), a
	ld (ix+15), a
	ld (ix+3), e
	ld (ix+4), d
	xor a
	ld (ix+12), a
	ld (ix+13), a
	ret

_LABEL_7F1_:
	ld (ix+17), a
	ld a, (de)
	inc de
	ld (ix+16), a
	bit 5, (ix+0)
	jr z, --
	ld a, (de)
	inc de
	ld (ix+21), a
	ld a, (de)
	inc de
	ld (ix+20), a
	jr --

_LABEL_80B_:
	ld a, (de)
	or a
	jp p, _LABEL_7CC_
	jr -

_LABEL_812_:
	ld hl, $0825
	push hl
	and $1F
	ld hl, $0829
	ld c, a
	ld b, $00
	add hl, bc
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	jp (hl)

_LABEL_825_:
	inc de
	jp _LABEL_784_

; Jump Table from 829 to 83A (9 entries, indexed by unknown)
.dw _LABEL_88A_ _LABEL_88F_ _LABEL_867_ _LABEL_87E_ _LABEL_83B_ _LABEL_849_ _LABEL_84F_ _LABEL_883_
.dw _LABEL_841_

; 5th entry of Jump Table from 829 (indexed by unknown)
_LABEL_83B_:
	ex de, hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	dec de
	ret

; 9th entry of Jump Table from 829 (indexed by unknown)
_LABEL_841_:
	ld a, (de)
	add a, (ix+5)
	ld (ix+5), a
	ret

; 6th entry of Jump Table from 829 (indexed by unknown)
_LABEL_849_:
	res 5, (ix+0)
	dec de
	ret

; 7th entry of Jump Table from 829 (indexed by unknown)
_LABEL_84F_:
	ld a, (de)
	inc de
	add a, $17
	ld c, a
	ld b, $00
	push ix
	pop hl
	add hl, bc
	ld a, (hl)
	or a
	jr nz, +
	ld a, (de)
	ld (hl), a
+:
	inc de
	dec (hl)
	jp nz, _LABEL_83B_
	inc de
	ret

; 3rd entry of Jump Table from 829 (indexed by unknown)
_LABEL_867_:
	ld a, (de)
	or $E0
	push af
	call _LABEL_8AD_
	pop af
	or $FC
	inc a
	jr nz, +
	res 6, (ix+0)
	ret

+:
	set 6, (ix+0)
	ret

; 4th entry of Jump Table from 829 (indexed by unknown)
_LABEL_87E_:
	ld a, (de)
	ld (ix+7), a
	ret

; 8th entry of Jump Table from 829 (indexed by unknown)
_LABEL_883_:
	ld a, $80
	ld ($CF02), a
	dec de
	ret

; 1st entry of Jump Table from 829 (indexed by unknown)
_LABEL_88A_:
	ld a, (de)
	ld (ix+8), a
	ret

; 2nd entry of Jump Table from 829 (indexed by unknown)
_LABEL_88F_:
	ld a, $80
	ld ($CF01), a
	xor a
	ld (ix+0), a
	call _LABEL_89E_
	pop hl
	pop hl
	ret

_LABEL_89E_:
	ld a, (ix+1)
	and $0F
	ld c, a
	ld b, $00
	ld hl, $0707
	add hl, bc
	ld a, (hl)
	or $0F
_LABEL_8AD_:
	bit 2, (ix+0)
	ret nz
	out ($7F), a
	ret

_LABEL_8B5_:
	exx
	ld hl, $CF01
	ld de, $CF02
	ld bc, $007F
	ld (hl), $00
	ldir
	exx
	exx
	ld hl, $08D1
	ld c, $7F
	ld b, $04
	otir
	xor a
	exx
	ret

; Data from 8D1 to 966 (150 bytes)
.db $9F $BF $DF $FF $00 $00 $FF $03 $C7 $03 $90 $03 $5D $03 $2D $03
.db $FF $02 $D4 $02 $AB $02 $85 $02 $61 $02 $3F $02 $1E $02 $00 $02
.db $E3 $01 $C8 $01 $AF $01 $96 $01 $80 $01 $6A $01 $56 $01 $43 $01
.db $30 $01 $1F $01 $0F $01 $00 $01 $F2 $00 $E4 $00 $D7 $00 $CB $00
.db $C0 $00 $B5 $00 $AB $00 $A1 $00 $98 $00 $90 $00 $88 $00 $80 $00
.db $79 $00 $72 $00 $6C $00 $66 $00 $60 $00 $5B $00 $55 $00 $51 $00
.db $4C $00 $48 $00 $44 $00 $40 $00 $3C $00 $39 $00 $36 $00 $33 $00
.db $30 $00 $2D $00 $2B $00 $28 $00 $26 $00 $24 $00 $22 $00 $20 $00
.db $1E $00 $1C $00 $1B $00 $19 $00 $18 $00 $16 $00 $15 $00 $14 $00
.db $13 $00 $12 $00 $11 $00

_LABEL_967_:
	ld d, $00
	ld l, d
	ld b, $08
-:
	add hl, hl
	jr nc, +
	add hl, de
+:
	djnz -
	ret

_LABEL_973_:
	ld b, $08
-:
	adc hl, hl
	ld a, h
	jr c, +
	cp e
	jr c, ++
+:
	sub e
	ld h, a
	or a
++:
	djnz -
	ld a, l
	rla
	cpl
	ret

; Data from 986 to B8F (522 bytes)
.db $04 $A0 $20 $05 $C7 $09 $05 $01 $04 $0D $A0 $21 $05 $BB $09 $05
.db $01 $04 $0D $A0 $22 $05 $D2 $09 $05 $01 $04 $0D $80 $23 $05 $AB
.db $09 $00 $00 $00 $00 $80 $10 $E2 $04 $E3 $01 $E0 $0D $8D $08 $E3
.db $02 $E0 $0F $1C $E1 $81 $A5 $08 $E5 $80 $E7 $8D $81 $10 $80 $0C
.db $E1 $85 $A9 $08 $E5 $80 $8A $8A $10 $80 $08 $E1 $88 $AC $08 $E5
.db $80 $86 $86 $10 $80 $08 $E1 $04 $80 $20 $05 $02 $0A $00 $01 $02
.db $0E $80 $21 $05 $14 $0A $00 $01 $02 $0F $80 $22 $05 $27 $0A $00
.db $01 $02 $0E $80 $23 $05 $3A $0A $00 $00 $00 $00 $8F $02 $02 $02
.db $E3 $04 $0F $E3 $02 $8D $03 $03 $8F $E3 $04 $91 $18 $E1 $9F $02
.db $02 $02 $E3 $04 $A0 $0F $E3 $02 $9D $03 $03 $9F $E3 $04 $A1 $18
.db $E1 $A2 $02 $02 $02 $E3 $04 $A4 $0F $E3 $02 $A0 $03 $03 $A2 $E3
.db $04 $A4 $18 $E1 $E2 $04 $E3 $01 $E0 $0D $8D $02 $02 $02 $E3 $02
.db $E0 $0F $0F $E3 $01 $E0 $0D $03 $03 $03 $E3 $02 $E0 $0F $18 $E1
.db $04 $80 $20 $02 $7B $0A $00 $01 $03 $0B $80 $21 $02 $7D $0A $00
.db $01 $03 $0C $80 $22 $02 $A2 $0A $00 $01 $03 $0B $80 $23 $02 $C7
.db $0A $00 $00 $00 $00 $80 $01 $99 $08 $A0 $9D $A0 $9E $9B $9D $9B
.db $99 $9B $9D $9B $94 $96 $94 $80 $99 $08 $A0 $9D $A0 $9E $9B $9D
.db $9B $9D $9B $99 $98 $99 $99 $99 $80 $E4 $7D $0A $8D $08 $99 $8D
.db $99 $92 $9E $94 $A0 $8D $94 $8D $99 $88 $8F $88 $94 $8D $08 $99
.db $8D $99 $92 $9E $94 $A0 $8D $94 $88 $94 $8D $99 $8D $80 $E4 $A2
.db $0A $E2 $04 $E3 $01 $E0 $0D $8D $08 $E3 $02 $E0 $0F $08 $E3 $01
.db $E0 $0D $04 $04 $E3 $02 $E0 $0F $08 $E4 $C9 $0A $04 $80 $20 $04
.db $07 $0B $00 $01 $02 $0E $80 $21 $04 $13 $0B $00 $01 $02 $0D $80
.db $22 $04 $1F $0B $00 $01 $02 $0E $80 $23 $04 $2B $0B $00 $00 $00
.db $00 $9D $02 $9B $9D $A0 $9D $A0 $E3 $04 $A5 $10 $E1 $99 $02 $98
.db $99 $9D $99 $9D $E3 $04 $A2 $10 $E1 $8D $02 $02 $02 $88 $88 $88
.db $E3 $04 $91 $10 $E1 $E2 $04 $E3 $01 $E0 $0D $8D $04 $E3 $02 $E0
.db $0F $04 $E3 $01 $E0 $0D $02 $02 $E3 $02 $E0 $0F $10 $E1 $AF $EC
.db $EE $01 $BC $DD $EE $FF $EE $FF $DD $CC $BB $DD $EE $FF $EE $DD
.db $CC $BB $CC $DD $EE $FF $FE $DC $BC $DE $FE $DB $AB $CD $EF $ED
.db $CB $AB $CD $EF $EE $01 $FF $EE $00 $77 $0B $79 $0B $80 $0B $44
.db $0B $FF $02 $FF $ED $DC $CB $BA $A9 $02 $EF $FF $FF $EE $02 $48
.db $0B $86 $09 $DD $09 $56 $0A $E2 $0A $FF

_LABEL_B90_:
	ld a, $FF
	ld ($C20B), a
	ld hl, $0D93
	ld de, $C230
	ld bc, $0004
	ldir
	ld hl, $C9C2
	ld b, $1E
-:
	ld (hl), $03
	inc hl
	inc hl
	djnz -
	di
	ld hl, $0D9E
	ld de, $7964
	ld b, $0A
	rst $10	; _LABEL_10_
_LABEL_BB5_:
	di
	ld hl, $C230
	ld de, $796A
	call _LABEL_D76_
	ei
	ld a, $10
	call _LABEL_426_
	ld ix, $C400
	call _LABEL_F27_
	ld (ix+7), $08
	ld (ix+12), $0D
	ld (ix+4), $04
	ld ix, $C440
	call _LABEL_F27_
	ld ix, $C420
	call _LABEL_F27_
	call _LABEL_CC1_
	ld a, $01
	ld ($C400), a
	inc a
	ld ($C440), a
	inc a
	ld ($C420), a
	call _LABEL_E99_
	ld bc, $1401
	call _LABEL_CBA_
	ld a, $83
	ld ($CF00), a
-:
	ld a, $11
	call _LABEL_426_
	call _LABEL_E99_
	ld hl, $C234
	ld a, (hl)
	or a
	jr nz, +
	inc hl
	ld a, (hl)
	or a
	jr z, -
	di
	ld hl, $0D97
	ld de, $79DA
	ld b, $07
	rst $10	; _LABEL_10_
	ei
	ld b, $00
--:
	push bc
	ld bc, $0180
	call _LABEL_CBA_
	ld de, $79DB
	ld hl, $C274
	inc (hl)
	di
	rst $20	; _LABEL_20_
	ld b, $07
-:
	ld a, ($C274)
	and $08
	out ($BE), a
	push af
	pop af
	in a, ($BE)
	djnz -
	ei
	pop bc
	djnz --
	jp _LABEL_377_

+:
	ld (hl), $00
	ld a, $84
	ld ($CF00), a
	ld bc, $6001
	call _LABEL_CBA_
	ld a, $D0
	ld ($C300), a
	ld bc, $3001
	call _LABEL_CBA_
	ld hl, $C230
	ld a, (hl)
	add a, $01
	daa
	ld (hl), a
	inc hl
	inc (hl)
	ld a, (hl)
	cp $0D
	jr nc, ++
	ld e, a
	ld d, $00
	ld hl, $0DA6
	add hl, de
	ld a, (hl)
	ld hl, ($C232)
	add a, h
	daa
	jr nc, +
	ld a, $99
+:
	ld h, a
	ld l, d
	ld ($C232), hl
	jp _LABEL_BB5_

++:
	ld a, $82
	ld ($CF00), a
	ld hl, $0CAA
	ld de, $79D0
	ld b, $10
	di
	rst $10	; _LABEL_10_
	ei
	ld bc, $0080
	call _LABEL_CBA_
	ld bc, $0080
	call _LABEL_CBA_
	jp _LABEL_377_

; Data from CAA to CB9 (16 bytes)
.db $43 $4F $4E $47 $52 $41 $54 $55 $4C $41 $54 $49 $4F $4E $53 $21

_LABEL_CBA_:
	ld a, c
	call _LABEL_426_
	djnz _LABEL_CBA_
	ret

_LABEL_CC1_:
	ld a, ($C231)
	dec a
	ld d, a
	add a, a
	add a, d
	ld e, a
	xor a
	rr d
	rra
	add a, e
	ld e, a
	ld hl, $19B1
	add hl, de
	ld a, (hl)
	ld ($C408), a
	inc hl
	ld a, (hl)
	ld ($C448), a
	inc hl
	ld a, (hl)
	ld ($C447), a
	inc hl
	ld de, $CA00
	ld b, $80
--:
	push bc
	ld b, $04
	ld c, (hl)
-:
	ld a, c
	inc a
	and $03
	add a, $04
	ld (de), a
	inc de
	xor a
	ld (de), a
	inc de
	rr c
	rr c
	djnz -
	inc hl
	pop bc
	djnz --
	ld hl, $CA3E
	ld bc, $1074
	ld de, $0040
-:
	ld (hl), c
	add hl, de
	djnz -
	ld a, $FF
	ld ($C244), a
	ld de, $8036
	rst $20	; _LABEL_20_
-:
	ld a, $08
	call _LABEL_426_
	ld a, ($C244)
	or a
	jr nz, -
	ld de, $8006
	rst $20	; _LABEL_20_
	ld de, $8AFF
	rst $20	; _LABEL_20_
	ret

_LABEL_D2A_:
	ld de, $8800
	rst $20	; _LABEL_20_
	ld de, $8A37
	rst $20	; _LABEL_20_
	ld hl, $C225
	ld a, (hl)
	dec (hl)
	jr nz, +
	xor a
	ld ($C244), a
	ret

+:
	neg
	ld e, a
	and $07
	ret nz
	ld a, e
	rrca
	rrca
	add a, $C0
	ld e, a
	ld d, $79
	ld l, a
	ld h, $C9
	ld b, $11
-:
	push bc
	ld a, (hl)
	rst $08	; _LABEL_8_
	ld bc, $0040
	add hl, bc
	ex de, hl
	add hl, bc
	ex de, hl
	pop bc
	djnz -
	ret

_LABEL_D5F_:
	ld hl, $C232
	dec (hl)
	ret p
	ld (hl), $3B
	inc hl
	ld a, (hl)
	sub $01
	daa
	ld (hl), a
	ld de, $797A
	jr nz, _LABEL_D76_
	dec a
	ld ($C235), a
	inc a
_LABEL_D76_:
	rst $20	; _LABEL_20_
	ld a, (hl)
	rra
	rra
	rra
	rra
	and $0F
	jr z, +
	or $30
+:
	call _LABEL_D8A_
	ld a, (hl)
	and $0F
	or $30
_LABEL_D8A_:
	out ($BE), a
	push af
	pop af
	ld a, $08
	out ($BE), a
	ret

; Data from D93 to DB0 (30 bytes)
.db $01 $01 $00 $61 $54 $49 $4D $45 $20 $55 $50 $52 $44 $20 $20 $20
.db $20 $54 $49 $4D $45 $30 $35 $25 $35 $35 $30 $35 $35 $35

; Jump Table from DB1 to DB8 (4 entries, indexed by $C420)
.dw $1510 _LABEL_DB9_ _LABEL_E43_ _LABEL_E6F_

; 2nd entry of Jump Table from DB1 (indexed by $C420)
_LABEL_DB9_:
	bit 7, (ix+1)
	jr z, ++
	dec (ix+31)
	jr z, +
	ld hl, ($C405)
	jp _LABEL_F85_

+:
	res 7, (ix+1)
	ld hl, $0000
	ld ($C409), hl
++:
	call _LABEL_E2C_
	ld a, ($C207)
	rrca
	call c, _LABEL_DF3_
	rrca
	call c, _LABEL_E05_
	rrca
	jr c, ++
	rrca
	ret nc
	bit 1, (hl)
	ret nz
	ld (ix+9), $01
	ld hl, $0E8D
	jr _LABEL_E10_

_LABEL_DF3_:
	push hl
	ld de, $FFC0
	add hl, de
	bit 0, (hl)
	pop hl
	ret nz
	ld (ix+10), $FF
	ld hl, $0E96
	jr +

_LABEL_E05_:
	bit 0, (hl)
	ret nz
	ld (ix+10), $01
	ld hl, $0E93
+:
	pop af
_LABEL_E10_:
	ld ($C405), hl
	ld (ix+31), $08
	set 7, (ix+1)
	jp _LABEL_F85_

++:
	dec hl
	dec hl
	bit 1, (hl)
	ret nz
	ld (ix+9), $FF
	ld hl, $0E90
	jr _LABEL_E10_

_LABEL_E2C_:
	ld a, ($C407)
	rrca
	rrca
	and $3E
	ld e, a
	ld a, ($C408)
	and $F8
	ld l, a
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	ld d, $C8
	add hl, de
	ret

; 3rd entry of Jump Table from DB1 (indexed by $C420)
_LABEL_E43_:
	bit 0, (ix+1)
	jr z, ++
	ld hl, ($C407)
	ld a, l
	cp (ix+7)
	jr nz, +
	ld a, h
	cp (ix+8)
	jr nz, +
	ld ($C234), a
	ret

+:
	ld hl, $0E87
	jp _LABEL_F85_

++:
	ld (ix+12), $0C
-:
	set 0, (ix+1)
	ld (ix+4), $1E
	ret

; 4th entry of Jump Table from DB1 (indexed by $C420)
_LABEL_E6F_:
	bit 0, (ix+1)
	jr z, +
	ld hl, $0E8A
	jp _LABEL_F85_

+:
	ld hl, ($C407)
	ld ($C427), hl
	ld (ix+12), $53
	jr -

; Data from E87 to E98 (18 bytes)
.db $02 $47 $0C $02 $20 $53 $02 $0D $0E $02 $0F $10 $02 $11 $12 $02
.db $13 $14

_LABEL_E99_:
	ld hl, $C300
	ld ($C209), hl
	ld ix, $C400
	ld b, $04
-:
	ld a, (ix+0)
	and $7F
	jp z, ++
	push bc
	ld hl, $0DB1
	call _LABEL_FB1_
	ld a, (ix+0)
	or a
	jp z, +
	call _LABEL_F6D_
	call _LABEL_F1D_
	call _LABEL_ED2_
+:
	pop bc
++:
	ld de, $0020
	add ix, de
	djnz -
	ld hl, ($C209)
	ld (hl), $D0
	ret

_LABEL_ED2_:
	ld a, (ix+0)
	or a
	ret z
	ld a, (ix+8)
	cp $C0
	ret nc
	ld hl, ($C209)
	ld (hl), a
	inc hl
	ex af, af'
	ld a, (ix+12)
	cp $0C
	jr z, +
	ld ($C209), hl
	dec hl
	sla l
	set 7, l
	ld a, (ix+7)
	ld (hl), a
	inc hl
	ld a, (ix+12)
	ld (hl), a
	ret

+:
	ex af, af'
	ld b, $04
	push hl
-:
	sub $08
	ld (hl), a
	inc hl
	djnz -
	ld ($C209), hl
	pop hl
	dec hl
	sla l
	set 7, l
	ld b, $05
	ld c, (ix+7)
	ex af, af'
-:
	ld (hl), c
	inc hl
	ld (hl), a
	inc hl
	dec a
	djnz -
	ret

_LABEL_F1D_:
	ld a, (ix+10)
	add a, (ix+8)
	ld (ix+8), a
	ret

_LABEL_F27_:
	xor a
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

_LABEL_F6D_:
	ld a, ($C228)
	ld d, (ix+9)
	add a, d
	ld d, a
	add a, (ix+7)
	ld (ix+7), a
	bit 7, d
	jr nz, +
	jr c, _LABEL_F27_
	ret

+:
	jr nc, _LABEL_F27_
	ret

_LABEL_F85_:
	ld b, (hl)
	inc hl
	ld d, b
	dec (ix+3)
	ld a, (ix+2)
	jr nz, ++
	ld e, (ix+4)
	ld (ix+3), e
	inc a
	cp d
	jr c, +
	xor a
+:
	ld (ix+2), a
++:
	ld c, a
	ld b, $00
	add hl, bc
	ld a, (hl)
	ld (ix+12), a
	ret

; Data from FA7 to FB0 (10 bytes)
.db $46 $23 $50 $DD $7E $02 $18 $EF $E6 $0F

_LABEL_FB1_:
	add a, a
	ld e, a
	ld d, $00
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	jp (hl)

; Data from FBB to 1FFF (4165 bytes)
.incbin "export1.3_fbb.inc"

