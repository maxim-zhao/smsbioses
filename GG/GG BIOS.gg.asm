.memorymap
slotsize $400
slot 0 $0000
defaultslot 0
.endme
.rombankmap
bankstotal 1
banksize $400
banks 1
.endro

.enum $C000 export
RAM_Start .db
RAM_Port3EValue db
.ende

.enum $C700 export
RAM_HeaderDetected db ; Non-zero if a TMR SEGA ROM header was found
RAM_SMSMode db ; Non-zero if we are in SMS mode
.ende

.enum $C800 export
RAM_BootGameCode dsb 8
.ende

; Ports
.define Port_SerialRaw $02
.define Port_MemoryControl $3E
.define Port_VDPData $BE
.define Port_VDPAddress $BF

; Input Ports
.define Port_VCounter $7E

; Paging registers
.define PAGING_SRAM $fffc
.define PAGING_SLOT_0 $fffd
.define PAGING_SLOT_1 $fffe
.define PAGING_SLOT_2 $ffff

; VDP constants
.define VDP_WRITE_MASK $4000
.define VDP_PALETTE_WRITE_MASK $c000
.define NAME_TABLE_ADDRESS $3800
.define SPRITE_TABLE_ADDRESS $3f00

.bank 0 slot 0
.org 0

.section "Boot" force
Boot:
  di

  ; Reset paging
  ld hl, $0000              ; Disable SRAM, 0:0
  ld (PAGING_SRAM), hl
  ld hl, $0201              ; 1:1, 2:2
  ld (PAGING_SLOT_1), hl
  
  ; Clear RAM (up to $dfef)
  ld hl, RAM_Start
  ld de, RAM_Start + 1
  ld bc, $1fef
  ld (hl), l
  ldir
  
  ; Initialise stack pointer
  ld sp, $dff0
  
  ; SMS mode detection
  ; In GG mode, we can read back what we write to port 2.
  ; In SMS mode, we can't.
  ld a, $f5
  out (Port_SerialRaw), a
  in a, (Port_SerialRaw)
  cp $f5
  jr nz, +
  ld a, $fa
  out (Port_SerialRaw), a
  in a, (Port_SerialRaw)
  cp $fa
  jr nz, +
  xor a
+:ld (RAM_SMSMode), a
  ld a, $ff
  out (Port_SerialRaw), a

  ; Wait for line 176
-:in a, (Port_VCounter)
  cp 176
  jr nz, -
  
  ; Disable sprites
  ld de,VDP_WRITE_MASK | SPRITE_TABLE_ADDRESS
  call SetVRAMAddress
  ld a, 208
  out (Port_VDPData), a
  
  ; Set VDP registers
  ld hl, VDPRegisters
  ld bc, 22 << 8 | Port_VDPAddress
  otir
  
  ; Blank name table
  ld hl, RAM_Start
  ld de, VDP_WRITE_MASK | NAME_TABLE_ADDRESS
  call SetVRAMAddress
  ld c, Port_VDPData
  otir ; b is 0 so this is the whole name table (32*24*2)
  otir
  otir
  otir
  jr + ; We jump past the NMI handler
  nop

NMIHandler:
  retn

+:otir
  otir
  
  ; Blank the palette
  ld hl, RAM_Start
  ld de, VDP_PALETTE_WRITE_MASK | 0
  ld bc, 64 << 8 | Port_VDPData
  call SetVRAMAddress
  otir
  
  ; Then load the palette for this mode
  ld hl, Palette_SMS
  ld b, 4
  ld a, (RAM_SMSMode)
  or a
  jr nz, +
  ld hl, Palette_GG
+:call SetVRAMAddress
  otir
  
  ; Load the tiles
  ld hl, Tiles_SMS
  ld a, (RAM_SMSMode)
  or a
  jr nz, +
  ld hl, Tiles_GG
+:ld de, VDP_WRITE_MASK | 0
  call DecompressTiles
  
  ; Check the ROM
  call CheckROMHeader
  ld a, (RAM_HeaderDetected)
  or a
-:jr z, - ; Lock up if nothing was found

  ld hl, NameTable_SMS
  ld a, (RAM_SMSMode)
  or a
  jr nz, +
  ld hl, NameTable_GG
+:ld de, VDP_WRITE_MASK | NAME_TABLE_ADDRESS | ((6 + (9 * 32)) * 2) ; 6, 9
  ld b, 21
  call WriteNameTable
  ld de, VDP_WRITE_MASK | NAME_TABLE_ADDRESS | ((6 + (11 * 32)) * 2) ; 6, 11
  ld b, 21
  call WriteNameTable
  ld de, VDP_WRITE_MASK | NAME_TABLE_ADDRESS | ((6 + (13 * 32)) * 2) ; 6, 13
  ld b, 21
  call WriteNameTable
  
  ld de, $81E0 ; Screen on
  call WriteVDPRegister
  
  ; Delay
  ld b, 7
  ld hl, 0
-:dec hl
  ld a, l
  or h
  jr nz, -
  djnz -
  
  ld de, $81A0 ; Screen off
  call SetVRAMAddress
  
  ld hl, BootGame
  ld de, RAM_BootGameCode
  ld bc, 8
  ldir
  jp RAM_BootGameCode
.ends

.section "Game boot code" force
BootGame:
  ld a, $A8
  ld (RAM_Port3EValue), a
  out (Port_MemoryControl), a
  rst 0  ; Boot game
.ends

.section "Header checks" force
CheckROMHeader:
  call _32KB
  call _16KB
  call _8KB
  ret

_8KB:
  ld de, $1ff0
  jr +

_16KB:
  ld de, $3ff0
  jr +

_32KB:
  ld de, $7ff0
+:ld hl, TMRSega
  ld b, 8
-:ld a, (de) ; Check for TMR SEGA in the ROM
  cp (hl)
  ret nz
  inc hl
  inc de
  djnz -
  ld (RAM_HeaderDetected), a
  pop af ; Discard return address
  ret
.ends

.section "Set VRAM address" force
SetVRAMAddress:
WriteVDPRegister:
  ld a, e
  out (Port_VDPAddress), a
  ld a, d
  out (Port_VDPAddress), a
  ret
.ends

.section "Tile decompressor" force
DecompressTiles:
; This is the "Phantasy Star RLE" decompressor (although a more efficient variant than the "original")
  ld b, 4
---:
  push bc
    push de
--:   ld a, (hl)            ; Get byte
      inc hl
      or a
      jr z, ++              ; 0 = end of bitplane
      res 7, a              ; Clear RLE bit
      ld b, a               ; Save counter
-:    call SetVRAMAddress
      ld a, (hl)            ; Emit byte of data
      out (Port_VDPData), a
      jp p, +               ; If it was RLE, don't increment the read pointer
      inc hl
+:    inc de                ; Move to next pixel for this bitplane
      inc de
      inc de
      inc de
      djnz -                ; Loop over counter
      jp m, --
      inc hl
      jr --
++: pop de
    inc de                  ; Next bitplane
  pop bc
  djnz ---
  ret
.ends

.section "Name table writer" force
WriteNameTable:
  call SetVRAMAddress
  xor a
-:ld c, Port_VDPData
  outi
  out (c), a
  jr nz, -
  ret
.ends

.section "Header check data" force
TMRSega:
.db "TMR SEGA"
.ends

.section "Graphics data" force
Palette_GG:
.dw $0f00, $0eee            ; Bright blue, off-white

Palette_SMS:
.db $30 $3F                 ; Bright blue, white

Tiles_GG:
.db $09 $00 $81 $0F $03 $0C $95 $0F $0C $0C $00 $8F $CC $CC $CF $8C
.db $0C $0C $00 $87 $CC $CC $8C $CC $CC $C7 $00 $8F $05 $CC $83 $8F
.db $00 $8C $05 $CC $83 $87 $00 $C7 $05 $CC $8B $87 $00 $8F $CC $CC
.db $0F $CC $CC $8F $00 $9F $05 $19 $83 $9F $00 $01 $05 $81 $8A $01
.db $00 $F1 $99 $99 $F0 $98 $98 $F0 $00 $03 $98 $81 $F0 $03 $60 $82
.db $00 $0F $05 $19 $8E $0F $00 $1F $99 $99 $9F $99 $99 $19 $00 $00
.db $80 $80 $00 $03 $80 $81 $00 $06 $CC $8B $78 $00 $8C $CC $EC $FC
.db $FC $DC $CC $00 $F8 $05 $CC $8E $F8 $00 $F9 $C1 $C1 $F1 $C1 $C1
.db $F9 $00 $F0 $98 $98 $F0 $03 $98 $81 $00 $06 $18 $83 $1F $00 $31
.db $05 $33 $AE $31 $00 $E3 $33 $33 $03 $33 $33 $E3 $00 $E4 $06 $07
.db $C7 $06 $06 $E6 $00 $63 $66 $66 $E3 $E0 $66 $23 $00 $C7 $66 $06
.db $C7 $66 $66 $C7 $00 $C0 $00 $00 $80 $00 $00 $C0 $00 $F9 $C1 $C1
.db $F1 $03 $C1 $8A $00 $F0 $99 $99 $F1 $99 $99 $98 $00 $F1 $05 $99
.db $BB $F1 $00 $04 $8C $DC $FC $AC $8C $8C $00 $78 $CC $C0 $78 $0C
.db $CC $78 $00 $F8 $C1 $C1 $F1 $C1 $C1 $F8 $00 $F0 $98 $98 $80 $B9
.db $99 $F9 $00 $60 $60 $F0 $90 $F8 $98 $98 $00 $1F $18 $18 $1E $18
.db $18 $1F $00 $23 $33 $3B $3F $37 $33 $31 $00 $3F $06 $0C $8D $00
.db $3E $30 $30 $3C $30 $30 $3E $00 $7C $66 $66 $7C $03 $66 $82 $00
.db $7C $03 $66 $9D $7C $60 $60 $00 $63 $66 $66 $63 $60 $66 $63 $00
.db $C7 $0C $0C $87 $00 $0C $C7 $00 $80 $C0 $00 $80 $C0 $C0 $80 $00
.db $C1 $05 $C0 $83 $FC $00 $F9 $06 $61 $82 $00 $F0 $05 $98 $81 $F0
.db $06 $00 $02 $C0 $00 $7F $00 $7F $00 $7F $00 $03 $00 $00 $7F $00
.db $7F $00 $7F $00 $03 $00 $00 $7F $00 $7F $00 $7F $00 $03 $00 $00

Tiles_SMS:
.db $08 $00 $81 $7C $03 $66 $81 $7C $03 $60 $81 $7C $03 $66 $81 $7C
.db $03 $66 $81 $3C $06 $66 $82 $3C $7C $06 $66 $81 $7C $07 $66 $02
.db $3C $02 $66 $02 $60 $02 $66 $85 $3C $7E $60 $60 $7C $03 $60 $85
.db $7E $7C $66 $66 $7C $03 $66 $81 $7C $04 $66 $81 $3C $03 $18 $88
.db $46 $66 $76 $7E $7E $6E $66 $62 $07 $60 $82 $7E $3C $06 $18 $02
.db $3C $8B $66 $60 $7C $3E $06 $66 $3C $7E $60 $60 $7C $04 $60 $85
.db $41 $63 $77 $7F $6B $03 $63 $8D $3C $66 $66 $60 $6E $66 $66 $3E
.db $3C $24 $66 $66 $7E $03 $66 $81 $7E $07 $18 $06 $00 $02 $18 $00
.db $7F $00 $21 $00 $00 $7F $00 $21 $00 $00 $7F $00 $21 $00 $00

NameTable_GG:
.db $00 $00 $00 $01 $02 $03 $04 $05 $06 $07 $08 $09 $0A $0B $0C $0D $0E $00 $00 $00 $00
.db $00 $00 $0F $10 $11 $12 $13 $14 $15 $16 $17 $18 $19 $1A $1B $1C $1D $1E $00 $00 $00
.db $00 $1F $20 $21 $22 $23 $24 $25 $26 $27 $28 $27 $29 $19 $2A $2B $2C $2D $2E $2F $00

NameTable_SMS:
.db $00 $00 $00 $01 $02 $03 $04 $05 $06 $07 $04 $00 $08 $09 $00 $03 $02 $00 $00 $00 $00 
.db $00 $05 $0A $04 $07 $02 $00 $0B $0C $06 $07 $0A $0D $07 $00 $0E $02 $03 $0F $00 $00 
.db $0D $07 $10 $11 $00 $07 $0A $12 $07 $02 $01 $02 $0C $0D $07 $0D $00 $0B $12 $04 $13
.ends

.section "VDP register data" force
VDPRegisters:
.db $16 $80
.db $A0 $81
.db $FF $82
.db $FF $83
.db $FF $84
.db $FF $85
.db $FF $86
.db $00 $87
.db $00 $88
.db $00 $89
.db $FF $8A
.ends

.db $FF ; unused byte
