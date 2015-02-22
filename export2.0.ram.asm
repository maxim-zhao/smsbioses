  in a,($dc)
  ld hl,$c74d
  ld b,$03
  ld a,$eb
  out ($3e),a
  ld a,(hl)
  ld ($c000),a
  out ($3e),a
  exx
  call $c726
  exx
  inc hl
  djnz $c707
  xor a
  ld ($c000),a
  ld a,$eb
  out ($3e),a
  ld a,$e3
  out ($3e),a
  ret
  ld hl,$0000
  ld bc,$0201
  ld de,$c9c0
  ldir
  ld a,($c9c0)
  ld hl,$c9c1
  ld bc,$0200
  cpi
  jr nz,$c742
  jp po,$c73a
  ret
  pop af
  jr $c71d
  ld d,h
  ld c,l
  ld d,d
  jr nz,$c79d
  ld b,l
  ld b,a
  ld b,c
  res 5,e
  ld l,e
  ld de,$0000
  ld a,($c26f)
  ld c,a
  and $f0
  cp $40
  jr z,$c765
  ld hl,($c270)
  inc hl
  ld ($c26a),hl
  ret
  ld a,c
  sub $0a
  and $0f
  push af
  ld hl,$c7b3
  ld c,a
  ld b,$00
  add hl,bc
  ld b,(hl)
  ld c,$f0
  ld hl,$0000
  call $c7a5
  ld ($c270),de
  pop af
  sub $04
  ret c
  ld c,a
  ld b,$00
  ld hl,$c7bc
  add hl,bc
  ld b,(hl)
  ld a,$02
  push bc
  ld ($ffff),a
  inc a
  push af
  call $c79f
  pop af
  pop bc
  djnz $c78d
  ld ($c270),de
  ret
  ld bc,$4000
  ld hl,$8000
  ld a,e
  add a,(hl)
  ld e,a
  ld a,d
  adc a,$00
  ld d,a
  inc hl
  dec bc
  ld a,b
  or c
  jr nz,$c7a5
  ret
  rra
  ccf
  ld a,a
  cp a
  ld a,a
  ld a,a
  ld a,a
  ld a,a
  ld a,a
  ld (bc),a
  ld b,$0e
  ld e,$21
  xor h
  inc bc
  ld ($c203),hl
  ld a,$03
  ei
  call $042e
  di
  ld a,$eb
  out ($3e),a
  ld a,($c000)
  out ($3e),a
  jp $0000
