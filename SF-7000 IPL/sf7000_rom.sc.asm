.memorymap
defaultslot 0
slotsize 8*1024 ; IPL
slot 0 $0000
slotsize 64*1024 ; RAM
slot 1 $0000
.endme

.rombankmap
bankstotal 1
banksize 8192
banks 1
.endro

.bank 0 slot 0

; RAM variables
.define disk_copy_buffer_size 40*1024 ; 40KB
.enum $4000 export
disk_copy_buffer               dsb disk_copy_buffer_size ; 40KB up to $dfff
.ende

.define self_test_result_passed $55

.enum $fc00 export
fdc_command_loop_counter       db     ; $fc00 ; used for counting retries on fdc comands
fdc_command_loop_counter_2     db     ; $fc01 ; used for counting inner loops on fdc comands
fdc_command_data_buffer        dsb 9  ; $fc02 ; 9 bytes .. $fc0a
fdc_command_result_buffer      dsb 7  ; $fc0b ; 0-7 bytes .. $fc11 depending on command
fdc_format_buffer              dsb 64 ; $fc12 ; 64 bytes .. $fc51
self_test_result               db     ; $fc52
cursor_x                       db     ; $fc53
cursor_y                       db     ; $fc54
write_char_value               db     ; $fc55 ; char being written to the screen
keyboard_input_buffer          dsb 9  ; $fc56 ;  9 bytes .. $fc5e - first 7 bytes are low bytes of rasters, 8th is high bits, 9th is remaining control keys
keyboard_input_buffer_copy     dsb 8  ; $fc5f ;  8 bytes .. $fc66 - copy of above for comparison - missing last byte
scrolling_buffer               dsb 40 ; $fc67 ; 40 bytes .. $fc8e
read_line_buffer               dsb 33 ; $fc8f ; 33 bytes .. $fcaf
unused                         dsb 7
at_key_held                    db     ; $fcb7 ; 1 if @ key was pressed last time
unused_2                       dsb 584
; stack goes here
top_of_stack                   .db
ram_code_area                  dsb 256 ; $ff00-$ffff ; anything running from ram is here (until the disk boot loader takes over)
.ende

.define disk_boot_loader ram_code_area+$20

; system stuff
.define tilemap_address                $0800
.define vram_size                      $4000
.define ipl_rom_size                   $2000
.define ipl_rom_slot_size              $4000
.define vdp_data                       $be
.define vdp_control                    $bf
.define psg                            $7f
.define sc_ppi_a                       $dc
.define sc_ppi_b                       $dd
.define sc_ppi_c                       $de
.define sc_ppi_control                 $df
.define sf7_ppi_a                      $e4       ; (FDC, printer) status (input)
.define sf7_ppi_b                      $e5       ; Printer port output - unused
.define sf7_ppi_c                      $e6       ; (FDD, FDC, ROM mapping, printer) control (output)
.define sf7_ppi_control                $e7
.define fdc_status                     $e0
.define fdc_data                       $e1

; ASCII
.define BS $08
.define FF $0c
.define CR $0d

; FDC stuff
; FDC status byte meaning
;   bit  meaning
;    7   RQM = Request for Master = ready to send/recieve data
;    6   DIO = Data Input/Output  = 1 when CPU should read, 0 when it should write
;    5   EXM = Execution Mode     = 1 during "execution phase", 0 during "results phase"
;    4   CB  = Controller Busy    = set when controller is busy and can't accept anything
;   3-0  DnB = Drive n Busy       = set when a particular drive is busy
;
; FDC status registers
;  These 4 bytes contain extended status info after an operation. Some are returned after some commands.
;  bit  meaning
;  ST0:
;  7-6  Interrupt Code    00 = OK, 01 = abnormal termination, 10 = invalid command, 11 = FDD changed state and invalidated command
;   5   Seek End          set after seek
;   4   Equipment Check   FDD failure signal or recalibration failed
;   3   Not Ready         FDD not ready, or side unavailable
;   2   Head Address      Side of disk when interrupt happened
;  1-0  Unit Select       Drive number when interrupt happened
.define fdc_status_0_ok_mask %11000000 ; mask for bits 7 and 6, zero if OK
;  ST1:
;   7   End of Cylinder   Sector number too large
;   6                     Unused
;   5   Data Error        CRC error reading disk
;   4   Overrun           FDC not serviced fast enough
;   3                     Unused
;   2   No Data           Can't find sector (read/write data/deleted data), or error reading ID, or error finding start sector (read diagnostic)
;   1   Not Writable      FDD says disk is write protected
;   0   Missing Address Mark Low level format stuff
;  ST2:
;   7                     Unused
;   6   Control Mark      Low level format stuff
;   5   Data Error in Data Field CRC error in data section
;   4   Wrong Cylinder    Low level format stuff
;   3   Scan Equal Hit    During scan, equal condition satisfied
;   2   Scan Not Satisfied During scan, condition not met
;   1   Bad Cylinder      Low level format stuff
;   0   Missing Address Mark in Data Field Low level format stuff?
;  ST3:
;   7   Fault             Copy of FDD signal
;   6   Write Protected   Copy of FDD signal
;   5   Ready             Copy of FDD signal
;   4   Track 0           Copy of FDD signal
;   3   Two Side          Copy of FDD signal
;   2   Head Address      Copy of FDD signal
;  1-0  Unit Select       Copy of FDD signal
;
; FDC commands:
; Modifiers for multitrack, MFM, skip deleted - only apply to some commands (see xyz below, respectively)
.define fdc_command_modifier_multitrack   %10000000
.define fdc_command_modifier_mfm          %01000000
.define fdc_command_modifier_skip_deleted %00100000

.define fdc_command_read_diagnostic $02
; $?2 = %0yz00010 %-----sdd $tt $hh $rr $nn $ee $gg $ll
;     "read diagnostic"
.define fdc_command_specify $03
; $03 = %00000011 %ssssuuuu %llllllln
;     "specify" - set up some parameters
;     s = Step Rate Time (multiple of 16ms)
;     u = Head Unload Time (multiple of 16ms)
;     l = Head Load Time (multiple of 2ms)
;     n = Non-DMA Mode flag
;     Results: none
.define fdc_command_sense $04
; $04 = %00001000 %-----sdd
;     "sense drive status"
;     s = side
;     d = drive
;     Results: ST3
.define fdc_command_write $05
; $?5 = %xy000110 %-----sdd $tt $hh $rr $nn $ee $gg $ll
;     write data:
;       x  = multitrack mode
;       y  = MFM mode
;       dd = drive
;       s  = side
;       tt = track
;       hh = head address
;       rr = sector
;       nn = byte count
;       ee = final sector number
;       gg = gap length
;       ll = data length ll (if nn = 0)
;     for SF-7000, xy = 01, dd = 0, s = 0, hh = 0, ee = 16, gg = 14, ll = don't care if nn > 0
;     Results: ST0, ST1, ST2, ( $tt, $hh, $rr, $nn ) after operation
.define fdc_command_read $06
; $?6 = %xyz00110 %-----sdd $tt $hh $rr $nn $ee $gg $ll
;     read data:
;       x  = multitrack mode
;       y  = MFM mode
;       z  = skip deleted data address mark
;       dd = drive
;       s  = side
;       tt = track
;       hh = head address
;       rr = sector
;       nn = byte count
;       ee = final sector number
;       gg = gap length
;       ll = data length ll (if nn = 0)
;     for SF-7000, xyz = 010, dd = 0, s = 0, hh = 0, ee = 16, gg = 14, ll = don't care if nn > 0
;     Results: ST0, ST1, ST2, ( $tt, $hh, $rr, $nn ) after operation
.define fdc_command_recalibrate $07
; $07 = %00000111 %------dd
;     recalibrate drive dd, seek to track 0
.define fdc_command_sense_interrupt $08
; $08 = %00001000
;     "sense interrupt status"
;     Returns: ST0, $tt = current track number
.define fdc_command_write_deleted $09
; $?9 $?? $?? $?? $?? $?? $?? $?? $??
;     "write deleted data"
.define fdc_command_read_id $0a
; $?a = %0y001010 %-----sdd
;     "read sector id"
;     reads the first "correct ID information" on the current track
;     Returns: ST0, ST1, ST2, tt, hh, rr, nn
.define fdc_command_read_deleted $0c
; $?c = %xyz01100 $-----sdd $tt $hh $rr $nn $ee $gg $ll
;     "read deleted data"
;       x  = multitrack mode
;       y  = MFM mode
;       z  = skip deleted data address mark
;       dd = drive
;       s  = side
;       tt = track
;       hh = head address
;       rr = sector
;       nn = byte count
;       ee = final sector number
;       gg = gap length
;       ll = data length ll (if nn = 0)
;     for SF-7000, xyz = 010, dd = 0, s = 0, hh = 0, ee = 16, gg = 14, ll = don't care if nn > 0
;     Results: ST0, ST1, ST2, ( $tt, $hh, $rr, $nn ) after operation
.define fdc_command_format $0d
; $?d = %0y001101 %-----sdd $nn $tt $gg $bb
;     "format a track"
;       y  = use MFM mode
;       s  = side
;       dd = drive
;       nn = bytes per sector
;       tt = sectors per track
;       gg = gap 3 length
;       bb = filler byte
.define fdc_command_seek $0f
; $0f = %-----sdd $tt
;     seek drive dd, side s, track tt
.define fdc_command_scan_equal $11
; $11 = %xyz10001 $-----sdd $tt $hh $rr $nn $ee $gg $ll
;     "scan equal" - "data compared between the FDD and the host system"
.define fdc_command_scan_low_equal $19
; $19 = %xyz11001 $-----sdd $tt $hh $rr $nn $ee $gg $ll
;     "scan low or equal"
.define fdc_command_scan_high_equal $1e
; $1e = %xyz11101 $-----sdd $tt $hh $rr $nn $ee $gg $ll
;     "scan high or equal"

; PPI stuff
; PPI control has 2 modes:
;   if bit 7 is set, it controls input/output and overall mode
;     bit  meaning (bit 7 set)
;     6,5  port A+C(high) mode selection
;      4   port A input if set
;      3   port C upper nibble input if set
;      2   port B+C(low) mode selection
;      1   port B mode selection
;      0   port C lower nibble input if set
;   if bit 7 is reset, it sets or resets bits in port C - useful for some modes
;     bit  meaning (bit 7 reset)
;    6,5,4 ignored
;    3,2,1 bit selection (0-7)
;      0   bit value
; SF-7000 PPI port A
;   PA0 = FDC INT: in non-DMA mode, set when there is a byte to output
;   PA1 = printer port BUSY
;   PA2 = FDC INDEX: high when at the start of a track
; SF-7000 PPI port B
;   Printer port output - unused
; SF-7000 PPI port C
;   PC0 = FDD /INUSE
;   PC1 = FDD /MOTOR ON
;   PC2 = FDD TC = terminal count: terminates data transfers when set
;   PC3 = FDC RESET
;   PC4 = ?
;   PC5 = ?
;   PC6 = /ROM SEL = map ROM to lower 4KB when zero - note that writing to ROM will write to RAM.
;   PC7 = /STROBE = printer output

;#######################################
; API entry points
;#######################################
; These are padded with $00 whereas the rest of the ROM is filled with $ff.
; It's simpler to block-fill the end than these gaps.
.emptyfill $00

.org $00
    jp self_test_redirector
.org $08
    jp disk_initialise
.org $10
    jp read_from_disk
.org $18
    jp write_to_disk
.org $20
    jp format_disk
.org $28
    jp sc3k_utility
.org $30
    jp self_test_passed_redirector
.org $38
    jp do_nothing_redirector

.org $66
nmi_handler:
    jp boot_system_redirector ; NMI handler, connected to reset button

;#######################################
; second-level entry points
;#######################################
self_test_redirector:
    jp self_test              ; not used
boot_system_redirector:
    jp boot_system
self_test_passed_redirector:
    jp self_test_passed       ; used for API 30
do_nothing_redirector:
    jp do_nothing             ; used for API 38 - a ret would work just as well

;#######################################
; Self-test error beeps
;#######################################
self_test_error_beep_1:
    ld e,$01
    jr self_test_error_beep
self_test_error_beep_2:
    ld e,$02
    jr self_test_error_beep
self_test_error_beep_3:
    ld e,$03
    ; fall through
self_test_error_beep:
; inputs: e = beep count
; never returns
; beeps are of the approximate form (eg. e=2)
; -__-__________________-__-__________________-__-___________
; 0s            1s            2s             3s            4s
; where 1 char is ~70ms, the beep section is variable length but the long pauses aren't
    ld d,e                   ; e -> d
---:ld a,%10001111           ; PSG: ch 0 frequency = %0000111111 = 1775.57Hz
    out (psg),a
    ld a,%00000011
    out (psg),a              ; PSG: ch 0 volume = 0 = full
    ld a,%10010000
    out (psg),a

    ld b,$3e                 ; sound for 249924 cycles = 69.8ms
--: ld c,$fb
-:  dec c
    jr nz,-
    djnz --

    ld a,%10011111
    out (psg),a

    ld b,$7d                 ; silence for 503877 cycles = 141ms
--: ld c,$fb
-:  dec c
    jr nz,-
    djnz --

    dec d                    ; repeat d times
    jp nz,---

    ld hl,$03e8              ; wait 4044005 cycles = 1130ms
--: ld c,$fb
-:  dec c
    jr nz,-
    dec hl
    ld a,h
    or l
    jr nz,--

    jp self_test_error_beep  ; loop forever

;#######################################
; Self test - also default entry point
;#######################################
self_test:
    ld sp,top_of_stack ; 000B6 31 00 FF
    di
    im 1
    ld bc,1000               ; repeat count

--: ld a,$ff                 ; pause: 4082 cycles = 1.14ms
-:  dec a
    jr nz,-
    ld a,%10011111           ; PSG: silence all channels
    out (psg),a
    ld a,%10111111
    out (psg),a
    ld a,%11011111
    out (psg),a
    ld a,%11111111
    out (psg),a
    xor a
    ld (self_test_result),a  ; reset self-test passed flag
    dec bc
    ld a,b
    or c
    jr nz,--                 ; repeat 1000 times - maybe to make it audible on error?

    ld hl,$55aa
    push hl
    pop de
    or a
    sbc hl,de
    jr nz,self_test_error_beep_2 ; Error: Z80/RAM failure

    ld a,%10010010           ; initialise SC PPI: set I/O to mode 0, A+B in, C out
    out (sc_ppi_control),a

    ld a,%10010000           ; initialise SF PPI: set I/O to mode 0, A in, B+C out
    out (sf7_ppi_control),a

    ld a,%00001011           ; reset FDC, FDD turned off
    ;     ||  |||`---- FDD /INUSE
    ;     ||  ||`----- FDD /MOTOR ON
    ;     ||  |`------ FDD TC
    ;     ||  `------- FDC RESET
    ;     |`---------- /ROM SEL
    ;     `----------- printer /STROBE
    out (sf7_ppi_c),a
    nop                      ; wait
    nop
    ld a,%00000011           ; let FDC start up, FDD still off
    ;     ||  |||`---- FDD /INUSE
    ;     ||  ||`----- FDD /MOTOR ON
    ;     ||  |`------ FDD TC
    ;     ||  `------- FDC RESET
    ;     |`---------- /ROM SEL
    ;     `----------- printer /STROBE
    out (sf7_ppi_c),a

    call read_vdp_status     ; why do it twice? or at all?
    call read_vdp_status

    ; check ROM - bytes should sum to 0
    ld hl,0
    ld bc,ipl_rom_size
    ld e,0                   ; sum bytes into e
-:  ld a,e
    add a,(hl)
    ld e,a
    inc hl
    dec bc
    ld a,b
    or c
    jr nz,-
    ld a,e
    or a
    jp nz,self_test_error_beep_1 ; error: ROM failure

    ; Initialise VDP with screen off
    ; Reg Data
    ; 0   %00000000 = disable ints, TMS mode
    ; 1   %10010000 = display blanked, mode 1
    ld a,%00000000
    out (vdp_control),a
    ld a,$80
    out (vdp_control),a
    ld a,%10010000
    out (vdp_control),a
    ld a,$81
    out (vdp_control),a

    ; Check VRAM:
    ; Set VRAM address to writing from 0
    ld a,$00
    out (vdp_control),a
    ld a,$40
    out (vdp_control),a
    ld de,vram_size
--: ld b,$04
-:  djnz -                   ; delay a little bit - VRAM access might be slow enough without it?
    ld a,e                   ; some constantly changing value
    add a,d
    out (vdp_data),a
    dec de
    ld a,e
    or d
    jr nz,--

    ; Set VRAM address to reading from 0
    ld a,$00
    out (vdp_control),a
    ld a,$00
    out (vdp_control),a
    ld de,vram_size
--: ld b,$04
-:  djnz -
    ld a,e
    add a,d
    ld b,a
    in a,(vdp_data)
    cp b
    jp nz,self_test_error_beep_3 ; Error 3: VRAM failure
    dec de
    ld a,e
    or d
    jr nz,--

    ; Check RAM:
    ; check RAM outside IPL ROM area
    ld hl,ipl_rom_slot_size
    ld de,$ffff

; this section of code is run once from ROM with the above parameters, and once from RAM for the IPL ROM slot area
check_ram:
    ld b,h                   ; hl -> bc
    ld c,l
    inc de
-:  ld a,l
    add a,h
    ld (hl),a
    inc hl
    ld a,d
    cp h
    jr nz,-
    ld a,e
    cp l
    jr nz,-
    ld h,b
    ld l,c

    ld b,$14                 ; pause
--: ld a,$fb
-:  dec a
    jr nz,-
    djnz --


-:  ld a,l                   ; read back, error on not equal to what was written
    add a,h
    cp (hl)
    jr z,+

    ld a,%00001100           ; set SF7 PC6 (/ROM SEL) to 0 = map in ROM
    out (sf7_ppi_control),a
    jp nz,self_test_error_beep_2

+:  ld (hl),$ff
    inc hl
    ld a,d
    cp h
    jr nz,-
    ld a,e
    cp l
    jr nz,-

install_ram_code:
    ; Install some code in RAM
    ld hl,ram_code_1
    ld de,ram_code_area
    ld bc,ram_code_1_end-ram_code_1
    ldir
    ld hl,install_ram_code   ; calculate code size at runtime - could be done by the assembler...
    ld bc,check_ram
    or a
    sbc hl,bc
    ld b,h                   ; bc = $0031 = size of check_ram code
    ld c,l
    ld hl,check_ram
    ldir
    ld hl,ram_code_2
    ld bc,ram_code_2_end-ram_code_2
    ldir
    ld hl,$0000              ; check IPL ROM slot area
    ld de,ipl_rom_slot_size-1
    jp ram_code_area         ; jump to code in RAM

ram_code_1: ; copied to RAM and executed
    ld a,%00001101           ; set SF7 PC6 (/ROM SEL) to 1 = map RAM to 0..$3fff
    out (sf7_ppi_control),a
ram_code_1_end:

ram_code_2: ; copied to RAM and executed
    ld a,%00001100           ; set SF7 PC6 (/ROM SEL) to 0 = map IPL to 0..$3fff
    out (sf7_ppi_control),a
    jp self_test_passed
ram_code_2_end:

;#######################################
; API 30 = self_test_passed
;#######################################
self_test_passed:
    ld a,self_test_result_passed ; self-test passed
    ld (self_test_result),a

;#######################################
; 6C, 1CD
;#######################################
boot_system:
    im 1
    di
    ld sp,top_of_stack
    ld a,(self_test_result)  ; check self-test passed flag
    cp self_test_result_passed
    jp nz,self_test          ; perform self-test if it hasn't run yet

    call setup_vdp_registers
    call fill_fdc_command_result_buffer ; doing this twice will blank it and make the FDC get on with things
    call fill_fdc_command_result_buffer
    call disk_initialise
    jp c,show_msg_disk_not_ready

    ld de,ram_code_area
    ld bc,$0100              ; track 0, sector 1
    call read_from_disk      ; copy into RAM
    or a
    jr nz,show_msg_disk_error

    ; check for "SYS:"
    ld hl,ram_code_area
    ld a,(hl)
    cp 'S'
    jp nz,show_msg_not_sys_disk
    inc hl
    ld a,(hl)
    cp 'Y'
    jp nz,show_msg_not_sys_disk
    inc hl
    ld a,(hl)
    cp 'S'
    jp nz,show_msg_not_sys_disk
    inc hl
    ld a,(hl)
    cp ':'
    jp nz,show_msg_not_sys_disk
    push hl
      ld hl,msg_ipl_loading
      call write_text
    pop hl                   ; hl now points to the disk name string
    call write_text
    jp disk_boot_loader      ; jump to the boot loader

msg_ipl_loading:
.db FF,CR,CR,CR
.db "    IPL is loading  ",CR
.db CR
.db "          ",0 ; spaces indent the disk name

show_msg_disk_error:
    ld a,(fdc_command_result_buffer)
    bit 3,a                  ; FDC ST0.3: "Not Ready"
    jr nz,show_msg_disk_not_ready
    ld hl,msg_cannot_read_disk ; some other error
    call write_text
    jp wait_for_space_then_boot
show_msg_disk_not_ready:
    ld hl,msg_disk_not_ready
    call write_text
    jp wait_for_space_then_boot

msg_cannot_read_disk:
.db FF,CR
.db CR
.db " can not read this disk",CR
.db CR
.db "  set next disk and hit space key",0
msg_disk_not_ready:
.db FF,CR
.db CR
.db " disk not ready",CR
.db CR
.db "  set disk and hit space key",0

show_msg_not_sys_disk:
    ld hl,msg_not_sys_disk
    call write_text
    jp wait_for_space_then_boot

msg_not_sys_disk:
.db FF,CR
.db CR
.db " This disk is not SYSTEM-DISK",CR
.db CR
.db "   set SYSTEM-DISK and hit space key",0

wait_for_space_then_boot:
-:  call read_key
    cp ' '
    jr nz,-
    jp boot_system
do_nothing:
    ret

;#######################################
; API_28 - SC-3000 Utility Program
;#######################################
sc3k_utility:
    ld sp,top_of_stack
    call stop_disk
    call setup_vdp_registers
    ld hl,msg_sc3k_utility
    call write_text
-:  ld a,'*'
    call read_line           ; gets key press in (de), other stuff too
    ld c,$00
    ld a,(de)                ; look at key pressed
    cp 'C'                   ; C -> copy disk
    jr z,+
    cp 'c'
    jr z,+
    inc c
    cp 'F'                   ; F -> format disk
    jr z,+
    cp 'f'
    jr z,+
    inc c
    cp 'B'                   ; B -> warm boot
    jr z,+
    cp 'b'
    jr z,+
    jr -

+:  inc de
    ld a,(de)
    cp $0d                   ; check if *(de+1) is $0d; loop if not
    dec de
    jr nz,-

    ld h,$00                 ; look up cth value in table
    ld l,c
    add hl,hl
    ld bc,sc3k_utility_functions
    add hl,bc
    ld a,(hl)
    inc hl
    ld h,(hl)
    ld l,a
    ld bc,-                  ; push loop point onto stack for return address
    push bc
    jp (hl)                  ; and invoke the routine

sc3k_utility_functions:
.dw sc3k_utility_disk_copy, sc3k_utility_disk_format, self_test_passed_redirector

msg_sc3k_utility:
.db "*  SC-3000 utility program ",CR,0

;#######################################
; Format disk UI
;#######################################
sc3k_utility_disk_format:
    ld hl,msg_disk_formatting
    call write_text
--: ld hl,msg_new_disk
    call write_text
-:  call read_key
    cp ' '
    jr nz,-
    call disk_initialise
    jr c,--
    call format_disk         ; physically format the disk
    jr c,format_exit_error

    ; now initialise the FAT
    ld hl,disk_copy_buffer   ; zero $4000-$4bff = 3KB = 12 sectors = filename area
    ld d,h
    ld e,l
    inc de
    ld bc,3*1024-1
    ld (hl),$00
    ldir

    ld c,$04
    inc hl

--: ld b,80
-:  ld (hl),$ff              ; fill next sector (FAT) with a blank FAT:
    inc hl                   ; Clusters in use = $ff x 80, $fe x 4, $ff x 76
    djnz -                   ; reserved = $00 x 96

    ld b,4
-:  ld (hl),$fe
    inc hl
    djnz -

    ld b,76
-:  ld (hl),$ff
    inc hl
    djnz -

    ld b,96
-:  ld (hl),$00
    inc hl
    djnz -
    dec c
    jr nz,--                 ; repeat pattern 4 times to fill 1KB with this pattern, total 4KB

    ld b,1                   ; write to sector 1, track 20
    ld c,20
    ld de,disk_copy_buffer
-:  call write_to_disk
    or a
    jr nz,format_exit_error
    inc d                    ; de += 256
    inc b                    ; sector++
    ld a,b
    cp 17                    ; repeat over 16 sectors
    jr nz,-
    call stop_disk
    ld hl,msg_format_done
    call write_text
    ret

format_exit_error:
    call stop_disk
    ld hl,msg_format_error
    call write_text
    ret

msg_disk_formatting:
.db CR,":::  disk formatting  :::",CR,0
msg_new_disk:
.db CR,"  set new disk and hit space key.",CR,0
msg_format_done:
.db CR,"  format completed.",CR,0
msg_format_error:
.db CR,"  sorry can not format.",CR,0

;#######################################
; Copy disk UI
;#######################################
sc3k_utility_disk_copy:
    ld hl,msg_disk_copy
    call write_text
    ld c,$00                 ; start with track 0
sc3k_utility_disk_copy_copy_chunk:
    push bc
--:   ld hl,msg_source_disk  ; ask for source disk
      call write_text
      ld a,$01
      call beep_a_times

-:    call read_key          ; wait for space
      cp ' '
      jr nz,-

      call disk_initialise   ; repeat if no disk present
      jr c,--

      ld de,disk_copy_buffer ; read to disk_copy_buffer
--:   ld b,$01               ; start from sector 1, c = 0
-:    call read_from_disk
      or a
      jr nz,sc3k_utility_disk_copy_error ; handle error

      inc b                  ; next sector
      inc d                  ; dest pointer += 256
      ld a,b
      cp 17                  ; sector 17?
      jr nz,-                ; no -> loop over sectors
      inc c                  ; yes -> increase track
      ld hl,disk_copy_buffer+disk_copy_buffer_size ; if de is $e000 then copy has filled the 40KB buffer
      or a
      sbc hl,de
      jr nz,--               ; else loop over tracks
    pop bc
--: ld hl,msg_dest_disk      ; ask for dest disk
    call write_text
    ld a,$02                 ; beep twice to be helpful: it's disk 2, you see
    call beep_a_times
-:  call read_key            ; wait for space
    cp ' '
    jr nz,-
    call disk_initialise
    jr c,--                  ; repeat if disk not found

    ld de,disk_copy_buffer   ; copy from disk_copy_buffer to disk
--: ld b,$01                 ; start at sector 0 - c = 0 again (from before push/pop bc above)
-:  call write_to_disk       ; write a sector to disk
    or a
    jr nz,sc3k_utility_disk_copy_error
    inc b
    inc d
    ld a,b
    cp 17                    ; loop over sectors
    jr nz,-
    inc c
    ld hl,disk_copy_buffer+disk_copy_buffer_size
    or a
    sbc hl,de
    jr nz,--                 ; loop over 40KB
    ld a,c                   ; check how many tracks have been done
    cp 40
    jr nz,sc3k_utility_disk_copy_copy_chunk ; repeat until 40 tracks are done
    call stop_disk
    ld hl,msg_copy_done
    call write_text
    ret

sc3k_utility_disk_copy_error:
    call stop_disk
    ld hl,msg_copy_error
    call write_text
    ret

msg_disk_copy:
.db CR
.db ":::  disk copy  :::",CR,0
msg_source_disk:
.db CR
.db "  set source disk",CR
.db "          and hit space key.",CR,0
msg_dest_disk:
.db CR
.db "  set destination disk",CR
.db "          and hit space key.",CR,0
msg_copy_done:
.db CR
.db "  copy completed.",CR,0
msg_copy_error:
.db CR
.db "  sorry can not copy.",CR,0

;#######################################
; Read line
;#######################################
; Read a line of text into RAM, handle Backspace, terminate on Enter
; return pointer to string, null-terminated
read_line:
    push af
    push bc
    push hl
      call write_char

      ld a,$01
      call beep_a_times

      ld hl,read_line_buffer ; fill read_line_buffer with CRs
      ld b,$22
-:    ld (hl),CR
      inc hl
      djnz  -

      ld de,read_line_buffer
      ld b,$00               ; x = 0
--:   push bc                ; a long pause for slower key repeat rate
        ld b,$32
-:      ld c,$ff
        call pause_loop_over_c
        djnz  -
      pop bc
      call read_key
      ld l,a                 ; backup key value
      cp $00
      jr z,--                ; null char -> repeat immediately

      ld a,$01               ; beep
      call beep_a_times

      ld a,l
      cp CR                  ; Enter
      jr z,read_line_cr
      cp BS                  ; Backspace
      jr z,read_line_bs

      ld a,b                 ; if x == 33 then loop
      cp 33
      jr z,--

      inc b                  ; else store key in buffer
      ld a,l
      ld (de),a
      inc de
      call write_char        ; write char to screen
      jr --

read_line_bs:
      ld a,b                 ; if x = 0, do nothing
      cp 0
      jr z,--
      dec b                  ; else x--,
      dec de                 ; move buffer pointer back
      ld a,CR                ; blank pointed char
      ld (de),a
      ld a,l                 ; update the screen
      call write_char
      jr --

read_line_cr:
      call write_char
      inc de
      xor a
      ld (de),a              ; null-terminate string
      ld de,read_line_buffer ; return de = string
    pop hl
    pop bc
    pop af
    ret

;#######################################
; Read char
;#######################################
read_key: ; read key, return char code in a
    push bc
    push de
    push hl
      ld hl,keyboard_input_buffer ; copy keyboard buffer up by 9
      ld de,keyboard_input_buffer_copy
      ld bc,8
      ldir
      call read_key_buffer   ; fill lower 9 bytes again
      ld c,$00               ; look through 8 bytes from (de) (ignore modifier keys)
      ld b,$08               ; compare to data from (hl)
      ld hl,keyboard_input_buffer ; count up in c until a new keypress is detected
      ld de,keyboard_input_buffer_copy ; (old = 0, new = 1)
-:    ld a,(de)
      cpl
      and (hl)
      jr nz,+
      inc de
      inc hl
      inc c
      djnz  -
      ld a,$00               ; found nothing -> return
      jr read_key_end
+:    ld h,$00               ; else: c = offset of data found
      ld l,c
      add hl,hl
      add hl,hl
      add hl,hl              ; multiply by 8
      ld b,a
      ld c,$00
-:    rrc b                  ; how many trailing zero bits there are in b, result in c
      jr c,+                 ; eg. %01010000 = 4, %01010100 = 2
      inc c
      jr -
+:    ld b,$00               ; add result to hl
      add hl,bc
      ld a,(keyboard_input_buffer+8) ; check if shift key was pressed
      bit 3,a
      ld bc,table_shifted_chars
      jr z,+
      ld bc,table_unshifted_chars
+:    add hl,bc              ; do the lookup and return the byte found
      ld a,(hl)
read_key_end:
    pop hl
    pop de
    pop bc
    ret

table_shifted_chars:
.db "1", "Q", "A", "Z",  0 , ",", "K", "I"
.db "2", "W", "S", "X", ' ', ".", "L", "O"
.db "3", "E", "D", "C",  0 , "/", ";", "P"
.db "4", "R", "F", "V",  BS,  0 , ":", "@"
.db "5", "T", "G", "B",  0 ,  0 , "]", "["
.db "6", "Y", "H", "N",  0 ,  0 ,  CR,  0
.db "7", "U", "J", "M",  0 ,  0 ,  0 ,  0
.db  0 ,  0 , '\', "^", "-", "0", "9", "8"
table_unshifted_chars:
.db  "!", "q", "a", "z",  0 , "<", "k", "i"
.db "\"", "w", "s", "x", ' ', ">", "l", "o"
.db  "#", "e", "d", "c",  0 , "?", "+", "p"
.db  "$", "r", "f", "v",  0 , "_", "*", "`"
.db  "%", "t", "g", "b",  0 ,  0 , "}", "{"
.db  "&", "y", "h", "n",  0 ,  0 ,  CR,  0
.db  "'", "u", "j", "m",  0 ,  0 ,  0 ,  0
.db   0 ,  0 , "|", "~", "=",  0 , ")", "("

;#######################################
; Write text
;#######################################
; writes text from (hl) until a zero byte is found
; parameters: hl = text pointer
; returns: nothing
; clobbers: nothing
write_text:
    push af
    push hl
-:    ld a,(hl)
      cp 0
      jr z,+
      call write_char
      inc hl
      jr -
+:  pop hl
    pop af
    ret

;#######################################
; Write char
;#######################################
; writes char in a
write_char:
    push af
    push bc
    push de
    push hl
    push ix
      ld (write_char_value),a
      cp CR                  ; handle control codes
      jp z,write_newline
      cp FF
      jp z,write_formfeed
      cp BS
      jp z,write_backspace
      cp ' '                 ; make sure char is in ASCII range (32<=n<128)
      jp c,write_char_exit
      cp $80
      jp nc,write_char_exit  ; do nothing for rest

      ; calculate cursor position in tilemap = tilemap_address + cursor_y * 40 + cursor_x
      ld a,(cursor_y)        ; calculate (cursor_y)*40
      ld hl,0
      ld de,40
      ld b,a
      inc b
-:    dec b
      jr z,+
      add hl,de
      jr -

+:    ld a,(cursor_x)        ; add (cursor_x)
      ld d,$00
      ld e,a
      add hl,de

      ld de,tilemap_address  ; add tilemap_address
      add hl,de

      call set_vram_write_address_to_hl
      ld a,(write_char_value) ; write value
      call wait_and_output_a_to_vdp

      ; increment cursor_x, deal with wrapping
      ld a,(cursor_x)        ; increment cursor_x
      inc a
      ld (cursor_x),a
      cp 40                  ; if < 40, finish, else fall through into newline
      jp nz,write_char_exit
      ; fall through

write_newline: ; line break - reset cursor to left, scroll if on last line else move cursor down
      call at_key_just_pressed ; if @ just pressed, pause
      jr nc,+

      call long_pause
-:    call at_key_just_pressed ; wait for it to be pressed again
      jr nc,-
      call long_pause

+:    ld a,2                  ; wrap cursor to position 2 on next line
      ld (cursor_x),a
      ld a,(cursor_y)
      inc a
      ld (cursor_y),a
      cp 24                   ; if y=24 then the screen needs to scroll
      jp nz,write_char_exit

write_newline_scroll_screen:
      ld hl,tilemap_address   ; copy to start of tilemap
      ld de,tilemap_address + 40 ; from second row
      ld c,23                 ; 23 rows to copy

      ; Read 40 bytes from VRAM at de into a buffer
--:   ex     de,hl
      call set_vram_read_address_to_hl

      ld ix,scrolling_buffer  ; RAM buffer (40 bytes)
      ld b,40                 ; loop counter
-:    call wait_and_read_from_vdp ; read bytes into buffer
      ld (ix+$00),a
      inc ix
      inc hl
      djnz -

      ; Write those 40 bytes back to VRAM at hl
      ex     de,hl
      call set_vram_write_address_to_hl
      ld ix,scrolling_buffer
      ld b,40
-:    ld a,(ix+$00)
      call wait_and_output_a_to_vdp
      inc ix
      inc hl
      djnz -
      dec c
      jr nz,--                ; repeat for 23 rows

      ld b,10                 ; loop 10 times (unrolled by 4)
      ld a,' '                ; write a space
-:    call wait_and_output_a_to_vdp
      call wait_and_output_a_to_vdp
      call wait_and_output_a_to_vdp
      call wait_and_output_a_to_vdp
      djnz -

      ld a,23                 ; put cursor at row 23
      ld (cursor_y),a
      jr write_char_exit

write_formfeed: ; clear screen, reset cursor
      ld a,2                  ; put cursor at (2,0)
      ld (cursor_x),a
      ld a,0
      ld (cursor_y),a

      ld hl,tilemap_address
      call set_vram_write_address_to_hl

      ld bc,40*24             ; fill tilemap with spaces
-:    ld a,' '
      call wait_and_output_a_to_vdp
      dec bc
      ld a,b
      or c
      jr nz,-
      jr write_char_exit

write_backspace:
      ld a,(cursor_x)         ; move cursor left 1
      dec a
      ld (cursor_x),a
      cp 1                    ; if x=1 then the cursor needs to go to the end of the previous row
      jr nz,delete_char_from_tilemap ; else we're done
write_backspace_wrap_to_previous_line:
      ld a,(cursor_y)         ; move cursor to end of prevous line
      dec a
      ld (cursor_y),a
      cp -1                   ; check if it's -1
      ld a,39                 ; cursor x = end of line
      ld (cursor_x),a
      jr nz,delete_char_from_tilemap ; if it wasn't -1 then we're done
write_backspace_reached_top_left:
      ld a,2                  ; else, just go to the top-left limit (ie. undo decrements, nothing happens)
      ld (cursor_x),a
      ld a,0
      ld (cursor_y),a
      jr write_char_exit      ; nothing to delete

delete_char_from_tilemap:
      ; calculate tilemap address of cursor and write a space there
      ; address = cursor_y * 40 + cursor_x + tilemap_address
      ld a,(cursor_y)
      ld hl,0
      ld de,40
      ld b,a
      inc b
-:    dec b
      jr z,+
      add hl,de
      jr -
+:    ld a,(cursor_x)
      ld d,$00
      ld e,a
      add hl,de
      ld de,tilemap_address
      add hl,de
      call set_vram_write_address_to_hl
      ld a,' '                ; write space = blank
      call wait_and_output_a_to_vdp
write_char_exit:
    pop ix
    pop hl
    pop de
    pop bc
    pop af
    ret

long_pause: ; TODO: calculate how long
    push af
    push bc
      ld b,$1e
-:    ld c,$ff
      call pause_loop_over_c
      djnz -
    pop bc
    pop af
    ret

setup_vdp_registers:
; returns: nothing
; parameters: none
; clobbers: a, b, de, hl
    ld b,$02
---:push bc
      call read_vdp_status    ; not sure why?
      ld c,$00
      ld a,%00000000          ; reg 0: no line ints, turn off features
      call set_vdp_register
      inc c
      ld a,%11010000          ; reg 1: display enable, mode 1
      call set_vdp_register
      inc c
      ld a,(tilemap_address)>>10 ; reg 2: tilemap address
      call set_vdp_register
      inc c
      ld a,$00                ; reg 3: colour table address
      call set_vdp_register
      inc c
      ld a,$03                ; reg 4: pattern generator address
      call set_vdp_register
      inc c
      ld a,$00                ; reg 5: sprite table address
      call set_vdp_register
      inc c
      ld a,$00                ; reg 6: sprite tile number modifier (?)
      call set_vdp_register
      inc c
      ld a,$f1                ; reg 7: backdrop color (1)
      call set_vdp_register

      ld hl,$1800
      call set_vram_write_address_to_hl
      xor a
      ld b,a
-:    call wait_and_output_a_to_vdp
      djnz -                  ; output 256 zero bytes at VRAM $1800

      ld de,font_data         ; output the font after that
      ld bc,font_data_end-font_data
-:    ld a,(de)
      call wait_and_output_a_to_vdp
      inc de
      dec bc
      ld a,b
      or c
      jr nz,-

      ld hl,$0800
      ld bc,960
      call set_vram_write_address_to_hl
-:    ld a,$20                ; set 960 bytes from VRAM $0800 to $20
      call wait_and_output_a_to_vdp
      dec bc
      ld a,b
      or c
      jr nz,-
    pop bc
    djnz ---                  ; do the whole thing twice - why?!?
    ld a,2                    ; set cursor to (2, 0)
    ld (cursor_x),a
    ld a,0
    ld (cursor_y),a
    ; fall through for ret, value returned in a is meaningless

read_vdp_status:
    in a,(vdp_control)
    ret

set_vdp_register:
; a = data
; c = register
; clobbers a
    out (vdp_control),a
    ld a,c
    and $07
    or $80
    out (vdp_control),a
    ret

set_vram_read_address_to_hl
    push af
      ld a,l
      out (vdp_control),a
      ld a,h
      and $3f
      out (vdp_control),a
    pop af
    ret

wait_and_read_from_vdp:
    nop
    nop
    nop
    nop
    nop
    in a,(vdp_data)
    ret

set_vram_write_address_to_hl:
    push af
      ld a,l
      out (vdp_control),a
      ld a,h
      and $3f
      or $40                  ; make it a write address
      out (vdp_control),a
    pop af
    ret

wait_and_output_a_to_vdp:
    nop
    nop
    nop
    nop
    out (vdp_data),a
    ret




;#######################################
; @ key just pressed
;#######################################
; returns carry set if @ key was pressed
;   since the last time it was called
; trashes nothing
; no parameters
at_key_just_pressed:
    push bc
      push af
        ld c,$00

        ; select keyboard raster 3
        in a,(sc_ppi_c)
        and %11111000         ; set value to ??????011
        or %00000011
        out (sc_ppi_c),a

        call pause_for_ppi
        in a,(sc_ppi_a)       ; read from keyboard
        and %10000000         ; is high bit set? = key "@" *not* pressed
        jr nz,+
        inc c                 ; if key was pressed this time (c=1)
+:      ld a,(at_key_held)    ; and not pressed last time (a=0)
        cpl                   ; then result = 1
        and c                 ; else result = 0
        ld b,a                ; result in b to be passed out
        ld a,c
        ld (at_key_held),a    ; save key state for next test
      pop af
      rr     b                ; shift result into carry
    pop bc
    ret

;#######################################
; Read key buffer
;#######################################
; fills 9 bytes from keyboard_input_buffer
; with key state information
; address                    bits
;                            7      6      5      4      3      2      1      0
; keyboard_input_buffer+0    I      K      ,  (eng dier) Z      A      Q      1
; keyboard_input_buffer+1    O      L      .    (space)  X      S      W      2
; keyboard_input_buffer+2    P      ;      /  (home/clr) C      D      E      3
; keyboard_input_buffer+3    @      :     (pi) (ins/del) V      F      R      4
; keyboard_input_buffer+4    [      ]    (down)          B      G      T      5
; keyboard_input_buffer+5          (cr)  (left)          N      H      Y      6
; keyboard_input_buffer+6          (up)  (right)         M      J      U      7
; keyboard_input_buffer+7    8      9      0      -      ^     (yen) (graph)(break)
; keyboard_input_buffer+8                              (shift)(ctrl) (func)
; blanks should be ignored.

read_key_buffer:
    push af
    push bc
    push de
    push hl
      in a,(sc_ppi_c)         ; set keyboard raster to 0
      and %11111000
      ld c,a

      ld b,6                  ; counter: first 6 rasters only
      ld hl,keyboard_input_buffer
-:    call raw_read_from_keyboard
      ld (hl),a               ; save low 8 bits in RAM
      rrc e                   ; save PB0 bits in d - PB1/2/3 are nothing for 1st 5 rasters
      rl d                    ; high bits correspond to low rasters: ??890-^(yen)
      inc c                   ; move to next keyboard raster
      inc hl
      djnz -                  ; repeat for all rasters

      ld b,e                  ; bit 2 is FUNC, others are nothing
      call raw_read_from_keyboard ; get last raster
      ld (hl),a               ; save in RAM
      sla d                   ; move d (high bits) up by 2
      sla d
      ld a,e                  ; and put GRAPH and BREAK in the gap
      and %00000011           ; so it's 890-^(yen)(graph)(break)
      add a,d
      inc hl                  ; stick the lot in the next slot in RAM
      ld (hl),a
      ld a,e
      and %00001100           ; CTRL and SHIFT are still unsaved
      ld e,a                  ; put them in e
      ld a,b                  ; get the FUNC key (?????(func)??)
      rrca
      and $02                 ; convert to 000000(func)0
      or e                    ; merge with 0000(shift)(ctrl)00
      ld (keyboard_input_buffer+8),a ; save in the last byte of the buffer
    pop hl
    pop de
    pop bc
    pop af
    ret

;#######################################
; Read from keyboard
;#######################################
; Parameters:
; c = SC3000 PPI C value
; Returns:
; ea = scan code (high bits are other SC3000 statuses)
raw_read_from_keyboard:
    ld a,c                    ; set keyboard raster with c
    out (sc_ppi_c),a
    call pause_for_ppi        ; wait before reading back
    in a,(sc_ppi_b)
    cpl                       ; read high bits into e, invert so they are active high
    ld e,a
    in a,(sc_ppi_a)           ; similar for low bits
    cpl
    ret

pause_for_ppi:
    nop
    nop
    ret

beep_a_times:
; beep on-off at 2kHz, repeat a times
    push af
    push bc
      ld b,a
-:    ld a,%10001000          ; PSG: channel 0 tone 0000111000 = 56 = 1998Hz
      out (psg),a
      ld a,%00000011
      out (psg),a
      ld a,%10010001          ; PSG: channel 0 volume 1 = almost full volume
      out (psg),a
      call wait_51ms
      ld a,%10011111          ; PSG: channel 0 volume f = off
      out (psg),a
      call wait_51ms
      djnz  -
    pop bc
    pop af
    ret

wait_51ms: ; 182150 cycles including call = 50.89ms
      push bc
      ld b,$32
-:    ld c,$ff
      call pause_loop_over_c
      djnz  -
    pop bc
    ret

pause_loop_over_c:
    push bc
-:    dec c
      jp nz,-
      nop
    pop bc
    ret

font_data:
.include "sf7000_rom.sc.font.asm"
font_data_end:

;#######################################
; disk_initialise
; API $08
;#######################################
; Initialises FDC and FDD
; returns carry on error, including disk not present
disk_initialise:
    push bc
    push de
    push hl
      in a,(sf7_ppi_c)        ; reset FDD /INUSE (ie. FDD on)
      and %11110010           ; reset FDD TC
      or %00100000            ; reset FDC RESET (ie. FDC on)
      out (sf7_ppi_c),a       ; set SF7 PC5 = ???
      bit 1,a                 ; check FDD /MOTOR ON
      jr z,+                  ; if not on, turn it on

      ld a,%00000010          ; set SF7 PC1 (FDD /MOTOR ON) to 0 = turn motor on
      out (sf7_ppi_control),a

      ld bc,869               ; wait for 3581154 cycles = 1.000s
--:   xor a
-:    dec a
      jr nz,-
      dec bc
      ld a,b
      or c
      jr nz,--

+:    ld d,250                ; outer loop 250 times - how may times to try to time a fast disk spin before failing
      ld e,7                  ; inner loop 7 times - how many times to try to detect a spin before failing

disk_initialise_loop:
      ld bc,$0000             ; wait for FDC INDEX (PA2) to be set
-:    in a,(sf7_ppi_a)
      and %00000100
      jr nz,+                 ; passed check (nz), carry on
      dec bc
      ld a,b
      or c
      jr nz,-
      jr disk_initialise_error ; flag was never set

+:    ld hl,$0000             ; wait for FDC INDEX (PA2) to be reset
-:    in a,(sf7_ppi_a)
      and %00000100
      jr z,+                  ; passed check (z), carry on
      dec hl
      ld a,h
      or l
      jr nz,-
      jr disk_initialise_error

+:    ld bc,15441             ; 20 wait (~207ms) for FDC INDEX (PA2) to be set
-:    in a,(sf7_ppi_a)        ; 11
      and %00000100           ;  4
      jr nz,+                 ;  7 (+5 on exit) passed check (nz), carry on
      dec bc                  ;  6
      ld a,b                  ;  4
      or c                    ;  4
      jr nz,-                 ; 12 (always pass)
      ; loop is 48 cycles + 31 since last port read, + 27 reaching successful exit

      ; ran out of time waiting
      dec e
      jr nz,disk_initialise_loop ; repeat disk_initialise_loop while e-- > 0 (inner loop)
      jr disk_initialise_error

      ; disk spin fully detected
      ; see how fast it's spinning - if bs was decremented to 2806 or less, it was too slow
      ; reaching 2807 is equivalent to 12634 loops and 1 successful exit
      ; which makes 606490 cycles between INDEX off and INDEX on
      ; = 169ms = 354RPM
+:    ld hl,2807              ; compare bc to 2807
      or a
      sbc hl,bc
      jr nc,+                 ; if less, it's good - disk is spinning fast
      dec d                   ; else, try again, looping on d (outer loop)
      jr nz,disk_initialise_loop
      jr disk_initialise_error

+:    ld hl,fdc_specification_data ; Set up FDC parameters
      ld a,fdc_specification_data_end-fdc_specification_data
      call write_to_fdc
      jr nz,disk_initialise_error
      call calibrate_and_seek_to_track_0
      jr c,disk_initialise_error
-:  pop hl
    pop de
    pop bc
    ret

disk_initialise_error:
    scf                       ; set carry = 1
    jr -                      ; return

fdc_specification_data:
.db fdc_command_specify       ; "specify"
.db (6<<4) | 0                ; Step Rate Time 6ms, Head Unload Time 0ms
.db (5<<1) | 1                ; Head Load Time 5ms, Non-DMA mode true
fdc_specification_data_end:

;#######################################
; Reads a sector (256B) from disk to memory
; API $10
;#######################################
; Parameters:
; de = destination buffer
; b = sector number on disk
; c = track number on disk
; Destroys af
; returns carry on error
; retries 40 times, re-seeking after 10 read errors
read_from_disk:
    push bc
    push de
    push hl
      ld hl,fdc_command_loop_counter
      ld (hl),4               ; try 4 times
--:   call seek_track_c
      jr c,+
      ld hl,fdc_command_loop_counter_2
      ld (hl),10              ; try 10 times
-:    call read_track_c_sector_b_to_de
      jr z,read_from_disk_error
      ld hl,fdc_command_loop_counter_2 ; failed: loop inner (reading)
      dec (hl)
      jr nz,-
      ld hl,fdc_command_loop_counter ; failed a lot: loop outer (seeking)
      dec (hl)
      jr z,+
      call calibrate_and_seek_to_track_0 ; so the seek will be recalibrated too
      jr nc,--
+:    scf
.db $06 ; will merge with next opcode to make "ld b, $b7" and thus not affect flags
read_from_disk_error:
      or a                    ; reset carry flag
    pop hl
    pop de
    pop bc
    ret

read_track_c_sector_b_to_de:
    ld a,fdc_command_read | fdc_command_modifier_mfm
    call fill_fdc_command_data_buffer ; track, sector passed in via b, c
    ld a,9                    ; write 9 byte command
    call write_to_fdc
    ret    nz                 ; return if
    push bc
    push de
      ex     de,hl            ; hl = dest address
      call try_raw_read_sector_from_disk
    pop de
    pop bc
-:  in a,(sf7_ppi_a)          ; wait for SF7 PA0 (FDC INT)
    rrca
    jr nc,-
    call fill_fdc_command_result_buffer
    ret

try_raw_read_sector_from_disk:
    di
    ld b,$00                  ; counter: read 256 bytes
    ld c,fdc_data
    ld e,%01000000            ; used for a mask in a moment

-:  in a,(fdc_status)         ; wait for FDC status bit 7 (RQM)
    rlca
    jr nc,-
    and e                     ; return if FDC status bit 5 (EXM) is not set (?)
    ret    z
    ini                       ; read a byte from fdc_data to hl
    jr nz,-                   ; loop until b = 0, ie. 256 times

    ld a,%00000101            ; set SF7 PC2 (FDD TC - transaction complete?) to 1
    out (sf7_ppi_control),a
    dec a                     ; then set it to 0, ie. just strobe it
    out (sf7_ppi_control),a
    ei
    ret

;#######################################
; Write a sector (256B) from memory to disk
; API $18
;#######################################
; Parameters:
; de = source buffer
; b = sector number on disk
; c = track number on disk
; Destroys af
; returns carry on error
; checks it can read back without error, does not verify data read back
; retries 256 times
write_to_disk:
    push bc
    push de
    push hl
      call seek_track_c
      jr c,++
      ld hl,fdc_command_loop_counter
      ld (hl),$00             ; counter: 256 tries
--:   call write_sector_from_de_to_fdc_track_c_sector_b
      jr nz,+
      ld a,$e0                ; pause
-:    dec a
      nop
      nop
      jr nz,-
      call test_read_sector_from_fdc_track_c_sector_b ; try to read it back (slow!)
      jr z,+++                ; exit if no error
+:    ld hl,fdc_command_loop_counter ; else decrement counter and loop
      dec (hl)
      jr nz,--
++:   scf
.db $06 ; or a eater
+++:  or a
    pop hl
    pop de
    pop bc
    ret

write_sector_from_de_to_fdc_track_c_sector_b:
    ld a,fdc_command_write | fdc_command_modifier_mfm; FDC Read mode; b and c set to sector, track already
    call fill_fdc_command_data_buffer
    ld a,9                    ; 9 bytes FDC command
    call write_to_fdc
    ret    nz
    push bc
    push de
      ex     de,hl
      call write_256_bytes_from_hl_to_fdc_when_ready
    pop de
    pop bc
-:  in a,(sf7_ppi_a)          ; wait for SF7 PA0 (FDC INT)
    rrca
    jr nc,-
    call fill_fdc_command_result_buffer
    ret

write_256_bytes_from_hl_to_fdc_when_ready:
    di
      ld b,$00                ; counter for 256 bytes
      ld c,fdc_data
      ld e,%01000000
-:    in a,(fdc_status)       ; loop until FDC status bit 7 (RQM) is set
      rlca
      jr nc,-
      and e
      ret    z                ; return if FDC status bit 5 (EQM) not set (?)
      outi                    ; output a byte
      jr nz,-
      ld a,%00000101          ; strobe SF7 PC2 (FDD TC) ->1->0 = terminate any data transfers
      out (sf7_ppi_control),a
      dec a
      out (sf7_ppi_control),a
    ei
    ret

test_read_sector_from_fdc_track_c_sector_b:
    ld a,fdc_command_read | fdc_command_modifier_mfm ; FDC read mode
    call fill_fdc_command_data_buffer
    ld a,9  
    call write_to_fdc
    ret    nz
    push bc
      call test_read_256_bytes_from_fdc_when_ready ; test read 256 bytes - all are discarded
    pop bc
-:  in a,(sf7_ppi_a)          ; wait for SF7 PA0 (FDC INT)
    rrca
    jr nc,-
    call fill_fdc_command_result_buffer
    ret

test_read_256_bytes_from_fdc_when_ready:
    di
    ld b,$00
    ld c,%01000000
-:  in a,(fdc_status)         ; wait for FDC status bit 7 (RQM) set
    rlca
    jr nc,-
    and c
    ret    z                  ; return if FDC status bit 5 (EQM) not set (?)
    in a,(fdc_data)           ; read from FDC, and discard immediately
    djnz -                    ; loop 256 times
    ld a,%00000101            ; strobe SF7 PC2 (FDD TC) ->1->0 to terminate transfers
    out (sf7_ppi_control),a;11EB D3 E7
    dec a
    out (sf7_ppi_control),a;11EE D3 E7
    ei
    ret

;#######################################
; Low-level disk format: API $20
;#######################################
; Parameters: none
; Destroys af
format_disk:
    push bc
    push de
    push hl
      call calibrate_and_seek_to_track_0
      jr c,format_disk_error
      ld c,0                  ; start with track 0
format_track_c:
      call seek_track_c
      jr c,format_disk_error

      ; fill a 64-byte buffer with data used during format (?)
      ; UPD765A manual doesn't mention it, presumably the FDC765 is different?
      ld a,1                  ; count up sectors in a
      ld hl,fdc_format_buffer
      dec hl                  ; could have loaded a predecremented value?
-:    inc hl                  ; loop over 16*4 bytes from fdc_format_buffer
      ld (hl),c               ; fill with c, 0, a, 1
      inc hl                  ; c = track, a = 1, 2, .. 16
      ld (hl),0
      inc hl
      ld (hl),a
      inc hl
      ld (hl),1
      inc a
      cp 17                   ; 16 times for 16 sectors, so a==17 = done
      jr nz,-

      ld hl,fdc_command_data_buffer
      ld (hl),fdc_command_format | fdc_command_modifier_mfm
      inc hl
      ld (hl),$00             ; side 0, drive 0
      inc hl
      ld (hl),$01             ; 1 data byte per sector?!?
      inc hl
      ld (hl),$10             ; sectors per track
      inc hl
      ld (hl),$2a             ; Gap 3 length (low level format stuff)
      inc hl
      ld (hl),$ff             ; filler byte
      push bc
        call do_disk_format
      pop bc
      jr nz,format_disk_error

      xor a                   ; another weird pause algorithm
-:    dec a
      nop
      nop
      nop
      jr nz,-

      inc c                   ; move on to next track
      ld a,c
      sub    40               ; repeat 40 times
      jr nz,format_track_c
-:  pop hl
    pop de
    pop bc
    ret

format_disk_error:
    scf
    jr -

do_disk_format:
    ld a,6                    ; 6 bytes in format command
    ld hl,fdc_command_data_buffer
    call write_to_fdc
    ret    nz                 ; return on error
    ld hl,fdc_format_buffer
    call output_64_bytes_from_hl_to_fdc
-:  in a,(sf7_ppi_a)          ; wait for SF7 PA0 (FDC INT)
    rrca
    jr nc,-
    call fill_fdc_command_result_buffer
    ret

output_64_bytes_from_hl_to_fdc:
    di
    ld b,64                   ; counter for 64 bytes
    ld c,fdc_data
    ld e,%01000000
-:  in a,(fdc_status)         ; wait for FDC RQM
    rlca
    jr nc,-
    and e
    ret    z                  ; error if FDC EXM not set
    outi                      ; output a byte
    jr nz,-                   ; repeat
    ld a,%00000101            ; strobe SF7 PC2 (FDD TC) ->1->0
    out (sf7_ppi_control),a
    dec a
    out (sf7_ppi_control),a
    ei
    ret

seek_track_c:
    push bc
    push hl
      call flush_fdc_status
      ld hl,fdc_command_data_buffer
      ld (hl),fdc_command_seek
      inc hl
      ld (hl),$00             ; drive 0 side 0
      inc hl
      ld (hl),c               ; track c
      ld a,3                  ; 3 byte command

output_no_data_fdc_command_and_get_result:
      ld hl,fdc_command_data_buffer
      call write_to_fdc
      jr nz,return_carry_set
-:    in a,(sf7_ppi_a)        ; wait for SF7 PA0 (FDC INT)
      rrca
      jr nc,-
      call fill_fdc_command_result_buffer
    pop hl
    pop bc
    scf                       ; return carry set if z flag not set, ie. FDC status indicates an error
    ret    nz
    ccf
    ret

return_carry_set:
    pop hl
    pop bc
    scf
    ret

calibrate_and_seek_to_track_0:
    push bc
    push hl
      call flush_fdc_status
      ld hl,fdc_command_data_buffer
      ld (hl),fdc_command_recalibrate
      inc hl
      ld (hl),$00             ; drive 0, side 0
      ld a,2                  ; 2 byte command
      jr output_no_data_fdc_command_and_get_result

; while FDC INT is set, read in the FDC status registers
flush_fdc_status:
    in a,(sf7_ppi_a)          ; if SF7 PA0 (FDC INT) set, fill status buffer and repeat
    rrca                      ; else exit
    ret    nc
    call fill_fdc_command_result_buffer
    jr flush_fdc_status

;#######################################
; Write a bytes from (hl) to FDC
;#######################################
; Parameters:
; a = number of bytes to write
; hl = where to write from
; Returns:
; carry set on error
; zero set on success
; Clobbers:
; af
write_to_fdc:
    push bc
    push hl
      ld b,a                  ; a = loop count
      ld c,fdc_data           ; c = fdc data port
-:    in a,(fdc_status)       ; wait for fdc status bit 4 to be 0
      bit 4,a
      jr nz,-
-:    in a,(fdc_status)       ; wait for fdc status bit 7 to be 1
      rlca
      jr nc,-
      rlca                    ; if fdc status bit 6 is 1, error
      jr c,write_to_fdc_error
      outi                    ; else, output a byte
      jr nz,-
    pop hl
    pop bc
    xor a
    ret

write_to_fdc_error:
      call fill_fdc_command_result_buffer
    pop hl
    pop bc
    ret

fill_fdc_command_data_buffer: ; $12de
; fills 9-byte fdc_command_data_buffer for command a, cylinder c, track b
; and default the other 5 bytes to work with this drive
; returns hl = fdc_command_data_buffer

; writes a, $0, c, $0, b, $1, $10, $e, $ff to fdc_command_data_buffer
;        |   |  |   |  |   |    |   |    `-- DTL = data length  = unused when N=0
;        |   |  |   |  |   |    |   `------- GPL = gap 3 length = some low-level stuff
;        |   |  |   |  |   |    `----------- EOT = end of track = highest possible sector number
;        |   |  |   |  |   `---------------- N   = number       = number of bytes to transfer
;        |   |  |   |  `-------------------- R   = record       = sector number
;        |   |  |   `----------------------- H   = head         = head address
;        |   |  `--------------------------- C   = cylinder     = track number
;        |   `------------------------------ drive 0, side 0
;        `---------------------------------- command + flags
    ld hl,fdc_command_data_buffer
    ld (hl),a                 ; command + flags
    inc hl
    ld (hl),$00               ; drive 0, side 0
    inc hl
    ld (hl),c                 ; track number
    inc hl
    ld (hl),$00               ; head address always 0
    inc hl
    ld (hl),b                 ; sector number
    inc hl
    ld (hl),1                 ; amount to read/write - 1 byte!
    inc hl
    ld (hl),16                ; highest sector number
    inc hl
    ld (hl),$0e               ; gap 3 stuff
    inc hl
    ld (hl),$ff               ; ignored
    ld hl,fdc_command_data_buffer
    ret

; char *p = fdc_command_result_buffer;
; wait_for_fdc_ready();               // wait for FDC to be ready to read
; if (fdc_bit_6() == 0)               // if status data bit is 0,
; {
;   fdc_write(8);                     // request status data
;   wait_for_fdc_ready();
;   if (fdc_bit_6() == 0)             // if no more status data exists
;     return *fdc_command_result_buffer; // return first byte
; }
; else                                // there is status data to be read
; {
;   *p++ = fdc_read();                // copy it into the buffer
; }

; read the FDC's status registers - however many it wants to return
; returns the contents of SR0 in a, and z set for normal termination status value
fill_fdc_command_result_buffer:
    push hl
      ld hl,fdc_command_result_buffer
-:    in a,(fdc_status)       ; wait for status bit 7 = 1 <-+-----+
      rlca                    ;                             |     |
      jr nc,-                 ; ----------------------------+     |
      rlca                    ;                                   |
      jr c,+                  ; if status bit 6 = 1 ----------+   |
      ld a,8                  ; else output 8                 |   |
      out (fdc_data),a        ;                               |   |
-:    in a,(fdc_status)       ; wait for status bit 7 = 1 <-+ |   |
      rlca                    ;                             | |   |
      jr nc,-                 ; ----------------------------+ |   |
      rlca                    ;                               |   |
      jr nc,++                ; if status bit 6 = 0 ----------|-+ |
+:    in a,(fdc_data)         ; else read byte to (hl++)  <---+ | |
      ld (hl),a               ;                                 | |
      inc hl                  ;                                 | |
      jr -                    ; --------------------------------|-+
                              ;                                 |
++:   ld a,(fdc_command_result_buffer) ; return a = FDC SR0  <--+
      and fdc_status_0_ok_mask ; return z if FDC SR0 indicates normal termination
    pop hl
    ret

.macro PPIControlValueToA args PCNum, value
  ld a, PCNum << 1 | value
.endm

stop_disk:
    push af
      PPIControlValueToA 1, 1 ; set SF7 PC1 (FDD /MOTOR ON) to 1 = turn FDD motor off
      out (sf7_ppi_control),a
      PPIControlValueToA 0, 1 ; set SF7 PC0 (FDD /INUSE) to 1 = set FDD off
      out (sf7_ppi_control),a
      PPIControlValueToA 5, 0 ; set SF7 PC5 (???) to 0
      out (sf7_ppi_control),a
    pop af
    ret

blank:
.dsb $1fff-$1330, $ff ; blank fill

.org $1fff
.db $b0                       ; value changed such that byte sum = 0
