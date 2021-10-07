        .include "opcodes.inc"
        .include "apple2.inc"
        .include "common.inc"
        .feature string_escapes

        .org $2000
        .setcpu "65C02"

;;; TODO: Identify 4 different clock drivers.
;;; * IIc System Clock (via Serial or Printer port)
;;; * Seiko DataGraph (via IIe Gameport)
;;; * Seiko DataGraph (via IIc Serial)
;;; * ???
;;; See https://www.applefritter.com/node/176 for some details
.enum MessageCode
        kInstall            =  0
        kNoSysFile          =  1
        kDiskError          =  2
        kIIc                =  3
        kIIe                =  4
        kCurrentYear        =  5
        kOkPrompt           =  6
        kNoClock            =  7
        kRunning            =  8
        kSeikoIIc           =  9
        kSeikoIIe           = 10
        kRemoveWriteProtect = 11
.endenum

kErrDiskWriteProtected = $2B


;;; Zero Page Locations
mach_type := $07                ; 0 = IIc, 1 = II+/IIe
case_mask := $08                ; mask with chars before COUT
has_80col := $09                ; 1 = has 80 column card
month     := $0A                ; month from current date ???
msg_num   := $0B                ; stashed message number

digits_buffer := $200           ; temporary usage during clock read

PRODOS_SYS_START := $2000

SLOT3_FIRMWARE           := $C300

;;; SSC in Slot 1
PORT1_ACIA_STATUS  := $C099
PORT1_ACIA_COMMAND := $C09A
PORT1_ACIA_CONTROL := $C09B

;;; SSC in Slot 2
PORT2_ACIA_STATUS  := $C0A9
PORT2_ACIA_COMMAND := $C0AA
PORT2_ACIA_CONTROL := $C0AB

;;; Annunciators/switch inputs - clock in game adapter port?
CLRAN0          := $C058
SETAN0          := $C059
CLRAN1          := $C05A
SETAN1          := $C05B
CLRAN2          := $C05C
SETAN2          := $C05D
RD63            := $C063        ; Switch Input 3

kClockRoutineMaxLength = 125    ; Per ProDOS 8 TRM

kDefaultDriveSlot = 6

kIIcVersionByte = HI('L')
kDefaultVersionByte     = HI(' ')

kDefaultYear = 1986

kDefaultDelay = $5D
kPatchedDelay = $5C

;;; ============================================================

L2000:
        default_slot := * + 1
        lda     #kDefaultDriveSlot ; For easy patching???

        version_test_byte := *+1
        lda     #kDefaultVersionByte
        cmp     #kIIcVersionByte
        bne     L2013

        sta     L239E + (L1272 - Chain) ; In relocated code
        lda     #kPatchedDelay
        sta     Patch0_delay
        sta     Patch2_delay
L2013:
        ;; Clear stack
        ldx     #$FF
        txs

        ;; Clear interpreter version
        ;; TODO: Remove this
        lda     #0
        sta     IBAKVER
        sta     IVERSION

        ;; Trash reset vector to cause reboot
        sta     ROMIN2
        lda     $03F3
        eor     #$FF
        sta     $03F4

        ;; Identify machine type
        ldy     #0
        ldx     #0
        lda     MACHID
        and     #%10001000      ; =0 if II+, >0 if IIe/IIc
        beq     :+
        inx                     ; Not a II+

:       cmp     #%10001000      ; IIc ?
        beq     is_iic
        lda     version_test_byte ; Hack ???
        cmp     #kIIcVersionByte
        beq     is_iic
        iny                     ; Not a IIc
is_iic: sty     mach_type

        ;; Set case mask (II+ vs. IIe/IIc)
        lda     case_mask_table,x
        sta     case_mask

        ;; Check 40/80 columns; wrap strings if 40 columns.
        lda     MACHID
        and     #%00000010      ; 80 Column card?
        lsr     a
        sta     has_80col
        bne     init_80_col

        ;; Convert spaces to newlines if 40 Columns
        lda     #CR|$80
        sta     chain + (wrap1 - Chain)
        sta     chain + (wrap4 - Chain)
        sta     chain + (wrap5 - Chain)
        sta     chain + (wrap3 - Chain)
        sta     chain + (wrap2 - Chain)

        ;; Adjust everything down a line
        inc     DoInstall_wndtop
        inc     DoInstall_wndbtm
        bne     :+

init_80_col:
        jsr     SLOT3_FIRMWARE
:

        ;; --------------------------------------------------
        ;; Copy to $1000

.proc RelocateChainingCode
        ldy     #0

        ;; End signature is two adjacent $FFs
loop:
        read_msb := * + 2
        lda     L239E,y
        cmp     #$FF
        bne     :+

        check_msb := * + 2      ; check for second $FF
        ldx     L239E+1,y
        cpx     #$FF
        beq     DoInstall           ; done!
:

        write_msb :=  * + 2
        sta     Chain,y
        iny
        bne     loop

        inc     read_msb
        inc     check_msb
        inc     write_msb

        bne     loop            ; always
.endproc

;;; ============================================================

.proc DoInstall
        ldy     #MessageCode::kInstall
        jsr     ShowMessage

        wndtop := * + 1
        ldy     #3
        sty     WNDTOP

        wndbtm := * + 1
        ldy     #5
        sty     WNDBTM

        jsr     SetPrefixAndGetFileInfo

        lda     file_info_params + GET_FILE_INFO_PARAMS::mod_date + 1
        lsr     a
        sta     year            ; patch driver
        jsr     ConvertToBCD
        sta     bcd_year

        ;; --------------------------------------------------
        ;; Identify drive slot, needed for Patch1
.scope
        ;; Search device list for Disk II device
        ldx     DEVCNT
:       lda     DEVLST,x
        and     #%00001111      ; low nibble = "device identification"
        beq     found           ; 0 = Disk II
        dex
        bpl     :-

        ;; Did not find Disk II, use default
        lda     default_slot
        and     #%00000111
        bne     :+
        lda     #6             ; default to slot 6 if was tweaked to 0
:       asl     a              ; shift to 0sss0000 (like a unit number)
        asl     a
        asl     a
        asl     a
        bne     assign

found:  lda     DEVLST,x

assign: and     #%01110000      ; slot
        ora     #$80
        sta     Patch1_firmware_byte ; Set $C0nn address
.endscope


;;; ------------------------------------------------------------
;;; Select and apply patch

;;; flags   mach_type   Version
;;; ...00   0           Patch2          IIc
;;; ...01   0           Patch0          IIc     Seiko DataGraph
;;; ...10   0           Patch0          IIc     Seiko DataGraph
;;; ...11   0           Patch0          IIc     Seiko DataGraph

;;; ...00   1           Patch1          IIe     Seiko DataGraph
;;; ...01   1           Patch3          IIe     Seiko DataGraph
;;; ...10   1           Patch0          IIe     Seiko DataGraph
;;; ...11   1           Patch0          IIe     Seiko DataGraph


;;; Patch0 - (default) IIc System Clock - SSC in either Slot 1 or Slot 2
;;; Patch1 - Seiko DataGraph in Gameport ???
;;; Patch2 - SSC in Slot 2, COMMAND not restored
;;; Patch3 - Seiko DataGraph in Gameport ???

        lda     flags
        and     #%00000011
        beq     not_seiko       ; not Seiko
        cmp     #%00000010
        bcs     check_time_maybe_install ; no patch
        lda     mach_type
        bne     not_iic         ; apply Patch3
        beq     check_time_maybe_install           ; always

not_seiko:
        ldy     #Patch2 - Patches
        lda     mach_type
        beq     apply_patch     ; if IIc
        ldy     #Patch1 - Patches
        jmp     apply_patch

        ;; Patch bytes on top of driver
not_iic:
        ldy     #Patch3 - Patches

apply_patch:
        ldx     #0
:       lda     Patches,y
        sta     patch_target,x
        iny
        inx
        cpx     #kPatchLength
        bne     :-

;;; ------------------------------------------------------------
;;; Compute time and install clock driver

check_time_maybe_install:
        lda     #2              ; 3 tries total
        sta     tries
:       jsr     InvokeDriverValidateDateTime
        bcs     MaybeTryOtherPort
        dec     tries           ; avoid false positives?
        bpl     :-
        bmi     InstallClock   ; always

tries:  .byte   0

.endproc
        DoInstall_wndtop := DoInstall::wndtop
        DoInstall_wndbtm := DoInstall::wndbtm

;;; ============================================================

tried_port1_flag:  .byte   0

.proc MaybeTryOtherPort
        lda     mach_type
        bne     fail            ; not IIc - give up

        ;; Is a IIc - have we tried the other port?
        lda     tried_port1_flag
        bne     fail            ; yes - give up

        ;; Switch to printer port...
        inc     tried_port1_flag
        ldy     #<PORT1_ACIA_STATUS
        sty     Patch0_acia_status_patch1
        iny                     ; <PORT1_ACIA_COMMAND
        sty     Patch0_acia_command_patch1
        sty     Patch0_acia_command_patch2
        lda     flags
        and     #%00000011
        beq     DoInstall::check_time_maybe_install

        sty     Patch0_acia_command_patch3
        ;; Try again
        bne     DoInstall::check_time_maybe_install ; always

        ;; --------------------------------------------------

fail:   ldy     #MessageCode::kNoClock
        sty     msg_num
        lda     MACHID          ; Check for clock card
        ror     a
        bcc     no_clock        ; Bit 0 = 0 means no clock card

        jsr     MON_HOME
        jmp     Chain

no_clock:
        lda     #0
        sta     DATELO
        sta     DATELO+1
        sta     TIMELO
        sta     TIMELO+1
        jmp     ShowMessageAndChainOrHang ; will chain since kNoClock
.endproc

;;; ============================================================
;;; Run after the selected driver has been verified to work.
;;; Prompts for current year, updates the timestamped file on disk,
;;; patches/relocates the driver, and then chains to next file.

.proc InstallClock
        lda     #OPC_JMP_abs
        sta     DATETIME
        lda     MACHID
        ora     #%00000001      ; has clock
        sta     MACHID

        bit     KBD             ; Key pressed?
        bmi     skip

        lda     month
        cmp     #11
        bcc     :+
        bit     flags
        bpl     :+
        sta     $1204
:       lda     $1204
        lsr     a
        cmp     #(kDefaultYear .mod 100)
        bcc     skip

        ;; Two byte compare
        lda     DATELO
        cmp     file_info_params + GET_FILE_INFO_PARAMS::mod_date
        lda     DATELO+1
        sbc     file_info_params + GET_FILE_INFO_PARAMS::mod_date + 1
        bcs     L21DF

skip:   bit     KBDSTRB

        ;; Set high bit of flags to (month >= 3) ???
        rol     flags
        lda     #3
        cmp     month
        ror     flags

        ;; --------------------------------------------------
        ;; Show current year prompt
.scope
retry:
        ldy     #MessageCode::kCurrentYear
        jsr     ShowMessage

        lda     bcd_year           ; 2-digit year
        jsr     PRBYTE

        ldy     #MessageCode::kOkPrompt
        jsr     ShowMessage

        ;; Wait for Y/N keypress
:       jsr     RDKEY
        and     #%11011111      ; lowercase --> uppercase
        cmp     #'Y'|$80
        beq     year_ok
        cmp     #'N'|$80
        bne     :-

        ;; Prompt for two digit year
        ldy     #MessageCode::kCurrentYear
        jsr     ShowMessage
        jsr     GetDigitKey     ; Decade
        asl     a               ; move tens digit to high nibble
        asl     a
        asl     a
        asl     a
        sta     bcd_year
        jsr     GetDigitKey     ; Year
        and     #$0F            ; mask ones digit
        ora     bcd_year
        sta     bcd_year

        jmp     retry
.endscope

        ;; Current year is okay - update the file with date/timestamp
year_ok:
        lda     $1204           ; unused???
        jsr     YearFromBCD
        jsr     InvokeDriver
        jsr     WriteFileInfo

        ;; Patch clock driver
L21DF:  lda     RWRAM1          ; Driver lives in LC Bank 1
        lda     RWRAM1          ; so bank that in

        lda     DATETIME+1      ; driver destination
        sta     install_ptr
        clc
        adc     #time_offset_table - Driver ; patch an internal reference (LSB)
        sta     offset_table_addr
        lda     DATETIME+2      ; driver destination
        sta     install_ptr+1
        adc     #0
        sta     offset_table_addr+1 ; patch an internal reference (MSB)

        ;; Relocate clock driver
        ldy     #kClockRoutineMaxLength - 1
:       lda     Driver,y
install_ptr := * + 1
        sta     $F000,y
        dey
        bpl     :-

        ;; Initialize the time (via driver)
        jsr     DATETIME
        lda     ROMIN2          ; Bank ROM back in

        ;; Chain
        jmp     Chain
.endproc

;;; ============================================================
;;; Convert year from BCD
;;; Input: bcd_year
;;; Output: year
;;; $06 is trashed

.proc YearFromBCD
        tmp := $06

        lda     bcd_year
        pha
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        sta     tmp
        asl     a
        asl     a
        adc     tmp
        asl     a
        sta     tmp
        pla
        and     #$0F
        clc
        adc     tmp
        sta     year            ; patch driver
        rts
.endproc

;;; ============================================================
;;; Convert to BCD
;;; Input: A = number
;;; Output: A = BCD number
;;; $06 is trashed

.proc ConvertToBCD
        tmp := $06

        ldx     #$FF
:       inx
        sec
        sbc     #10
        bcs     :-
        adc     #10
        sta     tmp
        txa
        asl     a
        asl     a
        asl     a
        asl     a
        ora     tmp
        rts
.endproc

;;; ============================================================
;;; Invoke the driver, then verify that the result is a valid
;;; date/time.
;;; Output: Carry clear if valid, set otherwise.

.proc InvokeDriverValidateDateTime
        jsr     InvokeDriver

        ;; Check month
        lda     DATELO+1
        ror     a
        lda     DATELO
        rol     a
        rol     a
        rol     a
        rol     a
        and     #$0F
        sec
        beq     done            ; Month = 0 is failure

        cmp     #13
        bcs     done            ; Month >= 13 is failure

        sta     month

        ;; Check hour
        lda     TIMELO+1
        cmp     #24
        bcs     done            ; Hour >= 24 is failure

        ;; Check minute
        lda     TIMELO
        cmp     #60             ; Min >= 60 is failure

done:   rts
.endproc

;;; ============================================================
;;; Clock Driver (Relocatable)
;;; ============================================================

Driver: cld
        cld                     ; TODO: Remove duplicate CLD
        php
        sei

patch_target:
        ;; --------------------------------------------------
        ;; Patch applied from here...
kPatchLength = $38

.proc Patch0
;;; Driver for IIc System Clock (via either Port 2 or Port 1)

acia_command_patch1 := * + 1
        ;; ------------------------------
        ;; Save COMMAND register
        lda     PORT2_ACIA_COMMAND
        pha

        ;; ------------------------------
        ;; Unlock COMMAND register
        ldy     #3              ; cycles
        ldx     #22             ; initial delay
        lda     #%00001000
command_loop:
acia_command_patch2 := * + 1
        sta     PORT2_ACIA_COMMAND

:       dex                     ; wait
        bne     :-

        eor     #%00001010
        ldx     #9              ; repeat delay and # of digits-1
        dey
        bne     command_loop

        ;; ------------------------------
        ;; Read bit out of STATUS register
        ;; 1 bit at a time in blocks of 4, giving
        ;; "MMDDhhmmss" in low nibble of each byte
        ;; except first byte where it's high nibble

        ldy     #4              ; 4 bits per digit
        bne     read_status     ; always; no waiting on first iteration
read_loop:

delay := * + 1
        lda     #kDefaultDelay
:       dec                     ; 65C02
        bne     :-

read_status:
acia_status_patch1 := * + 1
        lda     PORT2_ACIA_STATUS
        rol     a               ; shift out bit 5
        rol     a
        rol     a
        ror     digits_buffer,x ; and into digits
        lsr     digits_buffer+1,x
        dey
        bne     read_loop
        ldy     #4              ; 4 bits per digit
        dex
        bpl     read_loop

        ;; ------------------------------
        ;; Restore COMMAND register
        pla
acia_command_patch3 := * + 1
        sta     PORT2_ACIA_COMMAND

.endproc
        .assert .sizeof(Patch0) = kPatchLength, error, "Patch length"
        Patch0_acia_command_patch1 := Patch0::acia_command_patch1
        Patch0_acia_command_patch2 := Patch0::acia_command_patch2
        Patch0_acia_command_patch3 := Patch0::acia_command_patch3
        Patch0_delay := Patch0::delay
        Patch0_acia_status_patch1 := Patch0::acia_status_patch1
        ;; ...Patch applied to here.
        ;; --------------------------------------------------

        ;; digits_buffer $200...$207 now has "MMDDhhmm",
        ;; each digit (?) in the low nibble of a separate byte
        ;; except first byte where it's in high nibble
        ;;                             M        D---D   h---h   m---m
        ;; e.g.   1/2 03:04 would be: $10 $00 $00 $02 $00 $03 $00 $04
        ;; e.g. 12/31 23:59 would be: $C0 $00 $03 $01 $02 $03 $05 $09

        ;; --------------------------------------------------
        ;; Process fields (two digits/bytes at a time)

        ;; 6 and 7 (minute) --> TIMELO
        ;; 4 and 5 (hour)   --> TIMEHI
        ;; 2 and 3 (day)    --> DATELO

        ldx     #6
digit_loop:
        lda     digits_buffer+1,x ; ones place

:       dec     digits_buffer,x ; tens place
        bmi     :+
        clc
        adc     #10
        bcc     :-
:
        ;; A now holds binary value

        offset_table_addr := * + 1
        ldy     time_offset_table,x
        sta     DATELO - OPC_RTS,y ; y is offset by $60 for RTS hack below
        dex
        dex
        bne     digit_loop

        ;; Now TIMELO holds minutes, TIMEHI has hours,
        ;; DATELO has days. Months has not been processed.

        ;; --------------------------------------------------
        ;; Assign month in DATELO/DATEHI

L22BA:  lda     digits_buffer   ; top nibble is month (mmmm0000)
        asl     a               ; C=MSB of month
        and     #%11100000      ; DATELO = mmmddddd
        ora     DATELO
        sta     DATELO

        year := * + 1           ; modified by installer
        lda     #(kDefaultYear .mod 100)
        rol     a               ; DATEHI = yyyyyyym, shift in month MSB
        sta     DATELO+1
        ;; --------------------------------------------------

        ;; Convert $207/$208 to ASCII digits at $20E/$02F (Why???)
        ldy     #$01
:       lda     $0208,y
        ora     #$B0            ; '0'|$80
        sta     $020F,y
        dey
        bpl     :-

        ;; TODO: X=0 first time through; does this loop run 256 times?

        dex
        bne     L22BA

        ;; --------------------------------------------------
        ;; Exit driver
        plp                     ; restore interrupt state

        ;; HACK: This RTS=$60 doubles as the first real entry in the
        ;; offset table.
        .assert OPC_RTS = $60, error, "Offset mismatch"
        ;;
        rts

        ;; Offset from MMDDhhmm digits to DATE/TIME fields in
        ;; ProDOS global page. Only the even entries are used
        ;; and months get special handling.

time_offset_table       := * - 3

        .byte   $FF               ; dummy
        .byte   TIMELO+1 - DATELO + OPC_RTS ; offset to hours field
        .byte   $FF               ; dummy
        .byte   TIMELO   - DATELO + OPC_RTS ; offset to minutes field

        ;; End of relocated clock driver

;;; ============================================================

        ;; Patches applied to driver (length $38, at offset 0)

Patches:

;;; ============================================================
;;; Patch 1:
;;;
;;; Driver for Seiko DataGraph via IIe Gameport

.proc Patch1
        ;; Trigger reading
firmware_byte    := * + 1
        lda     $C0E0           ; Set to $C0x0, n=slot+8 - Disk II PHASE0 ???
        lda     CLRAN1

        ldy     #1
        ldx     #22
trigger_loop:
        dex
        bne     trigger_loop
        lda     CLRAN1,y
        ldx     #11
        dey
        bpl     trigger_loop

:       dex
        bne     :-

        ;; ------------------------------
        ;; Read bit out of register
        ;; 1 bit at a time in blocks of 4, giving
        ;; "MMDDhhmmss" in low nibble of each byte
        ;; except first byte where it's high nibble

        ldx     #9              ; # of digits - 1
        ldy     #4              ; 4 bits per digit
        bne     read_register   ; always; no waiting on first iteration

        ;; Wait a bit
read_loop:
        lda     #kDefaultDelay
        sec
:       sbc     #1
        bne     :-

read_register:
        lda     RD63
        rol     a
        ror     digits_buffer,x
        lsr     digits_buffer+1,x
        nop
        dey
        bne     read_loop
        ldy     #4              ; 4 bits per digit
        dex
        bpl     read_loop
.endproc
        .assert .sizeof(Patch1) = kPatchLength, error, "Patch length"
        Patch1_firmware_byte := Patch1::firmware_byte

;;; ============================================================
;;; Patch 2:
;;;
;;; Driver for Seiko DataGraph via IIc Serial Communication Port (???)

.proc Patch2
        lda     PORT2_ACIA_COMMAND ; unused
        nop                     ; could be PHA to save

        ;; ------------------------------
        ;; Unlock COMMAND register
        ldy     #3              ; cycles
        ldx     #22             ; initial delay
        lda     #%00000010
command_loop:
        sta     PORT2_ACIA_COMMAND

:       dex                     ; wait
        bne     :-

        eor     #%00001010
        ldx     #9              ; repeat delay
        dey
        bne     command_loop

        ;; ------------------------------
        ;; Read bit out of STATUS register
        ;; 1 bit at a time in blocks of 4, giving
        ;; "MMDDhhmmss" in low nibble of each byte
        ;; except first byte where it's high nibble

        ldy     #4              ; 4 bits per digit
        bne     read_status     ; always; no waiting on first iteration

read_loop:
delay := * + 1
        lda     #kDefaultDelay
:       dec                     ; 65C02
        bne     :-

read_status:
        lda     PORT2_ACIA_STATUS
        eor     #%00100000      ; invert bit 5
        rol     a               ; shift out bit 5
        rol     a
        rol     a
        ror     digits_buffer,x ; and into digits
        lsr     digits_buffer+1,x
        dey
        bne     read_loop
        ldy     #4              ; 4 bits per digit
        dex
        bpl     read_loop

        ;; Padding
        nop
        nop
.endproc
        .assert .sizeof(Patch2) = kPatchLength, error, "Patch length"
        Patch2_delay := Patch2::delay

;;; ============================================================
;;; Patch 3:
;;;
;;; Driver for Seiko Datagraph via IIe Gameport (different model ???)

.proc Patch3
        ;; Trigger reading
        lda     SETAN1
        sta     SETAN0
        sta     CLRAN2
        nop
        nop
        nop
        nop
        lda     CLRAN0

        ldx     #21
:       dex                     ; wait
        bne     :-

        ;; ------------------------------
        ;; Read bit out of register
        ;; 1 bit at a time in blocks of 4, giving
        ;; "MMDDhhmmss" in low nibble of each byte
        ;; except first byte where it's high nibble

        ldx     #9              ; # of digits - 1
read_loop:
        ldy     #4              ; bits per digit
bit_loop:
        lda     RD63
        rol     a
        ror     digits_buffer,x
        lsr     digits_buffer+1,x
        sta     SETAN2
        nop
        nop
        nop
        nop
        nop
        nop
        sta     CLRAN2
        dey
        bne     bit_loop
        dex
        bpl     read_loop

        ;; Finish up
        lda     CLRAN1
.endproc
        .assert .sizeof(Patch3) = kPatchLength, error, "Patch length"

;;; ============================================================


.proc InvokeDriver
        jsr     Driver
        rts
.endproc

;;; ------------------------------------------------------------

;;; Prompt, loop until digit key is pressed

.proc GetDigitKey
        jsr     RDKEY
        cmp     #'0' | $80
        bcc     GetDigitKey
        cmp     #('9'+1) | $80
        bcs     GetDigitKey
        jmp     COUT
.endproc

;;; ------------------------------------------------------------

case_mask_table:
        .byte   %11011111       ; map lowercase to uppercase
        .byte   %11111111       ; preserve case

chain:
L239E:
        ;; Relocated to $1000

;;; ============================================================
;;; Chain to next .SYSTEM file
;;; ============================================================

        .org $1000

.proc Chain

entry_ptr               := $00
entry_length            := $02
entries_per_block       := $03
entry_num               := $04
name_length             := $05

        ldy     #$00
        sty     read_block_block_num+1
        iny
        sty     entry_num
        iny
        sty     read_block_block_num
        jsr     ReadBlock
        lda     block_buffer + VolumeDirectoryBlockHeader::entry_length
        sta     entry_length
        lda     block_buffer + VolumeDirectoryBlockHeader::entries_per_block
        sta     entries_per_block
        lda     #<$1C2B
        sta     entry_ptr
        lda     #>$1C2B
        sta     entry_ptr+1

entries_loop:
        ;; SYS file?
        ldy     #FileEntry::file_type
        lda     (entry_ptr),y
        cmp     #FileType::kSYS
        bne     next_entry

        ldy     #0
        lda     (entry_ptr),y
        and     #$30            ; storage_type
        beq     next_entry

        ;; Check name
        lda     (entry_ptr),y
        and     #$0F            ; name_length
        sta     name_length

        ;; Does name have ".SYSTEM" suffix?
        tay
        ldx     #strlen_str_system - 1
:       lda     (entry_ptr),y
        cmp     str_system,x
        bne     next_entry      ; nope, continue
        dey
        dex
        bpl     :-

        ;; Is it "CLOCK.SYSTEM" (i.e. this file)?
        ldy     #strlen_str_clock_system
:       lda     (entry_ptr),y
        cmp     str_clock_system,y
        bne     :+
        dey
        bne     :-
        beq     next_entry      ; match - (but want *next* system file)

:       lda     name_length
        sta     open_pathname
        sta     $0280
        inc     name_length

        ;; Show clock type message
        lda     msg_num
        cmp     #MessageCode::kNoClock
        beq     show_running
        ldy     #MessageCode::kIIc
        jsr     MaybeAddSeikoToMessage

        lda     mach_type
        beq     :+
        iny                     ; k(Seiko)IIc --> k(Seiko)IIe
:       jsr     ShowMessage

show_running:
        ldy     #MessageCode::kRunning
        jsr     ShowMessage

        ;; Copy and print pathname being invoked
        ldy     #1
:       lda     (entry_ptr),y
        sta     open_pathname,y
        sta     $0280,y
        ora     #$80
        jsr     COUT
        iny
        cpy     name_length
        bne     :-

        jsr     LoadSysFile

        ;; Restore text window
        lda     #0
        sta     WNDTOP
        lda     #24
        sta     WNDBTM
        jsr     MON_HOME
        lda     has_80col

        beq     L10A5
        lda     #$15            ; ??? Mousetext?
        jsr     COUT
        lda     #HI(CR)
        jsr     COUT
L10A5:  jmp     PRODOS_SYS_START

next_entry:
        clc
        lda     entry_ptr
        adc     entry_length
        sta     entry_ptr
        lda     entry_ptr+1
        adc     #0
        sta     entry_ptr+1
        inc     entry_num
        lda     entry_num
        cmp     entries_per_block
        bne     L10E2

        ldy     block_buffer + VolumeDirectoryBlockHeader::next_block
        sty     read_block_block_num
        lda     block_buffer + VolumeDirectoryBlockHeader::next_block+1
        sta     read_block_block_num+1
        bne     :+              ; Error if next_block LSB/MSB are both 0
        tya
        bne     :+
        ldy     #MessageCode::kNoSysFile
        jmp     ShowMessageAndChainOrHang ; will hang since not kNoClock

:       jsr     ReadBlock
        lda     #$00
        sta     entry_num
        lda     #$04            ; skip past prev_block/next_block
        sta     entry_ptr
        lda     #>block_buffer
        sta     entry_ptr+1
L10E2:  jmp     entries_loop


.proc MaybeAddSeikoToMessage
        lda     flags
        and     #%00000011
        bne     :+
        ldy     #MessageCode::kSeikoIIc
:       rts
.endproc

.endproc

;;; ------------------------------------------------------------
;;; Call with message number in Y.
;;; Clears screen unless kOkPromptor or kRunning.

.proc ShowMessage
        lda     message_table_lo,y
        sta     ptr
        lda     message_table_hi,y
        sta     ptr+1

        cpy     #MessageCode::kOkPrompt
        beq     :+
        cpy     #MessageCode::kRunning
        beq     :+
        jsr     MON_HOME
:       ldy     #0

        ptr := *+1
loop:   lda     $F000,y
        beq     done
        cmp     #'`'|$80        ; Not lower case?
        bcc     :+
        and     case_mask
:       jsr     COUT
        iny
        bne     loop

done:   rts
.endproc

;;; ------------------------------------------------------------
;;; Write current date/time to a file.

.proc WriteFileInfo
retry:
        lda     #7              ; SET_FILE_INFO count
        sta     file_info_params

        ;; Fill params with current date/time.
        ldy     #3
:       lda     DATELO,y
        sta     file_info_params + GET_FILE_INFO_PARAMS::mod_date,y
        dey
        bpl     :-

        lda     #OPC_RTS        ; Temporarily disable driver
        sta     DATETIME
        PRODOS_CALL MLI_SET_FILE_INFO, file_info_params
        bne     :+
        lda     #OPC_JMP_abs    ; Re-enable driver
        sta     DATETIME
        rts

        ;; Error; maybe retry?

:       cmp     #kErrDiskWriteProtected
        bne     ShowDiskErrorAndChain ; Failed

        ;; Write protected - show error and retry
        ldy     #MessageCode::kRemoveWriteProtect
        jsr     ShowMessage
        jsr     Bell
        jsr     RDKEY
        jmp     retry
.endproc

;;; ------------------------------------------------------------

.proc LoadSysFile
        PRODOS_CALL MLI_OPEN, open_params
        bne     ShowDiskErrorAndChain
        lda     open_params_ref_num
        sta     read_params_ref_num

        PRODOS_CALL MLI_READ, read_params
        bne     ShowDiskErrorAndChain

        PRODOS_CALL MLI_CLOSE, close_params
        bne     ShowDiskErrorAndChain
        rts
.endproc

;;; ------------------------------------------------------------
;;; Set prefix to most recently used device's name, and
;;; get this driver's file info (which holds current year)
;;; On error, displays the error and chains to next file.

.proc SetPrefixAndGetFileInfo
        lda     DEVNUM          ; Most recently accessed device
        sta     on_line_unit_num
        sta     read_block_unit_num

        ;; Get the volume name
        PRODOS_CALL MLI_ON_LINE, on_line_params
        bne     ShowDiskErrorAndChain

        ;; Convert to a path
        lda     on_line_buffer
        and     #$0F            ; mask off length
        tay
        iny
        sty     set_prefix_buffer ; increase length by one...
        lda     #'/'              ; for leading '/'
        sta     on_line_buffer

        ;; Set the prefix
        PRODOS_CALL MLI_SET_PREFIX, set_prefix_params
        bne     ShowDiskErrorAndChain

        ;; And get this file's info
        PRODOS_CALL MLI_GET_FILE_INFO, file_info_params
        bne     ShowDiskErrorAndChain
        rts
.endproc

;;; ------------------------------------------------------------
;;; Read a block
;;; On error, displays the error and chains to next file.

.proc ReadBlock
        PRODOS_CALL MLI_READ_BLOCK, read_block_params
        bne     ShowDiskErrorAndChain
        rts
.endproc

;;; ------------------------------------------------------------

.proc ShowDiskErrorAndChain
        ldy     #MessageCode::kDiskError
        ;; fall through
.endproc

;;; Show message. If kNoClock, then chain to next system file;
;;; otherwise: hang.

.proc ShowMessageAndChainOrHang
        sty     msg_num
        jsr     ShowMessage
        jsr     Bell
        lda     msg_num
        cmp     #MessageCode::kNoClock
        bne     loop
        lda     #OPC_RTS
        sta     DATETIME
        jmp     Chain

loop:   jmp     loop           ; Infinite loop!
.endproc

;;; ------------------------------------------------------------
;;; Make a tone (from ProDOS Technical Reference Manual)
;;; $0C is trashed

.proc Bell

        length  := $0C

        lda     #$20            ; duration of tone
        sta     length
bell1:  lda     #$2             ; short delay...click
        jsr     WAIT
        sta     SPKR
        lda     #$24            ; long delay...click ($20 in TRM)
        jsr     WAIT
        sta     SPKR
        dec     length
        bne     bell1           ; repeat length times
        rts
.endproc

;;; ------------------------------------------------------------
;;; MLI call params

on_line_params:
        .byte   2               ; param_count
on_line_unit_num:
        .byte   $60             ; unit_num
        .addr   on_line_buffer  ; data_buffer

set_prefix_params:
        .byte   1               ; param_count
        .addr   set_prefix_buffer ; pathname

block_buffer = $1C00

read_block_params:
        .byte   3               ; param_count
read_block_unit_num:
        .byte   $60             ; unit_num
        .addr   block_buffer    ; data_buffer
read_block_block_num:
        .word   $0000           ; block_num

open_params:
        .byte   3               ; param_count
        .addr   open_pathname   ; pathname
        .addr   $1C00           ; io_buffer
open_params_ref_num:
        .byte   0               ; ref_num

read_params:
        .byte   4               ; param_count
read_params_ref_num:
        .byte   0                       ; ref_num
        .addr   PRODOS_SYS_START        ; data_buffer
        .word   PRODOS-PRODOS_SYS_START ; request_count
        .word   0                       ; trans_count

close_params:
        .byte   1               ; param_count
        .byte   0               ; ref_num

file_info_params:
        .byte   $A               ; param_count
        .addr   str_clock_system ; pathname
        .byte   0                ; access
        .byte   0                ; file_type
        ;; ...

;;; ------------------------------------------------------------
;;; Misc variables


;;; bit 0/1:    00 - if IIc => Patch2, else => Patch1
;;;             01 - if IIc => Patch0, else => Patch3; Seiko
;;;             10 - Patch0; Seiko
;;;             11 - Patch0; Seiko
;;; ...
;;; bit 7:
flags:  .byte   0

bcd_year:
        .byte   0               ; 2-digit (shared)

L1200:
        ;; buffer for variables, filename
        .res 46, 0

set_prefix_buffer       := $120B
on_line_buffer          := set_prefix_buffer+1
open_pathname           := $121D

str_system:
        .byte   ".SYSTEM"
        strlen_str_system = .strlen(".SYSTEM")

str_clock_system:
        PASCAL_STRING "CLOCK.SYSTEM"
        strlen_str_clock_system = .strlen("CLOCK.SYSTEM")

;;; ------------------------------------------------------------
;;; Message strings

message_table_lo:
        .byte   <msgInstall,<msgNoSysFile,<msgDiskError,<msgIIc
        .byte   <msgIIe,<msgCurrentYear,<msgOkPrompt,<msgNoClock
        .byte   <msgRunning,<msgSeikoIIc,<msgSeikoIIe,<msgRemoveWriteProtect

message_table_hi:
        .byte   >msgInstall,>msgNoSysFile,>msgDiskError,>msgIIc
        .byte   >msgIIe,>msgCurrentYear,>msgOkPrompt,>msgNoClock
        .byte   >msgRunning,>msgSeikoIIc,>msgSeikoIIe,>msgRemoveWriteProtect

msgInstall:
        HIASCII "Install Clock Driver 1.5"
L1272:  HIASCII " "             ; Modified at launch
        HIASCII "\rCopyright (c) 1986 "
        wrap1 := *-1
        HIASCIIZ "Creative Peripherals Unlimited, Inc."

msgNoSysFile:
        HIASCIIZ "Unable to find a '.SYSTEM' file!"

msgRemoveWriteProtect:
        HIASCII "Remove Write-Protect tab, "
        wrap2 := *-1
        HIASCIIZ "Replace disk, and Press a key..."

msgDiskError:
        HIASCIIZ "Disk error! Unable to continue!!!"

msgSeikoIIc:
        HIASCII "Seiko "
msgIIc: HIASCIIZ "//c driver installed. "
        wrap4 := *-2

msgSeikoIIe:
        HIASCII "Seiko "
msgIIe: HIASCIIZ "//e driver installed. "
        wrap5 := *-2

msgCurrentYear:
        HIASCIIZ  "Current year is 19"

msgOkPrompt:
        HIASCII "."
        wrap3 := *
        HIASCIIZ "    OK? (Y/N) "

msgNoClock:
        HIASCIIZ "No clock! Driver not installed...\r"

msgRunning:
        HIASCIIZ "Running "

        ;; Signature for end of range to copy to $1000
        .byte   $FF,$FF

;;; ============================================================


        .byte   $00,$04,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$00,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$00
        .byte   $FF,$00,$FF,$00,$FF,$00,$FF,$65
        .byte   $00,$FF,$00,$FF,$00,$FF,$00,$FF
        .byte   $00,$FF,$00,$FF,$00,$FF,$00,$FF
        .byte   $00,$FF,$00,$FF,$00,$FF,$00,$FF
        .byte   $00
