        .include "opcodes.inc"
        .include "apple2.inc"
        .include "common.inc"
        .feature string_escapes

        .org $2000
        .setcpu "65C02"

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


SLOT3_FIRMWARE           := $C300

PORT2_ACIA_STATUS  := $C0A9
PORT2_ACIA_COMMAND := $C0AA
PORT2_ACIA_CONTROL := $C0AB

DISXY           := $C058
ENBXY           := $C059
DISVBL          := $C05A
ENVBL           := $C05B
X0EDGE1         := $C05C
X0EDGE2         := $C05D

MOUSE_BTN          := $C063

kClockRoutineMaxLength = 125    ; Per ProDOS 8 TRM

L2000:
L2001           := * + 1
        lda     #$06
L2003           := * + 1        ; Patched location for ...?
        lda     #$A0
        cmp     #$CC
        bne     L2013

        sta     L239E + (L1272 - L1000) ; In relocated code
        lda     #$5C
        sta     L2285
        sta     L2336
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
        ldy     #$00
        ldx     #$00
        lda     MACHID
        and     #%10001000      ; IIe or later, modifier
        beq     L2035
        inx
L2035:  cmp     #%10001000      ; IIc ?
        beq     is_iic
        lda     L2003
        cmp     #$CC
        beq     is_iic
        iny
is_iic: sty     $07
        lda     L239C,x
        sta     $08


        ;; Check 40/80 columns; wrap strings if 40 columns.
        lda     MACHID
        and     #%00000010      ; 80 Column card?
        lsr     a
        sta     $09
        bne     init_80_col

        lda     #CR|$80

        ;; Convert spaces to newlines if 40 Columns
        sta     chain + (wrap1 - L1000)
        sta     chain + (wrap4 - L1000)
        sta     chain + (wrap5 - L1000)
        sta     chain + (wrap3 - L1000)
        sta     chain + (wrap2 - L1000)

        inc     L2095
        inc     L2099
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

L2072           := * + 2
        lda     L239E,y
        cmp     #$FF
        bne     L207E

L2079           := * + 2
        ldx     L239E+1,y
        cpx     #$FF
        beq     L208F
L207E:
L2080           := * + 2
        sta     L1000,y
        iny
        bne     loop

        inc     L2072
        inc     L2079
        inc     L2080

        bne     loop            ; always
.endproc


L208F:  ldy     #MessageCode::kInstall
        jsr     ShowMessage

L2095           := * + 1
        ldy     #$03
        sty     $22
L2099           := * + 1
        ldy     #$05
        sty     $23
        jsr     L116E
        lda     $1204
        lsr     a
        sta     L22C7
        jsr     L222B
        sta     year
        ldx     DEVCNT
L20AF:  lda     DEVLST,x
        and     #$0F
        beq     L20C8
        dex
        bpl     L20AF
        lda     L2001
        and     #$07
        bne     L20C2
        lda     #$06
L20C2:  asl     a
        asl     a
        asl     a
        asl     a
        bne     L20CB

kPatchLength = $38
kPatch1Offset = $0
kPatch2Offset = $38
kPatch3Offset = $70


L20C8:  lda     DEVLST,x
L20CB:  and     #%01110000      ; slot
        ora     #$80
        sta     L22E3           ; Set $C0nn address
        lda     L11FE
        and     #$03
        beq     L20E3
        cmp     #$02
        bcs     L20FE
        lda     $07
        bne     L20EE
        beq     L20FE
L20E3:  ldy     #kPatch2Offset
        lda     $07
        beq     apply_patch
        ldy     #kPatch1Offset
        jmp     apply_patch

        ;; Patch bytes on top of driver
L20EE:  ldy     #kPatch3Offset

apply_patch:
        ldx     #$00
:       lda     Patches,y
        sta     L2269,x
        iny
        inx
        cpx     #kPatchLength
        bne     :-

L20FE:  lda     #$02
        sta     L210F
L2103:  jsr     L223F
        bcs     L2111
        dec     L210F
        bpl     L2103
        bmi     L2156

L210F:  .byte   0
L2110:  .byte   0

L2111:  lda     $07
        bne     L2135
        lda     L2110
        bne     L2135
        inc     L2110
        ldy     #$99
        sty     L228A
        iny
        sty     L226A
        sty     L2274
        lda     L11FE
        and     #$03
        beq     L20FE
        sty     L229F
        bne     L20FE

L2135:  ldy     #MessageCode::kNoClock
        sty     $0B
        lda     MACHID          ; Check for clock card
        ror     a
        bcc     no_clock        ; Bit 0 = 0 means no clock card

        jsr     MON_HOME
        jmp     L1000

no_clock:
        lda     #0
        sta     DATELO
        sta     DATELO+1
        sta     TIMELO
        sta     TIMELO+1
        jmp     ShowMessageAndMaybeChain

        ;; --------------------------------------------------


L2156:  lda     #OPC_JMP_abs
        sta     DATETIME
        lda     MACHID
        ora     #%00000001      ; has clock
        sta     MACHID
        bit     KBD
        bmi     L218C
        lda     $0A
        cmp     #$0B
        bcc     L2176
        bit     L11FE
        bpl     L2176
        sta     $1204
L2176:  lda     $1204
        lsr     a
        cmp     #$56
        bcc     L218C
        lda     DATELO
        cmp     $1203
        lda     DATELO+1
        sbc     $1204
        bcs     L21DF
L218C:  bit     KBDSTRB
        rol     L11FE
        lda     #$03
        cmp     $0A
        ror     L11FE

        ;; Show current year prompt
L2199:  ldy     #MessageCode::kCurrentYear
        jsr     ShowMessage

        lda     year           ; 2-digit year
        jsr     PRBYTE

        ldy     #MessageCode::kOkPrompt
        jsr     ShowMessage

        ;; Wait for keypress
:       jsr     RDKEY
        and     #%11011111      ; lowercase --> uppercase
        cmp     #'Y'|$80
        beq     L21D3
        cmp     #'N'|$80
        bne     :-

        ;; Prompt for two digit year
        ldy     #MessageCode::kCurrentYear
        jsr     ShowMessage
        jsr     GetDigitKey     ; Decade
        asl     a
        asl     a
        asl     a
        asl     a
        sta     year
        jsr     GetDigitKey     ; Year
        and     #$0F
        ora     year
        sta     year

        jmp     L2199

        ;; Current year is okay
L21D3:  lda     $1204
        jsr     L2210
        jsr     L238A
        jsr     L111A
L21DF:  lda     RWRAM1
        lda     RWRAM1
        lda     DATETIME+1
        sta     install_ptr
        clc
        adc     #$76
        sta     L22B1
        lda     DATETIME+2
        sta     install_ptr+1
        adc     #0
        sta     L22B2

        ;; Relocate clock driver
        ldy     #kClockRoutineMaxLength - 1
:       lda     Driver,y
install_ptr := * + 1
        sta     $F000,y
        dey
        bpl     :-

        ;; Initialize the time (via driver)
        jsr     DATETIME
        lda     ROMIN2

        ;; Chain
        jmp     L1000

L2210:  lda     year
        pha
        lsr     a
        lsr     a
        lsr     a
        lsr     a
        sta     $06
        asl     a
        asl     a
        adc     $06
        asl     a
        sta     $06
        pla
        and     #$0F
        clc
        adc     $06
        sta     L22C7
        rts

L222B:  ldx     #$FF
L222D:  inx
        sec
        sbc     #$0A
        bcs     L222D
        adc     #$0A
        sta     $06
        txa
        asl     a
        asl     a
        asl     a
        asl     a
        ora     $06
        rts

L223F:  jsr     L238A
        lda     DATELO+1
        ror     a
        lda     DATELO
        rol     a
        rol     a
        rol     a
        rol     a
        and     #$0F
        sec
        beq     L2264
        cmp     #$0D
        bcs     L2264
        sta     $0A
        lda     TIMELO+1
        cmp     #$18
        bcs     L2264
        lda     TIMELO
        cmp     #$3C
L2264:  rts

;;; ============================================================
;;; Clock Driver (Relocatable)
;;; ============================================================

Driver: cld
        cld                     ; TODO: Remove duplicate CLD
        php
        sei
L2269:
L226A           := * + 1
        lda     PORT2_ACIA_COMMAND
        pha                     ; Save command register
        ldy     #$03
        ldx     #$16
        lda     #%00001000
L2273:
L2274           := * + 1
        sta     PORT2_ACIA_COMMAND
L2276:  dex
        bne     L2276
        eor     #$0A
        ldx     #$09
        dey
        bne     L2273
        ldy     #$04
        bne     L2289
L2284:
L2285           := * + 1
        lda     #$5D
L2286:  dec                     ; 65C02
        bne     L2286
L2289:
L228A           := * + 1
        lda     PORT2_ACIA_STATUS
        rol     a
        rol     a
        rol     a
        ror     $0200,x
        lsr     $0201,x
        dey
        bne     L2284
        ldy     #$04
        dex
        bpl     L2284

        pla                     ; Restore command register
L229F           := * + 1
        sta     PORT2_ACIA_COMMAND
        ldx     #$06

L22A3:  lda     $0201,x
:       dec     $0200,x
        bmi     L22B0
        clc
        adc     #10
        bcc     :-
L22B0:
L22B1           := * + 1
L22B2           := * + 2
        ldy     L22DB,x
        sta     $BF30,y         ; Modifying DEVLST ???
        dex
        dex
        bne     L22A3

L22BA:  lda     $0200
        asl     a
        and     #$E0
        ora     DATELO
        sta     DATELO
L22C7           := * + 1
        lda     #$56
        rol     a
        sta     DATELO+1
        ldy     #$01
L22CE:  lda     $0208,y
        ora     #$B0
        sta     $020F,y
        dey
        bpl     L22CE
        dex
L22DB           := * + 1
        bne     L22BA
        plp
        rts

        .byte   $FF
        .byte   $63
        .byte   $FF
        .byte   $62

        ;; End of relocated clock driver

;;; ============================================================

        ;; Patches applied to driver (length $38, at offset 0)

Patches:

        ;; Patch #1
patch1:
L22E3           := * + 1
        lda     $C0E0           ; Set to $C0x0, n=slot+8
        lda     DISVBL
        ldy     #$01
        ldx     #$16
L22EC:  dex
        bne     L22EC
        lda     DISVBL,y
        ldx     #$0B
        dey
        bpl     L22EC
L22F7:  dex
        bne     L22F7
        ldx     #$09
        ldy     #$04
        bne     L2307
L2300:  lda     #$5D
        sec
L2303:  sbc     #$01
        bne     L2303
L2307:  lda     MOUSE_BTN
        rol     a
        ror     $0200,x
        lsr     $0201,x
        nop
        dey
        bne     L2300
        ldy     #$04
        dex
        bpl     L2300
        .assert * - patch1 = kPatchLength, error, "Patch length"

        ;; --------------------------------------------------
        ;; Patch #2
patch2:
        .assert * = Patches + kPatch2Offset, error, "Offset changed"

        lda     PORT2_ACIA_COMMAND
        nop
        ldy     #$03
        ldx     #$16
        lda     #%00000010
L2324:  sta     PORT2_ACIA_COMMAND
L2327:  dex
        bne     L2327
        eor     #$0A
        ldx     #$09
        dey
        bne     L2324
        ldy     #$04
        bne     L233A
L2335:
L2336           := * + 1
        lda     #$5D
L2337:  dec                     ; 65C02
        bne     L2337
L233A:  lda     PORT2_ACIA_STATUS
        eor     #$20
        rol     a
        rol     a
        rol     a
        ror     $0200,x
        lsr     $0201,x
        dey
        bne     L2335
        ldy     #$04
        dex
        bpl     L2335

        nop
        nop

        .assert * - patch2 = kPatchLength, error, "Patch length"

        ;; --------------------------------------------------
        ;; Patch #3
patch3:
        .assert * = Patches + kPatch3Offset, error, "Offset changed"

        lda     ENVBL
        sta     ENBXY
        sta     X0EDGE1
        nop
        nop
        nop
        nop
        lda     DISXY
        ldx     #$15
L2364:  dex
        bne     L2364
        ldx     #$09
L2369:  ldy     #$04
L236B:  lda     MOUSE_BTN
        rol     a
        ror     $0200,x
        lsr     $0201,x
        sta     X0EDGE2
        nop
        nop
        nop
        nop
        nop
        nop
        sta     X0EDGE1
        dey
        bne     L236B
        dex
        bpl     L2369
        lda     DISVBL
        .assert * - patch3 = kPatchLength, error, "Patch length"

;;; ============================================================


L238A:  jsr     Driver
        rts

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

L239C:  .byte   $DF
        .byte   $FF


chain:
L239E:
        ;; Relocated to $1000

;;; ============================================================
;;; Chaining code???
;;; ============================================================

        .org $1000

L1000:
        ldy     #$00
        sty     read_block_block_num+1
        iny
        sty     $04
        iny
        sty     read_block_block_num
        jsr     L119F
        lda     $1C23
        sta     $02
        lda     $1C24
        sta     $03
        lda     #$2B
        sta     $00
        lda     #$1C
        sta     $01
L1021:  ldy     #$10
        lda     ($00),y
        cmp     #$FF
        bne     L10A8
        ldy     #$00
        lda     ($00),y
        and     #$30
        beq     L10A8
        lda     ($00),y
        and     #$0F
        sta     $05
        tay
        ldx     #strlen_str_system - 1
L103A:  lda     ($00),y
        cmp     str_system,x
        bne     L10A8
        dey
        dex
        bpl     L103A
        ldy     #strlen_str_clock_system
L1047:  lda     ($00),y
        cmp     str_clock_system,y
        bne     L1053
        dey
        bne     L1047
        beq     L10A8
L1053:  lda     $05
        sta     $121D
        sta     $0280
        inc     $05
        lda     $0B
        cmp     #$07
        beq     L1070
        ldy     #$03
        jsr     L10E5
        lda     $07
        beq     L106D
        iny
L106D:  jsr     ShowMessage
L1070:  ldy     #MessageCode::kRunning
        jsr     ShowMessage
        ldy     #$01
L1077:  lda     ($00),y
        sta     $121D,y
        sta     $0280,y
        ora     #$80
        jsr     COUT
        iny
        cpy     $05
        bne     L1077
        jsr     L114F
        lda     #$00
        sta     WNDTOP
        lda     #$18
        sta     WNDBTM
        jsr     MON_HOME
        lda     $09
        beq     L10A5
        lda     #$15
        jsr     COUT
        lda     #$8D
        jsr     COUT
L10A5:  jmp     L2000

L10A8:  clc
        lda     $00
        adc     $02
        sta     $00
        lda     $01
        adc     #$00
        sta     $01
        inc     $04
        lda     $04
        cmp     $03
        bne     L10E2
        ldy     $1C02
        sty     read_block_block_num
        lda     $1C03
        sta     read_block_block_num+1
        bne     :+
        tya
        bne     :+
        ldy     #MessageCode::kNoSysFile
        jmp     ShowMessageAndMaybeChain

:       jsr     L119F
        lda     #$00
        sta     $04
        lda     #$04
        sta     $00
        lda     #$1C
        sta     $01
L10E2:  jmp     L1021

L10E5:  lda     L11FE
        and     #$03
        bne     L10EE
        ldy     #$09
L10EE:  rts

;;; ------------------------------------------------------------
;;; Call with message number in Y

.proc ShowMessage
        lda     message_table_lo,y
        sta     L1109
        lda     message_table_hi,y
        sta     L1109+1
        cpy     #$06
        beq     L1106
        cpy     #$08
        beq     L1106
        jsr     MON_HOME
L1106:  ldy     #$00

        L1109 := *+1

L1108:  lda     $F000,y
        beq     L1119
        cmp     #$E0
        bcc     L1113
        and     $08
L1113:  jsr     COUT
        iny
        bne     L1108
L1119:  rts
.endproc

;;; ------------------------------------------------------------

L111A:  lda     #$07            ; SET_FILE_INFO count
        sta     file_info_params
        ldy     #$03
L1121:  lda     DATELO,y
        sta     $1203,y
        dey
        bpl     L1121
        lda     #OPC_RTS
        sta     DATETIME
        PRODOS_CALL MLI_SET_FILE_INFO, file_info_params
        bne     :+

        lda     #OPC_JMP_abs
        sta     DATETIME
        rts

:       cmp     #$2B
        bne     ShowDiskErrorAndChain
        ldy     #MessageCode::kRemoveWriteProtect
        jsr     ShowMessage
        jsr     L11C3
        jsr     RDKEY
        jmp     L111A

L114F:  PRODOS_CALL MLI_OPEN, open_params
        bne     ShowDiskErrorAndChain
        lda     open_params_ref_num
        sta     read_params_ref_num

        PRODOS_CALL MLI_READ, read_params
        bne     ShowDiskErrorAndChain

        PRODOS_CALL MLI_CLOSE, close_params
        bne     ShowDiskErrorAndChain
        rts

L116E:  lda     DEVNUM
        sta     on_line_unit_num
        sta     read_block_unit_num

        PRODOS_CALL MLI_ON_LINE, on_line_params
        bne     ShowDiskErrorAndChain

        lda     $120C
        and     #$0F
        tay
        iny
        sty     $120B
        lda     #'/'
        sta     $120C

        PRODOS_CALL MLI_SET_PREFIX, set_prefix_params
        bne     ShowDiskErrorAndChain

        PRODOS_CALL MLI_GET_FILE_INFO, file_info_params
        bne     ShowDiskErrorAndChain
        rts

L119F:  PRODOS_CALL MLI_READ_BLOCK, read_block_params
        bne     ShowDiskErrorAndChain
        rts

;;; ------------------------------------------------------------

.proc ShowDiskErrorAndChain
        ldy     #MessageCode::kDiskError
        ;; fall through
.endproc

;;; Show message and chain to next system file, unless
;;; kNoClock (in which case: hang)

.proc ShowMessageAndMaybeChain
        sty     $0B
        jsr     ShowMessage
        jsr     L11C3
        lda     $0B
        cmp     #MessageCode::kNoClock
        bne     loop
        lda     #OPC_RTS
        sta     DATETIME
        jmp     L1000

loop:   jmp     loop           ; Infinite loop!
.endproc

;;; ------------------------------------------------------------

L11C3:  lda     #$20
        sta     $0C
L11C7:  lda     #$02
        jsr     WAIT
        sta     SPKR
        lda     #$24
        jsr     WAIT
        sta     SPKR
        dec     $0C
        bne     L11C7
        rts

;;; ------------------------------------------------------------
;;; MLI call params

on_line_params:
        .byte   2               ; param_count
on_line_unit_num:
        .byte   $60             ; unit_num
        .addr   $120C           ; data_buffer

set_prefix_params:
        .byte   1               ; param_count
        .addr   $120B           ; pathname

read_block_params:
        .byte   3               ; param_count
read_block_unit_num:
        .byte   $60             ; unit_num
        .addr   $1C00           ; data_buffer
read_block_block_num:
        .word   $0000           ; block_num

open_params:
        .byte   3               ; param_count
        .addr   $121D           ; pathname
        .addr   $1C00           ; io_buffer
open_params_ref_num:
        .byte   0               ; ref_num

read_params:
        .byte   4               ; param_count
read_params_ref_num:
        .byte   0               ; ref_num
        .addr   $2000           ; data_buffer
        .word   $9F00           ; request_count
        .word   0               ; trans_count

close_params:
        .byte   1               ; param_count
        .byte   0               ; ref_num

file_info_params:
        .byte   $A              ; param_count
        .addr   str_clock_system ; pathname
        .byte   0               ; access
        .byte   0               ; file_type
        ;; ...

;;; ------------------------------------------------------------
;;; Misc variables

L11FE:  .byte   0               ; ???
year:   .byte   0               ; 2-digit (shared)

L1200:
        ;; buffer for variables, filename
        .res 46, 0

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
