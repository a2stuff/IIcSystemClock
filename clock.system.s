        .include "opcodes.inc"
        .include "apple2.inc"
        .include "common.inc"
        .feature string_escapes

        .org $2000
        .setcpu "6502"

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

L2000:
L2001           := * + 1
        lda     #$06
L2003           := * + 1
        lda     #$A0
        cmp     #$CC
        bne     L2013
        sta     L239E + (L1272 - L1000) ; In relocated code
        lda     #$5C
        sta     L2285
        sta     L2336
L2013:  ldx     #$FF
        txs
        lda     #$00
        sta     IBAKVER
        sta     IVERSION
        sta     ROMIN2
        lda     $03F3
        eor     #$FF
        sta     $03F4
        ldy     #$00
        ldx     #$00
        lda     MACHID
        and     #%10001000      ; IIe or later, modifier
        beq     L2035
        inx
L2035:  cmp     #$88
        beq     L2041
        lda     L2003
        cmp     #$CC
        beq     L2041
        iny
L2041:  sty     $07
        lda     L239C,x
        sta     $08
        lda     MACHID
        and     #%00000010      ; 80 Column card?
        lsr     a
        sta     $09
        bne     L206B

        lda     #$8D

        ;; Convert spaces to newlines if 40 Columns
        sta     chain + (wrap1 - L1000)
        sta     chain + (wrap4 - L1000)
        sta     chain + (wrap5 - L1000)
        sta     chain + (wrap3 - L1000)
        sta     chain + (wrap2 - L1000)

        inc     L2095
        inc     L2099
        bne     L206E

L206B:  jsr     SLOT3_FIRMWARE


        ;; --------------------------------------------------

        ;; Copy to $1000

L206E:  ldy     #0

        ;; End signature is two adjacent $FFs
L2070:
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
        bne     L2070

        inc     L2072
        inc     L2079
        inc     L2080

        bne     L2070           ; always

L208F:  ldy     #$00
        jsr     L10EF
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
        sta     L11FF
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
L20E3:  ldy     #$38            ; offset into patch #1
        lda     $07
        beq     L20F0
        ldy     #$00            ; offset into patch #2
        jmp     L20F0

        ;; Patch bytes on top of driver
L20EE:  ldy     #$70            ; offset into patch #3
L20F0:  ldx     #$00
L20F2:  lda     L22E2,y
        sta     L2269,x
        iny
        inx
        cpx     #$38
        bne     L20F2

L20FE:  lda     #$02
        sta     L210F
L2103:  jsr     L223F
        bcs     L2111
        dec     L210F
        bpl     L2103
        bmi     L2156
L210F:  brk
L2110:  brk
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
L2135:  ldy     #$07
        sty     $0B
        lda     MACHID
        ror     a
        bcc     L2145
        jsr     MON_HOME
        jmp     L1000

        ;; --------------------------------------------------

L2145:  lda     #$00
        sta     DATELO
        sta     DATELO+1
        sta     TIMELO
        sta     TIMELO+1
        jmp     L11AA

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
L2199:  ldy     #$05
        jsr     L10EF
        lda     L11FF
        jsr     PRBYTE
        ldy     #$06
        jsr     L10EF
L21A9:  jsr     RDKEY
        and     #$DF
        cmp     #$D9
        beq     L21D3
        cmp     #$CE
        bne     L21A9
        ldy     #$05
        jsr     L10EF
        jsr     L238E
        asl     a
        asl     a
        asl     a
        asl     a
        sta     L11FF
        jsr     L238E
        and     #$0F
        ora     L11FF
        sta     L11FF
        jmp     L2199

L21D3:  lda     $1204
        jsr     L2210
        jsr     L238A
        jsr     L111A
L21DF:  lda     RWRAM1
        lda     RWRAM1
        lda     DATETIME+1
        sta     L2202
        clc
        adc     #$76
        sta     L22B1
        lda     DATETIME+2
        sta     L2203
        adc     #0
        sta     L22B2

        ;; Relocate clock driver

        ldy     #$7C
L21FE:  lda     L2265,y
L2202           := * + 1
L2203           := * + 2
        sta     $F000,y
        dey
        bpl     L21FE
        jsr     DATETIME
        lda     ROMIN2
        jmp     L1000

L2210:  lda     L11FF
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

L2265:  cld
        cld
        php
        sei
L2269:
L226A           := * + 1
        lda     PORT2_ACIA_COMMAND
        pha
        ldy     #$03
        ldx     #$16
        lda     #$08
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
L2286:  .byte   $3A
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
        pla
L229F           := * + 1
        sta     PORT2_ACIA_COMMAND
        ldx     #$06
L22A3:  lda     $0201,x
L22A6:  dec     $0200,x
        bmi     L22B0
        clc
        adc     #$0A
        bcc     L22A6
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

        ;; Patch #1
patch1:
L22E2:
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
        .assert * - patch1 = $38, error, "Patch length"

        ;; --------------------------------------------------
        ;; Patch #2
patch2:
        .assert * - $38 = L22E2, error, "Offset changed"

        lda     PORT2_ACIA_COMMAND
        nop
        ldy     #$03
        ldx     #$16
        lda     #$02
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
L2337:  .byte   $3A
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

        .assert * - patch2 = $38, error, "Patch length"

        ;; --------------------------------------------------
        ;; Patch #3
patch3:
        .assert * - $70 = L22E2, error, "Offset changed"

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
        .assert * - patch3 = $38, error, "Patch length"

;;; ============================================================


L238A:  jsr     L2265
        rts

L238E:  jsr     RDKEY
        cmp     #'0' | $80
        bcc     L238E
        cmp     #('9'+1) | $80
        bcs     L238E
        jmp     COUT

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
        sty     L11E8
        iny
        sty     $04
        iny
        sty     L11E7
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
L106D:  jsr     L10EF
L1070:  ldy     #$08
        jsr     L10EF
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
        sty     L11E7
        lda     $1C03
        sta     L11E8
        bne     L10D3
        tya
        bne     L10D3
        ldy     #$01
        jmp     L11AA

L10D3:  jsr     L119F
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

L10EF:  lda     $1242,y
        sta     L1109
        lda     L124E,y
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

L111A:  lda     #$07
        sta     L11F9
        ldy     #$03
L1121:  lda     DATELO,y
        sta     $1203,y
        dey
        bpl     L1121
        lda     #OPC_RTS
        sta     DATETIME
        PRODOS_CALL MLI_SET_FILE_INFO, L11F9
        bne     :+

        lda     #OPC_JMP_abs
        sta     DATETIME
        rts

:       cmp     #$2B
        bne     L11A8
        ldy     #$0B
        jsr     L10EF
        jsr     L11C3
        jsr     RDKEY
        jmp     L111A

L114F:  PRODOS_CALL MLI_OPEN, $11E9
        bne     L11A8
        lda     L11EE
        sta     L11F0

        PRODOS_CALL MLI_READ, $11EF
        bne     L11A8

        PRODOS_CALL MLI_CLOSE, $11F7
        bne     L11A8
        rts

L116E:  lda     DEVNUM
        sta     L11DD
        sta     L11E4

        PRODOS_CALL MLI_ON_LINE, $11DC
        bne     L11A8

        lda     $120C
        and     #$0F
        tay
        iny
        sty     $120B
        lda     #'/'
        sta     $120C

        PRODOS_CALL MLI_SET_PREFIX, $11E0
        bne     L11A8

        PRODOS_CALL MLI_GET_FILE_INFO, L11F9
        bne     L11A8
        rts

L119F:  PRODOS_CALL MLI_READ_BLOCK, $11E3
        bne     L11A8
        rts

L11A8:  ldy     #$02
L11AA:  sty     $0B
        jsr     L10EF
        jsr     L11C3
        lda     $0B
        cmp     #$07
        bne     L11C0
        lda     #OPC_RTS
        sta     DATETIME
        jmp     L1000

L11C0:  jmp     L11C0

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

        .byte   $02
L11DD:  .byte   $60

        .byte   $0C
        .byte   $12
        .byte   $01,$0B
        .byte   $12,$03
L11E4:  .byte   $60
        .byte   $00,$1C
L11E7:  .byte   $00
L11E8:  .byte   $00
        .byte   $03
        .byte   $1D,$12,$00,$1C
L11EE:  .byte   $00
        .byte   $04
L11F0:  .byte   $00
        .byte   $00
        .byte   $20,$00,$9F,$00,$00,$01,$00
L11F9:  .byte   $0A
        .byte   $35,$12

L11FC:  .byte   0
L11FD:  .byte   0
L11FE:  .byte   0
L11FF:  .byte   0
        .res 46, 0

str_system:
        .byte   ".SYSTEM"
        strlen_str_system = .strlen(".SYSTEM")

str_clock_system:
        PASCAL_STRING "CLOCK.SYSTEM"
        strlen_str_clock_system = .strlen("CLOCK.SYSTEM")

        .byte   $5A,$AC,$08,$30,$4D,$64,$77,$87
        .byte   $AA,$2A,$47,$CD
L124E:  .byte   $12,$12,$13,$13
        .byte   $13,$13,$13,$13,$13,$13,$13,$12

        HIASCII "Install Clock Driver 1.5"

L1272:  .byte   $A0

        HIASCII "\rCopyright (c) 1986 "
        wrap1 := *-1
        HIASCIIZ "Creative Peripherals Unlimited, Inc."

        HIASCIIZ "Unable to find a '.SYSTEM' file!"

        HIASCII "Remove Write-Protect tab, "
        wrap2 := *-1
        HIASCIIZ "Replace disk, and Press a key..."

        HIASCIIZ "Disk error! Unable to continue!!!"
        HIASCIIZ "Seiko //c driver installed. "
        wrap4 := *-2

        HIASCIIZ "Seiko //e driver installed. "
        wrap5 := *-2

        HIASCII  "Current year is 19"
        .byte    0, $AE
        wrap3 := *
        HIASCIIZ "    OK? (Y/N) "
        HIASCIIZ "No clock! Driver not installed...\r"
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
