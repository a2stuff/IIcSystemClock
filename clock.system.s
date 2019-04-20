        .include "opcodes.inc"
        .include "apple2.inc"
        .include "common.inc"
        .feature string_escapes

        .org $2000
        .setcpu "6502"

L068D           := $068D
L1000           := $1000
L1021           := $1021
L10E5           := $10E5
L10EF           := $10EF
L111A           := $111A
L114F           := $114F
L116E           := $116E
L119F           := $119F
L11AA           := $11AA
L11C0           := $11C0
L11C3           := $11C3
LC300           := $C300

L2000:
L2001           := * + 1
        lda     #$06
L2003           := * + 1
        lda     #$A0
        cmp     #$CC
        bne     L2013
        sta     L2610
        lda     #$5C
        sta     L2285
        sta     L2336
L2013:  ldx     #$FF
        txs
        lda     #$00
        sta     $BFFC
        sta     $BFFD
        sta     ROMIN2
        lda     $03F3
        eor     #$FF
        sta     $03F4
        ldy     #$00
        ldx     #$00
        lda     $BF98
        and     #$88
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
        lda     $BF98
        and     #$02
        lsr     a
        sta     $09
        bne     L206B
        lda     #$8D
        sta     $2624           ; TODO: modifying string resource?
        sta     $26E3           ; ???
        sta     $2700           ; ???
        sta     $2716           ; ???
        sta     $2684           ; ???
        inc     L2095
        inc     L2099
        bne     L206E
L206B:  jsr     LC300
L206E:  ldy     #$00
L2070:
L2072           := * + 2
        lda     L239E,y
        cmp     #$FF
        bne     L207E
L2079           := * + 2
        ldx     L239F,y
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
        bne     L2070
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
        sta     $11FF
        ldx     $BF31
L20AF:  lda     $BF32,x
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
L20C8:  lda     $BF32,x
L20CB:  and     #$70
        ora     #$80
        sta     L22E3
        lda     $11FE
        and     #$03
        beq     L20E3
        cmp     #$02
        bcs     L20FE
        lda     $07
        bne     L20EE
        beq     L20FE
L20E3:  ldy     #$38
        lda     $07
        beq     L20F0
        ldy     #$00
        jmp     L20F0

L20EE:  ldy     #$70
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
        lda     $11FE
        and     #$03
        beq     L20FE
        sty     L229F
        bne     L20FE
L2135:  ldy     #$07
        sty     $0B
        lda     $BF98
        ror     a
        bcc     L2145
        jsr     MON_HOME
        jmp     L1000

L2145:  lda     #$00
        sta     $BF90
        sta     $BF91
        sta     $BF92
        sta     $BF93
        jmp     L11AA

L2156:  lda     #OPC_JMP_abs
        sta     DATETIME
        lda     $BF98
        ora     #$01
        sta     $BF98
        bit     $C000
        bmi     L218C
        lda     $0A
        cmp     #$0B
        bcc     L2176
        bit     $11FE
        bpl     L2176
        sta     $1204
L2176:  lda     $1204
        lsr     a
        cmp     #$56
        bcc     L218C
        lda     $BF90
        cmp     $1203
        lda     $BF91
        sbc     $1204
        bcs     L21DF
L218C:  bit     $C010
        rol     $11FE
        lda     #$03
        cmp     $0A
        ror     $11FE
L2199:  ldy     #$05
        jsr     L10EF
        lda     $11FF
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
        sta     $11FF
        jsr     L238E
        and     #$0F
        ora     $11FF
        sta     $11FF
        jmp     L2199

L21D3:  lda     $1204
        jsr     L2210
        jsr     L238A
        jsr     L111A
L21DF:  lda     $C08B
        lda     $C08B
        lda     $BF07
        sta     L2202
        clc
        adc     #$76
        sta     L22B1
        lda     $BF08
        sta     L2203
        adc     #$00
        sta     L22B2
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

L2210:  lda     $11FF
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
        lda     $BF91
        ror     a
        lda     $BF90
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
        lda     $BF93
        cmp     #$18
        bcs     L2264
        lda     $BF92
        cmp     #$3C
L2264:  rts

L2265:  cld
        cld
        php
        sei
L2269:
L226A           := * + 1
        lda     $C0AA
        pha
        ldy     #$03
        ldx     #$16
        lda     #$08
L2273:
L2274           := * + 1
        sta     $C0AA
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
        lda     $C0A9
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
        sta     $C0AA
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
        sta     $BF30,y
        dex
        dex
        bne     L22A3
L22BA:  lda     $0200
        asl     a
        and     #$E0
        ora     $BF90
        sta     $BF90
L22C7           := * + 1
        lda     #$56
        rol     a
        sta     $BF91
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
L22E2:
L22E3           := * + 1
        lda     $C0E0
        lda     $C05A
        ldy     #$01
        ldx     #$16
L22EC:  dex
        bne     L22EC
        lda     $C05A,y
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
L2307:  lda     $C063
        rol     a
        ror     $0200,x
        lsr     $0201,x
        nop
        dey
        bne     L2300
        ldy     #$04
        dex
        bpl     L2300
        lda     $C0AA
        nop
        ldy     #$03
        ldx     #$16
        lda     #$02
L2324:  sta     $C0AA
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
L233A:  lda     $C0A9
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
        lda     $C05B
        sta     $C059
        sta     $C05C
        nop
        nop
        nop
        nop
        lda     $C058
        ldx     #$15
L2364:  dex
        bne     L2364
        ldx     #$09
L2369:  ldy     #$04
L236B:  lda     $C063
        rol     a
        ror     $0200,x
        lsr     $0201,x
        sta     $C05D
        nop
        nop
        nop
        nop
        nop
        nop
        sta     $C05C
        dey
        bne     L236B
        dex
        bpl     L2369
        lda     $C05A
L238A:  jsr     L2265
        rts

L238E:  jsr     RDKEY
        cmp     #$B0
        bcc     L238E
        cmp     #$BA
        bcs     L238E
        jmp     COUT

L239C:  .byte   $DF
        .byte   $FF
L239E:
L239F           := * + 1
        ldy     #$00
        sty     $11E8
        iny
        sty     $04
        iny
        sty     $11E7
        jsr     L119F
        lda     $1C23
        sta     $02
        lda     $1C24
        sta     $03
        lda     #$2B
        sta     $00
        lda     #$1C
        sta     $01
        ldy     #$10
        lda     ($00),y
        cmp     #$FF
        bne     L2446
        ldy     #$00
        lda     ($00),y
        and     #$30
        beq     L2446
        lda     ($00),y
        and     #$0F
        sta     $05
        tay
        ldx     #$06
L23D8:  lda     ($00),y
        cmp     $122E,x
        bne     L2446
        dey
        dex
        bpl     L23D8
        ldy     #$0C
L23E5:  lda     ($00),y
        cmp     $1235,y
        bne     L23F1
        dey
        bne     L23E5
        beq     L2446
L23F1:  lda     $05
        sta     $121D
        sta     $0280
        inc     $05
        lda     $0B
        cmp     #$07
        beq     L240E
        ldy     #$03
        jsr     L10E5
        lda     $07
        beq     L240B
        iny
L240B:  jsr     L10EF
L240E:  ldy     #$08
        jsr     L10EF
        ldy     #$01
L2415:  lda     ($00),y
        sta     $121D,y
        sta     $0280,y
        ora     #$80
        jsr     COUT
        iny
        cpy     $05
        bne     L2415
        jsr     L114F
        lda     #$00
        sta     $22
        lda     #$18
        sta     $23
        jsr     MON_HOME
        lda     $09
        beq     L2443
        lda     #$15
        jsr     COUT
        lda     #$8D
        jsr     COUT
L2443:  jmp     L2000

L2446:  clc
        lda     $00
        adc     $02
        sta     $00
        lda     $01
        adc     #$00
        sta     $01
        inc     $04
        lda     $04
        cmp     $03
        bne     L2480
        ldy     $1C02
        sty     $11E7
        lda     $1C03
        sta     $11E8
        bne     L2471
        tya
        bne     L2471
        ldy     #$01
        jmp     L11AA

L2471:  jsr     L119F
        lda     #$00
        sta     $04
        lda     #$04
        sta     $00
        lda     #$1C
        sta     $01
L2480:  jmp     L1021

        lda     $11FE
        and     #$03
        bne     L248C
        ldy     #$09
L248C:  rts

        lda     $1242,y
        sta     $1109
        lda     $124E,y
        sta     $110A
        cpy     #$06
        beq     L24A4
        cpy     #$08
        beq     L24A4
        jsr     MON_HOME
L24A4:  ldy     #$00
L24A6:  lda     $F000,y
        beq     L24B7
        cmp     #$E0
        bcc     L24B1
        and     $08
L24B1:  jsr     COUT
        iny
        bne     L24A6
L24B7:  rts

        lda     #$07
        sta     $11F9
        ldy     #$03
L24BF:  lda     $BF90,y
        sta     $1203,y
        dey
        bpl     L24BF
        lda     #OPC_RTS
        sta     DATETIME
        jsr     PRODOS
        .byte   $C3
        sbc     $D011,y
        asl     $A9
        jmp     L068D

        .byte   $BF
        rts

        cmp     #$2B
        bne     L2546
        ldy     #$0B
        jsr     L10EF
        jsr     L11C3
        jsr     RDKEY
        jmp     L111A

        jsr     PRODOS
        iny
        sbc     #$11
        bne     L2546
        lda     $11EE
        sta     $11F0
        jsr     PRODOS
        dex
        .byte   $EF
        ora     ($D0),y
        .byte   $43
        jsr     PRODOS
        cpy     $11F7
        bne     L2546
        rts

        lda     $BF30
        sta     $11DD
        sta     $11E4
        jsr     PRODOS
        cmp     $DC
        ora     ($D0),y
        and     #$AD
        .byte   $0C
        .byte   $12
        and     #$0F
        tay
        iny
        sty     $120B
        lda     #$2F
        sta     $120C
        jsr     PRODOS
        dec     $E0
        ora     ($D0),y
        .byte   $12
        jsr     PRODOS
        cpy     $F9
        ora     ($D0),y
        asl     a
        rts

        jsr     PRODOS
        .byte   $80
        .byte   $E3
        ora     ($D0),y
        ora     ($60,x)
L2546:  ldy     #$02
        sty     $0B
        jsr     L10EF
        jsr     L11C3
        lda     $0B
        cmp     #$07
        bne     L255E
        lda     #OPC_RTS
        sta     DATETIME
        jmp     L1000

L255E:  jmp     L11C0

        lda     #$20
        sta     $0C
L2565:  lda     #$02
        jsr     WAIT
        sta     $C030
        lda     #$24
        jsr     WAIT
        sta     $C030
        dec     $0C
        bne     L2565
        rts

        .byte   $02
        rts

        .byte   $0C
        .byte   $12
        ora     ($0B,x)
        .byte   $12,$03,$60,$00,$1C,$00,$00,$03
        .byte   $1D,$12,$00,$1C,$00,$04,$00,$00
        .byte   $20,$00,$9F,$00,$00,$01,$00,$0A
        .byte   $35,$12

        .res 50, 0

        .byte   ".SYSTEM"

        PASCAL_STRING "CLOCK.SYSTEM"

        .byte   $5A,$AC,$08,$30,$4D,$64,$77,$87
        .byte   $AA,$2A,$47,$CD,$12,$12,$13,$13
        .byte   $13,$13,$13,$13,$13,$13,$13,$12

        HIASCII "Install Clock Driver 1.5"

L2610:  HIASCIIZ " \rCopyright (c) 1986 Creative Peripherals Unlimited, Inc."
        HIASCIIZ "Unable to find a '.SYSTEM' file!"
        HIASCIIZ "Remove Write-Protect tab, Replace disk, and Press a key..."
        HIASCIIZ "Disk error! Unable to continue!!!"
        HIASCIIZ "Seiko //c driver installed. "
        HIASCIIZ "Seiko //e driver installed. "
        HIASCII  "Current year is 19"
        .byte    0
        HIASCIIZ ".    OK? (Y/N) "
        HIASCIIZ "No clock! Driver not installed...\r"
        HIASCIIZ "Running "

        .byte   $FF,$FF,$00,$04,$00
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
