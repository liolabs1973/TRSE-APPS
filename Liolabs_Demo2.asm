 processor 6502

	ORG $801
	.byte    $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $32,$30,$36,$34
	.byte    $29, $00, $00, $00

	ORG $810

Scroller2x2
	jmp block41
val	dc.b	
pText	= $02
ms_scroll	dc.b	$07
ms_sshift	dc.b	$00
text		dc.b	"AN ANOTHER HORIZONTAL TEXT SCROLLER BY LIOLABS                     "
	dc.b	0
	
	
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
mul16x8_num1Hi = $4C
mul16x8_num1 = $4E
mul16x8_num2 = $50
mul16x8_procedure
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop  ; accumulating multiply entry point (enter with .A=lo, .Y=hi)
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
	
	
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
multiplier = $4C
multiplier_a = $4E
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $4E
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop
	bcc mul_skip
mul_mod
	adc multiplier_a
mul_skip
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end
	txa
	rts
multiply_eightbit18467
	rts
	
	
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
	jmp moveto6334
screenmemory =  $fe
screen_x = $4C
screen_y = $4E
SetScreenPosition
	sta screenmemory+1
	lda #0
	sta screenmemory
	ldy screen_y
	beq sydone
syloop
	clc
	adc #40
	bcc sskip
	inc screenmemory+1
sskip
	dey
	bne syloop
sydone
	ldx screen_x
	beq sxdone
	clc
	adc screen_x
	bcc sxdone
	inc screenmemory+1
sxdone
	sta screenmemory
	rts
moveto6334
	rts
	
	
	; ***********  Defining procedure : MainScreenScrollerEnds
	;    Procedure type : User-defined procedure
	
 ; Temp vars section
 ; Temp vars section ends
MainScreenScrollerEnds

	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; Wait
	ldx #6
	dex
	bne *-1
	; Assigning memory location (poke replacement)
	; Assigning single variable : $d021
	lda #11
	sta $d021
	lda #0
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	; Wait for no of raster lines
	lda #1
	clc 
	adc $d012
	cmp $d012
	bne *-3
	; Wait
	ldx #9
	dex
	bne *-1
	; Assigning memory location (poke replacement)
	; Assigning single variable : $d021
	lda #0
	sta $d021
	; RasterIRQ : Hook a procedure
	lda #130
	sta $d012
	lda #<MainScreen
	sta $fffe
	lda #>MainScreen
	sta $ffff
	; Binary clause Simplified: EQUALS
	lda ms_scroll
	; Compare with pure num / var optimization
	cmp #$1;keep
	bne elsedoneblock29358
ConditionalTrueBlock15724

	; memcpy
	ldx #0
memcpy23281
	lda $4c9+ $00,x
	sta 1224,x
	inx
	cpx #39
	bne memcpy23281
	; memcpy
	ldx #0
memcpy16827
	lda $4f1+ $00,x
	sta 1264,x
	inx
	cpx #39
	bne memcpy16827

elseblock11478
elsedoneblock29358
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla

	rti
	
	
	; ***********  Defining procedure : CalculatePositionInCharset
	;    Procedure type : User-defined procedure
	
cc_val	dc.b	
block9961
CalculatePositionInCharset

	; Binary clause Simplified: EQUALS
	lda cc_val
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne elsedoneblock4827
ConditionalTrueBlock2995
	lda #160
	rts
elseblock11942
elsedoneblock4827
	; Binary clause Simplified: GREATER
	lda cc_val
	; Compare with pure num / var optimization
	cmp #$40;keep
	bcc elsedoneblock292
	beq elsedoneblock292
ConditionalTrueBlock3902
	; Assigning single variable : cc_val
	; Optimizer: a = a +/- b
	lda cc_val
	sec
	sbc #65
	sta cc_val
elseblock153
elsedoneblock292
	; Binary clause Simplified: GREATER
	lda cc_val
	; Compare with pure num / var optimization
	cmp #$10;keep
	bcc elsedoneblock5447
	beq elsedoneblock5447
ConditionalTrueBlock19718
	; Assigning single variable : cc_val
	; Optimizer: a = a +/- b
	lda cc_val
	clc
	adc #24
	sta cc_val
elseblock19895
elsedoneblock5447
	; Assigning single variable : cc_val
	; Right is PURE NUMERIC
	; 8 bit mul of power 2
	
	lda cc_val
	
	asl
	
	sta cc_val
	; Assigning single variable : cc_val
	; 8 bit binop
	; Add/sub where right value is constant number
	clc
	adc ms_sshift
	 ; end add / sub var with constant
	
	sta cc_val
	rts

	rts
	
	
	; ***********  Defining procedure : UpdateTextScroll
	;    Procedure type : User-defined procedure
	
UpdateTextScroll

	; Assigning single variable : ms_scroll
	lda #7
	sta ms_scroll
	inc ms_sshift
	; Binary clause Simplified: EQUALS
	lda ms_sshift
	; Compare with pure num / var optimization
	cmp #$2;keep
	bne elsedoneblock26299
ConditionalTrueBlock19912

	; Assigning single variable : ms_sshift
	lda #0
	sta ms_sshift
	; Assigning single variable : pText
	; WORD optimization: a=a+b
	lda pText+0
	
	clc
	adc #1
	bcc WordAdd7711
	inc pText+1
WordAdd7711
	sta pText+0
	; Binary clause Simplified: EQUALS
	; Load pointer array
	ldy #$0
	lda (pText),y
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne elsedoneblock27644
ConditionalTrueBlock6868
	; Assigning single variable : pText
	lda #<text
	ldx #>text
	sta pText
	stx pText+1
elseblock25547
elsedoneblock27644

elseblock25667
elsedoneblock26299

	rts
	
	
	; ***********  Defining procedure : DrawText
	;    Procedure type : User-defined procedure
	
DrawText

	; Binary clause Simplified: GREATER
	lda ms_scroll
	; Compare with pure num / var optimization
	cmp #$7f;keep
	bcc elsedoneblock27529
	beq elsedoneblock27529
ConditionalTrueBlock8723

	; MoveTo optimization
	lda #$c8
	sta screenmemory
	lda #$04
	sta screenmemory+1
	; Assigning single variable : val
	; Assigning single variable : cc_val
	; Load pointer array
	ldy #$0
	lda (pText),y
	
	sta cc_val
	jsr CalculatePositionInCharset
	
	sta val
	; Assigning single variable : screenmemory
	ldy #39
	sta (screenmemory),y
	; Assigning single variable : screenmemory
	; 8 bit binop
	; Add/sub where right value is constant number
	lda val
	clc
	adc #40
	 ; end add / sub var with constant
	
	pha
	ldy #79 ; optimized, look out for bugs
	
	pla
	sta (screenmemory),y
	jsr UpdateTextScroll

elseblock9741
elsedoneblock27529

	rts
	
	
	; ***********  Defining procedure : MainRaster
	;    Procedure type : User-defined procedure
	
MainRaster

	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; Wait
	ldx #6
	dex
	bne *-1
	; Assigning memory location (poke replacement)
	; Assigning single variable : $d021
	lda #11
	sta $d021
	; Wait for no of raster lines
	lda #1
	clc 
	adc $d012
	cmp $d012
	bne *-3
	; Wait
	ldx #10
	dex
	bne *-1
	; Assigning memory location (poke replacement)
	; Assigning single variable : $d021
	lda #0
	sta $d021
	; Assigning single variable : ms_scroll
	; Optimizer: a = a +/- b
	lda ms_scroll
	sec
	sbc #2
	sta ms_scroll
	; 8 bit binop
	; Add/sub where right value is constant number
	and #7
	 ; end add / sub var with constant
	; ScrollX method
	sta $58
	lda $d016  
	and #$F8
	ora $58
	sta $d016
	jsr DrawText
	; RasterIRQ : Hook a procedure
	lda #110
	sta $d012
	lda #<MainScreenScrollerEnds
	sta $fffe
	lda #>MainScreenScrollerEnds
	sta $ffff
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla

	rti
	
	
	; ***********  Defining procedure : MainScreen
	;    Procedure type : User-defined procedure
	
MainScreen

	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	; RasterIRQ : Hook a procedure
	lda #86
	sta $d012
	lda #<MainRaster
	sta $fffe
	lda #>MainRaster
	sta $ffff
	jsr $1003
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla

	rti
block41

	; Assigning memory location (poke replacement)
	; Assigning single variable : $d021
	lda #0
	sta $d021
	; Assigning memory location (poke replacement)
	; Assigning single variable : $d020
	sta $d020
	; initsid
	tax
	tay
	jsr $1000
	lda $d018
	and #%11110001
	ora #8
	sta $d018
	; Clear screen with offset
	lda #160
	ldx #$fa
clearloop1842
	dex
	sta $0000+$400,x
	sta $00fa+$400,x
	sta $01f4+$400,x
	sta $02ee+$400,x
	bne clearloop1842
	; Clear screen with offset
	lda #5
	ldx #$fa
clearloop288
	dex
	sta $0000+$d800,x
	sta $00fa+$d800,x
	sta $01f4+$d800,x
	sta $02ee+$d800,x
	bne clearloop288
	; Assigning single variable : pText
	lda #<text
	ldx #>text
	sta pText
	stx pText+1
	lda $D016
	ora #%1000
	sta $D016
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	sei
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	lda $dc0d   ; cancel all CIA-IRQs in queue/unprocessed
	lda $dd0d   ; cancel all CIA-IRQs in queue/unprocessed
	
	; RasterIRQ : Hook a procedure
	lda #1
	sta $d012
	lda #<MainScreen
	sta $fffe
	lda #>MainScreen
	sta $ffff
	; Enable raster IRQ
	lda $d01a
	ora #$01
	sta $d01a
	lda #$1B
	sta $d011
	asl $d019
	cli
	jmp * ; loop like (�/%

EndSymbol
	org $1000
	incbin "tutorials/C64/Tutorials//resources/sid/_courier.dat"
	org $2000
charset
	incbin "tutorials/C64/Tutorials///resources/charsets/charset_16x16.bin"
