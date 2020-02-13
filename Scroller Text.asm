 processor 6502

	ORG $801
	.byte    $0E, $08, $0A, $00, $9E, $20, $28
	.byte   $32,$30,$36,$34
	.byte    $29, $00, $00, $00

	ORG $810

SpriteScroller
	jsr initsine_calculate
	jmp block41
zp	= $02
tp	= $04
p1	= $08
ep	= $16
time	= $0B
i	dc.b	
j	dc.b	
k	dc.b	
col	dc.b	
temp2	dc.b	 
	org temp2+256
textSprite_x	dc.b	 
	org textSprite_x+8
textSprite_y	dc.b	 
	org textSprite_y+8
textSprite_col	dc.b	 
	org textSprite_col+8
textSprite_status	dc.b	 
	org textSprite_status+8
textSprite_t	dc.b	 
	org textSprite_t+8
textSprite_cnt	dc.b	$00
ms_x	dc.w	
scrollerText		dc.b	" SCROLLER TEXT BY LIOLABS  "
	dc.b	0
	dc.b	0
colorsGreen	dc.b $0b, $0b, $05, $0c, $0d, $0d, $0f, $07
	dc.b $07, $0f, $0d, $0d, $0c, $05, $0b, $0b
	dc.b 
	
	
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
initdiv16x8_divisor = $4C     ;$59 used for hi-byte
initdiv16x8_dividend = $4E	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4E ;save memory by reusing divident to store the result
divide16x8	lda #0	        ;preset remainder to 0
	sta initdiv16x8_remainder
	sta initdiv16x8_remainder+1
	ldx #16	        ;repeat for each bit: ...
divloop16	asl initdiv16x8_dividend	;dividend lb & hb*2, msb -> Carry
	rol initdiv16x8_dividend+1
	rol initdiv16x8_remainder	;remainder lb & hb * 2 + msb from carry
	rol initdiv16x8_remainder+1
	lda initdiv16x8_remainder
	sec
	sbc initdiv16x8_divisor	;substract divisor to see if it fits in
	tay	        ;lb result -> Y, for we may need it later
	lda initdiv16x8_remainder+1
	sbc initdiv16x8_divisor+1
	bcc skip16	;if carry=0 then divisor didn't fit in yet
	sta initdiv16x8_remainder+1	;else save substraction result as new remainder,
	sty initdiv16x8_remainder
	inc initdiv16x8_result	;and INCrement result cause divisor fit in 1 times
skip16	dex
	bne divloop16
	rts
	
	
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
	
	
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
div8x8_c = $4C
div8x8_d = $4E
div8x8_e = $50
	; Normal 8x8 bin div
div8x8_procedure
	lda #$00
	ldx #$07
	clc
div8x8_loop1 rol div8x8_d
	rol
	cmp div8x8_c
	bcc div8x8_loop2
	sbc div8x8_c
div8x8_loop2 dex
	bpl div8x8_loop1
	rol div8x8_d
	lda div8x8_d
div8x8_def_end
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
	
	
	; ***********  Defining procedure : initsinetable
	;    Procedure type : Built-in function
	;    Requires initialization : no
	
sine .byte 0 
	org sine +#255
value .word 0
delta .word 0
initsine_calculate
	ldy #$3f
	ldx #$00
initsin_a
	lda value
	clc
	adc delta
	sta value
	lda value+1
	adc delta+1
	sta value+1
	sta sine+$c0,x
	sta sine+$80,y
	eor #$ff
	sta sine+$40,x
	sta sine+$00,y
	lda delta
	adc #$10   ; this value adds up to the proper amplitude
	sta delta
	bcc initsin_b
	inc delta+1
initsin_b
	inx
	dey
	bpl initsin_a
	rts
	
	
	; ***********  Defining procedure : GenerateSprites
	;    Procedure type : User-defined procedure
	
 ; Temp vars section
 ; Temp vars section ends
GenerateSprites

	; Assigning single variable : tp
	lda zp
	ldx zp+1
	sta tp
	stx tp+1
	; Assigning single variable : tp
	
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit
	; Integer constant assigning
	ldy #$01
	lda #$40
	
rightvarInteger_var19169 = $54
	sta rightvarInteger_var19169
	sty rightvarInteger_var19169+1
	
	lda tp+1
	clc
	adc rightvarInteger_var19169+1
	tay
	lda tp
	clc
	adc rightvarInteger_var19169
	bcc wordAdd26500
	iny
wordAdd26500
	
	sta tp
	sty tp+1
	; Assigning single variable : k
	lda #0
	sta k
for15724
forLoopFix19895

	lda #0
	ldy #0
fill11538
	sta (p1),y
	iny
	cpy #64
	bne fill11538
	; Assigning single variable : j
	lda #0
	sta j
	; Assigning single variable : i
	sta i
for1869

	; Assigning single variable : p1
	; Load pointer array
	ldy i
	lda (zp),y
	
	ldy j
	sta (p1),y
	; Assigning single variable : p1
	; Load pointer array
	; LDA stuff
	; 8 bit binop
	; Add/sub where right value is constant number
	lda i
	clc
	adc #8
	 ; end add / sub var with constant
	
	tay
	lda (zp),y
	
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	lda j
	clc
	adc #1
	 ; end add / sub var with constant
	
	tay
	pla
	sta (p1),y
	; Assigning single variable : p1
	; Load pointer array
	ldy i
	lda (tp),y
	
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	lda j
	clc
	adc #24
	 ; end add / sub var with constant
	
	tay
	pla
	sta (p1),y
	; Assigning single variable : p1
	; Load pointer array
	; LDA stuff
	; 8 bit binop
	; Add/sub where right value is constant number
	lda i
	clc
	adc #8
	 ; end add / sub var with constant
	
	tay
	lda (tp),y
	
	pha
	; 8 bit binop
	; Add/sub where right value is constant number
	lda j
	clc
	adc #25
	 ; end add / sub var with constant
	
	tay
	pla
	sta (p1),y
	; Assigning single variable : j
	; Optimizer: a = a +/- b
	lda j
	clc
	adc #3
	sta j

	inc i
	lda #8
	cmp i ;keep
	bne for1869
forLoopDone25667
	; Assigning single variable : p1
	; WORD optimization: a=a+b
	lda p1+0
	
	clc
	adc #64
	bcc WordAdd17035
	inc p1+1
WordAdd17035
	sta p1+0
	; Assigning single variable : zp
	; WORD optimization: a=a+b
	lda zp+0
	
	clc
	adc #16
	bcc WordAdd9894
	inc zp+1
WordAdd9894
	sta zp+0
	; Assigning single variable : tp
	; WORD optimization: a=a+b
	lda tp+0
	
	clc
	adc #16
	bcc WordAdd28703
	inc tp+1
WordAdd28703
	sta tp+0
	; Binary clause Simplified: EQUALS
	lda k
	; Compare with pure num / var optimization
	cmp #$f;keep
	bne elsedoneblock17673
ConditionalTrueBlock31322

	; Assigning single variable : zp
	
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit
	; Integer constant assigning
	ldy #$01
	lda #$80
	
rightvarInteger_var32662 = $54
	sta rightvarInteger_var32662
	sty rightvarInteger_var32662+1
	
	lda zp+1
	clc
	adc rightvarInteger_var32662+1
	tay
	lda zp
	clc
	adc rightvarInteger_var32662
	bcc wordAdd27644
	iny
wordAdd27644
	
	sta zp
	sty zp+1
	; Assigning single variable : tp
	
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit
	; Integer constant assigning
	ldy #$01
	lda #$80
	
rightvarInteger_var20037 = $54
	sta rightvarInteger_var20037
	sty rightvarInteger_var20037+1
	
	lda tp+1
	clc
	adc rightvarInteger_var20037+1
	tay
	lda tp
	clc
	adc rightvarInteger_var20037
	bcc wordAdd32757
	iny
wordAdd32757
	
	sta tp
	sty tp+1

elseblock30333
elsedoneblock17673

	inc k
	lda #26
	cmp k ;keep
	beq forLoopDone5447
forLoopNotDone21726
	jmp for15724
forLoopDone5447

	rts
	
	
	; ***********  Defining procedure : Maintain16x16Sprites
	;    Procedure type : User-defined procedure
	
Maintain16x16Sprites

	; Assigning single variable : col
	lda #0
	sta col
	; Assigning single variable : i
	sta i
for8723
forLoopFix31115

	; Binary clause: EQUALS
	; Load Byte array
	ldx i
	lda textSprite_status,x
	
	; Compare with pure num / var optimization
	cmp #$1;keep
	; BC done
	bne binaryclausefailed15574
binaryclausesuccess12052
	lda #1; success
	jmp binaryclausefinished4031
binaryclausefailed15574
	lda #0 ; failed state
binaryclausefinished4031
	cmp #1
	beq ConditionalTrueBlock13977
	jmp elsedoneblock31673
ConditionalTrueBlock13977

	; Assigning single variable : textSprite_x
	ldx i
	; Optimize byte array dec 
	dec textSprite_x,x
	; Assigning single variable : k
	; Load Byte array
	lda textSprite_t,x
	
	sta k
	; Assigning single variable : textSprite_y
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Byte array
	ldx k
	lda temp2,x
	
	clc
	adc #170
	 ; end add / sub var with constant
	
	ldx i
	sta textSprite_y,x
	; Assigning single variable : ms_x
	ldy #0 ; ::AssignVariable ldy #0
	; Right is PURE NUMERIC
	; 16 bit mul or div
	; Mul 16x8 setup
	
	; Load Byte array
	lda textSprite_x,x
	
	sta mul16x8_num1
	lda #0
	sta mul16x8_num1Hi
	
	lda #2
	
	sta mul16x8_num2
	jsr mul16x8_procedure
	
	sta ms_x
	sty ms_x+1
	; Setting sprite position
	lda i
	pha
	tax
	lda #1
shiftbit21724
	cpx #0
	beq shiftbitdone13966
	asl
	dex
	jmp shiftbit21724
shiftbitdone13966
bitmask_var3430 = $54
	sta bitmask_var3430
	pla
	asl
	tax
	; integer assignment NodeVar
	ldy ms_x+1 ; Next one ; optimized, look out for bugs
	lda ms_x
	sta $D000,x
	; integer assignment NodeVar
	ldy ms_x+1 ; Next one ; optimized, look out for bugs
	lda ms_x+1
	beq spritepos1150
	lda $D010
	ora bitmask_var3430
	sta $D010
	jmp spriteposcontinue16941
spritepos1150
	lda #$FF
	eor bitmask_var3430
	sta bitmask_var3430
	lda $D010
	and bitmask_var3430
	sta $D010
spriteposcontinue16941
	inx
	txa
	tay
	; Load Byte array
	ldx i
	lda textSprite_y,x
	
	sta $D000,y
	; Assigning single variable : textSprite_t
	; Optimizer: a = a +/- b
	; Load Byte array
	lda textSprite_t,x
	clc
	adc #3
	sta textSprite_t,x
	ldx i ; optimized, look out for bugs
	inc textSprite_col,x
	; Assigning single variable : k
	; 8 bit binop
	; Add/sub where right value is constant number
	; Load Byte array
	ldx i
	lda textSprite_col,x
	
	and #15
	 ; end add / sub var with constant
	
	sta k
	; Assigning single variable : $d027
	; Load Byte array
	ldx k
	lda colorsGreen,x
	
	ldx i
	sta $d027,x
	; Binary clause Simplified: GREATER
	; Load Byte array
	lda textSprite_x,x
	
	; Compare with pure num / var optimization
	cmp #$fd;keep
	bcc elsedoneblock11337
	beq elsedoneblock11337
ConditionalTrueBlock30191

	ldx i ; optimized, look out for bugs
	lda #1
shiftbit8909
	cpx #0
	beq shiftbitdone32209
	asl
	dex
	jmp shiftbit8909
shiftbitdone32209
bitmask_var9758 = $54
	sta bitmask_var9758
	lda #$FF
	eor bitmask_var9758
	sta bitmask_var9758
	lda $d015
	and bitmask_var9758
	sta $d015
	; Assigning single variable : textSprite_status
	lda #0
	ldx i
	sta textSprite_status,x

elseblock18007
elsedoneblock11337

elseblock2306
elsedoneblock31673

	inc i
	lda #8
	cmp i ;keep
	beq forLoopDone4639
forLoopNotDone29658
	jmp for8723
forLoopDone4639

	rts
	
	
	; ***********  Defining procedure : Renew16x16Sprites
	;    Procedure type : User-defined procedure
	
Renew16x16Sprites

	inc textSprite_cnt
	; Binary clause Simplified: LESS
	lda textSprite_cnt
	; Compare with pure num / var optimization
	cmp #$18;keep
	bcs elsedoneblock27506
ConditionalTrueBlock6422
	rts
elseblock24946
elsedoneblock27506
	; Assigning single variable : textSprite_cnt
	lda #0
	sta textSprite_cnt
	; Assigning single variable : k
	; Load pointer array
	ldy #$0
	lda (ep),y
	
	sta k
	; Binary clause Simplified: EQUALS
	; Compare with pure num / var optimization
	cmp #$20;keep
	bne elsedoneblock18762
ConditionalTrueBlock900

	; Assigning single variable : ep
	; WORD optimization: a=a+b
	lda ep+0
	
	clc
	adc #1
	bcc WordAdd3602
	inc ep+1
WordAdd3602
	sta ep+0
	; Binary clause Simplified: EQUALS
	; Load pointer array
	ldy #$0
	lda (ep),y
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne elsedoneblock9374
ConditionalTrueBlock10291
	; Assigning single variable : ep
	lda #<scrollerText
	ldx #>scrollerText
	sta ep
	stx ep+1
elseblock30836
elsedoneblock9374
	rts

elseblock32591
elsedoneblock18762
	; Binary clause: GREATER
	lda k
	; Compare with pure num / var optimization
	cmp #$40;keep
	; BC done
	bcc binaryclausefailed3195
	beq binaryclausefailed3195
binaryclausesuccess3093
	lda #1; success
	jmp binaryclausefinished20485
binaryclausefailed3195
	lda #0 ; failed state
binaryclausefinished20485
	cmp #1
	beq ConditionalTrueBlock27348
	jmp elsedoneblock19668
ConditionalTrueBlock27348

	; Assigning single variable : k
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda k
	sec
	sbc #64
	 ; end add / sub var with constant
	
	sec
	sbc #1
	 ; end add / sub var with constant
	
	sta k
	; Assigning single variable : j
	lda #255
	sta j
	; Assigning single variable : i
	lda #0
	sta i
for30523
	; Binary clause Simplified: EQUALS
	; Load Byte array
	ldx i
	lda textSprite_status,x
	
	; Compare with pure num / var optimization
	cmp #$0;keep
	bne elsedoneblock20798
ConditionalTrueBlock15281
	; Assigning single variable : j
	lda i
	sta j
elseblock19589
elsedoneblock20798
	inc i
	lda #8
	cmp i ;keep
	bne for30523
forLoopDone20580
	; Binary clause Simplified: NOTEQUALS
	lda j
	; Compare with pure num / var optimization
	cmp #$ff;keep
	beq elsedoneblock12292
ConditionalTrueBlock23622

	; Assigning single variable : textSprite_x
	lda #176
	ldx j
	sta textSprite_x,x
	; Assigning single variable : textSprite_status
	lda #1
	sta textSprite_status,x
	ldx j ; optimized, look out for bugs
shiftbit11511
	cpx #0
	beq shiftbitdone16202
	asl
	dex
	jmp shiftbit11511
shiftbitdone16202
bitmask_var2634 = $54
	sta bitmask_var2634
	lda $d015
	ora bitmask_var2634
	sta $d015
	; Assigning single variable : textSprite_col
	lda #0
	ldx j
	sta textSprite_col,x
	; Set sprite location
	; Generic 16 bit op
	ldy #0 ; ::HandleGenericBinop16bit
	lda k
rightvarInteger_var20328 = $54
	sta rightvarInteger_var20328
	sty rightvarInteger_var20328+1
	
	lda #160
	ldy #0
	
	; Low bit binop:
	clc
	adc rightvarInteger_var20328
	bcc wordAdd24272
	inc rightvarInteger_var20328+1
wordAdd24272
	sta rightvarInteger_var20328
	; High-bit binop
	tya
	clc
	adc rightvarInteger_var20328+1
	tay
	lda rightvarInteger_var20328
	ldx j
	sta $07f8 + $0,x
	; Assigning single variable : textSprite_t
	; integer assignment NodeVar
	ldy time+1 ; Next one ; optimized, look out for bugs
	lda time
	sta textSprite_t,x
	; Assigning single variable : ep
	; WORD optimization: a=a+b
	lda ep+0
	
	clc
	adc #1
	bcc WordAdd22646
	inc ep+1
WordAdd22646
	sta ep+0

elseblock18538
elsedoneblock12292

elseblock23199
elsedoneblock19668

	rts
	
	
	; ***********  Defining procedure : Init16x16Sprites
	;    Procedure type : User-defined procedure
	
Init16x16Sprites

	; Assigning single variable : zp
	lda #0
	ldx #20
	sta zp
	stx zp+1
	; Assigning single variable : p1
	ldx #40
	sta p1
	stx p1+1
	jsr GenerateSprites
	; Assigning memory location (poke replacement)
	; Assigning single variable : $d015
	lda #0
	sta $d015
	; Assigning memory location (poke replacement)
	; Assigning single variable : $d01d
	lda #255
	sta $d01d
	; Assigning memory location (poke replacement)
	; Assigning single variable : $d017
	sta $d017
	lda #0
	ldx #0
fill4886
	sta textSprite_status,x
	inx
	cpx #8
	bne fill4886
	; Assigning single variable : i
	lda #0
	sta i
for18875
	; Assigning single variable : temp2
	; Right is PURE NUMERIC
	; 8 bit mul of power 2
	
	; Load Unknown type array
	ldx i
	lda sine,x
	
	lsr
	lsr
	lsr
	lsr
	
	sta temp2,x
	inc i
	lda #0
	cmp i ;keep
	bne for18875
forLoopDone29869

	rts
	
	
	; ***********  Defining procedure : RasterMain
	;    Procedure type : User-defined procedure
	
RasterMain

	; StartIRQ
	pha
	txa
	pha
	tya
	pha
	asl $d019
	inc time
	jsr Renew16x16Sprites
	jsr Maintain16x16Sprites
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla

	rti
block41

	sei
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	lda $dc0d   ; cancel all CIA-IRQs in queue/unprocessed
	lda $dd0d   ; cancel all CIA-IRQs in queue/unprocessed
	
	; Set Memory Config
	lda $01
	and #%11111000
	ora #%101
	sta $01
	jsr Init16x16Sprites
	; Assigning single variable : ep
	lda #<scrollerText
	ldx #>scrollerText
	sta ep
	stx ep+1
	sei
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	lda $dc0d   ; cancel all CIA-IRQs in queue/unprocessed
	lda $dd0d   ; cancel all CIA-IRQs in queue/unprocessed
	
	; RasterIRQ : Hook a procedure
	lda #0
	sta $d012
	lda #<RasterMain
	sta $fffe
	lda #>RasterMain
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
	org $1400
charset
	incbin "tutorials/C64/Tutorials///resources/charsets/charset_16x16.bin"
