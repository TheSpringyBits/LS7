Z1 = $00				;ZERO 0
Z2 = $01				;ZERO 1

AIS = $0200					;A INTERUPPT STORE
XIS = $0201					;X INTERRUPT STORE
YIS = $0202					;Y INTERRUPT STORE
CP = $0203					;CURRENT KEYBOARD PRESS
MP = $0204					;MODIFIER KEYBOARD PRESS
PP = $0205					;PREVIOUS KEYBOARD PRESS
K1 = $0206					;KERNEL/SUBROUTINE REG1
K2 = $0207					;...
K3 = $0208		
K4 = $0209
K5 = $020A
K6 = $020B
K7 = $020C
K8 = $020D
K9 = $020E					
CX = $020F					;CURSOR X
CY = $0210					;CURSOR Y
PC = $0211 
CSP = $0212					;CURSOR STORE
CXP = $0213					;CURSOR X PREVIOUS
CYP = $0214					;CURSOR Y PREVIOUS
XP = $0215
CML = $0216					;COMMAND LENGTH
IRQVECTOR = $FE		
TYPE = $0400


SYS_SPEED = $BD00
SYS_LED = $BD40
SYS_ROW = $BD80

		;VIA REGS
PORTA = $BF01
PORTB = $BF00
DDRB = $BF02
DDRA = $BF03
T1CL = $BF04
T1CH = $BF05
ACR = $BF0B
IFR = $BF0D
IER = $BF0E

		;VIDEO REGS
VIDX = $BE00					;X REG
VIDY = $BE01					;Y REG
VIDM = $BE02					;MOG REG
VIDD = $BE03					;DATA REG

	.ORG $C000
		;POPULAR SUBROUTINES
INIT_TIMER:
	LDA #$40
	STA ACR
	LDA K1
	STA T1CL
	LDA K2
	STA T1CH
	LDA #$C0
	STA IER
	CLI
	RTS

WRITE:
	STX VIDX
	STY VIDY
	STA VIDD
	NOP
	STX VIDX
	STY VIDY
	STA VIDD
	NOP
	STX VIDX
	STY VIDY
	STA VIDD
	NOP
	STX VIDX
	RTS
READ:
	STX VIDX
	STY VIDY
	NOP
	NOP
	NOP	
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP	
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP
	NOP	
	NOP
	LDA VIDD
	RTS
DRAW_RECT:								;K1=X CORD END , K2=Y CORD END, X=X CORD BEGIN, Y=Y CORD BEGIN, A=FILL VALUE
	STX K3
	INC K1
	INC K2
DRAW_RECT_LOOP
	JSR WRITE
	INX
	CPX K1
	BNE DRAW_RECT_LOOP
	LDX K3
	INY
	CPY K2
	BNE DRAW_RECT_LOOP
	LDX K3
	RTS
PRINT_TEXT:								;Z1=LOW TEXT ADDRESS, Z2=HIGH TEXT ADRESS, X=X BEGIN, Y=Y BEGIN
	STY K1
	LDY #$00
	STY K2
PRINT_TEXT_LOOP:
	LDY K2
	LDA (Z1), Y
	CMP #$FF
	BEQ RETURN_1
	CPY #$FF
	BEQ RETURN_1
	INC K2
	LDY K1
	JSR WRITE
	INX
	JMP PRINT_TEXT_LOOP
RETURN_1:
	RTS
CURSOR:
	;STA K7
	;STX K8
	;STY K9

	
	
	;LDA K7
	;LDX K8
	;LDY K9
	;RTS

	STA K5
	STX K6
	STY K7
	LDX CXP
	LDY CYP
	LDA CSP
	JSR WRITE
	LDA CX
	CLC
	ADC #$40
	STA K1
	LDY CY
	STY K2
	LDX K1
	LDY K2
	JSR READ
	STX CXP
	STY CYP
	STA CSP
	JSR BYTE_FLIP
	JSR WRITE
	LDA K5
	LDX K6
	LDY K7
	RTS
HIDE_CURSOR:
	LDX CX
	LDY CY
	STX K8
	STY K9
	LDX #$FF
	LDY #$FF
	STX CX
	STY CY
	JSR CURSOR
	LDX K8
	LDY K9
	STX CX
	STY CY
	RTS
BYTE_FLIP:
	STA K1
	ASL
	ASL
	ASL
	ASL
	AND #$F0
	STA K2
	LDA K1
	LSR
	LSR
	LSR
	LSR
	AND #$0F
	ORA K2
	RTS
BYTE_SPLIT:
	STA K1
	AND #$0F
	TAX
	LDA K1
	ROR
	ROR
	ROR
	ROR
	AND #$0F
	TAY
	RTS
RESET:
	SEI
	CLD
	LDA #<IRQ
	STA IRQVECTOR
	LDA #>IRQ
	STA IRQVECTOR+1
	LDA #$FF
	STA DDRB
	STA CXP
	STA CYP
	LDA #$00
	STA DDRA
	STA CP
	STA PP
	STA Z2
	LDA #$0E
	STA VIDM

	LDA #$01
	STA SYS_SPEED

	LDA #$03
	STA SYS_LED
	
	LDA #$07
	STA SYS_ROW

	LDA #$2A		
	STA K1
	LDA #$FF
	STA K2
	LDA #$F0
	LDX #$00
	LDY #$00
	JSR DRAW_RECT						;CLEAR CHARACTERS

	LDA #$6A
	STA K1
	LDA #$FF
	STA K2
	LDA #$0A
	LDX #$40
	LDY #$00
	JSR DRAW_RECT						;CLEAR CHARACTERS

	LDX #<START_TEXT
	STX Z1
	LDX #>START_TEXT
	STX Z2
	LDX #$00
	LDY #$00
	JSR PRINT_TEXT	

	LDA #$00						;TIMER COUNTER 1 LOW
	STA K1
	LDA #$D0						;TIMER COUNTER 1 HIGH
	STA K2
	JSR INIT_TIMER						;INIT AND START VIA TIMER 1

PRE_LOOP:
	LDA CP
	CMP #$F0
	BNE PRE_LOOP
QUICK_START:
	INC Z2
	LDA #$6E
	STA VIDM

	LDA Z2
	STA Z1
	LDX #$00
	LDY #$00

FILL:
	LDA Z1
	JSR WRITE
	INX
	INC Z1
	CPX #$7F
	BNE FILL
	LDX #$00
	INY
	CPY #$FF
	BNE FILL

	LDA #$4E
	STA VIDM

	LDX #$00
	LDY #$00
	STX Z1

	LDA Z2
	STA Z1

CHAR_LOOP:
	JSR READ
	STA K6
	CMP Z1
	BNE ERROR
	INX
	INC Z1
	CPX #$7F
	BNE CHAR_LOOP
	LDX #$00
	INY
	CPY #$FF
	BNE CHAR_LOOP
	LDA #$2C
	LDA #$00
	LDA #$00
	JSR WRITE
	LDA #$2E
	STA VIDM
	JMP LOOP

ERROR:
	LDA #$2D
	JSR WRITE
	LDA Z1
	STA K7

	STX K8
	STY K9

	LDX #<ERROR_TEXT
	STX Z1
	LDX #>ERROR_TEXT
	STX Z2
	LDX #$00
	LDY #$00
	JSR PRINT_TEXT	

	LDX #<EXPECTED_TEXT
	STX Z1
	LDX #>EXPECTED_TEXT
	STX Z2
	LDX #$00
	LDY #$01
	JSR PRINT_TEXT	

	LDX #<GOT_TEXT
	STX Z1
	LDX #>GOT_TEXT
	STX Z2
	LDX #$00
	LDY #$02
	JSR PRINT_TEXT	

	LDA #$4D
	STA K1
	LDA #$02
	STA K2
	LDA #$0A
	LDX #$40
	LDY #$00
	JSR DRAW_RECT						;CLEAR CHARACTERS

	LDA K7
	JSR BYTE_SPLIT
	STY K4
	STX K5
	LDX #$0C
	LDY #$01
	LDA K4
	JSR WRITE
	INX
	LDA K5
	JSR WRITE

	LDA K6
	JSR BYTE_SPLIT
	STY K4
	STX K5
	LDX #$0C
	LDY #$02
	LDA K4
	JSR WRITE
	INX
	LDA K5
	JSR WRITE

	LDA K8
	JSR BYTE_SPLIT
	STY K4
	STX K5
	LDA K9
	JSR BYTE_SPLIT
	STY K6
	STX K7

	LDX #$08
	LDY #$00
	LDA K4
	JSR WRITE
	INX
	LDA K5
	JSR WRITE

	LDX #$0C
	LDA K6
	JSR WRITE
	INX
	LDA K7
	JSR WRITE
	
	LDA #$1E
	STA VIDM

LOOP:
	LDA CP
	CMP #$F0
	BNE LOOP
	JMP QUICK_START
	;JMP LOOP
	
IRQ:
	STA AIS
	STX XIS
	STY YIS
	LDA $40
	STA SYS_ROW
	INC $40
	JSR Keyboard_Read_Master
	STX CP
	STY MP
	JSR KEY_HANDLER
	STA XP
	BIT T1CL
	CLC
	LDA AIS
	LDX XIS
	LDY YIS
	RTI
KEY_HANDLER:
	LDA MP
	CMP #$F4
	BEQ KEY_SHIFT
	CMP #$F3
	BEQ KEY_CONTROL
	CMP #$F8
	BEQ KEY_SHIFT_CONTROL
	LDA CP
	RTS
KEY_SHIFT:
	LDA CP
	CLC
	CLD
	ADC #$30
	RTS
KEY_CONTROL:
	LDA CP
	CLC
	CLD
	ADC #$60
	RTS
KEY_SHIFT_CONTROL:
	LDA CP
	CLC
	CLD
	ADC #$90
	RTS
Keyboard_Read_Master:
	lda #$20
	sta PORTB
	lda PORTA
	ldy #$F0
	ldx #$F1
	cmp #$01
	beq Key_Return_1 					; Backspace
	cmp #$09
	beq Key_Return_1					;Backspace+Shift
	cmp #$05
	beq Key_Return_1					;Backspace+Control
	ldx #$F2
	cmp #$02
	beq Key_Return_1					; New Line
	ldy #$F3
	cmp #$04
	beq Keyboard_Read_1					; Control
	ldy #$F4
	cmp #$08
	beq Keyboard_Read_1					; Shift
	ldy #$F8
	cmp #$0C
	beq Keyboard_Read_1	 				; Control+Shift
	ldy #$F0
	ldx #$F0
	cmp #$10
	beq Key_Return_1 					; Space
	cmp #$18
	beq Key_Return_1					;Space+Shift
	cmp #$14
	beq Key_Return_1					;Space+Control
	ldy #$F0
	ldx #$F6
	cmp #$20
	beq Keyboard_Read_1					; Exit
	ldy #$F7
	cmp #$40
	beq Keyboard_Read_1					; Hyper
	ldy #$F9
	cmp #$44
	beq Keyboard_Read_1					; Hyper+Control
	ldy #$FA
	cmp #$48
	beq Keyboard_Read_1					; Hyper+Shift
	ldy #$FB
	cmp #$4C
	beq Keyboard_Read_1					; Hyper+Control+Shift
	ldy #$F0
	ldx #$FD
	cmp #$64
	beq Key_Reset 					;Hyper+Control+Exit
	ldx #$FC
	cmp #$60
	beq Key_Return_1					; Hyper+Exit
	ldy #$F0
	jmp Keyboard_Read_1
Key_Reset:
	jmp RESET
Key_Return_1:
	rts
Keyboard_Read_1:
	lda #$01
	sta PORTB
	lda PORTA
	ldx #$01				; 1
	cmp #$01
	beq Key_Return_1
	ldx #$02				; 2
	cmp #$02
	beq Key_Return_1
	ldx #$03				; 3
	cmp #$04
	beq Key_Return_1
	ldx #$04				; 4
	cmp #$08
	beq Key_Return_1
	ldx #$05				; 5
	cmp #$10
	beq Key_Return_1
	ldx #$06				; 6
	cmp #$20
	beq Key_Return_1
	ldx #$07				; 7
	cmp #$40
	beq Key_Return_1
	ldx #$08				; 8
	cmp #$80
	beq Key_Return_1
	lda #$02
	sta PORTB
	lda PORTA
	ldx #$1A				; Q
	cmp #$01
	beq Key_Return_1
	ldx #$20				; W
	cmp #$02
	beq Key_Return_1
	ldx #$0E				; E
	cmp #$04
	beq Key_Return_1
	ldx #$1B				; R
	cmp #$08
	beq Key_Return_1
	ldx #$1D				; T
	cmp #$10
	beq Key_Return_1
	ldx #$23				; Z
	cmp #$20
	beq Key_Return_1
	ldx #$1E				; U
	cmp #$40
	beq Key_Return_1
	ldx #$12				; I
	cmp #$80
	beq Key_Return_1
	lda #$04
	sta PORTB
	lda PORTA
	ldx #$0A				; A
	cmp #$01
	beq Key_Return_2
	ldx #$1C				; S
	cmp #$02
	beq Key_Return_2
	ldx #$0D 				; D
	cmp #$04
	beq Key_Return_2
	ldx #$0F				; F
	cmp #$08
	beq Key_Return_2
	ldx #$10				; G
	cmp #$10
	beq Key_Return_2
	ldx #$11				; H
	cmp #$20
	beq Key_Return_2
	ldx #$13				; J
	cmp #$40
	beq Key_Return_2
	ldx #$14				; K
	cmp #$80
	beq Key_Return_2
	lda #$08
	sta PORTB
	lda PORTA
	ldx #$22				; Y
	cmp #$01
	beq Key_Return_2
	ldx #$21				; X
	cmp #$02
	beq Key_Return_2
	ldx #$0C				; C
	cmp #$04
	beq Key_Return_2
	ldx #$1F				; V
	cmp #$08
	beq Key_Return_2
	ldx #$0B				; B
	cmp #$10
	beq Key_Return_2
	ldx #$17				; N
	cmp #$20
	beq Key_Return_2
	ldx #$16				; M
	cmp #$40
	beq Key_Return_2
	ldx #$25				; ,
	cmp #$80
	beq Key_Return_2
	jmp Keyboard_Read_2
Key_Return_2:
	rts
Keyboard_Read_2:
	lda #$10
	sta PORTB
	lda PORTA

	ldx #$09				; 9
	cmp #$01
	beq Key_Return_2
	ldx #$00				; 0
	cmp #$02
	beq Key_Return_2
	ldx #$26				; -
	cmp #$04
	beq Key_Return_2
	ldx #$27				; +
	cmp #$08
	beq Key_Return_2
	ldx #$18				; O
	cmp #$10
	beq Key_Return_2
	ldx #$19				; P
	cmp #$20
	beq Key_Return_2
	ldx #$15				; L
	cmp #$40
	beq Key_Return_2
	ldx #$24				; .
	cmp #$80
	beq Key_Return_2
	ldx #$FF
	rts
IRQ_JUMP:
	JMP (IRQVECTOR)

START_TEXT:
	.BYTE $49, $4B, $3E, $4C, $4C, $F0, $4C, $49, $3A, $3C, $3E, $F0, $4D, $48, $F0
	.BYTE $4C, $4D, $3A, $4B, $4D, $FF
ERROR_TEXT:
	.BYTE $3E, $4B, $4B, $48, $4B, $F0 ,$F0, $51, $F0, $F0,$F0, $52, $F0, $F0, $FF
EXPECTED_TEXT:
	.BYTE $3E, $51, $49, $3E, $3C, $4D, $3E, $3D, $F0, $F0, $F0, $F0, $F0, $F0, $FF
GOT_TEXT:
	.BYTE $40, $48, $4D, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0, $FF

	.ORG $FFFA
	.WORD RESET							;NMI VECTOR
	.WORD RESET							;RESET VECTOR
	.WORD IRQ_JUMP							;IRQ VECTOR









