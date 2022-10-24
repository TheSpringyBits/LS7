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
	LDA #$20
	STA K2
	LDA #$F0
	LDX #$00
	LDY #$00
	JSR DRAW_RECT						;CLEAR CHARACTERS

	LDA #$6A
		
	STA K1
	LDA #$20
	STA K2
	LDA #$0A
	LDX #$40
	LDY #$00
	JSR DRAW_RECT						;CLEAR CHARACTERS

	LDA #$2F
	LDX #$00
	LDY #$00
	JSR WRITE						;DRAW ">"
	INX	
	STX CX
	STY CY
	JSR CURSOR						;SET CURSOR

	LDA #$00						;TIMER COUNTER 1 LOW
	STA K1
	LDA #$D0						;TIMER COUNTER 1 HIGH
	STA K2
	JSR INIT_TIMER						;INIT AND START VIA TIMER 1
	JMP LOOP
LOOP:
	LDX CP						;LOAD CURRENT KEYBOARD PRESS
	CLC
	CPX PP						;COMPARE TO PREVIOUS PRESS
	BEQ LOOP
	STX PP						;IF TEST NOT EQUAL, STORE CP TO PP AND CONTINUE

	LDA CP
	CMP #$FF						;TEST FOR EMPTY KEYBOARD RETURN
	BEQ LOOP	
	CMP #$F2						;TEST FOR RETURN PRESS
	BEQ NEW_LINE		
	CMP #$F1
	BEQ BACKSPACE
	
	JSR WRITE_CHAR
	JSR CHECK_OVERFLOW
	JSR CURSOR
	JMP LOOP

NEW_LINE:
	LDX CML
	LDA #$FF
	STA TYPE, X
	LDA #$00
	STA CML
	CLC
	LDA CY
	CMP #$1F
	BCC RETURN_CONTINUE
	JSR HIDE_CURSOR
	JSR SCROLL
	LDY #$1E
	STY CY
RETURN_CONTINUE:
	LDA #$00
	STA CX
	LDX CX
	INC CY
	LDY CY 
	LDA #$2F
	JSR WRITE
	INC CX
	JSR CURSOR
	JMP CHECK_COMMANDS
BACKSPACE:
	DEC CML
	LDX CML
	LDA #$FF
	STA TYPE, X
	LDA CX
	CMP #$01
	BEQ BACKSPACE_TEST
	CMP #$00
	BEQ BACKSPACE_TEST_2
	DEC CX
	LDY CY
	LDX CX
	LDA #$F0
	JSR WRITE
	JSR CURSOR
	JMP LOOP
BACKSPACE_TEST:
	LDX #$00
	LDY CY
	INC CML
	JSR READ
	CMP #$2F
	BEQ LOOP_JUMP
	DEC CML
	DEC CX
	LDX CX
	LDA #$F0
	JSR WRITE
	JSR CURSOR
	JMP LOOP
BACKSPACE_TEST_2:
	LDX #$27
	STX CX
	DEC CY
	LDY CY
	LDA #$F0
	JSR WRITE
	JSR CURSOR
	JMP LOOP
LOOP_JUMP:
	JMP LOOP
SCROLL:
	LDX #$00
	LDY #$01
	CLC
SCROLL_LOOP:
	JSR READ
	DEY
	JSR WRITE
	INX
	INY
	CPX #$4F
	BNE SCROLL_LOOP
	LDX #$00
	INY
	CPY #$21
	BNE SCROLL_LOOP
	RTS
CHECK_OVERFLOW:
	LDA CX
	CMP #$28
	BCS CHECK_CONTINUE1
	RTS
CHECK_CONTINUE1:
	CLC
	LDA CY
	CMP #$1F
	BCC CHECK_CONTINUE2
	JSR HIDE_CURSOR
	JSR SCROLL
	LDY #$1E
	STY CY
CHECK_CONTINUE2:
	LDA #$00
	STA CX
	INC CY
	RTS
WRITE_CHAR:
	LDX CX
	LDY CY
	LDA XP  
	JSR WRITE
	LDX CML
	INC CML
	STA TYPE, X
	INX
	LDA #$FF
	STA TYPE, X
	INC CX
	RTS
CHECK_COMMANDS:
	LDA TYPE
	CMP #$00
	BNE CHECK_ERROR
	LDX #$00
	LDY #$00
	JSR WRITE
CHECK_ERROR:
	JMP LOOP
	
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

	.ORG $FFFA
	.WORD RESET							;NMI VECTOR
	.WORD RESET							;RESET VECTOR
	.WORD IRQ_JUMP							;IRQ VECTOR









