;MonoRail Emulator

.include "m2560def.inc"

;================CONSTANTS====================


.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F

;================DEFINITIONS=================

.def temp = r16
.def InputCountFlag = r17
.def firstChar = r18
.def stringLength = r19
.def mask = r20
.def col = r21
.def row = r23
.def temp2 = r24
;===============MACROS======================
.macro conflictPush
	push r0
	push r1
	push r2
	push r3
	push r5
	push r6
	push r7
	push r8
	push r9
	push r10
	push r11
	push r12
	push r13
	push r14
	push r15
	push r16
	push r17
	push r18
	push r19
	push r20
	push r21
	push r22
	push r23
	push r24
	push r25
	push r26
	push r27
	push r28
	push r29
	push r30
	push r31
	in r16, SREG
	push r16
.endmacro


.macro conflictPop
	pop r16
	out SREG, r16
	pop r31
	pop r30
	pop r29
	pop r28
	pop r27
	pop r26
	pop r25
	pop r24
	pop r23
	pop r22
	pop r21
	pop r20
	pop r19
	pop r18
	pop r17
	pop r16
	pop r15
	pop r14
	pop r13
	pop r12
	pop r11
	pop r10
	pop r9
	pop r8
	pop r7
	pop r6
	pop r5
	pop r4
	pop r3
	pop r2
	pop r1
	pop r0
.endmacro


;============LCD Output Macros===========
.macro do_lcd_command
	ldi r22, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_data
	mov r22, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro do_lcd_char
	ldi r22, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro lcd_set
	sbi PORTA, @0
.endmacro

.macro lcd_clr
	cbi PORTA, @0
.endmacro
;=========================================


;==========LCD Commands==========
.set LCD_DISP_ON = 0b00001110
.set LCD_DISP_OFF = 0b00001000
.set LCD_DISP_CLR = 0b00000001

.set LCD_FUNC_SET = 0b00111000 						; 2 lines, 5 by 7 characters
.set LCD_ENTR_SET = 0b00000110 						; increment, no display shift
.set LCD_HOME_LINE = 0b10000000 					; goes to 1st line (address 0)
.set LCD_SEC_LINE = 0b10101000 						; goes to 2nd line (address 40)
;=================================




.dseg
	Max_Stations: .byte 1 ;maximum number of stations
    Max_Stoptime: .byte 1 ;maximum stoptime
    Station1:   .byte 11  ;the first byte of the station tells you how long the name is
    Station2:   .byte 11
    Station3:   .byte 11
    Station4:   .byte 11
    Station5:   .byte 11
    Station6:   .byte 11
    Station7:   .byte 11
    Station8:   .byte 11
    Station9:   .byte 11
    Station10:  .byte 11

;=========times for between 2 stations, max size of 10s =========

    time1_2:     .byte 1
    time2_3:     .byte 1
    time3_4:     .byte 1
    time4_5:     .byte 1
    time5_6:     .byte 1
    time6_7:     .byte 1
    time7_8:     .byte 1
    time8_9:     .byte 1
    time9_10:    .byte 1
    time10_1:    .byte 1
	
.cseg	
; Vector Table
.org 0x0000
	jmp RESET
	jmp DEFAULT						; IRQ0 Handler
	jmp DEFAULT						; IRQ1 Handler
	jmp DEFAULT 					; IRQ2 Handler
	jmp DEFAULT 					; IRQ3 Handler
	jmp DEFAULT 					; IRQ4 Handler
	jmp DEFAULT 					; IRQ5 Handler
	jmp DEFAULT 					; IRQ6 Handler
	jmp DEFAULT 					; IRQ7 Handler
	jmp DEFAULT 					; Pin Change Interrupt Request 0
	jmp DEFAULT 					; Pin Change Interrupt Request 1
	jmp DEFAULT 					; Pin Change Interrupt Request 2
	jmp DEFAULT 					; Watchdog Time-out Interrupt
	jmp DEFAULT 					; Timer/Counter2 Compare Match A
	jmp DEFAULT 					; Timer/Counter2 Compare Match B
	jmp DEFAULT 					; Timer/Counter2 Overflow
	jmp DEFAULT 					; Timer/Counter1 Capture Event
	jmp DEFAULT 					; Timer/Counter1 Compare Match A
	jmp DEFAULT 					; Timer/Counter1 Compare Match B
	jmp DEFAULT 					; Timer/Counter1 Compare Match C
	jmp DEFAULT 					; Timer/Counter1 Overflow
	jmp DEFAULT 					; Timer/Counter0 Compare Match A
	jmp DEFAULT 					; Timer/Counter0 Compare Match B
	jmp DEFAULT 					; Timer/Counter0 Overflow
	jmp DEFAULT 					; SPI Serial Transfer Complete
	jmp DEFAULT 					; USART0, Rx Complete
	jmp DEFAULT 					; USART0 Data register Empty
	jmp DEFAULT 					; USART0, Tx Complete
	jmp DEFAULT 					; Analog Comparator
	jmp DEFAULT 					; ADC Conversion Complete
	jmp DEFAULT 					; EEPROM Ready
	jmp DEFAULT 					; Timer/Counter3 Capture Event
	jmp DEFAULT 					; Timer/Counter3 Compare Match A
	jmp DEFAULT 					; Timer/Counter3 Compare Match B
	jmp DEFAULT 					; Timer/Counter3 Compare Match C
	jmp DEFAULT 					; Timer/Counter3 Overflow
.org 0x0072
DEFAULT:
	reti							; used for interrupts that are not handled


RESET:
	ldi r16, low(RAMEND)
	out SPL, r16
	ldi r16, high(RAMEND)
	out SPH, r16

	ser temp					;Set temp to all 1's
	out DDRF, temp				;Port F = output
	out DDRA, temp				;Port A = output
	clr temp
	out PORTF, temp
	out PORTA, temp
;=============LCD SetUP=====================
	do_lcd_command LCD_FUNC_SET		;2x5x7
	rcall sleep_5ms
	do_lcd_command LCD_FUNC_SET		;2x5x7
	rcall sleep_1ms
	do_lcd_command LCD_FUNC_SET
	do_lcd_command LCD_FUNC_SET
	do_lcd_command LCD_DISP_OFF
	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_ENTR_SET
	do_lcd_command LCD_DISP_ON

;=============Keypad Setup==================

	ldi temp, PORTLDIR ; columns are outputs, rows are inputs
	STS DDRL, temp     ; cannot use out


clr InputCountFlag
clr stringLength

STRING_KEYPAD:
	ldi mask, INITCOLMASK ; initial column mask
	clr col ; initial column

	colloop_string:
		STS PORTL, mask ; set column to mask value (sets column 0 off)
		ldi temp, 0xFF ; implement a delay so the hardware can stabilize

	delay_string:
		dec temp
		brne delay_string
		LDS temp, PINL ; read PORTL. Cannot use in 
		andi temp, ROWMASK ; read only the row bits
		cpi temp, 0xF ; check if any rows are grounded
		breq nextcol_string ; if not go to the next column
		ldi mask, INITROWMASK ; initialise row check
		clr row ; initial row

	rowloop_string:      
		mov temp2, temp
		and temp2, mask ; check masked bit
		brne skipconv_string ; if the result is non-zero, we need to look again
		rcall convert_string ; if bit is clear, convert the bitcode
		jmp STRING_KEYPAD ; and start again

	skipconv_string:
		inc row ; else move to the next row
		lsl mask ; shift the mask to the next bit
		jmp rowloop_string
		
	nextcol_string:     
		cpi col, 3 ; check if we are on the last column
		breq STRING_KEYPAD ; if so, no buttons were pushed,
		; so start again.

		sec ; else shift the column mask:
		; We must set the carry bit
		rol mask ; and then rotate left by a bit, shifting the carry into bit zero. We need this to make sure all the rows have pull-up resistors
		inc col ; increment column value
		jmp colloop_string ; and check the next column convert function converts the row and column given to a binary number and also outputs the value to PORTC. 
		; Inputs come from registers row and col and output is in temp.

	convert_string:
		cpi col, 3 ; if column is 3 we have a letter
		breq letters_string

		cpi row, 3 ; if row is 3 we have a symbol or 0
		breq symbols_string

		cpi row, 0
		breq row1_string

		cpi row, 1
		breq row2_string

		cpi row, 2
		breq row3_string

	row1_string:
		ldi temp, 1
		add temp, col ; add the column address
		jmp convert_end_string

	row2_string:
		ldi temp, 4
		add temp, col ; add the column address
		jmp convert_end_string

	row3_string:
		ldi temp, 7
		add temp, col ; add the column address
		jmp convert_end_string


	; to get the offset from 1
	; add 1. Value of switch is
	; row*3 + col + 1.
		jmp convert_end_string

	letters_string:
		ldi temp, 'A'
		add temp, row ; increment from 0xA by the row value
		cpi temp, 'A'
		breq printZ
		jmp endString
		printZ:
			ldi r22, 'Z'
			jmp printVal_string
	symbols_string:
		cpi col, 0 ; check if we have a star
		breq star

	star:	
		; SAVE THE STRING
		jmp endString

	zero:
		ldi temp, '0' ; set to zero

	convert_end_string:
		rcall sleep_100ms
		rcall sleep_100ms
		mov r22, temp
		cpi InputCountFlag, 0
		brne convertTwoOne_jmp
		
        ldi InputCountFlag, 1
		mov firstChar, r22
		jmp endConvert_string

		convertTwoOne_jmp:
			jmp convertTwoOne

		Ones:
			cpi firstChar, 1
			brne endConvert_string
			ldi r22, ' ' 
			jmp printVal_string
		Twos:
			cpi firstChar, 1
			breq printA
			cpi firstChar, 2
			breq printB
			cpi firstChar, 3
			breq printC
			jmp endConvert_string
			printA:
				ldi r22, 'A'
				jmp printVal_string
			printB:
				ldi r22, 'B'
				jmp printVal_string
			printC:
				ldi r22, 'C'
				jmp printVal_string

		Threes:
			cpi firstChar, 1
			breq printD
			cpi firstChar, 2
			breq printE
			cpi firstChar, 3
			breq printF
			jmp endConvert_string
			printD:
				ldi r22, 'D'
				jmp printVal_string
			printE:
				ldi r22, 'E'
				jmp printVal_string	
			printF:
				ldi r22, 'F'
				jmp printVal_string

		Fours:
			cpi firstChar, 1
			breq printG
			cpi firstChar, 2
			breq printH
			cpi firstChar, 3
			breq printI
			jmp endConvert_string
			printG:
				ldi r22, 'G'
				jmp printVal_string
			printH:
				ldi r22, 'H'
				jmp printVal_string
			printI:
				ldi r22, 'I'
				jmp printVal_string

	
			endConvert_string:
			rcall sleep_25ms
			rcall sleep_25ms
			rcall sleep_25ms
			rcall sleep_100ms
			ret ; return to caller

		convertTwoOne:
		
			ldi InputCountFlag, 0
			
			cpi r22, 1
			breq Ones

			cpi r22, 2
			breq Twos

			cpi r22, 3
			breq Threes

			cpi r22, 4
			breq Fours

			cpi r22, 5
			breq Fives
			jmp nextChunk

			Nines:
				cpi firstChar, 1
				breq printW
				cpi firstChar, 2
				breq printX
				cpi firstChar, 3
				breq printY
				jmp endConvert_string
				printW:
					ldi r22, 'W'
					jmp printVal_string
				printX:
					ldi r22, 'X'
					jmp printVal_string
				printY:
					ldi r22, 'Y'
					jmp printVal_string

			nextChunk:
			cpi r22, 6
			breq Sixes

			cpi r22, 7
			breq Sevens

			cpi r22, 8
			breq Eights
			
			cpi r22, 9
			breq Nines
			
		

			Fives:
				cpi firstChar, 1
				breq printJ
				cpi firstChar, 2
				breq printK
				cpi firstChar, 3
				breq printL
				jmp endConvert_string
				printJ:
					ldi r22, 'J'
					jmp printVal_string
				printK:
					ldi r22, 'K'
					jmp printVal_string
				printL:
					ldi r22, 'L'
					jmp printVal_string
					
			Sixes:
				cpi firstChar, 1
				breq printM
				cpi firstChar, 2
				breq printN
				cpi firstChar, 3
				breq printO
				jmp endConvert_string
				printM:
					ldi r22, 'M'
					jmp printVal_string
				printN:
					ldi r22, 'N'
					jmp printVal_string
				printO:
					ldi r22, 'O'
					jmp printVal_string
					
			Sevens:
				cpi firstChar, 1
				breq printP
				cpi firstChar, 2
				breq printR
				cpi firstChar, 3
				breq printS
				jmp endConvert_string
				printP:
					ldi r22, 'P'
					jmp printVal_string
				printR:
					ldi r22, 'R'
					jmp printVal_string
				printS:
					ldi r22, 'S'
					jmp printVal_string


			Eights:
				cpi firstChar, 1
				breq printT
				cpi firstChar, 2
				breq printU
				cpi firstChar, 3
				breq printV
				jmp endConvert_string
				printT:
					ldi r22, 'T'
					jmp printVal_string
				printU:
					ldi r22, 'U'
					jmp printVal_string
				printV:
					ldi r22, 'V'
					jmp printVal_string

			

		printVal_string:
			rcall lcd_data
			rcall lcd_wait
			rjmp endConvert_string
	
endString:
	ldi r22, '='
	rcall lcd_data
	rcall lcd_wait
	rjmp endConvert_string
	; called when '*' is pressed
	;trigger to set end of string, and ask for new input or run emulator


	;==========LCD Display Code Below (Note r16)==========
; Delay Constants
.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4 				; 4 cycles per iteration - setup/call-return overhead

.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

lcd_command:
	out PORTF, r22
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, r22
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r22
	clr r22
	out DDRF, r22
	out PORTF, r22
	lcd_set LCD_RW
lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in r22, PINF
	lcd_clr LCD_E
	sbrc r22, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r22
	out DDRF, r22
	pop r22
	ret

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret

sleep_25ms:
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	rcall sleep_5ms
	ret
sleep_100ms:
	rcall sleep_25ms
	rcall sleep_25ms
	rcall sleep_25ms
	rcall sleep_25ms
	ret