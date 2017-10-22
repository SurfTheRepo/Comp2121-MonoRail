;MonoRail Emulator

.include "m2560def.inc"

;==========LCD Commands==========
.set LCD_DISP_ON = 0b00001110
.set LCD_DISP_OFF = 0b00001000
.set LCD_DISP_CLR = 0b00000001

.set LCD_FUNC_SET = 0b00111000 						; 2 lines, 5 by 7 characters
.set LCD_ENTR_SET = 0b00000110 						; increment, no display shift
.set LCD_HOME_LINE = 0b10000000 					; goes to 1st line (address 0)
.set LCD_SEC_LINE = 0b10101000 						; goes to 2nd line (address 40)
;=================================


;===============MACROS======================
.marco conflictPush
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


.marco conflictPop
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

;; LCD STUFF
.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4
.set LCD_HOME_LINE = 0b00000001

.macro do_lcd_command
	ldi r21, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro funky_do_lcd_command
	mov r21, @0
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

;==========================END MACROS=======================

;================defining registers=======================;
;But try to use just the register number for most of the time
.def temp1 = r16
.def temp2 = r17
.def mask = r18
.def col = r19
.def row = r20

.dseg

.org 0x100
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

	temporary_string .byte 10
	
.cseg	;;; Got this table from lecture slides

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
;================STRING CONSTANTS===================;
Print_Enter_Stations: .db "Enter the numberof stations: " ;16-12
Print_Not_Int: .db "Input 1-9"	;9
DEFAULT:
	reti							; used for interrupts that are not handled

RESET:

	;=======Set LCD stuff========;
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


;Deals with initialising all the station names and station times
Initialisation:
    rcall printMaxStations
	;r16 is going to hold the value of the max stations


;Runs the monorail Loop
MonorailLoop:
    

;===========Prints "Enter the maxumim number of stations"===========;
printMaxStations:
	;prologue
	push r16
	push r17
	push Zl
	push Zh
	ldi Zl,low(Print_Enter_Stations<<1)	;Load z-pointer and make it pointer to the first constant of "s"
	ldi Zh,high(Print_Enter_Stations<<1)

	;body ==== print stuff =====;
	clr r17

	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_HOME_LINE
	
	for_printMaxStation1:
		lpm r16, z+
		do_lcd_data r16
		inc r17
		cpi r17, 16
		brlo for_printMaxStation1
	
	do_lcd_command LCD_SEC_LINE
	for_PrintMaxStation2:
		lpm r16, z+
		do_lcd_data r16
		inc r17
		cpi r17, 13
		brlo for_printMaxStation2

	pop Zh
	pop Zl
	pop r17
	pop r16

	ret






;============Int KeyBoard=========;
INT_KEYPAD:
	push yl
	push yh
	Int_start:
	ldi mask, INITCOLMASK ; initial column mask
	clr col ; initial column

	ldi yl, low(temporary_string)
	ldi yh, high(temporary_string)

	colloop:
		STS PORTL, mask ; set column to mask value (sets column 0 off)
		ldi temp, 0xFF ; implement a delay so the hardware can stabilize

	delay:
		dec temp
		brne delay
		LDS temp, PINL ; read PORTL. Cannot use in 
		andi temp, ROWMASK ; read only the row bits
		cpi temp, 0xF ; check if any rows are grounded
		breq nextcol ; if not go to the next column
		ldi mask, INITROWMASK ; initialise row check
		clr row ; initial row

	rowloop:      
		mov temp2, temp
		and temp2, mask ; check masked bit
		brne skipconv ; if the result is non-zero, we need to look again
		rcall convert ; if bit is clear, convert the bitcode
		jmp main ; and start again

	skipconv:
		inc row ; else move to the next row
		lsl mask ; shift the mask to the next bit
		jmp rowloop    
		
	nextcol:     
		cpi col, 3 ; check if we are on the last column
		breq main ; if so, no buttons were pushed,
		; so start again.

		sec ; else shift the column mask:
		; We must set the carry bit
		rol mask ; and then rotate left by a bit, shifting the carry into bit zero. We need this to make sure all the rows have pull-up resistors
		inc col ; increment column value
		jmp colloop ; and check the next column convert function converts the row and column given to a binary number and also outputs the value to PORTC. 
		; Inputs come from registers row and col and output is in temp.

	convert:
		cpi col, 3 ; if column is 3 we have a letter
		breq IntStart

		cpi row, 3 ; if row is 3 we have a symbol or 0
		breq symbol

		cpi row, 0
		breq row1

		cpi row, 1
		breq row2

		cpi row, 2
		breq row3

	row1:
		ldi temp, '1'
		add temp, col ; add the column address
		jmp int_end

	row2:
		ldi temp, '4'
		add temp, col ; add the column address
		jmp int_end

	row3:
		ldi temp, '7'
		add temp, col ; add the column address
		jmp int_end


	int_end:
		do_lcd_data temp
		sts temp, y+
		ret

	not_int:
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_Not_Int<<1)	;Load z-pointer and make it pointer to the first constant of "s"
		ldi Zh,high(Print_Not_Int<<1)

;body ==== print stuff =====;
	clr r17

	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_HOME_LINE
		
	for_printMaxStation1:
		lpm r16, z+
		do_lcd_data r16
		inc r17
		cpi r17, 16
		brlo for_printMaxStation1
		
	do_lcd_command LCD_SEC_LINE
	for_PrintMaxStation2:
		lpm r16, z+
		do_lcd_data r16
		inc r17
		cpi r17, 13
		brlo for_printMaxStation2
	;pops for print
	pop Zh
	pop Zl
	pop r17
	pop r16

	;pops for function
	pop r20
	pop r19
	pop r18
	pop r17
	clr r16
	ret

;============LCD STUFF============;
//LCD COMMANDS
lcd_command:
	out PORTF, r21
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	ret

lcd_data:
	out PORTF, r22
	lcd_set LCD_RS
	nop
	nop
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r21
	clr r21
	out DDRF, r21
	out PORTF, r21
	lcd_set LCD_RW

	lcd_wait_loop:
		nop
		lcd_set LCD_E
		nop
		nop
		nop
		in r21, PINF
		lcd_clr LCD_E
		sbrc r21, 7
		rjmp lcd_wait_loop
	
	lcd_clr LCD_RW
	ser r21
	out DDRF, r21
	pop r21
	ret

//sleepstuff
	//Delay functions taken from previous lab
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
