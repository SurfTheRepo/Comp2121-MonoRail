;MonoRail Emulator

.include "m2560def.inc"

;================CONSTANTS====================


.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F

;================DEFINITIONS=================

.def temp = r16

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

;==========================END MACROS=======================






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

;=============Keypad Setup==================

	ldi temp, PORTLDIR ; columns are outputs, rows are inputs
	STS DDRL, temp     ; cannot use out



MAIN_KEYPAD:
ldi mask, INITCOLMASK ; initial column mask
clr col ; initial column

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
breq letters

cpi row, 3 ; if row is 3 we have a symbol or 0
breq symbols

cpi row, 0
breq row1

cpi row, 1
breq row2

cpi row, 2
breq row3

row1:
ldi temp, '1'
add temp, col ; add the column address
jmp convert_end

row2:
ldi temp, '4'
add temp, col ; add the column address
jmp convert_end

row3:
ldi temp, '7'
add temp, col ; add the column address
jmp convert_end


; to get the offset from 1
 ; add 1. Value of switch is
; row*3 + col + 1.
jmp convert_end

letters:
ldi temp, 'A'
add temp, row ; increment from 0xA by the row value
jmp convert_end

symbols:
cpi col, 0 ; check if we have a star
breq star
cpi col, 1 ; or if we have zero
breq zero

ldi temp, '#' ; we'll output 0xF for hash
jmp convert_end

star:
ldi temp, '*' ; we'll output 0xE for star
jmp convert_end

zero:
ldi temp, '0' ; set to zero

convert_end:
mov r22, temp
;; SO instead of pushin it to lcd we need to 

rcall lcd_data
rcall lcd_wait
rcall sleep_25ms
rcall sleep_25ms
rcall sleep_25ms
rcall sleep_100ms
ret ; return to caller




;
;
; count if this is the second or first input, if first save value and find second value
; if second, convert the two values into a single value and output to lcd
;
;;
;
;
;
;
;
;
