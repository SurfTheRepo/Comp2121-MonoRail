;MonoRail Emulator

.include "m2560def.inc"
;==============CONSTANTS================;

	.equ PORTLDIR = 0xF0
	.equ INITCOLMASK = 0xEF
	.equ INITROWMASK = 0x01
	.equ ROWMASK = 0x0F

	.equ F_CPU = 16000000
	.equ DELAY_1MS = F_CPU / 4 / 1000 - 4

	.equ LCD_RS = 7
	.equ LCD_E = 6
	.equ LCD_RW = 5
	.equ LCD_BE = 4
	.set LCD_HOME_LINE = 0b00000001

;==========LCD Commands==========
	.set LCD_DISP_ON = 0b00001110
	.set LCD_DISP_OFF = 0b00001000
	.set LCD_DISP_CLR = 0b00000001

	.set LCD_FUNC_SET = 0b00111000 						; 2 lines, 5 by 7 characters
	.set LCD_ENTR_SET = 0b00000110 						; increment, no display shift
	.set LCD_HOME_LINE = 0b10000000 					; goes to 1st line (address 0)
	.set LCD_SEC_LINE = 0b10101000 						; goes to 2nd line (address 40)
;=================================

;================DEFINITIONS=================

	.def temp = r16
	.def temp2 = r17
	.def mask = r18
	.def col = r19
	.def row = r20
	.def stopFlag = r22
	.def secondCount = r17
	.def stop_time = r18
	.def current_station = r24
	.def current_travel_time = r25

	.def InputCountFlag = r24
	.def firstChar = r21
	.def stringLength = r23

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


	

	.macro do_lcd_command
		push r21
		ldi r21, @0
		call lcd_command
		call lcd_wait
		pop r21
	.endmacro

	.macro funky_do_lcd_command
		push r21
		mov r21, @0
		call lcd_command
		call lcd_wait
		pop r21
	.endmacro

	.macro do_lcd_data
		push r22
		mov r22, @0
		call lcd_data
		call lcd_wait
		pop r22
	.endmacro

	.macro do_lcd_char
		push r22
		ldi r22, @0
		call lcd_data
		call lcd_wait
		pop r22
	.endmacro

	.macro lcd_set
		sbi PORTA, @0
	.endmacro

	.macro lcd_clr
		cbi PORTA, @0
	.endmacro

;==========================END MACROS=======================




.dseg

.org 0x200
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

    time1:     .byte 1
    time2:     .byte 1
    time3:     .byte 1
    time4:     .byte 1
    time5:     .byte 1
    time6:     .byte 1
    time7:     .byte 1
    time8:     .byte 1
    time9:     .byte 1
    time10:    .byte 1

	temporary_string: .byte 10
	
.cseg	;;; Got this table from lecture slides

; Vector Tablett
.org 0x0000
	jmp RESET
	jmp DEFAULT			 			; IRQ0 Handler
	jmp BUTTONINTERRUPT				; IRQ1 Handler
	jmp BUTTONINTERRUPT	 			; IRQ2 Handler
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
	Print_Give_Stn_Names: .db "Giv Stn Name: "
	Print_Not_Int: .db "Input 1-9"	;9
	Print_Success: .db "SUCCESS!"
	Print_Enter_Times: .db "Enter Time "
	Print_Enter_Name: .db "Give Stn Nm "
	Print_too_Large: .db "Too Large Redo"
	Print_too_Small: .db "Too Small Redo"
	Print_Stop_times: .db "Enter Stop Time:";16
	Print_No_String: .db "Plz Enter String" ;16
DEFAULT:
	reti							; used for interrupts that are not handled

RESET:
	;========Setting Up stack=============
		ldi r16, low(RAMEND)
		out SPL, r16
		ldi r16, high(RAMEND)
		out SPH, r16



	;========buttons for Interrupt 1&2============

		ldi temp, (1 << ISC21 | 1 << ISC11 | 1 << ISC01)      ; set INT2 as falling-edge 
    	sts EICRA, temp             ; edge triggered interrupt
		;=========Enable Interrupt=========
   		in temp, EIMSK              ; enable INT2
    	ori temp, (1<<INT2 | 1<<INT1 | 1<<INT0)
    	out EIMSK, temp





	;=======LED STUFF======;
		ser temp
		out DDRC, temp
		
		out PORTC, temp

	;==========Motor Setup===========;
		ldi temp, (1<<PE4)		;labeled PE2 actually PE4 
		out DDRE, temp   		;output

		ldi temp, (1<<WGM30)|(1<<COM3B1) ; set the Timer3 to Phase Correct PWM mode (8-bit) aka Mode1
		sts TCCR3A, temp
		ldi temp, (1<<CS31)
		sts TCCR3B, temp		; Prescaling value=8
		
	;==========keyPadInit======;
		ldi temp, PORTLDIR ; columns are outputs, rows are inputs
		STS DDRL, temp     ; cannot use out
		

		ser temp					;Set temp to all 1's
		out DDRF, temp				;Port F = output
		out DDRA, temp				;Port A = output
		clr temp
		out PORTF, temp
		out PORTA, temp

	;=======Set LCD stuff========;
		do_lcd_command LCD_FUNC_SET		;2x5x7
		call sleep_5ms
		do_lcd_command LCD_FUNC_SET		;2x5x7
		call sleep_1ms
		do_lcd_command LCD_FUNC_SET
		do_lcd_command LCD_FUNC_SET
		do_lcd_command LCD_DISP_OFF
		do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_ENTR_SET
		do_lcd_command LCD_DISP_ON
		

		do_lcd_command LCD_HOME_LINE
		do_lcd_char 'F'
		do_lcd_char 'U'
		do_lcd_char 'C'
		do_lcd_char 'K'
		do_lcd_char ' '
		do_lcd_char 'R'
		do_lcd_char 'E'
		do_lcd_char 'S'
		do_lcd_char 'E'
		do_lcd_char 'T'
		do_lcd_char '!'
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms


	jmp Initialisation

;======================Deals with initialising all the station names and station times==========================
Initialisation:
	cli

	in YL, SPL
	in YH, SPH
	sbiw Y, 4
	out SPL, YL
	out SPH, YH


	call number_stations
	
	call FindStnNames

	call sleep_1s

	call FindTimes

	call findStopTime

	call sleep_100ms

	
	jmp start_emulator
	

loopforever:
	jmp loopforever

;Runs the monorail Loop
start_emulator:
	do_lcd_command LCD_SEC_LINE
	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_HOME_LINE
	do_lcd_char 'P'
	do_lcd_char 'L'
	do_lcd_char 'E'
	do_lcd_char 'A'
	do_lcd_char 'S'
	do_lcd_char 'E'
	do_lcd_char ' '
	do_lcd_char 'W'
	do_lcd_char 'A'
	do_lcd_char 'I'
	do_lcd_char 'T'

	call sleep_1s
	; call sleep_1s
	; call sleep_1s
	; call sleep_1s
	; call sleep_1s
	do_lcd_command LCD_DISP_CLR
	do_lcd_command LCD_HOME_LINE
	do_lcd_char 'E'
	do_lcd_char 'M'
	do_lcd_char 'U'
	do_lcd_char 'L'
	do_lcd_char 'A'
	do_lcd_char 'T'
	do_lcd_char 'E'

	sei

	lds r15, Max_Stations
	clr temp
	clr secondCount
	inc secondCount
	ldi current_station, 1
	call MotorStart
	jmp emulator

;MonorailLoop:
emulator:
	cpi temp, 3 ;;;loops every 0.3seconds so finds if 1 second occured
	breq second_occurred_jmp
	jmp over_second_occured
	second_occurred_jmp:
		call second_occured

	over_second_occured:

	inc temp
	call printStnName
	
	; call sleep_100ms
	push r16
	ldi r16, 20
	out PORTC, r16
	call sleep_one_third
	ldi r16, 10
	out PORTC, r16
	call sleep_one_third
	pop r16


	jmp emulator

second_occured:
	inc secondCount
	mov temp, current_station

	call getting_time_between_station

	;;now compare current_travel by the elapsed seconds
	cp current_travel_time, secondCount
	breq at_new_station 
	jmp end_second_occured
	
	at_new_station:
		clr secondCount
		cpi stopFlag, 1
		breq stationStop_call
		jmp change_station

		stationStop_call:
			call stationStop
		;
		change_station:
			inc current_station
			lds r18, Max_Stations	
			cp r18, current_station
			breq station_loop_around
			jmp end_second_occured
			station_loop_around:
				ldi current_station, 1

	end_second_occured:
		clr temp
		ret


getting_time_between_station:

	cpi r16, 1
	breq get_time_1
	cpi r16, 2
	breq get_time_2
	cpi r16, 3
	breq get_time_4
	cpi r16, 5
	breq get_time_5
	jmp second_occured_bunch
	get_time_1:
		
		lds current_travel_time, time1
		jmp got_current_travel
	get_time_2:
		lds current_travel_time, time2
		jmp got_current_travel
	get_time_3:
		lds current_travel_time, time3		
		jmp got_current_travel
	get_time_4:
		lds current_travel_time, time5
		jmp got_current_travel
	get_time_5:
		lds current_travel_time, time5
		jmp got_current_travel
	;compare second against time1, time2, etc
	second_occured_bunch:
	cpi r16, 6
	breq get_time_6
	cpi r16, 7
	breq get_time_7
	cpi r16, 8
	breq get_time_8
	cpi r16, 9
	breq get_time_9
	cpi r16, 10
	breq get_time_10
	get_time_6:
		lds current_travel_time, time6
		jmp got_current_travel
	get_time_7:
		lds current_travel_time, time7
		jmp got_current_travel
	get_time_8:
		lds current_travel_time, time8	
		jmp got_current_travel
	get_time_9:
		lds current_travel_time, time9
		jmp got_current_travel
	get_time_10:
		lds current_travel_time, time10
		jmp got_current_travel

	got_current_travel:
		clr temp
		ret

;; Stops the train at station for max stop time
stationStop:	;;;
	call MotorStop
	push temp
	push r18
	push r16
	ldi r16, 3
	clr temp
	lds r18, Max_Stoptime
	stationStopLoop:
		cp r18, temp
		breq stop_time_done
		inc temp
		call sleep_1s
		jmp stationStopLoop	
	stop_time_done:
	pop r16
	pop r18
	pop temp
	call MotorStart
	ret		

MotorStart:
		push temp
		clr temp
		ldi temp,0x7E
		sts OCR3BL, temp		;Determine duty free
		clr temp
		sts OCR3BH, temp
		pop temp
		ret

MotorStop:
		push temp
		clr temp
		ldi temp,0x00
		sts OCR3BL, temp		;Determine duty free
		clr temp
		sts OCR3BH, temp
		
		pop temp
		ret


BUTTONINTERRUPT:
	cli 
	push r16
	ldi stopFlag, 1
	ldi r16, 244
	out PORTC, r16
	call sleep_1s
	clr r16
	out PORTC, r16
	pop r16
	reti






;===========Prints "Enter the maxumim number of stations"===========;
printStatements:
	printMaxStations:
		;prologue
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_Enter_Stations<<1)	
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
		
		clr r17
		do_lcd_command LCD_SEC_LINE
		for_PrintMaxStation2:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 12
			brlo for_printMaxStation2

		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	printEnterStation:
		;prologue
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_Enter_Name<<1)	
		ldi Zh,high(Print_Enter_Name<<1)

		;body ==== print stuff =====;
		clr r17

		do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_HOME_LINE
		
		for_Print_Enter_Station:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 12
			brlo for_Print_Enter_Station

		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	printEnterTimes:
		;prologue
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_Enter_Times<<1)	
		ldi Zh,high(Print_Enter_Times<<1)

		;body ==== print stuff =====;
		clr r17

		do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_HOME_LINE
		
		for_Print_Enter_Times:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 11
			brlo for_Print_Enter_Times

		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	printSuccess:
		;prologue
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_Success<<1)	
		ldi Zh,high(Print_Success<<1)

		;body ==== print stuff =====;
		clr r17

		do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_HOME_LINE
		
		for_Print_Success:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 8
			brlo for_Print_Success

		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	print_TooLARGE:
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_too_Large<<1)	
		ldi Zh,high(Print_Too_Large<<1)

		;body ==== print stuff =====;
		clr r17

		;do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_SEC_LINE
		
		for_Print_TooLarge:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 14
			brlo for_Print_TooLarge
		call sleep_1s

		call printEnterTimes
		do_lcd_command LCD_SEC_LINE
		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	print_TooLARGE_funky:
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_too_Large<<1)	
		ldi Zh,high(Print_Too_Large<<1)

		;body ==== print stuff =====;
		clr r17

		;do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_SEC_LINE
		
		for_Print_TooLarge_funky:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 14
			brlo for_Print_TooLarge_funky
		call sleep_1s

		;call printEnterTimes
		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	print_TooSMALL:
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_too_Small<<1)	
		ldi Zh,high(Print_Too_Small<<1)

		;body ==== print stuff =====;
		clr r17

		;do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_SEC_LINE
		
		for_Print_TooSmall:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 14
			brlo for_Print_TooSmall
		call sleep_1s

		;call printEnterTimes
		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	PrintNoString:
		;prologue
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_No_String<<1)	
		ldi Zh,high(Print_No_String<<1)

		;body ==== print stuff =====;
		clr r17

		;do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_SEC_LINE
		
		for_Print_No_String:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 16
			brlo for_Print_No_String


		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	printGivStnName:
		;prologue
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_Give_Stn_Names<<1)	
		ldi Zh,high(Print_Give_Stn_Names<<1)

		;body ==== print stuff =====;
		clr r17

		;do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_SEC_LINE
		
		for_Print_Give_Stn_Names:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 12
			brlo for_Print_Give_Stn_Names

		pop Zh
		pop Zl
		pop r17
		pop r16

		ret

	printEnterStopTime:
		;prologue
		push r16
		push r17
		push Zl
		push Zh
		ldi Zl,low(Print_Stop_times<<1)	
		ldi Zh,high(Print_Stop_times<<1)

		;body ==== print stuff =====;
		clr r17

		do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_HOME_LINE
		
		for_Print_Enter_Stop:
			lpm r16, z+
			do_lcd_data r16
			inc r17
			cpi r17, 16
			brlo for_Print_Enter_Stop

		pop Zh
		pop Zl
		pop r17
		pop r16

		ret


	printStnName:
		push yL
		push yH
		push r15
		push r16
		push current_station
		mov r16, current_station
		cpi r16, 1
		breq print_stn1_Name
		cpi r16, 2
		breq print_stn2_Name
		cpi r16, 3
		breq print_stn3_Name
		cpi r16, 4
		breq print_stn4_Name
		cpi r16, 5
		breq print_stn5_Name

		jmp print_next_stn_name_bunch
		print_stn1_Name:
			ldi yL, low(Station1)
			ldi yH, high(Station1)
			call printStation
			jmp print_names_full

		print_stn2_Name:
			ldi yL, low(Station2)
			ldi yH, high(Station2)
			call printStation
			jmp print_names_full

		print_stn3_Name:
			ldi yL, low(Station3)
			ldi yH, high(Station3)
			call printStation
			jmp print_names_full

		print_stn4_Name:
			ldi yL, low(Station4)
			ldi yH, high(Station4)
			call printStation
			jmp print_names_full

		print_stn5_Name:
			ldi yL, low(Station5)
			ldi yH, high(Station5)
			call printStation
			jmp print_names_full

		print_next_stn_name_bunch:
		cpi r16, 6
		breq print_stn6_Name
		cpi r16, 7
		breq print_stn7_Name
		cpi r16, 8
		breq print_stn8_Name
		cpi r16, 9
		breq print_stn9_Name
		cpi r16, 10
		breq print_stn10_Name
		;;
		print_stn6_Name:
			ldi yL, low(Station6)
			ldi yH, high(Station6)
			call printStation
			jmp print_names_full

		print_stn7_Name:
			ldi yL, low(Station7)
			ldi yH, high(Station7)
			call printStation
			jmp print_names_full

		print_stn8_Name:
			ldi yL, low(Station8)
			ldi yH, high(Station8)
			call printStation
			jmp print_names_full

		print_stn9_Name:
			ldi yL, low(Station9)
			ldi yH, high(Station9)
			call printStation
			jmp print_names_full

		print_stn10_Name:
			ldi yL, low(Station10)
			ldi yH, high(Station10)
			call printStation
			jmp print_names_full
		
		print_names_full:
		pop current_station
		pop r16
		pop r15
		pop yH
		pop yL

		ret

	printStation:
		ld stringLength, y+
		push r16
		push r17
		clr r17

		do_lcd_command LCD_DISP_CLR
		do_lcd_command LCD_HOME_LINE

		for_print_station_loop:
			ld r16, y+
			do_lcd_data r16
			inc r17
			cp r17, stringLength
			brne for_print_station_loop

		pop r17
		pop r16
		ret 
	

findStopTime:
	call printEnterStopTime
	call sleep_500ms
	do_lcd_command LCD_SEC_LINE
	call INT_KEYPAD

	lds r16, temporary_string
	;out PORTC, r16
	cpi r16, 6
	brsh findStopTime_Large
	cpi r16, 2
	brlo findStopTime_Small
	sts Max_Stoptime, r16
	ret

	findStopTime_Large:
	call Print_tooLARGE_funky
	jmp findStopTime

	findStopTime_Small:
	call Print_tooSMALL
	jmp findStopTime

number_stations:
	call printMaxStations	
	call sleep_500ms
	call INT_KEYPAD

	lds r16, temporary_string
	out PORTC, r16
	cpi r16, 11
	brsh number_stations
	
	sts Max_Stations, r16
	ret
;================FINDS TIMES BETWEEN STNs===========================;
FindTimes:
	lds r15, Max_Stations
	inc r15
	clr r16
	push r16
	stnTimeLoop:
		pop r16
		inc r16
		cp r16, r15
		brsh times_full_JMP
		push r16
		do_lcd_command LCD_DISP_CLR
		call printEnterTimes	
		jmp over_times_full
		times_full_JMP:
			jmp times_full
		over_times_full:
		out PORTC, r16
		call sleep_100ms

		cpi r16, 1
		breq stn1Time
		cpi r16, 2
		breq stn2Time
		cpi r16, 3
		breq stn3Time
		
		jmp next_time_bunch1
		stn1Time:
			do_lcd_command 0b10001011
			do_lcd_char '1'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time1)
			ldi yH, high(time1)
			call INT_KEYPAD
			lds r16, temporary_string
			cpi r16, 11
			brsh stn1Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn1Time_Big:
			call print_TooLARGE
			jmp stn1Time
		stn2Time:
			do_lcd_command 0b10001011
			do_lcd_char '2'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time2)
			ldi yH, high(time2)
			call INT_KEYPAD
			lds r16, temporary_string
			cpi r16, 11
			brsh stn2Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn2Time_Big:
			call print_TooLARGE
			jmp stn2Time
		stn3Time:
			do_lcd_command 0b10001011
			do_lcd_char '3'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time3)
			ldi yH, high(time3)
			call INT_KEYPAD
			lds r16, temporary_string
			cpi r16, 11
			brsh stn3Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn3Time_Big:
			call print_TooLARGE
			jmp stn3Time

		next_time_bunch1:
		cpi r16, 4
		breq stn4Time
		cpi r16, 5
		breq stn5Time
		jmp next_time_bunch2
		stn4Time:
			do_lcd_command 0b10001011
			do_lcd_char '4'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time4)
			ldi yH, high(time4)
			call INT_KEYPAD
			lds r16, temporary_string
			; out PORTC, r16
			cpi r16, 11
			brsh stn4Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn4Time_Big:
			call print_TooLARGE
			jmp stn4Time
		stn5Time:
			do_lcd_command 0b10001011
			do_lcd_char '5'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time5)
			ldi yH, high(time5)
			call INT_KEYPAD
			lds r16, temporary_string
			; out PORTC, r16
			cpi r16, 11
			brsh stn5Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn5Time_Big:
			call print_TooLARGE
			jmp stn5Time
		next_time_bunch2:

		cpi r16, 6
		breq stn6Time
		cpi r16, 7
		breq stn7Time
		cpi r16, 8
		breq stn8Time
		jmp next_stn_name_bunch3
		stn6Time:
			do_lcd_command 0b10001011
			do_lcd_char '6'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time6)
			ldi yH, high(time6)
			call INT_KEYPAD
			lds r16, temporary_string
			cpi r16, 11
			brsh stn6Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn6Time_Big:
			call print_TooLARGE
			jmp stn6Time
		stn7Time:
			do_lcd_command 0b10001011
			do_lcd_char '7'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time7)
			ldi yH, high(time7)
			call INT_KEYPAD
			lds r16, temporary_string
			; out PORTC, r16
			cpi r16, 11
			brsh stn7Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn7Time_Big:
			call print_TooLARGE
			jmp stn7Time
		stn8Time:
			do_lcd_command 0b10001011
			do_lcd_char '8'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time8)
			ldi yH, high(time8)
			call INT_KEYPAD
			lds r16, temporary_string
			; out PORTC, r16
			cpi r16, 11
			brsh stn8Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn8Time_Big:
			call print_TooLARGE
			jmp stn8Time

		next_stn_name_bunch3:
		
		cpi r16, 9
		breq stn9Time
		cpi r16, 10
		breq stn10Time

		
		stn9Time:
			do_lcd_command 0b10001011
			do_lcd_char '9'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time9)
			ldi yH, high(time9)
			call INT_KEYPAD
			lds r16, temporary_string
			; out PORTC, r16
			cpi r16, 11
			brsh stn9Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn9Time_Big:
			call print_TooLARGE
			call sleep_1ms
			jmp stn9Time
		stn10Time:
			do_lcd_command 0b10001011
			do_lcd_char '1'
			do_lcd_char '0'
			do_lcd_command LCD_SEC_LINE
			ldi yL, low(time10)
			ldi yH, high(time10)
			call INT_KEYPAD
			lds r16, temporary_string
			; out PORTC, r16
			cpi r16, 11
			brsh stn10Time_Big
			st Y, r16
			jmp stnTimeLoop
			stn10Time_Big:
			call print_TooLARGE
			call sleep_1ms
			jmp stn10Time

	times_full:
	ret



FindStnNames:
	clr r16
	lds r15, Max_Stations 
	inc r15
	;;get back Max_Stations
	stnNameLoop:
		inc r16 
		cp r16, r15
		brsh names_full_JMP
		
		do_lcd_command LCD_DISP_CLR
		call printEnterStation		
		subi r16, -'0'
		do_lcd_data r16
		subi r16, '0'
		do_lcd_command LCD_SEC_LINE
		
		jmp over_names_full
		names_full_JMP:
			jmp names_full
		over_names_full:
		out PORTC, r16
		call sleep_100ms
		clr stringLength

		cpi r16, 1
		breq stn1_Name
		cpi r16, 2
		breq stn2_Name
		cpi r16, 3
		breq stn3_Name
		cpi r16, 4
		breq stn4_Name
		cpi r16, 5
		breq stn5_Name

		jmp next_stn_name_bunch
		stn1_Name:
			ldi yL, low(Station1)
			ldi yH, high(Station1)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station1)
			ldi yH, high(Station1)
			st y, stringLength
			jmp stnNameLoop

		stn2_Name:
			ldi yL, low(Station2)
			ldi yH, high(Station2)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station2)
			ldi yH, high(Station2)
			st y, stringLength
			jmp stnNameLoop

		stn3_Name:
			ldi yL, low(Station3)
			ldi yH, high(Station3)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station3)
			ldi yH, high(Station3)
			st y, stringLength
			jmp stnNameLoop

		stn4_Name:
			ldi yL, low(Station4)
			ldi yH, high(Station4)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station4)
			ldi yH, high(Station4)
			st y, stringLength
			jmp stnNameLoop

		stn5_Name:
			ldi yL, low(Station5)
			ldi yH, high(Station5)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station5)
			ldi yH, high(Station5)
			st y, stringLength
			jmp stnNameLoop

		next_stn_name_bunch:
		cpi r16, 6
		breq stn6_Name
		cpi r16, 7
		breq stn7_Name
		cpi r16, 8
		breq stn8_Name
		cpi r16, 9
		breq stn9_Name
		cpi r16, 10
		breq stn10_Name
		;;
		stn6_Name:
			ldi yL, low(Station6)
			ldi yH, high(Station6)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station6)
			ldi yH, high(Station6)
			st y, stringLength
			jmp stnNameLoop

		stn7_Name:
			ldi yL, low(Station7)
			ldi yH, high(Station7)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station7)
			ldi yH, high(Station7)
			st y, stringLength
			jmp stnNameLoop

		stn8_Name:
			ldi yL, low(Station8)
			ldi yH, high(Station8)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station8)
			ldi yH, high(Station8)
			st y, stringLength
			jmp stnNameLoop

		stn9_Name:
			ldi yL, low(Station9)
			ldi yH, high(Station9)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station9)
			ldi yH, high(Station9)
			st y, stringLength
			jmp stnNameLoop

		stn10_Name:
			ldi yL, low(Station10)
			ldi yH, high(Station10)
			call STRING_KEYPAD_CALL
			ldi yL, low(Station10)
			ldi yH, high(Station10)
			st y, stringLength
			jmp stnNameLoop
		
		names_full:
		
	ret

;=================================KEYPAD FOR STN NAMES====================
STRING_KEYPAD_CALL:
	adiw yH:yL,1 ;; so that first position is not used, as at end we change first position to be length of string(really just used as incremet)
	push r16
	clr stringLength
	ldi InputCountFlag, 0
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
		jmp convert_string ; if bit is clear, convert the bitcode
		;jmp STRING_KEYPAD ; and start again

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

	letters_string:
		ldi temp, 'A'
		add temp, row ; increment from 0xA by the row value
		cpi temp, 'A'
		breq printZ
		jmp STRING_KEYPAD
		printZ:
			ldi r22, 'Z'
			jmp printVal_string
	symbols_string:
		cpi col, 0 ; check if we have a star
		breq star_string
		jmp STRING_KEYPAD

	star_string:	
		
		cpi stringLength, 0
		breq no_string
		; SAVE THE STRING
		call sleep_100ms
		call sleep_100ms
		jmp endString

	no_string:
		call PrintNoString
		call sleep_1s
		call clear_sec_line
		jmp STRING_KEYPAD

	zero_string:
		ldi temp, '0' ; set to zero

	convert_end_string:
		cpi stringLength, 10
		brsh endString_JMP
		jmp overEndstring_jmp
		endString_JMP:
			jmp endString
		overEndstring_jmp:
		call sleep_100ms
		call sleep_100ms
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
			call sleep_25ms
			call sleep_25ms
			call sleep_25ms
			call sleep_100ms
			jmp STRING_KEYPAD
		convertTwoOne:
		
			ldi InputCountFlag, 0
			
			cpi r22, 1
			breq OnesJMP
		
			cpi r22, 2
			breq TwosJMP

			cpi r22, 3
			breq ThreesJMP

			cpi r22, 4
			breq FoursJMP

			cpi r22, 5
			breq FivesJMP
			jmp nextChunk

			OnesJMP:
				jmp Ones
			TwosJMP:
				jmp Twos
			ThreesJMP:
				jmp Threes
			FoursJMP:
				jmp Fours
			FivesJMP:
				jmp Fives

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
			inc stringLength
			call lcd_data
			call lcd_wait
			st Y+, r22
			rjmp endConvert_string
	
	
	endString:

		
		;ldi r22, '='
		;call lcd_data
		;call lcd_wait
		pop r16
		ret
	; called when '*' is pressed
	;trigger to set end of string, and ask for new input or run emulator



;=====================================Int KeyBoard===============================;
INT_KEYPAD:
	;conflictPush
	push yl
	push yh

	clr temp
	sts temporary_string, temp
	sts temporary_string + 1, temp
	

	ldi yl, low(temporary_string)
	ldi yh, high(temporary_string)
	
	clr r21
	Int_start:

	ldi mask, INITCOLMASK ; initial column mask
	clr col ; initial column
	clr row
	clr temp
	clr temp2
	
	

	int_keypad_colloop:
		STS PORTL, mask ; set column to mask value (sets column 0 off)
		ldi temp, 0xFF ; implement a delay so the hardware can stabilize

	int_keypad_delay:
		dec temp
		brne int_keypad_delay
		LDS temp, PINL ; read PORTL. Cannot use in 
		andi temp, ROWMASK ; read only the row bits
		cpi temp, 0xF ; check if any rows are grounded
		breq int_keypad_nextcol ; if not go to the next column
		ldi mask, INITROWMASK ; initialise row check
		clr row ; initial row

	int_keypad_rowloop:      
		mov temp2, temp
		and temp2, mask ; check masked bit
		brne int_keypad_skipconv ; if the result is non-zero, we need to look again
		jmp int_keypad_convert ; if bit is clear, convert the bitcode
		;jmp Int_start ; and start again

	int_keypad_skipconv:
		inc row ; else move to the next row
		lsl mask ; shift the mask to the next bit
		jmp int_keypad_rowloop    
		
	int_keypad_nextcol:     
		cpi col, 3 ; check if we are on the last column
		breq Int_start ; if so, no buttons were pushed,
		; so start again.

		sec ; else shift the column mask:
		; We must set the carry bit
		rol mask ;
		inc col ; increment column value
		jmp int_keypad_colloop  
		; Inputs come from registers row and col and output is in temp.

	int_keypad_convert:
		cpi col, 3 ; if column is 3 we have a letter
		breq Int_Start

		cpi row, 3 ; if row is 3 we have a symbol or 0
		breq int_keypad_symbol

		cpi row, 0
		breq int_keypad_row1

		cpi row, 1
		breq int_keypad_row2

		cpi row, 2
		breq int_keypad_row3

	int_keypad_row1:
		ldi temp, '1'
		add temp, col ; add the column address
		jmp int_end

	int_keypad_row2:
		ldi temp, '4'
		add temp, col ; add the column address
		jmp int_end

	int_keypad_row3:
		ldi temp, '7'
		add temp, col ; add the column address
		jmp int_end

	int_keypad_symbol:
		
		cpi col, 0 ;========END OF LINE========;  *
		breq int_parse ;JUMP TO PARSING THE NUMBER	

		cpi col, 2 ;#
		breq Int_start_jmp

		ldi temp, '0'
		jmp int_end

		Int_start_jmp:
		jmp Int_start

	int_end:
		do_lcd_data temp
		call sleep_100ms
		call sleep_100ms
		st y+, temp
		inc r21 ;number is only ever 2 digits long
		cpi r21, 2
		brlo Int_start_jmp
		
	int_parse:
		call sleep_100ms
		call sleep_100ms
		;do_lcd_char '*'
		push temp
		push r17
		push r18
		ldi r18, 10
		ldi yl, low(temporary_string)
		ldi yh, high(temporary_string)

		Int_parse_loop:
			ld r17, y+	
			subi r17, '0'
			
			cpi r21, 2
			brne int_parse_end

			mul r17, r18
			ld r17, y+
			subi r17, '0'
			add r17, r0

		int_parse_end:
			sts temporary_string, r17

			pop r18
			pop r17
			pop temp
			;do_lcd_char 'U'
	
	pop yh
	pop yl

	ret 

	;conflictPop

	
	;ret 
	;jmp finished_int_keypad

	

;=================================LCD STUFF===========================;

LCD:
	clear_sec_line:
		push r16
		push r17
		ldi r17, ' '
		clr r16
		do_lcd_command LCD_SEC_LINE
		for_clr_sec_line:
			inc r16
			do_lcd_data r17
			cpi r16, 17
			brlo for_clr_sec_line
		do_lcd_command LCD_SEC_LINE
		pop r17
		pop r16
	ret

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

sleepstuff:	
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
		call sleep_1ms
		call sleep_1ms
		call sleep_1ms
		call sleep_1ms
		call sleep_1ms
		ret

	sleep_25ms:
		call sleep_5ms
		call sleep_5ms
		call sleep_5ms
		call sleep_5ms
		call sleep_5ms
		ret

	sleep_100ms:
		call sleep_25ms
		call sleep_25ms
		call sleep_25ms
		call sleep_25ms
		ret

	sleep_one_third:
		call sleep_100ms
		call sleep_25ms
		call sleep_25ms
		call sleep_5ms
		call sleep_5ms
		call sleep_5ms
		call sleep_1ms
		ret
	
	sleep_500ms:
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		ret

	sleep_1s:
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		call sleep_100ms
		ret