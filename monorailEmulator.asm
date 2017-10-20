;MonoRail Emulator

.include "m2560def.inc"

;CONSTANTS



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

    time12:     .byte 1
    time23:     .byte 1
    time34:     .byte 1
    time45:     .byte 1
    time56:     .byte 1
    time67:     .byte 1
    time78:     .byte 1
    time89:     .byte 1
    time910:    .byte 1
    time101:    .byte 1
	
.cseg	;;; Got this table from lecture slides

; Vector Table
.org 0x0000
	jmp RESET
	jmp EXT_INT0						; IRQ0 Handler
	jmp EXT_INT1						; IRQ1 Handler
	jmp INTERRUPT2 					; IRQ2 Handler
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
	jmp Timer0OVF 					; Timer/Counter0 Overflow
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


;Deals with initialising all the station names and station times
Initialisation:
    rcall printMaxStations


;Runs the monorail Loop
Monorail_Loop:
    

printMaxStations: