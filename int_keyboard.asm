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