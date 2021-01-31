	JP 0100h  ; jump to main routine

.ORG 0038h  ;interrupt routine - reads the keyboard key press and place char receive in a specific memory location (FDFFh)

	PUSH AF			; save Accumulator and Flags to Stack
	IN A,(key_port)	; get a character from the keyboard (port 40h)
	LD (keyb_buf),A	; put character into keyboard buffer (1 byte)
	POP AF			; retrieve Accumulator and Flags from Stack
	EI  			; enable interrupts
	RETI  			; return from interrupt

.ORG 0100h  ; main rountine starts here

; Base UART I/O port address is 20h
; UART registers on subsequent addresses
;
UART_BASE	.EQU 20h
UART_THR	.EQU UART_BASE + 00h
UART_IER	.EQU UART_BASE + 01h
UART_FCR	.EQU UART_BASE + 02h
UART_LCR	.EQU UART_BASE + 03h
UART_MCR	.EQU UART_BASE + 04h
UART_LSR	.EQU UART_BASE + 05h
UART_MSR	.EQU UART_BASE + 06h
UART_SR		.EQU UART_BASE + 07h

; Define some char constants for UART
;
eos         .EQU 00h		; End of string (when the keyboard buffer is read, it is resetted by writing a eos on it)
cr          .EQU 0Dh		; Carriage return
lf          .EQU 0Ah		; Line feed
space       .EQU 20h		; Space
tab         .EQU 09h		; Tabulator

; IDE and Keyboard I/O ports adresses
;
key_port	.EQU 40h		; keyboard I/O address
ide_base	.EQU 60h		; disk I/O address 

; VDP (TMS9918A) Command and Data ports
;
VDP_DATAP	.EQU 0C0h		; read/write to VRAM
VDP_CMDP	.EQU 0C1h		; read/write VDP registers, write VRAM address

; VDP VRAM bank select ports

VDP_VBANK0	.EQU 0C2h		; select VRAM Bank 0
VDP_VBANK1	.EQU 0C6h		; select VRAM Bank 1


; Memory addresses
;
rombeg		.EQU 0000h			; ROM start address
romend		.EQU 3FFFh			; ROM end address
rambeg		.EQU 4000h			; RAM start address		
ramend		.EQU 0FFFFh			; RAM end address
buffer      .EQU ramend - 01FFh	; 512 byte IDE general purpose buffer (FE00h)
keyb_buf	.EQU buffer - 01h	; keyboard buffer (FDFFh)

; 'main' starts here
;
main:

; Init stack to end of IDE buffer (FDFEh)
;	
	LD SP, buffer - 02h

; Init UART 
; taken from http://www.cosam.org/projects/z80/serial.html
;
	
	; disable all interrupts
	LD A, 00h ; load h00 to accumulator
 	OUT (UART_IER), A ; send to Interrupt Enable Register
		
	; set LCR bit 7 to 1 in order to activate Baud Rate Generator Divisor 
	LD A, 80h ; mask to set DLAB on
	OUT (UART_LCR), A ; send to Line Control Register
		
	; divisor of 12 = 9600 bps with 1.8432 MHz clock (1843200 Hz / 16 / 12 = 9600 bps)
	LD A, 12 ; this will be the LSB of the divisior (h0c)
	OUT (UART_THR), A ; sent to THR (DLL - LSB of Baud Rate Generator Divisor)
	
	;now MSB of Baud Rate Generator Divisor is set (DLM) 
	LD A, 00 ; load h00 to accumulator send to the MSB register
	OUT (UART_IER), A ; send to IER (DLM - MSB of Baud Rate Generator Divisor)
	
	; 8 bits, 1 stop, no parity (and clear DLAB)
	LD A, 03h ; load h03 to accumulator
	OUT (UART_LCR), A ; write new value to LCR

; Initialize keyboard buffer
;
	IM 1				; set interrupt mode 1
	EI  				; enable interrupts
	LD HL, keyb_buf		; HL points to keyboard buffer
	LD (HL), eos		; reset keyboard buffer 

; Display welcome messages and prompt
;
    ;            LD      HL, welcome_msg	; display welcome message 
    ;            CALL    puts			; on serial terminal
				;CALL	crlf			; skip a line
;loop;:			LD 		HL, com_msg		; display available commands
	   ;         CALL    puts			; on serial terminal
				;CALL	crlf			; skip a line
				;	
;loop;1:			CALL	read_keyb		; put the char inside the keyboard buffer into Accumulator
				;CP		eos
				;JP		Z, loop1
				;CALL	putc			; print the character
				;CALL	crlf			; skip a line
				;CP		'a'				; if the char in Accumulator is 'a'
				;JP		Z, start		; start VDP test
				;JP 		loop			; else jump back to commands message

; ;D;efine main routine messages
;
;w;el;come_msg		.DB		"VDP TMS99810A test.", cr, lf, eos;
;com_;msg			.DB		"Press 'a' to start.", cr, lf, eos;

; ;Program start
;
start:

; Select VRAM Bank 0
;
				XOR A					; Accumulator to zero
				OUT (VDP_VBANK0),A		; write port to select VRAM Bank 0

; Let's set VDP write address to #0000
;
				XOR A
				OUT (VDP_CMDP),A		; lower 8 bits of VRAM address 
				LD A,40h
				OUT (VDP_CMDP),A		; high 8 bits of VRAM address (first two bits are '01')

; Now let's clear first 16Kb of VDP memory
;
				LD B,0h
				LD HL,3FFFh
				LD C,VDP_DATAP
clear:
				OUT (C),B
				DEC HL
				LD A,H
				OR L
				NOP 					; Let's wait 8 clock cycles just in case VDP is not quick enough.
				NOP
				JR NZ,clear

; Now it is time to set up VDP registers
; to activate 40 columns text mode
;----------------------------------------
; Register #0 to 0h (00000000)
; Set mode selection bit M3 to zero (bit 6) and 
; disable external VDP Plane (bit 7 = '0')

				LD C,VDP_CMDP
				LD E,80h				; E points to register #0

				OUT (C),A				; data to be written (i.e. 0h)
				OUT (C),E				; select register #0

;---------------------------------------- 
; Register #1 to D0h (11010000)
; Select 16K VRAM, 40 column text mode, enable screen and disable vertical interrupt

				LD A,0D0h
				INC E					; E points to register #1
	
				OUT (C),A
				OUT (C),E

;---------------------------------------- 
; Register #2 to 0h (00000000)
; Set starting VRAM addess of Name Table to 0000h (0*400h)

				XOR A					; set A to 00h
				INC E					; E points to register #2
				
				OUT (C),A
				OUT (C),E

;---------------------------------------- 
; Register #3 is ignored as 40 column text mode does not need color table

				INC E					; E points to register #3

;---------------------------------------- 
; Register #4 to 1h (00000001)
; Set starting VRAM address of Pattern Table to 800h (1*800h)

				INC A					; set A to 1h (0h +1)
				INC E					; E points to register #4
				
				OUT (C),A
				OUT (C),E

;---------------------------------------- 
; Registers #5 (Sprite Attribute) & #6 (Sprite Pattern) are ignored 
; as 40 column text mode does not have sprites

				INC E					; E points to register #5
				INC E					; E points to register #6

;---------------------------------------- 
; Register #7 to F1h (11110001)
; Set text colors to white on black

				LD A,0F1h
				INC E					; E points to register #7
				
				OUT (C),A
				OUT (C),E

;----------------------------------------
; Let's set VDP write address to 808h so, that we can write
; character set to memory into Pattern Table (starting at 800h)
; (No need to write SPACE it is clear char already, being the first char in pre-initialized VRAM - so add 8h to start address)

				LD A,8					; skip fist byte (space character)
				OUT (C),A
				LD A,48h				; addess low byte with '01' MSB (write to VRAM)
				OUT (C),A

; Let's copy the character set

				LD HL,chars				; HL points to start of char set
				LD B,chars_end-chars	; load in B total number of bytes to be copied
copychars:
				LD A,(HL)
				OUT (VDP_DATAP),A		; write data byte to VDP VRAM
				INC HL
				NOP 					; Let's wait 8 clock cycles just in case VDP is not quick enough
				NOP
				DJNZ copychars			; write loop

; Let's set write address to start of Name Table (0000h)

				XOR A					; A to zero
				OUT (C),A
				LD A,40h				; addess low byte with '01' MSB (write to VRAM)
				OUT (C),A

; Let's put characters to screen

				LD HL,order
				LD B,order_end-order
copyorder:
				LD A,(HL)
				OUT (VDP_DATAP),A
				INC HL
				DJNZ copyorder

; The end
				HALT

; Character set:
; --------------

order:
	.DB 1,2,3,3,4,0,5,4,6,3,7
order_end:

chars:

; H
	.DB %10001000
	.DB %10001000
	.DB %10001000
	.DB %11111000
	.DB %10001000
	.DB %10001000
	.DB %10001000
	.DB %00000000
; e
	.DB %00000000
	.DB %00000000
	.DB %01110000
	.DB %10001000
	.DB %11111000
	.DB %10000000
	.DB %01110000
	.DB %00000000
; l
	.DB %01100000
	.DB %00100000
	.DB %00100000
	.DB %00100000
	.DB %00100000
	.DB %00100000
	.DB %01110000
	.DB %00000000
; o
	.DB %00000000
	.DB %00000000
	.DB %01110000
	.DB %10001000
	.DB %10001000
	.DB %10001000
	.DB %01110000
	.DB %00000000
; W
	.DB %10001000
	.DB %10001000
	.DB %10001000
	.DB %10101000
	.DB %10101000
	.DB %11011000
	.DB %10001000
	.DB %00000000

; r
	.DB %00000000
	.DB %00000000
	.DB %10110000
	.DB %11001000
	.DB %10000000
	.DB %10000000
	.DB %10000000
	.DB %00000000
; d
	.DB %00001000
	.DB %00001000
	.DB %01101000
	.DB %10011000
	.DB %10001000
	.DB %10011000
	.DB %01101000
	.DB %00000000

chars_end:

;----------------------------------------------------------------------------
; Read Keyboard:
;----------------------------------------------------------------------------
;
;  Copies keyboard buffer content (created by interrupt routine) into Accumulator and reset the buffer 
;

read_keyb:

			LD A, (keyb_buf)	; put character in keyboard buffer into Accumulator
			LD HL, keyb_buf		; point to keyboard buffer
			LD (HL), eos		; reset keyboard buffer writing eos on it
			RET

;----------------------------------------------------------------------------
; UART functions 
;----------------------------------------------------------------------------

; Read a single character from the serial line, result is in A
;

getc:           CALL    rx_ready
                IN      A, (UART_THR)
                RET

; Send a single character to the serial line (A contains the character)
;

putc:
				CALL    tx_ready
                OUT     (UART_THR), A
                RET

; Send a CR/LF pair
;
crlf:
				PUSH    AF
                LD      A, cr
                CALL    putc
                LD      A, lf
                CALL    putc
                POP     AF
                RET

; Send a string to the serial line, HL contains the pointer to the string
;

puts:
				PUSH    AF
                PUSH    HL
puts_loop:		LD      A, (HL)
                CP      eos             ; End of string reached?
                JR      Z, puts_end     ; Yes
                CALL    putc
                INC     HL              ; Increment character pointer
                JR      puts_loop       ; Transmit next character
puts_end:		POP     HL
                POP     AF
                RET

; Wait for UART to become ready to transmit a byte
;

tx_ready:
				PUSH    AF
tx_ready_loop:	IN      A, (UART_LSR)
                BIT     5, A
                JR      z, tx_ready_loop
                POP AF
                RET

; Wait for an incoming character on the serial line
;

rx_ready:
				PUSH    AF
rx_ready_loop:  IN      A, (UART_LSR)
                BIT     0, A
                JR      Z, rx_ready_loop
                POP     AF
                RET				

.END
