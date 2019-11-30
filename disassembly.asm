; Disassembly of the code of the MSX-1 BOOGA-BOO game.
; Under GNU-GPL by Miguel Colom. See LICENSE file for details.
;
; Disassembly of the original work of Steve & Ann Haigh (Quicksilva).

; -------------------------------------------------------------------

; During play,  VDP mode 1 (mode 0, BASIC SCREEN 1, VDP GRAPHIC 1)
; During title, VDP mode 2 (BASIC SCREEN 2, VDP GRAPHIC 2)
; Pulga, Bicho, and plants are sprites

; Include MSX BIOS and system variables definitions
include 'headers/bios.asm'
include 'headers/video.asm'

VDP_VRAM_PORT: equ 0x98
CURSOR_OFFSET_IN_ROW:  equ 0xfcb3
CURSOR_ROW:  equ 0xfcb4

PLANT_BASE_CHR: equ 0x3F

; Tiles whose value belongs to [0, ..., 24] are not compressed.
; Tiles over 24 are compressed
MAX_WITH_COMPRESSION: equ 24

org	08000h

; *********************************************************************
; * Print the string @DE with white foreground and black background   *
; * Used in SCREEN 2 (title)                                          *
; *********************************************************************	
PRINT_LOCATE_WHITE:
	ld a,COLOR_WHITE_BLACK	;8000

; *********************************************************************
; * Print the string @DE with the color in A at position DE           *
; * Used in SCREEN 2 (title)                                          *
; *********************************************************************	
PRINT_LOCATE_COLOR:
	ld (FORE_AND_BACK_COLORS),a
	ld (CURSOR_OFFSET_IN_ROW),hl		;8005

; Print string @DE
l8008h:
	ld a,(de)		;8008
	inc de			;8009
	or a			;800a
	ret z			; Return if finished printing
	call PRINT_CHAR		;800c
	jr l8008h		;800f

; *********************************************************************
; * Print the 5-digit number in HL                                    *
; * Used in SCREEN 2 (title)                                          *
; *********************************************************************
PRINT_NUMBER:
	ld de, 10000		;8011
	call PRINT_NUMBER_DIGIT		;8014
	ld de,  1000		;8017
	call PRINT_NUMBER_DIGIT		;801a
	ld de,   100		;801d
	call PRINT_NUMBER_DIGIT		;8020
	ld de,    10		;8023
	call PRINT_NUMBER_DIGIT		;8026
	ld a,l			;8029
	call PRINT_DIGIT		;802a
	xor a			;802d
	jp PRINT_DIGIT		;802e

; *********************************************************************
; * Print the digit of the number in HL given the decimal position DE *
; * For example, if HL = 12345 and DE = 100, print '3'.               *
; * Used in SCREEN 2 (title)                                          *
; *********************************************************************
PRINT_NUMBER_DIGIT:
	; Obtain HL \ DE
	xor a			;8031	af 	. 
l8032h:
	inc a			;8032	3c 	< 
	sbc hl,de		;8033	ed 52 	. R 
	jr nc,l8032h		;8035	30 fb 	0 . 
	add hl,de			;8037	19 	. 
	dec a			;8038	3d 	= 
PRINT_DIGIT:
	add a,030h		;8039 Convert digit to ASCII code

; ***********************************************************
; * Print the character given in A in position CURSOR with  *
; * foreground/background color in (FORE_AND_BACK_COLORS).  *
; * Used in SCREEN 2 (title)                                *
; ***********************************************************
PRINT_CHAR:
	push hl			;803b
	push de			;803c
	push bc			;803d
	ld de,(CGTABL)	; MSX Character Set address in Main-ROM
	or a			;8042
	jp p,l8049h		;8043
	ld de,BOOGABOO_TITLE_PATTERNS	; If A > 127 use addr DE=0x9233 (BOOGA-BOO patterns) instead of DE=CGT
l8049h:
	ld l,a			;8049
	ld h,000h		; HL = A
	add hl,hl
	add hl,hl
	add hl,hl		; HL = 8*A
	add hl,de		; HL = 8*A + CGT. It points to the character definition of A.
					; It might use DE=0x9233 instead of CGT.
	ld de,(CURSOR_OFFSET_IN_ROW)	; DE = CURSOR
	push de				; Save CURSOR
		
	; Have a look at the organization of SCREEN 2 in, for example, the MSX Red Book, Figure 20.
	; CURSOR_ROW chooses the row ([0..23])
	; CURSOR_OFFSET_IN_ROW is the line offset within the row
	;
	; For example, if DE = 0x0f68 then CURSOR_ROW = 0x0f = 15 and CURSOR_OFFSET_IN_ROW = 104 (13*8).
	; The offset within a row refers to the number of lines.
	; Thus, CURSOR = 0x0F68 ==> [X, Y] = [13, 15]

	ld bc,00008h	 ; Copy 8 rows of the character
	; Now we have in HL the address of character #A as HL = 8*A + CGT.
	; The following call will copy its 8 rows to VRAM to position CURSOR.
	call LDIRVM		 ; Move BC=8 bytes from RAM @HL to VRAM @DE=CURSOR
	pop hl			 ; Recover HL = CURSOR
	push hl			 ; Save CURSOR
	
	
	; Set HL to the Color Table.
	; As pointed out in the MSX Red Book:
	; "There is an exact byte-to-byte mapping from the Character Pattern Table to the Colour Table."
	; Since the CT start at 0x2000, we need to add 0x20 to H (this is, activating bit 5).
	set 5,h			 ; HL = CURSOR + 0x2000 (CT)
	ld a,(FORE_AND_BACK_COLORS)	; Get color in A
	ld bc,00008h	 ; 8062
	call FILVRM		 ; Copy value A BC=8 times to VRAM @HL (fill color)
	pop de			 ; Recover DE = CURSOR
	ld hl,00008h	 ; 8069
	add hl,de		 ; HL = CURSOR + 8 = CURSOR Y, CURSOR_OFFSET_IN_ROW + 8
	ld (CURSOR_OFFSET_IN_ROW),hl ; CURSOR_OFFSET_IN_ROW += 8 (increments cursor for next character)
	pop bc
	pop de
	pop hl
	ret				;8073


; *******************************************************
; * Wait until a key stroke or joystick trigger pressed *
; *******************************************************
; User when entering the name in the score
WAIT_KEY:		; 8074
	ld a,001h
	call GTTRIG
	or a
	ld a,00dh
	ret nz ; Joystick trigger pressed
	call CHSNS
	jr z,WAIT_KEY ; Wait until keyboard not empty...
	call BEEP
	jp CHGET ; Return pressed key


; *********************************************************************
; * Switch to screen 2. Set back and border colors to black (1)       *
; *********************************************************************
SCREEN2:
	ld a, COLOR_TRANSPARENT_BLACK
	ld (BAKCLR),a
	ld (BDRCLR),a
	call CHGCLR
	call INIGRP ; Screen 2
	call SETGRP	; VDP Screen 2
	ret

; *********************************************************
; * Write scenario and the plant, if any.                 *
; * It draws the scenario from (SCROLL_X) and (SCROLL_Y). *
; *********************************************************
WRITE_SCENARIO:
	; Input: DE = SCROLL
	ld hl,NAME_TABLE ;809a
	xor a			 ;809d
	ld (PLANT_IS_VISIBLE),a ;809e Clear presence of plant. It'll be set if any found.
	call SETWRT		 ;80a1 VRAM write in HL == NAME_TABLE = 0x1800
	ex de,hl		 ;80a4 HL = DEo, DE = NAME_TABLE
	; DE = 0x1800, HL = 0
	ld (SCROLL_X),hl ;80a5 SCROLL = DEo
	ld c,l			 ;80a8 C = SCROLL_X
	ld a,h			 ;80a9 A = SCROLL_Y
	; C = SCROLL_X
	; A = SCROLL_Y
	; It needs to multiply by two because each pointer is 2 bytes long
	add a,a			 ;80aa A = 2*SCROLL_Y

	ld h,000h		 ;80ab
	ld l,a			 ;80ad HL = 2*SCROLL_Y
	ld de, GAME_MAP_POINTERS	 ;80ae
	add hl,de		 ;80b1 HL = GAME_MAP_POINTERS + 2*SCROLL_Y = GAME_MAP_POINTERS[SCROLL_Y]
	ld d, 22		 ;80b2 D = rows left

l80b4h:
	; Using HL as a GAME_MAP_POINTERS pointer (pointer to the rows)
	ld a,(hl)			;80b4 A = GAME_MAP_POINTERS[SCROLL_Y]
	inc hl				;80b5
	ld e,(hl)			;80b6 E = GAME_MAP_POINTERS[SCROLL_Y+1]
	inc hl				;80b7
	
	; A, E = two reads of (HL)
	; AE = GAME_MAP_POINTERS[SCROLL_Y] (a word)
	; A = 8B, E = 96 (this is what's in GAME_MAP_POINTERS). Address 0x968B

	push hl			;80b8
	ld h,e			;80b9
	ld l,a			;80ba HL = EA = GAME_MAP_POINTERS[SCROLL_Y]
	ld a,(hl)		;80bb A = *GAME_MAP_POINTERS[SCROLL_Y]. This read a tile
	or a			;80bc tile == 0? Termination mark.
	jr z,l80d7h		;80bd 	Yes, get out
	ld e,000h		;80bf 	No

; Skip row data until it reaches the visible start (SCROLL_X)
; It takes into account the codes indicating repetition of blanks (when tile <= MAX_WITH_COMPRESSION)
; Input E: current column (COL)
l80c1h:
	ld a,e			;80c1
	sub c			;80c2 A = COL - SCROLL_X
	jr z,l80deh		;80c3 Done if COL == SCROLL_X
	jr nc,l80d3h	;80c5 Jump if COL > SCROLL_X
	; Here COL < SCROLL_X
	ld a,(hl)		;80c7
	inc hl			;80c8 Read tile and increment pointer
	inc e			;80c9 next COL
	cp MAX_WITH_COMPRESSION ;80ca Is it a compression code?
	jr nc,l80c1h	;80cc No, keep skipping
	
	; It's a repetition code
	; A: number of repetitions of the blank
	dec e			;80ce
	add a,e			;80cf A = NUM_DUPS + COL - 1; 
	ld e,a			;80d0 Add the number of repetitions to the current COL
	jr l80c1h		;80d1 Keep iterating...

l80d3h:
	ld b, 32		;80d3	06 20 	.   
	jr l80e6h		;80d5	18 0f 	. . 
l80d7h:
	ld b, 32		;80d7	06 20 	.   
	ld e,b			;80d9	58 	X 
	ld a,02bh		;80da	3e 2b 	> + 
	jr loop_write_vram_E_times		;80dc	18 0b 	. .

; Write a row
l80deh:
	ld b, 32		;80de Columns left

loop_write_HL_vram_B_times:
	ld a,(hl)			;80e0
	inc hl				;80e1
	cp MAX_WITH_COMPRESSION 				;80e2
	jr nc,tile_write		;80e4 if (HL) <= MAX_WITH_COMPRESSION then tile_write (normal write, no repetitions)
	; (HL) <= 24
	; This is used the draw the large empty spaces between flying rocks
l80e6h:
	ld e,a			;80e6	E = (HL). This is the number of iterations to ...
	ld a, ' '		;80e7   ... write ' '. Compressed data!

; Loop to write the value A to VRAM E times (or when ended, B == 0)
loop_write_vram_E_times:
	out (VDP_VRAM_PORT),a		  ;80e9 Write VRAM
	dec b						  ;80eb
	jr z,draw_plant				  ;80ec DEC B. If B==0 then done. Then draw the plant.
	dec e						  ;80ee
	jr nz,loop_write_vram_E_times ;80ef If E != 0 then keep iterating.
	jr loop_write_HL_vram_B_times ;80f1 E == 0, then done

tile_write:
	out (VDP_VRAM_PORT),a		;80f3
	cp PLANT_BASE_CHR			;80f5
	jr nz,l8108h				;80f7
	ld (PLANT_IS_VISIBLE),a		;80f9 Set PLANT_IS_VISIBLE = PLANT_BASE_CHR
	; We've drawn the base of a plan
	; It gets the plant column as 32 - cols_left and the
	; current row as 22 - rows_left
	ld a, 32					;80fc
	sub b						;80fe
	ld (PLANT_X),a 		;80ff
	ld a, 22					;8102
	sub d						;8104
	ld (PLANT_Y),a 			;8105
l8108h:
	djnz loop_write_HL_vram_B_times		;8108

; Draw the plant
draw_plant:
	pop hl			;810a Recover HL (current row)
	dec d			;810b Decrement plant row
	jp nz,l80b4h	;810c If PLANT_Y != current_row, get out. Keep drawing scenario rows
	ld d,0c0h		;810f
	ld a,(PLANT_IS_VISIBLE)		;8111
	or a						;8114
	jr z,draw_plants_scroll		;8115
	ld hl,(PLANT_X)		;8117
	add hl,hl			;811a
	add hl,hl			;811b
	add hl,hl			;811c ; HL = 8 * PLANT_X
	ld a,l			;811d
	or a			;811e
	; Draw plants with Early Clock if 8 * PLANT_X == 0
	jr z,draw_plants_scroll_EC		;811f

	ld de,0eefbh		;8121 -18, -5
	add hl,de			;8124	19 	. 
	ld (PLANT_X),hl		;8125	22 46 bf 	" F . 
	ex de,hl			;8128	eb 	. 
	
draw_plants_scroll:
	; Set position and color of the plants
	ld hl,SPRITE_ATTRIBUTE_TABLE + 4*4	;8129 Sprite #4
	call WRITE_DE_VRAM_FROM_HL_INC		;812c Set position
	inc hl								;812f Skip sprite pattern
	ld a, COLOR_DARK_YELLOW				;8130
	jp WRTVRM							;8132 Set color

draw_plants_scroll_EC:
	ld de,0ee1bh		;8135	-18, 27
	add hl,de			;8138	19 	. 
	ld (PLANT_X),hl		;8139	22 46 bf 	" F . 
	ex de,hl			;813c	eb 	. 

	; Set position and color of the plants
	ld hl,SPRITE_ATTRIBUTE_TABLE + 4*4	;813d Sprite #4
	call WRITE_DE_VRAM_FROM_HL_INC		;8140 Set position
	inc hl								;8143 Skip sprite pattern
	ld a, COLOR_DARK_YELLOW_WITH_EC		;8144
	jp WRTVRM							;8146 Set yellow color and activate Early Clock

; *******************************************************
; * Write two bytes (D and E) in VRAM @HL. Increment HL *
; *******************************************************
WRITE_DE_VRAM_FROM_HL_INC: ;8149
	ld a,d
	call WRTVRM
	inc hl
	ld a,e
	call WRTVRM
	inc hl
	ret

; ************************************************************************
; * Print the digit of the number in HL given the decimal position DE    *
; * For example, if HL = 12345 and DE = 100, print '3'.                  *
; * It won't write anything to (BC). Since it uses PRINT_STORE_NUMBER_S1 *
; * it sets BC = 0 (in the ROM), which has no effect.                    *
; * Used in SCREEN 2 (title)                                             *
; ************************************************************************
PRINT_NUMBER_S1: ;8154
	ld bc, 0		

; *********************************************************************
; * Print the digit of the number in HL given the decimal position DE *
; * For example, if HL = 12345 and DE = 100, print '3'.               *
; * It stores the 4 digits in (BC) too.                                 *
; * Used in SCREEN 2 (title)                                          *
; *********************************************************************
PRINT_STORE_NUMBER_S1: 			;8157
	ld de, 1000				;8157
	call PRINT_STORE_BC_DIGIT_S1		;815a
	ld de,  100				;815d
	call PRINT_STORE_BC_DIGIT_S1		;8160
	ld de,   10				;8163
	call PRINT_STORE_BC_DIGIT_S1		;8166
	ld a,l					;8169
	out (VDP_VRAM_PORT),a	;816a
	ld (bc),a				;816c
	ret						;816d


; **********************************************************************
; * Print the digit of the number in HL given the decimal position DE. *
; * It also stores the digit in @BC in RAM and increments BC.          *
; * For example, if HL = 12345 and DE = 100, print '3'.                *
; * Used in SCREEN 1 (game).                                           *
; **********************************************************************
PRINT_STORE_BC_DIGIT_S1: ;816eh
	xor a
l816fh:
	inc a
	sbc hl,de
	jr nc,l816fh
	add hl,de
	dec a				  ; A = HL \ DE
	out (VDP_VRAM_PORT),a ; 8176 Write A to VRAM
	; Previously the programmer put the characters '0', '1', ... at
	; position 0, 1, ... in the CGT. This way they can now simply
	; write the number and OUT it to VRAM
	ld (bc),a			  ;8178 Store A in (BC)
	inc bc		     	  ;8179	Next BC
	ret			          ;817a


DELAY_HL:
	dec hl			;817b	2b 	+ 
	ld a,l			;817c	7d 	} 
	or h			;817d	b4 	. 
	jr nz,DELAY_HL		;817e	20 fb 	  . 
	ret			;8180	c9 	. 

; Simulated jump
; Input HL: length of jump (how many ticks the simulated key is pressed)
SIMULATED_JUMP:
	push hl			;8181	e5 	. 
	call STOP_JUMP_ERASE_BAR		;8182	cd d9 8b 	. . . 
	pop hl			;8185	e1 	. 
	
	; Change LD A,0; NOP ;89fc to NOP; LD A,3 (3=jump_right)
	; This changes the user input by a jump_right
	ld a,003h		;8186 INC BC opcode
	ld (l89fch+2),a		;8188	32 fe 89 	2 . . 

l818bh:
	push hl			;818b
	call GAME_MOVE_CYCLE	;818c
	ld b,06eh		;818f
l8191h:
	djnz l8191h		;8191 A small delay
	pop hl			;8193
	dec hl			;8194 Done tick: decrement
	ld a,l			;8195
	or h			;8196
	jr nz,l818bh	;8197 If not all ticks done, loop

	xor a			;8199
	ld (l89fch+2),a	;819a Auto-modification: release the jump key
	ld hl,00500h	;819d For 500 ticks
l81a0h:
	push hl			;81a0	e5 	. 
	call GAME_MOVE_CYCLE	;81a1	cd 12 89 	. . . 
	ld b,06eh		;81a4 
l81a6h:
	djnz l81a6h		;81a6 Same small delay
	pop hl			;81a8
	dec hl			;81a9
	ld a,l			;81aa
	or h			;81ab
	jr nz,l81a0h	;81ac If not done, keep looping the key released simulation
	ret				;81ae Done



; ***************************************
; *             ENTRY POINT             *
; ***************************************

; The program starts here
START:
	di
	ld sp,0ffffh
	call ERAFNK ; Erase functionkey display
	
	; Set sprite size to 16x16
	ld hl, RG1SAV ; Mirror of VDP register 1 (Basic: VDP(1))
	set 1,(hl)
	ld b,(hl)
	ld c,001h
	call WRTVDP

	; Reset score table
	; 5 entries, with 2 bytes for the record and 14 for the name.
	ld hl, SCORE_TABLE
	ld b, 5 * (2 + 14)
l81c6h:
	ld (hl),000h
	inc hl
	djnz l81c6h

	ld a,003h		;81cb	3e 03 	> . 
	ld (RANDOM_SOUND_BYTE),a		;81cd	32 59 bf 	2 Y . 
	
; Print title screen text messages (and BOOGA-BOO with redefined patterns)
; From QUICKSILVA_STR = 0x94DF to 0x95BC (222 bytes)
l81d0h:
	ld sp,0ffffh		;81d0	31 ff ff 	1 . . 
	call SCREEN2		;81d3	cd 88 80 	. . . 
	call CLS_PRINT_SCORE_TABLE		;81d6	cd 61 85 	. a . 
	ld de,QUICKSILVA_STR		;81d9	11 df 94 	. . . 
	ld hl,00018h		;81dc	21 18 00 	! . . 
	call PRINT_LOCATE_WHITE		;81df	cd 00 80 	. . . 
	ld hl,00218h		;81e2	21 18 02 	! . . 
	ld b,004h		;81e5 Four strings to print
l81e7h:
	; Here it prints the big "BOOGA-BOO" title and
	; "By Steve & Ann Haigh"
	call PRINT_LOCATE_WHITE		;81e7	cd 00 80 	. . . 
	inc h			;81ea	24 	$ 
	djnz l81e7h		;81eb	10 fa 	. . 
	ld hl,00728h		;81ed	21 28 07 	! ( . 
	ld a,COLOR_YELLOW_BLACK		;81f0
	call PRINT_LOCATE_COLOR		;81f2
	ld hl,00a28h		;81f5	21 28 0a 	! ( . 
	ld b,004h		;81f8 Strings to print
l81fah:
	; Here it prints "Hit fire button." and the following text
	ld a,COLOR_GRAY_BLACK		;81fa
	call PRINT_LOCATE_COLOR		;81fc	cd 02 80 	. . . 
	inc h			;81ff	24 	$ 
	djnz l81fah		;8200	10 f8 	. . 
	ld a,0f0h		;8202	3e f0 	> . 
	ld (TIMEOUT),a		;8204	32 32 bf 	2 2 . 

; Wait until the space bar or the joystick has been triggered, or a timeout.
; Meanwhile, roll colors in the BOOGABOO title screen

title_screen_wait:
	call CHSNS		;8207	cd 9c 00 	. . . 
	jr nz,l823dh		;820a	20 31 	  1 
	ld a,001h		;820c	3e 01 	> . 
	call GTTRIG		;820e	cd d8 00 	. . . 
	or a			;8211	b7 	. 
	ld a,001h		;8212	3e 01 	> . 
	jr nz, init_game	;8214	User pressed space or trigger. Start game
	ld a,(FORE_AND_BACK_COLORS)		;8216
	ld hl,PIXELBYTE_COLOUR_TABLE + 0200h		;8219
	ld bc,00400h		;821c BC: counter for number of color rotations

title_roll_colors:
	add a,010h		;821f	Next color
	cp 0f1h			;8221
	jr nz,l8227h	;8223
	ld a,021h		;8225   if color == 241 then color = 33 
l8227h:
	ld d,a			;8227
	call WRTVRM		;8228	Update color in VRAM
	inc hl			;822b	Next screen position
	dec bc			;822c	BC -= 1
	ld a,b			;822d
	or c			;822e
	ld a,d			;822f
	jr nz,title_roll_colors	;8230	if BC == 0 then keep rolling colors

	ld (FORE_AND_BACK_COLORS),a		;8232
	ld hl,TIMEOUT		;8235
	dec (hl)			;8238
	jr z,init_game		;8239	Timeout: start game
	jr title_screen_wait		;823b	18 ca 	. . 
l823dh:
	call CHGET		;823d
	cp 020h		;8240 Space pressed?
	ld a,000h		;8242	3e 00 	> . 
	jr nz,title_screen_wait		;8244	20 c1 	  .


init_game:
	; a = 0: keyboard
	; a = 1: joystick
	; It uses auto-modified code and changes the instructions to take
	; into account the keyboard/joystick choice (!)
	ld (08955h),a		;8246
	ld (08976h),a		;8249
	ld (089fbh),a		;824c

	ld hl,00000h		;824f	21 00 00 	! . . 
	ld (SCORE),hl		;8252	22 56 bf 	" V . 
start_game:
	ld a,001h		;8255	3e 01 	> . 
	ld (BAKCLR),a		;8257	32 ea f3 	2 . . 
	ld (BDRCLR),a		;825a	32 eb f3 	2 . . 
	call CHGCLR		;825d	cd 62 00 	. b . 
	call INIT32		;8260	cd 6f 00 	. o . 
	call SETT32		;8263	cd 7b 00 	. { . 
	xor a			;8266	af 	. 
	call CLS		;8267	cd c3 00 	. . . 
	ld hl,00000h		;826a	21 00 00 	! . . 
	call SETWRT		;826d	cd 53 00 	. S . 
	ld hl,(SCORE_TABLE)		;8270	2a 5a bf 	* Z . 
	ld bc,DECODED_HISCORE		;8273	01 2d 96 	. - . 
	call PRINT_STORE_NUMBER_S1		;8276	cd 57 81 	. W . 
	ld hl,(SCORE)		;8279	2a 56 bf 	* V . 
	ld bc,DECODED_SCORE		;827c	01 24 96 	. $ . 
	call PRINT_STORE_NUMBER_S1		;827f	cd 57 81 	. W . 

	; Set characters
	ld hl,(CGTABL)		; MSX Character Set address in Main-ROM
	ld de, 48*8			;8285
	add hl,de			;8288	HL = CGT + 48*8
	ld de,00000h		;8289
	ld bc,00050h		;828c
	; LDIRVM: copy BC bytes from HL to VRAM's DE
	call LDIRVM			;828f	Copy 50h bytes from CGT+48*8 (the numbers) to VRAM's 0 (CGT)
	; With this call it can print the number directy by writing the byte number(0, 1, 2, ...) to VRAM

	; Set game tiles/characters
	ld hl,GAME_TILES	;8292
	ld de,00050h		;8295
	ld bc,246*8			;8298
	call LDIRVM			;829b	Copy 1968 bytes from 0xADC4 to VRAM's 50h (CGT+0x50)

	; Set colors
	; From http://bifi.msxnet.org/msxnet/tech/tms9918a.txt :
	; Each character is 8 x 8. The character number refers to an character pattern in
	; the PG, which has 256 characters patterns. Each pattern is 8 bytes, so the
	; entire PG is 256 x 8 = 2048 bytes. Each byte is a pixel line of the character
	; (top first). If a bit is set, the foreground colour in the CT is shown,
	; otherwise the background colour in the CT.
	;
	; The CT sets the colour of the character. Each byte in the CT sets the colour
	; of 8 complete characters, so the CT is 32 bytes. The high four bits set the
	; foreground colour, the low the background colour. If either of these is 0,
	; it is transparent and the colour of the backdrop (BD) is used. To calculate
	; the element in the CT, divide the character number in PN by 8.
	
	ld hl,GAME_COLOR_TABLE		;829e
	ld de,CHARACTERS_COLOUR_TABLE ;82a1
	ld bc, 32			;82a4 The CT has 32 bytes
	call LDIRVM			;82a7	Copy 32 bytes to VRAM's 2000h (characters color table, CCT) from 0xB674

	xor a			;82aa
	ld (FORE_AND_BACK_COLORS),a		;82ab
	call CLRSPR		;82ae

	ld a, COLOR_RED		;82b1
	ld hl,SPRITE_ATTRIBUTE_TABLE + 20*4 + 3 ;82b3
	call WRTVRM		;82b6	Sprite attribute table 1B00-1B7F

	ld a, COLOR_GRAY		;82b9
	ld hl, SPRITE_ATTRIBUTE_TABLE + 6*4 + 3 ;82bb
	call WRTVRM		;82be	Sprite attribute table 1B00-1B7F

	; Reset the 16 first sprites
	ld hl, SPRITE_CHARACTER_PATTERNS	;82c1
	ld bc, 16 * 8	;82c4 16 sprites, each defined by 8 bytes
	ld a, 0xff		;82c7
	call FILVRM		;82c9

	ld hl, SPRITE_ATTRIBUTE_TABLE		;82cc
	ld de,0xb000		;82cf ; D=0xb0, E=0
	ld b,4		;82d2 4 writes of 2 bytes = 8 writes to SPRITE_ATTRIBUTE_TABLE
l82d4h:
	call WRITE_DE_VRAM_FROM_HL_INC		;82d4	cd 49 81 	. I . 
	inc hl			;82d7	23 	# 
	inc hl			;82d8	23 	# 
	djnz l82d4h		;82d9	10 f9 	. . 

	ld hl,02664h		;82db
	ld (PULGA_X),hl		;82de Set PULGA_X=100, PULGA_Y = 38

	ld a,005h		;82e1
	ld (PULGA_ACTION),a		;82e3 PULGA_ACTION = 5 (falling left)

	xor a			;82e6	af 	. 
	ld (BICHO_THREAT_STATUS),a		;82e7	32 3c bf 	2 < . 
	call DO_ANIM_PLANT_FRAME		;82ea	cd ea 88 	. . . 
	call GICINI		;82ed	cd 90 00 	. . . 
	ld a,014h		;82f0	3e 14 	> . 
	ld (l8a4eh+1),a		;82f2	32 4f 8a 	2 O . 

	ld a,(RANDOM_SOUND_BYTE)		;82f5	3a 59 bf 	: Y . 
	add a,a			;82f8	87 	. 
	ld (SOUND_SNAP),a		;82f9	32 6f 8e 	2 o . 

	ld hl,PELICAN_WALK + 1		;82fc
	ld (PELICAN_POS_PTR),hl		;82ff  Point to TY of the first record

	; Set ABS_BICHO_X = BICHO_ABS_TARGET_X = 960
	ld hl,960				;8302
	ld (ABS_BICHO_X),hl		;8305
	ld (BICHO_ABS_TARGET_X),hl	;8308

	; Set ABS_BICHO_Y = BICHO_ABS_TARGET_Y = 552
	ld hl,552				;830b
	ld (ABS_BICHO_Y),hl		;830e
	ld (BICHO_ABS_TARGET_Y),hl	;8311

	ld de,00000h		;8314
	call WRITE_SCENARIO	;8317

	ld (TIMEOUT),a		;831a

	; Disable GTSTCK and GTTRIG
	ld hl,03e00h		;831d	Automodified code: LD A, 0
	ld (l8956h),hl		;8320	Changes GTTRIG ;8956 cd d8 00 to 'LD A,0', 'NOP'
	ld (l89fch),hl		;8323	Changes GTSTCK ;89fc cd d5 00 to 'LD A,0', 'NOP'

	xor a				;8326
	ld (l8956h+2),a		;8327 	Useless? call GTTRIG ;8956 cd d8 *00*
	
	; This is the series of automatic jumps of Pulga at the beginning
	; HL is the number of ticks the jump key is pressed during the simulation

	ld hl,00400h		;832a	21 00 04 	! . . 
	call SIMULATED_JUMP		;832d	cd 81 81 	. . . 

	ld hl,00800h		;8330	21 00 08 	! . . 
	call SIMULATED_JUMP		;8333	cd 81 81 	. . . 

	ld hl,00500h		;8336	21 00 05 	! . . 
	call SIMULATED_JUMP		;8339	cd 81 81 	. . . 

	ld hl,00780h		;833c	21 80 07 	! . . 
	call SIMULATED_JUMP		;833f	cd 81 81 	. . . 

	; Enable back GTSTCK and GTTRIG
	ld hl,GTSTCK		;8342	21 d5 00 	! . . 
	ld (l89fch+1),hl		;8345	22 fd 89 	" . . 

	ld hl,GTTRIG		;8348	21 d8 00 	! . . 
	ld (l8956h+1),hl	;834b	22 57 89 	" W . 

	ld a,0cdh			;834e
	ld (l8956h),a		;8350
	ld (l89fch),a		;8353

	; Set initial depth
	ld a,96					 ;8356
	ld (SCORE_PULGA_DEPTH),a ;8358
	
	ld hl,8000			; We start with BONUS = 8000
	ld (BONUS),hl

	; Write the bottom line, slowly while performing GAME_MOVE_CYCLE
	; bonus=9999 score=00000 hi=00000
	ld hl, NAME_TABLE + 23*32 + 1 	; [1, 23] (row 23, col 1)
	ld de,GRAPH_SCORE_LINE		;8364	11 13 96 	. . . 
l8367h:
	ld a,(de)		;8367 Read character of the bottom line
	inc de			;8368 Point to next character
	inc a			;8369 Done with the line?
	jr z,game_loop		;836a If so, start game
	; No, not done
	dec a			;836c
	call WRTVRM		;836d Print character
	inc hl			;8370 Next position in the screen
	ld b,000h		;8371
l8373h:
	push hl			;8373
	push de			;8374
	push bc			;8375
	call GAME_MOVE_CYCLE	;8376
	ld b,05ah		;8379
l837bh:
	djnz l837bh		;837b Small delay
	pop bc			;837d
	pop de			;837e
	pop hl			;837f

	djnz l8373h		;8380 Repeat 255 times from l8373h (the GAME_MOVE_CYCLE)
					;     It's like a delay
	jr l8367h		;8382 Keep printing characters until line done...

; Here it starts the interactive game
game_loop:
	call BICHO_CYCLE		;8384	cd 08 86 	. . . 
	call GAME_MOVE_CYCLE		;8387	cd 12 89 	. . . 
	call PLANT_CYCLE		;838a	cd d1 88 	. . . 
	ld a,(RESET_PSG_FLAG)		;838d	3a 45 bf 	: E . 
	or a			;8390	b7 	. 
	jr z,l839ah		;8391	28 07 	( . 
	xor a			;8393	af 	. 
	ld (RESET_PSG_FLAG),a		;8394	32 45 bf 	2 E . 
	call GICINI		;8397	cd 90 00 	. . . 
l839ah:
	ld a,(PULGA_Y)		;839a
	rra					;839d
	rra					;839e
	rra					;839f A = 8*PULGA_Y
	and 01fh			;83a0 only values [0, ..., 31]
	ld hl,SCROLL_Y		;83a2
	add a,(hl)			;83a5 A = 8*PULGA_Y + SCROLL_Y
	cp 15				;83a6 
	jr nc,check_new_score		;83a8 Jump if 8*PULGA_Y + SCROLL_Y >= 15
	ld b,a				;83aa B = 8*PULGA_Y + SCROLL_Y
	ld a,(PULGA_X)		;83ab
	rra					;83ae
	rra					;83af
	rra					;83b0 A = 8*PULGA_X
	and 01fh			;83b1 only values [0, ..., 31]
	ld hl,SCROLL_X		;83b3
	add a,(hl)			;83b6 A = 8*PULGA_X + SCROLL_X
	cp 54				;83b7
	ld a,b				;83b9 A = 8*PULGA_Y + SCROLL_Y
	jr nc,check_new_score		;83ba Jump if 8*PULGA_X + SCROLL_X >= 54
	
	; Pulga has managed to escape! :D
	; Indeed, 8*PULGA_Y + SCROLL_Y < 15 ==> Very high and
	; 8*PULGA_X + SCROLL_X < 54 ==> On the left of the map
	; Then, pulga escaped!

	call PLAY_VIVALDI		;83bc	cd 00 8e 	. . . 
	ld hl,00000h		;83bf	21 00 00 	! . . 
	call DELAY_HL		;83c2	cd 7b 81 	. { . 
	call DELAY_HL		;83c5	cd 7b 81 	. { . 

	ld hl, NAME_TABLE + 23*32 + 7 ;83c8 Coord [7, 23]	 

	; Read record from screen
	call RDVRM		;83cb Read digit
	add a,a			;83ce A = 2*digitA
	ld b,a			;83cf B = 2*digitA
	add a,a			;83d0
	add a,a			;83d1 A = 4*2*digitA = 8*digitA
	add a,b			;83d2 A = 8*digitA + 2*digitA = 10*digitA
	ld l,a			;83d3
	ld h,000h		;83d4 HL = 10*digitA

	in a,(VDP_VRAM_PORT)	;83d6 Read digitB
	add a,l					;83d8 A = digitB + 10*digitA
	ld l,a					;83d9 L = digitB + 10*digitA
	add hl,hl				;83da HL = 2*digitB + 20*digitA
	ld c,l					;83db C = 2*digitB + 20*digitA
	ld b,h					;83dc B = 0
	add hl,hl				;83dd HL = 4*digitB + 40*digitA
	add hl,hl				;83de HL = 8*digitB + 80*digitA
	add hl,bc				;83df HL = 8*digitB + 80*digitA + 2*digitB + 20*digitA =
							; = 100*digitA + 10*digitB

	in a,(VDP_VRAM_PORT)	;83e0 Read digitC
	ld c,a					;83e2 C = digit
l83e3h:
	ld b,000h				;83e3
	add hl,bc				;83e5 HL = 100*digitA + 10*digitB + digitC
	ld bc,(SCORE)			;83e6 BC = SCORE
	add hl,bc				;83ea HL = 100*digitA + 10*digitB + digitC + SCORE
	ld (SCORE),hl			;83eb Store new score

	call SCREEN2				;83ee
	ld de,CONGRATULATIONS_STR 	;83f1
	ld hl,00a40h				;83f4
	call PRINT_LOCATE_WHITE		;83f7 Print "CONGRATULATIONS"
	ld hl,00f18h				;83fa
	call PRINT_LOCATE_WHITE		;83fd Print "You dit it"

	ld b,13		;8400 13 delays
l8402h:
	ld hl,00000h		;8402	21 00 00 	! . . 
	call DELAY_HL		;8405	cd 7b 81 	. { . 
	djnz l8402h		;8408	10 f8 	. . 
	jp start_game		;840a	c3 55 82 	. U . 

check_new_score:
	; It arrives here when pulga reaches a new high score
	ld hl,SCORE_PULGA_DEPTH		;840d	21 58 bf 	! X . 
	cp (hl)					;8410 
	jp nc,l842bh			;8411 Skip score update if SCORE_PULGA_DEPTH <= 8*PULGA_Y + SCROLL_Y
	; 8*PULGA_Y + SCROLL_Y (or X) < SCORE_PULGA_DEPTH ==> New record!
	ld b,a					;8414
	ld a,(hl)				;8415
	sub b					;8416 Store height diff. in A
	ld (hl),b				;8417 Store new record height
	ld c,a					;8418 C = height diff
	ld b,000h				;8419
	ld hl, NAME_TABLE + 23*32 + 18 ;841b Coord [18, 23] --> Score="..."
	call SETWRT			 ;841e
	ld hl,(SCORE)		 ;8421
	add hl,bc			 ;8424
	ld (SCORE),hl		 ;8425 SCORE = SCORE + diff
	call PRINT_NUMBER_S1 ;8428 Update SCORE visualization at [18, 23]
l842bh:
	ld a,(JIFFY)		;842b	3a 9e fc 	: . . 
	cp 008h		;842e	fe 08 	. . 
	jp c,l844dh		;8430	da 4d 84 	. M . 
	xor a			;8433	af 	. 
	ld (JIFFY),a		;8434	32 9e fc 	2 . . 
	ld hl, NAME_TABLE + 23*32 + 7 ; [7, 23] row 23, col 7
	call SETWRT		;843a	cd 53 00 	. S . 

	ld hl,(BONUS)
	ld de, 500
	rst 20h
	jr z,l8447h		; BONUS = 500? If so, don't decrement
	dec hl			; Decrement BONUS
l8447h:
	ld (BONUS),hl	; Store updated BONUS value
	call PRINT_NUMBER_S1		;844a	cd 54 81 	. T . 
l844dh:
	ld a,006h		;844d
	call SNSMAT		;844f
	bit 5,a			;8452 line #6, bit #5: F1 key
	jr nz,l846bh	;8454 Jump if F1 not pressed

; Pause/exit control follows

; Pause while F1 pressed
l8456h:
	ld a,006h		;8456
	call SNSMAT		;8458
	bit 5,a			;845b line #6, bit #5: F1 key
	jr z,l8456h		;845d Loop while F1 pressed

	ld a,(RANDOM_SOUND_BYTE)		;845f	3a 59 bf 	: Y . 
	xor 003h		;8462	ee 03 	. . 
	ld (RANDOM_SOUND_BYTE),a		;8464	32 59 bf 	2 Y . 
	add a,a			;8467	87 	. 
	ld (SOUND_SNAP),a		;8468	32 6f 8e 	2 o . 

; End game if user pressed ESC
l846bh:
	ld a,007h		;846b
	call SNSMAT		;846d
	bit 2,a			;8470 line #7, bit #2: ESC key
	jp z,END_GAME	;8472

	bit 4,a			;8475 line #7, bit #4: STOP key
	jr nz,l849ch	;8477 Jump if STOP not pressed

; Here STOP has been pressed.
; Wait until the user stops touching...
l8479h:
	ld a,007h		;8479
	call SNSMAT		;847b
	bit 4,a			;847e line #7, bit #4: STOP key
	jr z,l8479h		;8480 Loop while STOP pressed
	call GICINI		;8482

; Here user touched and released STOP.
; Now wait until STOP is pressed again.
; This pauses the game actually.
l8485h:
	ld a,007h		;8485
	call SNSMAT		;8487
	bit 2,a			;848a line #7, bit #2: ESC key
	jp z,END_GAME	;848c
	bit 4,a			;848f line #7, bit #4: STOP key
	jr nz,l8485h	;8491 Loop until STOP pressed

; Simply wait until user finally releases STOP
l8493h:
	ld a,007h		;8493
	call SNSMAT		;8495
	bit 4,a			;8498 line #7, bit #4: STOP key
	jr z,l8493h		;849a Loop while STOP pressed
; Getting out of stop pause

l849ch:
	call KILBUF		;849c	cd 56 01 	. V . 
	ld a,00dh		;849f	3e 0d 	> . 
	ld (REPCNT),a		;84a1	32 f7 f3 	2 . . 
	ld b,028h		;84a4	06 28 	. ( 
l84a6h:
	djnz l84a6h		;84a6	10 fe 	. . 
	jp game_loop		;84a8	c3 84 83 	. . .


; **********************************************************************
; * End of game.                                                       *
; * Check if the score if a new records, store it in the table if so.  *
; **********************************************************************
END_GAME:
	ld sp,0ffffh		;84ab	31 ff ff 	1 . . 
	call GICINI		;84ae	cd 90 00 	. . . 
	call SCREEN2		;84b1	cd 88 80 	. . . 
	ld hl,01388h		;84b4	21 88 13 	! . . 
	call DELAY_HL		;84b7	cd 7b 81 	. { . 
	ld de,(SCORE)		;84ba	ed 5b 56 bf 	. [ V . 
	call CHECK_STORE_NEW_RECORD		;84be
	jp l81d0h		;84c1	c3 d0 81 	. . . 

; **********************************************************
; * Check if the current score is the highest in the       *
; * table. If so, scan the scores in the table until the   *
; * right sorting position is found, move the existing     *
; * records down, and ask the users to type their name and *
; * store the new entry.                                   *
; *                                                        *
; * Input DE = (SCORE)                                     *
; **********************************************************
CHECK_STORE_NEW_RECORD:
	push de			;84c4	d5
	; Set sprites invisible
	ld a,COLOR_TRANSPARENT_BLACK	;84c5
	ld (FORE_AND_BACK_COLORS),a		;84c7
	call CLRSPR		;84ca
	ld hl,SPRITE_CHARACTER_PATTERNS + 4*1 + 3 ;84cd
	ld a,0ffh		;84d0
	call WRTVRM		;84d2
	pop de			;84d5

	ld hl,SCORE_TABLE		;84d6
	ld b,005h		;84d9 The score table has 5 entries
l84dbh:
	ld c,(hl)		;84db
	inc hl			;84dc
	ld a,(hl)		;84dd
	dec hl			;84de
	push hl			;84df
	ld l,c			;84e0
	ld h,a			;84e1
	rst 20h			;84e2 Compare score in table (HL) with given score (DE)
	pop hl			;84e3
	jr c,l84f2h		;84e4 Jump if DE > HL <==> give score > table score
	ld a,010h		;84e6	3e 10 	> . 
	add a,l			;84e8	85 	. 
	ld l,a			;84e9	6f 	o 
	jr nc,l84edh		;84ea	30 01 	0 . 
	inc h			;84ec	24 	$ 
l84edh:
	djnz l84dbh		;84ed	10 ec 	. . 
	jp CLS_PRINT_SCORE_TABLE		;84ef	c3 61 85 	. a . 

l84f2h:
	dec b			;84f2	05 	. 
	ld a,b			;84f3	78 	x 
	jr z,l8508h		;84f4	28 12 	( . 
	add a,a			;84f6	87 	. 
	add a,a			;84f7	87 	. 
	add a,a			;84f8	87 	. 
	add a,a			;84f9	87 	. 
	ld c,a			;84fa	4f 	O 
	ld b,000h		;84fb	06 00 	. . 
	push de			;84fd	d5 	. 
	ld de, DUMMY_SCORE_ENTRY_6 - 1	;84fe	End of entry #5
	ld hl, SCORE_ENTRY_5 - 1			;8501	End of entry #4
	; Moves BC entries down, to make space for the new entry.
	; The new entry will be stored in the right sorted posicion.
	lddr		;8504 (DE--) <-- (HL--), BC times
	inc hl			;8506	23 	# 
	pop de			;8507	d1 	. 
l8508h:
	push hl			;8508	e5 	. 
	push de			;8509	d5 	. 
	xor a			;850a	af 	. 
	call CLS		;850b	cd c3 00 	. . . 
	ld hl,00c28h		;850e	21 28 0c 	! ( . 
	ld de,NAME_STR		;8511	11 bd 95 	. . . 
	call PRINT_LOCATE_WHITE		;8514	cd 00 80 	. . . 
	pop de			;8517	d1 	. 
	pop hl			;8518	e1 	. 
	ld (hl),e			;8519	73 	s 
	inc hl			;851a	23 	# 
	ld (hl),d			;851b	72 	r 
	inc hl			;851c	23 	# 
	push hl			;851d	e5 	. 
	ld b,00eh		;851e	06 0e 	. . 
l8520h:
	ld (hl),000h		;8520	36 00 	6 . 
	inc hl			;8522	23 	# 
	djnz l8520h		;8523	10 fb 	. . 
	pop hl			;8525	e1 	. 
	ld b,00dh		;8526	06 0d 	. . 
	ld (hl),0ffh		;8528	36 ff 	6 . 
l852ah:
	push bc			;852a	c5 	. 
	push hl			;852b	e5 	. 
	call PRINT_SCORE_TABLE		;852c	cd 68 85 	. h . 
	ld hl,(GYPOS)	; HL = GYPOS
	ld e,l
	ld a,h			; AE = HL
	add a,a
	add a,a
	add a,a			; A = 8*A = 8*H
	ld d,a			; D = 8*H
	ld hl, SPRITE_ATTRIBUTE_TABLE		;8538	Sprite attribute table 1B00-1B7F
	; Here DE = 8*H, L where HL came from GYPOS
	call WRITE_DE_VRAM_FROM_HL_INC		;853b	cd 49 81 	. I . 
	call WAIT_KEY		;853e	cd 74 80 	. t . 
	pop hl			;8541	e1 	. 
	pop bc			;8542	c1 	. 
	cp 00dh		;8543	fe 0d 	. . 
	jr z,CLS_PRINT_SCORE_TABLE		;8545	28 1a 	( . 
	cp 008h		;8547	fe 08 	. . 
	jr nz,l8556h		;8549	20 0b 	  . 
	ld a,b			;854b	78 	x 
	cp 00dh		;854c	fe 0d 	. . 
	jr nc,l852ah		;854e	30 da 	0 . 
	dec hl			;8550	2b 	+ 
	ld (hl),0ffh		;8551	36 ff 	6 . 
	inc b			;8553	04 	. 
	jr l852ah		;8554	18 d4 	. . 
l8556h:
	cp 020h		;8556	fe 20 	.   
	jr c,l852ah		;8558	38 d0 	8 . 
	ld (hl),a			;855a	77 	w 
	ld a,0ffh		;855b	3e ff 	> . 
	inc hl			;855d	23 	# 
	ld (hl),a			;855e	77 	w 
	djnz l852ah		;855f	10 c9 	. . 


; **********************************
; * Show the record's table screen *
; **********************************
CLS_PRINT_SCORE_TABLE:
	; Clear sprites and CLS
	call CLRSPR		;8561
	xor a			;8564
	call CLS		;8565
PRINT_SCORE_TABLE:
	ld a, 15		;8568
	ld (CURSOR_ROW),a	;856a CURSOR_ROW = 15
	ld b,005h		;856d It's a TOP 5 record table
	ld hl,SCORE_TABLE	;856f HL = pointer to the score table

print_score_lines:
	ld a,COLOR_GREEN_BLACK	;8572
	ld (FORE_AND_BACK_COLORS),a		;8574	32 e9 f3 	2 . . 
	ld a, 6*8		;8577
	ld (CURSOR_OFFSET_IN_ROW),a		;8579 CURSOR_OFFSET_IN_ROW = 6*8 (column #6).
	ld e,(hl)			;857c
	inc hl				;857d
	ld d,(hl)			;857e DE = record
	inc hl				;857f
	ex de,hl			;8580 HL = record, DE = pointer
	push de				;8581
	call PRINT_NUMBER	;8582 Print the record
	ld a, ' '			;8585
	call PRINT_CHAR		;8587 Print a blank
	pop hl				;858a HL = pointer again

	ld c, 14			;858b The name at each entry is 14 bytes long
l858dh:
	ld a,(hl)			;858d Read a character ch from the name
	inc a				;858e
	jr z,l8598h			;858f If ch == 0xFF, then jump
	dec a				;8591
	jr nz,l8596h		;8592 If ch != 0 then jump (do not change it by a blank)
	ld a, ' '			;8594 ch == 0 ==> Use a blank
l8596h:
	jr l85a3h			;8596

l8598h:
	; ch == 0xFF, set GYPOS = CURSOR_OFFSET_IN_ROW and change it by a blank
	ld de,(CURSOR_OFFSET_IN_ROW)		;8598
	ld (GYPOS),de		;859c
	ld a, ' '    		;85a0
	ld (hl),a			;85a2

l85a3h:
	; ch != 0, print it
	call PRINT_CHAR		;85a3 Print a char of the name
	inc hl				;85a6
	dec c				;85a7 Update character counter for the 14 characters of the name
	jr nz,l858dh		;85a8 If not yet printed 14 characters, keep printing...

	; Done printing 14 characters of the name
	ld a,(CURSOR_ROW)		;85aa
	inc a				;85ad
	inc a				;85ae
	ld (CURSOR_ROW),a		;85af CURSOR_ROW += 2
	djnz print_score_lines			;85b2 Still lines to print? B == 0? If not, keep printing lines
	ret					;85b4





	; Some zeros here...
	defs 0x85ff - 0x85b5 + 1

	ld a,0c9h		;8600	3e c9 	> . 
	ld (H_OUTD),a		;8602	32 e4 fe 	2 . . 
	jp 0411fh		;8605	c3 1f 41 	. . A 

BICHO_CYCLE:
    ; Is it time to update Bicho?
	ld hl, BICHO_COUNTER		;8608	21 3a bf 	! : . 
	dec (hl)			;860b	35 	5 
	ret nz				;860c	c0 	.   miguel: changing this to a ret makes Bicho not able to move!!
	
	; Yes, we need to update Bicho
	ld (hl), 50		;860d	Set next Bicho delay. 
DO_BICHO_CYCLE:
	xor a						;860f
	ld (IS_SCROLLING),a			;8610 Set IS_SCROLLING = 0
	ld hl,(ABS_BICHO_X)			;8613
	ld de,(BICHO_ABS_TARGET_X)	;8616 HL=ABS_BICHO_X, DE=BICHO_ABS_TARGET_X
	exx							;861a Use Y
	ld hl,(ABS_BICHO_Y)			;861b
	ld de,(BICHO_ABS_TARGET_Y)	;861e HL'=ABS_BICHO_Y, DE'=BICHO_ABS_TARGET_Y
	rst 20h					;8622 Compare ABS_BICHO_Y with BICHO_ABS_TARGET_Y
	
	exx						;8623 Use X
	jr nz,l862ah			;8624 Jump if ABS_BICHO_Y != BICHO_ABS_TARGET_Y
	; ABS_BICHO_Y == BICHO_ABS_TARGET_Y

	rst 20h					;8626 Compare ABS_BICHO_X with BICHO_ABS_TARGET_X
	jp z,l8877h				;8627 Jump if ABS_BICHO_X == BICHO_ABS_TARGET_X.
							;     We also have ABS_BICHO_Y == BICHO_ABS_TARGET_Y, so
							; this is a "go on, nothing to do".

; ABS_BICHO_Y != BICHO_ABS_TARGET_Y
l862ah:
	; if ABS_BICHO_X > BICHO_ABS_TARGET_X:
	;     HL = ABS_BICHO_X - 1
	; elif ABS_BICHO_X < BICHO_ABS_TARGET_X:
	;     HL = ABS_BICHO_X + 1

	rst 20h			;862a Compare ABS_BICHO_X with BICHO_ABS_TARGET_X
	jr z,l8632h		;862b Leave if equal
	; ABS_BICHO_X != BICHO_ABS_TARGET_X
	inc hl			;862d HL = ABS_BICHO_X + 1
	jr c,l8632h		;862e C if HL < DE <==> ABS_BICHO_X < BICHO_ABS_TARGET_X
	; BICHO_ABS_TARGET_X <= ABS_BICHO_X + 1
	dec hl			;8630
	dec hl			;8631 HL = ABS_BICHO_X - 1, since BICHO_ABS_TARGET_X <= ABS_BICHO_X
	
	
l8632h:
	; if ABS_BICHO_Y > BICHO_ABS_TARGET_Y:
	;     HL' = ABS_BICHO_Y - 1
	; elif ABS_BICHO_Y < BICHO_ABS_TARGET_Y:
	;     HL' = ABS_BICHO_Y + 1
	rra			;8632
	ld (BICHO_STATUS1),a ;8633 Activate bit #7 if ABS_BICHO_X < BICHO_ABS_TARGET_X ==> Look right

	exx				;8636 Use Y
	rst 20h			;8637 Compare ABS_BICHO_Y with BICHO_ABS_TARGET_Y
	jr z,l863fh		;8638 Leave if equal
	inc hl			;863a HL = ABS_BICHO_Y + 1
	jr c,l863fh		;863b C if HL < DE <==> ABS_BICHO_Y < BICHO_ABS_TARGET_Y
	dec hl			;863d
	dec hl			;863e HL = ABS_BICHO_Y - 1

l863fh:
	; Update new position of bicho
	ld (ABS_BICHO_Y),hl		;863f
	exx						;8642 Use X
	ld (ABS_BICHO_X),hl		;8643

sub_8646h:
	ex de,hl			;8646
	ld hl,(SCROLL_X)	;8647 HL = SCROLL_X, DE = ABS_BICHO_X
	dec l				;864a
	dec l				;864b HL = SCROLL_X - 2

	ld h,000h			;864c
	add hl,hl			;864e
	add hl,hl			;864f
	add hl,hl			;8650 HL = (SCROLL_X - 2)*8
	rst 20h				;8651 NC if ABS_BICHO_X <= (SCROLL_X - 2)*8 (Bicho not visible, on the left of scroll X start)
	
	jp nc,sprites_8_9_10_invisible_and_ret		;8652
	ld bc,00122h		;8655
	add hl,bc			;8658 HL = (SCROLL_X - 2)*8 + 290
	rst 20h				;8659 C if ABS_BICHO_X > (SCROLL_X - 2)*8 + 290 (Bicho not visible, on the right of scroll X end)
	jp c,sprites_8_9_10_invisible_and_ret ;865a

	; Bicho is visible
	ld bc, -275			;865d
	add hl,bc			;8660 HL = (SCROLL_X - 2)*8 + 290 - 275 = (SCROLL_X - 2)*8 + 15
	exx					;8661 Use Y
	ex de,hl			;8662
	; DE = 8*(SCROLL_X - 2) + 15
	; HL = ABS_BICHO_X

	ld a,(SCROLL_Y)		;8663
	sub 004h			;8666 A = SCROLL_Y - 4
	ld l,a				;8668
	ld h,000h			;8669 HL = SCROLL_Y - 4
	add hl,hl			;866b
	add hl,hl			;866c
	add hl,hl			;866d HL = 8*(SCROLL_Y - 4)
	rst 20h				;866e NC if ABS_BICHO_Y <= 8*(SCROLL_Y - 4) (Bicho not visible, above start of scroll Y)
	jp nc,sprites_8_9_10_invisible_and_ret		;866f
	ld bc,208			;8672
	add hl,bc			;8675 HL = 8*(SCROLL_Y - 4) + 208
	rst 20h				;8676 C if ABS_BICHO_Y > 8*(SCROLL_Y - 4) + 208 (Bicho not visible, below scroll Y end)
	jp c,sprites_8_9_10_invisible_and_ret		;8677	da 62 88 	. b . 

	ld bc, -176			;867a
	add hl,bc			;867d HL = 8*(SCROLL_Y - 4) + 208 - 176 = 8*(SCROLL_Y - 4) + 32
	ex de,hl			;867e
	; DE = 8*(SCROLL_Y - 4) + 32
	; HL = ABS_BICHO_Y

	or a			;867f	b7 	. 
	sbc hl,de		;8680 HL = ABS_BICHO_Y - 8*(SCROLL_Y - 4) + 32
	ld a,l			;8682
	ex af,af'		;8683 Keep A = ABS_BICHO_Y - 8*(SCROLL_Y - 4) + 32 in your sleeve

	exx				;8684 Use X
	ex de,hl		;8685
	; DE = (SCROLL_X - 2)*8 + 15
	; HL = ABS_BICHO_X
	
	or a			;8686	Clear carry
	sbc hl,de		;8687	HL = ABS_BICHO_X - (SCROLL_X - 2)*8 + 15
	inc h			;8689	HL = ABS_BICHO_X - (SCROLL_X - 2)*8 + 271
	ld a,002h		;868a
	jr z,l86a0h		;868c   Leave if ABS_BICHO_X == (SCROLL_X - 2)*8 + 15

	dec h			;868e	HL = ABS_BICHO_X - (SCROLL_X - 2)*8 + 15
	ld de, 254		;868f   
	rst 20h			;8692	NC if HL >= DE <==> ABS_BICHO_X  >= (SCROLL_X - 2)*8 + 15 + 254
	ld a, 1			;8693
	jr nc,l86a0h	;8695	Leave if ABS_BICHO_X  >= (SCROLL_X - 2)*8 + 15 + 254
	ld de, 18		;8697	
	rst 20h			;869a	C if HL < DE <==> ABS_BICHO_X < (SCROLL_X - 2)*8 + 15 + 18
	ld a, 2			;869b
	jr c,l86a0h		;869d	Leave if ABS_BICHO_X < (SCROLL_X - 2)*8 + 15 + 18
	xor a			;869f	af 	. 
l86a0h:
	ex af,af'		;86a0
	; Recover A = ABS_BICHO_Y - 8*(SCROLL_Y - 4) + 32 for the sleeve
	;
	; In A' we have the code 0, 1, or 2
	;
	; if ABS_BICHO_X == (SCROLL_X - 2)*8 + 15:
	;     A = 2
	; elif ABS_BICHO_X >= (SCROLL_X - 2)*8 + 15 + 254:
	;     A = 1
	; elif ABS_BICHO_X < (SCROLL_X - 2)*8 + 15 + 18
	;     A = 2
	; else:
	;     A = 0
	
	ld h,a			;86a1 H = ABS_BICHO_Y - 8*(SCROLL_Y - 4) + 32
	push hl			;86a2 Push H=ABS_BICHO_Y - 8*(SCROLL_Y - 4) + 32, L=ABS_BICHO_X - 8*(SCROLL_X - 2)*8 + offset
	
	; This is executed only if Bicho in visible in the screen
	ld a,(IS_SCROLLING)		;86a3
	or a					;86a6
	jp nz,l8790h			;86a7  Leave if scrolling

	ld de,(PULGA_X)			;86aa  D=PULGA_Y, E=PULGA_X
	ld a,e					;86ae  A = PULGA_X
	cp 32					;86af
	jp c,skip_pulga_captured				;86b1  Leave if PULGA_X < 32
	cp 208					;86b4
	jp nc,skip_pulga_captured			;86b6  Leave if PULGA_X >= 208

	; Here 32 <= PULGA_X < 208
	sub 24					;86b9  A = PULGA_X - 24
	cp l					;86bb  NC if BICHO_X <= PULGA_X - 24 
	jp nc,skip_pulga_captured			;86bc  Leave if BICHO_X <= PULGA_X - 24
	;
	add a,040h				;86bf  A =  PULGA_X + 40
	cp l					;86c1  C if BICHO_X > PULGA_X + 40
	jp c,skip_pulga_captured				;86c2  Leave if BICHO_X > PULGA_X + 40

	; Here PULGA_X - 24 < BICHO_X <= PULGA_X + 40
	ld a,d					;86c5  A = PULGA_Y
	sub 36					;86c6  A = PULGA_Y - 36
	cp h					;86c8  NC if BICHO_Y <= PULGA_Y - 36
	jp nc,skip_pulga_captured			;86c9  Leave if BICHO_Y <= PULGA_Y - 36
	;
	add a,048h				;86cc  A = PULGA_Y - 36 + 72 = PULGA_Y + 36
	cp h					;86ce  C if BICHO_Y + 32 > PULGA_Y + 36
	jp c,skip_pulga_captured				;86cf  Leave if BICHO_Y > PULGA_Y + 36
	
	; Here
	; PULGA_X - 24 < BICHO_X + offset <= PULGA_X + 40
	; AND
	; PULGA_Y - 36 < BICHO_Y + 32 <= PULGA_Y + 36
	
	; It doesn't have a variable for the position of Bicho in the screen, only its absolute position.
	; Thus, It subtracts the scroll to obtain the position relative to the screen and compare with pulga's.
	; At the end, it does all this to check if Bicho should be drawn with its mouth open.

	nop			;86d2
	nop			;86d3
	nop			;86d4

	ld a,e			;86d5 A = PULGA_X
	sub 16			;86d6 A = PULGA_X - 16
	cp l			;86d8 C if BICHO_X > PULGA_X - 16
	jp c,draw_bicho_open_mouth_left		;86d9 Draw open mouth left if BICHO_X > PULGA_X - 16
	
	; ABS_BICHO_X <= PULGA_X - 16 ==> Bicho should look right
	ld hl,00080h						;86dc
	ld (BICHO_STATUS1),hl				;86df Set BICHO_STATUS1 = 80 (look right), BICHO_THREAT_STATUS = 0
	ld hl,SPRITE_BICHO_OPEN_MOUTH_ADDR	;86e2 Update sprite
	jp draw_bicho						;86e5

draw_bicho_open_mouth_left:
	add a,48		;86e8	A = PULGA_X - 16 + 48  = PULGA_X  + 32
	cp l			;86ea
	jp nc,l86fah	;86eb Leave if BICHO_X <= PULGA_X + 32, too far

	ld hl,00000h			;86ee
	ld (BICHO_STATUS1),hl	;86f1 Set BICHO_STATUS1 = 0 (look left), BICHO_THREAT_STATUS = 0
	ld hl,SPRITE_BICHO_OPEN_MOUTH_ADDR		;86f4 Update sprite
	jp draw_bicho			;86f7

l86fah:
	ld a,d			;86fa	A = PULGA_Y
	sub 28			;86fb	A = PULGA_Y - 28
	cp h			;86fd
	jp c,l8713h		;86fe	Jump if BICHO_Y > PULGA_Y - 28 (Bicho is down, can look up)

	; Here BICHO_Y <= PULGA_Y - 28
	ld a,e			;8701	A = PULGA_X
	add a, 6		;8702	A = PULGA_X + 6
	cp l			;8704
	ld hl,00180h		;8705	
	jr nc,l870dh		;8708	Jump if BICHO_X <= PULGA_X + 6 
	ld hl,00100h		;870a
l870dh:
	ld (BICHO_STATUS1),hl		;870d Set BICHO_STATUS1=L (look left or right) and BICHO_THREAT_STATUS=H (1)
	jp draw_bicho_mouth_closed	;8710
l8713h:
	; Already BICHO_Y > PULGA_Y - 28
	add a,032h		;8713	A = PULGA_Y + 22
	cp h			;8715
	; Pulga will be captured when PULGA_Y - 28 < BICHO_Y < PULGA_Y + 22
	jp nc,set_pulga_captured		;8716	Jump is BICHO_Y <= PULGA_Y + 22
	ld a,e			;8719
	add a,006h		;871a   A = PULGA_X + 6
	cp l			;871c
	ld hl,00080h		;871d Look right
	jr nc,draw_bicho_looking_up		;8720    BICHO_X <= PULGA_X + 6. Bicho is on the left of pulga. It should look right.
	ld hl,00000h		;8722	Look left

draw_bicho_looking_up:
	ld (BICHO_STATUS1),hl		;8725 Update BICHO_STATUS1 (look left or right) and BICHO_THREAT_STATUS
	ld hl,SPRITE_BICHO_LOOKING_UP_ADDR		;8728 Update sprite
	jp draw_bicho		;872b	c3 3e 87 	. > . 

set_pulga_captured:
	; PULGA_Y - 28 < BICHO_Y <= PULGA_Y + 22
	; AND
	; PULGA_X - 16 < BICHO_X <= PULGA_X  + 32
	ld a,(BICHO_THREAT_STATUS)		;872e

	set 7,a		;8731 Activate Pulga-captured (oops!) flag.
	; Removing this instructions makes Bicho unable to grab Pulba
	
	ld (BICHO_THREAT_STATUS),a		;8733

skip_pulga_captured:
	ld a,00eh		;8736	3e 0e 	> . 
	ld (sound_script1),a		;8738	32 44 8e 	2 D . 
draw_bicho_mouth_closed:
	ld hl,SPRITE_BICHO_FACE ;873b	21 d0 93 	! . . 
draw_bicho:
	ld de,07900h			;873e [ToDo] What is this address in VRAM???
	ld a,(BICHO_STATUS1)	;8741
	rla						;8744
	jr c,draw_bicho_legs_right	;8745 Jump if Bicho looks to the right
	; Bicho looks to the left
	ld bc,00020h		;8747	01 20 00 	.   . 
	call LDIRVM		;874a Update sprite
	ld hl,SPRITE_BICHO_LEGS_ADDR ;874d	21 f0 93 	! . . 
	ld de,SPRITE_CHARACTER_PATTERNS + 32*9 ;8750
	ld bc,00020h		;8753	01 20 00 	.   . 
	call LDIRVM		;8756 Update sprite
	jr choose_bicho_wings_frame		;8759
draw_bicho_legs_right:
	call FLIP_SPRITE_WITH_INIT		;875b
	ld hl,SPRITE_BICHO_LEGS_ADDR	;875e
	call FLIP_SPRITE				;8761
choose_bicho_wings_frame:
	ld a,(BICHO_FRAME)		;8764
	inc a			;8767
	and 003h		;8768
	ld (BICHO_FRAME),a
	ld hl,SPRITE_BICHO_WINGS1 ;876d
	jr z,draw_bicho_wings		;8770
	ld hl,SPRITE_BICHO_WINGS3 ;8772
	cp 002h		;8775 
	jr z,draw_bicho_wings		;8777
	ld hl,SPRITE_BICHO_WINGS2 ;8779
draw_bicho_wings:
	ld a,(BICHO_STATUS1)		;877c
	rla			;877f
	jr c,l878dh		;8780
	ld de,SPRITE_CHARACTER_PATTERNS + 32*10 ;8782
	ld bc,00020h		;8785
	call LDIRVM		;8788 Update sprite
	; Sprites #13, #15, #14 (wings of Bicho)
	jr l8790h		;878b
l878dh:
	call FLIP_SPRITE		;878d

l8790h:
	pop de			;8790 POP what it pushed in 86a2: H=BICHO_Y, L=BICHO_X
	; D=BICHO_Y, E=BICHO_X

	ld a,(BICHO_STATUS1)	;8791
	rla						;8794
	jr c,l87afh		;8795	38 Jump if bicho's looking left

	; Bicho's looking right
	ld a,d			;8797
	add a,9			;8798	A = BICHO_Y + 9
	ld d,a			;879a	D = BICHO_Y + 9
	inc e			;879b
	inc e			;879c	E = BICHO_X + 2
	push de			;879d	*** PUSH BICHO_Y + 9, BICHO_X + 2

	add a,006h		;879e	A = BICHO_Y + 15
	ld d,a			;87a0	D = BICHO_Y + 15
	ld a,e			;87a1	A = BICHO_X + 2
	sub 12			;87a2	A = BICHO_X - 10
	ld e,a			;87a4	E = BICHO_X - 10
	push de			;87a5	*** PUSH BICHO_Y + 15, BICHO_X - 10

	sub 008h		;87a6	A = BICHO_X - 18
	ld e,a			;87a8	E = BICHO_X - 18
	ld a,d			;87a9	A = BICHO_Y + 15
	sub 010h		;87aa	A = BICHO_Y - 1
	ld d,a			;87ac	D = BICHO_Y - 1
	jr l87c7h		;87ad

l87afh:
	; Bicho's looking left
	ld a,d			;87af	
	add a,9			;87b0	A = BICHO_Y + 9
	ld d,a			;87b2	D = BICHO_Y + 9
	ld a,e			;87b3	A = BICHO_X
	sub 18			;87b4	A = BICHO_X - 18
	ld e,a			;87b6	E = BICHO_X - 18
	push de			;87b7	*** PUSH BICHO_Y + 9, BICHO_X - 18 

	add a,12		;87b8	A = BICHO_X - 6
	ld e,a			;87ba	E = BICHO_X - 6
	ld a,d			;87bb	A = BICHO_Y + 9
	add a,006h		;87bc	A = BICHO_Y + 15
	ld d,a			;87be	D = BICHO_Y + 15
	push de			;87bf	*** PUSH BICHO_Y + 15, BICHO_X - 6

	sub 010h		;87c0	A = BICHO_Y - 1
	ld d,a			;87c2	D = BICHO_Y - 1
	ld a,e			;87c3	A = BICHO_X - 6
	add a,008h		;87c4	A = BICHO_X + 1
	ld e,a			;87c6	E = BICHO_X + 1
	; D = BICHO_Y - 1
	; E = BICHO_X + 1

l87c7h:
    ; Set the position of Bicho and draw him
	ld hl, SPRITE_ATTRIBUTE_TABLE + 32*1 ;87c7
	ex af,af'		;87ca A = code (0, 1, 2), see above
	or a			;87cb
	jp nz,check_case_1_or_2	;87cc Case != 0
	
	; Case 0: 
	; (SCROLL_X - 2)*8 + 15 + 18 < ABS_BICHO_X < (SCROLL_X - 2)*8 + 15 + 254	
	call WRITE_DE_VRAM_FROM_HL_INC		;87cf Set vertical and horizontal positions from D, E
	inc hl				;87d2 Skip sprite pattern
	ld a, COLOR_CYAN	;87d3
	call WRTVRM			;87d5 Set color
	inc hl				;87d8 Next position (vertical and horizontal positions of next sprite)

	pop de							;87d9
	call WRITE_DE_VRAM_FROM_HL_INC	;87da Set vertical and horizontal positions from D, E
	inc hl			 ;87dd Skip sprite pattern
	ld a, COLOR_CYAN ;87de
	call WRTVRM	 	 ;87e0	Set color
	inc hl			 ;87e3	Next position (vertical and horizontal positions of next sprite)

	pop de			;87e4
	call WRITE_DE_VRAM_FROM_HL_INC	;87e5 Set vertical and horizontal positions from D, E
	inc hl							;87e8 Skip sprite pattern
	ld a,COLOR_CYAN					;87e9
	call WRTVRM						;87eb Set color
	ret			;87ee Done!

check_case_1_or_2:
	rra				;87ef Check case 01b (1) or 10b (2).
	jr nc,case_2		;87f0	Jump if case 2
	
	; Case #1
	; ABS_BICHO_X >= (SCROLL_X - 2)*8 + 15 + 254
	; D = BICHO_Y - 1
	; E = BICHO_X + 1
	bit 7,e			;87f2 E = BICHO_X + 1 
	
	; if BICHO_X < 126:
	;     d = 192 (invisible position X)

	jr nz,l87f8h	;87f4
	ld d,192		;87f6
l87f8h:
	call WRITE_DE_VRAM_FROM_HL_INC		;87f8
	inc hl			;87fb
	ld a,COLOR_CYAN	;87fc
	call WRTVRM		;87fe
	inc hl			;8801
	pop de			;8802
	bit 7,e			;8803
	jr nz,l8809h	;8805
	ld d,192		;8807
l8809h:
	call WRITE_DE_VRAM_FROM_HL_INC		;8809
	inc hl			;880c
	ld a,COLOR_CYAN	;880d
	call WRTVRM		;880f
	inc hl			;8812
	pop de			;8813
	bit 7,e			;8814
	jr nz,l881ah	;8816
	ld d,192		;8818
l881ah:
	call WRITE_DE_VRAM_FROM_HL_INC		;881a
	inc hl			;881d
	ld a,COLOR_CYAN	;881e
	call WRTVRM		;8820
	ret				;8823

case_2:
	;ABS_BICHO_X < (SCROLL_X - 2)*8 + 15 + 18
	; D = BICHO_Y - 1
	; E = BICHO_X + 1
	ld a,e			;8824  
	add a,020h		;8825  A = BICHO_X + 33
	ld e,a			;8827  E = BICHO_X + 33
	cp 010h			;8828  Check BICHO_X >= -23
	jr nc,l882eh	;882a
	; BICHO_X < -23
	ld d,0c0h		;882c  Set Bicho invisible
l882eh:
	call WRITE_DE_VRAM_FROM_HL_INC	;882e  Write position
	inc hl							;8831  Skip sprite pattern
	ld a,COLOR_CYAN_EC				;8832  Activate EC
	call WRTVRM		;8834
	inc hl			;8837
	pop de			;8838
	ld a,e			;8839
	add a,020h		;883a
	ld e,a			;883c
	cp 010h			;883d
	jr nc,l8843h	;883f
	ld d,0c0h		;8841
l8843h:
	call WRITE_DE_VRAM_FROM_HL_INC		;8843  Write position
	inc hl								;8846  Skip sprite pattern
	ld a,COLOR_CYAN_EC					;8847  Activate EC
	call WRTVRM		;8849
	inc hl			;884c
	pop de			;884d
	ld a,e			;884e
	add a,020h		;884f
	ld e,a			;8851
	cp 010h			;8852
	jr nc,l8858h	;8854
	ld d,0c0h		;8856
l8858h:
	call WRITE_DE_VRAM_FROM_HL_INC		;8858  Write position
	inc hl								;885b  Skip sprite pattern
	ld a,COLOR_CYAN_EC					;885c  Activate EC
	call WRTVRM							;885e
	ret									;8861

sprites_8_9_10_invisible_and_ret:
	; Set vertical position of sprite #8, #9, and #10 to 191.
	; Since there are 192 lines (from 0 to 191), this will make these
	; sprites disappear.
	ld a, 192		;8862
	ld hl, SPRITE_ATTRIBUTE_TABLE + 4*8 ;8864 Sprite #8
	call WRTVRM		;8867
	ld hl, SPRITE_ATTRIBUTE_TABLE + 4*9	;886a Sprite #9
	call WRTVRM		;886d	cd 4d 00 	. M . 
	ld hl, SPRITE_ATTRIBUTE_TABLE + 4*10 ;8870 Sprite #10
	call WRTVRM		;8873
	ret			;8876	c9 	. 

; Edited code? There was an extra condition than could end the game?
; ABS_BICHO_X == BICHO_ABS_TARGET_X
l8877h:
	jp l887eh		;8877
	rla				;887a
	jp c,END_GAME	;887b
l887eh:
	call UPDATE_ABS_PULGA_POS		;887e
	ld bc,(PELICAN_POS_PTR)			;8881  BC = pointer to TY (second coordinate)

	push bc			;8885 Save the pointer of TY in the current block
	call GET_NEXT_BICHO_TARGET_IN_CHAIN		;8886
	call L1_DIST_TARGET_PULGA				;8889

	push hl			;888c Save HL = abs(BICHO_ABS_TARGET_Y - ABS_PULGA_Y) + abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)
	call GET_NEXT_BICHO_TARGET_IN_CHAIN		;888d
	call L1_DIST_TARGET_PULGA				;8890

	push hl			;8893 Save HL = abs(BICHO_ABS_TARGET_Y - ABS_PULGA_Y) + abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)
	call GET_NEXT_BICHO_TARGET_IN_CHAIN		;8894	cd 95 8c 	. . . 
	call L1_DIST_TARGET_PULGA				;8897

	push hl			;889a Save HL = abs(BICHO_ABS_TARGET_Y - ABS_PULGA_Y) + abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)
	call GET_NEXT_BICHO_TARGET_IN_CHAIN		;889b
	call L1_DIST_TARGET_PULGA				;889e

	; DE = abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)
	; HL = abs(BICHO_ABS_TARGET_Y - ABS_PULGA_Y) + abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)

	ex de,hl		;88a1
	; DE = abs(BICHO_ABS_TARGET_Y - ABS_PULGA_Y) + abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)

	ld c,3			;88a2
	pop hl			;88a4  Recover one of the L1 distances
	rst 20h			;88a5  Compare with reference DE.
					;	   C ==> DE > HL
	jr nc,l88abh	;88a6  Jump if DE <= HL
	; DE > HL
	ex de,hl		;88a8  HL contains the bigger
	ld c,002h		;88a9
l88abh:
	pop hl			;88ab
	rst 20h			;88ac
	jr nc,l88b2h	;88ad
	ex de,hl		;88af
	ld c,001h		;88b0
l88b2h:
	pop hl			;88b2
	rst 20h			;88b3
	jr nc,l88b8h	;88b4
	ld c,000h		;88b6
l88b8h:
	pop hl			;88b8  Recover HL = pointer to TY in the block
	ld b,000h		;88b9
	ld a,r			;88bb  A = random
	rra				;88bd
	jr nc,l88c3h	;88be  With probability 1/2 randomize the case
	and 003h		;88c0
	ld c,a			;88c2  Choose a random case in [0, 1, 2, 3].

l88c3h:
	add hl,bc		;88c3
	; HL = pointer to TY + case
	
	ld b,h			;88c4
	ld c,l			;88c5 BC = pointer to TY + case

	; Pick record #k
	call GET_NEXT_BICHO_TARGET_IN_CHAIN		;88c6
	
	; Update target of Bicho
	ld (BICHO_ABS_TARGET_X),de				;88c9
	ld (BICHO_ABS_TARGET_Y),hl				;88cd
	ret										;88d0

; **************************************************************
; * Plant cycle                                                *
; * Check if the plant timeout is due. If so, update animation *
; * frame and with a small probability make plant's sound      *
; **************************************************************
PLANT_CYCLE:
	ld hl,PLANT_TIMEOUT	;88d1
	dec (hl)		;88d4
	ret nz			;88d5
	ld (hl),120		;88d6
	ld a,(PLANT_FRAME)	;88d8
	or a			;88db	b7 	. 
	jr nz,DO_ANIM_PLANT_FRAME		;88dc
	ld a,r		;88de
	cp 4		;88e0
	ret nc		;88e2
	; Prob 4/256 = 1/64 of making a sound
DO_PLANT_SOUND_SEMIOPEN:
	ld a,004h		;88e3
	ld (PLANT_FRAME),a		;88e5
	jr l8902h		;88e8


; ************************************************************
; * Set the current plant frame and update the sprite.       *
; * Make a random noise                                      *
; * Input A: current plant frame                             *
; ************************************************************
DO_ANIM_PLANT_FRAME:
	inc a							;88ea
	and 003h						;88eb
	ld (PLANT_FRAME),a				;88ed
	jr nz,l88fch					;88f0
	ld hl,SPRITE_PLANT_OPEN_ADDR	;88f2
	ld a,001h						;88f5
	ld (RESET_PSG_FLAG),a			;88f7
	jr l8905h						;88fa
l88fch:
	dec a							 ;88fc
	ld hl,SPRITE_PLANT_CLOSED_ADDR   ;88fd
	jr z,08905h						 ;8900
l8902h:
	ld hl,SPRITE_PLANT_SEMIOPEN_ADDR ;8902
l8905h:
	call PLAY_RANDOM_SOUND	;8905
	ld de,03880h			;8908
	ld bc,00020h			;890b
	call LDIRVM				;890e Update sprite
	ret						;8911


; Input HL: timeout
GAME_MOVE_CYCLE:
	; If not time to check yet, leave
	ld hl,TIMEOUT		;8912
	dec (hl)			;8915
	ret nz				;8916

	; Bicho eating pulga?
	ld a,(BICHO_THREAT_STATUS)	;8917
	rla							;891a
	jr nc,l894dh				;891b No, jump

	; Indeed, bicho is eating pulga :S
	ld (hl),60		;891d Set TIMEOUT=60
	ld b,a			;891f B=2
	ld hl, SPRITE_ATTRIBUTE_TABLE + 4*8 ;8920 Read vertical position of sprite #8
	call RDVRM		;8923	cd 4a 00 	. J . 
	bit 1,b			;8926	Bit #1 of B activate? <==> Bit #0 of BICHO_THREAT_STATUS active?
	jr nz,l8930h	;8928	20 06 	  . 
	sub 006h		;892a	d6 06 	. . 
	ld b,00ch		;892c	06 0c 	. . 
	jr l8934h		;892e	18 04 	. . 
l8930h:
	add a,012h		;8930	c6 12 	. . 
	ld b,003h		;8932	06 03 	. . 
l8934h:
	ld d,a			;8934	57 	W 
	ld a,(BICHO_STATUS1)		;8935	3a 3b bf 	: ; . 
	rla			;8938	17 	. 
	in a,(VDP_VRAM_PORT)		;8939	db 98 	. . 
	jr c,l8942h		;893b	38 05 	8 . 
	sub b			;893d	90 	. 
	ld b,080h		;893e	06 80 	. . 
	jr l8945h		;8940	18 03 	. . 
l8942h:
	add a,b			;8942	80 	. 
	ld b,000h		;8943	06 00 	. . 
l8945h:
	ld e,a			;8945	5f 	_ 
	ld a,b			;8946	78 	x 
	ld (PULGA_ACTION),a		;8947	32 30 bf 	2 0 . 
	jp l8da0h		;894a	c3 a0 8d 	. . . 

l894dh:
	ld a,(PULGA_ACTION)		;894d	3a 30 bf 	: 0 . 
	rra			;8950	1f 	. 
	jp c,l8a4eh		;8951	da 4e 8a 	. N . 
	ld a,001h		;8954	3e 01 	> . 
l8956h:
	call GTTRIG		;8956	cd d8 00 	. . . 
	or a			;8959	b7 	. 
	jp z,l89f5h		;895a	ca f5 89 	. . . 
	ld a,001h		;895d	3e 01 	> . 
	ld (TIMEOUT),a		;895f	32 32 bf 	2 2 . 
	jr l8964h		;8962	18 00 	. . 
l8964h:
	; Leave if we are looking too far away.
	; Actually, remove the RETs to be able to look around freely! :D
	; 	16 < PULGA_X <=  144
	; 	24 < PULGA_Y <=  224
	ld hl,(PULGA_X); L=PULGA_X, H=PULGA_Y
	ld a,h
	cp 16
	ret c		; 896a   ret if PULGA_Y < 16
	cp 144
	ret nc		; ret if PULGA_Y >= 144
	ld a,l
	cp 24
	ret c		; ret if PULGA_X < 24
	cp 224
	ret nc		; ret if PULGA_X >= 224

	; ret if space bar (or joystick button) pressed
	ld a,001h		; read joystick #1
	call GTSTCK
	or a
	ret z			; ret if no data
	
	; A=7: left
	; A=3: right
	; A=1: up
	; A=5: down

	; Let Pulga look around
	ld bc,DO_SCROLL		;897c 
	push bc				;897f Force a DO_SCROLL after calling the
							; PULGA_SCROLL below and returning to the caller
	; CALLER --> l8912 --> PULGA_SCROLL
	; PULGA_SCROLL --> DO_SCROLL --> CALLER
	ld de,(SCROLL_X)		;8980 E=SCROLL_X, D=SCROLL_Y
	sub 003h		;8984	d6 03 	. . 
	jp c,PULGA_SCROLL_UP		;8986 A=1 (up)
	jp z,PULGA_SCROLL_RIGHT		;8989 A=3 (right)
	sub 004h
	jp c,PULGA_SCROLL_DOWN		;898e A=5 (down)
	jp z,PULGA_SCROLL_LEFT		;8991 A=7 (left)

PULGA_SCROLL_UP:
	; E=SCROLL_X, D=SCROLL_Y
	; L=PULGA_X, H=PULGA_Y
	ld a,d			;8994 A = SCROLL_Y
	or a			;8995
	ret z			;8996 Leave if SCROLL_Y == 0
	dec d			;8997 D = SCROLL_Y - 1
	ld a,(PULGA_Y)	;8998
	add a,008h		;899b A = PULGA_Y + 8
	ld (PULGA_Y),a	;899d PULGA_Y += 8
	inc l			;89a0 L = PULGA_X + 1
	ret				;89a1

PULGA_SCROLL_DOWN:
	; E=SCROLL_X, D=SCROLL_Y
	; L=PULGA_X, H=PULGA_Y
	ld a,d			;89a2 A = SCROLL_Y
	cp 106			;89a3
	ret z			;89a5 Leave if SCROLL_Y == 106
	inc d			;89a6 D = SCROLL_Y + 1
	ld a,(PULGA_Y)	;89a7
	sub 008h		;89aa
	ld (PULGA_Y),a	;89ac PULGA_Y -= 8
	inc l			;89af L = PULGA_X + 1
	ret				;89b0

; Adding a RET here prevents the scroll when pulga reaches the left.
; Pulga will appear on the right.
PULGA_SCROLL_LEFT:
	; E=SCROLL_X, D=SCROLL_Y
	; L=PULGA_X, H=PULGA_Y
	ld a,e			;89b1 A = SCROLL_X
	or a			;89b2
	ret z			;89b3 Leave if SCROLL_X == 0
	dec e			;89b4 E = SCROLL_X - 1
	ld a,(PULGA_X)	;89b5
	add a,008h		;89b8
	ld (PULGA_X),a	;89ba PULGA_X += 8
	inc l			;89bd L = PULGA_X + 1
	ret				;89be

; Decrements PULGA_X in 8 steps if e != 96. INC E, INC L
; Adding a RET here prevents the scroll when pulga reaches the right.
; Pulga will appear on the left.
PULGA_SCROLL_RIGHT:
	; E=SCROLL_X, D=SCROLL_Y
	; L=PULGA_X, H=PULGA_Y
	ld a,e			;89bf A = SCROLL_X
	cp 96			;89c0
	ret z			;89c2 Leave if SCROLL_X == 96
	inc e			;89c3 E = SCROLL_X + 1
	ld a,(PULGA_X)	;89c4
	sub 008h		;89c7
	ld (PULGA_X),a	;89c9 PULGA_X -= 8
	inc l			;89cc L = PULGA_X + 1
	ret			;89cd	c9 	. 

DO_SCROLL:
	call WRITE_SCENARIO			;89ce
	ld de,(PULGA_X)				;89d1

	; Update position of sprite #20
	ld hl, SPRITE_ATTRIBUTE_TABLE + 20*4	;89d5 pulga face sprite (#20)
	call WRITE_DE_VRAM_FROM_HL_INC			;89d8
	ld a,d									;89db A = D = PULGA_Y
	add a,008h								;89dc A = PULGA_Y + 8
	ld d,a									;89de D = PULGA_Y + 8

	; Update position of sprite #6
	ld hl,SPRITE_ATTRIBUTE_TABLE + 6*4 		;89df
	call WRITE_DE_VRAM_FROM_HL_INC			;89e2

	ld a,001h								;89e5
	ld (IS_SCROLLING),a						;89e7

	ld hl,(ABS_BICHO_Y)		;89ea	2a 40 bf 	* @ . 
	exx			;89ed	d9 	. 
	ld hl,(ABS_BICHO_X)		;89ee	2a 3e bf 	* > . 
	call sub_8646h		;89f1	cd 46 86 	. F . 
	ret			;89f4	c9 	. 




l89f5h:
	ld a,03ch		;89f5	3e 3c 	> < 
	ld (TIMEOUT),a		;89f7	32 32 bf 	2 2 . 
	ld a,001h		;89fa	3e 01 	> . 
l89fch:
	call GTSTCK		;89fc	cd d5 00 	. . . 
	or a			;89ff	b7 	. 
	jr z,l8a0dh		;8a00	28 0b 	( . 
	cp 003h		;8a02	fe 03 	. . 
	jp z,jump_right		;8a04	ca 35 8a 	. 5 . 
	cp 007h		;8a07	fe 07 	. . 
	jp z,jump_left		;8a09	ca 3c 8a 	. < . 
	ret			;8a0c	c9 	. 
l8a0dh:
	ld a,(PULGA_ACTION)		;8a0d
	or a			;8a10
	jr z,l8a2ch		;8a11 Leave if pulga is idle

	ex af,af'			;8a13
	ld a,(JUMP_LENGTH)		;8a14
	ld b,a			;8a17 
	add a,a			;8a18
	add a,b			;8a19
	ld (FLIGHT),a	;8a1a FLIGHT =  4 * JUMP_LENGTH

	ld a,0f0h		;8a1d
	ld (jump_sound_pitch),a	;8a1f
	ex af,af'		;8a22 A = PULGA_ACTION

	set 0,a			;8a23 
	ld (PULGA_ACTION),a	;8a25 Set bit #0 in PULGA_ACTION: flying
	rra				;8a28
	jp l8a4eh		;8a29
l8a2ch:
	call STOP_JUMP_ERASE_BAR		;8a2c	cd d9 8b 	. . . 
	ld hl,SPRITE_PULGA_FACE1_ADDR		;8a2f	21 d0 92 	! . . 
	jp l8a47h		;8a32	c3 47 8a 	. G . 

jump_right:
	call INCREMENT_AND_DRAW_JUMP		;8a35
	ld a,040h		;8a38 Oriented right, preparing jump
	jr l8a41h		;8a3a	18 05 	. . 

jump_left:
	call INCREMENT_AND_DRAW_JUMP		;8a3c	cd bc 8b 	. . . 
	ld a,0c0h		;8a3f Oriented left, preparing jump

l8a41h:
	ld (PULGA_ACTION),a		;8a41	32 30 bf 	2 0 . 
	ld hl,SPRITE_PULGA_JUMPING_FACE1_ADDR		;8a44	21 10 93 	! . . 
l8a47h:
	ld de,(PULGA_X)		;8a47 E=PULGA_X, D=PULGA_Y
	jp check_pulga_hitting_plant		;8a4b

l8a4eh:
	; A = PULGA_ACTION rotated right
	ld (hl),014h	;8a4e
	push af			;8a50
	call PLAY_JUMP	;8a51
	pop af			;8a54
	ld de,(PULGA_X)	;8a55  E=PULGA_X, D=PULGA_Y
	rra				;8a59
	jp c,l8a94h		;8a5a  Jump if going parabola down
	rra				;8a5d
	jp c,l8b02h		;8a5e  Jump if sliding
	
	; Not parabola down, not sliding. Flying up.
	ld a,(FLIGHT)	;8a61
	or a			;8a64
	jr z,iteration_parabola_down2		;8a65 Leave if flying up over. Keep doing parabola down.

	; Decrease D = PULGA_Y (depth) ==> Fly up
	dec a			;8a67
	ld (FLIGHT),a	;8a68
	dec d			;8a6b

	call GET_NEXT_PULGA_HORIZONTAL		;8a6c

	; Choose position to the left or to the right of Pulga to check for obtacles
	ld hl,0070dh		;8a6f
	ld a,(PULGA_ACTION)	;8a72
	rla					;8a75 Check Pulga looking left
	jr nc,l8a7bh		;8a76
	ld hl,00702h		;8a78
l8a7bh:
	add hl,de					;8a7b
	call GET_VRAM_TILE_POS_H_L	;8a7c
	cp 028h						;8a7f
	jp nc,set_action_pulga_fall	;8a81
	; Not hitting any block. Check if hitting the plat and leave.
	ld hl,SPRITE_PULGA_JUMPING_FACE2_ADDR	;8a84
	jp check_pulga_hitting_plant			;8a87

iteration_parabola_down2:
	ld a, 47			;8a8a
	ld (FLIGHT),a		;8a8c FLIGHT = 47
	ld hl,PULGA_ACTION	;8a8f
	set 1,(hl)			;8a92 Activate PULGA_ACTION bit #1 (jump is going down)
l8a94h:
	ld a,(FLIGHT)	;8a94
	or a			;8a97
	jr z,set_action_pulga_fall		;8a98 If finished flying, leave
	dec a			;8a9a
	ld (FLIGHT),a	;8a9b DEC FLIGHT

	ld c,a			;8a9e
	ld b,000h		;8a9f ; BC = FLIGHT

	; Here D = PULGA_Y
	; Adjust Y coordinate of Pulga to have a parabolic descent
	ld hl,PARABOLA_TABLE	;8aa1
	add hl,bc		;8aa4  HL = PARABOLA_TABLE + FLIGHT
	ld a,(hl)		;8aa5  A = [HL] = [PARABOLA_TABLE + FLIGHT]
	add a,d			;8aa6  A = [PARABOLA_TABLE + FLIGHT] + PULGA_Y
	ld d,a			;8aa7  Update current Y of Pulga, D = [PARABOLA_TABLE + FLIGHT] + PULGA_Y
	call GET_NEXT_PULGA_HORIZONTAL		;8aa8
	ld a,(hl)			;8aab A = [PARABOLA_TABLE + FLIGHT]
	or a				;8aac Adding any vertical shift?
	ld a,(PULGA_ACTION)	;8aad 
	jr nz,jumping_with_vertical_shift		;8ab0 Jump if we're adding any vertical shift

	; We're not adding any vertical shift
	ld hl,00e0dh		;8ab2
	rla					;8ab5 Is Pulga looking to the left (bit #7)?
	jr nc,l8abbh		;8ab6 Jump if pulga is looking to the right

	; Pulga looking to the left, without any vertical shift added
	ld hl,00e02h		;8ab8 Looking to the left
l8abbh:
	; Looking to the left
	add hl,de			;8abb
	; H = [PARABOLA_TABLE + FLIGHT] + PULGA_Y + 14
	;
	; If looking to the left:
	; 	  L = PULGA_X + 2
	; else:
	; 	  L = PULGA_X + 13
	
	call GET_VRAM_TILE_POS_H_L	;8abc
	cp 028h						;8abf
	jp nc,set_action_pulga_fall	;8ac1 Jump if tile >= 28 (touched something)
	; No, we didn't touch anything, leave
	jp draw_pulga_and_check_scroll		;8ac4

jumping_with_vertical_shift:
	; a = PULGA_ACTION
	jp p,iteration_parabola_down	;8ac7  Jump to end of flying up. Start to parabolic descent
							; Bit #7: oriented left
							; Bit #6: preparing jump
							; ...
							; Bit #2: actually sliding (left:0x05 or right:0x85)
							; Bit #1: jump is going down	
	
	ld hl,0090dh		;8aca
	rla					;8acd  C = Pulga looking to the left
	jr nc,jumping	;8ace  Jumping to the right
	; Pulga is jumping to the left
	ld hl,00902h		;8ad0

jumping:
	add hl,de			;8ad3
	; D = PULGA_Y
	; E = PULGA_X
	
	; H = PULGA_Y + 9

	; if looking to the left:
	;     L = PULGA_X + 2
	; else:
	;     L = PULGA_X + 13

	call GET_VRAM_TILE_POS_H_L				;8ad4
	cp 028h									;8ad7
	jp nc,set_action_pulga_fall				;8ad9 tile >= 0x28 ==> Touched something
	; Didn't touch anything, leave
	ld hl,SPRITE_PULGA_JUMPING_FACE2_ADDR	;8adc
	jp check_pulga_hitting_plant								;8adf

iteration_parabola_down:
	call CHECK_PULGA_BLOCK_UPDATE_ACTION	;8ae2
	ld hl,0120dh							;8ae5
	rla										;8ae8 Check if Pulga's looking to the left
	jr nc,l8aeeh							;8ae9
	ld hl,01201h							;8aeb Looking to the left
l8aeeh:
	add hl,de			;8aee
	
	; H = 0x12 + D = 0x12 + PULGA_Y = 18 + PULGA_Y
	; L = PULGA_Y + (1 if looking left else 13)
	call GET_VRAM_TILE_POS_H_L		;8aef
	cp 028h							;8af2
	jp nc,set_action_pulga_fall		;8af4 If touched something while parabola down, just fall down
	ld hl,SPRITE_PULGA_FACE1_ADDR		;8af7	21 d0 92 	! . . 
	jp check_pulga_hitting_plant		;8afa	c3 0a 8b 	. . . 

set_action_pulga_fall:
	; Pulga touched something with the right left while falling
	ld a,005h			;8afd
	ld (PULGA_ACTION),a	;8aff PULGA_ACTION = 5 (fall)
l8b02h:
	inc d			;8b02
	inc d			;8b03
	call CHECK_PULGA_BLOCK_UPDATE_ACTION	;8b04
	ld hl,SPRITE_PULGA_FACE1_ADDR			;8b07
check_pulga_hitting_plant:
	push hl			;8b0a	e5 	. 
	call CHECK_PULGA_HITS_PLANT		;8b0b	cd f6 8c 	. . . 
	pop hl			;8b0e	e1 	. 
l8b0fh:
	push de			;8b0f	d5 	. 
	ld de,07a80h		;8b10	11 80 7a 	. . z 
	ld a,(PULGA_ACTION)		;8b13	3a 30 bf 	: 0 . 
	rla			;8b16 Check pulga's orientation
	jr nc,l8b24h		;8b17 Jump if oriented right
	; Pulga oriented left
	call FLIP_SPRITE_WITH_INIT		;8b19	cd e8 8b 	. . . 
	ld de,078c0h		;8b1c	11 c0 78 	. . x 
	call FLIP_SPRITE_WITH_INIT		;8b1f	cd e8 8b 	. . . 
	jr l8b36h		;8b22	18 12 	. . 
l8b24h:
	ld bc, 4*8		;8b24 Four blocks of 8 bytes each, sprites 16x16
	push hl			;8b27
	call LDIRVM		;8b28
	pop hl			;8b2b
	ld de,078c0h	;8b2c
	ld bc, 4*8		;8b2f Four blocks of 8 bytes each, sprites 16x16
	add hl,bc		;8b32
	call LDIRVM		;8b33
l8b36h:
	pop de			;8b36	d1 	. 

draw_pulga_and_check_scroll:
	ld hl, SPRITE_ATTRIBUTE_TABLE + 4*20 ;8b37	Update position of pulga face sprite (sprite #20)
	ld (PULGA_X),de		;8b3a
	call WRITE_DE_VRAM_FROM_HL_INC		;8b3e
	ld a,d			;8b41
	add a,008h		;8b42
	ld d,a			;8b44				; D = A + 8 = D + 8 = PULGA_X + 8
	ld hl,SPRITE_ATTRIBUTE_TABLE + 4*6	; Pulga body sprite (sprite #6)
	call WRITE_DE_VRAM_FROM_HL_INC		;8b48 Update position of pulga body

	; Check if pulga coordinates (PULGA_X, PULGA_Y) are within the
	; range [18, 144] (X) and [18, 216] (Y). If so, no need to scroll.
	ld b,001h		;8b4b	B=1 means PULGA_X out of range
	ld a,d			;8b4d
	cp 24			;8b4e	PULGA_X < 18?
	jr c, scroll_needed		;8b50
	cp 144			;8b52	PULGA_X > 144?
	jr nc, scroll_needed	;8b54
	ld b,000h		;8b56   B=0 means PULGA_Y out of range
	ld a,e			;8b58
	cp 018h			;8b59	PULGA_Y < 18?
	jr c, scroll_needed		;8b5b
	cp 216			;8b5d	PULGA_Y > 216?
	jr nc, scroll_needed	;8b5f
	ret				;8b61 PULGA_X and PULGA_Y in range [18, 144] and [18, 216]. Don't scroll.

; PULGA_X or PULGA_Y not in range. Scroll needed.
scroll_needed:
	dec b			;8b62
	jr nz,l8b75h	;8b63 Jump if PULGA_Y out of range

	; PULGA_X is out of range
	ld a,d			;8b65	A = PULGA_X	
	srl a			;8b66	A =/ 2
	srl a			;8b68	A =/ 2
	srl a			;8b6a	A =/ 2; A = PULGA_X / 8
	sub 00fh		;8b6c	A = PULGA_X/8 - 15
	jp m,l8b73h		;8b6e	A == PULGA_X/8 - 15 < 0?; Have we subtracted too much?
	add a,00ah		;8b71		if so, A = PULGA_X/8 - 15 + 10 = PULGA_X/8 - 5. Subtract less.
l8b73h:
	jr l8b7eh		;8b73

	; PULGA_Y is out of range
l8b75h:
	ld a,d			;8b75	A = PULGA_X	
	srl a			;8b76	A =/ 2
	srl a			;8b78	A =/ 2
	srl a			;8b7a	A =/ 2; A = PULGA_X / 8
	sub 010h		;8b7c	A = PULGA_X/8 - 16
l8b7eh:
	ld b,a			;8b7e	B = PULGA_X/8 - 16
	ld a,e			;8b7f	A = PULGA_Y
	srl a			;8b80	A /= 2
	srl a			;8b82	A /= 2
	srl a			;8b84	A /= 2
	sub 00fh		;8b86	A = PULGA_Y/8 - 15
	ld c,a			;8b88	C = PULGA_Y/8 - 15
l8b89h:
	ld l,001h		;8b89
	ld de,(SCROLL_X)	;8b8b D = SCROLL_Y, E = SCROLL_X
	ld a,b			;8b8f 	   A = PULGA_X/8 - 16
	or a			;8b90	   PULGA_X/8 vs 16?
	jr z,l8ba0h		;8b91	       Jump if PULGA_X/8 == 16
	jp m,l8b9ch		;8b93	       Jump if PULGA_X/8 < 16
	
	;Here PULGA_X/8 > 16
	call PULGA_SCROLL_DOWN		;8b96
	dec b			;8b99	05 	. 
	jr l8ba0h		;8b9a	18 04 	. . 
l8b9ch:
	call PULGA_SCROLL_UP		;8b9c	cd 94 89 	. . . 
	inc b			;8b9f	04 	. 
l8ba0h:
	ld a,c			;8ba0	79 	y 
	or a			;8ba1	b7 	. 
	jr z,l8bb1h		;8ba2	28 0d 	( . 
	jp m,l8badh		;8ba4	fa ad 8b 	. . . 
	call PULGA_SCROLL_RIGHT		;8ba7	cd bf 89 	. . . 
	dec c			;8baa	0d 	. 
	jr l8bb1h		;8bab	18 04 	. . 
l8badh:
	call PULGA_SCROLL_LEFT		;8bad	cd b1 89 	. . . 
	inc c			;8bb0	0c 	. 
l8bb1h:
	push bc			;8bb1	c5 	. 
	dec l			;8bb2	2d 	- 
	call nz, DO_SCROLL		;8bb3	c4 ce 89 	. . . 
	pop bc			;8bb6	c1 	. 
	ld a,b			;8bb7	78 	x 
	or c			;8bb8	b1 	. 
	jr nz,l8b89h		;8bb9	20 ce 	  . 
	ret			;8bbb	c9 	. 
	
	
; ************************************************************
; * Increment JUMP_LENGTH. Wrap to zero is JUMP_LENGTH >= 48 *
; * Draw bar accordingly                                     *
; * It uses two characters in the bar:                       *
; * 0x15 for just a piece (say, then end of the bar), and    *
; * 0x16 (it fill all the caracters).                        *
; ************************************************************
INCREMENT_AND_DRAW_JUMP:
	ld a, (JUMP_LENGTH)				;8bbc
	inc a							;8bbf	A = JUMP_LENGTH + 1
	cp 48							;8bc0	Is JUMP_LENGTH >= 48?
	jr nc,STOP_JUMP_ERASE_BAR		; If JUMP_LENGTH >= 48 then JUMP_LENGTH = 0
	ld (JUMP_LENGTH), a				;8bc4 Store updated JUMP_LENGTH += 1
	srl a							;8bc7	A = JUMP_LENGTH / 2
	ld b,015h						;8bc9
	jr nc,l8bceh					;8bcb	B=0x15 if JUMP_LENGTH / 2 is pair, then B=0x16
	inc b							;8bcd   B=0x16, complete "__". 0x15 is just one "_".
l8bceh:
	ld e,a							;8bce
	ld d,000h						;8bcf DE = JUMP_LENGTH / 2
	ld hl, NAME_TABLE + 22*32 + 2 	; Row 22, column 2
	add hl,de						;8bd4 HL = NAME_TABLE + 22*32+2 + JUMP_LENGTH/2
	ld a,b							;8bd5
	jp WRTVRM						;8bd6 Write value 0x15 or 0x16 to the piece of jump bar


; ***********************************************************
; * Sets JUMP_LENGTH = 0 and erases the 24 positions of the *
; * the the jump bar.                                       *
; ***********************************************************
STOP_JUMP_ERASE_BAR:
	xor a							;8bd9
	ld (JUMP_LENGTH),a				;8bda
	ld hl, NAME_TABLE + 22*32 + 2 	; Row 22, column 2
	ld bc, 24						;8be0
	ld a, ' '						;8be3
	jp FILVRM						;8be5


; ********************************************************************
; * Flip sprite in VRAM.                                             *
; * This routine also initializes the VDP to write at the given      *
; * address and sets the port to VDP_VRAM_PORT.                      *
; * Input DL: sprite data in RAM                                     *
; * Input DE: VRAM address of the character pattern of the sprite    *
; ********************************************************************
FLIP_SPRITE_WITH_INIT:
	; Prepare to read from DE to write to VDP's address HL
	ex de,hl			;8be8
	call SETWRT			;8be9
	di					;8bec
	ex de,hl			;8bed
	ld c, VDP_VRAM_PORT	;8bee

; ********************************************************************
; * Flip sprite in VRAM, after calling FLIP_SPRITE_WITH_INIT first.  *
; * Input DL: sprite data in RAM                                     *
; * Input DE: VRAM address of the character pattern of the sprite    *
; ********************************************************************
FLIP_SPRITE:
	push hl				;8bf0
	
	; It will copy first the blocks on the right of the 16x16 sprite.
	; Indeed, it can be seen as a
	; 8 8
	; 8 8
	; block, so it'll first do the right part than actually will
	; appear on the left, and then it'll do the same with the
	; blocks on the left, than will appear on the right.
	ld de, 16			;8bf1
	add hl,de			;8bf4

; Process blocks on the right

; Flip the columns of the following 8 rows
l8bf5h:
	ld a,(hl)		;8bf5
	inc hl			;8bf6
	ld b, 8 		;8bf7 8 rows

; RLCA:     C-->76543210-->C
; RR:       C<--76543210<--C
; Flip row and write to VRAM flipped row
l8bf9h:
	rlca			;8bf9
	rr d			;8bfa
	djnz l8bf9h		;8bfc ; Keep flipping columns

	out (c),d		;8bfe
	dec e			;8c00 All 16 rows done?
	jr nz,l8bf5h	;8c01
	
	; Repeat the same for the blocks on the left
	ex (sp),hl		;8c03 Recover the original HL, without adding 16
	ld e, 16		;8c04
l8c06h:
	ld a,(hl)		;8c06
	inc hl			;8c07
	ld b, 8			;8c08

l8c0ah:
	rlca			;8c0a
	rr d			;8c0b
	djnz l8c0ah		;8c0d

	out (c),d		;8c0f
	dec e			;8c11
	jr nz,l8c06h	;8c12
	pop hl			;8c14
	ei				;8c15
	ret				;8c16

; **********************************************
; * Get the next horizontal position of pulga. *
; * This is used during flight.                *
; * Input E: PULGA_X                           *
; **********************************************
GET_NEXT_PULGA_HORIZONTAL:
	ld a,(PULGA_ACTION)		;8c17
	rla						;8c1a Check pulga's orientation
	jr c,l8c1fh				;8c1b 
	; Looking right ==> Move right
	inc e					;8c1d
	ret						;8c1e
l8c1fh:
	; Looking left ==> move left
	dec e					;8c1f
	ret						;8c20



; ******************************************
; * Checks the block pulga hits. Then,     *
; * update Pulga's action, and play sounds *
; * Input E=PULGA_X
; * Input D=PULGA_Y
; ******************************************
CHECK_PULGA_BLOCK_UPDATE_ACTION:
	call CHECK_PLANT_EATS_PULGA		;8c21 If plant would eat Pulga, animation and end game

	ld hl,014feh		;8c24	H=20, L=-2
	add hl,de			;8c27	H=PULGA_Y + 20, L=PULGA_X - 2
	call GET_VRAM_TILE_POS_H_L		;8c28 
	ld b,a							;8c2b B = tile1
	in a,(VDP_VRAM_PORT)			;8c2c (in the middle, discarded)
	
	; A delay?
	ex (sp),hl						;8c2e
	ex (sp),hl						;8c2f

	in a,(VDP_VRAM_PORT)		;8c30 A = tile2

	; tile1: left leg
	; tile2: right leg
	; So amazing that this is taken into acount... :)

	cp 028h						;8c32
	jr nc,l8c54h				;8c34 If tile2 >= 0x28, keep checking
	ld a,b						;8c36 A = tile1
	cp 028h						;8c37
	jr nc,l8c40h				;8c39 If tile1 >= 0x28, keep checking
	xor a						;8c3b
	ld (TIMEOUT+1),a			;8c3c
	ret							;8c3f
;
l8c40h:
	pop hl			;8c40	e1 	. 
set_sliding_fall_right:
	; left leg:  touching something
	; right leg: free
	ld a, 5				;8c41
	ld (PULGA_ACTION),a	;8c43	PULGA_ACTION = 5 (falling sliding right)
	ld de,(PULGA_X)		;8c46	E=PULGA_X, D=PULGA_Y
	inc e				;8c4a	E=PULGA_X + 1 (fall to the right)
	ld hl,SPRITE_PULGA_FACE2_ADDR	;8c4b
	call PLAY_PULGA_SLIDING			;8c4e
	jp l8b0fh		;8c51
;
l8c54h:
	; right leg: touching something
	; left leg: check here if also touching something
	pop hl			;8c54
	ld a,b			;8c55 A=tile1 (left leg)
	cp 028h			;8c56
	jr c,set_sliding_fall_left		;8c58 If left leg <= 0x28, then nothing on the left
	; Something on both the left and the right legs: in good balance over something.
	xor a			;8c5a
	ld (PULGA_ACTION),a	;8c5b PULGA_ACTION = 0 (idle)
	ld a,0f0h							;8c5e
	ld (pulga_sliding_sound_script),a	;8c60
	call PLAY_PULGA_LANDED				;8c63
	ret									;8c66
;
set_sliding_fall_left:
	; right leg: touching something
	; left leg: not touching
	ld a,085h				;8c67
	ld (PULGA_ACTION),a		;8c69 PULGA_ACTION = 0x85 (falling sliding left)
	ld de,(PULGA_X)			;8c6c E=PULGA_X, D=PULGA_Y
	dec e					;8c70 E=PULGA_X - 1 (fall to the left)
	ld hl,SPRITE_PULGA_FACE2_ADDR	;8c71
	call PLAY_PULGA_SLIDING			;8c74
	jp l8b0fh						;8c77


; ********************************************
; *                                          *
; * Get the tile at (sprite) position [H, L] *
; *                                          *
; ********************************************
GET_VRAM_TILE_POS_H_L:
	ld c,l			;8c7a
	ld l,h			;8c7b
	ld h,000h		;8c7c
	add hl,hl		;8c7e
	add hl,hl		;8c7f HL = L * 4 = Ho * 4
	ld a,l			;8c80 A = Ho * 4
	and 0e0h		;8c81 A = (Ho * 4) AND 1110 0000

	; It ANDs Ho * 4 with 1110 0000 because it wants to get the
	; nearest multiple of 32 of Ho*4.
	; Example: if [H, L] = [58, 120] it will do A = 58*4 AND 11100000 = 232 AND 11100000 = 224 = 7 * 32
	ld l,a			;8c83 L = (Ho * 4) AND 1110 0000	
	srl c			;8c84 ; C = Lo
	srl c			;8c86
	srl c			;8c88 C = Lo / 8
	; For example, if Lo = 120, C = Lo/8 = 120/8 = 15

	ld b,000h		;8c8a
	add hl,bc		;8c8c HL = ((Ho * 4) AND 1110 0000) + Lo/8
	; For example, HL = 224 + 15 = 239
	ld bc,NAME_TABLE ;8c8d
	add hl,bc		;8c90 HL = NAME_TABLE + ((Ho * 4) AND 1110 0000) + Lo/8
	; For example, NAME_TABLE + 239 = NAME_TABLE + 224 + 15 = NAME_TABLE + 32*7 + 15 = NAME_TABLE[7, 15]
	
	call RDVRM
	ret				;8c94


; *****************************************************
; * Obtain the next Bicho's target.                   *
; *                                                   *
; * Input:  BC = PELICAN_POS_PTR (a pointer) =        *
; *            = TABLE_START - 1                      *
; * Output: DE = 8*[6*[TABLE_START] + PELICAN_WALK]   *
; * Output: HL = 8*[6*[TABLE_START] + PELICAN_WALK+1] *
; *****************************************************
; This function is amazing! :)
; It uses a data structure which are records of
; six bytes each.
;
; The records have the following structure:
; TX, TX, B0, B1, B2, B3.

; Each record contains an target location (TX, TY) followed by
; four indices other four blocks.
; These other four blocks are associated to target coordinates such that
; Bicho can travel from the current (TX, TY) directly without
; encountering any obstacles.
;
; This way Bicho never needs to check if it will find an obstable to
; change its route. If Bicho follows the chain of positions, it will
; never face an obstacle.
GET_NEXT_BICHO_TARGET_IN_CHAIN:
	inc bc			;8c95 BC = [PELICAN_POS_PTR]+1
	
	; We shall call TABLE_START = [PELICAN_POS_PTR]+1
	; When it arrives here it BC points to the choice [0, 1, 2, 3].
	; If BC (input parameter) pointed to TY+k, then now it'll pick record #k
	ld a,(bc)		;8c96
	ld l,a			;8c97 
	ld h,000h		;8c98 HL = [TABLE_START]
	add hl,hl		;8c9a HL = 2*[TABLE_START]
	ld d,h			;8c9b
	ld e,l			;8c9c DE = HL == 2*[TABLE_START]
	add hl,hl		;8c9d HL = 2 * HL = 4*[TABLE_START]
	add hl,de		;8c9e HL = HL + DE = 6*[TABLE_START]
	ld de,PELICAN_WALK	;8c9f
	add hl,de		;8ca2 HL = 6*[TABLE_START] + 0x9000
	push hl			;8ca3 PUSH 6*[TABLE_START] + 0x9000

	; Here it's pushed the address of TX

	ld l,(hl)		;8ca4
	ld h,000h		;8ca5 HL = [6*[TABLE_START] + PELICAN_WALK]
	add hl,hl		;8ca7
	add hl,hl		;8ca8
	add hl,hl		;8ca9 HL = 8*[6*[TABLE_START] + PELICAN_WALK]
	
	; Here it has the value of TX. It multiplies by 8 since the
	; size is given in tile units. Thus, to have the absolute coordinate one
	; needs to multiply by the side of the tile (8).
	
	ex de,hl		;8caa DE = 8*[6*[TABLE_START] + PELICAN_WALK]
	pop hl			;8cab POP HL = 6*[TABLE_START] + PELICAN_WALK
	
	; Point to TY and set the position of the current record to the new
	inc hl			;8cac HL = 6*[TABLE_START] + PELICAN_WALK + 1
	ld (PELICAN_POS_PTR),hl	;8cad SET [PELICAN_POS_PTR] = 6*[TABLE_START] + PELICAN_WALK+1
	
	; Obtain TY
	ld l,(hl)		;8cb0
	ld h,000h		;8cb1 HL = [6*[TABLE_START] + PELICAN_WALK+1]
	add hl,hl		;8cb3
	add hl,hl		;8cb4
	add hl,hl		;8cb5 HL = 8*[6*[TABLE_START] + PELICAN_WALK+1]

	ret				;8cb6


; ****************************************************************
; * Compute the L1 distance between the                          *
; * target at (DE, HL) and ABS_PULGA.                            *
; *                                                              *
; * Input:  DE = BICHO_ABS_TARGET_X                              *
; * Input:  HL = BICHO_ABS_TARGET_Y                              *
; * Output: HL = abs(HLo - ABS_PULGA_Y) + abs(DEo - ABS_PULGA_X) *
; *                                                              *
; ****************************************************************
L1_DIST_TARGET_PULGA:
	push hl					;8cb7

	ld hl,(ABS_PULGA_X)		;8cb8
	rst 20h					;8cbb ; Compare HL = ABS_PULGA_X with BICHO_ABS_TARGET_X
	jr nc,l8cbfh			;8cbc
	; HL=ABS_PULGA_X < BICHO_ABS_TARGET_X
	ex de,hl				;8cbe DE = ABS_PULGA_X, HL = BICHO_ABS_TARGET_X
l8cbfh:
	; HL = max(BICHO_ABS_TARGET_X, ABS_PULGA_X)
	; DE = min(BICHO_ABS_TARGET_X, ABS_PULGA_X)
	or a			;8cbf Clear carry
	sbc hl,de		;8cc0     HL = abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)

	ex (sp),hl		;8cc2 (SP) = abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)

	ld de,(ABS_PULGA_Y)	;8cc3

	; DE = ABS_PULGA_Y
	; HL = BICHO_ABS_TARGET_Y
	rst 20h			;8cc7 Compare HL == BICHO_ABS_TARGET_Y with DE == ABS_PULGA_Y
	jr nc,l8ccbh		;8cc8
	; HL < DE=ABS_PULGA_Y
	ex de,hl			;8cca
l8ccbh:
	or a			;8ccb Clear carry
	sbc hl,de		;8ccc HL = abs(BICHO_ABS_TARGET_Y - ABS_PULGA_Y)
	
	pop de			;8cce DE = abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)
	add hl,de		;8ccf HL = abs(BICHO_ABS_TARGET_Y - ABS_PULGA_Y) + abs(BICHO_ABS_TARGET_X - ABS_PULGA_X)
	ret				;8cd0

; ********************************************
; * Update the absolute pulga position as    *
; * ABS_PULGA_X = 8 * SCROLL_X + PULGA_X     *
; * ABS_PULGA_Y = 8 * SCROLL_Y + PULGA_Y     *
; ********************************************
UPDATE_ABS_PULGA_POS:
	ld hl,(SCROLL_X)		;8cd1
	ld h,000h		;8cd4	HL = SCROLL_X
	add hl,hl
	add hl,hl
	add hl,hl		; HL = 8 * SCROLL_X
	ld de,(PULGA_X)	;8cd9 
	ld d,000h		; DE = PULGA_X
	add hl,de		; HL = 8 * SCROLL_X + PULGA_X
	ld (ABS_PULGA_X),hl		;8ce0

	ld hl,(SCROLL_Y)		;8ce3
	ld h,000h		;8ce6	HL = SCROLL_Y
	add hl,hl
	add hl,hl
	add hl,hl			; HL = 8 * SCROLL_Y
	ld de,(PULGA_Y)
	ld d,000h			; DE = PULGA_Y
	add hl,de			; HL = 8 * SCROLL_Y + PULGA_Y
	ld (ABS_PULGA_Y),hl		;8cf2
	ret			;8cf5

; ************************************************
; * Check if pulga is hitting the plant, if any. *
; * If so, shake plant and make plant sound.     *
; ************************************************
CHECK_PULGA_HITS_PLANT:
	ld a,(PLANT_IS_VISIBLE)		;8cf6	3a 43 bf 	: C . 
	or a			;8cf9	b7 	. 
	ret z			;8cfa	c8 	Putting a RET (C9) here makes pulga be able to jump through the plants
	
	; The plan is visible. Check if Pulga is hitting it
	ld hl,(PLANT_X)	;8cfb  H = PLANT_Y, L = PLANT_X
	ld a,e			;8cfe
	add a,00eh		;8cff  A = PULGA_X + 14
	cp l			;8d01
	ret c			;8d02  Leave, PLANT_X > PULGA_X + 14
	sub 01ch		;8d03  A = PULGA_X + 14 - 28 = PULGA_X - 14
	cp l			;8d05
	ret nc			;8d06  Leave, PLANT_X <= PULGA_X - 14
	
	; Here PULGA_X - 14 < PLANT_X <= PULGA_X + 14
	
	ld a,d			;8d07
	add a,00eh		;8d08  A = PULGA_Y + 14
	cp h			;8d0a
	ret c			;8d0b  Leave if PLANT_Y > PULGA_Y + 14
	sub 022h		;8d0c  A = PULGA_Y + 14 - 34 = PULGA_Y - 20
	cp h			;8d0e
	ret nc			;8d0f  Leave if PLANT_Y <= PULGA_Y - 20

	; Discard return addresses
	pop bc			;8d10
	pop bc			;8d11
	
	exx				;8d12	d9 	. 
	call DO_PLANT_SOUND_SEMIOPEN	;8d13
	exx								;8d16

	; Choose to which side fall to
	ld a,e							;8d17 A = PULGA_X
	cp l							;8d18
	jp nc,set_sliding_fall_right	;8d19 PLANT_X <= PULGA_X ==> Fall right
	jp set_sliding_fall_left		;8d1c

; This seems to check if pulga is being eaten by a plan
; This is only called when pulga is falling, but not if steady or jumping.
; Input: E = PULGA_X
CHECK_PLANT_EATS_PULGA:
	ld a,(PLANT_IS_VISIBLE)		;8d1f
	or a			;8d22
	ret z			;8d23  Get out if the plant isn't visible.
	ld hl,(PLANT_X) ;8d24  H = PLANT_Y, L = PLANT_X
	ld a,e			;8d27  A = E = PULGA_X
	add a,10		;8d28  A = PULGA_X + 10
	cp l			;8d2a
	ret c			;8d2b  Get out if PULGA_X + 10 < PLANT_X <==> PULGA_X < PLANT_X - 10

	sub 20  		;8d2c  A = PULGA_X + 10 - 20 = PULGA_X - 10
	cp l			;8d2e
	ret nc			;8d2f  Get out if PULGA_X - 10 >= PLANT_X <==> PULGA_X >= PLANT_X + 10
	
	; In summary: go on only if pulga is too close to the plant, when
	; PLANT_X - 10 <= PULGA_X < PLANT_X + 10

	ld a,d			;8d30  D = PULGA_Y
	add a,20		;8d31  A = PULGA_Y + 20
	cp h			;8d33
	ret c			;8d34  Get out if PULGA_Y + 20 < PLANT_Y
	sub 26  		;8d35  A = PULGA_Y + 20 - 26 = PULGA_Y - 6
	cp h			;8d3 
	ret nc			;8d38  Get out if PULGA_Y - 6 >= PLANT_Y <==> PULGA_Y >= PLANT_Y + 6

	; In summary: go on only if pulga is too close to the plant, when
	; PLANT_Y - 6 <= PULGA_Y < PLANT_Y + 20

	; Here it performs two POP most probably to discard returns addresses and to
	; jump directly to END_GAME later.
	pop bc			;8d39	It arrives here when the plant is eating pulga
	pop bc			;8d3a
	ld a,d			;8d3b A = PULGA_Y
	cp h			;8d3c 
	jr z,l8d6bh		;8d3d Continue only if PULGA_Y != PLANT_Y.
					;     This condition is probably because pulga goes
					;     down as the plant eats it.
	
	exx			;8d3f	d9 	. 
	ld a,00ch		;8d40	3e 0c 	> . 
	ld (SOUND_SNAP),a		;8d42	32 6f 8e 	2 o . 
	call DO_PLANT_SOUND_SEMIOPEN		;8d45	cd e3 88 	. . . 
	exx			;8d48	d9 	. 
	dec d			;8d49	15 	. 
	ld a,e			;8d4a	7b 	{ 
	cp l			;8d4b	bd 	. 
	dec e			;8d4c	1d 	. 
	jr nc,l8d51h		;8d4d	30 02 	0 . 
	inc e			;8d4f	1c 	. 
	inc e			;8d50	1c 	. 
l8d51h:
	ld a,r		;8d51	ed 5f 	. _ 
	ld hl,SPRITE_PULGA_FACE2_ADDR		;8d53	21 90 93 	! . . 
	cp 040h		;8d56	fe 40 	. @ 
	jr c,l8d5dh		;8d58	38 03 	8 . 
	ld hl,SPRITE_PULGA_JUMPING_FACE1_ADDR		;8d5a	21 10 93 	! . . 
l8d5dh:
	rra			;8d5d	1f 	. 
	ld a,00ah		;8d5e	3e 0a 	> . 
	rra			;8d60	1f 	. 
	ld (PULGA_ACTION),a		;8d61	32 30 bf 	2 0 . 
	xor a			;8d64	af 	. 
	ld (l8a4eh+1),a		;8d65	32 4f 8a 	2 O . 
	jp l8b0fh		;8d68	c3 0f 8b 	. . . 
l8d6bh:
	ld b,028h		;8d6b	06 28 	. (

; Animation: the plan moves after eating pulga
anim_plant_after_pulga_eaten:
	push bc								;8d6d
	ld hl,SPRITE_ATTRIBUTE_TABLE + 4*6  ;8d6e Pulga body sprite (sprite #6)
	ld a,0c0h							;8d71 Position of pulga: invisible
	call WRTVRM							;8d73
	call DO_BICHO_CYCLE					;8d76  BICHO_CYCLE, but without checking the Bicho's counter
	ld a,(PLANT_FRAME)					;8d79
	call DO_ANIM_PLANT_FRAME			;8d7c

	ld hl,02000h						;8d7f
	call DELAY_HL						;8d82

	pop bc								;8d85
	djnz anim_plant_after_pulga_eaten	;8d86
	ld a,00ah							;8d88
	ld e,000h							;8d8a
	call WRTPSG							;8d8c
	jp END_GAME							;8d8f
	
	; Some zeros here...
	defs 0x8d9f - 0x8d92 + 1

l8da0h:
	ld a,(bicho_eating_counter)		;8da0	3a ff 8d 	: . . 
	dec a			;8da3	3d 	= 
	dec a			;8da4	3d 	= 
	ld (bicho_eating_counter),a		;8da5	32 ff 8d 	2 . . 
	jp z,END_GAME		;8da8	ca ab 84 	. . . 

	ld (bicho_eating_sound_script),a		;8dab Change the sound
	; Change color of pulga's body
	and 00fh		;8dae Limit to MSX's 15 colors
	ld hl, SPRITE_ATTRIBUTE_TABLE + 4*6 + 3 ;8db0 Pulga body sprite (sprite #6), color
	call WRTVRM		;8db3	cd 4d 00 	. M . 
	call PLAY_BICHO_EATING		;8db6	cd 50 8f 	. P . 
	ld hl,SPRITE_PULGA_FACE2_ADDR		;8db9	21 90 93 	! . . 
	jp l8b0fh		;8dbc	c3 0f 8b 	. . . 

	; Some zeros here...
	defs 0x8dfe - 0x8dbf + 1

bicho_eating_counter: db 0

PLAY_VIVALDI:
	call GICINI			;8e00	cd 90 00 	. . . 
	call PLAY_VIVALDI_AFTER_INIT		;8e03 Play music and return directly to the caller, to 0x83BF
	; Vivaldi 4-stations music
	defb 0xd1, 0x10, 0x27, 0x0f, 0x01, 0x1d, 0x0f, 0, 0xbe, 0x8, 0
	defb 0xd5, 8, 0, 0xe2, 0x0f, 0, 0xd5, 0x0f, 0, 0xbe, 0x0f, 0
	defb 0xa9, 0x1e, 0, 0xbe, 0x0f, 0, 0xd5, 0x0f, 0, 0xe2, 0x08, 0
	defb 0xfe, 0x08, 0x01, 0x1d, 0x0f, 0, 0xfe, 0x0f, 0, 0xfe, 0x1e
	defb 0x01, 0x1d, 0, 0x6a

	; This seems to be unused code
	ld a,(sound_script1)		;8e38	3a 44 8e 	: D . 
	or a			;8e3b	b7 	. 
	ret z			;8e3c	c8 	. 
	xor a			;8e3d	af 	. 
	ld (sound_script1),a		;8e3e	32 44 8e 	2 D . 
	call MAKE_SOUND		;8e41	cd b4 8e 	. . . 
	sound_script1:
	defb 0xe, 0x0, 0x1f, 0x9b, 0x0, 0x0, 0x10, 0x0, 0x10, 0x4
	ret			;8e4e

PLAY_PULGA_LANDED:
	call MAKE_SOUND		;8e4f	cd b4 8e 	. . . 
	defb 0x80, 0x0, 0xf, 0x9b, 0x0, 0x0, 0x10, 0x0, 0x1, 0x0
	ret

PLAY_RANDOM_SOUND:
	ld a,r		;8e5d	ed 5f 	. _ 
	and 03fh		;8e5f	e6 3f 	. ? 
	add a,014h		;8e61	c6 14 	. . 
	ld (sound_script),a		;8e63	32 69 8e 	2 i . 
	call MAKE_SOUND		;8e66	cd b4 8e 	. . . 
sound_script:
	defb 0x3c, 0x00, 0xff, 0xbb, 0x00, 0x00
SOUND_SNAP: defb 0x06
	defb 0xff, 0xff, 0xff
	ret			;8e73 --> It will arrive here after exiting the MAKE_SOUND above

PLAY_PULGA_SLIDING:
	ld a,(TIMEOUT+1)	;8e74
	or a				;8e77
	ret nz				;8e78 Play only at certain TIMEOUT increments
	;
	inc a				;8e79
	ld (TIMEOUT+1),a	;8e7a
	ld a,(pulga_sliding_sound_script)	;8e7d
	add a,010h							;8e80
	ld (pulga_sliding_sound_script),a						;8e82
	call MAKE_SOUND						;8e85
pulga_sliding_sound_script:
	defb 0xf0, 0x1, 0xff, 0xbb, 0x0, 0x0, 0x10, 0x0, 0x4, 0x0
	ret									;8e92

PLAY_JUMP:
	ld a,(jump_sound_pitch)		;8e93	3a a9 8e --> Changing the first to 0x33 makes it exit to BASIC :)
	or a			;8e96	b7 	. 
	ret z			;8e97	c8 	. 
	sub 008h		;8e98	d6 08 	. . 
	ld (jump_sound_pitch),a		;8e9a	32 a9 8e 	2 . . 
	rra			;8e9d	1f 	. 
	rra			;8e9e	1f 	. 
	rra			;8e9f	1f 	. 
	rra			;8ea0	1f 	. 
	and 00fh		;8ea1	e6 0f 	. . 
	ld (l8eafh),a		;8ea3	32 af 8e 	2 . . 
	call MAKE_SOUND		;8ea6	cd b4 8e 	. . . 
jump_sound_pitch:
	nop			;8ea9	00 	. 
	nop			;8eaa	00 	. 
	rst 38h			;8eab	ff 	. 
	cp e			;8eac	bb 	. 
	nop			;8ead	00 	. 
	nop			;8eae	00 	. 
l8eafh:
	nop			;8eaf	00 	. 
	rst 38h			;8eb0	ff 	. 
	rst 38h			;8eb1	ff 	. 
	rst 38h			;8eb2	ff 	. 
	ret			;8eb3	c9 	. 


; ********************************************************************
; * Make a sound.                                                    *
; * It also delays the execution, which is needed for synchonization *
; * The return address will be put 0xe - 4 + 1= 11 bytes after the   *
; * sound script.                                                    *
; ********************************************************************
MAKE_SOUND:
	ex (sp),hl		;8eb4 It uses the ret address to make sounds! :D
	push de			;8eb5
	ld a,004h		;8eb6 We start with the PSG register #4: tone generator control
l8eb8h:
	ld e,(hl)		;8eb8 E: value to write to the PSG
	inc e			;8eb9
	jr z,l8ec0h		;8eba If E == 255 then end
	dec e			;8ebc
	call WRTPSG		;8ebd Play sound
l8ec0h:
	inc hl			;8ec0 Increment pointer
	inc a			;8ec1
	cp 00eh			;8ec2 If psg_reg != 14 then repeat
	jr nz,l8eb8h	;8ec4
	pop de			;8ec6
	ex (sp),hl		;8ec7 Point the return address to the ret instruction after the sound script
	ret				;8ec8

; ********************************************
; * Play Vivaldi's Four Seasons after GICINI *
; ********************************************
PLAY_VIVALDI_AFTER_INIT:
	ex (sp),hl			;8ec9 ; HL = return address = 0x8E06
	ld de,VOICAQ		;8eca
	di					;8ecd
	xor a				;8ece
	ld (de),a			;8ecf Write VOICAQ[0] = 0
	ld a,(hl)			;8ed0 A = (0x8E06) = 0xD1
	inc hl				;8ed1 HL = 0x8E07
	ex af,af'			;8ed2
	ld c,(hl)			;8ed3 C = (0x8E07) = 0x10
	inc hl				;8ed4 HL = 0x8E08
	ld b,(hl)			;8ed5 B = (0x8E08) = 0x27
	inc hl				;8ed6 HL = 0x8E09
	inc de				;8ed7 DE = VOICAQ + 1
	ld a,b				;8ed8 A = (0x8E08)
	or c				;8ed9 A (0x8E08) == C (0x8E07)?
	jr z,l8f08h			;8eda Jump if A == B

l8edch:
	ld a,(hl)			;8edc A = (0x8E09) = 0xF
	inc hl				;8edd HL = 0x8E0A
	or a				;8ede A == 0?
	jp z,l8f2dh			;8edf Jump if A == 0

	inc de				;8ee2 DE = VOICAQ + 2
	ld (de),a			;8ee3 Write (VOICAQ + 2)
	dec de				;8ee4 DE = VOICAQ + 2
l8ee5h:
	ld a,(hl)			;8ee5	7e 	~ 
	inc a			;8ee6	3c 	< 
	ld a,0a0h		;8ee7	3e a0 	> . 
	jr nz,l8eedh		;8ee9	20 02 	  . 
	ld a,020h		;8eeb	3e 20 	>   
l8eedh:
	ld (de),a			;8eed	12 	. 
	inc de			;8eee	13 	. 
	inc de			;8eef	13 	. 
	ld a,080h		;8ef0	3e 80 	> . 
	jr z,l8f03h		;8ef2	28 0f 	( . 
	ex af,af'			;8ef4	08 	. 
	ld (de),a			;8ef5	12 	. 
	ex af,af'			;8ef6	08 	. 
	inc de			;8ef7	13 	. 
	ld a,b			;8ef8	78 	x 
	ld (de),a			;8ef9	12 	. 
	inc de			;8efa	13 	. 
	ld a,c			;8efb	79 	y 
	ld (de),a			;8efc	12 	. 
	inc de			;8efd	13 	. 
	ld a,(hl)			;8efe	7e 	~ 
	ld (de),a			;8eff	12 	. 
	inc de			;8f00	13 	. 
	inc hl			;8f01	23 	# 
	ld a,(hl)			;8f02	7e 	~ 
l8f03h:
	ld (de),a			;8f03	12 	. 
	inc de			;8f04	13 	. 
	inc hl			;8f05	23 	# 
	jr l8edch		;8f06	18 d4 	. . 
l8f08h:
	ld a,(hl)			;8f08	7e 	~ 
	inc hl			;8f09	23 	# 
	or a			;8f0a	b7 	. 
	jr z,l8f2dh		;8f0b	28 20 	(   
	inc de			;8f0d	13 	. 
	ld (de),a			;8f0e	12 	. 
	dec de			;8f0f	1b 	. 
	ld a,(hl)			;8f10	7e 	~ 
	inc a			;8f11	3c 	< 
	ld a,060h		;8f12	3e 60 	> ` 
	jr nz,l8f18h		;8f14	20 02 	  . 
	ld a,020h		;8f16	3e 20 	>   
l8f18h:
	ld (de),a			;8f18	12 	. 
	inc de			;8f19	13 	. 
	inc de			;8f1a	13 	. 
	ld a,080h		;8f1b	3e 80 	> . 
	jr z,l8f28h		;8f1d	28 09 	( . 
	ex af,af'			;8f1f	08 	. 
	ld (de),a			;8f20	12 	. 
	ex af,af'			;8f21	08 	. 
	inc de			;8f22	13 	. 
	ld a,(hl)			;8f23	7e 	~ 
	ld (de),a			;8f24	12 	. 
	inc de			;8f25	13 	. 
	inc hl			;8f26	23 	# 
	ld a,(hl)			;8f27	7e 	~ 
l8f28h:
	ld (de),a			;8f28	12 	. 
	inc de			;8f29	13 	. 
	inc hl			;8f2a	23 	# 
	jr l8f08h		;8f2b	18 db 	. . 
l8f2dh:
	ld a,0ffh		;8f2d	3e ff 	> . 
	ld (de),a			;8f2f	12 	. 
	ld a,(hl)			;8f30	7e 	~ 
	inc hl			;8f31	23 	# 
	ld (QUETAB),a		;8f32	32 59 f9 	2 Y . 
	xor a			;8f35	af 	. 
	ld (QUETAB+1),a		;8f36	32 5a f9 	2 Z . 

	ex (sp),hl			;8f39	e3 	. 
	ld hl,00001h		;8f3a	21 01 00 	! . . 
	ld (VCBA),hl		;8f3d	22 41 fb 	" A . 
	ld (VCBB),hl		;8f40	22 66 fb 	" f . 
	ld (VCBC),hl		;8f43	22 8b fb 	" . . 
	ld a,007h		;8f46	3e 07 	> . 
	ld (MUSICF),a		;8f48	32 3f fb 	2 ? . 
	pop hl				;8f4b HL = 0x8E38
	ei					;8f4c Return to 0x83BF
	ret					;8f4d
	
	defb 0x0, 0x0 ; 8f4e

PLAY_BICHO_EATING:
	call MAKE_SOUND		;8f50	cd b4 8e 	. . . 
bicho_eating_sound_script:
	ld (bc),a			;8f53	02 	. 
	ld bc,0bbffh		;8f54	01 ff bb 	. . . 
	nop			;8f57	00 	. 
	nop			;8f58	00 	. 
	ld a,(bc)			;8f59	0a 	. 
	rst 38h			;8f5a	ff 	. 
	rst 38h			;8f5b	ff 	. 
	rst 38h			;8f5c	ff 	. 
	ret			;8f5d	c9 	. 

	; Some zeros here...
	defs 0x8fff - 0x8f5e + 1

PELICAN_WALK:
include 'pelican_script.asm' 

BOOGABOO_TITLE_PATTERNS:
	ld e,(hl)			;9233	5e 	^ 
	ld h,l			;9234	65 	e 
	cpl			;9235	2f 	/ 
	ld e,l			;9236	5d 	] 
	ld e,a			;9237	5f 	_ 
	ld h,b			;9238	60 	` 
	ld h,b			;9239	60 	` 
	ld h,(hl)			;923a	66 	f 
	dec (hl)			;923b	35 	5 
	ld e,(hl)			;923c	5e 	^ 
	ld h,h			;923d	64 	d 
	ld h,l			;923e	65 	e 
	ld h,l			;923f	65 	e 
	ld (hl),b			;9240	70 	p 
	cpl			;9241	2f 	/ 
	ld e,(hl)			;9242	5e 	^ 
	ld e,(hl)			;9243	5e 	^ 
	ld h,c			;9244	61 	a 
	ld h,c			;9245	61 	a 
	ld (hl),l			;9246	75 	u 
	ld hl,(06060h)		;9247	2a 60 60 	* ` ` 
	ld h,d			;924a	62 	b 
	ld h,d			;924b	62 	b 
	ld (hl),l			;924c	75 	u 
	jr 0x92b0		;924d	18 61 	. a 
	ld h,c			;924f	61 	a 
	ld h,e			;9250	63 	c 
	ld h,e			;9251	63 	c 
	ld h,h			;9252	64 	d 
	jr $+100		;9253	18 62 	. b 
	ld h,d			;9255	62 	b 
	ld e,h			;9256	5c 	\ 
l9257h:
	ld e,h			;9257	5c 	\ 
	ld l,l			;9258	6d 	m 
	inc a			;9259	3c 	< 
	ld e,a			;925a	5f 	_ 
	ld e,a			;925b	5f 	_ 
	ld (bc),a			;925c	02 	. 
	ld (bc),a			;925d	02 	. 
	ld h,d			;925e	62 	b 
	scf			;925f	37 	7 
	ld e,a			;9260	5f 	_ 
	ld e,a			;9261	5f 	_ 
	ld h,(hl)			;9262	66 	f 
l9263h:
	ld h,(hl)			;9263	66 	f 
	ld d,a			;9264	57 	W 
	ld b,d			;9265	42 	B 
	ld h,l			;9266	65 	e 
	ld h,l			;9267	65 	e 
	ld h,a			;9268	67 	g 
	ld h,a			;9269	67 	g 
	ld d,(hl)			;926a	56 	V 
	ld b,d			;926b	42 	B 
	ld h,(hl)			;926c	66 	f 
	ld h,(hl)			;926d	66 	f 
	ld d,h			;926e	54 	T 
	ld d,h			;926f	54 	T

include 'sprites.asm'

PARABOLA_TABLE: ;0x94b0
include 'parabola_table.asm'

; Title screen text messages (and BOOGA-BOO with redefined patterns)
; From QUICKSILVA_STR = 0x94DF to 0x95BC (222 bytes)
include 'title_text.asm'


NAME_STR: ; 95bdh
	defb "Please enter your name", 0
CONGRATULATIONS_STR: ; 95d4h
	defb "CONGRATULATIONS", 0
YOU_DID_IT_STR: ; 95e4h
	defb "You did it. Now try again.", 0
TIME_STR: ; 95ffh
	defb "time", 0
LEVEL_STR: ; 9604h
	defb "level", 0
SCORE_STR: ; 960ah
	defb "score", 0
HI_STR: ; 9610h
	defb "hi", 0

GRAPH_SCORE_LINE:
	ld a,(bc)			;9613	0a 	. 
l9614h:
	djnz l9625h		;9614	10 0f 	. . 
	inc de			;9616	13 	. 
	ld (de),a			;9617	12 	. 
	inc d			;9618	14 	. 
	ex af,af'			;9619	08 	. 
	nop			;961a	00 	. 
	nop			;961b	00 	. 
	nop			;961c	00 	. 
	jr nz,l9631h		;961d	20 12 	  . 
l961fh:
	dec bc			;961f	0b 	. 
	djnz l9633h		;9620	10 11 	. . 
l9622h:
	inc c			;9622	0c 	. 
l9623h:
	inc d			;9623	14 	. 
DECODED_SCORE:
	nop			;9624	00 	. 
l9625h:
	nop			;9625	00 	. 
	nop			;9626	00 	. 
	nop			;9627	00 	. 
	nop			;9628	00 	. 
	jr nz,l9638h		;9629	20 0d 	  . 
	ld c,014h		;962b	0e 14 	. . 
DECODED_HISCORE:
	nop			;962d	00 	. 
	nop			;962e	00 	. 
	nop			;962f	00 	. 
	nop			;9630	00 	. 
l9631h:
	nop			;9631	00 	. 
	rst 38h			;9632	ff 	. 
l9633h:
	rlca			;9633	07 	. 
	rlca			;9634	07 	. 
	rlca			;9635	07 	. 
	rlca			;9636	07 	. 
	rlca			;9637	07 	. 
l9638h:
	rlca			;9638	07 	. 
	rlca			;9639	07 	. 
	rlca			;963a	07 	. 
	rst 38h			;963b	ff 	. 
	rst 38h			;963c	ff 	. 
	rst 38h			;963d	ff 	. 
l963eh:
	nop			;963e	00 	. 
	nop			;963f	00 	. 
	nop			;9640	00 	. 
	nop			;9641	00 	. 
	nop			;9642	00 	. 
	nop			;9643	00 	. 
l9644h:
	nop			;9644	00 	. 
	nop			;9645	00 	. 
	nop			;9646	00 	. 
	nop			;9647	00 	. 
	rst 38h			;9648	ff 	. 
	rst 38h			;9649	ff 	. 
	rst 38h			;964a	ff 	. 
	ret p			;964b	f0 	. 
	call m,01efeh		;964c	fc fe 1e 	. . . 
	rrca			;964f	0f 	. 
	rlca			;9650	07 	. 
	rlca			;9651	07 	. 
	rlca			;9652	07 	. 
	rlca			;9653	07 	. 
	rlca			;9654	07 	. 
	rlca			;9655	07 	. 
l9656h:
	rrca			;9656	0f 	. 
	ld e,0feh		;9657	1e fe 	. . 
	call m,078f0h		;9659	fc f0 78 	. . x 
	inc a			;965c	3c 	< 
	ld e,00eh		;965d	1e 0e 	. . 
	rlca			;965f	07 	. 
	rlca			;9660	07 	. 
	rlca			;9661	07 	. 
	rlca			;9662	07 	. 
	nop			;9663	00 	. 
	ld bc,00303h		;9664	01 03 03 	. . . 
	rlca			;9667	07 	. 
	rlca			;9668	07 	. 
l9669h:
	rlca			;9669	07 	. 
	rlca			;966a	07 	. 
	rlca			;966b	07 	. 
	rlca			;966c	07 	. 
	rlca			;966d	07 	. 
	rlca			;966e	07 	. 
	inc bc			;966f	03 	. 
	inc bc			;9670	03 	. 
	ld bc,07f00h		;9671	01 00 7f 	. .  
	rst 38h			;9674	ff 	. 
	rst 38h			;9675	ff 	. 
	ret nz			;9676	c0 	. 
	add a,b			;9677	80 	. 
	nop			;9678	00 	. 
	nop			;9679	00 	. 
	nop			;967a	00 	. 
	nop			;967b	00 	. 
	nop			;967c	00 	. 
	nop			;967d	00 	. 
	add a,b			;967e	80 	. 
	ret nz			;967f	c0 	. 
	rst 38h			;9680	ff 	. 
	rst 38h			;9681	ff 	. 
	ld a,a			;9682	7f 	 
	rlca			;9683	07 	. 
	rlca			;9684	07 	. 
	rlca			;9685	07 	. 
	rlca			;9686	07 	. 
	rlca			;9687	07 	. 
	rst 38h			;9688	ff 	. 
	rst 38h			;9689	ff 	. 
	rst 38h			;968a	ff 	.
	 
GAME_MAP:
	include 'game_map.asm'

GAME_TILES: ; 246*8	bytes, from 0xadc4 to 0xb573
	include 'game_tiles.asm'

GAME_MAP_POINTERS:
	include 'game_map_pointers.asm'
	
GAME_COLOR_TABLE: ; 0xB674 -- 0xB693
	include 'color_table.asm'
	
	; A block of mainly 0xff words
	include 'block1.asm'

; 085h: falling right
;   5:  fall
; 0x40: jump right
; 0xc0: jump left
; ...

; Bit #7: oriented left
; Bit #6: preparing jump
; ...
; Bit #2: actually sliding (left:0x05 or right:0x85)
; Bit #1: jump is going down
; Bit #0: flying

PULGA_ACTION: defb 0 ;bf30

defb 0xff ;bf31 Unused?

TIMEOUT: defb 0x00, 0xff ; bf32

; Local coordinates of pulga, within the screen
PULGA_X: defb 0    ;bf34
PULGA_Y: defb 0xff ;bf35

ABS_PULGA_X: defb 0x00, 0xff   ;bf36
ABS_PULGA_Y: defb 0x00, 0xff   ;bf38

BICHO_COUNTER: defb 0x00	   ;bf3a

BICHO_STATUS1: defb 0xff	   ;bf3b # bit 7: look right
BICHO_THREAT_STATUS: defb 0x00 ;bf3c bit 0: near to pulga. bit 7: pulga catched (pulga position = Bicho's)

BICHO_FRAME: defb 0xff ;bf3d
 
ABS_BICHO_X: defb 0x00, 0xFF    ;bf3e
ABS_BICHO_Y: defb 0x00, 0xFF    ;bf40

PLANT_TIMEOUT: defb 0x00 		;bf42

; 0x3F (PLANT_BASE_CHR) if the plan is present in the visible part of the scenario
; 0                     otherwise
PLANT_IS_VISIBLE: defb 0xff
PLANT_FRAME: defb 0x00
RESET_PSG_FLAG:  defb 0xff ; bf45 Flag to reset the PSG after the plant makes noise
PLANT_X: defb 0x00
PLANT_Y: defb 0xff

PELICAN_POS_PTR: defw 0xff00 ; bf48

BICHO_ABS_TARGET_X: defb 0x00, 0xFF    ;bf4a
BICHO_ABS_TARGET_Y: defb 0x00, 0xFF    ;bf4c

JUMP_LENGTH: defw 0xff00 ; bf4e

FLIGHT: defb 0x00 ; bf50
IS_SCROLLING: defb 0xff ; bf51 1 = scrolling, 0 = not scrolling

; The starting coordinate of the visible scenario
SCROLL_X: defb 0x00 ;bf52
SCROLL_Y: defb 0xFF ;bf53

BONUS:	defw 0xff00 ;bf54

SCORE: defw 0xff00 ;bf56

SCORE_PULGA_DEPTH: defb 0x00 ;bf58
RANDOM_SOUND_BYTE: defb 0xff  ;bf59

SCORE_TABLE: ;bf5a
; It starts at BF5A, with format: score (2 bytes) + 14 bytes for the name
; Second score: at BF6A. And so on.
; The score are show multiplied by 10.
; 5 entries, with 2 bytes for the record and 14 for the name.

; Entry #1: 0xBF5A
defb 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff
; Entry #2: 0xBF6A
defb 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff
; Entry #3: 0xBF7A
defb 0x00, 0xff, 0x00, 0xff, 0x00, 0x86, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff
; Entry #4: 0xBF8A
defb 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff
; Entry #5: 0xBF9A
SCORE_ENTRY_5:
defb 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xfe, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff, 0x00, 0xff
DUMMY_SCORE_ENTRY_6:

; Unused? 0xBFAA
include 'block2.asm'
