; This address is used to control the foreground and
; background colors of the characters.
FORE_AND_BACK_COLORS: equ FORCLR

; Screen 1 (in game)
SPRITE_CHARACTER_PATTERNS: equ 0x3800
SPRITE_ATTRIBUTE_TABLE: equ 0x1B00
CHARACTERS_COLOUR_TABLE: equ 0x2000
NAME_TABLE: equ 0x1800

; Screen 2 (title screen)
PIXELBYTE_COLOUR_TABLE: equ 0x2000 ;2000-37FF

COLOR_CYAN: equ 7
COLOR_RED: equ 8
COLOR_GRAY: equ 14

COLOR_WHITE_BLACK: equ 0xF1       ; White foreground, back background
COLOR_TRANSPARENT_BLACK: equ 0x01 ; Transparent foreground, back background
COLOR_GREEN_BLACK: equ 0xC1       ; dark green foreground, back background
COLOR_YELLOW_BLACK: equ 0xA1       ; dark yellow foreground, back background
COLOR_GRAY_BLACK: equ 0xE1       ; gray foreground, back background


COLOR_DARK_YELLOW: equ 0x0A
COLOR_DARK_YELLOW_WITH_EC: equ 0x8A ; Color with Early Clock active
COLOR_CYAN_EC: equ 0x87

SPRITE_PLANT_OPEN_ADDR: equ 0x9270
SPRITE_PLANT_SEMIOPEN_ADDR: equ 0x9290
SPRITE_PLANT_CLOSED_ADDR: equ 0x92b0

SPRITE_PULGA_FACE1_ADDR: equ 0x92d0
SPRITE_PULGA_JUMPING_FACE1_ADDR: equ 0x9310
SPRITE_PULGA_JUMPING_FACE2_ADDR: equ 0x9350
SPRITE_PULGA_FACE2_ADDR: equ 0x9390

SPRITE_BICHO_FACE: equ 0x93d0
SPRITE_BICHO_LEGS_ADDR: equ 0x93f0
SPRITE_BICHO_OPEN_MOUTH_ADDR: equ 0x9470
SPRITE_BICHO_LOOKING_UP_ADDR: equ 0x9490

SPRITE_BICHO_WINGS1: equ 0x9410
SPRITE_BICHO_WINGS2: equ 0x9430
SPRITE_BICHO_WINGS3: equ 0x9450


; From https://www.msx.org/forum/msx-talk/development/initial-colors-and-screen-mode-startup
; SCREEN 1 (coloured text mode, 32 column):
;     Character patterns (font)        0000-07FF
;     Name table (char positions)      1800-1AFF
;     Sprite attribute table           1B00-1B7F
;     Characters colour table (8/byte) 2000-201F
;     Sprite character patterns        3800-3FFF

;SCREEN 2 (256*192 Graphics mode):
;   Character patterns              0000-17FF
;   Name table (char positions)     1800-1AFF
;   Sprite attribute table          1B00-1B7F
;   PixelByte colour table          2000-37FF
;   Sprite character patterns       3800-3FFF

; Sprite attribute table
; SA ->	 0	Y sprite 0
;        1	X sprite 0
;        2	sprite pattern 0
;        3	colour sprite 0 + EC
;        ---
;        4	Y sprite 1
;        5	X sprite 1
;        6	sprite pattern 1
;        7	colour sprite 1 + EC
;        .
;        .

; See colors from example at https://paulwratt.github.io/programmers-palettes/HW-MSX/HW-MSX-palettes.html

