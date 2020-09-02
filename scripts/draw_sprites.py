#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

'''
Draw the BOOGA-BOO tiles

(c) 2019 Miguel Colom
http://mcolom.info
'''

import argparse
import cv2
import numpy as np
from funcs import get_msx1_colors, binary, extract_color_table
import sys
        
description = "Draw BOOGA-BOO's sprites"
epilog = '(C) Miguel Colom, GNU GPL3 license. http://mcolom.info'

parser = argparse.ArgumentParser(description=description, epilog=epilog)
#parser.add_argument("input")
parser.parse_args()
args = parser.parse_args()

msx1_colors = get_msx1_colors()

# Read all game dump
with open('../a.bin', 'rb') as f:
    dump = f.read()

offset = 0x8000
patterns_addr = 0x9270

# Extract color table
CT = extract_color_table(dump, offset, msx1_colors)

T = np.zeros((16, 16, 3))

# Get color

shifts = ((0,0), (8,0),(0,8),(8,8))

for sprite_num in range(18):
    addr = patterns_addr + sprite_num*32

    if sprite_num in (3, 9):
        background, foreground  = msx1_colors[0], msx1_colors[8]
    elif 4 <= sprite_num <= 10:
        background, foreground  = msx1_colors[0], msx1_colors[14]
    elif 11 <= sprite_num <= 17:
        background, foreground  = msx1_colors[0], msx1_colors[7]
    elif 0 <= sprite_num <= 2:
        background, foreground  = msx1_colors[0], msx1_colors[10]
    else:
        background, foreground  = msx1_colors[10], msx1_colors[11]

    for i in range(4):
        sx, sy = shifts[i % 4]
        
        # Iterate through the rows of the pattern
        for r in range(8):
            string = binary(dump[patterns_addr + sprite_num*32 + i*8 + r - offset])
            # Iterate through the columns of the tile
            for c in range(8):
                for ch in range(3):
                    T[r+sx,c+sy, 2-ch] = background[ch] if string[c] == '0' else foreground[ch]
    
    R = cv2.resize(T, (128, 128), interpolation=cv2.INTER_AREA)
    cv2.imwrite(f'gi_sprite_{sprite_num}_{hex(addr)}.png', R)
