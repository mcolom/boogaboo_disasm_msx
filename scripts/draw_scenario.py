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
        
description = "Draw the BOOGA-BOO scenario"
epilog = '(C) Miguel Colom, GNU GPL3 license. http://mcolom.info'

parser = argparse.ArgumentParser(description=description, epilog=epilog)
#parser.add_argument("input")
parser.parse_args()
args = parser.parse_args()

msx1_colors = get_msx1_colors()

num_tiles = 246
num_rows = 128
num_columns = 128

def read_row_pointer(addr, offset, dump):
    return dump[addr+1 - offset]*256 + dump[addr - offset]

# Read all game dump
with open('../a.bin', 'rb') as f:
    dump = f.read()

tiles = np.zeros((num_tiles, 3))
offset = 0x8000

# Extract color table
CT = extract_color_table(dump, offset, msx1_colors)

# Extract all tiles
tiles_addr = 0xADC4
tiles = {}
for i in range(num_tiles):
    tile_idx = 10 + i
    T = np.zeros((8, 8, 3))

    # Get color
    background, foreground  = CT[int((tile_idx) / 8)] # + 10 to skip numbers    

    # Iterate through the rows of the tile
    for r in range(8):
        string = binary(dump[tiles_addr + i*8 + r - offset])
        # Iterate through the columns of the tile
        for c in range(8):
            for ch in range(3):
                T[r,c, 2-ch] = background[ch] if string[c] == '0' else foreground[ch]    
    #cv2.imwrite('gi_tiles/tile_{}.png'.format(tile_idx), T)
    tiles[tile_idx] = T


# Save tiles in color as an image
I = np.zeros((num_rows*8, num_columns*8, 3))

# Get all row pointers
row_pointers = []
start = 0xb574
for i in np.arange(start, start+num_rows*2, 2):
    ptr = read_row_pointer(i, offset, dump)
    #print("{} --> {}".format(hex(i), hex(ptr)))
    row_pointers.append(ptr)

# Draw scenario
y = 0
for ptr in row_pointers:
    print("*** row ptr={}, y={}".format(hex(ptr), y))
    
    # Decode row at ptr
    x = 0
    i = 0
    while x < 128*8:
        print("\tx={}, y={}".format(x, y))
        tile_idx = dump[ptr + i - offset]
        #tile_idx = 10
        print("\t\tRead tile={}".format(hex(tile_idx)))
        if tile_idx == 0:
            break
        elif tile_idx < 24:
            # repetition of blanks
            print("\t\trepe, tile_idx={}".format(tile_idx))
            x += 8 * tile_idx
        else:
            # Normal tile
            I[y:y+8, x:x+8, :] = tiles[tile_idx]
            x += 8
        i += 1

    y += 8
    print('')

# Fill in the ground
# [ToDo] Where in the ASM code is this done?
I[-16:, :, :] = msx1_colors[2]

# Save map image
cv2.imwrite('gi_scenario.png', I)
