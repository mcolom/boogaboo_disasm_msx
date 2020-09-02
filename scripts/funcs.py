#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

'''
BOOGA-BOO functions

(c) 2019 Miguel Colom
http://mcolom.info
'''

import argparse
import cv2
import numpy as np
import sys

def binary(number):
    '''
    Return a 8-character string representing the given unsigned byte
    '''
    res = ''
    for i in range(8):
        b = 7 - i
        res += '0' if number & (2**b) == 0 else '1'
    return res

def get_msx1_colors():
    '''
    Return the palette of MSX1 colors (T7937A)
    TOSHIBA_PALETTE from openMSX sources, in src/video/VDP.cc
    '''
    msx1_colors = {0:(  0,  0,  0), 1:( 0,  0,  0), 2:( 102, 204, 102), 3:( 136, 238, 136),
         4:( 68, 68, 221), 5:( 119, 119, 255), 6:( 187, 85, 85), 7:( 119, 221, 221),
         8:( 221, 102,102), 9:( 255, 119, 119),10:(204, 204, 85), 11:(238, 238, 136),
         12:( 85, 170, 85), 13:(187, 85, 187), 14:(204, 204, 204), 15:(238, 238, 238)}
    '''
    msx1_colors = {0: (0,0,0), 1: (0,0,0), 2: (62,184,73), 3: (116,208,125),
              4: (89,85,224), 5: (128,118,241), 6: (185,94,81),
              7: (101,219,239), 8: (219,101,89), 9: (255,137,125),
              10: (204,195,94), 11: (222,208,135), 12: (58,162,65),
              13: (183,102,181), 14: (204,204,204), 15: (255,255,255)}
    '''
    return msx1_colors

def extract_color_table(dump, offset, msx_colors):
    '''
    Extract the game's color table
    '''
    CT = []
    for i in range(0xB674 - offset, 0xB674 - offset + 32+1):
        back_fore = dump[i]
        foreground = (back_fore & 0xf0) >> 4
        background = back_fore &  0x0f
        CT.append((msx_colors[background], msx_colors[foreground]))
    return CT
