#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

'''
Draw the pelican's travel points

(c) 2024 Miguel Colom
http://mcolom.info
'''

import argparse
import skimage.io as imageio
import skimage.draw as draw
import numpy as np
import sys
        
description = "Draw the pelican's travel points"
epilog = '(C) Miguel Colom, GNU GPL3 license. http://mcolom.info'

parser = argparse.ArgumentParser(description=description, epilog=epilog)
parser.parse_args()
args = parser.parse_args()

# Read all game dump
with open('../a.bin', 'rb') as f:
    dump = f.read()

# Get list the list of points in the pelican's walk
points = {}

i = 0x1000
pos = 0
while i < 0x1233 + 6*10: # +6*10: it seems to be overwritting the BOOGA-BOO patterns
    y = dump[i] * 8
    x = dump[i+1] * 8
    c1, c2, c3, c4 = dump[i+2], dump[i+3], dump[i+4], dump[i+5]
    print(x, y, c1, c2, c3, c4)
    points[pos] = (x, y, c1, c2, c3, c4)
    pos += 1
    i += 6

# Read map image
I = imageio.imread("gi_scenario.png")

# Mark pelican's points
for pos, (x, y, c1, c2, c3, c4) in points.items():
    print(x)
    I[x-2:x+2, y-2:y+2] = (0*255, 1*255, 1*255)
    
    next1 = points[c1]
    next2 = points[c2]
    next3 = points[c3]
    next4 = points[c4]
    
    rr, cc = draw.line(x, y, next1[0], next1[1])
    rr, cc = draw.line(x, y, next2[0], next2[1])
    rr, cc = draw.line(x, y, next3[0], next3[1])
    rr, cc = draw.line(x, y, next4[0], next4[1])
    
    
    I[rr, cc] = (255, 255, 255)
    
# Save marked map
I = imageio.imsave("gi_marked_scenario.png", I)

