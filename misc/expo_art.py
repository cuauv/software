#!/usr/bin/env python3
import sys

c_high = float(24000)
c_low = float(10000 * 3 / 2) # c_low data point was dim, so multipled by factor of 1.5

def find_exposure(lux):
    exposure = c_high/lux/2 + c_low/lux/2
    print('Recommended Exposure: %s' %(exposure))

lux = float(sys.argv[1])
find_exposure(lux)
