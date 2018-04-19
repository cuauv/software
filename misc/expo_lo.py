#!/usr/bin/env python3
import sys

c_high = 128000000
c_high_low = 96000000
c_low = 60000000

def find_exp(lux):
    print(c_high/lux/4 + c_high_low/lux/4 + c_low/lux/2)

lux = int(sys.argv[1])
find_exp(lux)
