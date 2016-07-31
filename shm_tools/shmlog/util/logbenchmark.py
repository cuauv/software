#!/usr/bin/env python2
from shm_tools.shmlog.parser import LogParser
import argparse
import sys
from datetime import datetime
from time import sleep, mktime, time
import os
import struct
import sys

'''
Utility for benchmarking log file access
Jeff Heidel 2013
'''

GROUP = 0xFFFF
END_STBL = 0xFFFFFFFFFFFFFFFF

ap = argparse.ArgumentParser(description='Benchmark a log file playback.')
ap.add_argument('log file', type=str, help='filename of the log file')
args = vars(ap.parse_args())

filename = args['log file']

try:
    login = open(filename, 'r')
except IOError:
    print "Input filename " + filename + " not found."
    sys.exit(0)

st = time()

try:
    parse = LogParser(filename, parse_file_end=False)
except:
    print "There was a problem with the log header." 
    sys.exit(0)

et = time() - st
print "Log header parse took %.2f ms" % (et * 1e3)

#Starting log benchmark

st = time()

total_time = 0
all_type_times = {}
slices = 0

#Continue parsing and copying until an error occurs!
while True:
    logslice = parse.parse_one_slice(benchmark=True)
    if logslice is None:
        break
    (dt, vc, parse_time, type_times) = logslice

    total_time += parse_time

    for k in type_times.keys():
        if k not in all_type_times:
            all_type_times[k] = type_times[k]
        else:
            all_type_times[k] = all_type_times[k] + type_times[k]

    slices += 1

et = time() - st
print "Complete log parse took %.2f sec" % et
print "Parsed %d slices" % slices
print "%15s  %11s" % ("Var Type", "Time")
print "-"*28
for (k,v) in zip(all_type_times.keys(), all_type_times.values()):
    print "%15s: %7.2f sec" % (k.__name__, v)
print "-"*28
print "Log benchmark complete"
