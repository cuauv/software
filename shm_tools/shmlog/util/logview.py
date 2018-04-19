#!/usr/bin/env python2
from shm_tools.shmlog.parser import LogParser
import argparse
import sys
from datetime import datetime
from time import sleep
import os

'''
Program to display information about a shared variable log file in a human-readable format.
See logview.py --help for details

Jeff Heidel 2012
'''

ap = argparse.ArgumentParser(description='')
ap.add_argument('filename', type=str, help='filename of the shm log to display')
ap.add_argument('-t', action="store_true", help='include variable table in printout')
ap.add_argument('-v', action="store_true", help='include all variable changes in printout')
ap.add_argument('-s', action="store_true", help='include snapshot table in printout')
args = vars(ap.parse_args())

filename = args['filename']

parse = LogParser(filename)

### General Log Information
print "Info:      ", parse.info.strip()
log_start_time = parse.get_starttime()
print "Log starts:", str(log_start_time)
log_end_time = parse.get_endtime()
print "Log ends:  ", str(log_end_time)
print "Duration:  ", str(log_end_time - log_start_time)
print "Contains:   %s variables" % str(len(parse.svars))
size = os.path.getsize(filename)
print "Log size:   %d (%s MB)" % (size, str(round(size / (1024.0**2), 2)))


### Display any log warnings
if len(parse.get_warnings()) > 0:
    print "\n---- Log Warnings ----"
    print '\n'.join(parse.get_warnings())

### Display variable table
if args["t"]:
    print "\n---- Variable Table ----"
    for i, (s, vstr, vtyp) in parse.svars.iteritems():
        print "%i: %s (%s)" % (i+1, vstr, vtyp.__name__)

### Parse entire log / all variable changes
if args["v"]:
    print "\n---- Variable Changes ----"
    parse.verbose = True
    while not parse.finished_parsing():
        parse.parse_one_slice()

### Display snapshot table
if args["s"]:
    print "\n---- Snapshot Table ----"
    for (snap_t, pos) in parse.snapshot_table:
        print "Snapshot %s at file position %s" % (str(snap_t), str(hex(pos)))



