#!/usr/bin/env python2
from shm_tools.shmlog.parser import LogParser
import argparse
import os
import sys
from datetime import datetime
from time import sleep
from threading import Thread
import csv
import libshm.parse

'''
Program to extract entries from a shared memory log and generate a csv formatted file.
See log2csv.py --help for details

Jeff Heidel 2012
'''

ap = argparse.ArgumentParser(description='')
ap.add_argument('variables', type=str, nargs='+', help='list of variables to include in this csv file (must match logfile strings, i.e. depth.depth) *OR* the filename of a filter config file containing the variables you wish to output')
ap.add_argument('-o', dest='OUTPUT_FILENAME', type=str, help='output csv filename (required)', required=True)
ap.add_argument('-i', dest='INPUT_FILENAME', type=str, help='input shm log filename (required)', required=True)
ap.add_argument('--wrt', dest='WRT_VARIABLE', type=str, default="", help='"with respect to", a shared variable. Every time this variable changes, a new entry in the CSV file will be logged. Time will not be logged.')
args = vars(ap.parse_args())

if len(args['variables']) == 1 and os.path.isfile(args['variables'][0]):
    #input was filename of a filter file; use this
    des = []
    gps = libshm.parse.parse(args['variables'][0])
    for g in gps:
        des += map(lambda x : g['groupname'] + "." + x, g['vars'].keys())
else:
    #input was a list of shared variables; use these
    des = args['variables']

wrt_mode = (len(args['WRT_VARIABLE']) > 0)
wrt_prev = None

ddict = dict(map(lambda x : (x,0), des)) #output variable dictionary
if wrt_mode:
    ddict[args['WRT_VARIABLE']] = 0
    des += [args['WRT_VARIABLE']]

ivaldict = dict(map(lambda x : (x,False), des)) #initial value set?

outputfilename = args['OUTPUT_FILENAME']
filename = args['INPUT_FILENAME']

parse = LogParser(filename)

log_start_time = parse.get_starttime()
log_end_time = parse.get_endtime()
local_start_time = datetime.now()

cur_time = log_start_time
interrupted = False

def timediff(t1, t2):
    z = (t2 - t1)
    return z.seconds + z.microseconds/1e6

print "Generating CSV file %s from %s..." % (args['OUTPUT_FILENAME'], args['INPUT_FILENAME'])

print "Including variables: " + ', '.join(ddict.keys())

cw = csv.writer(open(outputfilename, 'wb'))

if wrt_mode: #write header
    print "Log with respect to", args['WRT_VARIABLE']
    cw.writerow(ddict.keys())
else:
    cw.writerow(["timestamp"] + ddict.keys())
    
initial_values_set = False

#Write one row of the log
def new_row(timestamp):
    global wrt_prev
    if wrt_mode:
        newval = ddict[args['WRT_VARIABLE']]
        if newval != wrt_prev:
            wrt_prev = newval 
            cw.writerow(ddict.values()) 
    else:
        cw.writerow([str(timestamp)] + ddict.values()) 

def parse_all():
    while not parse.finished_parsing() and not interrupted:
        global cur_time, ddict, initial_values_set
        changetime, changelist = parse.parse_one_slice()
        cur_time = changetime
        updated = False
        #Set all variables in this slice
        for (svar, varstr, vtype, val) in changelist:
            if varstr in des:
                ddict[varstr] = val     
                
                #check if all initial values are set before writing to csv
                if not initial_values_set:
                    ivaldict[varstr] = True 
                    initial_values_set = (False not in ivaldict.values())
                    if initial_values_set:
                        update = True
                else:
                    updated = True

        if updated:
            new_row(changetime)


#Start log parsing thread
t = Thread(target=parse_all)
t.start()

#XXX: This progress bar is actually time dependant, not progress dependant;
#     If logging at a constant speed; should be relatively accurate. Not a priority.
PROGRESS_WIDTH = 50
def write_progress(percent):
        pwf = int(round(PROGRESS_WIDTH * (percent / 100.0)))
        pstr = "[" + pwf*'=' + (PROGRESS_WIDTH-pwf)*' ' + "]"
        sys.stdout.write("\r%s %d%%" % (pstr, percent))
        sys.stdout.flush()

try:
    #Status indication
    total_time_diff = timediff(log_start_time, log_end_time)

    while not parse.finished_parsing():
        current_diff = timediff(log_start_time, cur_time)
        percent = int(round(100.0 * current_diff / total_time_diff))
        write_progress(percent)
        sleep(0.1)

except KeyboardInterrupt:
    interrupted = True


if not initial_values_set:
    #One variable must not be found in the log
    print "\nCSV Generation failed!"
    for k,v in ivaldict.iteritems():
        if not v:
            print "ERROR: %s not found in log file" % k
    sys.exit(0)


if interrupted:
    print "\nCSV generation interrupted."
else:
    write_progress(100)
    print "\nCSV generation complete."
