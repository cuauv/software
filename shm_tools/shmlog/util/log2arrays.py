#!/usr/bin/env python2
from shm_tools.shmlog.parser import LogParser
import pickle
import numpy
import argparse
import os
import sys
import datetime
import time
from time import sleep
from threading import Thread
import libshm.parse

'''
Program to extract entries from a shared memory log and generate a pickle of a dictionary of numpy arrays of the data.
See log2arrays.py --help for details

Stolen from:
Jeff Heidel 2012

Modified by:
Tom Brooks 2013
'''

ap = argparse.ArgumentParser(description='')
ap.add_argument('variables', type=str, nargs='+', help='list of variables to include in this pickle file (must match logfile strings, i.e. depth.depth) *OR* the filename of a filter config file containing the variables you wish to output')
ap.add_argument('-o', dest='OUTPUT_FILENAME', type=str, help='output pickle filename (required)', required=True)
ap.add_argument('-i', dest='INPUT_FILENAME', type=str, help='input shm log filename (required)', required=True)
ap.add_argument('--wrt', dest='WRT_VARIABLE', type=str, default="", help='"with respect to", a shared variable. Every time this variable changes, a new entry in the arrays will be logged. Time will not be logged.')
ap.add_argument('--freq', dest='freq', type=float, default=0, help="frequency at which to record entires. If zero, then entries are recorded whenever any value updates.")
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

cur_time = log_start_time
interrupted = False

def timediff(t1, t2):
    z = (t2 - t1)
    return z.seconds + z.microseconds/1e6

print "Generating pickle file %s from %s..." % (args['OUTPUT_FILENAME'], args['INPUT_FILENAME'])

if args['freq'] > 0:
    print "Recording at a rate of %s Hz"%args['freq']
print "Including variables: " + ', '.join(ddict.keys())

data = dict( [(key, []) for key in ddict.keys()] )
data['time'] = []

initial_values_set = False
def parse_all():
    updated = False
    start = time.mktime(log_start_time.timetuple())
    while not parse.finished_parsing() and not interrupted:
        global cur_time, ddict, initial_values_set
        changetime, changelist = parse.parse_one_slice()
        #Set all variables in this slice
        for (svar, varstr, vtype, val) in changelist:
            if varstr in des:
                ddict[varstr] = val     
                
                if args['freq'] == 0:
                    #check if all initial values are set before writing to csv
                    if not initial_values_set:
                        ivaldict[varstr] = True 
                        initial_values_set = (False not in ivaldict.values())
                        if initial_values_set:
                            updated = True
                    else:
                        updated = True
        if args['freq'] and changetime > datetime.timedelta(seconds = 1./args['freq']) + cur_time:
            cur_time = changetime
            updated = True

        if updated:
            for k in ddict.keys():
                data[k].append(ddict[k])
            data['time'].append(time.mktime(changetime.timetuple())-start + changetime.microsecond / 1e6)


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

if interrupted:
    print "\nFile generation interrupted."
else:
    write_progress(100)
    out = open(outputfilename, 'w')

    # Convert data to numpy arrays
    outdata = dict([(k,numpy.array(x)) for k,x in data.items()])

    pickle.dump(outdata, out)

    print "\nFile generation complete."
