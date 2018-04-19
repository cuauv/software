#!/usr/bin/env python2
from shm_tools.shmlog.parser import LogParser
import argparse
import sys
from datetime import datetime
from time import sleep, mktime
import os
import struct
import sys

'''
Utility to recover / rebuild incomplete log files.
This can happen if the log file is not properly finalized (i.e. the sub loses power).
It may also be able to partially reconstruct logfiles that have suffered data loss.
See logrebuild.py --help for details

Jeff Heidel 2012
'''

GROUP = 0xFFFF
END_STBL = 0xFFFFFFFFFFFFFFFF

ap = argparse.ArgumentParser(description='')
ap.add_argument('incomplete', type=str, help='filename of the incomplete log file')
ap.add_argument('new', type=str, help='filename of the new log to output')
args = vars(ap.parse_args())

filename = args['incomplete']

try:
    login = open(filename, 'r')
except IOError:
    print "Input filename " + filename + " not found."
    sys.exit(0)

if args['incomplete'] == args['new']:
    print "Your output file cannot be the same as your input file."
    sys.exit(0)

try:
    parse = LogParser(filename, parse_file_end=False)
except:
    print "There was a problem with the log header. This file cannot be rebuilt."
    sys.exit(0)

log_start_time = parse.get_starttime()
print "Log starts at: " + str(log_start_time)

logout = open(args['new'], 'w')

def copy_to(x):
    #copies login to logout up to position x
    while login.tell() != x:
        logout.write(login.read(1))

copy_to(parse.f.tell()) #copy start of the file including header information

snapshot_table = [] 

print "Rebuilding log. This may take a while..."

svarstrs = set(map(lambda x: x[1], parse.svars.values()))

def is_snapshot(vc):
    #Checks if the given variable change snapshot contains all variables    
    vlist = set(map(lambda x: x[1], vc))
    return (svarstrs == vlist)

#Continue parsing and copying until an error occurs!
while True:
    try:
        global last_time, last_pos

        try: #XXX: Hack to bypass a python warning
            last_pos
        except NameError:
            last_pos = parse.f.tell()

        (dt, vc) = parse.parse_one_slice()
        
        #Parse of this slice succeeded, copy!
        copy_to(parse.f.tell())

        if is_snapshot(vc):
            snapshot_table.append((dt, last_pos - 2))

        last_time = dt
        last_pos = parse.f.tell()

    except:
        #This must be the point of failure
        break

print "Log build completed at index", str(hex(parse.f.tell()))


#Finalize variable change table with group seperator
logout.seek(-2,1)
logout.write(struct.pack("=H", GROUP))


def write_posix_time(timestamp):
    #Writes a timestamp to the output file in posix format
    (sec, usec) = (int(mktime(timestamp.timetuple())), timestamp.microsecond)
    logout.write(struct.pack("=ll", sec, usec))

print "Log ends at: " + str(last_time)

print "Finalizing log..."

#Write final timestamp
write_posix_time(last_time)

#Write "end" of snapshot table
logout.write(struct.pack("=Q", END_STBL))
logout.write(struct.pack("=Q", 0))

snapshot_table.reverse() #place these into the file in reverse order

#Snapshot table reconstruct
for (dt, fpos) in snapshot_table:
    logout.write(struct.pack("=Q", fpos))
    write_posix_time(dt)

print "Log rebuilt successfully. Written to", args['new']
