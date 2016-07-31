#!/usr/bin/env python2
from shm_tools.shmlog.parser import LogParser
from datetime import datetime, timedelta
from time import time, sleep
import sys
import termios
import argparse
from threading import Thread, Event, Lock
import libshm.parse
import shm

'''
Program to play back shared memory log files.
See logplayback.py --help for details

Jeff Heidel 2012
'''

ap = argparse.ArgumentParser(description='Play back a shared memory log file.')
ap.add_argument('filename', type=str, help='filename of the shm log to parse')
ap.add_argument('--filter', type=str, default="", help='filename of the config file   \
        containing variables to play back (filter config files take the same format   \
        as shared memory config files; simply copy vars.conf and remove all unwanted  \
        variables)')
ap.add_argument('--speed', type=float, default=1.0, help='playback speed factor (i.e. 2 is 2x as fast)')
ap.add_argument('--seek', type=float, default=0.0, help='percentage at which to start playback')
ap.add_argument('-y', action="store_true", help='automatically accept any warnings')
args = vars(ap.parse_args())

filename = args['filename']

parse = LogParser(filename)

#Input verification
if args['speed'] < 0:
    print "Invalid speed argument"
    sys.exit(0)
if args['seek'] < 0 or args['seek'] > 100:
    print "Invalid seek argument. Must be 0 - 100."
    sys.exit(0)

#Check for warnings and inform user
if len(parse.get_warnings()) > 0:
    print "Log parsing encountered the following warnings:"
    print '\n'.join(parse.get_warnings())
    print "These variables will be ignored during playback."
    if not args["y"]:
        c = raw_input("Continue? (y/n)")
        if 'y' not in c.lower():
            print "Aborted."
            sys.exit(0)

KEY_PAUSE = " "
KEY_SPEED_UP = "="
KEY_SPEED_DOWN = "-"
KEY_SEEK_FORWARD = "."
KEY_SEEK_REVERSE = ","

log_start_time = parse.get_starttime()
log_end_time = parse.get_endtime()
local_start_time = datetime.now()

speed = args['speed']
cur_time = log_start_time
interrupted = False
running = Event()
running.set()
speed_lock = Lock()
seek_reverse = False
seek_forward = False

def timediff(t1, t2):
    z = (t2 - t1)
    return z.seconds + z.microseconds/1e6

seeking = (args['seek'] != 0)

print "-- Starting playback of log at %sX speed --" % str(speed)

filtervars = []
filtering = (args['filter'] != "")

if filtering:
    gps = libshm.parse.parse(args['filter'])
    for g in gps:
        filtervars += map(lambda x : g['groupname'] + "." + x, g['vars'].keys())
    print "Filter applied; playback of %d / %d variables" % (len(filtervars), len(parse.svars))


print "Start:   %s\nEnd:     %s" % (str(log_start_time), str(log_end_time))

if seeking:
    tdta = timediff(log_start_time, log_end_time)
    frac = tdta * args['seek'] / 100
    seek_time = log_start_time + timedelta(0,frac)
    parse.seek_nearest_snapshot(seek_time) #seek log to nearest snapshot
    print "Seek to:", str(seek_time)

def parse_all():
    last_change_time = log_start_time
    while not parse.finished_parsing() and not interrupted:
        global cur_time, seeking, local_start_time, seek_reverse, seek_forward
        if seek_reverse:
            parse.seek_nearest_snapshot(changetime, prev=True)
            seek_reverse = False
            last_change_time = None

        elif seek_forward:
            parse.seek_nearest_snapshot(changetime, ahead=True)
            seek_forward = False
            last_change_time = None

        parse_start_time = datetime.now()
        changetime, changelist = parse.parse_one_slice()

        with speed_lock:
            current_speed = speed

        if seeking:
            if changetime > seek_time: #seek from snapshot point to desired time
                #Stop seeking
                seeking = False
                dt = timediff(log_start_time, seek_time)
                local_start_time = datetime.now() - \
                                   timedelta(0, dt / current_speed)
        else:
            if last_change_time is not None:
                available_time = timediff(last_change_time, changetime) /\
                                 current_speed
                used_time = timediff(parse_start_time, datetime.now())

                if used_time < available_time:
                    sleep(available_time - used_time)

        cur_time = changetime
        last_change_time = changetime

        #Set all variables in this slice
        for (svar, varstr, vtype, val) in changelist:
            if svar is not None:
                if filtering and varstr not in filtervars:
                    continue
                svar.set(val)

        running.wait()

#Start log playback thread
t = Thread(target=parse_all)
t.start()

PROGRESS_WIDTH = 31
def write_progress(percent, tme):
    pwf = int(round(PROGRESS_WIDTH * (percent / 100.0)))

    pstr = "[" + pwf*'=' + (PROGRESS_WIDTH-pwf)*' ' + "]"

    note = shm.notes.note.get()
    if note != write_progress.last_note:
        sys.stdout.write("\nNote: %s\n" % note)
        write_progress.last_note = note

    if seeking:
        sys.stdout.write("\rSeeking... %d%%" %  percent)
    else:
        sys.stdout.write("\r%s   %s %d%%" % \
                         ("Current: " + str(tme), pstr, percent))

    sys.stdout.flush()

def process_input():
    while 1:
        key = sys.stdin.read(1)
        if key == KEY_PAUSE:
            if running.is_set():
                running.clear()

            else:
                running.set()

        elif key in [KEY_SPEED_UP, KEY_SPEED_DOWN]:
            with speed_lock:
                global speed
                if key == KEY_SPEED_UP and speed < 8.0:
                    speed *= 2.0

                elif key == KEY_SPEED_DOWN and speed > 0.01:
                    speed /= 2.0

            print "\nSpeed is now %f." % speed

        elif key == KEY_SEEK_REVERSE:
            global seek_reverse
            seek_reverse = True

        elif key == KEY_SEEK_FORWARD:
            global seek_forward
            seek_forward = True

write_progress.last_note = shm.notes.note.get()

# Setup terminal for key by key input
old_options = termios.tcgetattr(sys.stdin)
new_options = old_options[:]
new_options[3] &= ~(termios.ECHO | termios.ICANON)
termios.tcsetattr(sys.stdin, termios.TCSANOW, new_options)

try:
    #Status indication
    total_time_diff = timediff(log_start_time, log_end_time)

    input_thread = Thread(target=process_input)
    input_thread.daemon = True
    input_thread.start()

    while not parse.finished_parsing():
        current_diff = timediff(log_start_time, cur_time)
        percent = int(round(100.0 * current_diff / total_time_diff))
        write_progress(percent, cur_time)

        sleep(0.1)

except KeyboardInterrupt:
    interrupted = True
    # Unblock parsing thread
    running.set()

finally:
    # Reset terminal input options to originals no matter what
    termios.tcsetattr(sys.stdin, termios.TCSANOW, old_options)

if interrupted:
    print "\nLog playback interrupted."
else:
    write_progress(100, log_end_time)
    print "\nLog playback complete."

t.join()
