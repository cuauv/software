#!/usr/bin/env python2
''' Test script to easily run the Locator from a log file.

Uses default shm.layout.state to get default Buoy positions.
Meant for use with log files of actual mission runs.
'''

import locator
import shm
import pickle
import argparse

parser = argparse.ArgumentParser(description="locator test script")
parser.add_argument('color', type=str, choices=["orange", "yellow", "green"], help="which buoy to track")
args = parser.parse_args()


try:
    state = pickle.loads(shm.layout.state.get())
except:
    print "Invalid mission layout state! Cannot initialize buoy positions."
    print "You might need to start running a mission log."
    exit()


#Gather buoy positions
n_off = state[0]
e_off = state[1]
positions = state[2]

orange,yellow,green = [(positions[2*i,0]-n_off, positions[2*i+1,0]-e_off) for i in range(3,6)]

print "Starting locator, looking for %s buoy" % args.color
#Start
locator = locator.LocatorRunner(args.color,
                                orange, green, yellow,
                                1.0,
                                display=True,
                                locator_var=shm.locator1,
                                settings_var=shm.locator1_settings)

watcher = shm.watchers.watcher()
watcher.watch(shm.kalman)
while True:
    watcher.wait()
    locator.update()
