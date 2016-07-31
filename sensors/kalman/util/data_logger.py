#!/usr/bin/env python2
''' This file logs relevant heading data so that offline analysis of
kalman filtering may be performed.

The output is to be run with kalman_replay.py'''

import shm

variables = dict(
            hdg_dvl = shm.dvl.heading,
            hdg_dvl_linear = shm.dvl.linear_heading,
            hdg_3dmg = shm.threedmg.heading,
            rate_3dmg = shm.threedmg.heading_rate,
            rate_imu = shm.imu.yaw_vel,

            pitch = shm.threedmg.pitch,
            pitch_rate = shm.threedmg.pitch_rate,
            roll = shm.threedmg.roll,
            roll_rate = shm.threedmg.roll_rate
            )

#Update Rate
dt = 0.02

#Stores a list of readings for each variable
data = dict()
for var in variables.keys():
    data[var] = []

import sys
#Where to store the data
if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    import datetime
    filename = "log." + str(datetime.datetime.now())
outfile = file(filename, "w")

import time
print "Starting to log every", dt, "seconds."
try:
    while(True):
        time.sleep(dt)
        for key, var in variables.items():
            data[key].append( var.get() )
except KeyboardInterrupt:
    print "Keyboard Interrupt detected! Halting logging."
    

    import pickle
    pickle.dump(data, outfile)
    
    print "Output written to file '", filename, "'"
