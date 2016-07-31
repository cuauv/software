#!/usr/bin/env python2

import sys
import time
import thread
import shm
from auv_python_helpers.angles import heading_sub_degrees

heading_var = shm.kalman.heading

if len(sys.argv) > 1:
    target_heading = float(sys.argv[1])
else:
    target_heading = 0.0

space_time = None
space_heading = None


def input_thread(event_function, heading):
    global space_time
    global space_heading
    global heading_var
    sys.stdout.write("Press <ENTER> when sub is at %.3f heading\n" % heading) 
    event_function()
    space_time = time.time()
    space_heading = heading_var.get()

thread.start_new_thread(input_thread, (raw_input, target_heading,))
time.sleep(0.001)

initial_diff = heading_sub_degrees(target_heading, heading_var.get())

while 1:
    diff = heading_sub_degrees(target_heading, heading_var.get())
    if initial_diff * diff < 0:
        sensor_time = time.time()
        break

if space_time is None:
    sys.stdout.write("The sub reached the target heading before you pressed enter!\n")
    while space_time is None:
        pass

sys.stdout.write("Lag: %f\n" % (sensor_time - space_time))
sys.stdout.write("The sub was at %.3f degrees heading at enter\n" % space_heading)

