#!/usr/bin/env python2

import shm

from LUTlinearizer import linearize
#from PolyLinearizer import linearize
from auv_math.quat import Quaternion

heading_input = shm.threedmg.heading
pitch_input = shm.threedmg.pitch
roll_input = shm.threedmg.roll

heading_output = shm.linear_heading.heading

quat_output = [shm.linear_heading.q0, shm.linear_heading.q1, 
               shm.linear_heading.q2, shm.linear_heading.q3]

watcher = shm.watchers.watcher()
watcher.watch(shm.threedmg)
group = shm.linear_heading

while(1):
    watcher.wait()
    linear_heading = linearize(heading_input.get())
    hpr = [linear_heading, pitch_input.get(), roll_input.get()]

    heading_output.set(linear_heading)
    quat = Quaternion(hpr=hpr)
    for i in range(0, 4):
        quat_output[i].set(quat[i])

