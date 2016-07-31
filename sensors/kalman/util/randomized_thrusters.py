#!/usr/bin/env python2
from time import sleep, time
import random

import shm

thrusters = ['port', 'starboard', 'sway_aft', 'sway_fore', 'aft', 'fore']
pwm_values = [50,75,100,125,150,175,200,225,255,
              -50,-75,-100,-125,-150,-175,-200,-225,-255]

pairs = [(t,v) for t in thrusters for v in pwm_values]
random.shuffle(pairs)

start = time()
for t,v in pairs:
        # Set desired heading to current so we don't flip out
        shm.navigation_desires.heading.set(shm.kalman.heading.get())

        # Enable controller so that we don't go floating off
        print "%0.2f: Resetting..." % (time()-start)
        shm.settings_control.enabled.set(1)
        sleep(1)
        shm.settings_control.enabled.set(0)
        sleep(1)

        # Hold motor value constant for a while
        print "%0.2f: Trying %s at %s..." % (time()-start,t,v)
        shm.motor_desires.__getattribute__(t).set(v)

        sleep(2)

        shm.motor_desires.__getattribute__(t).set(0)

