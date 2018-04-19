#!/usr/bin/env python2

import shm
from time import sleep, time
import sys

def calibrate_depth():
    st = time()
    while True:
        if shm.dvl.tick.get() > 1:
            break
        print "Waiting for DVL to come online..."
        if time() - st > 30.0:
            print "DVL not coming online. Aborting calibration."
            return False
        sleep(1)

    in_water = ((shm.dvl.low_amp_1.get() != 1 or shm.dvl.low_amp_2.get() != 1 or shm.dvl.low_amp_3.get() != 1 or shm.dvl.low_amp_4.get() != 1) and shm.dvl.tick.get() != 0)

    if in_water:
        print "DVL indicates beams are unblocked."
        print "Depth cannot be calibrated while in water!"
        return False

    # Do calibrate
    shm.depth.calibrate.set(1)
    sleep(0.5)
    shm.depth.calibrate.set(0)
    print "Depth calibrated!"
    return True




