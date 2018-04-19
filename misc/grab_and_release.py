#!/usr/bin/env python2

import sys
import signal
import time
from auval.vehicle import shared_vars

grabber_grab = shared_vars["grabber_grab"]
grabber_release = shared_vars["grabber_release"]

def reset():
    grabber_grab.set(0)
    grabber_release.set(0)

def cleanup(signal, frame):
    reset()
    sys.exit(0)

signal.signal(signal.SIGINT, cleanup)
signal.signal(signal.SIGTERM, cleanup)

reset()
while 1:
    raw_input("Enter to grab ")
    grabber_grab.set(1)
    time.sleep(0.3)
    grabber_grab.set(0)

    raw_input("Enter to release ")
    grabber_release.set(1)
    time.sleep(0.3)
    grabber_release.set(0)
    print
