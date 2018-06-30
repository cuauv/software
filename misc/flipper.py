#!/usr/bin/env python3

import time
from shm.actuator_desires import trigger_01 as a, trigger_11 as b

status = True

def switch():
    global status
    a.set(True if status else False)
    b.set(False if status else True)
    status = not status

def fire():
    switch()
    time.sleep(0.500)
    switch()
