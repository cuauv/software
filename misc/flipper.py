#!/usr/bin/env python3

import time
from shm.actuator_desires import trigger_11 as a, trigger_01 as b

status = True

# Note: This script is out of date. Use 'auv-mr actuate.FireGreen' instead.

def switch():
    global status
    a.set(True if status else False)
    b.set(False if status else True)
    status = not status

def fire():
    print('Firing')
    switch()
    time.sleep(0.500)
    switch()

if __name__ == '__main__':
    fire()
