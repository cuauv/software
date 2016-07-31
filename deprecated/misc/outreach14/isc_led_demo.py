#!/usr/bin/env python

# isc_led_demo.py
# Flashes LEDs
# Written for Ithaca Sciencenter Outreach, March 2014
# Last Updated 1 March 2014
# Questions? Email cwg46@cornell.edu

import shm
from time import sleep
import random

TDEC = 3

if __name__ == '__main__':
    while True:
        n = random.randint(1, 6)
        if n == 1: shm.led.port1.set(True)
        elif n == 2: shm.led.port2.set(True)
        elif n == 3: shm.led.port3.set(True)
        elif n == 4: shm.led.port4.set(True)
        elif n == 5: shm.led.port5.set(True)
        elif n == 6: shm.led.port6.set(True)
        sleep(TDEC)
        #TDEC *= random.random()
        TDEC *= 0.8
        shm.led.port1.set(False)
        shm.led.port2.set(False)
        shm.led.port3.set(False)
        shm.led.port4.set(False)
        shm.led.port5.set(False)
        shm.led.port6.set(False)
