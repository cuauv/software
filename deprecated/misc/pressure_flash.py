#!/usr/bin/env python2

import shm
from auval.LED import LEDS
from time import sleep
import sys

PRESSURE_NOMINAL = 11.8
PRESSURE_CRITICAL_HIGH = 12.2
PRESSURE_CRITICAL_LOW = 11.2

try:
    while True:
        ip = shm.pressure.internal.get()
        sys.stdout.write("Pressure: " + str(ip))
        sys.stdout.write("\r")
        sys.stdout.flush()
        sleep(0.1)

        if ip >= PRESSURE_CRITICAL_HIGH:
            #Pressure is too high!
            LEDS["red"].set(True)
            LEDS["green"].set(False)
            LEDS["blue"].set(False)
            continue
        if ip <= PRESSURE_CRITICAL_LOW:
            #Pressure is too low!
            LEDS["red"].set(True)
            LEDS["green"].set(True)
            LEDS["blue"].set(False)
            continue
        if ip <= PRESSURE_NOMINAL:
            #Pressure is good!
            LEDS["red"].set(False)
            LEDS["green"].set(True)
            LEDS["blue"].set(False)
            continue
        #Pressure is below critical; but above nominal value
        LEDS["red"].set(False)
        LEDS["green"].set(False)
        LEDS["blue"].set(True)
except KeyboardInterrupt:
    LEDS["red"].set(False)
    LEDS["green"].set(False)
    LEDS["blue"].set(False)





    


