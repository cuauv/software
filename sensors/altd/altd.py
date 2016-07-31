#!/usr/bin/env python3

import serial
import shm
import sys

SERIAL_DEVICE = '/dev/ttyUSB_loki_ALT'
BAUD_RATE = 9600

with serial.Serial(SERIAL_DEVICE, BAUD_RATE) as port:
    while True:
        line = port.readline()
        alt = None
        try:
            alt = float(line.strip()[:-1]) # strip 'm' off end
        except ValueError:
            sys.stderr.write('Invalid read: {}'.format(line))
            continue

        shm.altitude.altitude.set(alt)

