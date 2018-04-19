#!/usr/bin/env python3

import random
import threading

from time import sleep

from control.thrusters import thrusters
from control.util import zero_motors
from shm import motor_desires

test_values = (-255,-190, -140, -100, 60, 0, 60, 100, 140, 190, 255)
#test_values = range(-128,128)
#test_values = (-255, -127, 0, 127, 255)
#test_values = (-255, 255)

def manage_thruster(thruster):
    print("Thread to manage thruster {} started!".format(thruster.name))
    while True:
        sleep(random.uniform(1, 2))
        speed = random.choice(test_values)
        thruster.set(speed)
        print("Setting thruster {} to speed {}.".format(thruster.name, speed))

for thruster in thrusters:
    t = threading.Thread(target=manage_thruster, daemon=True, args=(thruster,))
    t.start()

try:
    while True:
        sleep(120)
except:
    zero_motors()
    print("Exception caught, quitting gracefully")
