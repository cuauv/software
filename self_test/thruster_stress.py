#!/usr/bin/env python3

import sys

from random import randint
from time import sleep

import shm

from control.thrusters import thrusters

if __name__ == "__main__":
    print("== Random thruster value stress test utility ==")

    if shm.switches.hard_kill.get():
        print("Hard kill appears to be set. Can't do anything. Sorry!")
        sys.exit(0)

    if shm.switches.soft_kill.get():
        c = input("Soft kill appears to be set. Do you want me to override? [y/N] ")
        if not "y" in c.lower():
            print("Ok. Bye!")
            sys.exit(0)
        soft_kill.set(0)

    print("Available thrusters:")
    for i, t in enumerate(thrusters):
        print(" %d) %s" % (i, t.name))
    print("Please input the number of thruster # to test:")
    n = input(" > ")
    try:
        n = int(n)
        if n < 0 or n >= len(thrusters):
            raise Exception
    except:
        print("Invalid input.")
        sys.exit(1)

    t = thrusters[n]

    print("Now stressing %s" % t.name)
    try:
        while True:
            t.set(randint(-255,255))
            sleep(0.25)
    except KeyboardInterrupt:
        print("Interrupt! Stopping thruster. Bye!")
        t.set(0)
