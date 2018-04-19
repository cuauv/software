#!/usr/bin/env python3
import sys
import argparse
from time import sleep

from control.thrusters import all_thrusters
from control.util import zero_motors
from shm import switches

DEFAULT_SPEED = 30

# Create dictionary of thruster names to thruster
thrusters = {}
for t in all_thrusters:
    thrusters[t.name] = t

def check_soft_kill(log=sys.stdout):
    if switches.soft_kill.get():
        log.write("Vehicle is soft-killed, unkill before testing motors\n\n")
        return False
    return True

def test_thruster(name, speed=DEFAULT_SPEED, log=sys.stdout):
    t = thrusters[name]
    log.write("spinning %s to %d...\n" % (name, speed))
    log.flush()
    t.set(speed)
    sleep(3)
    t.set(0)
    log.write("\tdone\n")
    log.flush()

def thruster_name(name):
    if name not in thrusters:
        raise argparse.ArgumentTypeError("Thruster name not in {}".format(list(thrusters)))
    return name

def pwm_speed(speed):
    speed = int(speed)
    if 0 <= speed and speed <= 255:
        return speed
    else:
        raise argparse.ArgumentTypeError("Thruster speed must be [0,255]")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('thruster_name', help='Name of thruster to spin', type=thruster_name)
    parser.add_argument('speed', help='PWM speed of thruster', nargs='?', default=DEFAULT_SPEED, type=pwm_speed)
    args = parser.parse_args()

    try:
        zero_motors()

        if switches.soft_kill.get():
            response = input("Softkill is enabled, would you like to override it? [yN] ")
            if response == 'y':
                print("Disabling softkill...")
                switches.soft_kill.set(0)
            else:
                if response in ('', 'n'):
                    print("Softkill is enabled. Exiting.")
                    sys.exit()
                else:
                    print("Invalid response.")
                    sys.exit(1)

        test_thruster(args.thruster_name, args.speed)

    except Exception as e:
        zero_motors()
        print("Exception caught, quitting gracefully")
