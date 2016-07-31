#!/usr/bin/env python3

import sys
import time

from misc.actuator import fire_actuators, set_names_off
from misc.utils import register_exit_signals

class ActuatorTest(object):
    actuators = ["torpedo_top", "torpedo_bottom", "left_marker", "right_marker"]

    @classmethod
    def run_test(cls):
        for act in cls.actuators:
            print("Firing %s..." % act)
            fire_actuators([act])
            time.sleep(0.8)

        cls.cleanup()

    @classmethod
    def cleanup(cls):
        set_names_off(cls.actuators)

if __name__ == "__main__":
    def interrupt(signal, frame):
        ActuatorTest.cleanup()
        sys.exit(0)

    register_exit_signals(interrupt)

    print("ATTENTION: This will fire both torpedos and markers!")
    print("\tBe sure that grabbers have not been reassigned")
    print("\tPress ENTER to continue or Control-C to quit")
    input()

    ActuatorTest.run_test()
