#!/usr/bin/env python3
import sys

from time import sleep

from control.thrusters import all_thrusters
from control.util import zero_motors, set_all_motors_from_seq
from shm import switches

test_values = (30, 0)

def check_soft_kill(log=sys.stdout):
    if switches.soft_kill.get():
        log.write("Vehicle is soft-killed, unkill before testing motors\n\n")
        return False
    return True

def test_motor_sweep(log=sys.stdout, speed=1.0):
    if check_soft_kill(log):
        sleep(1.5/speed)

        for motor in all_thrusters:
            for pwm in test_values:
                log.write("spinning %s to %d...\n" % (motor.name, pwm))
                log.flush()
                motor.set(pwm)
                sleep(3/speed)
                motor.set(0)
                log.write("\tdone\n")
                log.flush()
                sleep(1/speed)

def test_motor_spinup(log=sys.stdout, speed=1.0):
    if check_soft_kill(log):
        sleep(1.5/speed)

        for motor in all_thrusters:
            log.write("spinning %s to 127...\n" % motor.name)
            log.flush()
            motor.set(127)
            sleep(1/speed)

        sleep(1.5/speed)
        zero_motors()
        log.write("\tdone\n")
        log.flush()
        sleep(1.5/speed)

def test_motor_floorit(log=sys.stdout, speed=1.0):
    if check_soft_kill(log):
        sleep(1.5/speed)
        log.write("Flooring all motors...\n")
        log.flush()

        set_all_motors_from_seq([t.max_pwm for t in all_thrusters])

        sleep(5/speed)
        zero_motors()
        sleep(1.5/speed)
        log.write("Done\n")
        log.flush()


def test_motor_dockside(log=sys.stdout, speed=1.0):
    log.write("\nBEGINNING DOCKSIDE MOTOR TEST\n\n")
    log.flush()

    # Back when we had thrusters which could be run out of water (i.e. not
    # BlueRobotics thrusters) we used to use the spinup and floorit tests
    # These variables are left as arrays in case we ever want to use those
    # tests again
    tests = [test_motor_sweep]
    names = ["SWEEP"]

    if check_soft_kill(log):
        for test, name in zip(tests, names):
            log.write("\n****** MOTOR TEST: %s ******\n\n" % name)
            log.flush()
            test(speed=speed)
            sleep(1)

    log.write("\nFINISHED DOCKSIDE MOTOR TEST\n\n")
    log.flush()

if __name__ == "__main__":
    prev_soft_kill = switches.soft_kill.get()

    try:
        zero_motors()

        if prev_soft_kill:
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

        input("This will begin a thruster test. Press ENTER to continue or Ctrl-C to exit.")

        test_motor_dockside(speed=2.4 * 3)

        print("Thruster test completed.")
    except:
        zero_motors()
        print("Exception caught, quitting gracefully")

    finally:
        switches.soft_kill.set(prev_soft_kill)
