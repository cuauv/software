#!/usr/bin/env python2
'''
measure_thrusters.py attempts to determine the impulse response
of each thruster for system identification.
That is, suppose:
T(t) = thrust value at time t
R(t) = angular rate at time t
R(t) depends upon T(tao) for tao < t
If we assume that R is linear in T and time-invariant then
R(t) = \int_0^\infty g(k) T(t-k) dk for some function g(k), k > 0
or, discretized,
R(t) = \sum_{k=0}^\infty g(k) T(t-k)
We want to determine g(k) so that we predict R given past T values
To do this we measure R for fixed, simple inputs of T
In particular, we use a step function T=0 before t=0
and T=constant afterwards
(We stop running the thruster at some point, but this should be
long enough later that the vehicle has 'forgotten' about the
initial T=0 state.)

In this simple case then R(t) = \sum_{k=0}^t g(k) T
where T is the constant value of T(t>0)
which then gives that: g(t) = R'(t)/T which is easy to determine!
'''
from time import sleep, time

from numpy import array

import shm

# Which axis/variable we are trying to measure with
measured_vars = ['shm.imu.yaw_vel',
                 'shm.kalman.heading_rate',
                 'shm.kalman.heading',
                 'shm.dvl.velocity_x',
                 'shm.dvl.velocity_y',
                 #'shm.kalman.vely',
                 #'shm.kalman.velx',
                 'shm.kalman.depth',
                 'shm.kalman.roll',
                 'shm.kalman.pitch',
                 'shm.kalman.accelx',
                 'shm.kalman.accely',
                 ]

thrusters = ['port', 'starboard', 'sway_aft', 'sway_fore', 'aft', 'fore']
pwm_values = [50,75,100,125,150,175,200,225,255,
              -50,-75,-100,-125,-150,-175,-200,-225,-255]


TRIAL_LENGTH = 6.0 #Seconds
RESET_TIME = 4.0
CALM_TIME = 4.0
MEASUREMENT_PERIOD = 0.02

data = dict( (mv,dict( (t,dict((v,[]) for v in pwm_values)) for t in thrusters )) for mv in measured_vars)

if __name__ == '__main__':
    start = time()
    runs = []
    for t in thrusters:
        for v in pwm_values:
            done = False
            while not done:
                try:
                    shm.motor_desires.__getattribute__(t).set(0)

                    # Set desired heading to current so we don't flip out
                    shm.navigation_desires.heading.set(shm.kalman.heading.get())

                    # Enable controller so that we don't go floating off
                    print "%0.2f: Resetting..." % (time()-start)
                    shm.settings_control.enabled.set(1)
                    sleep(RESET_TIME)

                    # Let sub drift with no input
                    shm.settings_control.enabled.set(0)
                    sleep(CALM_TIME)

                    # Hold motor value constant for a while
                    print "%0.2f: Trying %s at %s..." % (time()-start,t,v)
                    shm.motor_desires.__getattribute__(t).set(v)

                    # Measure data
                    begin = time()
                    count = 0
                    while time() < begin+TRIAL_LENGTH:
                        while time()-begin > count*MEASUREMENT_PERIOD:
                            for mv in measured_vars:
                                data[mv][t][v].append(eval(mv).get())
                            count += 1
                        sleep(0)

                    shm.motor_desires.__getattribute__(t).set(0)

                    # Set desired heading to current so we don't flip out
                    shm.navigation_desires.heading.set(shm.kalman.heading.get())

                    # Enable controller so that we don't go floating off
                    print "%0.2f: Resetting..." % (time()-start)
                    shm.settings_control.enabled.set(1)
                    sleep(RESET_TIME)

                    done = True
                except KeyboardInterrupt:
                    # Clear log
                    data[t][v] = []

                    # Set desired heading to current so we don't flip out
                    shm.navigation_desires.heading.set(shm.kalman.heading.get())

                    # Enable controller so that we don't go floating off
                    shm.settings_control.enabled.set(1)

                    print "Interrupted!"
                    print "Stopping measurements so that the sub may be driven"
                    raw_input("[ENTER] when done (or CTRL-C again to kill)")

    shm.settings_control.enabled.set(1)
    print "Done recording!"
    print "Writing output to 'thrusts.pickle'"


    # Convert recorded data to arrays
    data = dict( (t,dict( (v,array(x)) for v,x in d.items())) for t,d in data.items() )

    # Write output:
    import pickle
    pickle.dump(data, open("thrusts.pickle","w"))
