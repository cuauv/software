#!/usr/bin/python

import shm

ACTS = 14

for act in range(1, ACTS+1):
    shm._eval('actuator_desires.trigger_{0:02d}'.format(act)).set(0)
