#!/usr/bin/env python3

import math

from mission.framework.task import Task
from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, Retry, Conditional, While, Either
from mission.framework.movement import Heading, RelativeToInitialHeading, VelocityX, VelocityY, Depth, RelativeToCurrentHeading, RelativeToCurrentDepth
from mission.framework.primitive import Log, NoOp, Zero, Succeed, Fail, FunctionTask
from mission.framework.helpers import ConsistencyCheck, call_if_function

import shm

FLIP = True

last_sub_heading = 0
last_pinger_heading = 0

def shm_heading():
    '''
    SHM heading output by hydrophones in the range [-pi,pi].
    If FLIP, then phase shift by pi radians.
    '''

    global last_sub_heading, last_pinger_heading

    h = shm.hydrophones_results_track.tracked_ping_heading_radians.get()
    print('raw: ' + str(h))
    if FLIP:
        h += math.pi
        if h > math.pi:
            h -= math.pi * 2

    print('flipped: ' + str(h))
    if h != last_pinger_heading:
        last_sub_heading = math.radians(shm.kalman.heading.get())
        last_pinger_heading = h

    return h + last_sub_heading

def get_desired_heading():
    h = shm_heading()
    if h < 0:
        h += math.pi * 2
    return math.degrees(h)

def get_desired_vel_x():
    h = shm_heading()
    if -math.pi/4 < h < math.pi/4:
        return math.cos(h) * 0.3 # maybe change this function later?
    else:
        return 0

track = Sequential(
    Depth(1.0),
    While(
        lambda: Sequential(
            Heading(get_desired_heading),
            VelocityX(get_desired_vel_x),
        ),
        lambda: True
    )
)
