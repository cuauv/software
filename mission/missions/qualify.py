import os

import shm

from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent
from mission.framework.movement import RelativeToInitialHeading, Depth
from mission.framework.position import MoveX, MoveY
from mission.framework.timing import Timer
from mission.framework.primitive import Log, VelocityX, VelocityY, Zero, FunctionTask

# MoveX for minisub w/o a DVL
def fake_move_x(d):
    v = 0.5
    if d < 0:
        d *= -1
        v *= -1
    return Sequential(MasterConcurrent(Timer(d / v), VelocityX(v)), Zero())

def fake_move_y(d):
    v = 0.2
    if d < 0:
        d *= -1
        v *= -1
    return Sequential(MasterConcurrent(Timer(d / v), VelocityY(v)), Zero())

def InterMoveX(d):
    return fake_move_x(d)
    #return MoveX(d) if os.environ['CUAUV_VEHICLE'] == 'castor' else fake_move_x(d)

def InterMoveY(d):
    return fake_move_y(d)
    #return MoveY(d) if os.environ['CUAUV_VEHICLE'] == 'castor' else fake_move_y(d)

def startup():
    shm.switches.hard_kill.set(0)
    shm.switches.soft_kill.set(0)

def shutdown():
    shm.switches.soft_kill.set(1)

Qualify = Sequential(
    RelativeToInitialHeading(0),
    FunctionTask(startup),

    Depth(1),
    Timer(2),
    Depth(2),
    Timer(2),
    Depth(2.5),
    Timer(2),

    # Castor
    InterMoveX(8),
    Depth(0.8),
    InterMoveX(7),

    # # Pollux
    # InterMoveX(3),
    # Depth(1.5),
    # InterMoveX(4),

    RelativeToInitialHeading(90),
    InterMoveX(3),
    RelativeToInitialHeading(90),

    # Castor
    InterMoveX(7),
    InterMoveY(1.7),

    # # Pollux
    # InterMoveX(4),
    # InterMoveY(0.5),

    Depth(2.5),
    #RelativeToInitialHeading(-12),
    InterMoveX(8),

    FunctionTask(shutdown),
)
