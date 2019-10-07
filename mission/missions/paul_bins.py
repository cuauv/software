#!/usr/bin/env python3

import math

import shm

from conf.vehicle import VEHICLE

from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, Retry, Conditional, While, Either
from mission.framework.helpers import get_downward_camera_center, ConsistencyCheck
from mission.framework.movement import Depth, Heading, Pitch, VelocityX, VelocityY, RelativeToCurrentHeading, RelativeToInitialHeading, VelocityDepth, RelativeToCurrentDepth, RelativeToInitialDepth
from mission.framework.position import PositionalControl
from mission.framework.primitive import Zero, Log, FunctionTask, Fail, NoOp
from mission.framework.search import SearchFor, VelocityTSearch, SwaySearch, PitchSearch, VelocitySwaySearch
from mission.framework.targeting import DownwardTarget, PIDLoop, HeadingTarget, ForwardTarget
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.framework.jank import TrackMovementY, RestorePosY
from mission.framework.actuators import FireActuator

from mission.constants.region import PATH_1_BEND_RIGHT, PATH_2_BEND_RIGHT

from mission.missions.will_common import Consistent, BigDepth, is_mainsub, FakeMoveX, FakeMoveY
from auv_python_helpers.angles import heading_sub_degrees
import numpy as np

def s(n):
    b = [' '] * 360
    b[180] = '+'
    b[n] = '#'
    return ''.join(b)
def s2(a, b, c):
    bl = [' '] * 360
    bl[180] = '+'
    bl[a] = 'A'
    bl[b] = 'B'
    bl[c] = 'D'
    return ''.join(bl)
def heading_to_vector(h):
    return np.exp([1j * h]).astype(np.complex128).view(np.float64)
def vector_to_heading(v):
    return math.atan2(v[1], v[0])
def hwrap(f):
    def _wrap(*args, **kwargs):
        print('c')
        return f(*args, **kwargs)
    return _wrap
DEADBAND = .06
def is_done(heading_vect, heading, pos, trg, dst, deadband, abs_offset):
    v = (np.float64([heading_vect()]).view(np.complex128) * -dst).view(np.float64)[0] + abs_offset
    pos_var = pos()
    #print(shm.bins_status.cover_x.get(), shm.bins_status.cover_y.get(), v)
    #print(pos_var)
    print(pos_var - v, trg, heading())
    vv = abs(heading_sub_degrees(trg, heading(), math.pi*2)) < math.radians(5) and \
            abs(pos_var[0] - v[0]) < deadband and \
            abs(pos_var[1] - v[1]) < deadband
    #print(vv)
    return vv


#ForwardTarget(point=centerf, target=CAM_CENTER, px=px, py=py, dx=d, dy=d, deadband=(db,db))

def PushLever():
    def TargetLever(py):
        return ForwardTarget(
            point=lambda: (shm.bins_status.lever_x.get(), shm.bins_status.lever_y.get() / (1+shm.bins_status.lever_sz.get() / 50)),
            #target=lambda: (0, .3 * min(100, shm.bins_status.lever_sz.get()) / 100),
            target=lambda: (0, .25 / (1+shm.bins_status.lever_sz.get() / 50)),
            valid=shm.bins_status.lever_visible.get,
            deadband=(DEADBAND, DEADBAND), px=1, py=py, ix=.05, iy=.05
        )
    return Sequential(
        Log("PushLever start"),
        FunctionTask(VelocityX(.2)),
        MasterConcurrent(
            Consistent(lambda: shm.bins_status.lever_sz.get() > 90, count=.5, total=.75, invert=False, result=True),
            While(lambda: TargetLever(1.5), lambda: True),
            While(lambda: FunctionTask(
                    VelocityX(.2 / (1 + 2 * (abs(shm.bins_status.lever_x.get()) + abs(shm.bins_status.lever_y.get()-.25))))
                ), lambda: True)
        ),
        #Log("Higher P"),
        #MasterConcurrent(
        #    Consistent(lambda: shm.bins_status.lever_sz.get() > 100, count=.5, total=.75, invert=False, result=True),
        #    While(lambda: TargetLever(.8), lambda: True),
        #    While(lambda: FunctionTask(
        #            VelocityX(.2 / (1 + 2 * (abs(shm.bins_status.lever_x.get()) + abs(shm.bins_status.lever_y.get()-.15))))
        #        ), lambda: True)
        #),
        #Log("targeting"),
        #TargetLever(),
        Log("zoom zoom"),
        #RelativeToInitialDepth(-.1),
        #Timed(RelativeToCurrentDepth(-2), .7),
        #FunctionTask(RelativeToInitialDepth(0)),
        Concurrent(
            Sequential(
                Timer(1),
                Log("Forwards!"),
                FunctionTask(VelocityX(1)),
                #VelocityDepth(0),
            ),
            Sequential(
                Timed(RelativeToCurrentDepth(-.4), 1),
                RelativeToInitialDepth(0),
            ),
            Timer(4),
        ),
        #Timed(
        #    While(TargetLever, lambda: True),
        #    5
        #),
        Timed(VelocityX(-.8), .5),
        VelocityX(0),
        Log("waiting"),
        Timer(4),
        #RelativeToInitialHeading(0),
        #Timed(VelocityX(-.8), 1),
        #RelativeToInitialHeading(0),
        #FunctionTask(VelocityX(0)),
        #Timer(5),
        #TargetLever()
    )

push_lever = PushLever()

class VisibleCounter(Task):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.init_count = 0
        self.last_count = 0
    def on_first_run(self, var):
        v = var()
        self.init_count = v
        self.last_count = v
    def on_run(self, var):
        self.last_count = var()
    def get_value(self):
        return self.last_count - self.init_count

class LeverWrapper(Task):
    def on_first_run(self, is_bat_getter, wrapped_task):
        is_bat = is_bat_getter()
        get_x = shm.bins_status.bat_x.get if is_bat else shm.bins_status.wolf_x.get
        get_y = shm.bins_status.bat_y.get if is_bat else shm.bins_status.wolf_y.get
        pointing_wrong_way = -1 if is_bat ^ (get_x() < 0) else 1
        x_amt = .8
        y_amt = 1.5 if is_bat else -1.5
        turn_amt = -90 if is_bat else 90
        r_x_amt = x_amt * pointing_wrong_way
        r_y_amt = y_amt * pointing_wrong_way
        r_turn_amt = turn_amt * pointing_wrong_way
        ret_y_amt = -.8 if is_bat else .8
        self.use_task(Sequential(
            FakeMoveY(r_y_amt, .3),
            Timer(.2),
            FakeMoveX(r_x_amt, .3),
            Timer(.2),
            RelativeToInitialHeading(r_turn_amt),
            wrapped_task,
            Log("Moving back"),
            # might have to deal with heaeding here
            FakeMoveX(-.8, .3),
            Log("moving Y {}".format(ret_y_amt)),
            FakeMoveY(ret_y_amt, .3), # intentionally swapped
            Timer(.5),
            #Log("moving X {}".format(y_amt * math.sin(math.radians(turn_amt)))),
            #FakeMoveX(y_amt * math.sin(math.radians(turn_amt)), .3), # intentionally swapped
            FakeMoveX(1.5, .3),
        ))


def PipeAlign(get_center, heading_vect, heading, get_visible, trg, dst, deadband, angle_range=math.pi*2, abs_offset=(0, 0)): return Sequential(
    Log("PipeAlign start"),
    MasterConcurrent(
        Consistent(lambda: is_done(heading_vect, heading, get_center, trg, dst, deadband, abs_offset), count=.5, total=.75, invert=False, result=True),
        While(
            lambda: Sequential(
                #Log("attempt asdasd"),
                Concurrent(
                    DownwardTarget(
                        lambda: get_center(),
                        target=lambda: (np.float64([heading_vect()]).view(np.complex128) * -dst).view(np.float64)[0] + abs_offset,
                        deadband=(deadband, deadband), px=2, py=2, dx=.5, dy=.5#, ix=.15, iy=.15
                    ),
                    While(
                        lambda: Sequential(
                            FunctionTask(lambda: shm.navigation_desires.heading.set((shm.kalman.heading.get()-.95*math.degrees(heading_sub_degrees(trg, heading(), angle_range))) % 360) if get_visible() else None),
                            #Log('{:03d} '.format(int(shm.navigation_desires.heading.get())) + s2(int(math.degrees(trg)), int(math.degrees(heading())), int(shm.desires.heading.get()))),
                            Timer(.05)
                            ),
                        lambda: abs(heading_sub_degrees(trg, heading(), math.pi*2)) > math.radians(5)
                    )#, count=6, total=8, invert=False, result=True),
                )
            ),
            lambda: True #lambda: abs(heading_sub_degrees(trg, heading(), math.pi*2)) > math.radians(5) or abs(shm.path_results.center_x.get()) > .08 or abs(shm.path_results.center_y.get()) > .08
        ),

            #, count=3, total=4, invert=False, result=True),
        #While(lambda: Log("V: {} h: {} d: {} x: {} y: {} c: {}".format(shm.path_results.num_lines.get(), heading(), math.degrees(heading_sub_degrees(trg, heading(), math.pi*2)), shm.path_results.center_x.get(), shm.path_results.center_y.get(), heading_to_vector(heading()) * dst)), lambda: True),
        #While(lambda: Log(s(int(math.degrees(heading_sub_degrees(trg, heading(), math.pi*2))) + 180)), lambda: True),
         #While(lambda: Log(s2(int(math.degrees(trg)), int(math.degrees(heading()))) if shm.path_results.num_lines.get() == 2 else 'X'), lambda: True),
    ),
    Log("Centered on Pipe in PipeAlign!"),
    #FunctionTask(lambda: shm.navigation_desires.heading.set(-180/math.pi*heading()+shm.kalman.heading.get()))
)

def t180(a): return (a + math.pi) % (2 * math.pi)

find_bins = lambda: Sequential(
    Depth(.1),
    FunctionTask(VelocityX(.5)),
    While(lambda: NoOp(),
          lambda: not shm.bins_status.cover_visible.get()
    ),
    VelocityX(0),
)

def get_cover_vect():
    return np.float32([shm.bins_status.cover_maj_x.get(), shm.bins_status.cover_maj_y.get()])
def get_cover_center():
    return shm.bins_status.cover_x.get(), shm.bins_status.cover_y.get()
def get_wolf_center():
    return (shm.bins_status.wolf_x.get(), shm.bins_status.wolf_y.get())
def get_bat_center():
    return (shm.bins_status.bat_x.get(), shm.bins_status.bat_y.get())

center_cover = lambda: PipeAlign(get_cover_center, get_cover_vect, lambda: vector_to_heading(get_cover_vect()), shm.bins_status.cover_visible.get, 0, 0, DEADBAND, angle_range=math.pi, abs_offset=(0,.15))
center_wolf = lambda: PipeAlign(get_wolf_center, lambda: heading_to_vector(shm.bins_status.wolf_angle.get()), shm.bins_status.wolf_angle.get, shm.bins_status.wolf_visible.get, math.pi/2, -.2j, DEADBAND)
center_bat = lambda: PipeAlign(get_bat_center, lambda: heading_to_vector(shm.bins_status.bat_angle.get()), shm.bins_status.bat_angle.get, shm.bins_status.bat_visible.get, math.pi/2, -.02-.15j, DEADBAND)

cb = center_bat()
full_wolf = lambda: Sequential(
        Log("Targeting wolf for drop"),
        Depth(1),
        center_wolf(),
        Depth(2.3),
        center_wolf(),
        Timer(.4),
        FireActuator('right_marker', 0.25),
        FireActuator('left_marker', 0.25),
        # do dropper stuff
)

full_bat = lambda: Sequential(
        Log("Targeting bat for drop"),
        Depth(1),
        center_bat(),
        Depth(2.3),
        center_bat(),
        Timer(.4),
        FireActuator('right_marker', 0.25),
        FireActuator('left_marker', 0.25),
        # do dropper stuff
)

#class er(Task):
#    def on_first_run(self, is_bat_getter, wrapped_task):
def DoComparison(counter_a, counter_b):
    return \
        While(
            lambda: Timed(Concurrent(
                While(center_cover, lambda: True),
                counter_a,
                counter_b,
            ), 1.5),
            lambda: counter_a.get_value() + counter_b.get_value() < 10
        )

def decide(target_first, counter_a, counter_b):
    if target_first:
        if counter_a > 0: return True
    else:
        if counter_b > 0: return False
    return counter_a > counter_b


def DecideAndPush():
    counter_wolf = VisibleCounter(shm.bins_status.wolf_visible_frames.get)
    counter_bat = VisibleCounter(shm.bins_status.bat_visible_frames.get)
    is_bat_getter = lambda: counter_wolf.get_value() < counter_bat.get_value()
    counter_wolf_2 = VisibleCounter(shm.bins_status.wolf_visible_frames.get)
    counter_bat_2 = VisibleCounter(shm.bins_status.bat_visible_frames.get)
    return Sequential(
        Depth(0.5),
        center_cover(),
        DoComparison(counter_wolf, counter_bat),
        FunctionTask(lambda: Log('wolf: {}'.format(counter_wolf.get_value()))()),
        FunctionTask(lambda: Log('bat: {}'.format(counter_bat.get_value()))()),
        
        #MasterConcurrent(
            Depth(2.5),
        #    While(center_cover, lambda: True)
        #),
        LeverWrapper(is_bat_getter=is_bat_getter, wrapped_task= 
            PushLever()
        ),
        #Conditional(Timed(FunctionTask(lambda: counter_wolf.get_value() > counter_bat.get_value()), .1),
        #    on_success=Log("wolf visible"),
        #    on_fail=Log("bat visible"),
        #),
        #Log('nani')
        Depth(.5),
        center_cover(),
        DoComparison(counter_wolf_2, counter_bat_2),
        FunctionTask(lambda: Log('wolf: {}'.format(counter_wolf_2.get_value()))()),
        FunctionTask(lambda: Log('bat: {}'.format(counter_bat_2.get_value()))()),
        Conditional(FunctionTask(lambda: decide(is_bat_getter(), counter_wolf_2.get_value(), counter_bat_2.get_value()),
            on_success=full_wolf(),
            on_fail=full_bat()
        ))
    )

cc = center_cover()

bins = Sequential(
    find_bins(),
    Log("found bins"),
    DecideAndPush(),

    #center_cover(),
)

