#!/usr/bin/env python3
from mission.framework.primitive import (
        Zero,
        Log,
        AlwaysLog,
        Succeed,
        Fail,
        FunctionTask,
        NoOp
)
from mission.framework.combinators import (
        Sequential,
        Concurrent,
        MasterConcurrent,
        Retry,
        Conditional,
        While
)
from mission.framework.targeting import ForwardTarget, HeadingTarget
from mission.framework.search import SearchFor, SwaySearch
from mission.framework.movement import RelativeToCurrentDepth, VelocityY, VelocityX, Depth
from mission.framework.position import MoveX, MoveY
from mission.framework.timing import Timeout
from mission.framework.actuators import FireActuator
# from mission.missions.ozer_common import StillHeadingSearch
from mission.missions.will_common import Consistent
from mission.missions.attilus_garbage import PIDStride, PIDSway, StillHeadingSearch, SwayOnlySearch

MOVE_DIRECTION = 1

BOARD_DEPTH = 2.7

import shm

CAM_CENTER = (shm.torpedoes_stake.camera_x.get(), shm.torpedoes_stake.camera_y.get())

# MOVE_DIRECTION = 1  # 1 if lever on left else -1 if on right


def heart():
    return (shm.torpedoes_stake.heart_x.get(), shm.torpedoes_stake.heart_y.get())

def belt():
    return (shm.torpedoes_stake.belt_x.get(), shm.torpedoes_stake.heart_y.get())

def left_hole():
    return (shm.torpedoes_stake.left_hole_x.get(), shm.torpedoes_stake.left_hole_y.get())

def right_hole():
    return (shm.torpedoes_stake.right_hole_x.get(), shm.torpedoes_stake.right_hole_y.get())

def close_left_visible():
    return shm.torpedoes_stake.close_left_visible.get()

def close_right_visible():
    return close_left_visible()

def close_heart_visible():
    return shm.torpedoes_stake.close_heart_visible.get()

def close_left_size():
    return shm.torpedoes_stake.close_left_size.get()

def close_right_size():
    if shm.torpedoes_stake.close_right_visible.get():
        return shm.torpedoes_stake.close_right_size.get()
    return close_left_size()

def close_heart_size():
    return shm.torpedoes_stake.close_heart_size.get()

def close_heart():
    return (shm.torpedoes_stake.close_heart_x.get(), shm.torpedoes_stake.close_heart_y.get())

def close_left():
    return (shm.torpedoes_stake.close_left_x.get(), shm.torpedoes_stake.close_left_y.get())

def close_right():
    if shm.torpedoes_stake.close_right_visible.get():
        return (shm.torpedoes_stake.close_right_x.get(), shm.torpedoes_stake.close_right_y.get())
    return close_left()

def lever():
    return (shm.torpedoes_stake.lever_origin_x.get(), shm.torpedoes_stake.lever_origin_y.get())

def align_h():
    return shm.torpedoes_stake.board_align_h.get()

def visible():
    return shm.torpedoes_stake.board_visible.get()

def size():
    return shm.torpedoes_stake.board_size.get()

def board_center():
    return (shm.torpedoes_stake.board_center_x.get(), shm.torpedoes_stake.board_center_y.get())


loaded_actuators = {'top_torpedo', 'bottom_torpedo'}

def Fire():
    try:
        fire = loaded_actuators.pop()
        return Sequential(Log('firing %s' % fire), FireActuator(fire, 0.3))
    except KeyError:
        return Fail(Log('no more torpedoes!'))


SearchBoard = lambda: Sequential(
        Log('Searching for torpedo board'),
        SearchFor(
            StillHeadingSearch(speed=10),
            visible,
            consistent_frames=(24, 42)
            ),
        Log('Found!'),
        Zero()
)

BackUpSway = lambda backx=1.5: Sequential(
        Log('Backing up'),
        MoveX(-backx),
        Log('Sway Search in Progress'),
        SwaySearch(width=2.5, stride=2)
        )

BackUpSwayOnly = lambda backx=0.3: Sequential(
        Log('Backing up'),
        MoveX(-backx),
        )

ReSearchLeftHoleOnFail = lambda timeout=30: Sequential(
        Timeout(SearchFor(
                BackUpSwayOnly(),
                close_left_visible,
                consistent_frames=(1.8*60, 2.0*60)
            ), timeout))


ReSearchRightHoleOnFail = lambda timeout=30: Sequential(
        Timeout(SearchFor(
                BackUpSwayOnly(),
                close_right_visible,
                consistent_frames=(1.8*60, 2.0*60)
            ), timeout))


ReSearchHeartOnFail = lambda timeout=30: Sequential(
        Timeout(SearchFor(
                BackUpSwayOnly(),
                close_heart_visible,
                consistent_frames=(1.8*60, 2.0*60)
            ), timeout))

ReSearchBoardOnFail = lambda backx=1.5, timeout=30: Sequential(  #TODO: Make it sway left if left before else right
    Timeout(SearchFor(
            BackUpSway(backx),
            visible,
            consistent_frames=(1.8*60, 2.0*60)
            ), timeout),
    Log('Found!'),
    Zero(),
    ApproachAlign()
    )

def withReSearchBoardOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(Timeout(task(*args, **kwargs), 60), on_fail=Fail(Sequential(Zero(), ReSearchBoardOnFail()))), attempts=2)

def withApproachAlignOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(Timeout(task(*args, **kwargs), 60), on_fail=Fail(Sequential(Zero(), ApproachAlign(), Zero()))), attempts=2)

def withShootRightOnFail(task):
    return lambda: Conditional(task(), on_fail=Fail(Sequential(Zero(), ApproachAlign(), ApproachRightHole() if MOVE_DIRECTION == 1 else ApproachLeftHole(), ApproachCloseRight() if MOVE_DIRECTION == 1 else ApproachCloseRight(), FireActuator('bottom_torpedo', 0.3), Backup())))

def withApproachBeltOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(Timeout(task(*args, **kwargs), 120), on_fail=Fail(Sequential(Zero(), Conditional(ReSearchHeartOnFail(), on_fail=ApproachBelt())))), attempts=2)

def withApproachLeftHoleOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(Timeout(task(*args, **kwargs), 120), on_fail=Fail(Sequential(Zero(), Conditional(ReSearchLeftHoleOnFail(), on_fail=ApproachLeftHole())))), attempts=2)

def withApproachRightHoleOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(Timeout(task(*args, **kwargs), 120), on_fail=Fail(Sequential(Zero(), Conditional(ReSearchRightHoleOnFail(), on_fail=ApproachRightHole())))), attempts=2)


close_to = lambda point1, point2, db=10: abs(point1[0]-point2[0]) < db and abs(point1[1]-point2[1]) < db
aligned = lambda align, db=4: abs(align) < db

@withReSearchBoardOnFail
def Align(centerf, alignf, visiblef, px=0.14, py=0.004, p=0.009, dx=0.00, dy=0.000, dsway=0.00, db=0):  # dx=0.005, dy=0.0005, dsway=0.002
    return MasterConcurrent(
            Consistent(lambda: close_to(centerf(), CAM_CENTER) and aligned(alignf()), count=1.5, total=2, invert=False, result=True),
            Consistent(visiblef, count=1.5, total=2.0, invert=True, result=False),
            HeadingTarget(point=centerf, target=CAM_CENTER, px=px, py=py, dy=dy, dx=dx, deadband=(db,db)),
            PIDSway(alignf, p=p, d=dsway, db=db),
            AlwaysLog(lambda: "align_h: %d" % (alignf(),)))

# def AlignBoard():
#     return Align(board_center, align_h, visible)
# def AlignHeart():
#     return Align(heart, align_h, visible)
# def AlignLeftHole():
#     return Align(left_hole, align_h, visible)
# def AlignRightHole():
#     return Align(right_hole, align_h, visible)

Center = lambda centerf,  visiblef, targetf=CAM_CENTER, px=0.0006, py=0.003, dx=0.000, dy=0.0, db=0, iy=0.0002, closedb=5: MasterConcurrent(
            Consistent(lambda: close_to(centerf(), targetf, db=closedb), count=4.0, total=5.0, invert=False, result=True),
            Consistent(visiblef, count=1.5, total=2.0, invert=True, result=False),
            ForwardTarget(point=centerf, target=targetf, px=px, py=py, dx=dx, dy=dy, iy=iy, deadband=(db,db)),
            AlwaysLog(lambda: "center: {}, target: {}".format(targetf, centerf())))

# @withApproachHeartOnFail
# def CenterHeart():
#     return Center(centerf=heart, visiblef=visible, py=0.002, closedb=20)
# def CenterBelt():
#     return Center(centef=belt, visiblef=visible, closedb=15)
# @withApproachLeftHoleOnFail
# def CenterLeftHole():
#     return Center(centerf=left_hole, visiblef=visible, closedb=15)
# @withApproachRightHoleOnFail
# def CenterRightHole():
#     return Center(centerf=right_hole, visiblef=visible, closedb=15)
@withApproachAlignOnFail
def CenterLever():
    return Center(centerf=lever, visiblef=visible, px=0.001, closedb=20)

# @withApproachAlignOnFail
# def CenterClose():
#     return Center(centerf=close, visiblef=close_visible, py=0.002, closedb=30)

# def CenterBoard():
#     return Center(centerf=board_center, visiblef=visible)



HOLE_SIZE=13000
APPROACH_SIZE = 80000
HEART_SIZE = 300000
def ApproachCenterSize(sizef, centerf, alignf, visiblef, size_thresh, p=0.000003, px=0.0009, py=0.005, dx=0.00, dy=0.01, d=0, consistent_total=2.0, closedb=20, db=30000):
    return MasterConcurrent(
            Consistent(lambda: abs(sizef()-size_thresh) < db and close_to(centerf(), CAM_CENTER, db=closedb), count=1.7, total=2.0, invert=False, result=True),
            Consistent(visiblef, count=1.3, total=consistent_total, invert=True, result=False),
            While(lambda: Center(centerf, visiblef, px=px, py=py, dx=dx, dy=dy), True),
            PIDStride(lambda: sizef()-size_thresh, p=p, d=d),
            AlwaysLog(lambda: "center: {}, target: {}, size{}".format(CAM_CENTER, centerf(), sizef())))

def ApproachAlignSize(sizef, centerf, alignf, visiblef, size_thresh, db=30000):
    return MasterConcurrent(
            Consistent(lambda: abs(sizef()-size_thresh) < db and aligned(alignf(), db=5), count=1.3, total=2.0, invert=False, result=True),
            Consistent(visiblef, count=1.3, total=2.0, invert=True, result=False),
            While(lambda: Align(centerf, alignf, visiblef), True),
            PIDStride(lambda: sizef()-size_thresh),
            AlwaysLog(lambda: "center: {}, target: {}, align: {}, size{}".format(CAM_CENTER, centerf(), alignf(), sizef())))

@withApproachAlignOnFail
def ApproachHeart():
    return ApproachCenterSize(size, heart, align_h, visible, APPROACH_SIZE)
@withApproachAlignOnFail
def ApproachBelt():
    return ApproachCenterSize(size, belt, align_h, visible, APPROACH_SIZE, consistent_total=3.0, closedb=50)
@withApproachAlignOnFail
def ApproachLeftHole():
    return ApproachCenterSize(size, left_hole, align_h, visible, APPROACH_SIZE, closedb=30, consistent_total=3.0)
@withApproachAlignOnFail
def ApproachRightHole():
    return ApproachCenterSize(size, right_hole, align_h, visible, APPROACH_SIZE, closedb=30, consistent_total=3.0)

# @withApproachAlignOnFail
# def ApproachClose():
#     return ApproachCenterSize(close_size, close, None, close_visible, HOLE_SIZE, p=0.000002, px=0.001, py=0.003, dy=0.005, db=4000, closedb=10, consistent_total=2.0)

@withApproachBeltOnFail
def ApproachCloseHeart():
    return ApproachCenterSize(close_heart_size, close_heart, None, close_heart_visible, HOLE_SIZE, p=0.000005, px=0.001, py=0.003, dy=0.005, dx=0.00005, db=4000, closedb=7, consistent_total=2.0)
@withApproachLeftHoleOnFail
def ApproachCloseLeft():
    return ApproachCenterSize(close_left_size, close_left, None, close_left_visible, HOLE_SIZE, p=0.000005, px=0.001, py=0.003, dy=0.005, dx=0.0001, db=4000, closedb=13, consistent_total=2.0)
@withApproachRightHoleOnFail
def ApproachCloseRight():
    return ApproachCenterSize(close_right_size, close_right, None, close_right_visible, HOLE_SIZE, p=0.000005, px=0.001, py=0.003, dy=0.005, dx=0.0001, db=4000, closedb=13, consistent_total=2.0)

@withReSearchBoardOnFail
def ApproachAlign():
    return ApproachAlignSize(size, board_center, align_h, visible, ALIGN_SIZE, db=3000)

def DeadReckonHeart():
    pass
def _DeadReckonLever():
    return Sequential(
        Succeed(Timeout(MoveX(.65, deadband=0.05), 20)),
        Succeed(Timeout(MoveY(MOVE_DIRECTION * 0.7, deadband=0.05), 20)),
        Succeed(Timeout(MoveY(MOVE_DIRECTION * -0.30, deadband=0.1), 20)),
        )

@withShootRightOnFail
def DeadReckonLever():
    return Retry(lambda: Sequential(
            CenterLever(),  # Approach?
            _DeadReckonLever(),
            Backup(),
            Succeed(Timeout(ApproachAlign(), 40)),
            Timeout(Consistent(lambda: shm.torpedoes_stake.lever_finished.get(), count=1.5, total=2.0, invert=False, result=True), 10)), attempts=2)


ALIGN_SIZE = 27000
@withReSearchBoardOnFail
def Backup(speed=0.2):
    return Sequential(
            Timeout(SearchFor(
                search_task=While(lambda: VelocityX(-speed), True),
                visible=lambda: visible() and size() < ALIGN_SIZE,
                consistent_frames=(1.7*60, 2.0*60),
                ), 15),
            Zero(),
            )


# Full = \
#     lambda: Sequential(
#         Log('Starting Stake'),
#         Depth(BOARD_DEPTH, error=0.2),
#         Timeout(SearchBoard(), 60),
#         ApproachAlign(),
#         Zero(),
#         ApproachBelt(),
#         ApproachCloseHeart(),
#         FireActuator('top_torpedo', 0.5),
#         Backup(),
#         ApproachAlign(),
#         DeadReckonLever(),
#         ApproachAlign(),
#         ApproachLeftHole() if MOVE_DIRECTION == 1 else ApproachRightHole(),
#         ApproachCloseLeft(),
#         FireActuator('bottom_torpedo', 0.5),
#         Backup(),
#         Log('Stake complete')
#     )



Full = \
    lambda: Sequential(
        Log('Starting Stake'),
        # Depth(BOARD_DEPTH, error=0.2),
        # Timeout(SearchBoard(), 60),
        FireActuator('top_torpedo', 0.3),
        Succeed(Timeout(ApproachAlign(), 20)),
        Log('what'),
        # ApproachLeftHole() if MOVE_DIRECTION == -1 else ApproachRightHole(),
        # ApproachCloseLeft() if MOVE_DIRECTION == -1 else ApproachCloseRight(),
        # Backup(),
        DeadReckonLever(),
        ApproachLeftHole() if MOVE_DIRECTION == 1 else ApproachRightHole(),
        ApproachCloseLeft() if MOVE_DIRECTION == 1 else ApproachCloseRight(),
        FireActuator('bottom_torpedo', 0.3),
        Backup(),
        Log('Stake complete')
    )


Test = \
    lambda: Sequential(
            ApproachAlign(),
            DeadReckonLever(),
            ApproachAlign(),
            ApproachRightHole(),
            Log('plox'),
            ApproachCloseRight(),
            Log('what'),
            FireActuator('top_torpedo', 0.5))
