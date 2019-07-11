#!/usr/bin/env python3
from mission.framework.primitive import (
        Zero,
        Log,
        AlwaysLog,
        Succeed,
        Fail,
        FunctionTask,
        # NoOp
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
from mission.framework.movement import RelativeToCurrentDepth, VelocityY, VelocityX
from mission.framework.position import MoveX, MoveY
from mission.framework.timing import Timeout
from mission.framework.actuators import FireActuator
# from mission.missions.ozer_common import StillHeadingSearch
from mission.missions.will_common import Consistent
from mission.missions.attilus_garbage import PIDStride, PIDSway, StillHeadingSearch


import shm

CAM_CENTER = (shm.torpedoes_stake.camera_x.get(), shm.torpedoes_stake.camera_y.get())

# At the moment, 90% of the mission is fudged and untested. Proceed with caution.

TARGETS = {"upper": "lower", "lower": "upper"}
current_target = ""
MOVE_DIRECTION=1  # 1 if lever on left else -1 if on right


def heart():
    return (shm.torpedoes_stake.heart_x.get(), shm.torpedoes_stake.heart_y.get())

def belt():
    return (shm.torpedoes_stake.belt_x.get(), shm.torpedoes_stake.heart_y.get())

def left_hole():
    return (shm.torpedoes_stake.left_hole_x.get(), shm.torpedoes_stake.left_hole_y.get())

def right_hole():
    return (shm.torpedoes_stake.right_hole_x.get(), shm.torpedoes_stake.right_hole_y.get())

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

SearchBoard = lambda: Sequential(
        Log('Searching for torpedo board'),
        SearchFor(
            StillHeadingSearch(speed=10),
            visible,
            consistent_frames=(1.7*60, 2.0*60)
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
    return lambda *args, **kwargs: Retry(lambda: Conditional(task(*args, **kwargs), on_fail=Fail(Sequential(Zero(), ReSearchBoardOnFail()))), attempts=2)

def withApproachAlignOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(task(*args, **kwargs), on_fail=Fail(Sequential(Zero(), ApproachAlign()))), attempts=2)

def withShootRightOnFail(task):
    return lambda: Conditional(task(), on_fail=Fail(Sequential(Zero(), ApproachAlign(), CenterRightHole(), ApproachRightHole(), DeadReckonRightHole(), Backup())))

def withApproachHeartOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(task(*args, **kwargs), on_fail=Fail(Sequential(Zero(), CenterHeart()))), attempts=2)

def withApproachLeftHoleOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(task(*args, **kwargs), on_fail=Fail(Sequential(Zero(), ApproachLeftHole()))), attempts=2)

def withApproachRightHoleOnFail(task):
    return lambda *args, **kwargs: Retry(lambda: Conditional(task(*args, **kwargs), on_fail=Fail(Sequential(Zero(), ApproachRightHole()))), attempts=2)


close_to = lambda point1, point2, db=10: abs(point1[0]-point2[0]) < db and abs(point1[1]-point2[1]) < db
aligned = lambda align, db=4: abs(align) < db

@withReSearchBoardOnFail
def Align(centerf, alignf, visiblef, px=0.14, py=0.004, p=0.009, dx=0.00, dy=0.000, dsway=0.00, db=0): #dx=0.005, dy=0.0005, dsway=0.002
    return MasterConcurrent(
            Consistent(lambda: close_to(centerf(), CAM_CENTER) and aligned(alignf()), count=1.5, total=2, invert=False, result=True),
            Consistent(visiblef, count=1.5, total=2.0, invert=True, result=False),
            HeadingTarget(point=centerf, target=CAM_CENTER, px=px, py=py, dy=dy, dx=dx, deadband=(db,db)),
            PIDSway(alignf, p=p, d=dsway, db=db),
            AlwaysLog(lambda: "align_h: %d" % (alignf(),)))

def AlignBoard():
    return Align(board_center, align_h, visible)
def AlignHeart():
    return Align(heart, align_h, visible)
def AlignLeftHole():
    return Align(left_hole, align_h, visible)
def AlignRightHole():
    return Align(right_hole, align_h, visible)

Center = lambda centerf,  visiblef, targetf=CAM_CENTER, px=0.0006, py=0.003, dx=0.000, dy=0.0, db=0, iy=0.0002, closedb=5: MasterConcurrent(
            Consistent(lambda: close_to(centerf(), targetf, db=closedb), count=4.0, total=5.0, invert=False, result=True),
            Consistent(visiblef, count=1.5, total=2.0, invert=True, result=False),
            ForwardTarget(point=centerf, target=targetf, px=px, py=py, dx=dx, dy=dy, iy=iy, deadband=(db,db)), 
            AlwaysLog(lambda: "center: {}, target: {}".format(targetf, centerf())))

@withApproachHeartOnFail
def CenterHeart():
    return Center(centerf=heart, visiblef=visible, py=0.002, closedb=20)
def CenterBelt():
    return Center(centef=belt, visiblef=visible, closedb=15)
@withApproachLeftHoleOnFail
def CenterLeftHole():
    return Center(centerf=left_hole, visiblef=visible, closedb=15)
@withApproachRightHoleOnFail
def CenterRightHole():
    return Center(centerf=right_hole, visiblef=visible, closedb=15)
@withApproachAlignOnFail
def CenterLever():
    return Center(centerf=lever, visiblef=visible, closedb=20)

def CenterBoard():
    return Center(centerf=board_center, visiblef=visible)



# TODO: tune everything
APPROACH_SIZE = 70000
HEART_SIZE = 300000
def ApproachCenterSize(sizef, centerf, alignf, visiblef, size_thresh, p=0.000003, px=0.0009, py=0.004, dx=0.00, dy=0.005, d=0, consistent_total=2.0, closedb=20, db=30000):
    return MasterConcurrent(
            Consistent(lambda: abs(sizef()-size_thresh) < db and close_to(centerf(), CAM_CENTER, db=closedb), count=2.7, total=3.0, invert=False, result=True),
            Consistent(visiblef, count=1.3, total=consistent_total, invert=True, result=False),
            While(lambda: Center(centerf, visiblef, px=px, py=py, dx=dx, dy=dy), True),
            PIDStride(lambda: sizef()-size_thresh, p=p, d=d),
            AlwaysLog(lambda: "center: {}, target: {}, align: {}, size{}".format(CAM_CENTER, centerf(), alignf(), sizef())))

def ApproachAlignSize(sizef, centerf, alignf, visiblef, size_thresh, db=30000):
    return MasterConcurrent(
            Consistent(lambda: abs(sizef()-size_thresh) < db and aligned(alignf(), db=4), count=3.3, total=4.0, invert=False, result=True),
            Consistent(visiblef, count=1.3, total=2.0, invert=True, result=False),
            While(lambda: Align(centerf, alignf, visiblef), True),
            PIDStride(lambda: sizef()-size_thresh),
            AlwaysLog(lambda: "center: {}, target: {}, align: {}, size{}".format(CAM_CENTER, centerf(), alignf(), sizef())))

@withApproachAlignOnFail
def ApproachHeart():
    return ApproachCenterSize(size, heart, align_h, visible, APPROACH_SIZE)
@withApproachAlignOnFail
def ApproachBelt():
    return ApproachCenterSize(size, belt, align_h, visible, HEART_SIZE, p=0.0000007, db=50000, consistent_total=2.5, closedb=50)
@withApproachAlignOnFail
def ApproachLeftHole():
    return ApproachCenterSize(size, left_hole, align_h, visible, APPROACH_SIZE, closedb=15, consistent_total=3.0)
@withApproachAlignOnFail
def ApproachRightHole():
    return ApproachCenterSize(size, right_hole, align_h, visible, APPROACH_SIZE, closedb=15, consistent_total=3.0)

@withReSearchBoardOnFail
def ApproachAlign():
    return ApproachAlignSize(size, board_center, align_h, visible, ALIGN_SIZE, db=5000)

def DeadReckonHeart():
    pass
def _DeadReckonLever():
    return Sequential(
        Succeed(Timeout(MoveX(.60, deadband=0.05), 20)),
        Succeed(Timeout(MoveY(MOVE_DIRECTION * .7, deadband=0.05), 20)),)

@withShootRightOnFail
def DeadReckonLever():
    return Retry(lambda: Sequential(
            CenterLever(),  # Approach?
            _DeadReckonLever(),
            Backup(),
            ApproachAlign(),
            Timeout(Consistent(lambda: shm.torpedoes_stake.lever_finished.get(), count=1.5, total=2.0, invert=False, result=True), 10)), attempts=2)

def DeadReckonLeftHole():
    return Sequential(MoveX(0.25),
            FireActuators())  # TODO
def DeadReckonRightHole():
    pass


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


Full = \
    lambda: Sequential(
        Log('Starting Stake'),
        SearchBoard(),  # Timeout
        ApproachAlign(),
        ApproachHeart(),
        CenterHeart(),
        DeadReckonHeart(),
        Backup(),
        ApproachAlign(),
        DeadReckonLever(),
        ApproachAlign(),
        ApproachLeftHole(),
        CenterLeftHole(),
        DeadReckonLeftHole(),  # Approach?
        Backup(),
        Log('Stake complete')
    )


Test = \
    lambda: Sequential(
            ApproachAlign(),
            ApproachRightHole(),
            Log('what'),
            FireActuator('bottom_torpedo', 0.3))
