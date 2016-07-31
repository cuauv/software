import math
import random
import time
from collections import namedtuple, deque
import numpy as np

import shm
import aslam
from mission.constants.config import recovery as constants
from mission.constants.region import WALL_TOWER_HEADING
from shm import recovery_state as world
from shm.watchers import watcher
from mission.framework.task import Task
from mission.framework.targeting import DownwardTarget, PIDLoop
from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent
from mission.framework.movement import Depth, RelativeToCurrentHeading, RelativeToCurrentDepth, \
    VelocityX, VelocityY, Heading
from mission.framework.timing import Timer
from mission.framework.primitive import Zero, FunctionTask, NoOp, Log
from mission.framework.helpers import get_downward_camera, ConsistencyCheck
from mission.framework.actuators import SetActuators
from mission.framework.position import MoveXY, MoveXYRough, MoveX, MoveY, GoToPosition, PositionalControl
from mission.framework.search import SpiralSearch
from mission.framework.track import Tracker, ConsistentObject
from mission.missions.ozer_common import Retry, Timeout, Success, SequentialSuccess, CheckDistance, \
    Conditional
from auv_python_helpers.angles import heading_sub_degrees

"""
    ____                                          ___   ____ ________
   / __ \___  _________ _   _____  _______  __   |__ \ / __ <  / ___/
  / /_/ / _ \/ ___/ __ \ | / / _ \/ ___/ / / /   __/ // / / / / __ \
 / _, _/  __/ /__/ /_/ / |/ /  __/ /  / /_/ /   / __// /_/ / / /_/ /
/_/ |_|\___/\___/\____/|___/\___/_/   \__, /   /____/\____/_/\____/
                                     /____/
"""

# TODO: rearrange stuff in file to make more sense
# TODO: log successes and failures of same task in same task?
# TODO: refactor vision results shm group into separate, per-object groups?

class Vision(Task):
    STACK_FIELDS = ['visible', 'red', 'x', 'y', 'area', 'aspect_ratio', 'angle']
    MARK_FIELDS = ['visible', 'x', 'y', 'area']
    REGION_FIELDS = ['visible', 'x', 'y', 'area']
    TABLE_FIELDS = ['visible', 'x', 'y', 'area']
    Stack = namedtuple('Stack', STACK_FIELDS)
    Mark = namedtuple('Mark', MARK_FIELDS)
    Region = namedtuple('Region', REGION_FIELDS)
    Table = namedtuple('Table', TABLE_FIELDS)
    COLORS = ['red', 'green']
    TRACKING_REJECT_WIDTH_RATIO = 0.15

    def on_first_run(self, camera, *args, **kwargs):
        self.camera = camera
        self.watcher = watcher()
        self.watcher.watch(shm.recovery_vision)

        self.stacks = [None] * 4
        tracker = lambda: Tracker(self.camera['width'] * Vision.TRACKING_REJECT_WIDTH_RATIO)
        self.red_stack_tracker = tracker()
        self.green_stack_tracker = tracker()
        self.mark_mappings = {color: ConsistentObject() for color in Vision.COLORS}
        self.region_mappings = {color: ConsistentObject() for color in Vision.COLORS}
        self.blocker_mappings = {color: ConsistentObject() for color in Vision.COLORS}
        self.table_mapping = ConsistentObject()

        self.pull()

    def on_run(self, *args, **kwargs):
        if self.watcher.has_changed():
            self.pull()

    def pull(self):
        self.shm = shm.recovery_vision.get()
        self.cam_center = (self.camera['width'] / 2, self.camera['height'] / 2)
        self.pull_stacks()
        self.pull_marks()
        self.pull_regions()
        self.pull_blockers()
        self.pull_table()

    def pull_stacks(self):
        red_stacks, green_stacks = [], []
        for i in range(4):
            vals = {}
            for field in Vision.STACK_FIELDS:
                vals[field] = getattr(self.shm, 'stack_{}_{}'.format(i+1, field))
            stack = Vision.Stack(**vals)
            if stack.visible:
                if stack.red:
                    red_stacks.append(stack)
                else:
                    green_stacks.append(stack)
        pad_list = lambda x: x + ([None] * (2 - len(x)))
        red_stacks = pad_list(red_stacks)
        green_stacks = pad_list(green_stacks)

        new_red_stacks = self.red_stack_tracker.track(*red_stacks)
        new_green_stacks = self.green_stack_tracker.track(*green_stacks)
        for i, s in enumerate(new_red_stacks + new_green_stacks):
            if s is not None and self.stacks[i] is None:
                self.logv('Started tracking {} stack at index {}'.format(
                    'red' if s.red else 'green', i))
            elif s is None and self.stacks[i] is not None:
                self.logv('Stopped tracking {} stack at index {}'.format(
                    'red' if self.stacks[i].red else 'green', i))
            self.stacks[i] = s

        self.debug_locations(self.stacks, 0)

    def pull_marks(self):
        self.marks = []
        for color in Vision.COLORS:
            vals = {}
            for field in Vision.MARK_FIELDS:
                vals[field] = getattr(self.shm, '{}_mark_{}'.format(color, field))
            mark = Vision.Mark(**vals)
            self.marks.append(self.mark_mappings[color].map(mark))

        self.debug_locations(self.marks, len(self.stacks))

    def pull_regions(self):
        self.regions = []
        for color in Vision.COLORS:
            vals = {}
            for field in Vision.REGION_FIELDS:
                vals[field] = getattr(self.shm, '{}_region_{}'.format(color, field))
            region = Vision.Region(**vals)
            self.regions.append(self.region_mappings[color].map(region))

        self.debug_locations(self.regions, len(self.stacks) + len(self.marks))

    def pull_blockers(self):
        self.blockers = {}
        for color in Vision.COLORS:
            blocker = getattr(self.shm, '{}_stack_blocking'.format(color))
            if not blocker:
                blocker = None
            self.blockers[color] = self.blocker_mappings[color].map(blocker)

    def pull_table(self):
        vals = {field: getattr(self.shm, 'table_{}'.format(field)) for field in Vision.TABLE_FIELDS}
        table = Vision.Table(**vals)
        self.table = self.table_mapping.map(table if table.visible else None)

    def debug_locations(self, objects, index_offset, coord_offset=(0, 0)):
        vision_debug = shm.vision_debug.get()
        for i, obj in enumerate(objects):
            if obj is not None and obj.visible:
                setattr(vision_debug, 'x{}'.format(i + index_offset), int(obj.x + coord_offset[0]))
                setattr(vision_debug, 'y{}'.format(i + index_offset), int(obj.y + coord_offset[1]))
                setattr(vision_debug, 'text{}'.format(i + index_offset), bytes(str(i + index_offset), encoding='utf-8'))
            else:
                setattr(vision_debug, 'text{}'.format(i + index_offset), b'')

        shm.vision_debug.set(vision_debug)

ExtendAmlan = lambda: SetActuators(['piston_extend'], ['piston_retract'])
RetractAmlan = lambda: SetActuators(['piston_retract'], ['piston_extend'])
#ExtendAmlan = NoOp
#RetractAmlan = NoOp

class Recovery(Task):
    initial_surface_retries = 10
    get_stack_retries = 10

    def on_first_run(self, vision, reset_state=True, *args, **kwargs):
        RetractAmlan()()
        if reset_state:
            w = world.get()
            w.stacks_on_tower = 4
            w.first_hstack_removed = 0
            w.grabber_stack_present = 0
            w.stacks_on_table = 0
            w.spiral_search_timed_out = 0
            world.set(w)

        initial_depth = shm.kalman.depth.get()
        get_stack = lambda: Retry(lambda: GetStack(vision), self.get_stack_retries)

        self.task = SequentialSuccess(subtasks=(
            [Retry(lambda: SequentialSuccess(
                Log('Performing initial surface'),
                MoveAboveTower(vision),
                Surface(),
            ), self.initial_surface_retries)] +
            [SequentialSuccess(
                get_stack(),
                Surface(),
                MoveAwayFromWall() if i == 0 else NoOp(),
                PlaceStack(vision)) for i in range(4)] +
            [Log('Finished recovery, returning to original depth'), Depth(initial_depth)]
        ))

    def on_run(self, vision, *args, **kwargs):
        if world.spiral_search_timed_out.get():
            self.loge('Timed out spiral search, exiting')
            self.finish()
            return

        if not self.task.finished:
            self.task()
        else:
            self.finish()
            if self.task.success:
                self.logi('Success!')
            else:
                self.loge('Failure :(')

    def on_finish(self, *args, **kwargs):
        RetractAmlan()()

class GetStack(Task):
    """
    Pick up one stack from the tower, including moving to the tower and grabbing a stack
    """
    def on_first_run(self, vision, *args, **kwargs):
        self.success = False
        self.choose_and_grab = ChooseAndGrab(vision)
        self.task = SequentialSuccess(
            MoveAboveTower(vision),
            self.choose_and_grab,
        )

    def on_run(self, vision, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.success = self.task.success
            self.finish(vision)

    def on_finish(self, vision, *args, **kwargs):
        if self.success:
            self.logi('Successfully grabbed stack!')
            world.stacks_on_tower.set(sum(s is not None for s in vision.stacks))
            choice = self.choose_and_grab.grab_choice
            world.grabber_stack_present.set(1)
            world.grabber_stack_red.set(choice.red)
            if choice.heading is not None:
                world.first_hstack_removed.set(1)
                world.second_hstack_heading.set((choice.heading + 180) % 360)
        else:
            self.loge('Failed to grab stack')
            RetractAmlan()()

class ChooseAndGrab(Task):
    """
    Choose a stack and try to grab it

    Begin: above tower looking at stacks
    """
    def on_first_run(self, vision, *args, **kwargs):
        self.success = False
        self.grab_choice = choose_next_stack(vision)
        if self.grab_choice.error_msg is not None:
            self.loge(self.grab_choice.error_msg)
            self.finish()
            return
        self.logi('Chose to grab {} {} stack at index {}'.format(
            'red' if self.grab_choice.red else 'green',
            'vertical' if self.grab_choice.vertical else 'horizontal',
            self.grab_choice.index,
        ))

        grab_task = None
        if self.grab_choice.vertical:
            grab_task = GrabVerticalStack(vision, self.grab_choice.index)
        else:
            grab_task = GrabHorizontalStack(
            vision, self.grab_choice.index, self.grab_choice.heading)

        initial_stacks = sum(stack is not None and stack.visible for stack in vision.stacks)
        self.task = SequentialSuccess(
            GrabAndRestore(vision, grab_task),
            VerifyGrab(vision, initial_stacks),
        )

    def on_run(self, vision, *args, **kwargs):
        if self.has_ever_finished:
            return
        if not self.task.finished:
            self.task()
        else:
            self.success = self.task.success
            self.finish()

class GrabAndRestore(Task):
    """
    Attempt to grab a stack and restore our initial position after
    """
    def on_first_run(self, vision, grab_task, *args, **kwargs):
        self.success = False

        north, east, depth = aslam.sub.position()
        self.task = Sequential(
            grab_task,

            Log('Moving to position before grab attempt'),
            Depth(depth),
            GoToPosition(north, east, optimize=False),
        )

    def on_run(self, vision, grab_task, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.success = grab_task.success
            self.finish()

class VerifyGrab(Task):
    """
    Check if the number of stacks on the tower is less than what we started
    with
    """
    def on_first_run(self, vision, original_stacks, *args, **kwargs):
        if original_stacks > 1:
            self.downward_target = SequentialSuccess(
                DownwardTargetObjects(vision, lambda: vision.stacks),
                Zero(),
            )
        else:
            self.downward_target = Success(Log('Not targeting stacks to verify grab, none should remain'))

    def on_run(self, vision, original_stacks, *args, **kwargs):
        if not self.downward_target.finished:
            self.downward_target()
        else:
            current_stacks = sum(stack is not None for stack in vision.stacks)
            self.success = self.downward_target.success and current_stacks < original_stacks
            if not self.success:
                self.loge('Grab failed, started with {} stacks but {} remain'.format(
                    original_stacks, current_stacks))

            # TODO fix
            # self.success = True
            self.finish()

class Search(Task):
    def on_first_run(self, *args, **kwargs):
        self.task = Timeout(Sequential(
            # Pause initially to give object-identifying tasks time to check current state
            Timer(0.5), SpiralSearch(
                relative_depth_range=0,
                optimize_heading=True,
                meters_per_revolution=1,
                min_spin_radius=1,
            )), 120)

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        elif not self.task.success:
            self.loge('Timed out while spiral searching')
            world.spiral_search_timed_out.set(1)

class MoveAboveTower(Task):
    """
    Move to above the tower as quickly as possible given the current known information
    """
    # TODO: support pinger tracking
    min_search_stacks = 2
    default_altitude = 2.8

    def on_first_run(self, vision, *args, **kwargs):
        self.success = False

        stacks_on_tower = world.stacks_on_tower.get()
        go_to_tower = None
        if stacks_on_tower < 4: # Only rely on tower position after we've picked up a stack
            north = world.tower_north.get()
            east = world.tower_east.get()
            depth = world.tower_depth.get()

            go_to_tower = Sequential(
                Log('Returning to tower position ({}, {}, {})'.format(north, east, depth)),
                Depth(depth), # Move to tower depth early so we don't crash into tower
                GoToPosition(north, east, optimize=True),
            )

        else:
            go_to_tower = Sequential(
                Log('Going to default tower altitude of {}'.format(self.default_altitude)),
                Altitude(self.default_altitude),
            )

        search_tower = None
        if stacks_on_tower > 0:
            search_stacks = 2 if stacks_on_tower == 4 else 1
            search_tower = Sequential(
                Log('Searching for tower'),
                Altitude(self.default_altitude),
                MasterConcurrent(
                    IdentifyObjects(lambda: vision.stacks, min_objects=search_stacks),
                    Search(),
                ),
            )
        else:
            search_tower = Log('Not searching for tower, no stacks on tower')

        center_tower = None
        if stacks_on_tower > 0:
            center_tower = SequentialSuccess(
                Log('Centering tower'),
                DownwardTargetObjects(vision, lambda: vision.stacks),
                Zero(),
            )
        else:
            center_tower = Log('No stacks on tower, not centering')

        self.task = SequentialSuccess(go_to_tower, search_tower, center_tower)

    def on_run(self, vision, *args, **kwargs):
        if not self.task.finished:
            self.task()

        else:
            if self.task.success:
                self.success = True
                north, east, depth = aslam.sub.position()
                world.tower_north.set(north)
                world.tower_east.set(east)
                world.tower_depth.set(depth)
            else:
                self.loge('Failed to move above tower')

            self.finish()

class IdentifyObjects(Task):
    """
    Finish when some objects we are looking for are in view
    """
    def on_run(self, objects_func, min_objects=1, *args, **kwargs):
        n = sum(obj is not None and obj.visible for obj in objects_func())
        if n >= min_objects:
            self.finish()

class DownwardTargetObjects(Task):
    """
    Downward target the center of all provided objects

    Begin: at least one object in view
    End: center of all objects in center of camera
    """
    ps = [0.002, 0.002, 0.001]
    deadbands = [(40, 40), (30, 30), (20, 20)]

    def centroid(self, objects):
        total_objects = 0
        center_x, center_y = 0, 0
        for obj in objects:
            if obj is not None and obj.visible:
                center_x += obj.x
                center_y += obj.y
                total_objects += 1

        center_x /= total_objects
        center_y /= total_objects
        return (center_x, center_y)

    def on_first_run(self, vision, objects_func, precision=0, *args, **kwargs):
        self.task = ConsistentTask(DownwardTarget(
            point=lambda: self.centroid(self.objects),
            target=vision.cam_center,
            deadband=self.deadbands[precision],
            px=self.ps[precision],
            py=self.ps[precision],
        ))

    def on_run(self, vision, objects_func, *args, **kwargs):
        self.success = False

        self.objects = objects_func()
        num_objects = sum(obj is not None and obj.visible for obj in self.objects)
        if num_objects == 0:
            self.loge("Can't see any objects, targeting aborted")
            self.finish()
            return

        self.task()
        if self.task.finished:
            self.success = True
            self.finish()

GrabChoice = namedtuple('GrabChoice', ['error_msg', 'index', 'red', 'vertical', 'heading'])

def choose_next_stack(vision):
    """
    Decide which stack to grab on the tower

    Current stack schedule: Pick up all vertical stacks, then all horizontal stacks.
    Needs to above the tower with all stacks in vision.
    """
    HORIZONTAL_ASPECT_RATIO = 2

    failedChoice = lambda info: GrabChoice(info, None, None, None, None)

    vstack_indices, hstack_indices = [], []
    for i, stack in enumerate(vision.stacks):
        if stack is not None:
            if stack.aspect_ratio >= HORIZONTAL_ASPECT_RATIO:
                hstack_indices.append(i)
            else:
                vstack_indices.append(i)

    target_indices = None
    if len(hstack_indices) == 2 or \
            (len(hstack_indices) == 1 and world.first_hstack_removed.get()):
        target_indices = hstack_indices
    else:
        target_indices = vstack_indices
    if len(target_indices) == 0:
        return failedChoice('No stacks found to grab')
    if len(target_indices) > 2:
        # return failedChoice('More than 2 {} stacks found, cannot grab'.format(
            # 'vertical' if target_indices is vstack_indices else 'horizontal'))
        target_indices = target_indices[:2]

    target_index = target_indices[int(random.random() * len(target_indices))]
    target_stack = vision.stacks[target_index]

    heading = None
    if target_index in hstack_indices:
        if len(hstack_indices) == 2:
            target_pos = np.array([target_stack.x, target_stack.y])
            other_index = None
            if target_index == hstack_indices[0]:
                other_index = hstack_indices[1]
            else:
                other_index = hstack_indices[0]
            other_stack = vision.stacks[other_index]

            avg_pos = np.array([
                (target_stack.x + other_stack.x) / 2,
                (target_stack.y + other_stack.y) / 2,
            ])
            stack_vec = target_pos - avg_pos

            heading = None
            heading = math.degrees(math.atan2(stack_vec[0], -stack_vec[1]))
            heading += shm.kalman.heading.get() # Now a global heading
            heading -= 90 # Align to the stack on the port side
            heading %= 360

        elif len(hstack_indices) == 1:
            heading = world.second_hstack_heading.get()

    return GrabChoice(
        error_msg=None,
        index=target_index,
        red=target_stack.red,
        vertical=target_index in vstack_indices,
        heading=heading,
    )

class GradualHeading(Task):
    relative_desire = 15
    deadband = 25

    def on_run(self, desire, *args, **kwargs):
        current = shm.kalman.heading.get()
        diff = heading_sub_degrees(desire, current)
        relative = math.copysign(self.relative_desire, diff)
        RelativeToCurrentHeading(relative)()
        if abs(diff) < self.deadband:
            self.finish()

class RelativeGradualDepth(Task):
    relative_desire = 0.2
    deadband = 0.05

    def on_first_run(self, offset, *args, **kwargs):
        self.desire = shm.kalman.depth.get() + offset

    def on_run(self, *args, **kwargs):
        diff = self.desire - shm.kalman.depth.get()
        relative = math.copysign(self.relative_desire, diff)
        RelativeToCurrentDepth(relative)()
        if abs(diff) < self.deadband:
            self.finish()

class GrabStack(Task):
    """
    Grabs the stack at the given index at the given approximate heading
    """
    align_p = 0.5
    align_deadband = 3
    scooch_retries = 20

    def on_first_run(self, vision, stack_i, timeout, dive_task, offset_task=None, approx_heading=None, snap_move=None, slide_move=None, *args, **kwargs):
        self.success = False
        self.must_see_stack = True
        initial_stack = vision.stacks[stack_i]
        if initial_stack is None:
            return
        self.color = 'red' if initial_stack.red else 'green'

        downward_target = lambda precision=0: DownwardTargetObjects(vision, lambda: [vision.stacks[stack_i]], precision=precision)
        precise_align = PIDLoop(
            lambda: vision.stacks[stack_i].angle,
            RelativeToCurrentHeading(),
            target=90,
            negate=True,
            p=self.align_p,
            deadband=self.align_deadband,
        )

        def ignore_stack():
            self.must_see_stack = False

        self.task = Timeout(SequentialSuccess(
            Log('Aligning to stack'),
            downward_target(),
            Zero(),

            Log('Going down and precisely aligning to stack'),
            Concurrent(
                Sequential(
                    GradualHeading(approx_heading),
                    precise_align,
                    finite=False,
                ) if approx_heading is not None else NoOp(),
                downward_target(precision=2),
                dive_task,
                finite=False,
            ),
            PositionalControl(),
            Zero(),

            FunctionTask(ignore_stack),
            Sequential(
                Log('Applying offset'),
                offset_task,
            ) if offset_task is not None else NoOp(),

            Log('Moving to stack'),
            snap_move if snap_move is not None else NoOp(),

            Log('Scooching away from stack'),
            Scooch(lambda: vision.blockers[self.color]),

            Log('Extending Amlan'),
            ExtendAmlan(),
            Timer(2),

            Log('Sliding stack'),
            Success(Timeout(slide_move, 10)) if slide_move is not None else NoOp(),

            Log('Hopefully grabbed stack'),
        ), timeout)

    def on_run(self, vision, stack_i, timeout, *args, **kwargs):
        if self.must_see_stack and vision.stacks[stack_i] is None:
            self.loge('Lost stack')
            Zero()()
            self.finish()
            return

        if not self.task.finished:
            self.task()
        else:
            self.finish()
            if self.task.success:
                self.success = True
            else:
                self.loge('Failed grab')

class ConsistentTask(Task):
    """
    Checks to make sure a non-finite task consistently is finished
    """
    def on_first_run(self, task, success=18, total=20, *args, **kwargs):
        self.cons_check = ConsistencyCheck(success, total)

    def on_run(self, task, *args, **kwargs):
        task()
        if self.cons_check.check(task.finished):
            self.finish()

MoveGrabberToCamera = lambda: ConsistentTask(MoveXY((-0.04064, 0.1), deadband=0.015))

def GrabVerticalStack(vision, stack_i):
    return GrabStack(
        vision, stack_i,
        timeout=60,
        dive_task=Altitude(constants.stack_dive_altitude),
        offset_task=MoveGrabberToCamera(),
        snap_move=Altitude(constants.vstack_grab_altitude),
    )

def GrabHorizontalStack(vision, stack_i, approx_heading):
    return GrabStack(
        vision, stack_i,
        timeout=90,
        dive_task=Altitude(constants.stack_dive_altitude),
        offset_task=MoveGrabberToCamera(),
        approx_heading=approx_heading,
        snap_move=Altitude(constants.hstack_grab_altitude),
        slide_move=MoveY(0.4),
    )

class Altitude(Task):
    min_depth = 0.5

    def on_first_run(self, altitude, p=0.5, d=0.1, *args, **kwargs):
        self.task = PIDLoop(
            shm.dvl.savg_altitude.get,
            RelativeToCurrentDepth(),
            target=altitude,
            p=p,
            d=d,
            negate=True,
            deadband=0.05,
        )

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
            if shm.navigation_desires.depth.get() < self.min_depth:
                self.loge('Min depth reached, preventing surface and exiting')
                shm.navigation_desires.depth.set(self.min_depth)
                self.finish()
        else:
            self.finish()

class AltitudeUntilStop(Task):
    """
    Attempt to move to the target altitude until the sub gets stuck
    """
    def on_first_run(self, altitude, *args, **kwargs):
        self.min_speed = 0.008
        self.min_delta = 0.1
        self.deque_size = 20

        self.success = False
        self.altitude_task = Altitude(altitude, p=0.3)
        self.stop_cons_check = ConsistencyCheck(3, 3)
        self.readings = deque()
        self.initial_altitude = shm.dvl.savg_altitude.get()
        self.last_altitude = self.initial_altitude

    def on_run(self, altitude, *args, **kwargs):
        if not self.altitude_task.has_ever_finished:
            self.altitude_task()
            current_altitude = shm.dvl.savg_altitude.get()
            self.readings.append((current_altitude, time.time()))
            if len(self.readings) > self.deque_size:
                self.readings.popleft()

            if abs(current_altitude - self.initial_altitude) >= self.min_delta and \
                    len(self.readings) >= self.deque_size:
                delta_altitude = self.readings[-1][0] - self.readings[0][0]
                delta_time = self.readings[-1][1] - self.readings[0][1]
                speed = abs(delta_altitude / delta_time)

                if self.stop_cons_check.check(speed < self.min_speed):
                    self.logi('Stopped changing altitude, finishing')
                    self.success = True
                    self.finish()
        else:
            self.loge('Bounding altitude reached')
            self.finish()

class Scooch(Task):
    """
    Scooch over to the right a little and check if we still see a blocking
    stack
    """
    distance_inc = 0.01
    max_distance = 0.5

    def on_first_run(self, blocking_func, *args, **kwargs):
        self.success = False
        self.distance_checker = CheckDistance(self.max_distance)

    def on_run(self, blocking_func, *args, **kwargs):
        if blocking_func() is None:
            self.success = True
            self.finish()

        if not self.distance_checker.finished:
            MoveY(self.distance_inc)()
            self.distance_checker()
        else:
            self.loge('Scooched too far, stack still blocking')
            self.finish()

    def on_finish(self, *args, **kwargs):
        Zero()()

class Wiggle(Task):
    def on_first_run(self, *args, **kwargs):
        self.task = Sequential(
            MoveX(-0.05, deadband=0.03),
            MoveX(0.1, deadband=0.04),
            MoveX(-0.1, deadband=0.04),
            MoveX(0.1, deadband=0.04),
            MoveX(-0.1, deadband=0.04),
            MoveX(0.1, deadband=0.04),
            MoveX(-0.1, deadband=0.04),
            MoveX(0.05, deadband=0.03),
        )

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.finish()

class Surface(Task):
    """
    Breaches the surface of the water inside the octogon for a fixed time

    Begin: Sub centered and zeroed directly over tower
    End: Sub slightly below surface
    """
    # Don't surface to a negative depth, it pushes bubbles under the sub onto the camera and dvl
    pre_surface_depth = 0.5
    surface_depth = 0
    surface_time = 3

    def on_first_run(self, *args, **kwargs):
        original_depth = shm.kalman.depth.get()

        self.task = Sequential(
            Log('Rising to just below surface'),
            PositionalControl(),
            Depth(self.pre_surface_depth),

            Log('Surfacing!'),
            MasterConcurrent(Timer(self.surface_time), Depth(self.surface_depth)),

            Log('Falling back below surface'),
            Depth(original_depth),
        )

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.finish()

MoveAwayFromWall = lambda: Sequential(
    Log('Moving away from wall'),
    Heading(WALL_TOWER_HEADING),
    MoveX(2),
)

class PlaceStack(Task):
    """
    Locate the table and place the stack we're holding on it

    Begin: holding a stack
    End: above the table after placing stack
    """
    move_above_table_retries = 10
    drop_stack_retries = 4

    def on_first_run(self, vision, *args, **kwargs):
        self.task = SequentialSuccess(
            Log('Moving above table'),
            Retry(lambda: MoveAboveTable(vision), self.move_above_table_retries),

            Log('Dropping stack'),
            Conditional(Retry(lambda: DropStack(vision), self.drop_stack_retries), on_failure=Sequential(
                Log('Failed to drop stack normally, performing blind drop', level='error'),
                DropStack(vision, blind=True),
            )),
        )

    def on_run(self, vision, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.finish()

    def on_finish(self, *args, **kwargs):
        self.success = self.task.success
        if self.success:
            world.stacks_on_table.set(world.stacks_on_table.get() + 1)
        else:
            self.loge('Failed to place stack')

class MoveAboveTable(Task):
    """
    Move to above the table as quickly as possible given the current known information

    Start: Anywhere
    End: Zeroed centered on the table
    """
    default_altitude = 2.6

    def on_first_run(self, vision, *args, **kwargs):
        self.success = False

        if world.stacks_on_table.get() > 0:
            north = world.table_north.get()
            east = world.table_east.get()
            depth = world.table_depth.get()

            self.task = SequentialSuccess(
                Log('Returning to table position ({}, {}, {})'.format(north, east, depth)),
                GoToPosition(north, east, optimize=True),
                Depth(depth), # Move to table depth late so we don't crash into tower

                Log('Searching for table'),
                MasterConcurrent(IdentifyObjects(lambda: [vision.table]), Search()),

                Log('Centering table'),
                DownwardTargetObjects(vision, lambda: [vision.table]),
                Zero(),
            )

        else:
            def record_position():
                north, east, depth = aslam.sub.position()
                world.table_north.set(north)
                world.table_east.set(east)
                world.table_depth.set(depth)

            self.task = SequentialSuccess(
                Log('Searching for table'),
                MasterConcurrent(IdentifyObjects(lambda: [vision.table]), Search()),

                Log('Centering table'),
                DownwardTargetObjects(vision, lambda: [vision.table]),

                Log('Moving to default table altitude'),
                Altitude(self.default_altitude),

                # Log('Centering colored table regions'),
                # DownwardTargetObjects(vision, lambda: vision.regions, precision=1),
                # Zero(),

                Log('Recording table position'),
                FunctionTask(record_position),
            )

    def on_run(self, vision, *args, **kwargs):
        if not self.task.finished:
            self.task()

        else:
            if self.task.success:
                self.success = True

                # The best time to record position is if we've never placed a
                # stack
                if world.stacks_on_table.get() == 0:
                    north, east, depth = aslam.sub.position()
                    world.table_north.set(north)
                    world.table_east.set(east)
                    world.table_depth.set(depth)
            else:
                self.loge('Failed to move above table')

            self.finish()

class DropStack(Task):
    """
    Try dropping the stack we're holding on the given mark color once

    Start: target mark visible in downcam
    End: above the table with stack dropped, or not
    """
    precise_align_altitude = 1.6
    drop_altitude = 0.6
    retract_amlan_time = 1
    after_drop_depth_delta = -0.5

    def on_first_run(self, vision, blind=False, *args, **kwargs):
        self.success = False

        if not world.grabber_stack_present.get():
            self.loge('No stack present in grabber, cannot drop')
            self.finish()
        red = world.grabber_stack_red.get()

        def empty_grabber():
            world.grabber_stack_present.set(0)

        target_region = lambda precision=0: DownwardTargetObjects(
            vision, lambda: [vision.regions[0 if red else 1]], precision=precision)

        self.task = SequentialSuccess(
            SequentialSuccess(
                Log('Targeting {} region'.format('red' if red else 'green')),
                target_region(),

                Log('Going down to target region more accurately'),
                Concurrent(
                    Altitude(self.precise_align_altitude),
                    target_region(precision=2),
                    finite=False,
                ),
            ) if not blind else NoOp(),

            PositionalControl(),
            Zero(),

            Log('Aligning target with grabber'),
            MoveGrabberToCamera(),

            Log('Going down to drop'),
            Altitude(self.drop_altitude),

            Log('Retracting Amlan'),
            RetractAmlan(),
            Timer(self.retract_amlan_time),
            FunctionTask(empty_grabber),

            Log('Moving up away from table slowly'),
            RelativeGradualDepth(self.after_drop_depth_delta),
        )

    def on_run(self, vision, *args, **kwargs):
        if self.has_ever_finished:
            return

        if not self.task.finished:
            self.task()
        else:
            self.success = self.task.success
            self.finish()
            if not self.success:
                self.loge('Failed to drop stack')

class VisionTask(Task):
    def on_first_run(self, task_class, *args, **kwargs):
        self.vision = Vision()
        task = task_class(self.vision, *args, **kwargs)
        self.task = Sequential(Timer(1), task)

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            try:
                camera = get_downward_camera()
                self.vision(camera)
                self.task()
            except RuntimeError:
                self.loge('Vision not running, refusing to run mission')
        else:
            self.finish()

def SimulatedTask(task):
    def update_altitude():
        shm.dvl.savg_altitude.set(3 - shm.kalman.depth.get())

    return MasterConcurrent(task, FunctionTask(update_altitude, finite=False))

class OptimalRecovery(Task):
    def desiredModules(self):
        return [shm.vision_modules.Recovery]

    def on_first_run(self):
        self.subtask = recovery()
        self.has_made_progress = True
        # TODO @AlexO Update when we've made progress! (first successful center on tower? first grabbed stack confirmed?)

    def on_run(self):
        self.subtask()
        if self.subtask.finished:
            self.finish()

class DownCalibrate(Task):
    def on_first_run(self, vision, stack_i, *args, **kwargs):
        self.align = DownwardTargetObjects(vision, lambda: [vision.stacks[stack_i]])

    def on_run(self, *args, **kwargs):
        self.align()
        if self.align.finished and not self.align.success:
            VelocityX(0)()
            VelocityY(0)()

class IdentifyTowerByPinger(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.task = ConsistentTask(IdentifyObjects(lambda: vision.stacks, min_objects=3), 28, 30)

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.finish()

identify_tower_by_pinger = lambda: VisionTask(IdentifyTowerByPinger)

recovery = lambda: VisionTask(Recovery)
recovery_noreset = lambda: VisionTask(Recovery, reset=False)
sim_recovery = lambda: SimulatedTask(recovery())
vision = lambda: Vision(get_downward_camera())
grab_vstack = lambda: VisionTask(GrabVerticalStack, 1)
sim_grab_vstack = lambda: SimulatedTask(grab_vstack())
grab_hstack = lambda: VisionTask(GrabHorizontalStack, 3, 0)
altitude_until_stop = lambda: AltitudeUntilStop(1)
move = lambda: MoveXYRough((-1, 0.5))
go_to_position = lambda: GoToPosition(0, 0, optimize=True)
altitude = lambda: Sequential(Altitude(2), Altitude(3), Altitude(3.5), Altitude(4))

sim_move_above_tower = lambda: SimulatedTask(VisionTask(MoveAboveTower))
sim_get_stack = lambda: SimulatedTask(VisionTask(GetStack))
sequential_success = lambda: SequentialSuccess(Timeout(NoOp(finite=False), 1), Log('next'))

def load_grabber():
    world.grabber_stack_present.set(1)
place_stack = lambda: Sequential(FunctionTask(load_grabber), VisionTask(PlaceStack))

sim_place_stack = lambda: SimulatedTask(VisionTask(PlaceStack))
surface = lambda: Surface()

calibrate = VisionTask(DownCalibrate, 1)
conditional = Conditional(NoOp(), on_failure=Log('oh no'))
gradual = lambda: RelativeGradualDepth(-0.5)
