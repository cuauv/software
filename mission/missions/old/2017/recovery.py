import time
from collections import deque
import itertools

import shm
from mission.constants.config import recovery as constants
from mission.constants.region import WALL_TOWER_HEADING
from shm.watchers import watcher
from mission.framework.task import Task
from mission.framework.search import SpiralSearch, VelocitySwaySearch
from mission.framework.combinators import (
    Sequential,
    MasterConcurrent,
    Retry,
    Conditional,
    While,
    Defer,
)
from mission.framework.movement import (
    Depth,
    RelativeToInitialDepth,
    RelativeToCurrentDepth,
    VelocityX,
    VelocityY,
    Heading,
)
from mission.framework.timing import Timer, Timeout, Timed
from mission.framework.primitive import (
    Zero,
    FunctionTask,
    NoOp,
    Log,
    Succeed,
    Fail,
)
from mission.framework.helpers import ConsistencyCheck
from mission.framework.position import (
    MoveX,
    MoveXRough,
    GoToPositionRough,
    WithPositionalControl,
)
from mission.framework.track import (
    Matcher,
    Match,
    Observation,
    ComplexColor,
    HeadingInvCameraCoord,
    HeadingInvAngle,
    ConsistentObject
)
from mission.missions.ozer_common import (
    GlobalTimeoutError,
    GradualHeading,
    GradualDepth,
    SearchWithGlobalTimeout,
    CenterCentroid,
    Disjunction,
    ConsistentTask,
    PrintDone,
    Altitude,
    AlignAmlan,
    AMLANS,
    Infinite,
    Except,
)


"""
    ____  ________________ _    ______________  __   ___   ____ ________
   / __ \/ ____/ ____/ __ \ |  / / ____/ __ \ \/ /  |__ \ / __ <  /__  /
  / /_/ / __/ / /   / / / / | / / __/ / /_/ /\  /   __/ // / / / /  / /
 / _, _/ /___/ /___/ /_/ /| |/ / /___/ _, _/ / /   / __// /_/ / /  / /
/_/ |_/_____/\____/\____/ |___/_____/_/ |_| /_/   /____/\____/_/  /_/

"""

# TODO: Always log info and errors with proper logging levels
# TODO: log successes and failures of same task in same task?
# TODO: rearrange stuff in file to make more sense


class GrabTube(Task):
    ALIGN_TIMEOUT = 60
    GRAB_ALTITUDE_TIMEOUT = 5

    def on_first_run(self, vision, tube, amlan):
        self.use_task(Sequential(
            Timeout(AlignAmlan(
                vision,
                lambda: vision.obj_with_id(vision.tubes, tube),
                amlan,
                Altitude(constants.tube_dive_altitude, overshoot_protect=True, deadband=0.07),
                align_during_depth=True,
            ), self.ALIGN_TIMEOUT),

            Log('Moving to tube grab altitude'),
            Conditional(
                Timeout(Altitude(constants.tube_grab_altitude), self.GRAB_ALTITUDE_TIMEOUT),
                on_fail=Log('Warning: grab altitude not fully reached, grabbing anyway'),
            ),

            Timer(1),
            amlan.Extend(),
            Timer(1.5),

            Log('Plucking tube'),
            Timeout(RelativeToInitialDepth(-0.25, error=0.10), 10),
        ))

class Vision(Task):
    class ColorObs(Observation):
        def __init__(self, shm_obj, camera_coord, heading_inv_angle, complex_color, size):
            self.adopt_attrs(shm_obj)
            self.camera_coord = camera_coord
            self.complex_color = complex_color
            self.heading_inv_angle = heading_inv_angle
            self.size = size

        def flow_distance_sq(self, obs):
            return self.camera_coord.flow_distance_sq(obs.camera_coord)

        def match_distance_sq(self, obs):
            # Only match along color and relative size dimensions since only these are known at this
            # point
            return (
                self.complex_color.match_distance_sq(obs.complex_color) +
                self.linear_distance_sq(self.size, obs.size, 0, 2)
            )

    class TubeObs(ColorObs):
        def match_distance_sq(self, obs):
            """ Match by relative position and angle, and forget about color since
            that's harder to match when there's less options """

            return (self.camera_coord.match_distance_sq(obs.camera_coord) +
                    self.heading_inv_angle.match_distance_sq(obs.heading_inv_angle))

    class EllipseObs(ColorObs):
        def match_distance_sq(self, obs):
            return self.camera_coord.match_distance_sq(obs.camera_coord)

    def __init__(self, *args, **kwargs):
        super().__init__()

        self.watcher = watcher()
        self.watcher.watch(shm.recovery_vision)

        # Initially pattern-match tubes and ellipses by color and size (later by pos/angle)

        def color_pattern(color_type_func):
            pattern = []
            for i, c in enumerate(constants.colors):
                camera_coord = HeadingInvCameraCoord(0, 0, 0)
                angle = HeadingInvAngle(0, 0)
                color = color_type_func(c)
                complex_color = ComplexColor(color.lab_a, color.lab_b, color.ycrcb_cr, color.ycrcb_cb)
                pattern.append(Match(i, self.ColorObs(
                    None,
                    camera_coord,
                    angle,
                    complex_color,
                    c.size,
                )))

            return pattern

        self.tube_matcher = Matcher(color_pattern(lambda c: c.tube_color))
        self.ellipse_matcher = Matcher(color_pattern(lambda c: c.ellipse_color))
        self.table_tracker = ConsistentObject()

        self.pull()

    def on_run(self, *args, **kwargs):
        if self.watcher.has_changed():
            self.pull()

    def classify_tubes(self):
        """
        Generate a new position and angle-based pattern for tube tracking with
        no color information (useful for tracking less than four tubes).

        All four colored tubes must be visible and identified, or classification
        cannot proceed.
        """

        if sum(t.id is not None for t in self.tubes) < len(constants.colors):
            self.loge('Cannot classify, not all tubes visible')
            return False

        # Create new pattern that's sensitive to pos / angle
        pattern = [Match(t.id, self.TubeObs(
            None,
            t.obs.camera_coord,
            t.obs.heading_inv_angle,
            t.obs.complex_color,
            t.obs.size,
        )) for t in self.tubes]
        self.tubes = self.tube_matcher.update_pattern(pattern)

    def classify_ellipses(self):
        """
        Generate a new position-based pattern for ellipse tracking with no color
        info.

        All four ellipses must be visible and identified, or classification
        cannot proceed.
        """

        if sum(e.id is not None for e in self.ellipses) < len(constants.colors):
            self.loge('Cannot classify, not all ellipses visible')
            return False

        pattern = [Match(e.id, self.EllipseObs(
            None,
            e.obs.camera_coord,
            e.obs.heading_inv_angle,
            e.obs.complex_color,
            e.obs.size,
        )) for e in self.ellipses]
        self.ellipses = self.ellipse_matcher.update_pattern(pattern)

    def remove_tube(self, i):
        new_pattern = [t for t in self.tube_matcher.pattern() if t.id != i]
        self.tube_matcher.update_pattern(new_pattern)

    def coords(self, objs, filter=lambda x: True):
        return [(obj.obs.x, obj.obs.y) for obj in objs if obj.obs is not None and filter(obj)]

    def obj_with_id(self, objs, id):
        candidates = [o for o in objs if o.id == id]
        if len(candidates) == 0:
            return None
        return candidates[0]

    def pull(self):
        self.tubes = self.tube_matcher.match(self.pull_colored_objs('tube', self.TubeObs))
        self.ellipses = self.ellipse_matcher.match(self.pull_colored_objs('ellipse', self.EllipseObs))

        self.debug(self.tubes, 0)
        self.debug(self.ellipses, len(self.tubes))

    def debug(self, objs, offset):
        for i, obj in enumerate(objs):
            abs_index = i + offset
            debug_g = shm._eval('vision_debug{}'.format(abs_index))
            debug = debug_g.get()

            if obj.obs is None:
                debug.x, debug.y = 0, 0
                debug.text = bytes(str(''), 'utf8')
            else:
                debug.x, debug.y = obj.obs.x, obj.obs.y

                if obj.id is None:
                    debug.text = bytes(str(abs_index), 'utf8')
                else:
                    debug.text = bytes(constants.colors[obj.id].name, 'utf8')

            debug_g.set(debug)

    def pull_colored_objs(self, name, obs_func):
        obs = []
        current_heading = shm.kalman.heading.get()

        for obj in self.read_shm_objs(name, len(constants.colors)):
            if not obj.visible:
                continue

            camera_coord = HeadingInvCameraCoord(obj.x, obj.y, current_heading)
            angle = HeadingInvAngle(
                obj.angle,
                current_heading,
                mod=180,
            )
            color = ComplexColor(obj.lab_a, obj.lab_b, obj.ycrcb_cr, obj.ycrcb_cb)

            obs.append(obs_func(obj, camera_coord, angle, color, obj.length))

        return obs

    def read_shm_objs(self, name, n):
        return [shm._eval('recovery_{}{}'.format(name, i)).get() for i in range(n)]

def FastDepth(d, *args, **kwargs): return Depth(d, error=0.2, *args, **kwargs)

class Recovery(Task):
    MAX_COLORS = 4 # How many colors to expect in shm

    def on_first_run(self, vision, reset_state=True, *args, **kwargs):
        # Limit navigation speed to prevent merge cutting out computer
        nav_speed = shm.navigation_settings.max_speed
        orig_speed = nav_speed.get()
        nav_speed.set(0.5)

        if reset_state:
            for loc in ['table', 'tower']:
                g_name = getattr(shm, 'recovery_world_{}'.format(loc))
                g = g_name.get()

                g.know_pos = False

                for i in range(self.MAX_COLORS):
                    setattr(g, 'has_tube{}'.format(i), loc == 'tower' and i < len(constants.colors))

                g_name.set(g)

            for a in AMLANS:
                a.shm_tube.set(-1)

        self.use_task(Conditional(
            Defer(
                Sequential(
                    Log('Starting recovery!'),

                    AMLANS[0].FastRetract(),
                    AMLANS[1].FastRetract(),

                    TryRecovery(vision),
                ),

                Sequential(
                    AMLANS[0].FastRetract(),
                    AMLANS[1].FastRetract(),

                    Log('Returning to pre-recovery depth'),
                    FastDepth(shm.kalman.depth.get()),

                    Log('Resetting navigation speed'),
                    FunctionTask(lambda: nav_speed.set(orig_speed)),

                    Log('Moving away from table'),
                    Heading(WALL_TOWER_HEADING),
                    MoveX(5, deadband=0.08),
                ),
            ),

            Log('Recovery success!'),

            Log('Recovery failure!'),
        ))

def get_shm_array(group, prefix):
    g = group.get()
    array = []

    for i in itertools.count():
        try:
            array.append(getattr(g, '{}{}'.format(prefix, i)))
        except AttributeError:
            break

    return array

def set_shm_array(group, prefix, array):
    g = group.get()
    for i, v in enumerate(array):
        setattr(g, '{}{}'.format(prefix, i), v)

    group.set(g)

def count_tubes(group):
    return sum(get_shm_array(group, 'has_tube'))

class TryRecovery(Task):
    def on_first_run(self, vision, *args, **kwargs):

        def AvoidWall():
            if not shm.recovery_world_table.know_pos.get():
                return Sequential(
                    Log('Moving away from wall'),
                    ConsistentTask(Heading(WALL_TOWER_HEADING)),
                    MoveXRough(2),
                )

            else:
                return NoOp()

        def Iteration(first_run=False):
            return Sequential(
                # Sequential(
                    # Log('Classifying tubes'),
                    # Retry(lambda: MoveAboveTower(vision), 3),
                    # FunctionTask(vision.classify_tubes),
                # ) if first_run else NoOp(),

                GetTube(vision),
                Succeed(GetTube(vision)), # It's OK if we don't grab the second tube
                Surface(),
                AvoidWall(),

                # Initially classify ellipses as their size / color may change
                # as tubes are added
                Sequential(
                    Log('Classifying ellipses'),
                    Retry(lambda: MoveAboveTable(vision), 3),
                    FunctionTask(vision.classify_ellipses),
                ) if first_run else NoOp(),

                PlaceTubes(vision),
            )

        self.use_task(Except(
            Sequential(
                Retry(lambda: MoveAboveTable(vision), 1000),
                Log('Moving towards wall'),
                ConsistentTask(Heading((WALL_TOWER_HEADING + 180) % 360)),
                MoveXRough(2),

                Iteration(first_run=True),

                While(
                    Iteration,
                    lambda: count_tubes(shm.recovery_world_tower) > 0,
                ),
            ),

            Fail(Log('Global timeout, done trying recovery T_T')),
            GlobalTimeoutError,
        ))

class GetTube(Task):
    """
    Pick up one tube from the tower, including moving to tower
    """

    MOVE_ABOVE_TOWER_TRIES = 3
    TRIES_PER_TUBE = 3

    def on_first_run(self, vision, *args, **kwargs):
        # Sort available tubes in priority order
        has_tubes = get_shm_array(shm.recovery_world_tower, 'has_tube')
        tubes = [i for i, t in enumerate(has_tubes) if t]
        tubes.sort()

        def amlan_avail(amlan):
            return amlan.shm_tube.get() == -1 and not amlan.broken

        # Try grabbing with different amlans if failing
        primary_amlan, alt_amlan = tuple(AMLANS)
        if not primary_amlan.available():
            primary_amlan = alt_amlan
        if not alt_amlan.available():
            if primary_amlan is alt_amlan:
                self.loge('Cannot get a tube, each amlan full or broken')
                self.finish(success=False)
                return

            alt_amlan = primary_amlan

        def grab_task(tube, amlan, tries):
            return Sequential(
                Log('Trying to grab {} tube with {} Amlan'.format(
                    constants.colors[tube].name,
                    amlan.name,
                )),
                Retry(lambda: MoveAboveTower(vision), 3),
                Retry(lambda: GetThisTube(vision, tube, amlan), tries),
            )

        # Try for highest priority tubes first, then lower ones
        subtasks = []
        for tube in tubes:
            subtasks.append(grab_task(
                tube,
                primary_amlan,
                self.TRIES_PER_TUBE // 2,
            ))
            subtasks.append(grab_task(
                tube,
                alt_amlan,
                self.TRIES_PER_TUBE - self.TRIES_PER_TUBE // 2,
            ))
        subtasks.append(Fail(Log('Failed to pick up tubes')))

        self.use_task(Disjunction(subtasks=subtasks))

class GetThisTube(Task):
    """
    Try to grab the given tube with the given amlan once.

    Begin: Above tower
    End: Given amlan closed if tubed grabbed
    """
    def on_first_run(self, vision, tube, amlan, *args, **kwargs):

        def apply_grab():
            # Add tube to Amlan in shm
            amlan.shm_tube.set(tube)

            # Remove tube from tower in shm
            tower_tubes = get_shm_array(shm.recovery_world_tower, 'has_tube')
            tower_tubes[tube] = False
            set_shm_array(shm.recovery_world_tower, 'has_tube', tower_tubes)

            # Remove tube from vision pattern
            vision.remove_tube(tube)

        north = shm.kalman.north.get()
        east = shm.kalman.east.get()
        depth = shm.desires.depth.get()

        initial_tubes = sum(t.obs is not None for t in vision.tubes)

        self.use_task(Conditional(
            Sequential(
                amlan.FastRetract(),

                Defer(
                    Conditional(
                        GrabTube(vision, tube, amlan),
                        on_fail=Fail(Sequential(
                            Log('Failed to grab, prematurely retracting grabber'),
                            amlan.FastRetract(),
                        )),
                    ),

                    Sequential(
                        Zero(),
                        Log('Restoring position from before grab'),
                        FastDepth(depth),
                        GoToPositionRough(north, east, optimize=False),
                    ),
                ),

                # TODO continue always assuming good grab?
                # VerifyGrab(vision, initial_tubes),
                FunctionTask(apply_grab),
            ),

            Log('Grabbed {} tube with {} Amlan!'.format(
                constants.colors[tube].name,
                amlan.name,
            )),

            Fail(amlan.FastRetract()),
        ))

class VerifyGrab(Task):
    """
    Check if the number of tubes on the tower is less than what we started
    with
    """
    def on_first_run(self, vision, initial_tubes, *args, **kwargs):
        if initial_tubes > 1:
            self.downward_target = Sequential(
                CenterCentroid(lambda: vision.coords(vision.tubes)),
                Zero(),
            )
        else:
            self.downward_target = Succeed(Log('Not targeting tubes to verify grab, none should remain'))

    def on_run(self, vision, initial_tubes, *args, **kwargs):
        self.downward_target()
        if self.downward_target.finished:
            current_tubes = sum(t.obs is not None for t in vision.tubes)
            success = self.downward_target.success and current_tubes < initial_tubes
            if not success:
                self.loge('Grab failed, started with {} tubes but {} remain'.format(
                    initial_tubes, current_tubes))

            self.finish(success=success)

class MoveAboveTower(Task):
    """
    Move to above the tower as quickly as possible given the current known information
    """
    def on_first_run(self, vision, *args, **kwargs):
        tower_tubes = get_shm_array(shm.recovery_world_tower, 'has_tube')
        search_tubes = 2 if sum(tower_tubes) == len(constants.colors) else 1

        tower = shm.recovery_world_tower.get()
        go_to_tower = NoOp()
        if tower.know_pos:
            go_to_tower = Sequential(
            Log('Returning to tower position ({}, {})'.format(tower.north, tower.east)),
                GoToPositionRough(tower.north, tower.east, optimize=False),
            )

        if sum(tower_tubes) > 0:
            search_tower = Sequential(
                Log('Searching for tower'),
                FastDepth(constants.tower_depth),
                MasterConcurrent(
                    IdentifyObjects(
                        lambda: vision.tubes,
                        search_tubes,
                    ),
                    SearchWithGlobalTimeout(),
                ),
            )
        else:
            search_tower = Log('Not searching for tower, no tubes on tower')

        if sum(tower_tubes) > 0:
            center_tower = Sequential(
                Log('Centering tower'),
                CenterCentroid(lambda: vision.coords(vision.tubes)),
                Zero(),
            )
        else:
            center_tower = Log('No tubes on tower, not centering')

        self.use_task(Sequential(
            # Go to depth early so we don't hit the tower
            Log('Going to default tower depth of {}'.format(constants.tower_depth)),
            FastDepth(constants.tower_depth),

            go_to_tower,
            search_tower,
            center_tower,
        ))

    def on_finish(self, *args, **kwargs):
        if self.success:
            tower = shm.recovery_world_tower.get()

            tower.know_pos = True
            tower.north = shm.kalman.north.get()
            tower.east = shm.kalman.east.get()
            tower.depth = shm.kalman.depth.get()

            shm.recovery_world_tower.set(tower)
        else:
            self.loge('Failed to move above tower')

class IdentifyObjects(Task):
    """
    Finish when some objects we are looking for are in view
    """
    def on_run(self, objects_func, min_objects=1, *args, **kwargs):
        if sum(obj.obs is not None for obj in objects_func()) >= min_objects:
            self.finish()

class AltitudeUntilStop(Task):
    """
    Attempt to move to the target altitude until the sub gets stuck
    """
    def on_first_run(self, altitude, *args, **kwargs):
        self.min_speed = 0.008
        self.min_delta = 0.1
        self.deque_size = 20

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
                    self.finish()
        else:
            self.loge('Bounding altitude reached')
            self.finish(success=False)

class Surface(Task):
    """
    Breaches the surface of the water inside the octogon

    Begin and end: Sub centered and zeroed directly over tower
    """

    def on_first_run(self, *args, **kwargs):
        original_depth = shm.kalman.depth.get()

        self.use_task(WithPositionalControl(Sequential(
            Log('Getting close to surface'),
            FastDepth(0.5),

            Log('Surfacing!'),
            Timed(Depth(0), 3),

            Log('Falling back below surface'),
            FastDepth(original_depth),
        )))

class PlaceTubes(Task):
    """
    Locate the table and place the tubes we're holding on it

    Begin: holding at least one tube
    End: holding no tubes
    """

    def on_first_run(self, vision, *args, **kwargs):

        def place_tube(amlan):
            return Defer(
                Conditional(
                    Retry(lambda: Sequential(
                        Log('Moving above table'),
                        MoveAboveTable(vision),

                        Log('Placing {} tube'.format(constants.colors[amlan.shm_tube.get()].name)),
                        DropTube(vision, amlan),
                    ), 3),

                    on_fail=Succeed(Sequential(
                        Log('Failed to drop tube normally, performing blind drop'),
                        DropTube(vision, amlan, blind=True),
                    )),
                ),

                amlan.FastRetract(),
            )

        self.use_task(Sequential(
            place_tube(AMLANS[0]) if AMLANS[0].shm_tube.get() != -1 else NoOp(),
            place_tube(AMLANS[1]) if AMLANS[1].shm_tube.get() != -1 else NoOp(),
        ))

    def on_finish(self, *args, **kwargs):
        if not self.success:
            self.loge('Failed to place tube')

class MoveAboveTable(Task):
    """
    Move to above the table as quickly as possible given the current known information

    Start: Anywhere
    End: Zeroed centered on the table
    """

    TIMEOUT = 1000000

    def on_first_run(self, vision, *args, **kwargs):
        table = shm.recovery_world_table.get()

        def record_position():
            table.know_pos = True
            table.north = shm.kalman.north.get()
            table.east = shm.kalman.east.get()
            shm.recovery_world_table.set(table)

        self.use_task(Sequential(
            # Move to the table depth first so we don't blow things off
            Log('Moving to table depth'),
            FastDepth(constants.table_depth),

            Sequential(
                Log('Returning to table position ({}, {})'.format(table.north, table.east)),
                GoToPositionRough(table.north, table.east, optimize=False),
            ) if table.know_pos else NoOp(),

            Log('Searching for table'),
            MasterConcurrent(
                IdentifyObjects(lambda: vision.ellipses, 2),
                SearchWithGlobalTimeout(timeout=self.TIMEOUT)
            ),

            Log('Centering table'),
            CenterCentroid(lambda: vision.coords(vision.ellipses)),
            Zero(),

            Log('Recording table position'),
            FunctionTask(record_position),
        ))

    def on_finish(self, *args, **kwargs):
        if not self.success:
            self.loge('Failed to move above table')

class DropTube(Task):
    """
    Try dropping the tube we're holding in the given amlan on its correct
    ellipse once

    Start: target mark visible in downcam
    End: above the table with tube dropped, or not
    """
    AFTER_DROP_DELTA = -2

    def on_first_run(self, vision, amlan, blind=False):
        initial_tube = amlan.shm_tube.get()

        def apply_drop():
            # Add tube to table
            table_tubes = get_shm_array(shm.recovery_world_table, 'has_tube')
            table_tubes[initial_tube] = True
            set_shm_array(shm.recovery_world_table, 'has_tube', table_tubes)

        self.use_task(Sequential(
            AlignAmlan(
                vision,
                lambda: vision.obj_with_id(vision.ellipses, amlan.shm_tube.get()),
                amlan,
                FastDepth(constants.ellipse_depth),
                blind=blind,
                align_during_depth=True,
            ),

            Sequential(
                Log('Moving to tube drop depth'),
                GradualDepth(constants.drop_depth),
            ) if not blind else NoOp(),
            Timer(1),

            amlan.Retract(),
            FunctionTask(apply_drop),

            Sequential(
                Log('Moving up away from table slowly'),
                GradualDepth(constants.drop_depth + self.AFTER_DROP_DELTA),
            ) if not blind else NoOp(),
        ))

class VisionTask(Task):
    def on_first_run(self, task_func, *args, **kwargs):
        vision = Vision()
        self.use_task(MasterConcurrent(task_func(vision, *args, **kwargs), vision))

class DownCalibrate(Task):
    def on_first_run(self, vision, tube, *args, **kwargs):
        self.align = CenterCentroid(lambda: vision.coords(
            vision.tubes,
            filter=lambda x: x.id == tube
        ))

    def on_run(self, *args, **kwargs):
        self.align()
        if self.align.finished and not self.align.success:
            VelocityX(0)()
            VelocityY(0)()

def AfterTwoGrabs():
    shm.recovery_world_grabbers.left_tube.set(0)
    shm.recovery_world_grabbers.right_tube.set(1)
    shm.recovery_world_tower.know_pos.set(True)
    shm.recovery_world_tower.north.set(shm.kalman.north.get())
    shm.recovery_world_tower.east.set(shm.kalman.east.get())
    shm.recovery_world_table.know_pos.set(False)

    return VisionTask(PlaceTubes)

# Shortcuts for grabber movement
def Gle(): return AMLANS[0].Extend()
def Glr(): return AMLANS[0].Retract()
def Gre(): return AMLANS[1].Extend()
def Grr(): return AMLANS[1].Retract()

def GradDepth(): return GradualDepth(1)

def Full(): return VisionTask(Recovery)

class FoundRecoveryObjs(Task):
    def on_first_run(self, vision, tower=True, *args, **kwargs):
        obj_func = (lambda: vision.tubes) if tower else (lambda: vision.ellipses)
        self.use_task(ConsistentTask(
            IdentifyObjects(obj_func, 3),
            success=20,
            total=30,
        ))

class Found(Task):
    def on_first_run(self, *args, **kwargs):
        self.use_task(VisionTask(FoundRecoveryObjs, tower=True))

class FoundTable(Task):
    def on_first_run(self, *args, **kwargs):
        self.use_task(VisionTask(FoundRecoveryObjs, tower=False))

def Classify():
    return VisionTask(lambda vision: Sequential(
        FunctionTask(lambda: shm.vision_modules.Debug.set(1)),
        Timer(2),
        FunctionTask(vision.classify_tubes),
        Infinite(Timer(1)),
    ))

def Alt(): return Altitude(1)
def DepthGrab(): return RelativeToCurrentDepth(0.3)
def FastSpiral():
    return SpiralSearch(
        relative_depth_range=0,
        optimize_heading=True,
        meters_per_revolution=1,
        min_spin_radius=1,
        deadband=1,
    )
def Swaying(): return VelocitySwaySearch(1, 1)
