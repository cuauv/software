import math
import itertools
import shm
from mission.framework.task import Task
from mission.framework.targeting import DownwardTarget, PIDLoop
from mission.framework.combinators import (
    Sequential,
    While,
    Concurrent,
    Defer,
)
from mission.framework.timing import Timer, Timeout, Timed
from mission.framework.movement import (
    Depth,
    RelativeToCurrentHeading,
    RelativeToCurrentDepth,
    Heading,
)
from mission.framework.helpers import call_if_function, ConsistencyCheck
from mission.framework.search import SpiralSearch
from mission.framework.primitive import (
    InvertSuccess,
    Log,
    Zero,
    FunctionTask,
    NoOp,
)
from mission.framework.position import MoveX, PositionalControl, MoveXY
from auv_python_helpers.angles import heading_sub_degrees
from mission.framework.actuators import SetActuators

"""
Tasks that I (Ozer) like to use in missions but don't feel are worthy of
necessary adding directly to the mission framework just yet.
"""


class Amlan:
    def __init__(self, downcam_offset, name, extend_piston, retract_piston, shm_tube, broken):
        self.downcam_offset = downcam_offset
        self.name = name
        self.extend_piston = extend_piston
        self.retract_piston = retract_piston
        self.shm_tube = shm_tube
        self.broken = broken

    def available(self):
        return self.shm_tube.get() == -1 and not self.broken

    def CenterFromDownCam(self):
        return ConsistentTask(MoveXY(
            tuple(-x for x in self.downcam_offset),
            deadband=0.015,
        ))

    def Extend(self):
        # return SetActuators([self.extend_piston], [self.retract_piston])
        return Sequential(
            Log('Extending {} Amlan'.format(self.name)),
            SetActuators([], [self.retract_piston]),
            Timer(0.3),
            SetActuators([self.extend_piston], []),
        )

    def Retract(self):
        # return SetActuators([self.retract_piston], [self.extend_piston])
        return Sequential(
            # THE THIRD PISTON STATE
            Log('Retracting {} Amlan'.format(self.name)),
            SetActuators([self.extend_piston, self.retract_piston], []),
            Timer(1),
            SetActuators([self.retract_piston], [self.extend_piston]),
            self._RemoveTube()
        )

    def FastRetract(self):
        return Sequential(
            Log('Fast retracting {} Amlan'.format(self.name)),
            SetActuators([self.retract_piston], [self.extend_piston]),
            self._RemoveTube()
        )

    def _RemoveTube(self):
        return FunctionTask(lambda: self.shm_tube.set(-1))

AMLANS = [
    Amlan(
        downcam_offset=(0.25019, -0.22256),
        name='left',
        extend_piston='left_piston_extend',
        retract_piston='left_piston_retract',
        shm_tube=shm.recovery_world_grabbers.left_tube,
        broken=False,
    ),
    Amlan(
        downcam_offset=(0.25019, 0.169672),
        name='right',
        extend_piston='right_piston_extend',
        retract_piston='right_piston_retract',
        shm_tube=shm.recovery_world_grabbers.right_tube,
        broken=False,
    ),
]

class AlignAmlan(Task):
    """
    Align an amlan with a visual object
    """
    def on_first_run(self, vision, obj_func, amlan, align_depth_task, blind=False, align_during_depth=False, *args, **kwargs):
        def tube_angle_heading():
            return shm.kalman.heading.get() + (obj_func().obs.angle % 180 - 90)

        self.must_see_obj = not blind
        def unsee():
            self.must_see_obj = False

        self.task = Sequential(
            Sequential(
                Log('Centering object'),
                CenterCentroid(lambda: vision.coords([obj_func()]), precision=0),
                Zero(),

                Sequential(
                    Log('Aligning to object'),
                    Concurrent(
                        CenterCentroid(lambda: vision.coords([obj_func()]), precision=0),
                        GradualHeading(tube_angle_heading),
                        finite=False,
                    ),
                ) if not align_during_depth else NoOp(),

                Log('Going down and precisely aligning to object'),
                ConsistentTask(Concurrent(
                    GradualHeading(tube_angle_heading),
                    CenterCentroid(lambda: vision.coords([obj_func()]), precision=2),
                    align_depth_task,
                    finite=False,
                )),
                FunctionTask(unsee),
            ) if not blind else NoOp(),

            PositionalControl(),
            Zero(),

            Log('Applying Amlan offset'),
            amlan.CenterFromDownCam(),
        )

    def on_run(self, vision, obj_func, amlan, *args, **kwargs):
        if obj_func() is None and self.must_see_obj:
            self.loge('Lost object, cannot align amlan')
            self.finish(success=False)

        else:
            self.task()
            if self.task.finished:
                self.finish(success=self.task.success)


class Altitude(Task):
    MIN_DEPTH = 0.3
    RELATIVE_DEPTH_OFFSET = 0.05

    def on_first_run(self, altitude, p=0.4, d=0.1, deadband=0.04, *args, **kwargs):
        rel_depth = RelativeToCurrentDepth()
        def relative_offset_depth(out):
            current_altitude = shm.dvl.savg_altitude.get()
            rel_depth(out + math.copysign(
                self.RELATIVE_DEPTH_OFFSET,
                current_altitude - altitude
            ))

        # Remedy for depth overshoots due to bad depth control
        # TODO fix control to make this not necessary
        # TODO fix Conditional to truly work as non-finite
        self.go_down = PIDLoop(
            shm.dvl.savg_altitude.get,
            relative_offset_depth,
            target=altitude,
            p=p,
            d=d,
            negate=True,
            deadband=deadband,
        )
        self.go_up = RelativeToCurrentDepth(-0.3)

    def on_run(self, altitude, overshoot_protect=False, *args, **kwargs):
        if altitude - shm.dvl.savg_altitude.get() > 0.05 and overshoot_protect:
            self.go_up()
        else:
            self.go_down()

        if self.go_down.finished:
            self.finish()

    def on_finish(self, *args, **kwargs):
        Depth(shm.kalman.depth.get())()

class CenterCentroid(Task):
    """
    Center the centroid of all provided objects in the downcam

    Begin: at least one object in view
    End: center of all objects in center of camera

    args:
        points_func: function which returns a list of (x, y) tuples
        corresponding to the coordinates of the points. Its output should not
        vary within a single tick.
    """
    PS = [1.0, 1.0, 0.6]
    DS = [0.3, 0.3, 0.2]
    # DEADBANDS = [(0.1, 0.1), (0.0375, 0.0375), (0.0375, 0.0375)]
    DEADBANDS = [(0.15, 0.15), (0.0675, 0.0675), (0.0475, 0.0475)]
    MAX_SPEED = 0.35

    def centroid(self, points_func):
        points = points_func()
        center_x, center_y = 0, 0
        for x, y in points:
            center_x += x
            center_y += y

        return (center_x / len(points), center_y / len(points))

    def on_first_run(self, points_func, target=(0, 0), precision=0, *args, **kwargs):
        self.task = ConsistentTask(DownwardTarget(
            point=lambda: self.centroid(points_func),
            target=target,
            deadband=self.DEADBANDS[precision],
            px=self.PS[precision],
            py=self.PS[precision],
            dx=self.DS[precision],
            dy=self.DS[precision],
            max_out=self.MAX_SPEED,
        ))

    def on_run(self, points_func, *args, **kwargs):
        if len(points_func()) == 0:
            self.loge("Can't see any objects, targeting aborted")
            self.finish(success=False)

        else:
            self.task()
            if self.task.finished:
                self.finish()

class GradualHeading(Task):

    class GradualApproxHeading(Task):
        def on_first_run(self, *args, **kwargs):
            self.rel_curr_heading = RelativeToCurrentHeading()

        def on_run(self, desire, relative_desire, relative_deadband, *args, **kwargs):
            current = shm.kalman.heading.get()
            diff = heading_sub_degrees(call_if_function(desire), current)
            if abs(diff) < relative_deadband:
                self.finish()
                return

            relative = math.copysign(relative_desire, diff)
            self.rel_curr_heading(relative)

    def on_first_run(self, desire, relative_desire=15, relative_deadband=25, *args, **kwargs):
        self.use_task(Sequential(
            self.GradualApproxHeading(desire, relative_desire, relative_deadband),
            Heading(desire, *args, **kwargs),
            finite=False,
        ))

class GradualDepth(Task):
    RELATIVE_DESIRE = 0.18

    def on_run(self, depth, error=0.08, *args, **kwargs):
        diff = call_if_function(depth) - shm.kalman.depth.get()
        relative = math.copysign(self.RELATIVE_DESIRE, diff)
        RelativeToCurrentDepth(relative)()
        if abs(diff) < error:
            self.finish()

class AlignHeadingToAngle(Task):
    """
    Align the sub's heading to make the given angle appear at a target angle.

    The angle is from objects beneath the sub, is in degrees, starts at 0 from the
    right, and wrap positively counterclockwise (like a standard math angle).
    """

    def on_first_run(self, current, desire, mod=360, *args, **kwargs):
        def desired_heading():
            return shm.kalman.heading.get() - (heading_sub_degrees(
                call_if_function(desire),
                call_if_function(current),
                mod=mod
            ))

        self.use_task(Sequential(
            GradualHeading(desired_heading),
            Heading(desired_heading),
            finite=False,
        ))

class GlobalTimeoutError(Exception):
    pass

# TODO improve speed
# Pitch/roll searching? Navigation spline?
# TODO make work without positional control too
class SearchWithGlobalTimeout(Task):
    def on_first_run(self, timeout=60, *args, **kwargs):
        self.use_task(Timeout(Sequential(
            # Pause initially to give object-identifying tasks time to check current state
            Timer(0.5),

            SpiralSearch(
                relative_depth_range=0,
                optimize_heading=True,
                meters_per_revolution=1,
                min_spin_radius=1,
                deadband=0.2,
            )
        ), timeout))

    def on_finish(self, *args, **kwargs):
        if not self.success:
            self.loge('Timed out while searching')
            raise GlobalTimeoutError()

class Except(Task):
    def on_first_run(self, *args, **kwargs):
        self.excepted = False

    def on_run(self, main_task, except_task, *exceptions, **kwargs):
        if not self.excepted:
            try:
                main_task()
                if main_task.finished:
                    self.finish(success=main_task.success)

                return

            except exceptions:
                self.excepted = True

        except_task()
        if except_task.finished:
            self.finish(success=except_task.success)

class ConsistentTask(Task):
    """
    Finishes when a non-finite task is consistently finished
    """
    def on_first_run(self, task, success=18, total=20, *args, **kwargs):
        self.cons_check = ConsistencyCheck(success, total)

    def on_run(self, task, *args, **kwargs):
        task()
        if self.cons_check.check(task.finished):
            self.finish()

class Disjunction(Task):
    """
    Run tasks in order as they fail, and succeed when the first task does. Fail
    if no task succeeds.

    Disjunction is to Sequential as 'or' is to 'and'.
    """
    def on_first_run(self, *tasks, subtasks=(), finite=True, **kwargs):
        self.use_task(InvertSuccess(Sequential(
            subtasks=[InvertSuccess(t) for t in itertools.chain(tasks, subtasks)]
        )))

# class ConcurrentOr(Concurrent):
    # """ Run tasks concurrently; finish if any finish. """
    # # TODO support finite
    # def on_run(self, *tasks, subtasks=(), finite=True, **kwargs):
        # super().on_run(*tasks, subtasks, finite, **kwargs)

        # iterable = itertools.chain(tasks, subtasks)
        # finished = []
        # for task in iterable:
            # if task.finished:
                # finished.append(task)

        # if len(finished) == 0:
            # return

        # all_success = True
        # for task in finished:
            # all_success = all_success and task.success
        # self.finish(success=all_success)

class StillHeadingSearch(Task):
    """
    Search for an object visible from the current location that is in front of
    the sub with highest probability.
    """

    TIMEOUT = 120

    def on_first_run(self, *args, **kwargs):
        init_heading = shm.kalman.heading.get()

        self.use_task(Timed(
            While(lambda: Sequential(
                # Pause a little to let object-recognizing tasks see the current fov
                Timer(0.5),

                # Check the right side
                GradualHeading(init_heading + 90),
                Timer(0.5),
                Heading(init_heading),

                # Check the left and back
                GradualHeading(init_heading - 90),
                GradualHeading(init_heading - 180),
                GradualHeading(init_heading - 270),
                Timer(0.5),
                Heading(init_heading),

                # Move back a bit, we might be too close
                MoveX(-1),
            ), True),

            self.TIMEOUT,
        ))

    def on_finish(self, *args, **kwargs):
        self.loge('Timed out while searching')
        raise GlobalTimeoutError()

class PrintDone(Task):
    def on_first_run(self, task, *args, **kwargs):
        self.task = task
        self.use_task(task)

    def on_finish(self):
        print('{} is done!'.format(self.task.__class__.__name__))

class Infinite(Task):
    def on_run(self, task, *args, **kwargs):
        task()
        if task.finished and not task.success:
            self.finish(success=False)

def Zeroed(task):
    """ Guarantee that a task is zeroed on completion """
    return Defer(task, Zero())

def WithQuaternionControl(task):
    v = shm.settings_control.quat_pid
    val = v.get()

    return Defer(
        Sequential(FunctionTask(lambda: v.set(True)), task),
        FunctionTask(lambda: v.set(val)),
    )
