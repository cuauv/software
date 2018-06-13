from functools import reduce

from mission.constants.config import buoys as constants

from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent, Conditional
from mission.framework.helpers import call_if_function, ConsistencyCheck, get_sub_position
from mission.framework.movement import Heading, RelativeToInitialHeading, VelocityX, VelocityY, Depth, RelativeToInitialDepth
from mission.framework.position import GoToPosition, MoveX
from mission.framework.primitive import Log, NoOp, Zero, FunctionTask, Succeed
from mission.framework.targeting import ForwardTarget
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.helpers import scaled_speed

import shm
from shm import red_buoy_results
from shm import green_buoy_results
from shm import yellow_buoy_results

# Hit order: Red Green Yellow

BUOY_SURGE_TIMEOUT = 10


def redRight():
    return constants.BUOY_ORDER.index('R') == 2


def greenRight():
    return constants.BUOY_ORDER.index('G') > constants.BUOY_ORDER.index('R')


def yellowRight():
    return constants.BUOY_ORDER.index('Y') > constants.BUOY_ORDER.index('G')


def secondRedRight():
    return constants.BUOY_ORDER.index('R') > constants.BUOY_ORDER.index('Y')


class HeadingRestore(Task):
    """
    Saves the current heading and restores it at a later time
    """

    def __init__(self, heading=None, *args, **kwargs):
        """
        heading - a heading to use as the original heading
        """
        super().__init__(*args, **kwargs)
        # Store the start heading of the sub
        if heading is None:
            self.start_heading = shm.kalman.heading.get()
        else:
            self.start_heading = heading
            self.heading_task = Heading(self.start_heading, error=4)

    def on_run(self):
        # Restore the sub's heading to the stored one
        # self.logv("Running {}".format(self.__class__.__name__))
        if not self.heading_task.finished:
            self.heading_task()
        else:
            self.finish()

    # def on_finish(self):
    #     self.logv('{} task finished in {} seconds!'.format(
    #         self.__class__.__name__,
    #         self.this_run_time - self.first_run_time))


class DepthRestore(Task):
    """
    Saves the current depth and restores it at a later time
    """

    def __init__(self, depth=None, *args, **kwargs):
        """
        depth - a depth to use as the original depth
        """
        super().__init__(*args, **kwargs)
        # Store the start depth of the sub
        if depth is None:
            self.start_depth = constants.BUOY_SEARCH_DEPTH
        else:
            self.start_depth = depth
            self.depth_task = Depth(self.start_depth, error=.01)

    # def on_first_run(self):
    #     self.logv("Starting {} task".format(self.__class__.__name__))

    def on_run(self):
        # Restore the sub's depth to the stored one

        # self.logv("Running {}".format(self.__class__.__name__))
        # self.logv("Des: {}".format(self.start_depth))
        if not self.depth_task.finished:
            self.depth_task()
        else:
            self.finish()

    # def on_finish(self):
    #     self.logv('{} task finished in {} seconds!'.format(
    #         self.__class__.__name__,
    #         self.this_run_time - self.first_run_time))


"""
class LocateBuoyBySpinning(Task):

    Locates a buoy by spinning.

    def __init__(self, validator, *args, **kwargs):

        validator - a function that returns True when a buoy is found and
            False otherwise.

        super().__init__(*args, **kwargs)
        #self.logv("Starting {} task".format(self.__class__.__name__))
        self.validator = validator
        self.start_heading = shm.kalman.heading.get()
        #self.subtasks = [Sequential(RelativeToInitialHeading(60, error=0.1), Timer(1)) for i in range(6)]
        self.subtasks = [
                         Sequential(RelativeToInitialHeading(20, error=0.5), Timer(0.5)),
                         Sequential(RelativeToInitialHeading(25, error=0.5), Timer(0.5)),
                         Sequential(HeadingRestore(self.start_heading), Timer(1.5)),
                         Sequential(RelativeToInitialHeading(-20, error=0.5), Timer(0.5)),
                         Sequential(RelativeToInitialHeading(-25, error=0.5), Timer(0.5)),
                         Sequential(HeadingRestore(self.start_heading), Timer(1.5))
                        ]
        self.spin_task = Sequential(subtasks=self.subtasks)
        self.zero_task = Zero()
        self.TIMEOUT = 20

    def on_run(self):
        # Perform a search for the buoy
        # If the validator() is True, then finish
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            HeadingRestore(self.start_heading)()
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.zero_task()
        #self.logv("Running {}".format(self.__class__.__name__))
        #self.logv("Spin step: {}/{}".format(
            #reduce(lambda acc, x: acc + 1 if x.finished else acc, self.subtasks, 1),
            #len(self.subtasks)))
        self.spin_task()
        if self.validator() or self.spin_task.finished:
            self.finish()

    def on_finish(self):
        #self.logv('{} task finished in {} seconds!'.format(
            #self.__class__.__name__,
            #self.this_run_time - self.first_run_time))
        self.zero_task()
"""


class LocateAdjacentBuoy(Task):
    """
    Locates a buoy using LocateBuoyStrafe
    """

    def __init__(self, validator, right=True, changeDir=True, *args, **kwargs):
        """
        validator - a function that returns True when a buoy is found and False
            otherwise.
        forward - determines whether the submarine should move forward or
            backward during its search
        checkBehind - determines whether the submarine should begin by moving
            backwards to see if the buoy is behind it
        """
        super().__init__(*args, **kwargs)
        self.validator = validator
        self.task_classes = []
        if changeDir:
            self.task_classes.append(
                lambda: LocateBuoyStrafe(validator, right=right))
            self.task_classes.append(lambda: DirectionalSurge(2, .3))
            self.task_classes.append(
                lambda: LocateBuoyStrafe(validator, right=(not right)))
            self.task_classes.append(lambda: DirectionalSurge(3, -.3))
        else:
            self.task_classes.append(
                lambda: LocateBuoyStrafe(validator, right=right))

        self.tasks = []
        self.task_index = 0
        self.TIMEOUT = 60

    def on_first_run(self):
        # self.logv("Starting {} task".format(self.__class__.__name__))
        self.tasks = [x() for x in self.task_classes]

    def on_run(self):
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            # self.loge("{} timed out!".format(self.__class__.__name__))
            return
        # self.logv("Running {}".format(self.__class__.__name__))
        self.tasks[self.task_index]()
        if self.validator():
            self.finish()
        if self.tasks[self.task_index].finished:
            # Reinstantiate subtask, because validator is not true
            self.tasks[self.task_index] = self.task_classes[self.task_index]()
            self.task_index = (self.task_index + 1) % len(self.tasks)

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__, self.this_run_time - self.first_run_time))


class LocateAlignedBuoy(Task):
    """
    Locates a buoy using LocateBuoySurge
    """

    def __init__(self, validator, forward=True, *args, **kwargs):
        """
        validator - a function that returns True when a buoy is found and False
            otherwise.
        forward - determines whether the submarine should move forward or
            backward during its search
        checkBehind - determines whether the submarine should begin by moving
            backwards to see if the buoy is behind it
        """
        super().__init__(*args, **kwargs)
        self.validator = validator
        self.task_classes = [
            lambda: LocateBuoySurge(validator, forward=forward)
        ]

        self.tasks = []
        self.task_index = 0
        self.TIMEOUT = 60

    def on_first_run(self):
        # self.logv("Starting {} task".format(self.__class__.__name__))
        self.tasks = [x() for x in self.task_classes]

    def on_run(self):
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            # self.loge("{} timed out!".format(self.__class__.__name__))
            return
        # self.logv("Running {}".format(self.__class__.__name__))
        self.tasks[self.task_index]()
        if self.validator():
            self.finish()
        if self.tasks[self.task_index].finished:
            # Reinstantiate subtask, because validator is not true
            self.tasks[self.task_index] = self.task_classes[self.task_index]()
            self.task_index = (self.task_index + 1) % len(self.tasks)

    def on_finish(self):
        pass
        # self.logv('{} task finished in {} seconds!'.format(
        #     self.__class__.__name__,
        #     self.this_run_time - self.first_run_time))


class LocateBuoySurge(Task):
    """
    Locates a buoy in front of or behind the current position of the submarine.
    """

    def __init__(self, validator, forward=True, *args, **kwargs):
        """
        validator - a function that returns True when a buoy is found and False
            otherwise.
        forward - determines whether the submarine should move forward or
            backward during its search
        timeout - the amount of time to surge
        """
        super().__init__(*args, **kwargs)
        # self.logv("Starting {} task".format(self.__class__.__name__))
        self.validator = validator
        self.forward = forward
        self.surge_task = VelocityX()
        self.zero_task = Zero()
        self.TIMEOUT = BUOY_SURGE_TIMEOUT

    def on_run(self, forward=None):
        """
        forward - determines whether the submarine should move forward or
            backward during its search
        """
        # Perform a search for the buoy
        # If the validator() is True, then finish
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            # self.loge("{} timed out!".format(self.__class__.__name__))
            return
        # self.logv("Running {}".format(self.__class__.__name__))
        velocity = 0.2 if self.forward else -0.7
        if forward is not None:
            velocity = 0.2 if forward else -0.7
            self.surge_task(velocity)
        if self.validator():
            self.finish()

    def on_finish(self):
        self.zero_task()
        # self.logv('{} task finished in {} seconds!'.format(
        #     self.__class__.__name__,
        #     self.this_run_time - self.first_run_time))


class LocateBuoyStrafe(Task):
    """
    Locates a buoy by strafing in defined direction
    """

    def __init__(self, validator, right=True, timeout=10, *args, **kwargs):
        """
        validator - a function that returns True when a buoy is found and False
            otherwise.
        right - determines whether the submarine should move right or
            left during its search
        timeout - the amount of time to surge
        """
        super().__init__(*args, **kwargs)
        # self.logv("Starting {} task".format(self.__class__.__name__))
        self.validator = validator
        self.right = right
        self.surge_task = VelocityY()
        self.zero_task = Zero()
        self.depth_task = Depth(constants.BUOY_SEARCH_DEPTH)
        self.TIMEOUT = timeout

    def on_run(self, right=True):
        """
        right - determines whether the submarine should move right or
            left during its search
        """
        # Perform a search for the buoy
        # If the validator() is True, then finish
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        velocity = 0.2 if self.right else -0.2
        self.surge_task(velocity)
        self.depth_task()
        if self.validator():
            self.finish()

    def on_finish(self):
        self.zero_task()


class LocateFirstBuoy(Task):
    """
    Locates the first buoy using LocateBuoyStrafe and LocateBuoySurge
    Surges until it sees either the desired buoy or the Middle buoy, then
    strafes.
    """

    def __init__(self,
                 validator,
                 forward=True,
                 right=True,
                 middle=yellow_buoy_results,
                 *args,
                 **kwargs):
        """
        validator - a function that returns True when a buoy is found and False
            otherwise.
        forward - determines whether the submarine should move forward or
            backward during its search
        """
        super().__init__(*args, **kwargs)
        self.validator = validator
        self.middle_buoy = middle
        if self.middle_check:
            self.task_classes = [lambda: LocateBuoyStrafe(validator, right)]
        else:
            self.task_classes = [
                lambda: LocateBuoySurge(validator, forward),
                lambda: LocateBuoyStrafe(validator, right)
            ]
            self.tasks = []
            self.task_index = 0
            self.TIMEOUT = 60

    def on_first_run(self):
        self.logv("Starting {} task".format(self.__class__.__name__))
        self.tasks = [x() for x in self.task_classes]

    def on_run(self):
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.logv("Running {}".format(self.__class__.__name__))
        self.tasks[self.task_index]()
        if self.validator():
            self.finish()
        if self.task_index == 0 and self.middle_check:
            self.tasks[self.task_index].finish()

        if self.tasks[self.task_index].finished:
            # Reinstantiate subtask, because validator is not true
            self.tasks[self.task_index] = self.task_classes[self.task_index]()
            self.task_index = (self.task_index + 1) % len(self.tasks)

    def middle_check(self):
        # Checks if middle buoy is visible and relatively close
        return self.seen_frames_checker.check(
            self.middle_buoy.probability.get() !=
            0) and (self.middle_buoy.percent_frame.get() > 1)


class LocateBuoy(Task):
    """
    Locates a buoy using LocateBuoyBySpinning and LocateBuoySurge
    """

    def __init__(self,
                 validator,
                 forward=True,
                 checkBehind=False,
                 *args,
                 **kwargs):
        """
        validator - a function that returns True when a buoy is found and False
            otherwise.
        forward - determines whether the submarine should move forward or
            backward during its search
        checkBehind - determines whether the submarine should begin by moving
            backwards to see if the buoy is behind it
        """
        super().__init__(*args, **kwargs)
        self.validator = validator
        self.task_classes = [
            lambda: LocateBuoyBySpinning(validator),
            lambda: LocateBuoySurge(validator, forward)
        ]
        if checkBehind:
            self.task_classes.insert(0,
                                     lambda: LocateBuoySurge(validator, False))
            self.tasks = []
            self.task_index = 0
            self.TIMEOUT = 60

    def on_first_run(self):
        # self.logv("Starting {} task".format(self.__class__.__name__))
        self.tasks = [x() for x in self.task_classes]

    def on_run(self):
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            # self.loge("{} timed out!".format(self.__class__.__name__))
            return
        # self.logv("Running {}".format(self.__class__.__name__))
        self.tasks[self.task_index]()
        if self.validator():
            self.finish()
        if self.tasks[self.task_index].finished:
            # Reinstantiate subtask, because validator is not true
            self.tasks[self.task_index] = self.task_classes[self.task_index]()
            self.task_index = (self.task_index + 1) % len(self.tasks)

    # def on_finish(self):
    #     self.logv('{} task finished in {} seconds!'.format(
    #         self.__class__.__name__,
    #         self.this_run_time - self.first_run_time))


class PreventSurfacing(Task):
    def on_first_run(self, *args, **kwargs):
        pass

    def on_run(self, *args, **kwargs):
        if shm.kalman.depth.get() < .3:
            self.loge('Tried to surface, killing')
            self.finish(success=False)


class AlignTarget(Task):
    """
    Aligns using ForwardTarget on a target coordinate, while ensuring that the
    target is visible
    """

    def __init__(self,
                 validator,
                 locator_task,
                 target_coords,
                 vision_group,
                 heading_task,
                 forward_target_p=0.001,
                 *args,
                 **kwargs):
        """
        validator - a function that returns True when the target is visible and
            False otherwise.
        locator_task - a task that locates the target
        target_coords - the coordinates of the target with which to align
        vision_group - the shm group for the buoy
        """
        super().__init__(*args, **kwargs)
        self.validator = validator
        self.locator_task = locator_task
        self.heading_task = heading_task

        def get_center():
            return (0, 0)

        # TODO use normalized coordinates instead
        self.target_task = ForwardTarget(
            target_coords,
            target=get_center,
            px=forward_target_p,
            dx=forward_target_p / 2,
            py=forward_target_p,
            dy=forward_target_p / 2,
            deadband=(30, 30))
        self.target_checker = ConsistencyCheck(5, 5, strict=True)

        self.TIMEOUT = 60

    # def on_first_run(self):
    #     self.logv("Starting {} task".format(self.__class__.__name__))

    def on_run(self):
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            # self.loge("{} timed out!".format(self.__class__.__name__))
            return
        # self.logv("Running {}".format(self.__class__.__name__))
        # if shm.kalman.depth.get() < .4:
        #     self.giveup_task()
        #     self.finish()

        if self.validator():
            # if abs(shm.kalman.depth.get() - BUOY_SEARCH_DEPTH) >= BUOY_DEPTH_VARIANCE:
            #     self.depthless_target()
            # else:
            self.target_task()
        else:
            #   self.heading_task()
            #    HeadingRestore()
            self.logv('lost buoy? searching and restoring depth')
            Depth(constants.BUOY_SEARCH_DEPTH)
            self.locator_task()
        if self.target_checker.check(self.target_task.finished):
            self.finish()

    # def on_finish(self):
    #     self.logv('{} task finished in {} seconds!'.format(
    #         self.__class__.__name__,
    #         self.this_run_time - self.first_run_time))


class RamTarget(Task):
    """
    Moves forward until collision with an object at a given coordinate in the
    yz-plane.
    """

    def __init__(self,
                 target_validator,
                 collision_validator,
                 locator_task,
                 concurrent_task=NoOp(),
                 ram_speed=None,
                 *args,
                 **kwargs):
        """
        target_validator - a function that returns True when a target is
            visible and False otherwise.
        collision_validator - a function that returns True when a collision is
            made and False otherwise.
        concurrent_task - an optional argument for a task to run while moving
            forward to ram the target. It may be used to continually align with
            the target while ramming it.
        ram_speed - a function that returns a speed at which to ram the target
        """
        super().__init__(*args, **kwargs)
        # self.logv("Starting {} task".format(self.__class__.__name__))
        self.target_validator = target_validator
        self.collision_validator = collision_validator
        self.ram_speed = ram_speed
        self.ram_task = VelocityX()
        self.locator_task = locator_task
        self.concurrent_task = concurrent_task
        self.commit_task = Sequential(VelocityX(1), Timer(1), VelocityX(0))
        self.ram_commit_phase = False
        self.TIMEOUT = 25

    def on_run(self):
        # Move forward for ramming target
        # If the validator function returns True, then finish the task
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            # self.loge("{} timed out!".format(self.__class__.__name__))
            return
        # self.logv("Running {}".format(self.__class__.__name__))

        if self.target_validator():
            if self.ram_speed is not None:
                speed = self.ram_speed()
            else:
                speed = 0.3
                self.ram_task(speed)
        else:
            self.locator_task()

        if self.collision_validator():
            self.ram_commit_phase = True
            self.finish()

        # Don't run concurrent task if we're committing the ram!
        if self.concurrent_task:
            self.concurrent_task()

    def on_finish(self):
        # self.logv('{} task finished in {} seconds!'.format(
        #     self.__class__.__name__,
        #     self.this_run_time - self.first_run_time))
        Zero()()


class DirectionalSurge(Task):
    """
    Backs away from rammed target
    """

    def __init__(self, timeout, speed=.3, compensate=False, *args, **kwargs):

        super().__init__(*args, **kwargs)
        # self.logv("Starting {} task".format(self.__class__.__name__))
        self.speed = speed
        self.ram_task = VelocityX()
        self.strafe_task = VelocityY()
        self.commit_task = Sequential(VelocityX(1), Timer(1), VelocityX(0))
        self.ram_commit_phase = False
        self.TIMEOUT = timeout
        self.compensate = compensate

    def on_run(self):
        # Move backward
        # If the validator function returns True, then finish the task
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            return
        # self.logv("Running {}".format(self.__class__.__name__))
        self.ram_task(self.speed)
        if self.compensate:
            self.strafe_task(.1)

    def on_finish(self):
        # self.logv('{} task finished in {} seconds!'.format(
        #     self.__class__.__name__,
        #     self.this_run_time - self.first_run_time))
        Zero()()


class SeqLog(Task):
    def __init__(self, message, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.message = message

    def on_run(self):
        self.logv(self.message)
        self.finish()


class BuoyAlign(Task):
    def __init__(self,
                 location_validator,
                 target_coordinates,
                 vision_group,
                 collision_validator,
                 ram_concurrent_task=NoOp(),
                 first_buoy=False,
                 right=True,
                 yellow=False,
                 *args,
                 **kwargs):
        super().__init__(*args, **kwargs)
        self.logv("Starting {} task".format(self.__class__.__name__))
        self.location_validator = location_validator
        self.target_coordinates = target_coordinates
        self.collision_validator = collision_validator
        self.ram_concurrent_task = ram_concurrent_task
        self.heading_task = HeadingRestore()
        if first_buoy:
            self.locator_task_a = LocateAlignedBuoy(
                self.location_validator, forward=True)
        else:
            self.locator_task_a = LocateAdjacentBuoy(
                self.location_validator, right=right)
            self.locator_task_b = LocateAlignedBuoy(
                self.location_validator, forward=False)

        self.align_task = AlignTarget(
            self.location_validator, self.locator_task_a,
            self.target_coordinates, vision_group, self.heading_task)
        self.ram_task = RamTarget(
            self.location_validator,
            self.collision_validator,
            # LocateBuoy(self.location_validator,checkBehind=True),
            self.locator_task_b,
            self.ram_concurrent_task)

        if yellow:
            self.forward_task = DirectionalSurge(6, .5)
            self.retreat_task = DirectionalSurge(4, -.5)
        else:
            self.forward_task = DirectionalSurge(constants.FORWARD_TIME, .2)
            self.retreat_task = DirectionalSurge(constants.BACKUP_TIME, -1)

        self.depth_task = DepthRestore()
        self.tasks = Sequential(
            Zero(),
            SeqLog("Restoring Depth"),
            self.depth_task,
            SeqLog("Locating Buoy"),
            self.locator_task_a,
            SeqLog("Aligning"),
            self.align_task,
            SeqLog("Approaching"),
            self.ram_task,
            Zero(),
            Timer(.5), )

        # self.tasks = Sequential(Zero(), self.depth_task, self.locator_task, self.align_task,
        #                         self.ram_task, self.forward_task, self.retreat_task,
        #                         self.heading_task
        #                         )
        self.TIMEOUT = 90

    def on_run(self):
        # Locate the buoy
        # Align with the buoy
        # Ram the buoy
        # Fulfill postcondition
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.tasks()
        if self.tasks.finished:
            self.finish()

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__, self.this_run_time - self.first_run_time))


class BuoyRam(Task):
    """
    Locates and rams a buoy.

    Precondition: The target buoy is located at a position (x-coordinate) in
    front of the position of the submarine.

    Postcondition: The submarine will have rammed the buoy and will be
    positioned at the same depth as determined by the target coordinates. The
    original heading of the submarine prior to the collision will be maintained
    after the collision is complete.
    """

    def __init__(self,
                 location_validator,
                 target_coordinates,
                 vision_group,
                 collision_validator,
                 ram_concurrent_task=NoOp(),
                 first_buoy=False,
                 right=True,
                 yellow=False,
                 *args,
                 **kwargs):
        """
        location_validator - a function that returns True when the target has
            been found and False otherwise
        target_coordinates - a tuple representing the coordinates of the target
            in the xz-plane
        vision_group - the shm group for the buoy
        collision_validator - a function that returns True when there has been
            a collision with the target and False otherwise.
        ram_concurrent_task - an optional task to run concurrently when ramming
            the target
        """
        super().__init__(*args, **kwargs)
        self.logv("Starting {} task".format(self.__class__.__name__))
        self.location_validator = location_validator
        self.target_coordinates = target_coordinates
        self.collision_validator = collision_validator
        self.ram_concurrent_task = ram_concurrent_task
        self.heading_task = HeadingRestore()
        if first_buoy:
            self.locator_task_a = LocateAlignedBuoy(
                self.location_validator, forward=True)
        else:
            self.locator_task_a = LocateAdjacentBuoy(
                self.location_validator, right=right)
            self.locator_task_b = LocateAlignedBuoy(
                self.location_validator, forward=False)

        self.align_task = AlignTarget(
            self.location_validator, self.locator_task_a,
            self.target_coordinates, vision_group, self.heading_task)
        self.ram_task = RamTarget(
            self.location_validator,
            self.collision_validator,
            # LocateBuoy(self.location_validator,checkBehind=True),
            self.locator_task_b,
            self.ram_concurrent_task)

        if yellow:
            self.forward_task = DirectionalSurge(6, .5)
            self.retreat_task = DirectionalSurge(3, -.5)
        else:
            self.forward_task = DirectionalSurge(constants.FORWARD_TIME, .2)
            self.retreat_task = DirectionalSurge(constants.BACKUP_TIME, -1)

        self.depth_task = DepthRestore()
        self.tasks = Sequential(Zero(),
                                SeqLog("Restoring Depth"), self.depth_task,
                                SeqLog("Locating Buoy"), self.locator_task_a,
                                SeqLog("Aligning"), self.align_task,
                                SeqLog("Approaching"), self.ram_task,
                                Zero(),
                                Timer(.5),
                                SeqLog("Ramming"), self.forward_task,
                                SeqLog("Backing up"), self.retreat_task,
                                SeqLog("Restoring heading"), self.heading_task)

        # self.tasks = Sequential(Zero(), self.depth_task, self.locator_task, self.align_task,
        #                         self.ram_task, self.forward_task, self.retreat_task,
        #                         self.heading_task)
        self.TIMEOUT = 90

    def on_run(self):
        # Locate the buoy
        # Align with the buoy
        # Ram the buoy
        # Fulfill postcondition
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.tasks()
        if self.tasks.finished:
            self.finish()

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__, self.this_run_time - self.first_run_time))


class Buoy(Task):
    """
    Wrapper around the BuoyRam class that will specifically ram a red or green
    buoy
    """

    def __init__(self,
                 buoy,
                 right=True,
                 first_buoy=False,
                 yellow=False,
                 align_only=False,
                 *args,
                 **kwargs):
        super().__init__(*args, **kwargs)
        # Instantiate the BuoyRam task
        self.buoy = buoy
        self.heading_task = HeadingRestore()

        if align_only:
            self.align_task = AlignTarget(self.location_validator,
                                          LocateAdjacentBuoy(
                                              self.location_validator,
                                              right=right),
                                          (self.buoy.r_side_x.get,
                                           self.buoy.center_y.get), self.buoy,
                                          self.heading_task)
            self.ram_task = BuoyAlign(
                self.location_validator, (self.buoy.r_side_x.get,
                                          self.buoy.center_y.get),
                self.buoy,
                self.collision_validator,
                self.align_task,
                right=right,
                first_buoy=first_buoy,
                yellow=yellow)

        elif yellow:
            self.align_task = AlignTarget(self.location_validator,
                                          LocateAdjacentBuoy(
                                              self.location_validator,
                                              right=right),
                                          (self.buoy.r_side_x.get,
                                           self.buoy.bottom_y.get), self.buoy,
                                          self.heading_task)
            self.ram_task = BuoyRam(
                self.location_validator, (self.buoy.r_side_x.get,
                                          self.buoy.bottom_y.get),
                self.buoy,
                self.collision_validator,
                self.align_task,
                right=right,
                first_buoy=first_buoy,
                yellow=yellow)
        else:
            self.align_task = AlignTarget(self.location_validator,
                                          LocateAdjacentBuoy(
                                              self.location_validator,
                                              right=right),
                                          (self.buoy.r_side_x.get,
                                           self.buoy.center_y.get), self.buoy,
                                          self.heading_task)
            self.ram_task = BuoyRam(
                self.location_validator, (self.buoy.r_side_x.get,
                                          self.buoy.center_y.get),
                self.buoy,
                self.collision_validator,
                self.align_task,
                right=right,
                first_buoy=first_buoy,
                yellow=yellow)

        self.seen_frames_checker = ConsistencyCheck(3, 3, strict=True)
        self.collision_checker = ConsistencyCheck(2, 2, strict=True)
        self.last_percent_frame = 0
        self.PERCENT_FRAME_THRESHOLD = 2.5
        self.PERCENT_FRAME_DELTA_THRESHOLD = 10
        self.TIMEOUT = 100

    def on_first_run(self):
        self.logv("Starting {} task".format(self.__class__.__name__))

    def on_run(self):
        # Perform BuoyRam task
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("Buoy ({}) timed out!".format(self.buoy))
            return
        self.ram_task()
        if self.ram_task.finished:
            self.finish()

    def on_finish(self):
        self.logv("Buoy ({}) task finished in {} seconds!".format(
            self.buoy, self.this_run_time - self.first_run_time))
        Zero()()

    def location_validator(self):
        # TODO even more robust location validator
        return self.seen_frames_checker.check(self.buoy.probability.get() != 0)

    def collision_validator(self):
        # TODO even more robust collision validator, susceptible to false
        # positives
        # if not shm.gpio.wall_1.get():
        #     self.logi("Detected buoy ram using touch sensor!")
        #     return True

        current = self.buoy.percent_frame.get()
        # self.logv("Buoy Percent Frame : {}".format(current))
        if current >= self.PERCENT_FRAME_THRESHOLD:
            # if self.collision_checker.check(abs(self.last_percent_frame - current) <= self.PERCENT_FRAME_DELTA_THRESHOLD):
            # self.logv("Returned true!")
            return True
        # self.last_percent_frame = current
        return False


class AllBuoys(Task):
    def desiredModules(self):
        return [shm.vision_modules.Buoys]

    def on_first_run(self):
        self.has_made_progress = True
        self.seen_frames_checker = ConsistencyCheck(3, 3, strict=False)

        def location_validator(buoy):
            return self.seen_frames_checker.check(buoy.probability.get() != 0)

        self.depth_task = DepthRestore()
        self.heading_task = HeadingRestore()
        self.up_task = DepthRestore(constants.BUOY_OVER_DEPTH)
        self.dodge_vel = -.4 if yellowRight else .4
        self.over_task = Timed(VelocityX(.4), 8)
        self.heading_task = HeadingRestore()
        self.task = Sequential(
            Depth(constants.BUOY_SEARCH_DEPTH),
            Conditional(
                MasterConcurrent(
                    Sequential(
                        self.heading_task,
                        # Depth(0.9, error=.01)
                        # SeqLog("Looking for red buoy"), LocateFirstBuoy(lambda: location_validator(red_buoy_results), forward=True, right=BUOY_RIGHT_TO_REACH[0], middle=yellow_buoy_results),
                        Buoy(
                            red_buoy_results,
                            first_buoy=True,
                            right=redRight()),
                        # self.depth_task,
                        # SeqLog("Looking for green buoy stage 1"), LocateBuoyStrafe(lambda: location_validator(yellow_buoy_results), right=True, timeout=3),
                        SeqLog("Looking for green buoy"),
                        LocateBuoyStrafe(
                            lambda: location_validator(green_buoy_results),
                            right=greenRight(),
                            timeout=3),
                        Buoy(green_buoy_results, right=greenRight()),
                        # self.depth_task,
                        SeqLog("Looking for yellow buoy"),
                        LocateBuoyStrafe(
                            lambda: location_validator(yellow_buoy_results),
                            right=yellowRight(),
                            timeout=2),
                        Buoy(
                            yellow_buoy_results,
                            right=yellowRight(),
                            yellow=True),
                        Log("re-aligning red buoy"),
                        LocateBuoyStrafe(
                            lambda: location_validator(red_buoy_results),
                            right=secondRedRight(),
                            timeout=2),
                        Buoy(
                            red_buoy_results,
                            right=secondRedRight(),
                            yellow=False,
                            align_only=True), ),
                    PreventSurfacing(), ),
                on_success=Sequential(
                    Zero(),
                    self.heading_task,
                    SeqLog("Rising to Over depth"),
                    self.up_task,
                    SeqLog("Going over buoys"),
                    self.over_task, ),
                on_fail=Sequential(
                    Zero(),
                    self.heading_task,
                    SeqLog("Going to Over depth"),
                    self.up_task,
                    SeqLog("Going over buoys"),
                    self.over_task,
                    Timed(VelocityX(.4), 8), )))

    def on_run(self):
        if self.task.finished:
            self.finish()
        else:
            self.task()


class RetryBuoys(Task):
    def desiredModules(self):
        return [shm.vision_modules.Buoys]

    def on_first_run(self):
        self.has_made_progress = True
        self.seen_frames_checker = ConsistencyCheck(3, 3, strict=False)

        def location_validator(buoy):
            return self.seen_frames_checker.check(buoy.probability.get() != 0)

        self.depth_task = DepthRestore()
        self.heading_task = HeadingRestore()
        self.up_task = DepthRestore(constants.BUOY_OVER_DEPTH)
        self.dodge_vel = -.4 if yellowRight else .4
        self.over_task = Sequential(
            Timed(VelocityY(self.dodge_vel), 2),
            DirectionalSurge(6, .4, compensate=True))
        self.heading_task = HeadingRestore()
        self.task = Sequential(
            Depth(constants.BUOY_SEARCH_DEPTH),
            self.heading_task,
            # Depth(0.9, error=.01)
            # SeqLog("Looking for red buoy"), LocateFirstBuoy(lambda: location_validator(red_buoy_results), forward=True, right=BUOY_RIGHT_TO_REACH[0], middle=yellow_buoy_results),
            Buoy(red_buoy_results, first_buoy=True, right=not redRight()),
            # self.depth_task,
            # SeqLog("Looking for green buoy stage 1"), LocateBuoyStrafe(lambda: location_validator(yellow_buoy_results), right=True, timeout=3),
            SeqLog("Looking for green buoy"),
            LocateBuoyStrafe(
                lambda: location_validator(green_buoy_results),
                right=not greenRight(),
                timeout=3),
            Buoy(green_buoy_results, right=not greenRight()),
            # self.depth_task,
            SeqLog("Looking for yellow buoy"),
            LocateBuoyStrafe(
                lambda: location_validator(yellow_buoy_results),
                right=not yellowRight(),
                timeout=2),
            Buoy(yellow_buoy_results, right=not yellowRight(), yellow=True),
            HeadingRestore(),
            SeqLog("Rising to Over depth"),
            self.up_task,
            SeqLog("Going around buoys"),
            self.over_task)

    def on_run(self):
        if self.task.finished:
            self.finish()
        else:
            self.task()


red = lambda: Buoy(red_buoy_results, first_buoy=True)
green = lambda: Buoy(green_buoy_results, first_buoy=True)
yellow = lambda: Buoy(yellow_buoy_results, first_buoy=True)
