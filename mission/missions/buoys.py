from functools import reduce

from mission.framework.combinators import Sequential, Concurrent, MasterConcurrent
from mission.framework.helpers import call_if_function, ConsistencyCheck, get_sub_position, get_forward_camera_center
from mission.framework.movement import Heading, RelativeToInitialHeading, VelocityX, VelocityY, Depth, RelativeToInitialDepth
from mission.framework.position import GoToPosition, MoveX
from mission.framework.primitive import Log, NoOp, Zero, FunctionTask
from mission.framework.targeting import ForwardTarget
from mission.framework.task import Task
from mission.framework.timing import Timer, Timed
from mission.helpers import scaled_speed

import shm
from shm import red_buoy_results
from shm import green_buoy_results
from shm import yellow_buoy_results

class HeadingRestore(Task):
    """
    Saves the current heading and restores it at a later time
    """
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Store the start heading of the sub
        self.start_heading = shm.kalman.heading.get()
        self.heading_task = Heading(self.start_heading, error=1)

    def on_first_run(self):
        self.logv("Starting {} task".format(self.__class__.__name__))

    def on_run(self):
        # Restore the sub's heading to the stored one
        self.logv("Running {}".format(self.__class__.__name__))
        if not self.heading_task.finished:
            self.heading_task()
        else:
            self.finish()

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__,
            self.this_run_time - self.first_run_time))

class LocateBuoyBySpinning(Task):
    """
    Locates a buoy by spinning.
    """
    def __init__(self, validator, *args, **kwargs):
        """
        validator - a function that returns True when a buoy is found and
            False otherwise.
        """
        super().__init__(*args, **kwargs)
        self.logv("Starting {} task".format(self.__class__.__name__))
        self.validator = validator
        self.start_heading = shm.kalman.heading.get()
        self.subtasks = [Sequential(RelativeToInitialHeading(60, error=0.1), Timer(1)) for i in range(6)]
        self.spin_task = Sequential(subtasks=self.subtasks)
        self.TIMEOUT = 20

    def on_run(self):
        # Perform a search for the buoy
        # If the validator() is True, then finish
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.logv("Running {}".format(self.__class__.__name__))
        self.logv("Spin step: {}/{}".format(
            reduce(lambda acc, x: acc + 1 if x.finished else acc, self.subtasks, 1),
            len(self.subtasks)))
        self.spin_task()
        if self.validator() or self.spin_task.finished:
            self.finish()

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__,
            self.this_run_time - self.first_run_time))
        Zero()()

class LocateBuoySurge(Task):
    """
    Locates a buoy in front of or behind the current position of the submarine.
    """
    def __init__(self, validator, *args, **kwargs):
        """
        validator - a function that returns True when a buoy is found and False
            otherwise.
        """
        super().__init__(*args, **kwargs)
        self.logv("Starting {} task".format(self.__class__.__name__))
        self.validator = validator
        self.surge_task = VelocityX()
        self.TIMEOUT = 5

    def on_run(self, forward=True):
        """
        forward - determines whether the submarine should move forward or
            backward during its search
        """
        # Perform a search for the buoy
        # If the validator() is True, then finish
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.logv("Running {}".format(self.__class__.__name__))
        velocity = 1 if forward else -1
        self.surge_task(velocity)
        if self.validator():
            self.finish()

    def on_finish(self):
        self.surge_task(0)
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__,
            self.this_run_time - self.first_run_time))

class AssumeBuoy(Task):
    def on_run(self, _):
        self.finish()

class LocateBuoy(Task):
    """
    Locates a buoy using LocateBuoyBySpinning and LocateBuoySurge
    """
    def __init__(self, validator, forward=True, *args, **kwargs):
        """
        validator - a function that returns True when a buoy is found and False
            otherwise.
        forward - determines whether the submarine should move forward or
            backward during its search
        """
        super().__init__(*args, **kwargs)
        self.validator = validator
        self.task_classes = [lambda: LocateBuoyBySpinning(validator),
                             lambda: LocateBuoySurge(validator, forward)]
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
        if self.tasks[self.task_index].finished:
            if self.validator():
                self.finish()
            else:
                # Reinstantiate subtask, because validator is not true
                self.tasks[self.task_index] = self.task_classes[self.task_index]()
                self.task_index = (self.task_index + 1) % len(self.tasks)

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__,
            self.this_run_time - self.first_run_time))

class AlignTarget(Task):
    """
    Aligns using ForwardTarget on a target coordinate, while ensuring that the
    target is visible
    """
    def __init__(self, validator, locator_task, target_coords, vision_group, forward_target_p=0.003, *args, **kwargs):
        """
        validator - a function that returns True when the target is visible and False
            otherwise.
        locator_task - a task that locates the target
        target_coords - the coordinates of the target with which to align
        """
        super().__init__(*args, **kwargs)
        self.validator = validator
        self.locator_task = locator_task
        def get_center():
            return get_forward_camera_center()

        self.target_task = ForwardTarget(target_coords, target=get_center,
                px=forward_target_p, dx=forward_target_p/3,
                py=forward_target_p, dy=forward_target_p/3, deadband=(30, 30))
        self.target_checker = ConsistencyCheck(10, 10)

        self.TIMEOUT = 60

    def on_first_run(self):
        self.logv("Starting {} task".format(self.__class__.__name__))

    def on_run(self):
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.logv("Running {}".format(self.__class__.__name__))
        if self.validator():
            self.target_task()
        else:
            self.locator_task()
        if self.target_checker.check(self.target_task.finished):
            self.finish()

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__,
            self.this_run_time - self.first_run_time))

class RamTarget(Task):
    """
    Moves forward until collision with an object at a given coordinate in the
    yz-plane.
    """
    def __init__(self, target_validator, collision_validator, locator_task, concurrent_task=NoOp(), ram_speed=None, *args, **kwargs):
        """
        target_validator - a function that returns True when a target is
            visible and False otherwise.
        collision_validator - a function that returns True when a collision is
            made and False otherwise.
        concurrent_task - an optional argument for a task to run while moving
            forward to ram the target. It may be used to continually align with
            the target while ramming it.
        """
        super().__init__(*args, **kwargs)
        self.logv("Starting {} task".format(self.__class__.__name__))
        self.target_validator = target_validator
        self.collision_validator = collision_validator
        self.ram_speed = ram_speed
        self.ram_task = VelocityX()
        self.locator_task = locator_task
        self.concurrent_task = concurrent_task
        self.TIMEOUT = 25

    def on_run(self):
        # Move forward for ramming target
        # If the validator function returns True, then finish the task
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.logv("Running {}".format(self.__class__.__name__))
        if self.target_validator():
            if self.ram_speed is not None:
              speed = self.ram_speed()
            else:
              speed = 0.6

            self.ram_task(speed)
        else:
            self.locator_task()
        if self.concurrent_task:
            self.concurrent_task()
        if self.collision_validator():
            self.finish()

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__,
            self.this_run_time - self.first_run_time))
        Zero()()

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
    def __init__(self, location_validator, target_coordinates, vision_group,
            collision_validator, ram_concurrent_task=NoOp(), *args, **kwargs):
        """
        location_validator - a function that returns True when the target has
            been found and False otherwise
        target_coordinates - a tuple representing the coordinates of the target
            in the xz-plane
        collision_validator - a function that returns True when there has been a
            collision with the target and False otherwise.
        ram_concurrent_task - an optional task to run concurrently when ramming
            the target
        """
        super().__init__(*args, **kwargs)
        self.logv("Starting {} task".format(self.__class__.__name__))
        self.location_validator = location_validator
        self.target_coordinates = target_coordinates
        self.collision_validator = collision_validator
        self.ram_concurrent_task = ram_concurrent_task
        self.locator_task = AssumeBuoy(self.location_validator)
        self.align_task = AlignTarget(self.location_validator,
                self.locator_task, self.target_coordinates, vision_group)
        self.ram_task = RamTarget(self.location_validator,
                self.collision_validator, self.locator_task,
                self.ram_concurrent_task)
        self.heading_task = HeadingRestore()
        self.tasks = Sequential(Zero(), self.locator_task, self.align_task,
                self.ram_task)
                #self.heading_task)
        self.TIMEOUT = 60

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
            self.__class__.__name__,
            self.this_run_time - self.first_run_time))

class Buoy(Task):
    """
    Wrapper around the BuoyRam class that will specifically ram a red or green
    buoy
    """
    def __init__(self, buoy, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # Instantiate the BuoyRam task
        self.buoy = buoy
        self.align_task = AlignTarget(self.location_validator,
                AssumeBuoy(self.location_validator),
                (self.buoy.center_x.get, self.buoy.center_y.get), self.buoy)
        self.ram_task = BuoyRam(self.location_validator,
                (self.buoy.center_x.get, self.buoy.center_y.get), self.buoy,
                self.collision_validator, self.align_task)
        self.seen_frames_checker = ConsistencyCheck(5, 5)
        self.last_percent_frame = 0
        self.PERCENT_FRAME_THRESHOLD = 10
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
        # TODO even more robust collision validator
        if not shm.gpio.wall_1.get():
            self.logi("Detected buoy ram using touch sensor!")
            return True

        current = self.buoy.percent_frame.get()
        if current >= self.PERCENT_FRAME_THRESHOLD:
            if abs(self.last_percent_frame - current) <= self.PERCENT_FRAME_DELTA_THRESHOLD:
                return True
            self.last_percent_frame = current
        return False

class ScuttleYellowBuoy(Task):
    """
    Locates and scuttles a yellow buoy by dragging it down.

    Precondition: The yellow buoy is located at a position in front of the
    position of the submarine.

    Postcondition: The submarine will have dragged down the yellow buoy, and the
    submarine will be positioned above the yellow buoy.
    """
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.locator_task = LocateBuoy(self.location_validator)
        self.locator_task = AssumeBuoy(self.location_validator)
        self.align_task = AlignTarget(self.location_validator,
                self.locator_task, (yellow_buoy_results.top_x.get,
            yellow_buoy_results.top_y.get), yellow_buoy_results, forward_target_p=0.002)
        self.concurrent_align_task = AlignTarget(self.location_validator,
                self.locator_task, (yellow_buoy_results.top_x.get,
            yellow_buoy_results.top_y.get), yellow_buoy_results, forward_target_p=0.002)

        self.MAX_RAM_SPEED = 0.6
        self.ram_speed = self.MAX_RAM_SPEED
        self.ram_task = RamTarget(self.location_validator,
                self.collision_validator, self.locator_task,
                self.concurrent_align_task, ram_speed=lambda: self.ram_speed)

        self.ready_to_ram_checker = ConsistencyCheck(40, 40)
        self.seen_frames_checker = ConsistencyCheck(5, 5)

        SCUTTLE_TIME = 2.0
        BUOY_MOUNTING_DISTANCE = 0.55
        self.scuttle = Sequential(Log("Rising to prepare for scuttle"),
                                  RelativeToInitialDepth(-0.2, error=0.02),
                                  Log("Moving on top of Yellow Buoy"),
                                  MoveX(BUOY_MOUNTING_DISTANCE),
                                  Log("Beginning %0.2f second scuttle!" % SCUTTLE_TIME),
                                  MasterConcurrent(Timer(SCUTTLE_TIME), VelocityX(1.0), RelativeToInitialDepth(3.0, error=0.03)),
                                  Log("Scuttle complete. Returning to targeting position."),
                                  Depth(1.0)
                                  # GoToPosition(lambda: self.target_position[0], lambda: self.target_position[1], depth=lambda: self.target_depth)
                                  )

        self.tasks = Sequential(self.locator_task, self.align_task,
                                self.ram_task, self.scuttle)

        #self.last_percent_frame = 0
        #self.PERCENT_FRAME_DELTA_THRESHOLD = 10

        self.PERCENT_FRAME_THRESHOLD = 2
        self.PERCENT_FRAME_SLOWDOWN_THRESHOLD = 0.4
        self.TIMEOUT = 100

    def on_first_run(self):
        self.logv("Starting {} task".format(self.__class__.__name__))

    def on_run(self):
        # Locate the yellow buoy
        # Align with the yellow buoy
        # Move forward until collision with the buoy (using RamTarget)
        # Descend to drag buoy downwards
        if self.this_run_time - self.first_run_time > self.TIMEOUT:
            self.finish()
            self.loge("{} timed out!".format(self.__class__.__name__))
            return
        self.tasks()
        if self.tasks.finished:
            self.finish()

    def on_finish(self):
        self.logv('{} task finished in {} seconds!'.format(
            self.__class__.__name__,
            self.this_run_time - self.first_run_time))
        Zero()()

    def location_validator(self):
        # TODO even more robust location validator
        return self.seen_frames_checker.check(yellow_buoy_results.probability.get() != 0)

    def collision_validator(self):
        # TODO even more robust collision validator
        current = yellow_buoy_results.percent_frame.get()

        aligned = self.concurrent_align_task.finished
        max_ram_speed = self.MAX_RAM_SPEED
        if not aligned:
          max_ram_speed /= 3

        self.ram_speed = scaled_speed(final_value=self.PERCENT_FRAME_THRESHOLD + 1,
                                      initial_value=self.PERCENT_FRAME_SLOWDOWN_THRESHOLD,
                                      final_speed=0.0, initial_speed=max_ram_speed,
                                      current_value=current)

        if self.ready_to_ram_checker.check(current >= self.PERCENT_FRAME_THRESHOLD and \
                                           aligned):
            self.target_position = get_sub_position()
            self.target_depth = shm.kalman.depth.get()
            self.logi("Close enough to yellow buoy to scuttle!")
            return True
            #if yellow_buoy_results.center_y.get() > (shm.camera.forward_height.get() - 10) \
            #   and abs(self.last_percent_frame - current) <= self.PERCENT_FRAME_DELTA_THRESHOLD:
            #    return True
            #self.last_percent_frame = current
        return False

red = lambda: Buoy(red_buoy_results)
green = lambda: Buoy(green_buoy_results)
ram = lambda: Sequential(Buoy(red_buoy_results), Sequential(VelocityX(-1,
    error=0.5), Timer(2)), Buoy(green_buoy_results))
#scuttle = lambda: ScuttleYellowBuoy()
#full = lambda: Sequential(ram(), Sequential(VelocityX(-1, error=0.5), Timer(2)),
        #scuttle())

SCUTTLE_TIME = 3.0
BUOY_MOUNTING_DISTANCE = 0.55
Scuttle = lambda: Sequential(Log("Rising to prepare for scuttle"),
                     RelativeToInitialDepth(-0.2, error=0.02),
                     Log("Moving on top of Yellow Buoy"),
                     MoveX(BUOY_MOUNTING_DISTANCE),
                     Log("Beginning %0.2f second scuttle!" % SCUTTLE_TIME),
                     MasterConcurrent(Timer(SCUTTLE_TIME), Sequential(Timed(VelocityX(1.0), 2.0), VelocityX(0.0)), RelativeToInitialDepth(3.0, error=0.03)),
                     Log("Scuttle complete. Returning to targeting position."),
                     MasterConcurrent(Timer(4.0), RelativeToInitialDepth(-1.0, error=0.02), VelocityX(-1.0)),
                     VelocityX(0.0))
                     #GoToPosition(lambda: self.target_position[0], lambda: self.target_position[1], depth=lambda: self.target_depth)
                          #)
