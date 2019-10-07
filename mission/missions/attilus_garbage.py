from mission.framework.task import Task
from mission.framework.helpers import ConsistencyCheck, call_if_function
from mission.framework.targeting import PIDLoop
from mission.framework.movement import VelocityX, VelocityY, RelativeToCurrentHeading, RelativeToInitialHeading, PositionN, PositionE
from mission.framework.position import MoveX, WithPositionalControl
from mission.framework.combinators import While, Sequential, MasterConcurrent, Concurrent
from mission.framework.primitive import FunctionTask, Succeed, Zero, Log, Fail
from mission.framework.timing import Timed, Timer

import shm
"""
A bunch of garbage that I (Attilus) want to use across different missions.
"""

# A task that runs a PID loop for VelocityY
class PIDSway(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid_loop = PIDLoop(output_function=VelocityY())

    def on_run(self, error, p=0.0005, i=0, d=0.0, db=0.01875, max_out=0.5, negate=False, *args, **kwargs):
        self.pid_loop(input_value=error, p=p, i=i, d=d, target=0, modulo_error=False, deadband=db, negate=negate, max_out=max_out)

    def stop(self):
        VelocityY(0)()

# A task that runs a PID loop for VelocityX
class PIDStride(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid_loop = PIDLoop(output_function=VelocityX())

    def on_run(self, error, p=0.00003,  i=0, d=0.0, db=0.01875, negate=False, max_out=0.5, *args, **kwargs):
        self.pid_loop(input_value=error, p=p, i=i, d=d, target=0, modulo_error=False, deadband=db, negate=negate, max_out=max_out)

    def stop(self):
        VelocityY(0)()

# A task that runs a PID loop for Heading
class PIDHeading(Task):
    def on_first_run(self, *args, **kwargs):
        self.pid_loop = PIDLoop(output_function=RelativeToCurrentHeading())

    def on_run(self, error, p=0.35,  i=0, d=0.0, db=0.01875, negate=False, max_out=20, *args, **kwargs):  # TODO: max_out
        self.pid_loop(input_value=error, p=p, i=i, d=d, target=0, modulo_error=360, deadband=db, negate=negate, max_out=max_out)

    def stop(self):
        RelativeToCurrentHeading(0)()

class StillHeadingSearch(Task):
    """
    Search for an object visible from the current location that is in front of
    the sub with highest probability. Edited from ozer_common
    """

    def on_first_run(self, speed=40, db=10, *args, **kwargs):
        init_heading = None

        def set_init_heading():
            nonlocal init_heading
            init_heading = shm.kalman.heading.get()
            return True

        set_init_heading()

        self.use_task(
            # TODO: DO WE NEED THE WHILE OR DO WE WANT IT TO TERMINATE
            # While(lambda: \
                Sequential(
                    RelativeToInitialHeading(speed),
                    MasterConcurrent(
                        FunctionTask(lambda: abs(shm.desires.heading.get() - init_heading) < db, finite=False),
                        RelativeToCurrentHeading(speed)),
                    # Move back a bit, we might be too close
                    # TODO: DO WE EVEN NEED THIS?
                    # MoveX(-1),
                    # Succeed(FunctionTask(set_init_heading))
                )
            # , True),
        )


class SlowHeading(Task):

    def on_first_run(self, speed=40, db=7, target=180, *args, **kwargs):
        init_heading = None

        def set_init_heading():
            nonlocal init_heading
            init_heading = shm.kalman.heading.get()
            return True

        set_init_heading()

        self.use_task(
            # TODO: DO WE NEED THE WHILE OR DO WE WANT IT TO TERMINATE
            # While(lambda: \
                Sequential(
                    RelativeToInitialHeading(speed),
                    MasterConcurrent(
                        FunctionTask(lambda: (abs(shm.desires.heading.get() - (init_heading + target)) % 360) < db, finite=False),
                        RelativeToCurrentHeading(speed)),
                    # Move back a bit, we might be too close
                    # TODO: DO WE EVEN NEED THIS?
                    # MoveX(-1),
                    # Succeed(FunctionTask(set_init_heading))
                )
            # , True),
        )


# Sway search but without moving forward
def SwayOnlySearch(speed=0.3, width=2.5, right_first=True):
    direction = 1 if right_first else -1
    return Sequential(
            Log('what'),
            Timed(VelocityY(direction*speed), width/(2*speed)),
            Timed(VelocityY(-direction*speed), width/(speed)),
            Timed(VelocityY(direction*speed), width/(2*speed)),
            Zero())


class MoveNE(Task):
    def on_first_run(self, vector, deadband=0.1):
        vector = call_if_function(vector)
        self.use_task(
                WithPositionalControl(
                Concurrent(PositionN(target=vector[0], error=deadband),
                           PositionE(target=vector[1], error=deadband),
                           finite=False))
        )


class PositionMarkers:
    def __init__(self):
        self._markers = {}

    def set(self, marker, value=None):
        if value is None:
            value = (shm.kalman.north.get(), shm.kalman.east.get())
        self._markers[marker] = value
        return value

    def unset(self, marker):
        try:
            del self._markers[marker]
            return True
        except KeyError:
            return False

    def get(self, marker):
        try:
            return self._markers[marker]
        except KeyError:
            return None

    def go_to(self, marker, deadband=0.1):
        target = self.get(marker)
        if target is not None:
            return Sequential(Log('Moving to marker {} at {}'.format(marker, target)), MoveNE(target, deadband))
        return Sequential(Log('Going to nonexistent marker {}'.format(marker)), Fail())

def singleton(cls):
    instances = {}
    def wrapper(*args, **kwargs):
        if cls not in instances:
          instances[cls] = cls(*args, **kwargs)
        return instances[cls]
    return wrapper

PositionMarkers = singleton(PositionMarkers)

class SetMarker(Task):
    def on_run(self, marker, value=None):
        marker = call_if_function(marker)
        value = call_if_function(value)
        self.log("Setting marker {} at {}".format(marker, PositionMarkers().set(marker, value)), level='info')
        self.finish()

class GoToMarker(Task):
    def on_first_run(self, marker, deadband=0.2):
        marker = call_if_function(marker)
        target = PositionMarkers().get(marker)
        if target is not None:
            self.log('Moving to marker {} at {}'.format(marker, target), level='info')
            self.use_task(MoveNE(target, deadband))
            return
        self.log('Attempted to move to nonexistent marker {}'.format(marker), level='error')
        self.finish(success=False)

class UnsetMarker(Task):
    def on_run(self, marker):
        marker = call_if_function(marker)
        if PositionMarkers().unset(marker):
            self.log('Marker {} unset'.format(marker), level='info')
            self.finish(success=True)
            return
        self.log('Attempted to unset nonexistent marker {}'.format(marker), level='error')
        self.finish(success=False)


class DualConsistency(Task):
    """edited from will_common"""
    def on_first_run(self, test_success, test_fail, count, total, invert):
        # Multiply by 60 to specify values in seconds, not ticks
        self.checker_success = ConsistencyCheck(count*60, total*60, default=False)
        self.checker_fail = ConsistencyCheck(count*60, total*60)

    def on_run(self, test_success, test_fail, count, total, invert):
        test_result_success = call_if_function(test_success)
        test_result_fail = call_if_function(test_fail)
        if self.checker_success.check(not test_result_success if invert else test_result_success):
            self.finish(success=True)
        elif self.checker_fail.check(not test_result_fail if invert else test_result_fail):
            self.finish(success=False)

class CheckedTimer(Task):
    """ A timer that only finishes when the timer is finished and a certain value is passed to it"""

    def on_first_run(self, seconds, function, value, *args, **kwargs):
        self.timer = Timer(seconds)

    def on_run(self, seconds, function, value, *args, **kwargs):
        self.timer()
        if call_if_function(function) == value and self.timer.finished:
            self.finish()
