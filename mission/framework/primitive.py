from mission.framework.task import Task
from mission.framework.movement import Heading, Pitch, Roll, \
                                       Depth, VelocityX, VelocityY, \
                                       PositionN, PositionE
from mission.framework.helpers import call_if_function
import shm

class NoOp(Task):
    """Do nothing - useful to use as a default argument instead of None. Similar
    to the Empty of an Option type."""

    def on_run(self, finite=True, *args, **kwargs):
        self.has_ever_finished = True
        if finite:
            self.finish()

class FunctionTask(Task):
    """
    Runs a function.

    The task finishes after one function invocation if finite, otherwise it
    calls the function each task run. If the function returns False, the task
    always finishes in a failed state.
    """

    def on_run(self, func, finite=True, *args, **kwargs):
        ret = func()
        success = ret is None or ret
        if finite or success:
            self.finish(success=success)

class ZeroWithoutHeading(Task):
    def on_run(self, pitch=True, roll=True):
        Depth(shm.kalman.depth.get())()
        PositionN(shm.kalman.north.get(), positional_controls=None)()
        PositionE(shm.kalman.east.get(), positional_controls=None)()
        Pitch(0)() if pitch else Pitch(shm.kalman.pitch.get())()
        Roll(0)() if roll else Roll(shm.kalman.roll.get())()
        VelocityX(0, positional_controls=None)()
        VelocityY(0, positional_controls=None)()
        self.finish()

class Zero(Task):
    """Zeroes desires - Sets velocities to zero and optionally maintains current
    orientation of the submarine"""
    def on_run(self, pitch=True, roll=True):
        Heading(shm.kalman.heading.get())()
        ZeroWithoutHeading(pitch=pitch, roll=roll)()
        self.finish()

class EnableController(Task):
    def on_run(self):
        self.logi("Enabling controller and all PID loops.")
        shm.settings_control.enabled.set(1)

        shm.settings_control.heading_active.set(1)
        shm.settings_control.pitch_active.set(1)
        shm.settings_control.roll_active.set(1)

        shm.settings_control.velx_active.set(1)
        shm.settings_control.vely_active.set(1)
        shm.settings_control.depth_active.set(1)

        self.finish()

class HardkillGuarded(Task):
    def on_run(self, task):
        if shm.switches.hard_kill.get():
            self.logi("HARDKILL DETECTED! Ending mission...")
            self.finish()
            return

        task()
        if task.finished:
            self.finish()

class Log(Task):
    #TODO: call_if_function (currently doesn't work on strings because call if function is weird)
    def on_run(self, message, level="info"):
        self.log(message, level=level)
        self.finish()
        
class AlwaysLog(Task):
    """Log until the end of time"""
    def on_run(self, message, level="info"):
        self.log(call_if_function(message), level=level)

class Succeed(Task):
    """Succeed a task on finish, no matter what"""
    def on_run(self, task=None, override=True, *args, **kwargs):
        if task is not None:
            task()
        if task is None or task.finished:
            self.finish(success=override)

class Fail(Task):
    """Fail a task on finish, no matter what

    Using a separate Fail class instead of parameterizing a success overriding
    task with a boolean may make logging easier.
    """
    def on_run(self, task=None, *args, **kwargs):
        if task is not None:
            task()
        if task is None or task.finished:
            self.finish(success=False)

class InvertSuccess(Task):
    """Finish with the opposite success state of a given task.

    InvertSuccess only finishes as the given task finishes.
    """

    def on_run(self, task, *args, **kwargs):
        task()
        if task.finished:
            self.finish(success=not task.success)
