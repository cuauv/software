from mission.framework.task import Task
from mission.framework.movement import Heading, Pitch, Roll, \
                                       Depth, VelocityX, VelocityY, \
                                       PositionN, PositionE
import shm

class NoOp(Task):
    """Do nothing - useful to use as a default argument instead of None. Similar to the Empty of an Option type."""

    def on_run(self, finite=True, *args, **kwargs):
        self.has_ever_finished = True
        if finite:
            self.finish()

class FunctionTask(Task):
    """Runs a function and finishes"""

    def on_run(self, func, finite=True, *args, **kwargs):
        func()
        if finite:
            self.finish()

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
    """Zeroes desires - Sets velocities to zero and optionally maintains current orientation of the submarine"""
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
    def on_run(self, message, level="info"):
        self.log(message, level=level)
        self.finish()
