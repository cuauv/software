from mission.framework.combinators import Sequential, MasterConcurrent
from mission.framework.helpers import call_if_function
from mission.framework.task import Task
from mission.framework.primitive import Fail, Log


class Timer(Task):
    """ A task that finishes after a set amount of time.

        Args:
            seconds: The amount of seconds to be waited before finishing.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.seconds = 0

    def on_run(self, seconds, *args, **kwargs):
        """
        Args:
            seconds: The amount of seconds to be waited before finishing.
        """
        self.seconds = call_if_function(seconds)
        if (self.this_run_time - self.first_run_time) >= self.seconds:
            self.finish()

class GuardedTimer(Task):
    """ A task that finishes after a set amount of time.

        Args:
            seconds: The amount of seconds to be waited before finishing.
    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.seconds = 0
        self.state = 'run'

    def on_run(self, seconds, task, restore, *args, **kwargs):
        """
        Args:
            seconds: The amount of seconds to be waited before finishing.
        """
        if self.state == 'run':
            self.seconds = call_if_function(seconds)
            task()
            if task.finished:
                self.finish()

            if (self.this_run_time - self.first_run_time) >= self.seconds:
                self.state = 'restore'
                return

        if self.state == 'restore':
            restore()
            if restore.finished: self.finish()

class Timeout(Task):
    """
    Try doing a task for a certain amount of time

    We are successful if the task completes in time and is successful.
    """

    def on_first_run(self, task, time, *args, **kwargs):
        # Client can check if task timed out with
        # timout.timer.has_ever_finished
        self.timer = Timer(time)
        self.use_task(MasterConcurrent(
            task,
            Fail(Sequential(
                self.timer,
                Log('{} timed out!'.format(task.__class__.__name__)),
            )),
        ))

"""Run a task for a certain amount of time, irrespective of the task's
completion.
"""
def Timed(task, seconds): return MasterConcurrent(Timer(seconds), task)
