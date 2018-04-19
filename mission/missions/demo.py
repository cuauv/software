import shm
from mission.framework.task import Task
from mission.framework.movement import Heading, Pitch, Roll
from mission.framework.timing import Timer
from mission.framework.combinators import Sequential, MasterConcurrent, Concurrent

ROTATION_SEGMENTS = 50
ROTATION_TIME = 2

def gen_rotate_task(axis_task):
    subtasks = []
    for i in range(ROTATION_SEGMENTS):
        axis = axis_task(360 / ROTATION_SEGMENTS * (i + 1), error=3)
        subtasks.append(Segment(axis))

    return Sequential(subtasks=subtasks)

class Segment(MasterConcurrent):
    def __init__(self, axis_task, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.axis_task = axis_task
        self.timer_task = Timer(ROTATION_TIME / ROTATION_SEGMENTS)

    def on_run(self):
        super().on_run(self.timer_task, self.axis_task)

class Logger(Task):
    def __init__(self, message, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.message = message

    def on_first_run(self):
        self.logv(self.message, copy_to_stdout=True)
        self.finish()

    def on_run(self):
        pass

roll = lambda: gen_rotate_task(Roll)
