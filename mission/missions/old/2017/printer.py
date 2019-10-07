from mission.framework.task import Task
from mission.framework.timing import Timer


# TODO: Documentation!
class Printer(Task):
    def __init__(self):
        super().__init__()
        self.timer_task = None  # type: Task

    def on_first_run(self, *args, **kwargs):
        self.timer_task = Timer(5)
        self.timer_task()  # FIXME: This is because without a run, no parameter is passed?
        print("First run called with args {} and kwargs {}.".format(args, kwargs))

    def on_run(self, *args, **kwargs):
        print("Running for {} seconds. Timer.is_finished(): {}".format(self.this_run_time - self.first_run_time,
                                                                       self.timer_task.finished))

        if self.timer_task.finished:
            self.finish()

    def on_finish(self, *args, **kwargs):
        print("Finish called with args {} and kwargs {}.".format(args, kwargs))