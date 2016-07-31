import itertools

import aslam
from mission.framework.task import Task
from mission.framework.timing import Timer
from mission.framework.primitive import NoOp
from mission.framework.combinators import Concurrent

"""
Common mission-writing primitives I (Ozer) enjoy using but am not sure should
belong in the mission system just yet. Some aren't completely generalized to
accomodate all types of tasks and maybe should be integrated into the mission
system differently.
"""

class Timeout(Task):
    """
    Try doing a task for a certain amount of time

    We are successful if the task completes in time and is successful.
    """
    def on_first_run(self, task, time, *args, **kwargs):
        self.success = False
        self.task = task
        self.timer = Timer(time)
        self.timed_out = False

    def on_run(self, *args, **kwargs):
        self.task()
        self.timer()
        if self.task.finished:
            if hasattr(self.task, 'success'):
                self.success = self.task.success
            else:
                self.success = True
            self.finish()
        elif self.timer.finished:
            self.timed_out = True
            self.finish()

class SuccessOverride(Task):
    def on_run(self, task, override=True, *args, **kwargs):
        task()
        if task.finished:
            self.success = override
            self.finish()

Success = lambda task: SuccessOverride(task, override=True)
Failure = lambda task: SuccessOverride(task, override=False)

class Retry(Task):
    """
    Keep attempting a task until it succeeds, or until it has been attempted a given
    number of times.
    """
    def on_first_run(self, task_func, attempts, *args, **kwargs):
        self.success = False
        self.task = task_func()
        self.task_name = self.task.__class__.__name__
        self.attempt = 0
        self.log_attempt = True

    def on_run(self, task_func, attempts, *args, **kwargs):
        if self.attempt < attempts:
            if self.log_attempt:
                self.logi('Attempt {} of {} at {}'.format(self.attempt + 1, attempts, self.task_name))
                self.log_attempt = False

            self.task()
            if self.task.finished:
                if self.task.success:
                    self.logi('{} succeeded on attempt {}!'.format(self.task_name, self.attempt + 1))
                    self.success = True
                    self.finish()
                else:
                    # If at first we don't succeed, umm, try, try again
                    self.attempt += 1
                    self.log_attempt = True
                    self.task = task_func()
        else:
            self.loge('Failed {} after {} attempts'.format(self.task_name, attempts))
            self.finish()

class SequentialSuccess(Task):
    def on_first_run(self, *args, **kwargs):
        self.success = False

    def on_run(self, *tasks, subtasks=(), finite=True, **kwargs):
        subtasks_iterable = itertools.chain(tasks, subtasks)

        for task in subtasks_iterable:
            if finite and task.has_ever_finished:
                continue
            task()
            if finite and task.has_ever_finished:
                if hasattr(task, 'success') and not task.success:
                    self.finish()
                    break
            if not task.finished:
                break
        else:
            self.success = True
            self.finish()

class ConcurrentSuccess(Concurrent):
    """
    Run tasks concurrently until a certain number finish

    The success state reflects that of the first finished task
    """
    def on_run(self, *tasks, subtasks=(), finite=True, n=None, **kwargs):
        super().on_run(*tasks, subtasks=subtasks, finite=finite, **kwargs)

        self.success = False
        it1, it2, it3 = itertools.tee(itertools.chain(tasks, subtasks), 3)
        if n is None:
            n = len(list(it1))
        num_finished = sum(t.finished for t in it2)
        if num_finished >= n:
            self.finish()
            for t in it3:
                if hasattr(t, 'success'):
                    self.success = t.success
                    break
            else:
                self.success = True

success_of = lambda task: not hasattr(task, 'success') or task.success

class Conditional(Task):
    """
    Do one task if a given task succeeds, else do another

    Currently assumes all tasks are finite.
    """
    def on_first_run(self, main_task, on_success=None, on_failure=None, *args, **kwargs):
        self.main_task = main_task
        self.on_success = on_success if on_success is not None else NoOp()
        self.on_failure = on_failure if on_failure is not None else NoOp()

    def on_run(self, *args, **kwargs):
        if not self.main_task.has_ever_finished:
            self.main_task()
        elif success_of(self.main_task):
            if not self.on_success.has_ever_finished:
                self.on_success()
            else:
                self.success = success_of(self.on_success)
                self.finish()
        else:
            if not self.on_failure.has_ever_finished:
                self.on_failure()
            else:
                self.success = success_of(self.on_failure)
                self.finish()

class CheckDistance(Task):
    """
    Finished with a failed state once we've traveled too far
    """
    def on_first_run(self, *args, **kwargs):
        self.success = False
        self.initial_pos = aslam.sub.position()[:2]

    def on_run(self, distance, *args, **kwargs):
        current_pos = aslam.sub.position()[:2]
        if sum((self.initial_pos - current_pos) ** 2) > distance ** 2:
            self.finish()

