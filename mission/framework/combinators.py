import itertools

from mission.framework.helpers import should_run, should_finish, call_if_function
from mission.framework.task import Task
from mission.framework.primitive import NoOp, Succeed


class Sequential(Task):
    """Do tasks in order.

    This task checks the .finished flag of each subtask in order.

    If the finite parameter (default True) is True, each task is considered finished if .has_ever_finished is True.
    Tasks are not run if .has_ever_finished is True (and finite is True).

    If any task fails, Sequential fails and does not proceed past the failing
    task.

    """

    def on_run(self, *tasks, subtasks=(), finite=True, **kwargs):
        """
        :param tasks: Tasks to be run sequentially.
        :param subtasks: More tasks to be run sequentially. Note: these Tasks are run after the `tasks` argument.
        :param finite: If True, each task is considered finished if .has_ever_finished is True. Tasks are not run if
        .has_ever_finished is True (and finite is True).
        """
        subtasks_iterable = itertools.chain(tasks, subtasks)

        for task in subtasks_iterable:
            if not should_run(task, finite):
                continue
            task()
            if finite and task.has_ever_finished and not task.success:
                self.finish(success=False)
                break
            if not task.finished:
                break
        else:
            self.finish()

class Concurrent(Task):
    """Do tasks at once.

    This task checks the .finished flag of each subtask after running all.

    If the finite parameter (default True) is True, each task is considered finished if .has_ever_finished is True.
    Tasks are not run if .has_ever_finished is True (and finite is True).

    All tasks are run on a tick. If any task finishes in a failed state, Concurrent does as well.

    """

    def on_run(self, *tasks, subtasks=(), finite=True, **kwargs):
        """
        :param tasks: Tasks to be run concurrently.
        :param subtasks: More tasks to be run concurrently.
        :param finite: If True, each task is considered finished if .has_ever_finished is True. Tasks are not run if
        .has_ever_finished is True (and finite is True).
        """

        subtasks_iterable, subtasks_iterable_copy = itertools.tee(itertools.chain(tasks, subtasks))
        all_success = True

        for task in subtasks_iterable:
            if not should_run(task, finite):
                continue
            task()
            all_success = all_success and (not task.finished or task.success)

        if not all_success:
            self.finish(success=False)
            return

        for task in subtasks_iterable_copy:
            if not should_finish(task, finite):
                break
        else:
            self.finish()

class MasterConcurrent(Task):
    """Do some tasks until one finishes.

    This task acts like Concurrent but only finish state always matches that of the master task.

    An example use of this task is a simple timeout. If the master_task is a Timer, then the other tasks will run
    concurrently until the timer finishes. Another example is a task that decides is a mission element was completed
    (or a timeout - this can be done by making the master_task a Concurrent). This master_task could try to detect if
    the buoy was rammed, while the slave task could be a sequential of center and drive forward.

    If the finite parameter (default True) is True, each task is considered finished if .has_ever_finished is True.
    Tasks are not run if .has_ever_finished is True (and finite is True).

    If either task finishes in a failed state, MasterConcurrent also finishes in
    a failed state, even if the master task is not finished.

    """

    def on_first_run(self, *args, **kwargs):
        self.master_task = None
        self.concurrent = Concurrent()

    def on_run(self, master_task, *tasks, subtasks=(),
               finite: bool = True, **kwargs):
        """
        :param master_task: Task to be run concurrently and the only Task to be checked for finishing.
        :param tasks: Tasks to be run concurrently.
        :param subtasks: More tasks to be run concurrently.
        :param finite: If True, each task is considered finished if .has_ever_finished is True. Tasks are not run if
        .has_ever_finished is True (and finite is True).
        """
        self.master_task = master_task
        self.concurrent(master_task, *(tasks + subtasks), finite=finite)

        for t in [self.concurrent, self.master_task]:
            if should_finish(t, finite):
                self.finish(success=t.success)
                break

class Retry(Task):
    """
    Keep attempting a finite task until it succeeds, or until it has been
    attempted a given number of times.

    task_func is an object which when called returns an instance of a Task. It
    could be a Task subclass or lambda which returns a Task instance, for
    example.

    Why pass in a 'Task function' instead of a Task? It's not in the scope of a
    Task to know how to restart itself from scratch when it enters a finished
    state. If this was so, the mission framework would need to know to call
    'on_first_run()' again, for instance.
    """

    def on_first_run(self, task_func, attempts, *args, **kwargs):
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
                    self.finish()
                else:
                    # If at first we don't succeed, umm, try, try again
                    self.attempt += 1
                    self.log_attempt = True
                    self.task = task_func()
        else:
            self.loge('Failed {} after {} attempts'.format(self.task_name, attempts))
            self.finish(success=False)

class Conditional(Task):
    """
    Do one task if a given task succeeds, else do another.

    If main_task succeeds, on_success is ran, else on_fail is fun. The success
    of Conditional is the success of whichever of on_success or on_fail is run.
    If a branch task is not provided and that branch is entered, Conditional
    succeeds.

    Args:
        main_task: The task to initially run and gauge the success of when it finishes.
        on_success: The task to run if main_task finishes successfully
        on_fail: The task to run if main_task finishes in a failed state
        finite: Whether to consider tasks finished solely by .has_ever_finished
    """
    def on_first_run(self, main_task, on_success=None, on_fail=None, *args, **kwargs):
        self.main_task = main_task
        self.on_success = on_success if on_success is not None else NoOp()
        self.on_fail = on_fail if on_fail is not None else NoOp()

    def on_run(self, *args, finite=True, **kwargs):
        if should_run(self.main_task, finite):
            self.main_task()

        if should_finish(self.main_task, finite):
            if self.main_task.success:
                self.run_branch(self.on_success, finite)

            else:
                self.run_branch(self.on_fail, finite)

    def run_branch(self, branch, finite):
        if should_run(branch, finite):
            branch()
        if should_finish(branch, finite):
            self.finish(success=branch.success)

class While(Task):
    """
    Repeatedly execute a task while a condition remains true.

    The condition is only checked between Task completions, like you'd expect
    from statements in a normal While block.

    While fails immediately if the task fails.

    Args:
        task_func: A object that, when called, returns a new task to see to
        completion.
        cond: A bool condition (may be callable)
    """

    def on_first_run(self, task_func, condition, *args, **kwargs):
        self.begin_iteration(task_func, condition)

    def on_run(self, task_func, condition, *args, **kwargs):
        if not hasattr(self, 'task'):
            self.finish(success=True)
            return
        self.task()
        if self.task.finished:
            self.begin_iteration(task_func, condition)

    def begin_iteration(self, task_func, condition):
        success = not hasattr(self, 'task') or self.task.success
        if success and call_if_function(condition):
            self.task = task_func()
        else:
            self.finish(success=success)

class Defer(Task):
    """
    Execute one task after another, even if the first fails. Fail if the
    deferred task fails, otherwise finish with the success of the original task.

    It's similar to Golang's defer statement!
    """

    def on_first_run(self, main_task, deferred, *args, **kwargs):
        self.task = Sequential(Succeed(main_task), deferred)

    def on_run(self, main_task, deferred, *args, **kwargs):
        self.task()
        if self.task.finished:
            self.finish(success=deferred.success and main_task.success)

class Either(Task):
    '''
    Execute two tasks concurrently and finish if either subtask finishes.
    Uses the success state of the task that finishes first. If both tasks
    finish in the same tick, then the success state of the first is used.
    '''

    def on_run(self, *tasks, **kwargs):
        for task in tasks:
            task()

        for task in tasks:
            if task.finished:
                self.finish(success=task.success)
                break
