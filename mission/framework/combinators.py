import itertools

from mission.framework.helpers import should_finish
from mission.framework.task import Task


class Sequential(Task):
    """Do tasks in order.

    This task checks the .finished flag of each subtask in order.

    If the finite parameter (default True) is True, each task is considered finished if .has_ever_finished is True.
    Tasks are not run if .has_ever_finished is True (and finite is True).

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
            if finite and task.has_ever_finished:
                continue
            task()
            if not task.finished:
                break
        else:
            self.finish()


class Concurrent(Task):
    """Do tasks at once.

    This task checks the .finished flag of each subtask after running all.

    If the finite parameter (default True) is True, each task is considered finished if .has_ever_finished is True.
    Tasks are not run if .has_ever_finished is True (and finite is True).

    """

    # TODO: Does this actually need to handle generators?
    def on_run(self, *tasks, subtasks=(), finite=True, **kwargs):
        """
        :param tasks: Tasks to be run concurrently.
        :param subtasks: More tasks to be run concurrently.
        :param finite: If True, each task is considered finished if .has_ever_finished is True. Tasks are not run if
        .has_ever_finished is True (and finite is True).
        """
        subtasks_iterable, subtasks_iterable_copy = itertools.tee(itertools.chain(tasks, subtasks))

        for task in subtasks_iterable:
            if finite and task.has_ever_finished:
                continue
            task()

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

    """

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.master_task = None
        self.concurrent = None

    def on_first_run(self, *args, **kwargs):
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

        if should_finish(master_task, finite):
            self.finish()
