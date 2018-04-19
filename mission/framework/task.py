import time
import shm
import functools

from auvlog.client import Logger
from mission.framework.helpers import dict_join


class Task:
    """A Task performs an action.

    The main methods needed to interact with a task are .run() and .finished. .run() will perform action of the
    task and .finished is True if the task is done trying to achieve its goal and False
    otherwise.

    Some common functionality is computed for all Tasks. Below is a description:
    - Timing: These values are the time as a floating point number expressed in seconds since the epoch, in UTC.
      The values will be None if not yet set.
        - .first_run_time: The time at which .run() was first called.
        - .this_run_time: The time at which .run() was called for this current run.
        - .last_run_time: The time at which .run() was previously called. This will always be None in
          .on_first_run(), but will be .first_run_time during the first call of .on_run().
    """

    task_call_stack = ["mission"]
    minimum_printed_log_level = None

    """
    Methods to be overridden by most tasks.

    These methods will be run at specified times managed by the mission framework.
    """

    def __init__(self, *args, **kwargs):
        """Make-A-Task

        Args:
            *args:
            **kwargs:

        Returns:

        """
        """If the .run() method has been called."""
        self.has_run = False  # type: bool

        """If the .finish() method has been called."""
        self.has_ever_finished = False  # type: bool

        """If the .finish() method has been called during the last run."""
        self.finished = False  # type: bool

        """If the task finished in a successful state.

        A task is either successful (True), unsuccessful (False), or unfinished
        (False). Thus, this attribute is meaningless if the task isn't finished.
        """
        self.success = False

        """Time of first run, or None if not yet run"""
        self.first_run_time = None

        """Time this run started."""
        self.this_run_time = None

        """Time of last run, or None if not yet run twice."""
        self.last_run_time = None

        """Name of the task."""
        self.task_name = type(self).__name__.split(".")[-1]  # type: str

        """Stored default args and kwargs"""
        self._args = args
        self._kwargs = kwargs

        """Task to run 'as this task' (see .use_task())"""
        self._task = None

    def __call__(self, *args, **kwargs):
        self.run(*(self._args + args), **dict_join(self._kwargs, kwargs))

    def on_first_run(self, *args, **kwargs):
        """Let's get it started.

        This method is run immediately before the task is run for the first time. This method should execute
        exactly once. This method will be run before on_run is executed. This method should not block, an ongoing
        operation should be managed by the on_run method, or split into subtasks.

        This should be overridden in a subclass if needed.

        Args:
            *args: Some positional arguments handled by the overridden method.
            **kwargs: Some keyword arguments handled by the overridden method.
        Returns:
            Optionally, a Task to run "as this task".
        """
        pass

    def on_run(self, *args, **kwargs):
        """Try to achieve the goal of the task.

        This method is run most frequently and contains the main logic of the task. This method should not block,
        instead it assumes it will be frequently called until it has achieved the goal of the task.

        This should be overridden in a subclass. The method will print a warning if not overridden.

        Args:
            *args: Some positional arguments handled by the overridden method.
            **kwargs: Some keyword arguments handled by the overridden method.

        """
        if self._task is None:
            print(".on_run() was called but never overridden. Class name: {}".format(type(self).__name__))

    def on_finish(self, *args, **kwargs):
        """Do some final action.

        This method is run once the task transitions into the finished state. This method should not block - if some
        long operation is needed, the task isn't really finished, is it?

        This should be overridden in a subclass if needed.

        Args:
            *args: Some positional arguments handled by the overridden method.
            **kwargs: Some keyword arguments handled by the overridden method.

        """
        pass

    """
    Don't override these methods without extremely good reason. The core Task behavior should be predictable.

    These methods are more core to the Task, and will probably only be touched by new types of tasks.
    """

    def run(self, *args, **kwargs):
        """Achieve the goal of the task!

        This will manage the running of the task. For example, the first time this is called, on_first_run will be
        run before on_run. This method does not block.

        Args:
            *args: Some positional arguments handled by the event handler.
            **kwargs: Some keyword arguments handled by the event handler.

        """

        Task.task_call_stack.append(self.__class__.__name__.lower())

        if not self.has_run:
            Task.task_call_stack.append('first_run')
            self.first_run_time = self.this_run_time = time.time()
            self.on_first_run(*args, **kwargs)
            self.has_run = True
            Task.task_call_stack.pop()

            if self.finished:
                Task.task_call_stack.pop()
                return

        self.last_run_time = self.this_run_time
        self.this_run_time = time.time()
        self.finished = False
        self.success = False

        if self._task is not None:
            self._task()
            if self._task.finished:
                self.finish(success=self._task.success)
        else:
            self.on_run(*args, **kwargs)

        Task.task_call_stack.pop()

    def finish(self, *args, success=True, **kwargs):
        """We're done trying to achieve the goal!

        This signals the task that it has finished. This is usually called by the task itself (as self.finish()),
        but can be called from above as well. Calling from above usually isn't the best way to signal the task.

        Args:
            *args: Some positional arguments handled by the event handler.
            success: Whether the task succeeded or not. Not passed to the event
            handler.
            **kwargs: Some keyword arguments handled by the event handler.

        """
        self.has_ever_finished = True
        self.finished = True
        self.success = success
        self.on_finish(*args, **kwargs)

    def use_task(self, task):
        """Use a task 'as this task'

        Task subclasses can specify a Task using .use_task() before .on_run()
        which acts in place of the outer task. The outer task finishes as the
        provided task does, adopting its success status.
        """
        if self.has_run:
            self.loge('.use_task() may not be called after .on_first_run()')
        else:
            self._task = task

    @staticmethod
    def should_print(message_level):
      if Task.minimum_printed_log_level == 'verbose':
        return True
      elif Task.minimum_printed_log_level == 'info':
        return message_level != 'verbose'
      elif Task.minimum_printed_log_level == 'warn':
        return message_level != 'info' and message_level != 'verbose'
      elif Task.minimum_printed_log_level == 'error':
        return message_level == 'error'
      return False

    @staticmethod
    def log(*args, level, copy_to_stdout = True, **kwargs) -> None:
        """Log to auvlog

        :param msg: The message to be logged
        :param level: The level to be logged. Use the other helper log methods for standard levels.
        :param copy_to_stdout: If True, the message will be copied to standard out. This is useful for quick debugging.
        """
        log_path = shm.active_mission.log_path.get()
        log_file = "%s/mission.log" % log_path
        Logger(Task.task_call_stack + [level])(*args,
                copy_to_stdout=copy_to_stdout or Task.should_print(level),
                copy_to_file=log_file, **kwargs)

    logv = functools.partialmethod(log, level="verbose")
    logi = functools.partialmethod(log, level="info")
    logw = functools.partialmethod(log, level="warn")
    loge = functools.partialmethod(log, level="error")

    # Compatibility
    logd = logv
