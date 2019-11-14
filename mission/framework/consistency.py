
import threading

import shm

from mission.framework.task import Task

class ConsistencyCheck:
    ''' Call 'check' on a value to tell if it is consistently True.
    This does dual-threshold hysteresis, so it requires 'count' of
    the last 'total' calls to be true to switch to returning true,
    and then it would wait for 'count' of the last 'total' values
    to be false before switching back to returning false.

    If 'strict' is used, then we require that at least 'count' of the last
    'total' values are True in order to give True - i.e. we do away
    with the dual thresholding.'''
    def __init__(self, count=3, total=5, default=False, strict=False):
        self.results = [1 if default else -1]*int(total)
        self.total = int(total)
        self.count = int(count)
        self.state = default
        self.default = default
        self.strict = strict


    def add(self, result):
        self.results.append(1 if result else -1)
        if len(self.results) > self.total:
            self.results[:] = self.results[-self.total:]

    def check(self, result=None):
        if result != None:
            self.add(result)

        if self.strict:
            return sum(x == 1 for x in self.results) >= self.count

        if sum( self.results ) >= (2*self.count-self.total):
            self.state = True
        elif sum( self.results ) <= -(2*self.count-self.total):
            self.state = False

        return self.state

    def __call__(self, result=None):
        return self.check(result)

    def clear(self, default=None):
        if default is None:
            self.results = [1 if self.default else -1]*self.total
            self.state = self.default
        else:
            self.results = [1 if default else -1]*self.total
            self.state = default

class ConsistentTask(Task):
    """
    Finishes when a non-finite task is consistently finished
    """
    def on_first_run(self, task, success=18, total=20, *args, **kwargs):
        self.cons_check = ConsistencyCheck(success, total)

    def on_run(self, task, debug=False, *args, **kwargs):
        task()
        if self.cons_check.check(task.finished):
            self.finish()
        if debug:
            success_char = ["x", "^"][self.cons_check.results[-1] == 1]
            dropped_char = ["x", "^"][self.cons_check.results[0] == 1]

            self.logd("{}/{}/{}/{}/{}".format(
                success_char,
                sum(x == 1 for x in self.cons_check.results),
                self.cons_check.count,
                self.cons_check.total,
                dropped_char,
            ))

class ConsistentShm(Task):
    """
    Finishes when a test predicate on a SHM group/variable consistently passes.

    The test function is only run when the SHM group/variable updates. This is implemented
    with a SHM watcher running on a separate thread.

    Note that this task is best-used with a SHM group that is constantly updating, such
    as the output from a vision module.

    Sample usage:

    ```
    consistent = ConsistentShm(shm.red_buoy_results,
            lambda group: group.heuristic_score > 0.8,
            count=10, total=15)
    ```

    Arguments:
     - group_or_var: SHM group or variable to watch (something that you can call get() on)
     - test: function that will be passed the result of group_or_var.get() and returns a boolean
     - count: number of updates in the sliding window that must pass
     - total: total number of updates in the sliding window
     - invert: invert the test output value to check for (default False)
     - result: finishing mode, True for success; False for failure (default True)
    """
    def on_first_run(self, group_or_var, test, count=3, total=5, invert=False, result=True):
        self.checker = ConsistencyCheck(count, total, default=False)
        self.watcher = shm.watchers.watcher()
        self.watcher.watch(group_or_var)

        def thread():
            while not self.has_ever_finished:
                self.watcher.wait()
                if not self.has_ever_finished:
                    test_result = test(group_or_var.get())
                    if self.checker.check(not test_result if invert else test_result):
                        self.finish(success=result)

        threading.Thread(target=thread, daemon=True).start()

    def on_run(self, *args, **kwargs):
        pass
