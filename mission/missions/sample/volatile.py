from mission.framework.task import Task
from mission.framework.primitive import (
    Succeed,
    Fail,
    Log,
    FunctionTask,
    InvertSuccess,
    NoOp,
)
from mission.framework.combinators import (
    Sequential,
    Concurrent,
    MasterConcurrent,
    Retry,
    Conditional,
)
from mission.framework.timing import Timer, Timeout

def LogSuccess(task):
    name = task.__class__.__name__
    return Conditional(
        task,
        Log('{} succeeded!'.format(name)),
        Fail(Log('{} failed!'.format(name))),
    )

def TimedLog(msg, t=0.02): return Sequential(Timer(t), Log(msg))

def TestSequential(success):
    return LogSuccess(Sequential(
        TimedLog('1'),
        TimedLog('2'),
        TimedLog('3'),
        Succeed(TimedLog('4'), success),
        TimedLog('5'),
    ))

def GoodSequential(): return TestSequential(True)
def BadSequential(): return TestSequential(False)

def TestRetry(): return LogSuccess(Retry(lambda: TestSequential(False), 3))

def TestConcurrent(success):
    return LogSuccess(Concurrent(
        Succeed(TimedLog('1', 0.03), success),
        TimedLog('2', 0.04),
        TimedLog('3', 0.01),
        TimedLog('4', 0.02),
    ))

def GoodConcurrent():
    # Should succeed
    return TestConcurrent(True)

def BadConcurrent():
    # Should fail after '1' finishes
    return TestConcurrent(False)

def TestMasterConcurrent1():
    # Should fail on master task
    return LogSuccess(MasterConcurrent(
        Fail(TimedLog('master task', 0.02)),
        TimedLog('slave task', 0.05),
    ))

def TestMasterConcurrent2():
    # Should fail on slave task
    return LogSuccess(MasterConcurrent(
        TimedLog('master task', 0.05),
        Fail(TimedLog('slave task', 0.02)),
    ))

def TestMasterConcurrent3():
    # Should succeed
    return LogSuccess(MasterConcurrent(
        TimedLog('master task', 0.02),
        Fail(TimedLog('slave task', 0.05)),
    ))

def TestMasterConcurrent4():
    # Should fail
    return LogSuccess(MasterConcurrent(
        Log('master task'),
        Fail(Log('slave task')),
    ))

def TestMasterConcurrent5():
    # Should fail
    return LogSuccess(MasterConcurrent(
        Fail(Log('master task')),
        Log('slave task'),
    ))

def TestTimeout():
    return LogSuccess(Timeout(TimedLog('takes too long to log this', 0.1), 0.05))

# Should only print single log message and succeed
class TestSubtask1(Task):
    def on_first_run(self):
        self.use_task(TimedLog('hi'))

# Should only print single log message and fail
class TestSubtask2(Task):
    def on_first_run(self):
        self.use_task(Fail(TimedLog('bye')))

# Should print an error for using a subtask outside on_first_run(), and also
# succeed
class TestSubtask3(Task):
    def on_run(self):
        self.use_task(Fail(NoOp()))

def TestAll():
    class Inc:
        def __init__(self):
            self.failed_tests = 0

        def inc(self):
            Log('BAD!')()
            self.failed_tests += 1

        def log(self):
            Log('Failed tests: {}'.format(self.failed_tests))()

    inc = Inc()

    def IncFailed(t): return Conditional(t, on_fail=FunctionTask(inc.inc))

    tasks = [
        GoodSequential(),
        InvertSuccess(BadSequential()),
        InvertSuccess(TestRetry()),
        GoodConcurrent(),
        InvertSuccess(BadConcurrent()),
        InvertSuccess(TestMasterConcurrent1()),
        InvertSuccess(TestMasterConcurrent2()),
        TestMasterConcurrent3(),
        InvertSuccess(TestMasterConcurrent4()),
        InvertSuccess(TestMasterConcurrent5()),
        InvertSuccess(TestTimeout()),
        TestSubtask1(),
        InvertSuccess(TestSubtask2()),
    ]

    return Sequential(
        subtasks=[IncFailed(t) for t in tasks] + [FunctionTask(inc.log)]
    )
