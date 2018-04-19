from collections import namedtuple
import copy
import shm
from shm.watchers import watcher
from mission.framework.task import Task
from mission.framework.targeting import PIDLoop, ForwardTarget
from mission.framework.combinators import (
    Sequential,
    Concurrent,
    MasterConcurrent,
    Retry,
    Conditional,
    Defer,
)
from mission.framework.timing import Timeout
from mission.framework.movement import (
    RelativeToInitialDepth,
    RelativeToCurrentDepth,
    VelocityX,
    VelocityY,
    Depth,
)
from mission.framework.primitive import (
    Zero,
    Log,
    Succeed,
    Fail,
    FunctionTask,
    NoOp,
)
from mission.framework.actuators import FireActuator
from mission.framework.position import (
    GoToPositionRough,
    MoveX,
    MoveY,
    MoveXRough,
    WithPositionalControl,
    NavigationSpeed,
)
from mission.framework.track import ConsistentObject
from mission.framework.helpers import call_if_function
from mission.missions.ozer_common import (
    ConsistentTask,
    Except,
    GlobalTimeoutError,
    StillHeadingSearch,
    GradualHeading,
    PrintDone,
    Infinite,
)
from mission.constants.config import torpedoes as constants

"""
Torpedoes 2017!
"""

class Vision(Task):
    # Board/cutout IDs are indices
    TENT_ID = 0
    SQUID_ID = 1

    def __init__(self, *args, **kwargs):
        super().__init__()
        # Vision object needs to be ready for rest of mission to access, so must
        # initialize before on_first_run()

        self.trackers = [ConsistentObject() for i in range(2)]

        self.watcher = watcher()
        self.watcher.watch(shm.torpedoes_vision)
        self.pull()

    def pull(self):
        shm_boards = [shm.torpedoes_tentacle.get(), shm.torpedoes_squid.get()]

        def noneify(g): return g if g.visible else None
        self.boards = [t.map(noneify(b)) for t, b in zip(self.trackers, shm_boards)]

    def on_run(self, *args, **kwargs):
        if self.watcher.has_changed():
            self.pull()

# Forecam offsets may be incorrect to compensate for inaccurate DVL-less control
class Torp:
    FIRE_TIME = 0.5

    def __init__(self, forecam_offset, actuator):
        self.forecam_offset = forecam_offset
        self.actuator = actuator

    def AlignFromForecam(self):
        return ConsistentTask(Concurrent(
            RelativeToInitialDepth(-self.forecam_offset[1]),
            MoveY(-self.forecam_offset[0], deadband=0.025),
            finite=False,
        ), success=2, total=3)

    def Fire(self):
        return Sequential(
            Log('Firing {} (!!)'.format(self.actuator)),
            FireActuator(self.actuator, self.FIRE_TIME),
        )

TORPS = [Torp((-0.02, -0.12), 'left_torpedo'), Torp((0.15, -0.12), 'right_torpedo')]

Cutout = namedtuple('Cutout', ['name', 'board_func', 'coord_func', 'is_noodle'])

class Torpedoes(Task):
    def on_first_run(self, vision, *args, **kwargs):

        def small_cutout_coord(board):
            return (board.small_cutout_x, board.small_cutout_y)
        def large_cutout_coord(board):
            return (board.large_cutout_x, board.large_cutout_y)

        small_tent_cutout = Cutout(
            'small tentacle',
            lambda: vision.boards[vision.TENT_ID],
            lambda: small_cutout_coord(vision.boards[vision.TENT_ID]),
            False,
        )
        # large_tent_cutout = Cutout(
            # 'large tentacle',
            # lambda: vision.boards[vision.TENT_ID],
            # lambda: large_cutout_coord(vision.boards[vision.TENT_ID]),
            # False,
        # )
        small_squid_cutout = Cutout(
            'small squid',
            lambda: vision.boards[vision.SQUID_ID],
            lambda: small_cutout_coord(vision.boards[vision.SQUID_ID]),
            True,
        )
        # large_squid_cutout = Cutout(
            # 'large squid',
            # lambda: vision.boards[vision.SQUID_ID],
            # lambda: large_cutout_coord(vision.boards[vision.SQUID_ID]),
            # False,
        # )

        self.use_task(Except(
            Sequential(
                Log('Starting torpedoes!'),
                Succeed(TryCompleteCutout(vision, small_tent_cutout, TORPS[0])),
                Succeed(TryCompleteCutout(vision, small_squid_cutout, TORPS[1])),
            ),

            Fail(Log('Global timeout, aborting torpedoes')),
            GlobalTimeoutError,
        )),

class TryCompleteCutout(Task):
    def on_first_run(self, vision, cutout, torp, *args, **kwargs):
        self.use_task(Conditional(
            Sequential(
                Log('Starting to attempt {} cutout'.format(cutout.name)),
                Retry(lambda: CompleteCutout(vision, cutout, torp), 3),
            ),

            on_fail=Fail(Sequential(
                Log('Failed to ever complete {} cutout, firing torpedo anyway'.format(cutout.name)),
                torp.Fire(),
            )),
        ))

class CompleteCutout(Task):
    MOVE_BOARD_TIMEOUT = 30
    ALIGN_CUTOUT_TIMEOUT = 60

    def on_first_run(self, vision, cutout, torp, *args, **kwargs):
        self.use_task(Sequential(
            Log('Attempting {} cutout'.format(cutout.name)),

            Conditional(
                Retry(lambda: Timeout(
                    MoveInFrontOfBoards(vision),
                    self.MOVE_BOARD_TIMEOUT,
                ), 3),
                on_fail=Fail(Log('Failed to ever move in front of boards')),
            ),

            RestorePos(WithPositionalControl(Sequential(
                Timeout(AlignCutout(cutout, torp), self.ALIGN_CUTOUT_TIMEOUT),
                torp.Fire(),
            ))),
        ))

class MoveInFrontOfBoards(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.use_task(Sequential(
            Log('Moving to torpedoes depth'),
            Depth(constants.depth),

            Log('Searching for boards'),
            MasterConcurrent(IdentifyBoard(vision), StillHeadingSearch()),

            AlignBoards(lambda: vision.boards, min_boards=2),
        ))

class IdentifyBoard(Task):
    def on_run(self, vision, *args, **kwargs):
        if sum(b is not None for b in vision.boards) > 0:
            self.finish()

class AlignBoards(Task):
    """
    Imprecisely align to both torpedoes boards.

    Pre: at least one board in sight
    Post: both boards centered in front of sub
    """

    TARGET_WIDTH = 0.8
    FOV = 30

    def board_count(self, boards_func):
        return sum(b is not None for b in boards_func())

    def on_first_run(self, boards_func, min_boards=0, *args, **kwargs):
        def avg_board(boards):
            if len(boards) == 1:
                return boards[0]

            for i, b in enumerate(boards):
                if b is not None:
                    avg_i = i
                    avg_b = copy.copy(b)
                    break

            other_b = boards[1]
            if avg_i == 0 and other_b is not None:
                for attr in ['x', 'y', 'skew', 'width', 'height']:
                    setattr(
                        avg_b,
                        attr,
                        (getattr(avg_b, attr) + getattr(other_b, attr)) / 2,
                    )

            return avg_b

        def assert_boards():
            return self.board_count(boards_func) >= min_boards

        self.task = Retry(lambda: Sequential(
            Log('Aligning boards'),
            Concurrent(
                AlignSurge(lambda: avg_board(boards_func()), self.TARGET_WIDTH),
                AlignSkew(lambda: avg_board(boards_func())),

                # Align heading
                GradualHeading(lambda: shm.kalman.heading.get() + avg_board(boards_func()).x * self.FOV),

                finite=False,
            ),
            Zero(),

            Conditional(
                FunctionTask(assert_boards),

                on_fail=Fail(Sequential(
                    Log('Less than {} boards aligned, backing up and retrying'.format(min_boards)),
                    MoveXRough(-0.25),
                )),
            ),
        ), 2)

    def on_run(self, boards_func, *args, **kwargs):
        if self.board_count(boards_func) == 0:
            self.loge('No boards visible, aborting align')
            self.finish(success=False)
        else:
            self.task()
            if self.task.finished:
                self.finish(success=self.task.success)

class AlignSurge(Task):
    DEADBAND = 0.03
    P = 0.5
    D = 0.2
    MAX_OUT = 0.3

    def on_first_run(self, board_func, width, *args, **kwargs):
        """ PID our distance instead of the raw width since it's linear """
        self.use_task(PIDLoop(
            input_value=lambda: 1 / board_func().width,
            output_function=VelocityX(),
            target=lambda: 1 / call_if_function(width),
            deadband=self.DEADBAND,
            p=self.P,
            d=self.D,
            negate=True,
            max_out=self.MAX_OUT,
        ))

class AlignSkew(Task):
    def on_first_run(self, board_func, *args, **kwargs):
        self.use_task(PIDLoop(
            input_value=lambda: board_func().skew,
            output_function=VelocityY(),
            target=0,
            deadband=0.1,
            p=1.5,
            d=0.5,
            max_out=0.2,
        ))

class RestorePos(Task):
    """
    Restore the position of the sub from before the given task started.
    """
    def on_first_run(self, task, *args, **kwargs):
        k = shm.kalman.get()

        self.use_task(Defer(task, Sequential(
            Log('Restoring position'),
            NavigationSpeed(GoToPositionRough(
                north=k.north,
                east=k.east,
                heading=k.heading,
                depth=k.depth,
            ), 0.3),
            Zero(),
        )))

class AlignCutout(Task):
    NORMAL_TARGET_BOARD_WIDTH = 2
    NORMAL_TARGET_BOARD_HALF_WIDTH = 1 / ((
        1 / NORMAL_TARGET_BOARD_WIDTH + 1 / AlignBoards.TARGET_WIDTH
    ) / 2)
    SEAWEED_TARGET_BOARD_WIDTH = 1

    ALIGN_POKER_DISTANCE = 0.63

    def on_first_run(self, cutout, torp, *args, **kwargs):
        def unsee_cutout():
            self.must_see_cutout = False

        self.must_see_cutout = True

        def TargetCutout(deadband):
            return ForwardTarget(
                lambda: cutout.coord_func(),
                (0, 0),
                deadband=(deadband, deadband),
                px=0.7,
                py=0.5,
                max_out=0.3,
            )

        def MoveClose(width, deadband):
            return ConsistentTask(Concurrent(
                TargetCutout(deadband),
                AlignSurge(cutout.board_func, width),
                finite=False,
            ), success=6, total=8)

        if cutout.is_noodle:
            target_cutout = Sequential(
                Log('Aligning in front of covered cutout'),
                MoveClose(self.SEAWEED_TARGET_BOARD_WIDTH, 0.04),
            )

        else:
            target_cutout = Sequential(
                Log('Aligning halfway to uncovered cutout'),
                MoveClose(self.NORMAL_TARGET_BOARD_HALF_WIDTH, 0.07),
                Log('Aligning close to uncovered cutout'),
                MoveClose(self.NORMAL_TARGET_BOARD_WIDTH, 0.07),
            )

        self.task = Sequential(
            Log('Aligning board containing cutout'),
            AlignBoards(lambda: [cutout.board_func()]),

            Log('Centering cutout'),
            TargetCutout(0.15),
            target_cutout,

            Zero(),
            FunctionTask(unsee_cutout),

            Sequential(
                Log('Moving poker beside seaweed'),
                NavigationSpeed(
                    MoveX(self.ALIGN_POKER_DISTANCE, deadband=0.02),
                    0.3,
                ),
            ) if cutout.is_noodle else NoOp(),

            Log('Aligning torpedo tube'),
            torp.AlignFromForecam(),
        )

    def on_run(self, cutout, torp, *args, **kwargs):
        if self.must_see_cutout and cutout.board_func() is None:
            self.loge('Cutout lost, cannot align')
            self.finish(success=False)

        else:
            self.task()
            if self.task.finished:
                self.finish(success=self.task.success)

class VisionTask(Task):
    def on_first_run(self, task_func, *args, **kwargs):
        vision = Vision()
        self.use_task(MasterConcurrent(task_func(vision, *args, **kwargs), vision))

def Full(): return VisionTask(Torpedoes)
