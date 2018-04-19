import shm
from shm.watchers import watcher
from mission.framework.task import Task
from mission.framework.targeting import PIDLoop
from mission.framework.combinators import (
    Sequential,
    Concurrent,
    MasterConcurrent,
    Retry,
    Conditional,
    Defer,
)
from mission.framework.movement import (
    Depth,
    RelativeToInitialDepth,
    RelativeToCurrentDepth,
    VelocityX,
    VelocityY,
    RelativeToInitialHeading,
)
from mission.framework.timing import Timer, Timeout, Timed
from mission.framework.primitive import (
    Zero,
    FunctionTask,
    NoOp,
    Log,
    Succeed,
    Fail,
)
from mission.framework.actuators import FireActuator
from mission.framework.position import MoveX, MoveXY, PositionalControl, MoveXYRough
from mission.framework.track import (
    Matcher,
    Match,
    Observation,
    HeadingInvCameraCoord,
)
from mission.missions.ozer_common import(
    ConsistentTask,
    CenterCentroid,
    AlignHeadingToAngle,
    SearchWithGlobalTimeout,
    Except,
    GlobalTimeoutError,
    Altitude,
    AlignAmlan,
    AMLANS,
    Zeroed,
)
from mission.constants.config import bins as constants

"""
Bins 2017!
"""

class Vision(Task):
    # Bin IDs
    TARGET_BIN = 0 # The bin that was originally covered (even if not currently)
    OTHER_BIN = 1 # The bin that was never covered

    class BinObs(HeadingInvCameraCoord):
        def __init__(self, shm_bin, heading):
            super().__init__(shm_bin.x, shm_bin.y, heading)
            self.adopt_attrs(shm_bin)

    def __init__(self, *args, **kwargs):
        super().__init__()
        # Vision object needs to be ready for rest of mission to access, so must
        # initialize before on_first_run()

        shm.vision_debug.color_r.set(0)
        shm.vision_debug.color_g.set(200)
        shm.vision_debug.color_b.set(255)

        self.bins_matcher = Matcher(
            [], # We don't know the bins' relative positions ahead of time
            num_trackers=2,
        )

        self.watcher = watcher()
        self.watcher.watch(shm.bins_vision)
        self.pull()

    def on_run(self, *args, **kwargs):
        if self.watcher.has_changed():
            self.pull()

    def coords(self, objs, filter=lambda x: True):
        return [(obj.obs.x, obj.obs.y) for obj in objs if obj.obs is not None and filter(obj)]

    def target_bin(self):
        targets = [bin for bin in self.bins if bin.id == self.TARGET_BIN]
        if len(targets) >= 1:
            return targets[0]
        else:
            return None

    def classify(self, single_bin=False):
        required_bins = 1 if single_bin else 2

        if sum(bin.obs is not None for bin in self.bins) < required_bins:
            self.loge('Failed to classify, less than {} bin{} available'.format(
                required_bins,
                '' if required_bins == 1 else 's',
            ))
            return False

        covered, uncovered = [], []
        for bin in self.bins:
            if bin.obs is not None:
                (covered if bin.obs.covered else uncovered).append(bin)

        if single_bin:
            # Prioritize uncovered bin
            if len(uncovered) > 0:
                target = uncovered[0]
                other = covered[0] if len(covered) > 0 else None
            else: # Guaranteed at least one bin exists somewhere
                target = covered[0]
                other = uncovered[0] if len(uncovered) > 0 else None

            pattern = [Match(self.TARGET_BIN, target.obs)]
            if other is not None:
                pattern.append(Match(self.OTHER_BIN, other.obs))

        else:
            # If two of the same kind of bin detected, guess a classification
            def log_same_kind(t):
                self.logw('Two {} bins detected, guessing the target/other classification'.format(t))

            if len(covered) == 2:
                log_same_kind('covered')
                target, other = tuple(covered)

            elif len(uncovered) == 2:
                log_same_kind('uncovered')
                target, other = tuple(uncovered)

            else: # Guaranteed one bin in each category
                target, other = covered[0], uncovered[0]

            pattern = [
                Match(self.TARGET_BIN, target.obs),
                Match(self.OTHER_BIN, other.obs),
            ]

        self.bins_matcher.update_pattern(pattern)

        return True

    def pull(self):
        shm_bins = [
            shm.bins_bin0.get(),
            shm.bins_bin1.get(),
        ]
        heading = shm.kalman.heading.get()
        observations = [self.BinObs(sbin, heading) for sbin in shm_bins if sbin.visible]
        self.bins = self.bins_matcher.match(observations)

        # Debug locations
        for i, bin in enumerate(self.bins):
            debug_info_g = shm._eval('vision_debug{}'.format(i))
            debug_info = debug_info_g.get()
            if bin.obs is None:
                debug_info.text = bytes('', 'utf8')
            else:
                if bin.id is not None:
                    debug_info.text = bytes('Target bin' if bin.id == self.TARGET_BIN else 'Other bin', 'utf8')
                else:
                    debug_info.text = bytes('Bin {}'.format(i), 'utf8')
                debug_info.x, debug_info.y = bin.obs.x, bin.obs.y

            debug_info_g.set(debug_info)

class Bins(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.use_task(Conditional(
            Except(Sequential(
                Log('Starting bins'),

                Log('Attempting bins assuming both are visible'),
                Conditional(
                    TryBins(vision, single_bin=False),

                    on_fail=Sequential(
                        Log('Attempting bins assuming only one is visible'),
                        TryBins(vision, single_bin=True),
                    ),
                )

            ), Fail(), GlobalTimeoutError),

            Log('Bins success! :O'),

            Fail(Sequential(Zero(), FastDrop(), Log('Bins failure! :('))),
        ))

class TryBins(Task):
    """ Try to complete bins given a certain number of bins to look for """

    def on_first_run(self, vision, single_bin, *args, **kwargs):
        self.use_task(Sequential(
            Log('Retracting Amlan'),
            AMLANS[0].FastRetract(),

            Conditional(
                Retry(lambda: ClassifyBins(vision, single_bin), 3),
                on_fail=Fail(Log('Failed to ever classify bins')),
            ),

            Conditional(
                Retry(lambda: Uncover(vision), 3),
                on_fail=Fail(Log('Failed to ever remove cover')),
            ),

            Conditional(
                Retry(lambda: Drop(vision), 3),
                on_fail=Fail(Log('Failed to ever accurately drop markers')),
            ),
        ))

class ClassifyBins(Task):
    def on_first_run(self, vision, single_bin, *args, **kwargs):
        self.use_task(Conditional(
            Sequential(
                MoveAboveBins(vision),
                Timer(0.5), # Wait for vision to stabilize
                FunctionTask(lambda: vision.classify(single_bin)),
            ),

            Log('Bins classified'),

            Fail(Log('Failed to classify bins')),
        ))

class Uncover(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.use_task(Conditional(
            Sequential(
                Retry(lambda: MoveAboveBins(vision), 3),
                RemoveCover(vision),
            ),

            Log('Bin uncovered!'),

            Fail(Log('Failed to uncover bin')),
        ))

class MoveAboveBins(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.use_task(Conditional(
            Sequential(
                Log('Moving to depth where bins are visible'),
                Depth(constants.see_both_depth, error=0.2, *args, **kwargs),

                Log('Searching for bin'),
                MasterConcurrent(IdentifyBin(vision), SearchWithGlobalTimeout()),

                Log('Centering bins'),
                CenterBins(vision),
            ),

            on_fail=Fail(Log('Failed to move above bins')),
        ))

class IdentifyBin(Task):
    def on_run(self, vision, *args, **kwargs):
        if any(bin.obs is not None for bin in vision.bins):
            self.finish()

class CenterBins(Task):
    def on_first_run(self, vision, filter=lambda bin: True, precision=0, *args, **kwargs):

        def bin_points():
           return [
               (bin.obs.x, bin.obs.y) for bin in vision.bins
               if bin.obs is not None and filter(bin)
           ]

        self.use_task(CenterCentroid(bin_points, precision=precision))


class RemoveCover(Task):
    def on_first_run(self, vision, *args, **kwargs):
        def check_removed():
            bin = vision.target_bin()
            return bin is not None and not bin.obs.covered

        def CheckRemoved():
            return FunctionTask(check_removed)

        self.use_task(Zeroed(Conditional(
            Sequential(
                MoveAboveBins(vision),
                Conditional(
                    CheckRemoved(),

                    Log('The cover is already gone?!?'),

                    Sequential(
                        Zero(),
                        AlignOverTargetBin(vision, 90),
                        LiftOffCover(vision),
                        Log('Verifying the cover was removed'),
                        MoveAboveBins(vision),
                        CheckRemoved(),
                    ),
                ),
            ),
            on_fail=Fail(Log('Failed to remove cover')),
        )))

class LiftOffCover(Task):
    PICKUP_DELTA_DEPTH = 0.4
    DELTA_DEPTH_TIMEOUT = 4
    TOTAL_TIMEOUT = 60
    SLIDE_SPEED = 0.4
    SLIDE_TIME = 4

    def on_first_run(self, vision, *args, **kwargs):
        self.use_task(Zeroed(Timeout(
            Sequential(
                Log('Attempting to grab handle'),
                GetHandle(vision, AMLANS[0]),

                Log('Carrying cover away'),
                RelativeToInitialDepth(-self.PICKUP_DELTA_DEPTH, error=0.1),
                Timed(VelocityX(self.SLIDE_SPEED), self.SLIDE_TIME),
                Zero(),

                Log('Dropping cover off here'),
                AMLANS[0].Retract(),
                Timer(1.5),

                Log('Attempting to return to near pre-grab location'),
                RelativeToInitialDepth(-1),
                Timed(VelocityX(-self.SLIDE_SPEED), self.SLIDE_TIME * 5 / 6),
                Zero(),
            ), self.TOTAL_TIMEOUT)))

class GetHandle(Task):
    GRAB_TIME = 4

    def on_first_run(self, vision, amlan, *args, **kwargs):
        self.use_task(Zeroed(Sequential(
            Log('Aligning Amlan'),
            AlignAmlan(
                vision,
                vision.target_bin,
                amlan,
                Depth(constants.above_bin_depth),
            ),

            Log('Moving to handle grab depth'),
            Timed(Depth(constants.grab_depth), self.GRAB_TIME),

            Log('Extending Amlan'),
            amlan.Extend(),
            Timer(2),
        )))

class AlignOverTargetBin(Task):
    TIMEOUT = 40

    def on_first_run(self, vision, angle, double_align=False, *args, **kwargs):

        def CenterTargetBin(precision):
            return CenterBins(vision, lambda bin: bin.id == vision.TARGET_BIN, precision=precision)

        def AlignTargetBin():
            return AlignHeadingToAngle(lambda: vision.target_bin().obs.angle, angle, mod=180)

        def DepthAlign(depth):
            return Concurrent(
                AlignTargetBin(),
                CenterTargetBin(1),
                Depth(depth),
                finite=False,
            )

        self.task = Zeroed(Timeout(Sequential(
            Log('Centering over target bin'),
            CenterTargetBin(0),

            Log('Aligning target bin'),
            Concurrent(
                AlignTargetBin(),
                CenterTargetBin(0),
                finite=False,
            ),

            Log('Going half down to precisely align with target bin'),
            DepthAlign((constants.see_both_depth + constants.above_bin_depth) / 2),

            Sequential(
                Log('Going down to fully align to bin'),
                DepthAlign(constants.above_bin_depth),
            ) if double_align else NoOp(),

            Zero(),
            PositionalControl(),
        ), self.TIMEOUT))

    def on_run(self, vision, *args, **kwargs):
        if vision.target_bin() == None:
            self.loge('Failed to align over target bin, lost bin')
            self.finish(success=False)
            Zero()()

        else:
            self.task()
            if self.task.finished:
                self.finish(success=self.task.success)

class AlignBinDepth(Task):
    def on_first_run(self, vision, bin_func, *args, **kwargs):
        self.use_task(ConsistentTask(PIDLoop(
            input_value=lambda: bin_func().obs.length,
            output_function=RelativeToCurrentDepth(),
            target=1.2,
            deadband=0.02,
            p=0.7,
            d=0.3,
        )))

class Drop(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.use_task(Conditional(
            Sequential(
                Log('Starting drop'),
                Retry(lambda: MoveAboveBins(vision), 3),
                AlignOverTargetBin(vision, 90, double_align=True),
                FireMarkers(),
            ),

            on_fail=Fail(Log('Failed to drop')),
        ))

class FireMarkers(Task):
    MARKERS_X_OFFSET = -0.04
    FIRE_TIME = 0.5

    def on_first_run(self, *args, **kwargs):
        self.use_task(Sequential(
            Log('Firing markers!'),
            MoveX(-self.MARKERS_X_OFFSET, deadband=0.02),
            FireActuator('left_marker', self.FIRE_TIME),
            FireActuator('right_marker', self.FIRE_TIME),
        ))

class FastDrop(Task):
    def on_first_run(self, *args, **kwargs):
        self.use_task(Sequential(
            Log('Dropping markers quickly wherever we are now'),
            FireMarkers(),
        ))

class VisionTask(Task):
    def on_first_run(self, task_func, *args, **kwargs):
        vision = Vision()
        self.use_task(MasterConcurrent(task_func(vision, *args, **kwargs), vision))

def Full(): return VisionTask(Bins)
def Found(): return ConsistentTask(VisionTask(IdentifyBin), success=20, total=30)
