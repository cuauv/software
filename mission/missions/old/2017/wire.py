import math
from collections import namedtuple

import shm
from mission.constants.config import wire as constants
from shm import kalman
from shm.watchers import watcher
from mission.framework.task import Task
from mission.framework.combinators import Concurrent, Sequential, MasterConcurrent, \
    Conditional, Retry, While
from mission.framework.movement import Heading, Roll, Pitch, Depth, VelocityX, VelocityY, \
    RelativeToCurrentHeading, RelativeToCurrentDepth, RelativeToCurrentRoll, RelativeToInitialHeading
from mission.framework.targeting import PIDLoop
from mission.framework.timing import Timer, Timed
from mission.framework.helpers import get_forward_camera, ConsistencyCheck
from mission.framework.primitive import Zero, Log, Fail, Succeed
from mission.framework.track import ConsistentObject
from mission.framework.search import VelocityHeadingSearch
from auv_python_helpers.angles import heading_sub_degrees
from mission.missions.ozer_common import ConsistentTask

class Vision(Task):
    """
    Process vision results for easy use by mission
    """
    bar_fields = ['x1', 'y1', 'x2', 'y2', 'area', 'prob']
    bar_types = ['left', 'right', 'bottom']
    Bar = namedtuple('Bar', bar_fields)

    def on_first_run(self, *args, **kwargs):
        self.watcher = watcher()
        self.watcher.watch(shm.wire_results)

        self.consistent_objs = {t: ConsistentObject() for t in self.bar_types}

        self.pull()

    def on_run(self, *args, **kwargs):
        if self.watcher.has_changed():
            self.pull()

    def pull(self):
        self.shm = shm.wire_results.get()
        cam = get_forward_camera()
        self.cam_width, self.cam_height = cam['width'], cam['height']

        for btype in self.bar_types:
            vals = {}
            for field in self.bar_fields:
                vals[field] = getattr(self.shm, '{}_{}'.format(btype, field))
            bar = self.Bar(**vals)
            if not bar.prob:
                bar = None
            setattr(self, btype, self.consistent_objs[btype].map(bar))

        # TODO fix
        self.left, self.right = None, None

class Wire(Task):
    """
    Find the channel, align with it, and go through with style
    """
    def on_first_run(self, vision, *args, **kwargs):
        initial_heading = shm.kalman.heading.get()
        depth_set = DepthRestore()

        self.use_task(
            Conditional(
                Sequential(
                    MasterConcurrent(
                        Sequential(
                            Retry(lambda: Sequential(
                                Log('Returning to initial heading'),
                                Heading(initial_heading),

                                Log('Going to depth'),
                                depth_set,

                                #Log('Moving forward away last pos'),
                                #Timed(VelocityX(0.5), 1),
                                #Zero(),

                                Log('Searching for gate'),
                                MasterConcurrent(
                                    IdentifyGate(vision),
                                    VelocityHeadingSearch(initial_heading=initial_heading),
                                ),
                                Zero(),

                                Log('Found gate, aligning'),
                                AlignChannel(vision),
                            ), float('inf')),
                        ),

                        Fail(Timer(180)),
                    ),
                    Log('Aligned to gate, moving closer and fixing depth'),
                    MoveCloser(2),
                    Log('Beginning spin'),
                    StyleSegmentedSpin(),
                ),
                
                Sequential(
                    Log('Wire completed successfully!'),
                    Timed(VelocityX(.4), 2),
                    Zero(),
                    RelativeToInitialHeading(180),
                ),

                Log('Traveled too far without task completion'),
            )
        )

def checkNotAligned(vision):
    #Returns False until aligned
    a = abs(x_ratio(vision) - .5) < .09
    b = abs(bbar_angle(vision)) < 5
    c = abs(bbar_width_ratio(vision)- constants.BBAR_RATIO_TARGET) < .05
    print("Heading Aligned: {}".format(a))
    print("Sway Aligned: {}".format(b))
    print("Fore Aligned: {}".format(c))
    return not (a and b and c)

class AlignChannel(Task):
    def on_first_run(self, vision, *args, **kwargs):
        check = lambda: checkAligned(vision)

        self.use_task(
            Conditional(
            While(
            lambda: Sequential(
                Log("Restoring depth"),
                Depth(constants.WIRE_DEPTH),
                Log('Align Heading'),
                AlignHeading(vision),
                Zero(),
                Log('Align Sway'),
                AlignSway(vision),
                Zero(),
                Log('Align Heading'),
                AlignHeading(vision),
                Zero(),
                Log('Align Fore'),
                AlignFore(vision),
                Zero(),
                ), lambda: checkNotAligned(vision)
            ),

            on_fail=Sequential(
                Log('Lost the gate, backing up and looking again'),
                Succeed(Concurrent(
                    Fail(Timed(VelocityX(-.4), 6)),
                    Fail(IdentifyGate(vision)),
                )),
                Fail(),
                )
            )
            )

def bbar_width_ratio(vision):
    if vision.bottom is not None:
        length = abs(vision.bottom.x1 - vision.bottom.x2)
        return length / vision.cam_width
    elif vision.left is not None and vision.right is not None:
        # We can see both left and right bars
        left_x = (vision.left.x1 + vision.left.x2) / 2
        right_x = (vision.right.x1 + vision.right.x2) / 2
        return (right_x - left_x) / vision.cam_width
    else:
        return 0

class MoveCloser(Task):
    def on_first_run(self, time, *args, **kwargs):
        self.use_task(Sequential(
            Depth(constants.WIRE_THRU_DEPTH, error=.1),
            Timed(VelocityX(.3), time),
            Zero(),
            #Timed(VelocityY(.2), 1),
            Zero()
        ))

class IdentifyGate(Task):
    """
    Finish when we can see a good enough amount of the gate
    """
    min_bbar_width = 0.3

    def on_first_run(self, vision):
        self.seen_cons_check = ConsistencyCheck(4, 5)

    def on_run(self, vision):
        self.seen_cons_check.add(vision.bottom is not None and \
                # (vision.left is not None or vision.right is not None) and \
                                 bbar_width_ratio(vision) >= self.min_bbar_width)
        if self.seen_cons_check.check():
            self.finish()

class CheckSpinCompletion(Task):
    def on_first_run(self, n):
        self.heading = shm.kalman.heading.get()
        self.cumul = 0

    def on_run(self, n):
        new_heading = shm.kalman.heading.get()
        self.cumul += heading_sub_degrees(new_heading, self.heading)
        self.heading = new_heading

        if abs(self.cumul) > 360 * n:
            self.finish()

class StyleSuperSpin(Task):
    def on_first_run(self, clockwise=True, steps=5, spins=2, *args, **kwargs):
        delta_heading = [-1, 1][clockwise] * 45
        subspins = [MasterConcurrent(CheckSpinCompletion(2),
                      RelativeToCurrentHeading(delta_heading))]

        self.use_task(Sequential(
            Timed(VelocityX(1), 1), Zero(),

            MasterConcurrent(
                Sequential(subtasks=subspins),
                VelocityX(lambda: 2 * math.cos(math.radians(shm.kalman.heading.get()))),
                VelocityY(lambda: 10 * math.sin(math.radians(shm.kalman.heading.get()))),
            ),

            Zero(),
        ))

class StyleSegmentedSpin(Task):
    def on_first_run(self, spins=2, segment_time=0.5, *args, **kwargs):
        initial_heading = shm.kalman.heading.get()

        def Spin():
            return Sequential(
                Timed(VelocityX(1), segment_time), Zero(),
                Heading(initial_heading + 90),
                Timed(VelocityY(-1), segment_time), Zero(),
                Heading(initial_heading + 180),
                Timed(VelocityX(-1), segment_time), Zero(),
                Heading(initial_heading + 270),
                Timed(VelocityY(1), segment_time), Zero(),
                Heading(initial_heading),
            )

        self.use_task(Sequential(subtasks=[Spin() for i in range(spins)]))

class AlignHeading(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.pid = PIDLoop(
            input_value=lambda: x_ratio(vision),
            output_function=RelativeToCurrentHeading(),
            target=0.5,
            deadband=.09,
            p=50,
            negate=True,
        )

    def on_run(self, vision, *args, **kwargs):
        # Try to center on entire gate first
        if vision.bottom is None:
            self.finish(success = False)
            return

        if vision.bottom is not None or \
                (vision.left is not None  and vision.right is not None):
            self.pid()

        elif vision.left is not None: # Otherwise try to find other vertical bar
            RelativeToCurrentHeading(1)()
        elif vision.right is not None:
            RelativeToCurrentHeading(-1)()
        else:
            self.finish()

        if self.pid.finished:
            self.finish()

class AlignDepth(Task):
    min_depth = 1

    def on_first_run(self, vision, *args, **kwargs):
        self.pid = PIDLoop(
            input_value=lambda: self.y_ratio(vision),
            output_function=RelativeToCurrentDepth(),
            target=0.75,
            deadband=constants.DEFAULT_DEADBAND,
            p=2,
            d=4,
            negate=True,
        )

    def on_run(self, vision, *args, **kwargs):
        if vision.bottom is not None:
            self.pid()
        desire = shm.desires.depth.get()
        shm.desires.depth.set(max(self.min_depth, desire))

        if self.pid.finished:
            self.finish()

    def y_ratio(self, vision):
        """
        Precondition: we can see at least one of the bars
        """
        avg_y = (vision.bottom.y1 + vision.bottom.y2) / 2
        return avg_y / vision.cam_height

class AlignFore(Task):
    min_bbar_width = 0.1

    def on_first_run(self, vision, *args, **kwargs):
        self.pid = PIDLoop(
            input_value=lambda: bbar_width_ratio(vision),
            output_function=VelocityX(),
            target= constants.BBAR_RATIO_TARGET,
            deadband=constants.DEFAULT_DEADBAND,
            p=1,
            d=4
        )
        self.retreat_task = Sequential(Log('Retreat'), Timed(VelocityX(-.3), 1))

    def on_run(self, vision, *args, **kwargs):

        if self.retreat_task.finished:
            self.finish()

        if (vision.bottom is not None and ((vision.bottom.x1 <5) ^ (abs(vision.bottom.x2 -vision.cam_width) < 5))):
            self.retreat_task()
            if self.retreat_task.finished:
                self.finish()

        elif vision.bottom is not None and (abs(vision.bottom.y1 - vision.cam_height) <5 or abs(vision.bottom.y2 - vision.cam_height) < 5):
            self.retreat_task()
            if self.retreat_task.finished:
                self.finish()


        elif vision.bottom is not None and abs(bbar_angle(vision)) > 10:
            self.finish()

        elif vision.bottom is None:
            self.finish(sucess=False)

        #print(bbar_width_ratio(vision))
        elif (vision.bottom is not None or \
            (vision.left is not None and vision.right is not None)) and \
            bbar_width_ratio(vision) >= self.min_bbar_width:
            self.pid()
            if self.pid.finished:
                self.finish()
        else:
            self.finish()

class AlignSway(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.pid = PIDLoop(
            input_value=lambda: bbar_angle(vision),
            output_function=VelocityY(),
            target=0,
            deadband=3,
            p=0.8,
            d=0.4,
        )
        self.depth = Depth(constants.WIRE_DEPTH)

    def on_run(self, vision, *args, **kwargs):
        # If we see both bars, try to sway to minimize their height difference
        #if vision.bottom is not None:
        #    self.pid()
       #else:
       #    self.finish()

        #TODO
        angle = bbar_angle(vision)
        if vision.bottom is None or (vision.bottom is not None and ((angle < 0 and vision.bottom.x1 <100) ^ \
            (angle > 0 and abs(vision.bottom.x2 - vision.cam_width) < 100) or \
                                     abs(vision.bottom.y1 - vision.cam_height) < 100) or \
                                     abs(vision.bottom.y2 - vision.cam_height) < 100):
        #if (shm.desires.sway_speed.get() > 0 and vision.left is None) or (shm.desires.sway_speed.get() < 0 and vision.right is None):
            self.finish()
        else:
            self.depth()
            self.pid();

        if self.pid.finished:
            self.finish()

def bbar_angle(vision):
    if vision.bottom is None:
        return 0
    left_y, right_y = vision.bottom.y1, vision.bottom.y2
    if vision.bottom.x1 > vision.bottom.x2:
        left_y, right_y = right_y, left_y
    width = abs(vision.bottom.x2 - vision.bottom.x1)
    bar_angle = math.degrees(math.atan2(right_y - left_y, width))
    roll_adjusted_angle = (bar_angle + shm.kalman.roll.get()) % 360
    if roll_adjusted_angle > 180:
        roll_adjusted_angle -= 360
    return roll_adjusted_angle



def x_ratio(vision):
    """
    Precondition: we can see the bottom bar, or the left and right bars
    """
    if vision.bottom is not None:
        avg_x = (vision.bottom.x1 + vision.bottom.x2) / 2
        return avg_x / vision.cam_width
    elif vision.left is not None and vision.right is not None: # We see left and right bars
        left_x_avg = (vision.left.x1 + vision.left.x2) / 2
        right_x_avg = (vision.right.x1 + vision.right.x2) / 2
        avg = (left_x_avg + right_x_avg) / 2
        return avg / vision.cam_width
    else:
        return 100

class DepthRestore(Task):
    """
    Saves the current depth and restores it at a later time
    """
    def __init__(self, depth=None, *args, **kwargs):
        """
        depth - a depth to use as the original depth
        """
        super().__init__(*args, **kwargs)
        # Store the start depth of the sub
        if depth is None:
            self.start_depth = constants.WIRE_DEPTH
        else:
            self.start_depth = depth
        self.depth_task = Depth(self.start_depth, error=.05)

    #def on_first_run(self):
        #self.logv("Starting {} task".format(self.__class__.__name__))

    def on_run(self):
        # Restore the sub's depth to the stored one
        if not self.depth_task.finished:
            self.depth_task()
        else:
            self.finish()

class OptimalMission(Task):
    def on_first_run(self, mode=None, main_task_func=None, *args, **kwargs):
        self.has_made_progress = False
        self.use_task(main_task_func())
        # TODO @AlexO Update when we've made progress! (seen gate?)

    def desiredModules(self):
        return [shm.vision_modules.Wire]

class VisionTask(Task):
    def on_first_run(self, task_class, *args, **kwargs):
        self.vision = Vision()
        task = task_class(self.vision, *args, **kwargs)
        self.task = Sequential(Timer(1), task)

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            try:
                camera = get_forward_camera()
                self.vision(camera)
                self.task()
            except RuntimeError:
                self.loge('Vision not running, refusing to run mission')
        else:
            self.finish()

align = lambda: VisionTask(AlignChannel)
full = lambda: OptimalMission(main_task_func=lambda: VisionTask(Wire))

# basicfull = lambda: Vision(Sequential(AlignChannel(), StyleBasic()))
# basic = lambda: Vision(StyleBasic())
# pitched = lambda: Vision(StylePitched())
# sideways = lambda: Vision(StyleSideways())
# upside_down = lambda: Vision(StyleUpsideDown())
# loop = lambda: Vision(StyleLoop())
superspin = lambda: StyleSuperSpin()

class Flip180(Task):
    def on_first_run(self, *args, **kwargs):
        #heading = Heading((kalman.heading.get() + 180) % 360, error=1)
        return Sequential(Pitch(0, error=1), Roll(0, error=1), Timer(1.5), Heading(lambda: kalman.heading.get() + 180, error=1), Timer(1))

unspin = lambda: StyleSuperSpin(clockwise=False)
