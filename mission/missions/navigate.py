import math
from collections import namedtuple

import aslam
import shm
from mission.constants.config import navigate as constants
from shm import kalman
from shm.watchers import watcher
from mission.framework.task import Task
from mission.framework.combinators import Concurrent, Sequential, MasterConcurrent
from mission.framework.movement import Heading, Roll, Pitch, Depth, VelocityX, VelocityY, \
    RelativeToCurrentHeading, RelativeToCurrentDepth, RelativeToCurrentRoll
from mission.framework.position import MoveX, MoveY, GoToPosition
from mission.framework.targeting import PIDLoop
from mission.framework.timing import Timer
from mission.framework.helpers import get_forward_camera, ConsistencyCheck
from mission.framework.primitive import Zero, Log
from mission.framework.track import ConsistentObject
from mission.framework.search import HeadingSearch
from mission.missions.ozer_common import SequentialSuccess, Conditional, Retry, \
    ConcurrentSuccess, CheckDistance
from auv_python_helpers.angles import heading_sub_degrees

class Vision(Task):
    """
    Process vision results for easy use by mission
    """
    bar_fields = ['x1', 'y1', 'x2', 'y2', 'area', 'prob']
    bar_types = ['left', 'right', 'bottom']
    Bar = namedtuple('Bar', bar_fields)

    def on_first_run(self, *args, **kwargs):
        self.watcher = watcher()
        self.watcher.watch(shm.navigate_results)

        self.consistent_objs = {t: ConsistentObject() for t in self.bar_types}

        self.pull()

    def on_run(self, *args, **kwargs):
        if self.watcher.has_changed():
            self.pull()

    def pull(self):
        self.shm = shm.navigate_results.get()
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

class Navigate(Task):
    """
    Find the channel, align with it, and go through with style
    """
    def on_first_run(self, vision, *args, **kwargs):
        initial_heading = shm.kalman.heading.get()

        self.task = Sequential(Conditional(
            Sequential(
                ConcurrentSuccess(CheckDistance(constants.max_distance), Sequential(
                    Retry(lambda: SequentialSuccess(
                        Log('Returning to initial heading'),
                        Heading(initial_heading),

                        Log('Moving forward away last pos'),
                        MoveX(1, deadband=0.2),

                        Log('Searching for gate'),
                        MasterConcurrent(IdentifyGate(vision), HeadingSearch(initial_heading)),
                        Zero(),

                        Log('Found gate, aligning'),
                        AlignChannel(vision),
                    ), float('inf')),
                ), n=1),
                Log('Aligned to gate, beginning to spin'),
                StyleSuperSpin(),
            ),

            Log('Navigate completed successfully!'),

            Log('Traveled too far without task completion'),
        ))

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.finish()

class GoToPipe(Task):
    """
    Move to and align with the pipe after buoys
    """
    def on_first_run(self, *args, **kwargs):
        pipe_results = shm.navigate_pipe_results.get()
        self.task = Sequential(
            Log('Returning to pipe position'),
            GoToPosition(
                pipe_results.north,
                pipe_results.east,
                depth=pipe_results.depth,
                optimize=True,
            ),

            Log('Aligning with pipe'),
            Heading(pipe_results.heading),
        )

    def on_run(self, *args, **kwargs):
        if not self.task.finished:
            self.task()
        else:
            self.finish()

def bbar_width_ratio(vision):
    if vision.bottom is not None:
        length = abs(vision.bottom.x1 - vision.bottom.x2)
        return length / vision.cam_width
    else:
        # We can see both left and right bars
        left_x = (vision.left.x1 + vision.left.x2) / 2
        right_x = (vision.right.x1 + vision.right.x2) / 2
        return (right_x - left_x) / vision.cam_width

class IdentifyGate(Task):
    """
    Finish when we can see a good enough amount of the gate
    """
    min_bbar_width = 0.15

    def on_first_run(self, vision):
        self.seen_cons_check = ConsistencyCheck(4, 5)

    def on_run(self, vision):
        self.seen_cons_check.add(vision.bottom is not None and \
                # (vision.left is not None or vision.right is not None) and \
                                 bbar_width_ratio(vision) >= self.min_bbar_width)
        if self.seen_cons_check.check():
            self.finish()

class Style(Task):
    """
    Base class for all styles

    Start: facing center of gate
    Finish: facing away from center of gate
    """
    def on_first_run(self, *args, **kwargs):
        self.logi('Starting')
        self.style_on_first_run(*args, **kwargs)

    def on_run(self, *args, **kwargs):
        self.style_on_run(*args, **kwargs)

    def on_finish(self, *args, **kwargs):
        self.style_on_finish(*args, **kwargs)
        Zero()()
        self.logi('Finished in {} seconds!'.format(
            self.this_run_time - self.first_run_time))

    """
    These should be overridden by child style classes
    """
    def style_on_first_run(self, *args, **kwargs):
        pass
    def style_on_run(self, *args, **kwargs):
        pass
    def style_on_finish(self, *args, **kwargs):
        pass

class StyleBasic(Style):
    """
    Simply moves forward
    """
    def style_on_first_run(self, distance=5, *args, **kwargs):
        self.movement = MoveX(distance)

    def style_on_run(self, *args, **kwargs):
        if not self.movement.has_ever_finished:
            self.movement()
        else:
            self.finish()

class StyleSideways(Style):
    """
    Heading changes 90 degrees starboard, so that sub is facing either right or left

    If `starboard` is False, then heading changes 90 degrees port
    """
    def style_on_first_run(self, starboard=True, *args, **kwargs):
        current_heading = kalman.heading.get()
        if starboard:
            change_heading = Heading(current_heading + 90, error=1)
            movement = MoveY(-5)
        else:
            change_heading = Heading(current_heading - 90, error=1)
            movement = MoveY(5)
        heading_restore = Heading(current_heading, error=1)
        self.style_sideways = Sequential(change_heading, movement, heading_restore)

    def style_on_run(self, *args, **kwargs):
        if not self.style_sideways.has_ever_finished:
            self.style_sideways()
        else:
            self.finish()

class StyleUpsideDown(Style):
    """
    Roll changes 180 degrees, so that sub is upside down
    """
    def style_on_first_run(self, *args, **kwargs):
        change_roll = Roll(180, error=1)
        movement = MoveX(5)
        restore_roll = Roll(0, error=1)
        self.style_upside_down = Sequential(change_roll, movement, restore_roll)

    def style_on_run(self, *args, **kwargs):
        if not self.style_upside_down.has_ever_finished:
            self.style_upside_down()
        else:
            self.finish()

class StylePitched(Style):
    """
    Pitch changes 75 degrees, so that sub is facing either down or up
    The reason for 75 degrees is so that th sub does not rapidly twist back
    and forth, in an attempt to maintain a stable heading

    If `up` is False, then sub pitches downwards
    """
    def style_on_first_run(self, up=True, *args, **kwargs):
        if up:
            change_pitch = Pitch(75, error=1)
        else:
            change_pitch = Pitch(-75, error=1)
        movement = MoveX(5)
        restore_pitch = Pitch(0, error=1)
        self.style_pitched = Sequential(change_pitch, movement, restore_pitch)

    def style_on_run(self, *args, **kwargs):
        if not self.style_pitched.has_ever_finished:
            self.style_pitched()
        else:
            self.finish()

class StyleLoop(Style):
    """
    Does a loop around the center bar of the channel

    Goes forward and under, backwards and over, then forwards and over
    """
    def style_on_first_run(self, *args, **kwargs):
        move_distance = 5 # meters
        depth_offset = 1 # offset to go up or down

        def generate_curve(distance, depth_offset, depth, iterations):
            #TODO: Make this curve more 'curvy'
            movement = []
            dist_tick = distance / iterations
            current_depth = depth
            depth_tick = depth_offset / (iterations - 1)
            for t in range(iterations):
                movement.append(Concurrent(MoveX(dist_tick), Depth(current_depth, error=.1)))
                current_depth += depth_tick
            return Sequential(subtasks=movement)

        current_depth = kalman.depth.get()
        forward_and_down = generate_curve(move_distance / 2, depth_offset, current_depth, 3)
        forward_and_up = generate_curve(move_distance / 2, -depth_offset, current_depth + depth_offset, 3)
        backward_and_up = generate_curve(-move_distance / 2, -depth_offset, current_depth, 3)
        backward_and_down = generate_curve(-move_distance / 2, depth_offset, current_depth - depth_offset, 3)
        forward = Sequential(generate_curve(move_distance / 2, -depth_offset, current_depth, 3),
                             generate_curve(move_distance / 2, depth_offset, current_depth - depth_offset, 3))
        self.style_loop = Sequential(forward_and_down, forward_and_up, backward_and_up,
                                     backward_and_down, forward)

    def style_on_run(self, *args, **kwargs):
        if not self.style_loop.has_ever_finished:
            self.style_loop()
        else:
            self.finish()

class CheckSpinCompletion(Task):
    def on_first_run(self, n):
        self.roll = shm.kalman.roll.get()
        self.cumul = 0

    def on_run(self, n):
        new_roll = shm.kalman.roll.get()
        self.cumul += heading_sub_degrees(new_roll, self.roll)
        self.roll = new_roll

        if abs(self.cumul) > 360 * n:
            self.finish()

class StyleSuperSpin(Style):
    def style_on_first_run(self, clockwise=True, steps=5, spins=2, *args, **kwargs):
        delta_roll = [-1, 1][clockwise] * 45
        subspins = [MasterConcurrent(CheckSpinCompletion(2),
                      RelativeToCurrentRoll(delta_roll))]

        self.movement = Sequential(
            # MoveY(0.3, deadband=0.1),
            MoveX(2, deadband=0.2),
            Concurrent(
                MoveX(3, deadband=0.2),
                Sequential(subtasks=subspins),
            )
        )

    def style_on_run(self, *args, **kwargs):
        if not self.movement.has_ever_finished:
            self.movement()
        else:
            self.finish()

DEFAULT_DEADBAND = 0.03

class AlignHeading(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.pid = PIDLoop(
            input_value=lambda: self.x_ratio(vision),
            output_function=RelativeToCurrentHeading(),
            target=0.5,
            deadband=DEFAULT_DEADBAND/3,
            p=40,
            d=20,
            negate=True,
        )

    def on_run(self, vision, *args, **kwargs):
        # Try to center on entire gate first
        if vision.bottom is not None or \
                (vision.left is not None  and vision.right is not None):
            self.pid()

        elif vision.left is not None: # Otherwise try to find other vertical bar
            RelativeToCurrentHeading(1)()
        elif vision.right is not None:
            RelativeToCurrentHeading(-1)()

        if self.pid.finished:
            self.finish()

    def x_ratio(self, vision):
        """
        Precondition: we can see the bottom bar, or the left and right bars
        """
        if vision.bottom is not None:
            avg_x = (vision.bottom.x1 + vision.bottom.x2) / 2
            return avg_x / vision.cam_width
        else: # We see left and right bars
            left_x_avg = (vision.left.x1 + vision.left.x2) / 2
            right_x_avg = (vision.right.x1 + vision.right.x2) / 2
            avg = (left_x_avg + right_x_avg) / 2
            return avg / vision.cam_width

class AlignDepth(Task):
    min_depth = 1

    def on_first_run(self, vision, *args, **kwargs):
        self.pid = PIDLoop(
            input_value=lambda: self.y_ratio(vision),
            output_function=RelativeToCurrentDepth(),
            target=0.75,
            deadband=DEFAULT_DEADBAND,
            p=2,
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
        self.success = True
        self.pid = PIDLoop(
            input_value=lambda: bbar_width_ratio(vision),
            output_function=VelocityX(),
            target=0.55,
            deadband=DEFAULT_DEADBAND,
            p=2,
        )

        # Maximum ratio of camera width bottom bar can be away from camera
        # edge
        self.EDGE_PROXIMITY = 0.05

        # Speed to back away from the bottom bar at when too close
        self.BACKUP_SPEED = 0.15

        # Speed to approach the gate at when not fully visible
        self.APPROACH_SPEED = 1

        self.STATE_INFO = {
            'lost': 'Lost gate',
            'found': 'Gate found, aligning',
            'too close': 'Too close to gate, backing up',
        }
        self.state = 'lost'
        self.old_state = ''

    def on_run(self, vision, *args, **kwargs):
        if vision.bottom is not None:
            # If the bottom bar touches the edge of the camera image, we're too
            # close and need to back up a bit. Otherwise, try to make it fill a
            # portion of the camera's width.
            # left_x, right_x = vision.bottom.x1, vision.bottom.x2
            # if left_x > right_x:
                # left_x, right_x = right_x, left_x
            # left_prox = left_x / vision.cam_width
            # right_prox = 1 - (right_x / vision.cam_width)

            # top_y, bottom_y = vision.bottom.y1, vision.bottom.y2
            # if top_y > bottom_y:
                # top_y, bottom_y = bottom_y, top_y
            # top_prox = top_y / vision.cam_height
            # bottom_prox = 1 - (bottom_y / vision.cam_height)

            # if left_prox < self.EDGE_PROXIMITY or right_prox < self.EDGE_PROXIMITY or \
                    # top_prox < self.EDGE_PROXIMITY or bottom_prox < self.EDGE_PROXIMITY:
                # VelocityX(-self.BACKUP_SPEED)()
                # self.state = 'too close'

            # else:
                # self.fast_approach(vision)

        # else:
            self.fast_approach(vision)

        if self.state != self.old_state:
            self.logi(self.STATE_INFO[self.state])
            self.old_state = self.state

        if self.pid.finished:
            self.finish()

    def fast_approach(self, vision):
        """
        If we're too far from the gate, approach fast. Otherwise, carefully
        align to a fixed distance from the gate.
        """
        if (vision.bottom is not None or \
                (vision.left is not None and vision.right is not None)) and \
                bbar_width_ratio(vision) >= self.min_bbar_width:
            self.pid()
            self.state = 'found'
        else:
            self.success = False
            self.state = 'lost'

# class AlignSway(Task):
    # def on_first_run(self, vision, *args, **kwargs):
        # self.pid = PIDLoop(
            # input_value=lambda: self.height_diff_ratio(vision),
            # output_function=VelocityY(),
            # target=0,
            # deadband=0.01,
            # p=20,
            # d=10,
        # )

    # def on_run(self, vision, *args, **kwargs):
        # # If we see both bars, try to sway to minimize their height difference
        # if all(b is not None for b in [vision.left, vision.right, vision.bottom]):
            # self.pid()

        # if self.pid.finished:
            # self.finish()

    # def height_diff_ratio(self, vision):
        # left_height = abs(vision.left.y1 - vision.left.y2)
        # right_height = abs(vision.right.y1 - vision.right.y2)
        # return (right_height - left_height) / vision.cam_height

class AlignSway(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.pid = PIDLoop(
            input_value=lambda: self.bbar_angle(vision),
            output_function=VelocityY(),
            target=0,
            deadband=3,
            p=0.1,
            d=0.05,
        )

    def on_run(self, vision, *args, **kwargs):
        # If we see both bars, try to sway to minimize their height difference
        if vision.bottom is not None:
            self.pid()

        if self.pid.finished:
            self.finish()

    def bbar_angle(self, vision):
        left_y, right_y = vision.bottom.y1, vision.bottom.y2
        if vision.bottom.x1 > vision.bottom.x2:
            left_y, right_y = right_y, left_y
        width = abs(vision.bottom.x2 - vision.bottom.x1)
        bar_angle = math.degrees(math.atan2(right_y - left_y, width))
        roll_adjusted_angle = (bar_angle + shm.kalman.roll.get()) % 360
        if roll_adjusted_angle > 180:
            roll_adjusted_angle -= 360
        return roll_adjusted_angle

class AlignChannel(Task):
    def on_first_run(self, vision, *args, **kwargs):
        self.success = False
        self.align_fore = AlignFore(vision)
        self.pids_task = Concurrent(
            AlignHeading(vision),
            AlignDepth(vision),
            self.align_fore,
            AlignSway(vision),
            finite=False,
        )
        self.logi('Starting')

    def on_run(self, vision, *args, **kwargs):
        if hasattr(self.align_fore, 'success') and not self.align_fore.success:
            self.loge('Lost gate, aborting align')
            self.finish()
            return

        if not self.pids_task.finished:
            self.pids_task()
        else:
            self.success = True
            self.finish()

    def on_finish(self, *args, **kwargs):
        Zero()()

class OptimalMission(Task):
    def on_first_run(self, mode=None, main_task_func=None, *args, **kwargs):
        self.main_task = main_task_func()
        self.has_made_progress = False
        # TODO @AlexO Update when we've made progress! (seen gate?)

    def on_run(self, mode=None, main_task=None, *args, **kwargs):
        if not self.main_task.has_ever_finished:
            self.main_task()
        else:
            self.finish()

    def desiredModules(self):
        return [shm.vision_modules.Navigate]

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
full = lambda: OptimalMission(main_task_func=lambda: VisionTask(Navigate))

basicfull = lambda: Vision(Sequential(AlignChannel(), StyleBasic()))
basic = lambda: Vision(StyleBasic())
pitched = lambda: Vision(StylePitched())
sideways = lambda: Vision(StyleSideways())
upside_down = lambda: Vision(StyleUpsideDown())
loop = lambda: Vision(StyleLoop())
superspin = lambda: StyleSuperSpin()

class Flip180(Task):
    def on_first_run(self, *args, **kwargs):
        #heading = Heading((kalman.heading.get() + 180) % 360, error=1)
        self.flip = Sequential(Pitch(0, error=1), Roll(0, error=1), Timer(1.5), Heading(lambda: kalman.heading.get() + 180, error=1), Timer(1))
    def on_run(self, *args, **kwargs):
        self.flip()
        if self.flip.has_ever_finished:
            self.finish()

unspin = lambda: StyleSuperSpin(clockwise=False)

est_all = lambda: Sequential(basic(), Heading(90, error=1), pitched(), Heading(270, error=1), sideways(), Heading(90, error=1), upside_down(), Heading(270, error=1), Depth(2.1, error=.1), loop())
