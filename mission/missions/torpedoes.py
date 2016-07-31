from framework.targeting import ForwardTarget, HeadingTarget, PIDLoop
from framework.helpers import within_deadband, get_forward_camera_size, ConsistencyCheck
from framework.task import Task
from framework.movement import VelocityX, VelocityY, RelativeToCurrentHeading, PositionN, PositionE, Depth, RelativeToInitialDepth, RelativeToInitialHeading, Pitch, Heading
from framework.primitive import Zero
from framework.combinators import Sequential, MasterConcurrent, Concurrent
from framework.timing import Timer, Timed
from framework.position import MoveX, MoveY, GoToPosition, MoveXY
from framework.actuators import FireActuator, FireActuators, SetActuators

from mission.missions.ozer_common import Timeout

import shm
from vision.modules.torpedoes import CutoutSize, CutoutName, TorpedoesMode
import aslam

from enum import Enum
from collections import namedtuple
from time import sleep
import numpy as np

direction = ["S", "W"]
USE_ACTUATORS = True
COVER_INITALLY_PRESENT = True
known_letters = [
    None, None,
    None, None,
]

heading_target_board = HeadingTarget()

def get_board_center(results):
    return results.board_center_x, results.board_center_y

def update_camera(results):
    camera_dims = get_forward_camera_size()
    if camera_dims is not None:
        Torpedoes.camera_width = camera_dims[0]
        Torpedoes.camera_height = camera_dims[1]
        Torpedoes.camera_center = (Torpedoes.camera_width/2, Torpedoes.camera_height/2)
        Torpedoes.camera_valid = True

class Torpedoes(Task):
    pass

'''
Moves the sub to the position directly in front of the torpedoes board.
Must have successfully transitioned out of the align_and_characterize state for
this task to work.
'''
class GoToBoardFront(Task):
    def on_first_run(self):
        self.logi("Started GoToBoardFront")
        self.task = GoToPosition(Torpedoes.board_front_north, Torpedoes.board_front_east,
                                 heading=Torpedoes.board_front_heading,
                                 depth=Torpedoes.board_front_depth)
        shm.torpedoes_settings.mode.set(TorpedoesMode.board_subsequent.value)

    def on_run(self):
        if self.task.finished:
            self.logi("Finished GoToBoardFront")
            self.finish()
        else:
            self.task()

class LocateAndApproach(Task):
    def on_first_run(self):
        self.task = Sequential(LocateBoard(), ApproachBoard())

    def on_run(self):
        if Torpedoes.failed or self.task.finished:
            self.finish()
        else:
            self.task()

'''
Rotates the sub until it sees the torpedoes board
'''
BOARD_DEPTH = 2.3
class LocateBoard(Task):
    def on_first_run(self):
        self.inital_heading = shm.kalman.heading.get()
        self.num_headings = 8
        self.heading_inc = 360 / self.num_headings
        shm.torpedoes_settings.mode.set(TorpedoesMode.board_inital.value)

        headings = []
        for i in range(0, self.num_headings):
            headings.append(Heading(self.inital_heading + (self.heading_inc * i) % 360))
            headings.append(Timer(2))
        spin = Sequential(*headings)

        self.spin_search = Sequential(Depth(BOARD_DEPTH), spin)
        self.logi("Started LocateBoard")
        if not USE_ACTUATORS:
            self.logi("Actuators are disabled!!")

        self.check = ConsistencyCheck(3, 5)
        self.board_watcher = shm.watchers.watcher()
        self.board_watcher.watch(shm.torpedoes_results)
        self.board_visible = False
        self.found_board = False

        # aslam_target = aslam.SimpleTarget(obj = aslam.world.torpedoes, relativePosition = np.array([-1, 0, 0]))

    def check_board_visible(self, vision_results):
        if self.board_watcher.has_changed():
            self.board_visible = self.check.check(vision_results.board_prob > 0.7)

    def on_run(self):
        # if not self.goto_board.finished:
        #    self.goto_board()  

        vision_results = shm.torpedoes_results.get()
        update_camera(vision_results)

        self.check_board_visible(vision_results)
        if self.board_visible:
            if heading_target_board.finished:
                self.found_board = True
                self.finish()
                self.logi("Finished LocateBoard")
            else:
                self.logi("Targeting board")

                board_coords = get_board_center(vision_results)
                heading_target_board.run(board_coords, px=0.02, py=0.001, target=Torpedoes.camera_center, deadband=(60, 60),
                                         valid=Torpedoes.camera_valid)
        elif self.spin_search.finished:
            Torpedoes.failed = True
            self.finish()
        else:
            self.spin_search()

'''
Moves forward (or backwards) until the board is a certain distance from the sub
Perhaps not necessary because the bins are so close to torpedoes
'''
class ApproachBoard(Task):
    def on_first_run(self):
        self.logi("Started ApproachBoard")

        self.target_board = ForwardTarget()
        self.zero_vel_x = VelocityX(0.0)
        self.zero_vel_y = VelocityY(0.0)

        self.pid_loop_x = PIDLoop(output_function=VelocityX())
        shm.torpedoes_settings.mode.set(TorpedoesMode.board_inital.value)

    def on_run(self):
        vision_results = shm.torpedoes_results.get()
        update_camera(vision_results)

        if self.pid_loop_x.finished:
            # Zero speed so the sub doesn't keep moving after finish
            self.zero_vel_x()
            self.zero_vel_y()

            self.logi("Finished ApproachBoard")
            self.finish()
        else:
            board_coords = get_board_center(vision_results)
            # TODO If we stop seeing the board do we still center? @johnm
            self.target_board(board_coords, target=Torpedoes.camera_center, valid=Torpedoes.camera_valid)
            self.pid_loop_x(input_value=vision_results.board_height, p=.001, i=0, d=0, target=Torpedoes.camera_height*0.7, deadband=100)

'''
Aligns the sub to face normal to the board
'''
class AlignToBoard(Task):
    def on_first_run(self):
        self.logi("Started AlignToBoard")
        self.pid_loop_y = PIDLoop(output_function=VelocityY())
        self.zero_vel_y = VelocityY(0.0)
        shm.torpedoes_settings.mode.set(TorpedoesMode.board_inital.value)
        self.pid_loop_x = PIDLoop(output_function=VelocityX())

    def on_run(self):
        results = shm.torpedoes_results.get()
        update_camera(results)

        # By keeping the board centered in the camera (via heading adjusment)
        # and swaying until skew is zero, we will align normal to the board
        board_coords = get_board_center(results)
        heading_target_board.run(board_coords, target=Torpedoes.camera_center, px=0.015, valid=Torpedoes.camera_valid)
        if Torpedoes.camera_valid:
          self.pid_loop_y(input_value=results.board_skew, p=0.01, i=0, target=0, deadband=1.0)
          self.pid_loop_x(input_value=results.board_height, p=.001, i=0, d=0, target=Torpedoes.camera_height*0.7, deadband=10)

        if self.pid_loop_y.finished and results.skew_valid:
            self.zero_vel_y()
            self.logi("Finished AlignToBoard, skew: {}, valid: {}".format(results.board_skew, results.skew_valid))
            self.finish()

class RemoveCover(Task):
    def on_first_run(self):
        self.task = Sequential(TargetCutout(), SlideCover())
        Torpedoes.target_cutout_failed = False

    def on_run(self):
        if self.task.finished:
            self.finish()
        else:
            self.task()

        if Torpedoes.target_cutout_failed:
            self.finish()

class ShootCutout(Task):
    def on_first_run(self):
        self.task = Sequential(TargetCutout(), FireTorpedo())
        Torpedoes.target_cutout_failed = False

    def on_run(self):
        if self.task.finished:
            self.finish()
        else:
            self.task()

        if Torpedoes.target_cutout_failed:
            self.logi("Wrong letter, exiting ShootCutout!")
            self.finish()
'''
Attempts to remove the cover from the covered cutout
'''
class SlideCover(Task):
    def on_first_run(self):
        Torpedoes.target = Torpedoes.cutouts[0]
        if (Torpedoes.target.name == CutoutName.top_left or
            Torpedoes.target.name == CutoutName.bottom_left):
            align_for_swipe = Concurrent(
                Timeout(RelativeToInitialDepth(-0.10330, error=0.005), 5.0),
                Sequential(
                    Timeout(MoveY(0.3, deadband=0.03), 5.0),
                    Timeout(MoveX(0.26), 5.0)
                )
            )
            self.remove_cover = Sequential(
                align_for_swipe, 
                Timeout(MoveY(-0.7, deadband=0.05), 5.0), 
                Timeout(MoveX(-0.2, deadband=0.05), 2.0),
                Timeout(Concurrent(
                    RelativeToInitialDepth(0.1), 
                    RelativeToInitialHeading(20, error=5)
                ), 3.0),
                Timeout(MoveX(-0.2, deadband=0.05), 2.0)
            )
        else:
            align_for_swipe = Concurrent(
                Timeout(RelativeToInitialDepth(-0.10330, error=0.005), 5.0),
                Sequential(
                    Timeout(MoveY(-0.1, deadband=0.03), 2.0),
                    Timeout(MoveX(0.26), 2.0)
                )
            )
            self.remove_cover = Sequential(
                align_for_swipe, 
                Timeout(MoveY(0.7, deadband=0.05), 5.0), 
                Timeout(MoveX(-0.2, deadband=0.05), 2.0),
                Timeout(Concurrent(
                    RelativeToInitialDepth(0.1), 
                    RelativeToInitialHeading(-20, error=5)
                ), 3.0),
                Timeout(MoveX(-0.2, deadband=0.05), 2.0)
            )

        self.logi("Started RemoveCover")
        # north = shm.kalman.north.get()
        # east = shm.kalman.east.get()
        # self.logi("north: {}, east: {}".format(north, east))
        depth = shm.kalman.depth.get()
        shm.torpedoes_settings.cover_initally_present.set(False)
        self.logi("depth: {}".format(depth))

    def on_run(self):
        if self.remove_cover.finished:
            self.logi("Finished RemoveCover")
            self.finish()
        else:
            # pass
            self.remove_cover()

'''
Select the covered cutout as the target
'''
class SelectCoveredCutout(Task):
    def on_run(self):
        global target
        # For now, just select the top left cutout (All cutouts have covers currently!)
        target = shm.torpedoes_cutout_top_left
        self.finish()

'''
Targets the selected cutout and shoots a torpedo thought it
'''
class TargetCutout(Task):
    def on_first_run(self):
        self.forward_target = ForwardTarget()
        self.zero_vel_y = VelocityY(0.0)

        def vel_x_bound(vel):
            VelocityX(min(vel, 0.3))()

        self.pid_loop_x = PIDLoop(output_function=vel_x_bound)
        self.zero_vel_x = VelocityX(0.0)

        self.targeted_once = False
        self.logi("Started TargetCutout")
        self.lost_target = False
        self.prev_lost_target = False
        self.lost_target_timer = None
        Torpedoes.target_cutout_failed = False
        Torpedoes.has_made_progress = True

    def on_run(self):
        update_camera(shm.torpedoes_results.get())
        if not Torpedoes.camera_valid:
            return

        rel_target_height = Torpedoes.target.results.height / Torpedoes.camera_height
        target_letter = Torpedoes.target.results.letter.decode('utf-8')
        if False: # rel_target_height > 0.23 and rel_target_height < 0.4:
            Torpedoes.target.known_letter = target_letter
            if Torpedoes.target.inital_letter != target_letter:
                self.logi("Wrong letter detected! Inital letter was %s but letter is %s now" % (Torpedoes.target.inital_letter, target_letter))

                if Torpedoes.target.results.covered:
                    self.logi("Continuing with cover removal...")
                else:
                    self.logi("Backing up to try another cutout!")
                    Torpedoes.target_cutout_failed = True
                    Torpedoes.target.attempted = False

                    self.zero_vel_y()
                    self.zero_vel_x()

                    self.finish()
                    return


        if self.forward_target.finished:
            self.targeted_once = True

        if self.pid_loop_x.finished and self.forward_target.finished:
            self.zero_vel_y()
            self.zero_vel_x()

            self.logi("Finished TargetCutout")
            self.finish()
        elif Torpedoes.target.results.visible:
            if self.lost_target:
                self.logi("Found Target!!")
                self.lost_target = False

            # actual_height = 0.1778
            # size = target.size.get()
            # if size == int(CutoutSize.small):
            #     actual_height = 0.1778
            # elif size == int(CutoutSize.large):
            #     actual_height = 0.3048
            # else:
            #     self.loge("Invalid size {}!!".format(size))

            # px_to_m = (actual_height / target.height.get())
            # coords = (Torpedoes.target.x.get() * px_to_m, Torpedoes.target.y.get() * px_to_m)
            coords = (Torpedoes.target.results.x, Torpedoes.target.results.y)

            if self.targeted_once:
                self.pid_loop_x(input_value=Torpedoes.target.results.width, p=.0005, i=0, d=0.0, target=Torpedoes.camera_height*0.55, deadband=10)
            # self.forward_target(coords, target=Torpedoes.camera_center, py=5, px=10, deadband=(0.05,0.05))
            self.forward_target(coords, target=Torpedoes.camera_center, py=0.0005, px=0.001, deadband=(120,120))
        else:
            if not self.lost_target:
                self.logi("Lost Target!!")
                self.lost_target = True
                self.lost_target_timer = Timer(2.0)

            self.zero_vel_y()
            VelocityX(-0.1)()
            self.lost_target_timer()

            if self.lost_target_timer.finished:
                Torpedoes.target_cutout_failed = True
                self.finish()

BOARD_WIDTH_METERS = 1.2
DIST_TO_BOARD = 0.7
CUTOUT_OFFSET_Y = 0.3 * BOARD_WIDTH_METERS
CUTOUT_OFFSET_Z = 0.3 * BOARD_WIDTH_METERS
class TargetCutoutFallback(Task):
    def on_first_run(self):
        self.task = Sequential(
            Concurrent(
                MoveXY((DIST_TO_BOARD, CUTOUT_OFFSET_Y), deadband=0.01),
                Depth(Torpedoes.board_front_depth - CUTOUT_OFFSET_Z, deadband=0.01)
            ),
            FireTorpedo(),
            Concurrent(
                MoveY(-CUTOUT_OFFSET_Y * 2, deadband=0.01),
                Depth(Torpedoes.board_front_depth + CUTOUT_OFFSET_Z, deadband=0.01)
            ),
            FireTorpedo()
        )

    def on_run(self):
        if self.task.finished:
            self.finish()
        else:
            self.task()

'''
Moves from targeting one cutout to another
'''
class TransitionToCutout(Task):
    def on_run(self):
        # Not implemented yet!
        pass

'''
Requests boad characterization from vision and continues once it has been done
'''
class CharacterizeBoard(Task):
    def on_first_run(self, target):
        self.logi("Started CharacterizeBoard, waiting for vision to characterize...")
        shm.torpedoes_settings.target.set(target.name.value)
        shm.torpedoes_settings.mode.set(1)

    def on_run(self):
        if shm.torpedoes_settings.mode.get() == 2:
            self.logi("Finished CharacterizeBoard")
            self.finish()
'''
Fires a torpedo through the cutout!
'''
class FireTorpedo(Task):
    def on_first_run(self):
        self.forward_target = ForwardTarget()
        self.pid_loop_x = PIDLoop(output_function=VelocityX())

        if USE_ACTUATORS:
            if Torpedoes.num_torpedoes == 1:
                self.logi("Firing bottom torpedo!")
                self.task = Sequential(RelativeToInitialDepth(0.05, error=0.01), FireActuator("torpedo_bottom", 0.1))
            else:
                self.logi("Firing top torpedo!")
                self.task = Sequential(RelativeToInitialDepth(0.07, error=0.01), FireActuator("torpedo_top", 0.1))
        else:
            self.logi("Actuators disabled, set USE_ACTUATORS to True to enable")
            self.task = Zero()

        Torpedoes.num_torpedoes -= 1
        Torpedoes.current_letter = None

        # Prevent drift
        shm.navigation_settings.position_controls.set(True)

        self.logi("Started FireTorpedo")

    def on_run(self):
        if self.task.finished:
            shm.navigation_settings.position_controls.set(False)
            self.finish()
        else:
            self.task()

class Cutout:
    def __init__(self, *, group, attempted, name):
        self.group = group
        self.attempted = attempted
        self.name = name
        self.inital_letter = None
        self.known_letter = None

    def __str__(self):
        return "Group: " + str(self.group) + ", Attempted: " + str(self.attempted) + ", Covered: " + str(self.results.covered) + ", Size: " + str(CutoutSize(self.results.size)) + ", Letter: " + self.results.letter.decode('utf-8')

    def update(self):
        self.results = self.group.get()

class Torpedoes(Task):
    def __init__(self, cover_present="cover", *args, **kwargs):
        super(Torpedoes, self).__init__(*args, **kwargs)
        global direction
        global known_letters

        # Set the direction variable in shm for vision
        dir_str = "".join(direction)
        shm.torpedoes_settings.direction.set(dir_str)

        # Set the known_letters for each cutout from the global var
        known_letters_str = ""
        for idx, letter in enumerate(known_letters):
            self.cutouts[idx].known_letter = letter
            
        shm.torpedoes_settings.cover_initally_present.set(COVER_INITALLY_PRESENT)
        
        update_camera(shm.torpedoes_results.get())
    
        self.has_made_progress = True

    def desiredModules(self):
        return [shm.vision_modules.Torpedoes]

    class State(Enum):
        locate_and_approach_board = 1
        align_and_characterize = 2
        remove_cover = 3
        target_uncovered_cutout = 4
        goto_board_front = 6
        end = 7
        target_cutout_fallback = 8

    board_front_north = 0
    board_front_east = 0
    board_front_heading = 0
    board_front_depth = 0
    state = State.locate_and_approach_board
    task = LocateAndApproach()
    current_letter = None
    camera_valid = False
    camera_width = 0
    camera_height = 0
    camera_center = (0,0)
    camera_dims_valid = False

    num_tries = {
        'remove_cover': 0,
        'top_left_target': 0,
        'top_right_target': 0,
        'bottom_left_target': 0,
        'bottom_right_target': 0
    }

    # The order of these is important! It is depended on implictly by the known_letters
    # variable (probably a bad idea)
    cutouts = [Cutout(group=shm.torpedoes_cutout_top_left, attempted=False,
                      name=CutoutName.top_left),
              Cutout(group=shm.torpedoes_cutout_top_right, attempted=False,
                     name=CutoutName.top_right),
              Cutout(group=shm.torpedoes_cutout_bottom_left, attempted=False,
                     name=CutoutName.bottom_left),
              Cutout(group=shm.torpedoes_cutout_bottom_right, attempted=False,
                     name=CutoutName.bottom_right)]


    failed = False
    done = False
    target = None
    lost_target = False
    num_torpedoes = 2

    def on_first_run(self):
        Torpedoes.num_torpedoes = 2
        Torpedoes.lost_target = False
        Torpedoes.current_letter = None
        Torpedoes.target_cutout_failed = False
        Torpedoes.has_made_progress = True
        
        # Master timeout after 4 minutes
        self.master_timeout = Timer(4 * 60)

    def end_mission(self):
        self.done = True
        self.finish()

    def on_run(self):
        self.master_timeout()

        if self.master_timeout.finished:
            SetActuators(off_triggers=["bottom_torpedo", "top_torpedo"])
            self.logi("Master timeout for torpedoes reached! Took 4 minutes")
            self.finish()
            return

        update_camera(shm.torpedoes_results.get())
        self.has_made_progress = Torpedoes.has_made_progress

        known_letters_str = ""
        for cutout in self.cutouts:
            # Populate the results variable of the cutout
            cutout.update()

            # Update the known_letters variable in shm
            letter = cutout.known_letter
            if letter == "N" or letter == "E" or letter == "S" or letter == "W":
                known_letters_str += letter
            elif letter == None:
                known_letters_str += "?"
            else:
                self.loge("Unknown letter found in known_letters!")
        shm.torpedoes_settings.known_letters.set(known_letters_str)

        # State transition table
        if self.task.finished:
            self.logi("Transitioning State!")
            if self.state == self.State.locate_and_approach_board:
                if (Torpedoes.failed):
                    self.logi("Couldn't find torpedoes board! Ending torpedoes mission")
                    self.end_mission()
                else:
                    self.logi("Finsihed Locate and Approach!")
                    self.state = self.State.align_and_characterize
                    self.task = AlignToBoard()
            elif self.state == self.State.align_and_characterize or self.state == self.State.goto_board_front:
                kalman = shm.kalman.get()
                Torpedoes.board_front_north = kalman.north
                Torpedoes.board_front_east = kalman.east
                Torpedoes.board_front_heading = kalman.heading
                Torpedoes.board_front_depth = kalman.depth

                covered_cutout = self.get_covered_cutout()
                if covered_cutout == None or self.num_tries['remove_cover'] >= 2:
                    self.state = self.State.target_uncovered_cutout

                    if Torpedoes.lost_target:
                        Torpedoes.lost_target = False
                    else:
                        Torpedoes.target = self.get_uncovered_cutout()

                    # Can't find a target! Exit (TODO: Re-align and try again?)
                    if self.target == None:
                        self.logi("Can't find a target cutout! Falling back to positional targeting")
                        self.state = self.State.target_cutout_fallback
                        self.task = TargetCutoutFallback()
                    else:                    
                        self.task = ShootCutout()

                else:
                    Torpedoes.target = covered_cutout
                    self.state = self.State.remove_cover
                    self.task = RemoveCover()

            elif self.state == self.State.target_cutout_fallback:
                self.state = self.State.end
                self.task = Sequential(GoToBoardFront(), Zero())
            elif self.state == self.State.remove_cover:
                if not Torpedoes.target_cutout_failed:
                    self.num_tries['remove_cover'] += 1

                self.state = self.State.goto_board_front
                self.task = GoToBoardFront()

            elif self.state == self.State.target_uncovered_cutout:
                if self.num_torpedoes > 0:
                    self.state = self.State.goto_board_front
                    self.task = GoToBoardFront()
                else:
                    self.state = self.State.end
                    self.task = Sequential(GoToBoardFront(), Zero())
            elif self.state == self.State.goto_board_front:
                self.state = self.State.align_and_characterize
                self.task = AlignToBoard()
            elif self.state == self.State.end:
                self.end_mission()

        self.task()

    def get_covered_cutout(self):
        for cutout in self.cutouts:
            if cutout.results.covered:
                shm.torpedoes_settings.target.set(cutout.name.value)
                shm.torpedoes_settings.mode.set(TorpedoesMode.track_target.value)

                cutout.inital_letter = cutout.results.letter.decode('utf-8')
                self.logi("Letter from results is %s" % cutout.results.letter.decode('utf-8'))
                self.logi("Inital letter for %s cutout is %s" % (cutout.name, cutout.inital_letter))
                return cutout
        return None

    def list_to_str(self, list):
        string = ""
        for e in list:
           string += str(e) + " "

        return string

    '''
    Decides which cutout should be targeted, the preference order is:
        1. Uncovered small cutout, with specified letter
        2. Big cutout, with specified letter
        3. Big cutout
        3. Small cutout
    '''

    def get_uncovered_cutout(self):
        global direction

        target = None

        self.logi("Direction is currently %s" % direction)

        if Torpedoes.current_letter == None:
            Torpedoes.current_letter = direction.pop(0)

        cutouts = [c for c in self.cutouts if not c.attempted and not c.results.covered and c.results.visible]

        small_cutouts = [c for c in cutouts if c.results.size == int(CutoutSize.small)]
        large_cutouts = [c for c in cutouts if c.results.size == int(CutoutSize.large)]

        correct_letter_small = [c for c in small_cutouts if c.results.letter.decode('utf-8') == self.current_letter]
        correct_letter_large = [c for c in large_cutouts if c.results.letter.decode('utf-8') == self.current_letter]

        self.logi("All Cutouts: %s" % self.list_to_str(self.cutouts))
        self.logi("Considered Cutouts: %s" % self.list_to_str(cutouts))
        self.logi("Small Cutouts: %s" % self.list_to_str(small_cutouts))
        self.logi("Large Cutouts: %s" % self.list_to_str(large_cutouts))
        self.logi("Correct Letter Small: %s" % self.list_to_str(correct_letter_small))
        self.logi("Correct Letter Large: %s" % self.list_to_str(correct_letter_large))

        if len(correct_letter_small) > 0:
            target = correct_letter_small[0]
            self.logi("Selected small cutout with correct letter: %s" % target)
        elif len(correct_letter_large) > 0:
            target = correct_letter_large[0]
            self.logi("Selected large cutout with correct letter: %s" % target)
        elif len(small_cutouts) > 0:
            target = small_cutouts[0]
            self.logi("Selected small cutout with unknown letter: %s" % target)
        elif len(large_cutouts) > 0:
            target = large_cutouts[0]
            self.logi("Selected large cutout with unknown letter: %s" % target)
        else:
            self.loge("Can't find a suitable target cutout")
            return None

        target.attempted = True
        shm.torpedoes_settings.target.set(target.name.value)
        shm.torpedoes_settings.mode.set(TorpedoesMode.track_target.value)
        target.inital_letter = target.results.letter.decode('utf-8')
        self.logi("Initial letter for %s cutout is %s" % (target.name, target.inital_letter))
        
        return target

full = Torpedoes()
fallback = TargetCutoutFallback()
