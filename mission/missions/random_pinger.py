from mission.framework.search import SpiralSearch
from mission.framework.helpers import ConsistencyCheck
from mission.missions.bins import BinsTask as Bins
from mission.missions.recovery import OptimalRecovery as Recovery
from mission.framework.task import Task
from mission.missions.torpedoes import LocateBoard, Torpedoes
from mission.missions.hydrophones import FindPinger
from mission.framework.combinators import Sequential
from mission.framework.primitive import Log
from mission.framework.timing import Timer

import shm

NONE = 0
TORPEDOES = 1
RECOVERY = 2

class RandomPinger(Task):
    def desiredModules(self):
        if hasattr(self, "after_pinger") and hasattr(self.after_pinger, "selected_mission"):
            selected_mission = self.after_pinger.selected_mission
            if selected_mission is None or selected_mission == "torpedoes":
                return [shm.vision_modules.Torpedoes]
            elif selected_mission == "recovery":
                return [shm.vision_modules.Recovery]
            elif selected_mission == "bins":
                return [shm.vision_modules.Bins]
        else:
            return [shm.vision_modules.Recovery]

    def on_first_run(self):
        random_task = shm.mission_state.random_task.get()

        find_pinger = FindPinger()
        if random_task == TORPEDOES:
          self.logi("Making FindPinger with safe elevations because we are going from Bins to Recovery!")
          find_pinger = FindPinger(safe_elevations=True)
        elif random_task == RECOVERY:
          self.logi("Making FindPinger with look_for_recovery=False because we are going from Recovery to Bins!")
          find_pinger = FindPinger(look_for_recovery=False)

        self.after_pinger = AfterPinger(find_pinger)
        self.task = Sequential(find_pinger, self.after_pinger)
        self.has_made_progress = True

    def on_run(self):
        # Always report pinger tracking has made progress
        # self.has_made_progress = self.after_pinger.selected_task.has_made_progress if self.after_pinger.selected_task is not None else True
        if self.task.finished:
            self.finish()
        else:
            self.task()

class AfterPinger(Task):
    task = None
    selected_task = None
    selected_mission = None

    def on_first_run(self, find_pinger=None):
        self.torpedoes = Torpedoes()
        self.recovery = Recovery()
        self.bins = Bins()

        self.random_task = shm.mission_state.random_task.get()

        # We need to identify where we are.
        if self.random_task == NONE:
            self.search = LocateBoard()

        self.selected_mission = None
        self.selected_task = None
        self.has_made_progress = True

    def at_recovery(self):
        if not self.recovery.finished:
            self.recovery()
            self.selected_mission = "recovery"
            self.selected_task = self.recovery
        else:
            self.finish()

    def at_bins_torpedoes(self):
        if not self.torpedoes.finished:
            self.selected_mission = "torpedoes"
            self.selected_task = self.torpedoes
            self.torpedoes()
        elif not self.bins.finished:
            self.selected_mission = "bins"
            self.selected_task = self.bins
            self.bins()
        else:
            self.finish()

    def on_run(self, find_pinger=None):
        # self.has_made_progress = self.selected_task.has_made_progress if self.selected_task is not None else False

        if self.random_task == NONE and self.search.finished:
            if self.search.found_board:
                shm.mission_state.random_task.set(TORPEDOES)
                self.at_bins_torpedoes()
            else:
                shm.mission_state.random_task.set(RECOVERY)
                self.at_recovery()

        elif self.random_task == TORPEDOES:
            shm.mission_state.random_task.set(RECOVERY)
            self.at_recovery()

        elif find_pinger is not None and find_pinger.found_recovery:
            shm.mission_state.random_task.set(RECOVERY)
            self.at_recovery()

        elif self.random_task == RECOVERY:
            shm.mission_state.random_task.set(TORPEDOES)
            self.at_bins_torpedoes()

        else:
            self.search()


class AfterPingerBinsRecovery(Task):
    def recovery_validator(self):
        results = shm.recovery_vision.get()

        stacks_visible = False
        stack_visible_vars = [results.stack_1_visible, results.stack_2_visible,
                              results.stack_3_visible, results.stack_4_visible]
        num_visible_stacks = 0
        for stack_visible in stack_visible_vars:
            if stack_visible:
                num_visible_stacks += 1
        
        if num_visible_stacks >= 2:
            stacks_visible =  True 
        
        return stacks_visible or results.green_mark_visible or \
               results.red_mark_visible or results.table_visible
        
    def bins_validator(self):
        return shm.bin_cover.probability.get() > 0.0 or \
               shm.bin_yellow_1.probability.get() > 0.0 or \
               shm.bin_yellow_2.probability.get() > 0.0
         
    def check_bins_visible(self):
        print("check bins")
        if self.bins_watcher.has_changed():
            bins_val = self.bins_validator()
            print("bins validator returned", bins_val)
            self.bins_visible = self.bins_check(bins_val)
        
    def check_recovery_visible(self):
        if self.recovery_watcher.has_changed():
            self.recovery_visible = self.recovery_check(self.recovery_validator())
        
    def on_first_run(self):
        self.search = SpiralSearch(optimize_heading=True, min_spin_radius=2.0)
        self.recovery_check = ConsistencyCheck(5, 5)
        self.bins_check = ConsistencyCheck(5, 5)

        self.bins_watcher = shm.watchers.watcher()
        self.bins_watcher.watch(shm.bin_cover)
        self.bins_watcher.watch(shm.bin_yellow_1)
        self.bins_watcher.watch(shm.bin_yellow_2)
        self.bins_visible = False

        self.recovery_watcher = shm.watchers.watcher()
        self.recovery_watcher.watch(shm.recovery_vision)
        self.recovery_visible = False

        self.bins = Bins()
        self.recovery = Recovery()

        self.task = None

    def on_run(self):
        self.check_recovery_visible()
        self.check_bins_visible()
     
        print("bins visible is", self.bins_visible)
     
        if self.task is None:
            if self.bins_visible:
                self.logi("Found bins!")
                self.task = self.bins
            elif self.recovery_visible:
                self.logi("Found recovery!")
                self.task = self.recovery
            else:
                self.search()
        else:
            self.task()
            
random_pinger = RandomPinger()
after_pinger = AfterPinger()
