from random import choice, uniform

from mission.framework.stateful_tasks import StatefulTask
from mission.framework.timing import Timer



class StatefulBuoy(StatefulTask):
    def on_first_run(self, *args, **kwargs):
        self.SearchTimer = Timer(3)
        self.TargetTimer = Timer(4)
        self.RamTimer = Timer(1)
        self.PassTimer = Timer(.5)

    def generate_states(self):
        return "search", {
            "search": {
                "tick": self.search,
                "enter": self.enter_search,
                "exit": self.exit_search
            },
            "target": self.target,
            "approach": self.approach,
            "ram": self.ram,
            "pass": self.pass_over
        }

    def enter_search(self):
        self.logi("Entering search")
        
    def exit_search(self):
        self.logi("Exiting search")

    def search(self, *args, **kwargs):
        # print("Searching")
        self.SearchTimer()
        if self.SearchTimer.finished:
            return "target"

    def target(self, *args, **kwargs):
        # print("targeting")
        self.TargetTimer()
        if self.TargetTimer.finished:
            return choice(("approach", "search"))

    def approach(self, *args, **kwargs):
        # print("approach")

        x = uniform(0.0, 1.0)

        if .9 < x < .95:
            print("Lost")
            self.SearchTimer = Timer(1)
            return "search"
        elif .95 < x < .97:
            print("Uncentered")
            self.TargetTimer = Timer(1)
            return "target"
        elif .97 < x:
            print("Approached!")
            return "ram"

    def ram(self, *args, **kwargs):
        # print("ram")
        self.RamTimer()
        if self.RamTimer.finished:
            return "pass"

    def pass_over(self, *args, **kwargs):
        # print("Passing!")
        self.PassTimer()
        if self.PassTimer.finished:
            print("Done")
            self.finish()
