from conf.vehicle import actuators

from misc.actuator import actuator_group, set_triggers_by_name

from mission.framework.timing import Timer
from mission.framework.combinators import Sequential
from mission.framework.task import Task

class SetActuators(Task):
    """
    Open and/or close the requested actuator triggers.
    """
    def on_run(self, on_triggers=None, off_triggers=None):
        if on_triggers is None:
            on_triggers = []
        if off_triggers is None:
            off_triggers = []

        set_triggers_by_name(on_triggers, off_triggers)
        self.finish()

def FireActuators(actuator_names, duration):
    return Sequential(SetActuators(on_triggers=actuator_names), Timer(duration),
                      SetActuators(off_triggers=actuator_names))

def FireActuator(actuator_name, duration):
    return FireActuators([actuator_name], duration)
