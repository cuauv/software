from mission.framework.task import Task

class StatefulTask(Task):
    """State Machine a la AUV Mission System Task

    A StatefulTask has a dictionary of states (Strings) that each map to a tick function.
    When run, the corresponding tick function is run. If a String is returned, the Statefultask will attempt to transition to that state.

    The generate_states function must be overridden to return the dictionary of states.

    """

    def __init__(self, debug_transitions=True, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.current_state = None  # type: str

        new_state, self.states = self.generate_states() # str, str -> function
        self.enter(new_state)

        if debug_transitions:
            print("Starting in state ({})".format(self.current_state))

    def generate_states(self):
        raise NotImplementedError

    def enter(self, new_state):
        if isinstance(self.states[new_state], dict) and \
           "enter" in self.states[new_state]:
            self.states[new_state]["enter"]()

        self.current_state = new_state

    def exit(self, state):
        if isinstance(self.states[state], dict) and \
           "exit" in self.states[state]:
            self.states[state]["exit"]()

    def tick(self, state):
        if isinstance(self.states[state], dict):
            return self.states[state]["tick"]()
        else:
            return self.states[state]()

    def on_run(self, debug_transitions=True, *args, **kwargs):
        new_state = self.tick(self.current_state)

        if new_state:
            if debug_transitions:
                print("Transitioning from state ({}) to state ({})".format(self.current_state, new_state))
            
            self.exit(self.current_state)
            self.enter(new_state)
