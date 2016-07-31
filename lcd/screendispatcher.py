import shm
from time import time
from control.thrusters import thrusters

from screens.statusscreen import StatusScreen
from screens.pressurescreen import PressureScreen
from screens.pitchrollscreen import PitchRollScreen
from screens.thrusterscreen import ThrusterScreen
from screens.dvlscreen import DvlScreen
from screens.sponsorsscreen import SponsorsScreen
from screens.messagescreen import MessageScreen
from screens.actuatorscreen import ActuatorScreen

PRESSURE_TIME_STORE = 5 #seconds
PRESSURE_THRESH = 0.2 # this delta over the pressure_time_store will trigger
PRESSURE_DISPLAY_TIME = 60 # seconds to display after triggered

PITCHROLL_DISPLAY_TIME = 300

THRUSTER_DISPLAY_TIME = 30
ACTUATOR_DISPLAY_TIME = 3

class ScreenDispatcher:
    """
    Handles all logic of choosing which screen to draw
    """
    def __init__(self):
        #screens
        self.statusscreen = StatusScreen()
        self.pressurescreen = PressureScreen()
        self.pitchrollscreen = PitchRollScreen()
        self.thrusterscreen = ThrusterScreen()
        self.dvlscreen = DvlScreen()
        self.sponsorsscreen = SponsorsScreen()
        self.messagescreen = MessageScreen()
        self.actuatorscreen = ActuatorScreen()

        self.screens = [self.statusscreen,
                self.pressurescreen,
                self.pitchrollscreen,
                self.thrusterscreen,
                self.dvlscreen,
                self.sponsorsscreen,
                self.messagescreen,
                self.actuatorscreen]

        #pressure state
        self.pressure_triggered = False
        self.pressure_trigger_time = 0
        self.pressure_readings = []

        #pitch-roll state
        self.pr_triggered = False
        self.pr_trigger_time = 0
        self.last_in_water = None

        #thruster test state
        self.thruster_triggered = False
        self.thruster_trigger_time = 0
        self.last_thrusters = None

        #actuator state
        self.actuator_triggered = False
        self.actuator_trigger_time = 0
        self.last_actuator_on = False
        self.afalse = True

    def stop_all(self):
        for s in self.screens:
            s.stop()

    def draw(self, cr):
        ### Pressure internal state update logic

        # Reset after no activity
        if self.pressure_triggered:
            if time() >= self.pressure_trigger_time + PRESSURE_DISPLAY_TIME:
                self.pressure_triggered = False

        # Take reading
        self.pressure_readings.append((time(), shm.pressure.internal.get()))
        
        #Remove old readings
        while self.pressure_readings[0][0] < time() - PRESSURE_TIME_STORE:
            self.pressure_readings.pop(0)

        # Trigger if threshold is met
        pressure_old = self.pressure_readings[0][1]
        if pressure_old != 0 and shm.pressure.internal.get() != 0 and abs(pressure_old - shm.pressure.internal.get()) >= PRESSURE_THRESH:
            self.pressure_triggered = True
            self.pressure_trigger_time = time()

        ### Pitch / roll state update logic

        # Reset after no activity
        if self.pr_triggered:
            if time() >= self.pr_trigger_time + PITCHROLL_DISPLAY_TIME:
                self.pr_triggered = False

        in_water = (shm.depth.depth.get() > 0.5) or ((shm.dvl.low_amp_1.get() != 1 or shm.dvl.low_amp_2.get() != 1 or shm.dvl.low_amp_3.get() != 1 or shm.dvl.low_amp_4.get() != 1) and shm.dvl.tick.get() != 0)

        if self.last_in_water is None:
            self.last_in_water = in_water

        if in_water != self.last_in_water:
            if in_water:
                self.pr_triggered = True
                self.pr_trigger_time = time()
            self.last_in_water = in_water

        ### Thruster test state update logic
        if self.thruster_triggered:
            if time() >= self.thruster_trigger_time + THRUSTER_DISPLAY_TIME:
                self.thruster_triggered = False

        current_thrusters = [m.get() for m in thrusters]

        if (shm.settings_control.enabled.get() == False) and (self.last_thrusters is not None) and (current_thrusters != self.last_thrusters) and (shm.switches.soft_kill.get() == False):
            self.thruster_triggered = True
            self.thruster_trigger_time = time()

        self.last_thrusters = current_thrusters

        ### Actuator sstate update logic

        any_actuator_triggered = any([v.trigger.get() == 1 for v in self.actuatorscreen.ACTUATORS.values()])
        if self.actuator_triggered:
            if time() >= self.actuator_trigger_time + ACTUATOR_DISPLAY_TIME:
                self.actuator_triggered = False

        if not any_actuator_triggered:
            self.afalse = True

        if self.afalse and not self.actuator_triggered and any_actuator_triggered:
            self.actuator_triggered = True
            self.actuator_trigger_time = time()
            self.afalse = False

        ### Dispatch
        screen_override = None
        shm_screen = shm.lcd.screen.get()
        if len(shm_screen) > 0:
            # Attempt to match shm screen with one of our screens
            for s in self.screens:
                if s.get_name().startswith(shm_screen):
                    screen_override = s
                    break

        if screen_override is not None:
            # A screen is specified in shm manually
            # let's display that one
            screen_override.draw(cr)
        else:
            valids = []
            if self.pressure_triggered:
                valids.append((time() - self.pressure_trigger_time, self.pressurescreen))
            if self.pr_triggered:
                valids.append((time() - self.pr_trigger_time, self.pitchrollscreen))
            if self.thruster_triggered:
                valids.append((time() - self.thruster_trigger_time, self.thrusterscreen))
            if self.actuator_triggered:
                valids.append((time() - self.actuator_trigger_time, self.actuatorscreen))

            if len(valids) == 0:
                self.statusscreen.draw(cr)
            else:
                valids.sort() # Pick most recently triggered screen
                screen = valids[0][1]
                screen.draw(cr)

