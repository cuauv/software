from cave.libcave.registered_elements import register_mission_element

import shm

"""
All mission elements must be defined here.

Classes decorated with register_mission_element will be included for
use in CAVE.
"""

#Mission element base class
class MissionElement:
    def __init__(self, enable_var, watch_group, train_watch_group=None):
        """
        If a train_watch_group is specified, we will wait on this group instead
        when training. It's hacky, but needed for elements that use different
        sources for training vs testing. Would have to rearchitect cave training
        to avoid this...
        """
        self.enable_var = enable_var
        self.watch_group = watch_group
        self.train_watch_group = train_watch_group
        if self.train_watch_group is None:
            self.train_watch_group = watch_group

    #Initialize, called before testing
    def init(self):
        self.enable_var.set(True)
        self.w = shm.watchers.watcher()
        self.w.watch(self.watch_group)
        self.tw = shm.watchers.watcher()
        self.tw.watch(self.train_watch_group)

    #Testing methods must check get_next to see if there
    #are any more results. Some elements (like pipes), write
    #results to multiple groups since multiple elements can
    #appear on screen at the same time. The tag should continue
    #testing while get_next returns true (at which point the
    #mission element should return results from the next availible
    #results group.
    def get_next(self):
        return False #Mission elements without multiple results groups
                     #can just use this default get_next method
    
    #Called by testing application after each frame test. should
    #reset the results_group state for the next test
    def reset(self):
        pass

    #Deinitialize, called after testing completed
    def deinit(self):
        self.enable_var.set(False)

    #Wait until the variable is updated by vision
    def wait(self, train=False):
        if train:
            self.tw.wait(False)
        else:
            self.w.wait(False)

    #Force variable update now! (for abort functionality))
    def broadcast(self):
        self.w.broadcast()
        self.tw.broadcast()

class Buoy(MissionElement):
    def __init__(self):
        enable_var = shm._eval("vision_modules." + self.buoy_color[0].upper() + self.buoy_color[1:] + "Buoy")
        group = shm._eval(self.buoy_color + "_buoy_results")
        MissionElement.__init__(self, enable_var, group)

    def get_x_y(self):
        return self.watch_group.center_x.get(), self.watch_group.center_y.get()

    def get_visible(self):
        return self.watch_group.probability.get() > 0.0

@register_mission_element
class RedBuoy(Buoy):
    name = "Buoy (Orange)"  # To keep all the old tags usable.
    buoy_color = "red"

@register_mission_element
class GreenBuoy(Buoy):
    name = "Buoy (Green)"
    buoy_color = "green"

@register_mission_element
class YellowBuoy(Buoy):
    name = "Buoy (Yellow)"
    buoy_color = "yellow"

#@register_mission_element
class LEDBuoy(MissionElement):
    def __init__(self, buoy_color):
        #Will watch led_buoy_results3 since this is the last one that is written to by vision
        MissionElement.__init__(self, shm.buoy_settings.enabled, shm.led_buoy_results3) 
        self.buoy_color = buoy_color
        self.reset()
    def reset(self):
        self.results_groups = [shm.led_buoy_results, shm.led_buoy_results2, shm.led_buoy_results3]
    def get_x_y(self):
        return self.results_groups[-1].center_x.get(), self.results_groups[-1].center_y.get()
    def get_visible(self):
        return (self.results_groups[-1].probability.get() > 0.5 and self.results_groups[-1].color.get() == self.buoy_color)
    def get_next(self): #override
        self.results_groups.pop()
        return len(self.results_groups) > 0 #have next if there are items left to process

@register_mission_element
class LEDBuoyRed(LEDBuoy):
    name = "LED Buoy (Red)"
    def __init__(self):
        LEDBuoy.__init__(self, "red")

@register_mission_element
class LEDLEDBuoyYellow(LEDBuoy):
    name = "LED Buoy (Yellow)"
    def __init__(self):
        LEDBuoy.__init__(self, "yellow")

@register_mission_element
class LEDLEDBuoyGreen(LEDBuoy):
    name = "LED Buoy (Green)"
    def __init__(self):
        LEDBuoy.__init__(self, "green")

class Wire(MissionElement):
    def __init__(self):
        MissionElement.__init__(self, shm.wire_settings.enabled, shm.wire_results)
    def get_x_y(self):
        return shm.wire_results.center_x.get(), shm.wire_results.center_y.get()
    def get_visible(self):
        return shm.wire_results.probability.get() > 0.5

@register_mission_element
class NormalWire(Wire):
    name = "Wire"
    def __init__(self):
        Wire.__init__(self)

@register_mission_element
class WireLEDRed(Wire):
    name = "Wire LED (Red)"
    def __init__(self):
        Wire.__init__(self)
    def get_visible(self): #overloaded
        #Must also verify light color & visibility
        return (Wire.get_visible(self) and
                shm.wire_results.light_probability.get() > 0.5 and
                shm.wire_results.light_color.get() == "red")

@register_mission_element
class WireLEDGreen(Wire):
    name = "Wire LED (Green)"
    def __init__(self):
        Wire.__init__(self)
    def get_visible(self): #overloaded
        #Must also verify light color & visibility
        return (Wire.get_visible(self) and
                shm.wire_results.light_probability.get() > 0.5 and
                shm.wire_results.light_color.get() == "green")

class Torpedoes(MissionElement):
    def __init__(self, color):
        self.color = color
        MissionElement.__init__(self, shm.nest_settings.enabled, shm.nest_results)
    def init(self): #overloaded
        MissionElement.init(self)
        shm.nest_settings.color.set(self.color) #must also set the color
    def get_x_y(self):
        return shm.nest_results.center_x.get(), shm.nest_results.center_y.get()
    def get_visible(self):
        return shm.nest_results.probability.get() > 0.5

@register_mission_element
class TorpedoesRed(Torpedoes):
    name = "Torpedoes (Red)"
    def __init__(self):
        Torpedoes.__init__(self, "red")

@register_mission_element
class TorpedoesBlue(Torpedoes):
    name = "Torpedoes (Blue)"
    def __init__(self):
        Torpedoes.__init__(self, "blue")

@register_mission_element
class TorpedoesGreen(Torpedoes):
    name = "Torpedoes (Green)"
    def __init__(self):
        Torpedoes.__init__(self, "green")

@register_mission_element
class TorpedoesYellow(Torpedoes):
    name = "Torpedoes (Yellow)"
    def __init__(self):
        Torpedoes.__init__(self, "yellow")

@register_mission_element
class Pipe(MissionElement):
    name = "Pipe"
    def __init__(self):
        #watch pipe_results3 since it is written to last by vision
        MissionElement.__init__(self, shm.vision_modules.Pipes, shm.pipe_results)
        self.reset()
    def reset(self):
        self.results_groups = [shm.pipe_results]
    def get_x_y(self):
        return self.results_groups[-1].center_x.get(), self.results_groups[-1].center_y.get()
    def get_visible(self):
        return self.results_groups[-1].heuristic_score.get() > 0
    def get_heading_delta(self):
        return self.results_groups[-1].angle.get()

#@register_mission_element
class Wheel(MissionElement):
    name = "Wheel"
    def __init__(self):
        MissionElement.__init__(self, shm.emperor_settings.enabled, shm.wheel_results)
    def get_x_y(self):
        return shm.wheel_results.x.get(), shm.wheel_results.y.get()
    def get_visible(self):
        return shm.wheel_results.probability.get() > 0.5

class Shifter(MissionElement):
    def __init__(self, is_up):
        MissionElement.__init__(self, shm.emperor_settings.enabled, shm.shifter_results)
        self.is_up = is_up
    def get_x_y(self):
        return shm.shifter_results.x.get(), shm.shifter_results.y.get()
    def get_visible(self):
        return (shm.shifter_results.probability.get() > 0.5 and
                shm.shifter_results.is_up() == self.is_up)
    
#@register_mission_element
class ShifterUp(Shifter):
    name = "Shifter (Up)"
    def __init__(self):
        Shifter.__init__(self, 1)

#@register_mission_element
class ShifterDown(Shifter):
    name = "Shifter (Down)"
    def __init__(self):
        Shifter.__init__(self, 0)

class Bins(MissionElement):
    def __init__(self, results_group):
        MissionElement.__init__(self, shm.shape_settings.enabled, results_group, train_watch_group=shm.shape_results_4)
        self.results_group = results_group
    def get_x_y(self):
        return self.results_group.x.get(), self.results_group.y.get()
    def get_visible(self):
        return self.results_group.probability.get() != 0

@register_mission_element
class BinsSquid(Bins):
    name = "Bins (Squid)"
    def __init__(self):
        Bins.__init__(self, shm.shape_classifier_squid)

@register_mission_element
class BinsCrab(Bins):
    name = "Bins (Crab)"
    def __init__(self):
        Bins.__init__(self, shm.shape_classifier_crab)

@register_mission_element
class BinsJellyfish(Bins):
    name = "Bins (Jellyfish)"
    def __init__(self):
        Bins.__init__(self, shm.shape_classifier_jellyfish)

@register_mission_element
class BinsMothership(Bins):
    name = "Bins (Mothership)"
    def __init__(self):
        Bins.__init__(self, shm.shape_classifier_mothership)
        
@register_mission_element
class Recovery(MissionElement):
    name = "Recovery"
    def __init__(self):
        MissionElement.__init__(self, shm.recovery_settings.enabled, shm.recovery_results)
    def get_x_y(self):
        return shm.recovery_results.center_x.get(), shm.recovery_results.center_y.get()
    def get_visible(self):
        return shm.recovery_results.probability.get() > 0.5
    def get_heading_delta(self):
        return shm.recovery_results.heading_delta.get()
