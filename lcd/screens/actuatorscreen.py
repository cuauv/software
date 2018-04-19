import cairo
from screen import Screen
import colorsys
import shm
from common import *

class ActuatorScreen(Screen):
    """
    Displays actuator firing info 
    """

    def __init__(self):
        self.last = "None"

    ACTUATORS =    {"Torp. Left": shm.actuator_2,
                    "Torp. Right": shm.actuator_3,
                    "Marker #1": shm.actuator_1,
                    "Marker #2": shm.actuator_4,
                    "Port Close": shm.actuator_8,
                    "Port Open": shm.actuator_7,
                    "Stbd. Close": shm.actuator_6,
                    "Stbd. Open": shm.actuator_5,
                    "Aft Close": shm.actuator_10,
                    "Aft Open": shm.actuator_9}

    def get_name(self):
        return "actuator"

    def draw(self, cr):

        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        cr.set_font_size(20)

        def draw_txt(txt, v, sz, clr):
            cr.set_font_size(sz)
            cr.set_source_rgb(*clr)
            (x, y, width, height, dx, dy) = cr.text_extents(txt)
            cr.move_to(320 / 2 - width/2, v)
            cr.show_text(txt)

        draw_txt("Actuator Status", 30, 20, WHITE)
        for k,v in self.ACTUATORS.items():
            if v.trigger.get() == 1:
                self.last = k
                break
        
        draw_txt("Firing Actuator", 90, 30, GREEN)
        draw_txt(self.last, 190, 50, RED)
