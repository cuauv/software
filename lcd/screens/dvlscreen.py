import cairo
from screen import Screen
import colorsys
import shm
from common import *

# Configs
GOOD_PSI = 11.5
HIGH_PSI = 13.5
LOW_PSI = 10.5

class DvlScreen(Screen):
    
    def get_name(self):
        return "dvl"

    def draw(self, cr):

        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        cr.set_font_size(20)

        txt = "DVL Beam Blockage Display"
        cr.set_source_rgb(*WHITE)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(320 / 2 - width/2, 30)
        cr.show_text(txt)

        # Kill indicator
        def beamindicator(x, y, txt, clr):
            w = 120
            h = 40
            cr.move_to(x, y)
            cr.set_source_rgb(*clr)
            cr.rectangle(x - w/2, y - h/2, w, h)
            cr.fill()

            cr.set_font_size(30)
            cr.move_to(x, y)
            cr.set_source_rgb(*WHITE)
            cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
            (x, y, width, height, dx, dy) = cr.text_extents(txt)
            cr.rel_move_to(-width/2, height/2)
            cr.show_text(txt)

        beamindicator(80, 80, "BEAM 1", RED if shm.dvl.low_amp_1.get() == 1 else GREEN)
        beamindicator(240, 80, "BEAM 2", RED if shm.dvl.low_amp_2.get() == 1 else GREEN)
        
        beamindicator(80, 150, "BEAM 3", RED if shm.dvl.low_amp_3.get() == 1 else GREEN)
        beamindicator(240, 150, "BEAM 4", RED if shm.dvl.low_amp_4.get() == 1 else GREEN)


