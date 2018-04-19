import cairo
from screen import Screen
import colorsys
import shm
from common import *
import math

GOOD_DEG = 0.8
BAD_DEG = 3.0

def color_for_deg(deg):
    deg = abs(deg)
    if deg < GOOD_DEG:
        deg = GOOD_DEG
    if deg > BAD_DEG:
        deg = BAD_DEG
    hs = (BAD_DEG - GOOD_DEG)
    ls = (deg - GOOD_DEG)
    h = float(1)/3 - float(1)/3 * float(ls) / float(hs)
    return colorsys.hsv_to_rgb(h, 1.0, 1.0)

class PitchRollScreen(Screen):
    """
    Displays pitch and roll
    """

    def get_name(self):
        return "pitchroll"

    def draw(self, cr):

        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
        cr.set_font_size(20)

        txt = "Vehicle Trimming"
        cr.set_source_rgb(*WHITE)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(320 / 2 - width/2, 30)
        cr.show_text(txt)

        # Draw pitch and roll
        cr.set_font_size(100)
        txt = u"%.1f\u00B0" % shm.kalman.pitch.get()
        cr.set_source_rgb(*(color_for_deg(shm.kalman.pitch.get())))
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(307 - width, 130)
        cr.show_text(txt)

        txt = u"%.1f\u00B0" % shm.kalman.roll.get()
        cr.set_source_rgb(*(color_for_deg(shm.kalman.roll.get())))
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(307 - width, 220)
        cr.show_text(txt)

        # Draw labels
        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        cr.set_font_size(20)
        cr.set_source_rgb(*WHITE)
        cr.move_to(5, 75)
        cr.show_text("PITCH")

        cr.move_to(5, 165)
        cr.show_text("ROLL")



