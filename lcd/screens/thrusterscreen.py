import cairo
from screen import Screen
import colorsys
import shm
from common import *
import math

import os, sys
__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0]))) 
img = cairo.ImageSurface.create_from_png(os.path.join(__location__, "screens", "motors.png"))

def color_for_thrust(thrust):
    thrust = abs(thrust)
    if thrust > 255:
        thrust = 255
    h = float(1)/3 - float(1)/3 * thrust / 255
    return colorsys.hsv_to_rgb(h, 1.0, 1.0)

class ThrusterScreen(Screen):
    """
    Displays current thruster values during tests
    """
    def get_name(self):
        return "thruster"

    def draw(self, cr):
        cr.set_source_surface(img, 0, 0)
        cr.paint()

        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
        cr.set_font_size(25)
        txt = "Vehicle Thruster Test"
        cr.set_source_rgb(*WHITE)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(320 / 2 - width/2, 30)
        cr.show_text(txt)

        def draw_thruster(xval, yval, rot, val):
            cr.set_font_size(25)
            txt = "%.0f%%" % (100 * float(val) / 255)
            cr.set_source_rgb(*(color_for_thrust(val)))
            (x, y, width, height, dx, dy) = cr.text_extents(txt)
            cr.move_to(xval, yval)
            cr.save()
            cr.rotate(rot)
            cr.rel_move_to(-width/2, -height/2)
            cr.show_text(txt)
            cr.restore()

        draw_thruster(50, 230, 0, shm.motor_desires.fore_port.get())
        draw_thruster(270, 230, 0, shm.motor_desires.aft_port.get())

        draw_thruster(50, 100, 0, shm.motor_desires.fore_starboard.get())
        draw_thruster(270, 100, 0, shm.motor_desires.aft_starboard.get())

        draw_thruster(160, 90, 0, shm.motor_desires.starboard.get())
        draw_thruster(160, 240, 0, shm.motor_desires.port.get())

        draw_thruster(35, 145, 3 * math.pi / 2, shm.motor_desires.sway_fore.get())
        draw_thruster(285, 145, math.pi / 2, shm.motor_desires.sway_aft.get())
