import cairo
import shm
import math
from threading import Thread, Lock
from time import time, sleep
from self_test.selftestengine import SelfTestEngine

from screen import Screen
from common import *

import os, sys
__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0]))) 

# Gemini png (currently this is the test pattern...)
img = cairo.ImageSurface.create_from_png(os.path.join(__location__, "screens", "gemini.png"))

class SelfTestThread(Thread):
    MAX_HZ = 5
    def __init__(self):
        Thread.__init__(self)
        self.results = {}
        self.engine = SelfTestEngine()
        self.l = Lock()
        self.killed = False
        self.start()

    def kill(self):
        self.killed = True

    def get_results(self):
        with self.l:
            return self.results

    def run(self):
        while not self.killed:
            start = time()
            results = {}
            for result in self.engine.run_tests():
                results[result["key"]] = (RED if not result["pass"] else (YELLOW if result["warn"] else GREEN)) + (0.8,)
            with self.l:
                self.results = results
            remaining = (1.0 / self.MAX_HZ) - (time() - start)
            if remaining > 0:
                sleep(remaining)

class StatusScreen(Screen):

    def __init__(self):
        self.stt = SelfTestThread()

    def get_name(self):
        return "status"

    def stop(self):
        self.stt.kill()

    def draw(self, cr):
        cr.set_source_surface(img, 0, 0)
        cr.paint()

        # Draw bounding box
        cr.set_source_rgba(0, 0, 0, 0.7)
        cr.rectangle(10, 10, 145, 143)
        cr.fill()

        # Draw pressure, pitch, roll
        cr.set_source_rgb(1.0, 1.0, 1.0)
        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        cr.set_font_size(20)
        spacing = 25
        py = 35
        cr.move_to(20, py)
        cr.show_text(u"Pitch: %.1f\u00B0" % (shm.kalman.pitch.get()))
        py += spacing
        cr.move_to(20, py)
        cr.show_text(u"Roll: %.1f\u00B0" % (shm.kalman.roll.get()))
        py += spacing
        cr.move_to(20, py)
        cr.show_text(u"Hdg: %.1f\u00B0" % (shm.kalman.heading.get()))
        py += spacing
        cr.move_to(20, py)
        cr.show_text("Hull: %.2f psi" % (shm.pressure.internal.get()))
        py += spacing

        # Kill indicator
        def killindicator(x, y, txt, clr):
            w = 60
            h = 25
            cr.move_to(x, y)
            cr.set_source_rgb(*clr)
            cr.rectangle(x - w/2, y - h/2, w, h)
            cr.fill()

            cr.move_to(x, y)
            cr.set_source_rgb(*WHITE)
            cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
            (x, y, width, height, dx, dy) = cr.text_extents(txt)
            cr.rel_move_to(-width/2, height/2)
            cr.set_font_size(20)
            cr.show_text(txt)

        killindicator(50, py - 5, "HK", RED if shm.switches.hard_kill.get() else DARKGREEN)
        killindicator(115, py - 5, "SK", RED if shm.switches.soft_kill.get() else DARKGREEN)


        


        #Draw self test indication
        PADDING = 2
        w = 85
        h = 16
        cx = 320 - w - PADDING
        cy = PADDING

        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        cr.set_font_size(14)
        
        for name,color in sorted(self.stt.get_results().items()):
            cr.set_source_rgba(*color)
            cr.rectangle(cx, cy, w, h)
            cr.fill()

            cr.set_source_rgb(*(BLACK if color == YELLOW else WHITE))
            (x, y, width, height, dx, dy) = cr.text_extents(name)
            cr.move_to(int(cx + float(w) / 2 - float(width)/2), int(cy + float(h)/2 + float(height)/2))
            cr.show_text(name)
            cy += h + PADDING

