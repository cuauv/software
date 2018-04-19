import cairo
from screen import Screen
import colorsys
import shm
from common import *

# Configs
GOOD_PSI = 11.5
HIGH_PSI = 13.5
LOW_PSI = 10.5


BAREXP = 1.8
BARWIDTHPX = 290

BARX = 15
BARY = 190
BARH = 30
TICKOVER = 10


class PressureScreen(Screen):
    """
    Displays hull pressure (during pressure changes)
    """

    def get_name(self):
        return "pressure"

    def draw(self, cr):

        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        cr.set_font_size(20)

        txt = "Internal Hull Pressure (PSI)"
        cr.set_source_rgb(*WHITE)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(320 / 2 - width/2, 30)
        cr.show_text(txt)

        """
        txt = "PSI"
        cr.set_source_rgb(*WHITE)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(320 / 2 - width/2, 180)
        cr.show_text(txt)
        """

        real_psi = shm.pressure.internal.get()

        # Compute color
        color_psi = real_psi
        if color_psi > HIGH_PSI:
            color_psi = HIGH_PSI
        if color_psi < LOW_PSI:
            color_psi = LOW_PSI
        if color_psi >= GOOD_PSI:
            hs = (HIGH_PSI - GOOD_PSI)
            ls = (color_psi - GOOD_PSI)
            h = float(1)/3 - float(1)/3 * float(ls) / float(hs)
        else:
            hs = (GOOD_PSI - LOW_PSI)
            ls = (color_psi - LOW_PSI)
            h = float(1)/3 * float(ls) / float(hs)
        PSICOLOR = colorsys.hsv_to_rgb(h, 1.0, 1.0)


        # Draw pressure
        cr.set_font_size(120)
        txt = "%.2f" % real_psi
        cr.set_source_rgb(*PSICOLOR)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(320 / 2 - width/2 - 12, 140)
        cr.show_text(txt)



        ### Draw pressure gague

        #pressure bar
        psirange = float(HIGH_PSI - GOOD_PSI) * BAREXP
        bottompsi = float(HIGH_PSI + GOOD_PSI) / 2 - psirange/2
        toppsi = float(HIGH_PSI + GOOD_PSI) / 2 + psirange/2
        if real_psi < bottompsi:
            real_psi = bottompsi
        if real_psi > toppsi:
            real_psi = toppsi
        pixels_per_psi = float(BARWIDTHPX) / psirange
        cr.set_source_rgb(*PSICOLOR)
        cr.rectangle(BARX, BARY, (real_psi - bottompsi) * pixels_per_psi, BARH)
        cr.fill()


        # ticks
        high_x = BARX + (HIGH_PSI - bottompsi) * pixels_per_psi
        low_x = BARX + (GOOD_PSI - bottompsi) * pixels_per_psi

        cr.set_source_rgb(*WHITE)
        cr.set_line_width(1)
        cr.move_to(high_x, BARY - TICKOVER)
        cr.line_to(high_x, BARY + BARH)
        cr.move_to(low_x, BARY - TICKOVER)
        cr.line_to(low_x, BARY + BARH)
        cr.stroke()

        #Labels
        cr.set_font_size(18)

        txt = "Unsealed"
        cr.set_source_rgb(*WHITE)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(high_x - (width/2), BARY-TICKOVER - 2)
        cr.show_text(txt)

        txt = "Sealed"
        cr.set_source_rgb(*WHITE)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(low_x - (width/2), BARY-TICKOVER - 2)
        cr.show_text(txt)

        #border
        cr.set_source_rgb(0.8, 0.8, 0.8)
        cr.set_line_width(5)
        cr.rectangle(BARX, BARY, BARWIDTHPX, BARH)
        cr.stroke()

