import cairo
from screen import Screen
import colorsys
import shm
from common import *
import math
import numpy

import Image #PIL

from time import time

import os, sys
__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0]))) 

LOGO_FOLDER = os.path.join(__location__, "screens", "logos")

LOGO_DISPLAY_TIME = 2 # seconds for each logo

#Set screens here, format is list of ("title for sponsor", "folder containing sponsor images")
SPONSOR_FOLDERS = [("Platinum Sponsors", os.path.join(LOGO_FOLDER, "Platinum Sponsors")),
                ("Gold Sponsors", os.path.join(LOGO_FOLDER, "Gold Sponsors")),
                ("Silver Sponsors", os.path.join(LOGO_FOLDER, "Silver Sponsors"))]

IMAGE_EXTS = [".jpg", ".jpeg", ".bmp", ".png"]

THUMB_W = 310
THUMB_H = 170

# TODO: Complete everything

class SponsorsScreen(Screen):
    """
    Displays a slideshow of sponsor logos
    """
    def __init__(self):
        self.all_logos = []
        self.last_draw_time = 0
        self.img = None

    def populate_logos(self):
        for (title, dir) in SPONSOR_FOLDERS:
            if not os.path.isdir(dir):
                print "ERROR: could not load directory \"%s\"" % dir
                return False
            for f in os.listdir(dir):
                if any([f.lower().endswith(x) for x in IMAGE_EXTS]):
                    self.all_logos.append((title, os.path.join(dir, f)))
        return True

    def get_name(self):
        return "sponsors"

    def get_img_from_path(self, pth):
        # Load up images using PIL
        imp = Image.open(pth)
        imp.load()

        # Attempt to normalize all the mess of color spaces that we have
        # in our sponsor logo images
        imp = imp.convert("RGBA")
        im = Image.new("RGB", imp.size, (255,255,255))
        splitted = imp.split()
        if len(splitted) == 4:
            im.paste(imp, mask=splitted[3])
        else:
            im.paste(imp)

        # Convert to thumbnail size
        w, h = im.size
        if w > THUMB_W or h > THUMB_H:
            im.thumbnail((THUMB_W,THUMB_H), Image.ANTIALIAS)

        # Swap B and R (PIL and cairo data use different formats)
        b, g, r = im.split()
        im = Image.merge("RGB", (r, g, b))

        # Convert to cairo surface
        im.putalpha(256)
        arr = numpy.array(im)
        h, w, channels = arr.shape
        #print "Slideshow: Loaded image %s (%dx%d)" % (pth, w, h)
        img_surface = cairo.ImageSurface.create_for_data(arr, cairo.FORMAT_RGB24, w, h)
        return img_surface

    def draw(self, cr):

        if time() > (self.last_draw_time + LOGO_DISPLAY_TIME):

            if len(self.all_logos) == 0:
                r = self.populate_logos()
                if not r:
                    return

                if len(self.all_logos) == 0:
                    return

            (self.title, imgpath) = self.all_logos.pop(0)
            self.img = self.get_img_from_path(imgpath)
            self.last_draw_time = time()

        cr.set_source_rgb(*WHITE)
        cr.rectangle(0,0,320,240)
        cr.fill()

        w = self.img.get_width()
        h = self.img.get_height()
        cr.set_source_surface(self.img, 320 / 2 - w/2, 240 /2 - h/2)
        cr.paint()

        # Window Title
        cr.select_font_face("FreeSans", cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
        cr.set_font_size(25)
        txt = self.title
        cr.set_source_rgb(*BLACK)
        (x, y, width, height, dx, dy) = cr.text_extents(txt)
        cr.move_to(320 / 2 - width/2, 30)
        cr.show_text(txt)

