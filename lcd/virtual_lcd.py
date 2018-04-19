import cairo
from gi.repository import Gtk, Gdk, GLib
import os, sys

from threading import Thread

from lcd import Lcd

__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0]))) 

class Canvas(Gtk.DrawingArea):
    def __init__ (self):
        Gtk.DrawingArea.__init__(self)
        self.connect('draw', self._do_expose)
        self.frame = None

    def set_frame(self, frame):
        self.frame = frame
        self.queue_draw()

    def _do_expose(self, widget, cr):
        #Draw the video frame
        if self.frame is not None:
            cr.set_source_surface(self.frame, 0, 0)
            cr.paint()
        cr.save()

class Window(Thread):
    def __init__(self):
        Thread.__init__(self)
        self.gladefile = os.path.join(__location__, "vlcd.glade")
        self.builder = Gtk.Builder()
        self.builder.add_from_file(self.gladefile)
        self.builder.connect_signals(self)
        self.window = self.builder.get_object("window")
        self.box = self.builder.get_object("box")
        self.canvas = Canvas()
        self.canvas.show()
        self.box.pack_start(self.canvas, True, True, 0)
        self.window.show()
        Gdk.threads_init()
        GLib.threads_init()
        self.start()

    def run(self):
        Gdk.threads_enter()
        Gtk.main()
        Gdk.threads_leave()

    def window_destroy(self, event):
        Gdk.threads_enter()
        Gtk.main_quit()
        Gdk.threads_leave()

    def set_surface(self, surface):
        Gdk.threads_enter()
        self.canvas.set_frame(surface)
        Gdk.threads_leave()

class VirtualLcd(Lcd):
    def __init__(self):
        self.screen = Window()
    
    def __del__(self):
        self.screen.window_destroy(None)

    def init(self):
        pass

    def clear(self):
        pass

    def write_surface(self, surface):
        self.screen.set_surface(surface)

