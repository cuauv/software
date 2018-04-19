#!/usr/bin/env python2
from gi.repository import Gtk, Gdk, GLib
import os, signal, sys
import string

from numpy import pi
from canvas import Canvas
from var_util import VarWatcher
from help_dialog import HelpDialog

import logging
from misc.log import init_logging, with_logging
__location__ = os.path.dirname(os.path.realpath(os.path.abspath(sys.argv[0]))) 

import user_config as conf

@with_logging
class PositionTracker3:
    """
    Main GUI for the Position Tracker
    """
    def __init__(self):
        self.gladefile = os.path.join(__location__, "pt3.glade")
        self.builder = Gtk.Builder()
        self.builder.add_from_file(self.gladefile)

        #Create & Link canvas
        self.canvas_box = self.builder.get_object("canvasBox")
        self.canvas = Canvas()
        self.watcher = VarWatcher(self.canvas)
        self.canvas.show()
        self.canvas_box.pack_start(self.canvas, True, True, 0)

        # Automatically connect signals to functions defined above
        # as specified in the glade file
        self.builder.connect_signals(self)

        # Get the main window
        self.window = self.builder.get_object("ptWindow")
        self.window.show()

        self.window.connect("key-press-event", self.key_press)

        #Ctrl+C handling
        def handler(signum, frame):
            self.log.warning("INTERRUPT: exiting gracefully")
            self.window_destroy()

        signal.signal(signal.SIGTERM, handler)
        signal.signal(signal.SIGINT, handler)

        #Fire up the main window 
        self.log.info("Launching GUI. Welcome to PT3! Press %s for help" % conf.key_binds["help"])
        Gtk.main()

    def window_destroy(self, data=None):
        # Close this application
        self.watcher.kill()
        self.logger.critical("Program shutdown!")
        self.canvas.kill()
        Gtk.main_quit()

    def help_menu(self):
        dialog = HelpDialog()
        response = dialog.run()
        dialog.destroy()

    def key_press(self,window,event):
        key = Gdk.keyval_name(event.keyval)

        if event.state == Gdk.ModifierType.CONTROL_MASK:
            key = "ctrl " + key
        if event.state == Gdk.ModifierType.SHIFT_MASK:
            key = "shift " + key

        key = string.lower(key)

        if key == "ctrl w":
            self.window_destroy()

        if key == conf.key_binds["bindings toggle"]:
            conf.bindings_on = not conf.bindings_on
            self.log.info("Key Bindings " + ("on" if conf.bindings_on else "off"))
        if not conf.bindings_on:
            return

        if key == conf.key_binds['quit']:
             self.window_destroy()
        elif key == conf.key_binds["help"]:
            self.help_menu()
        elif key == conf.key_binds["zoom in"]:
            self.canvas.zoom(conf.ZOOM_FACTOR)
        elif key == conf.key_binds["zoom out"]:
            self.canvas.zoom(1./conf.ZOOM_FACTOR)
        elif key == conf.key_binds["follow sub"]:
            self.canvas.follow_sub(True)
        elif key == conf.key_binds["follow position only"]:
            self.canvas.follow_sub(False)
        elif key == conf.key_binds["reset path"]:
            self.canvas.reset_path()
        elif key == conf.key_binds["pan up"]:
            self.canvas.pan(0, conf.PAN_DIST)
        elif key == conf.key_binds["pan up large"]:
            self.canvas.pan(0, conf.PAN_DIST_LARGE)
        elif key == conf.key_binds["pan down"]:
            self.canvas.pan(0, -conf.PAN_DIST)
        elif key == conf.key_binds["pan down large"]:
            self.canvas.pan(0, -conf.PAN_DIST_LARGE)
        elif key == conf.key_binds["pan right"]:
            self.canvas.pan(-conf.PAN_DIST, 0)
        elif key == conf.key_binds["pan right large"]:
            self.canvas.pan(-conf.PAN_DIST_LARGE, 0)
        elif key == conf.key_binds["pan left"]:
            self.canvas.pan(conf.PAN_DIST, 0)
        elif key == conf.key_binds["pan left large"]:
            self.canvas.pan(conf.PAN_DIST_LARGE, 0)
        elif key == conf.key_binds["center view"]:
            self.canvas.center()
        elif key == conf.key_binds["rotate cw"]:
            self.canvas.rotate(-pi/20)
        elif key == conf.key_binds["rotate ccw"]:
            self.canvas.rotate(pi/20)


if __name__ == "__main__":
    #Create an instance of the GTK app and run it
    init_logging(log_level = logging.DEBUG)
    main = PositionTracker3()
