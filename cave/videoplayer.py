
from threading import Thread, Condition, Event
from time import sleep, time

from gi.repository import Gtk, Gdk, GLib

from misc.log import with_logging

FPS = 18

@with_logging
class VideoPlayer(Thread):
    """
    Thread in charge of playing a movie when enabled
    """
    def __init__(self, parent):
        Thread.__init__(self)
        self.c = Condition()
        self.play = False
        self.kill = Event()

        self.parent = parent

        self.video_box = parent.video_box

        self.start()

    def set_play(self, play):
        with self.c:
            self.play = play
            self.c.notify()

    def destroy(self):
        with self.c:
            self.kill.set()
            self.c.notify()

    def run(self):
        while not self.kill.is_set():
            with self.c:
                while not self.play:
                    self.c.wait()
                    if self.kill.is_set():
                        break

            #Playing, increment frame count
            t1 = time()
            Gdk.threads_enter()
            #Currently loops. If undesired, change to:
            #self.parent.increment_frame(1)
            self.parent.increment_frame_loop(1)
            Gdk.threads_leave()
            dt = time() - t1
            self.kill.wait(1.0 / FPS - dt)
