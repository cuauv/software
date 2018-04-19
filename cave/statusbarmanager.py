
from threading import Thread, Condition, Event
from time import sleep

from gi.repository import Gtk, Gdk, GLib

class StatusBarManager(Thread):
    """
    Displays messages in a given status bar
    Features queueing of messages + timeouts
    Serves as a decent example of Gtk threading
    """
    def __init__(self, sbar):
        Thread.__init__(self)
        self.c = Condition()
        self.sbar = sbar
        self.mlist = []
        self.start()
        self.kill = Event()

    def display(self, message, sec):
        with self.c:
            self.mlist.insert(0, (message, sec))
            self.c.notify()
    
    def destroy(self):
        with self.c:
            self.kill.set()
            self.c.notify()

    def run(self):
        while True:
            with self.c:
                while len(self.mlist) == 0:
                    self.c.wait()
                    if self.kill.is_set():
                        return
                (msg, sec) = self.mlist.pop()
            #Display message
            Gdk.threads_enter()
            self.sbar.push(0, msg)
            Gdk.threads_leave()
            self.kill.wait(sec) #sleep, unless kill interrupts us
            if self.kill.is_set():
                return
            Gdk.threads_enter()
            self.sbar.pop(0)
            Gdk.threads_leave()


