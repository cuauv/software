from auval.shmem import *
from auval.watcher import *
from time import time, sleep

from threading import Thread


bat1 = SharedVar("/diagnostics/power/pod/port/current")
bat1_w = VarWatcher(bat1)

bat2 = SharedVar("/diagnostics/power/pod/starboard/current")
bat2_w = VarWatcher(bat1)



class CoulombCounter(Thread):
    def __init__(self, var, watch):
        Thread.__init__(self)
        self.var = var
        self.watch = watch
        
        self.go = True
        self.count = 0

        self.last_t = time()

    def get_count(self):
        return self.count

    def run(self):
        self.last_t = time()
        while self.go:
            dt = time() - self.last_t
            self.last_t = time()
            amp = self.var.get()
            dc = amp * dt
            self.count += dc

            self.watch.checkpoint()
            self.watch.wait_for_change()

    def kill(self):
        self.go = False

    
b1c = CoulombCounter(bat1, bat1_w)
b2c = CoulombCounter(bat2, bat2_w)
b1c.start()
b2c.start()

start = time()

print "Now counting coulombs..."

try:
    while True:

        c1 = b1c.get_count()
        c2 = b2c.get_count()

        print "PORT: %dC, STARBOARD: %dC" % (c1, c2)


        sleep(5)
except KeyboardInterrupt:
    b1c.kill()
    b2c.kill()
    print "Stopping count..."
    print "----------------------"
    print "Summary of data:"

    c1 = b1c.get_count()
    c2 = b2c.get_count()
    ct = c1 + c2

    wh = ct * 24 / 3600

    print "Time Run:          %s S" % (time() - start)
    print "PORT Total:        %s C" % c1
    print "STARBOARD Total:   %s C" % c2
    print "Overall Total:     %s C" % ct
    print "Watt-hours:        %s Wh (assuming 24V) " % wh

    


