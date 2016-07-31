from threading import Thread, Timer
import shm
from cairo import Matrix

class VarWatcher(Thread):
    def __init__(self, canvas):
        Thread.__init__(self)
        self.canvas = canvas
        self.killed = False
        self.w = shm.watchers.watcher()
        self.w.watch(shm.kalman)
        #self.w.watch(shm.mission)
        self.start()

    def kill(self):
        self.killed = True
        self.w.broadcast()

    def run(self):
        while not self.killed:
            self.w.wait(False)
            if self.killed:
                return
            self.canvas.queue_draw()

class SmoothVar():
    steps = 20
    def __init__(self, value, vel=0.01, initial=1, thresh=0):
        self.value = value
        self.desire = value
        self.final = initial
        self.initial = initial
        self.vel = vel
        self.snapThresh = thresh

    def setDesire(self, value):
        self.desire = value

    def setVelocity(self, value):
        self.vel = value

    def setThresh(self, value):
        self.snapThesh = value

    def setFinal(self, value):
        self.final = value

    def setInitial(self, value):
        self.initial = value

    def running(self):
        return self.value != self.desire

    def tick(self):
        speed = (self.desire - self.value) * self.vel
        if abs(speed)/self.vel < self.snapThresh:            # snap into place
            self.value = self.desire
        else:
            self.value += speed

'''
class SmoothMat():
    steps = 20
    def __init__(self, value, vel=.5, thresh=1):
        self.value = value
        self.desire = value
        self.vel = vel
        self.snapThresh = thresh
        self.active = False

    def setDesire(self, value):
        self.desire = value

    def running(self):
        return self.value != self.desire

    def tick(self):
        value = [i for i in self.value]
        for i in range(6):
            diff = self.desire[i] - value[i]
            speed = diff * self.vel
            if abs(diff) < self.snapThresh:
                value[i] = self.desire[i]
            else:
                value[i] += speed
        self.value = Matrix(*value)
'''
