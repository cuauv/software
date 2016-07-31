
import multiprocessing as mp
import shm
from time import sleep
from aslam import *
import pickle

CAMERA_PXW = 1024
CAMERA_FOV = 83.

# dl - distance, unc
# hl - heading, unc
# posl - (n, e), unc (relative)

L_TDC = 10

class Localizer:
    def __init__(s, dl=None, hl=None, posl=None, graph=False):
        s.log = lambda s: log('Localizer', s)
        s.dl, s.hl, s.posl, s.graph = dl, hl, posl, graph
        s.o = Object()
        s.curpos = lambda: vehicle_pos()
        s.centroid = mp.Array('d', range(2))
        s.ready = mp.Value('i', 0)
    def _update(s, centroid):
        while True:
            pos, time = s.curpos(), ctime()
            if s.dl is not None: 
                d, e = s.dl()
                s.log('Updating based on distance {0} error {1}...'.format(d, e))
                s.o.events.append(Event(dobs(d, e, pos), L_TDC))
            if s.hl is not None:
                h, e = s.hl()
                s.log('Updating based on heading {0} error {1}...'.format(h, e))
                s.o.events.append(Event(hobs(h, e, pos), L_TDC))
            if s.posl is not None:
                d, e = s.posl()
                s.log('Updating based on position {0} error {1}...'.format(d, e))
                s.o.events.append(Event(pobs(d, e, pos), L_TDC))
            cent = s.o.centroid(ctime())
            centroid[0], centroid[1] = cent
            if s.graph:
                outf = '/root/cuauv/software/aslam/raw/localizer.out'.format(s)
                s.log('Writing map to {0}'.format(outf))
                data = s.o._pmap(time)
                pickle.dump(data, open(outf, 'w'))
            s.ready.value = 1
    def spawn(s):
        s.log('Forking update process...')
        s.p = mp.Process(target=s._update, args=(s.centroid,))
        s.p.start()
    def kill(s):
        s.log('Killing update process...')
        s.p.terminate()
    def position_relative(s):
        return n.array(s.centroid) - s.curpos()
    def position_absolute(s):
        return n.array(s.centroid)
    def isready(s):
        return s.ready.value

x_to_heading = lambda x: (((x - CAMERA_PXW/2.)/CAMERA_PXW) * CAMERA_FOV) % 360

if __name__ == '__main__':
    l = Localizer()
    l.spawn()
    while True: 
        sleep(0.5)
        print l.position_relative()
