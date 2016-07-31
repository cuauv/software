'''
Adaptive Simultaneous Localization & Mapping
2014, Christopher Goes
'''

from __future__ import division
import shm
import math as m
import numpy as n
import scipy as s
import datetime
import time

#

PMAP_GRID_DIM = 125
PMAP_GRID_SCALE = 0.1
TIME_DECAY_COEFFICIENT = 0.01
SIGMA = 0.682689492137086

#

def log(t, s): print '[' + str(datetime.datetime.now()) + '] {ASLAM} (' + t + ') ' + s
ctime = lambda: time.time()

toAUVd = lambda v: (v[0]**2 + v[1]**2)**(1/2)
toAUVh = lambda v:  m.degrees(-m.atan2(v[1], v[0])) % 360
toAUV = lambda v: n.array([toAUVd(v), toAUVh(v)])
#fromAUV = lambda v: n.array([v[0]*m.cos(-m.radians(v[1])), v[0]*m.sin(-m.radians(v[1]))])

vehicle_pos = lambda: n.array([shm.kalman.north.get() + 12.5, -shm.kalman.east.get() + 12.5])

mag = lambda v: (v[0]**2 + v[1]**2) ** (1/2)

# observations ~ functions
gaussian = lambda mu, sigma, x: m.e ** (-(x-mu)**2 / (2*sigma**2))
hobs = lambda h, e, s: lambda p: gaussian(h, e, toAUVh(p-s))
dobs = lambda d, e, s: lambda p: gaussian(d, e, toAUVd(p-s))
pobs = lambda d, e, s: lambda p: gaussian(0., e, mag(p-(s+d)))
#iobs = lambda i, e: lambda p: gaussian(0., e, mag(p-i))
def iobs(i, e):
    def f(p):
        return gaussian(0., e, mag(p-i))
    return f

class Event:
    def __init__(s, f, tdc=TIME_DECAY_COEFFICIENT):
        s.tdc = tdc
        s.vals = n.ones((PMAP_GRID_DIM, PMAP_GRID_DIM))
        for x in range(PMAP_GRID_DIM):
            for y in range(PMAP_GRID_DIM):
                s.vals[x,y] *= f(n.array([x * PMAP_GRID_SCALE, y * PMAP_GRID_SCALE]))
        s.vals /= n.nansum(s.vals)
        s.t = ctime()
    def weighted(s, t):
        return s.vals ** (m.e ** (s.tdc * (s.t - t)))

class Object:
    def __init__(s, f=None):
        s.events = []
        if f is not None: s.events.append(Event(f))
        else: s.events.append(Event(lambda v: 1.))
    def _pmap(s, t):
        pmap = s.events[0].vals
        for e in s.events[1:]:
            data = e.weighted(t)
            if data.sum() == 0:
                s.events.remove(e)
            else: 
                pmap *= data
                pmap /= pmap.sum()
        return pmap / pmap.sum()
    def centroiderr(s, t):
        pmap = s._pmap(t)
        centx, centy = 0., 0.
        for x in range(PMAP_GRID_DIM):
            for y in range(PMAP_GRID_DIM):
                centx += pmap[x][y] * x
                centy += pmap[x][y] * y
        centx *= PMAP_GRID_SCALE
        centy *= PMAP_GRID_SCALE 
        spts = sorted([(x, y, mag((centx - x, centy - y))) for x in range(PMAP_GRID_DIM) for y in range(PMAP_GRID_DIM)], key=lambda t:t[2])
        psum = 0.
        i = -1
        while psum < SIGMA:
            i += 1
            psum += pmap[spts[i][0]][spts[i][1]]

        return ((centx, centy), spts[i][2])
    def centroid(s, t):
        pmap = s._pmap(t)
        centx, centy = 0., 0.
        for x in range(PMAP_GRID_DIM):
            for y in range(PMAP_GRID_DIM):
                centx += pmap[x][y] * x
                centy += pmap[x][y] * y
        centx *= PMAP_GRID_SCALE
        centy *= PMAP_GRID_SCALE
        return (centx, centy)
