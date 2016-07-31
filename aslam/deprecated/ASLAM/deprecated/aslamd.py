#!/usr/bin/env python2

from aslam import *
import math as m
import numpy as n
import scipy.stats as ss
import pickle

import matplotlib.pyplot as plt
import matplotlib.cm as cm

# with respect to object uncertainty
HEADING_THRESH = 2
DISTANCE_THRESH = 2

INIT_ERROR = 1.0

class State:
    def __init__(s):
        s.objects = {'sub': Object(iobs(vehicle_pos()), INIT_ERROR))}
        s.initpos = n.array(s.objects['sub'].centroid(ctime()))
        s.delta = lambda: vehicle_pos() - s.initpos
        s.derrs, s.herrs = [], []
        s.covariances = {}
        s.curpos = lambda t: n.array(s.objects['sub'].centroiderr(t)[0]) + s.delta()
    def add_object(s, oid, i_n, i_e, i_err):
        if oid in s.objects: return False
        s.objects[oid] = Object(iobs(n.array([i_n, i_e]), i_err))
    def _obs(s, time, obj, f, ef=None):
        cerr_before = s.objects[obj].centroiderr(time)
        if not ef(cerr_before): return False
        s.objects[obj].events.append(Event(f))
        cerr_after = s.objects[obj].centroiderr(time)
        # covariances
    def dObs(s, obj, d, err):
        time = ctime()
        pos = s.curpos(time)
        def ef(ce):
            op = n.array(ce[0])
            expd = toAUVd(op - pos)
            if abs(expd - d) > (DISTANCE_THRESH * ce[1]): return False # TODO: Technically, should be directionalized.
            s.derrs.append((d, expd))
            return True
        s._obs(time, obj, hobs(d, err, pos, ef))
    def hObs(s, obj, h, err): 
        time = ctime()
        pos = s.curpos(time)    
        def ef(ce):
            op = n.array(ce[0])
            expd, exph = toAUV(op - pos)
            if m.radians(abs(exph - h)) * expd > (HEADING_THRESH * ce[1]): return False # TODO: Technically, should be directionalized.
            s.herrs.append((h, expd))
            return True
        s._obs(time, obj, dobs(d, err, pos, ef))
    def dErr(s):
        return ss.linregress([x[0] for x in s.derrs], [x[1] for x in s.derrs])
    def hErr(s):
        return ss.linregress([x[0] for x in s.herrs], [x[1] for x in s.derrs])
    def getHD(s, obj):
        time = ctime()
        delta = s.objects[obj].centroid(time) - s.curpos(time)
        return toAUV(delta)
    def getRP(s, obj):
        time = ctime()
        return s.objects[obj].centroid(time) - s.curpos(time)
    def getP(s, o):
        time = ctime()
        return s.objects[o].centroid(time)
    def getPM(s, o):
        time = ctime()
        return s.objects[o]._pmap(time)
    def disp(s):
        plt.clf()
        time = ctime()
        SF = 1
        uniq = [[0. for y in range(0, PMAP_GRID_DIM, SF)] for x in range(0, PMAP_GRID_DIM, SF)]
        for o in s.objects.values():
            pmap = o._pmap(time)
            for x in range(0, PMAP_GRID_DIM/SF):
                for y in range(0, PMAP_GRID_DIM/SF):
                    uniq[x][y] = max(uniq[x][y], pmap[x*SF][y*SF]) 
        uniq = [(x, y, uniq[x][y]) for x in range(0, PMAP_GRID_DIM/SF) for y in range(0, PMAP_GRID_DIM/SF)]
        plt.scatter([x[0] for x in uniq], [x[1] for x in uniq], c=[x[2] for x in uniq], cmap=cm.coolwarm)
        plt.show()
    
if __name__ == '__main__':
    l = pickle.load(open('../mission/layout/maps/tagged_teagle_alt_mirrored.layout', 'r'))
    s = State()
    for o in l: 
        s.add_object(o, l[o][0], l[o][1], 1.)
    s.disp()
