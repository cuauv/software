#!/usr/bin/env python

from classes import *
from util import *
import shm
#import delayed
import layout.delayed as delayed

places = ['start', 'buoys', 'orange_buoy', 'yellow_buoy', 'green_buoy', 'led_buoy', 'wire', 'pipe_to_emperor', 'pipe_to_bins', 'torpedoes', 'pipe_bins_to_emperor', 'pipe_to_wire', 'bins', 'emperor']

class Layout:
    def __init__(s, filename):
        import os
        path_to_maps = os.path.join(os.environ['PYTHONPATH'], "mission", "layout", "maps")
        path_to_map = os.path.join(path_to_maps, filename)
        import pickle
        f = open(path_to_map, 'rb')
        s.positions = pickle.load(f)
        s.objects = {}
        for p in places: s.objects[p] = Object(s.positions[p], 0.5)
        s.l = Localizer()
        
    def set_start_position(s, n, e, place):
        s.objects['sub'] = Object((s.objects[place].centroid[0] - n, s.objects[place].centroid[1] - e), 0.5)
        s.lastn, s.laste = 0., 0.

    def update_position(s, place, n, e, sigma):
        sn, se = shm.kalman.north.get(), shm.kalman.east.get()
        s.l.update(movements = [Movement(s.objects['sub'], (sn - s.lastn, se - s.laste), 0.5, None)])
        s.l.update(observations = [Observation(s.objects['sub'], s.objects[place], toPolar(n, e)[0], sigma, toPolar(n, e)[1], sigma, None)]) # sigma to heading
        s.lastn, s.laste = sn, se

    def update_position_from_heading(s, place, n, e, heading, sigma):
        sn, se = shm.kalman.north.get(), shm.kalman.east.get()
        s.l.update(movements = [Movement(s.objects['sub'], (shm.kalman.north.get() - s.lastn, shm.kalman.east.get() - s.laste), 0.5, None)])
        s.l.update(observations = [Observation(Object((s.objects['sub'].centroid[0] + n, s.objects['sub'].centroid[1] + e), 0.5), s.objects[place], None, None, heading, 1, None)]) # sigma
        s.lastn, s.laste = s.objects['sub'].centroid
    
    def get_NE(s, place):
        delt = s.l.getDelta(s.objects['sub'], s.objects[place])
        print('delta {0}'.format(delt))
        return delt[0], delt[1]

    def get_distance(s, start, end):
        return delayed.DelayedDistance(start, end, s)

    def get_heading(s, start, end):
        return delayed.DelayedHeading(start, end, s)

    def get_position(s, target):
        return delayed.DelayedPosition(target, s)

