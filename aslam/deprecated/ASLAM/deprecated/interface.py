from classes import *
from util import *
import shm
import layout.delayed as delayed
import pickle
import os

places = ['start', 'buoys', 'orange_buoy', 'yellow_buoy', 'green_buoy', 'led_buoy', 'wire', 'pipe_to_emperor', 'pipe_to_bins', 'torpedoes', 'pipe_bins_to_emperor', 'pipe_to_wire', 'bins', 'emperor']

def toDistance(n, e): return (n**2 + e**2)**(1./2)

class Layout:
    def __init__(s, filename):
        path_to_maps = os.path.join(os.environ['PYTHONPATH'], "mission", "layout", "maps")
        path_to_map = os.path.join(path_to_maps, filename)
        f = open(path_to_map, 'rb')
        s.positions = pickle.load(f)


    def set_start_position(s, n, e, place):
        s.state = State(positions[place][0] - n, positions[place][1] - e, 1.)
        for p in [x for x in places if x is not 'start']: s.state.addObject(p, s.positions[p][0], s.positions[p][1])

    def update_position(s, place, n, e, sigma)
        s.state.dObs("sub", place, toDistance(n, e), sigma)

    def update_position_from_heading(s, place, n, e, heading, sigma):
        s.state.hObs("sub", place, heading, sigma)

    def get_NE(s, place):
        return s.state.getNE(place)

    def get_distance(s, start, end):
        return delayed.DelayedDistance(start, end, s)

    def get_heading(s, start, end):
        return delayed.DelayedHeading(start, end, s)

    def get_position(s, target):
        return delayed.DelayedPosition(target, s)

