import numpy as n
from time import time
from math import e, pi, sqrt, atan, degrees

ARRAY_SIZE = 5000
ARRAY_SCALE = 0.01

def gaussian(mean, stddev):
    return lambda x: (math.e ** ( - (x - mean)**2 / (2 * stddev**2))) / (sqrt(2*pi)*stddev)

def toPolar(dx, dy): # with respect to north
    dist = sqrt(dx**2 + dy**2)
    if dx == 0 and dy > 0: angle = 0.
    elif dx == 0 and dy < 0: angle = pi
    else: angle = -atan(dy / dx) - pi/2
    if dx > 0 and dy >= 0: angle = angle % pi
    elif dx > 0 and dy < 0: angle = angle % pi
    elif dx < 0 and dy <= 0: angle = angle % (2*pi)
    elif dx < 0 and dy > 0: angle = angle % (2*pi)
    return (dist, degrees(angle))

class DefaultDict:
    d = dict()
    def __init__(s, d):
        s.d = d
    def __getitem__(s, i):
        if i in s.d.keys(): return s.d.get(i)
        else: return None

class ProbabilityMap:
    m = n.ndarray((ARRAY_SIZE, ARRAY_SIZE), buffer=n.zeros(ARRAY_SIZE**2))
    def __getitem__(s, i):
        return s.m[i]
    def __init__(s, r, c, stddev):
        # Generate map
        pass
    def update(s, fxn):
        for r in len(pm):
            for c in len(pm[0]):
                s.m[r][c] = s.m[r][c] * fxn(r, c)
        s.m /= n.nansum(pm)
    def getSD():
        pass
        # Calculate standard deviation of map
    def getCentroid():
        pass

class Object:
    pm = ProbabilityMap(0, 0, 0)
       
class Observation:
    objfrom, objto, dist, distunc, heading, headingunc, timestamp, callerinfo = None, None, None, None, None, None, None, None
    def __init__(s, objfrom, objto, dist, distunc, heading, headingunc, callerinfo):
        s.objfrom, s.objto, s.dist, s.distunc, s.heading, s.headingunc, s.timestamp, s.callerinfo = objfrom, objto, dist, distunc, heading, headingunc, time(), callerinfo
    def bHeading(s, r, c):
        centroid = objfrom.getCentroid()
        return gaussian(heading, headingunc)(toPolar(c - centroid[1], r - centroid[0])[1])
    def bDistance(s, r, c):
        centroid = objfrom.getCentroid()
        return gaussian(distance, distanceunc)(toPolar(c - centroid[1], r - centroid[0])[0])

class Movement:
    obj, deltapos, unc, timestamp, callerinfo = None, None, None, None, None
    def __init__(s, obj, deltapos, unc, callerinfo):
        s.obj, s.deltapos, s.unc, s.timestamp, s.callerinfo = obj, deltapos, unc, time(), callerinfo
    def bMovement(s, r, c):
        pass

class Localizer:
    objects = []
    def update(**kwargs):
        kwargs = DefaultDict(kwargs)
        if kwargs['observations'] and kwargs['movements']: return False
        else:
            if kwargs['observations']:
                for o in kwargs['observations']: pass # TODO
            elif kwargs['movements']:
                for m in kwargs['movements']: m.obj.pm.update(m.bMovement)

    def getDelta(objfrom, objto):
        # TODO
