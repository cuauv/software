import numpy as n
from time import time
from util import gaussian, toPolar, DefaultDict
import math

# Constants
ARRAY_SIZE = 200 #500
ARRAY_SCALE = 0.1
UPDATE_FLOOR = 0. #0.1 / ARRAY_SIZE**2 # may need work

class Object: 
    def __init__(s, centroid, stddev):
        # Probability map, centroid, stddev all in N-E coordinates from origin at lower right
        s.pmap = n.ndarray((ARRAY_SIZE, ARRAY_SIZE), buffer=n.ones(ARRAY_SIZE**2))
        s.centroid, s.stddev = centroid, stddev
        s.update(lambda n, e: gaussian(0, s.stddev)(toPolar(n - s.centroid[0], e - s.centroid[1])[0]))

    def update(s, fxn):
        tcentn, tcente, pmaps = 0., 0., 0.
        for r in range(len(s.pmap)):
            for c in range(len(s.pmap[0])):
                s.pmap[r][c] = s.pmap[r][c] * fxn(r*ARRAY_SCALE, c*ARRAY_SCALE)
                pmaps += s.pmap[r][c]
        s.pmap /= pmaps
        for r in range(len(s.pmap)):
            for c in range(len(s.pmap[0])):
                tcentn += r * ARRAY_SCALE * s.pmap[r][c]
                tcente += c * ARRAY_SCALE * s.pmap[r][c]
        s.centroid = (tcentn, tcente)
        #print(s.centroid)
        s.stddev = 0. # TODO

class Observation:
    def __init__(s, objfrom, objto, dist, distunc, heading, headingunc, callerinfo):
        s.objfrom, s.objto, s.dist, s.distunc, s.heading, s.headingunc, s.timestamp, s.callerinfo = objfrom, objto, dist, distunc, heading, headingunc, time(), callerinfo
    def bHeading(s, n, e):
        return gaussian(s.heading, s.headingunc)(toPolar(n - s.objfrom.centroid[0], e - s.objfrom.centroid[1])[1]) + UPDATE_FLOOR
    def bDistance(s, n, e):
        return gaussian(s.dist, s.distunc)(toPolar(n - s.objfrom.centroid[0], e - s.objfrom.centroid[1])[0]) + UPDATE_FLOOR

class Movement:
    def __init__(s, obj, deltapos, unc, callerinfo):
        s.obj, s.deltapos, s.unc, s.timestamp, s.callerinfo = obj, deltapos, unc, time(), callerinfo
    def execute(s):
        tpmap = n.ndarray((ARRAY_SIZE, ARRAY_SIZE), buffer=n.zeros(ARRAY_SIZE**2))
        ncentroid = (s.obj.centroid[0] + s.deltapos[0], s.obj.centroid[1] + s.deltapos[1])
        tpmaps = 0.
        '''
        # Full Gaussian redistribution. 500^4 iterations --> too slow. Optimize, C, OpenCL?
        for r in range(len(s.obj.pmap)):
            for c in range(len(s.obj.pmap[0])):
                ar = int(round(r - s.deltapos[0] / ARRAY_SCALE))
                ac = int(round(c - s.deltapos[1] / ARRAY_SCALE))
                for tr in range(ARRAY_SIZE):
                    for tc in range(ARRAY_SIZE):
                        tpmap[tr][tc] += gaussian(0, s.unc)(toPolar((tr - ar)*ARRAY_SCALE, (tc - ac)*ARRAY_SCALE)[0]) * s.obj.pmap[ar][ac]
                tpmaps += tpmap[tr][tc]
                print('rc {0}:{1}'.format(r, c))
        '''
        # Hacky version. Shifts array, then estimates other points as Gaussian from new centroid.
        for r in range(ARRAY_SIZE):
            for c in range(ARRAY_SIZE):
                ar = int(round(r - s.deltapos[0] / ARRAY_SCALE))
                ac = int(round(c - s.deltapos[1] / ARRAY_SCALE))
                try: tpmap[r][c] = s.obj.pmap[ar][ac]
                except: tpmap[r][c] = gaussian(0, s.obj.stddev)(toPolar(ncentroid[0] - r*ARRAY_SCALE, ncentroid[1] - c*ARRAY_SCALE)[0])
                tpmaps += tpmap[r][c]

        tpmap /= tpmaps
        s.obj.pmap = tpmap
        s.obj.update(lambda n, e: 1.)

class Localizer:
    def update(s, **kwargs):
        kwargs = DefaultDict(kwargs)
        if kwargs['observations'] and kwargs['movements']: return False
        else:
            if kwargs['observations']:
                for o in kwargs['observations']: 
                    if o.heading: o.objto.update(o.bHeading)
                    if o.dist: o.objto.update(o.bDistance)
            elif kwargs['movements']:
                for m in kwargs['movements']: m.execute()

    def getDelta(s, objfrom, objto):
        return (objto.centroid[0] - objfrom.centroid[0], objto.centroid[1] - objfrom.centroid[1])
