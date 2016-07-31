GRIDSIZE = 50
GRIDSCALE = 0.1
TDCOEFF = 0.001
AUTOREJFRAC = 0.3

import numpy as n

from aux import toPolar, toCartesian, gaussian, curUT

from math import pi

class Event:
    def __init__(s, euf, eut, ndim = 2):
        s.euf, s.eut = euf, eut # Event Update Function, Event Unix Timestamp
        #s.eum = n.fromfunction(euf, (GRIDSIZE,)*ndim) # Event Update Matrix
        s.eum = n.ones((GRIDSIZE, GRIDSIZE))
        for x in range(len(s.eum)):
            for y in range(len(s.eum[0])):
                s.eum[x][y] = euf(x, y)
    def update(pmap, eut):
        print 'EUP'
        taeum = s.eum * e ** (- TDCOEFF * (cut - s.eut)) # Exponential Time Decay
        npmap = pmap * taeum
        return npmap / npmap.sum()

class Object:
    def __init__(s, size = GRIDSIZE, scale = GRIDSCALE, ndim = 2):
        s.size, s.scale, s.ndim = size, scale, ndim
        s.pmap = n.ones((GRIDSIZE, GRIDSIZE))
        s.centroid, s.error = None, None
        s.events = []
        s.covariances = {} # format: object => covariance

    def recalc(s):
        s.pmap = n.ones((s.size,)*s.ndim)
        for e in s.events: s.pmap = e.update(s.pmap, curUT())
        s.rcCentroid()
    
    def rcCentroid(s):
        # TODO: Revise centroiding algorithm.
        centroid = n.zeros(s.ndim)
        for ind in n.ndindex((s.size,)*s.ndim):     
            for i in range(s.ndim):
                centroid[i] += s.pmap[ind] * s.scale * ind[i]
        s.centroid = centroid
        s.error = None

class State:
    def __init__(s, iN, iNerr, iE, iEerr):
        sub = Object()
        euf = lambda n, e: gaussian(iN, iNerr)(n) * gaussian(iE, iEerr)(e)
        sub.pmap = n.fromfunction(euf, (GRIDSIZE, GRIDSIZE))
        sub.pmap /= sub.pmap.sum()
        sub.centroid = (iN, iE)
        s.objects = {'sub': sub}
        s.derr = Object(100, 0.01, 1)
        s.herr = Object(100, 0.1, 1)
    def doCovariance(s, obj, deltan, deltae, error):
        for o, c in obj.covariances.iteritems():
            iposn, ipose = s.objects[o].centroid
            uposn, upose = iposn + deltan, ipose + deltae
            cuf = lambda n, e: (gaussian(uposn, error)(n) * gaussian(upose, error)(e))**c
            obj.events.append(Event(uf, curUT(), 2))
    
    def hObs(s, obj, heading, err):
        print 'called'
        initn, inite = s.objects[obj].centroid
        subn, sube = s.objects['sub'].centroid
        prevd = toPolar(initn - subn, inite - sube)[0]
        newn, newe = toCartesian(prevd, heading)
        deltan, deltae = newn - initn, newe - inite
        expHeading = toPolar(initn - subn, inite - sube)[1]

        # Input rejection.
        #if abs(heading - expHeading) / expHeading > AUTOREJFRAC: return False

        # Bayesian update.
        print s.objects['sub'].centroid
        uf = lambda n, e: gaussian(heading, err)(toPolar(n - s.objects['sub'].centroid[0], e - s.objects['sub'].centroid[1])[0])
        
        print 'adding...', s.objects[obj].events
        s.objects[obj].events.append(Event(uf, curUT()))
        print s.objects[obj].events
    
        # Reverse Bayesian error estimation.
        #huf = lambda he: gaussian(heading - expHeading, err)(he)
        #s.herr.events.append(Event(huf, curUT(), 1))

        # Covariance
        s.doCovariance(s.objects[obj], deltan, deltae, prevd * err * pi/180) # Approximate conversion of heading error to position error using sin(x) ~ x in radians.

    def dObs(s, obj, distance, err):
        initn, inite = s.objects[obj].centroid
        subn, sube = s.objects['sub'].centroid
        prevh = toPolar(initn - subn, inite - sube)[1]
        newn, newe = toCartesian(distance, prevh)
        deltan, deltae = newn - initn, newe - inite
        expDistance = toPolar(initn - subn, inite - sube)[0]

        # Input rejection.
        if abs(distance - expDistance) > AUTOREJFRAC: return False

        # Bayesian update.
        uf = lambda n, e: gaussian(distance, err)(toPolar(n - s.objects['sub'].centroid[0], e - s.objects['sub'].centroid[0])[0])
        s.objects[obj].events.append(Event(uf, curUT(), 2))
        
        # Reverse Bayesian error estimation.
        duf = lambda de: gaussian(distance / expDistance, err / expDistance)(de)
        s.derr.events.append(Event(duf, curUT(), 1))
        
        # Covariance
        s.doCovariance(s.objects[obj], deltan, deltae, err) # Distance error is close enough to position error.
    
    def move(s, dN, dE, err):
        tpmap = n.ones((GRIDSIZE, GRIDSIZE))
        ncentroid = (s.objects['sub'].centroid[0] + dN, s.objects['sub'].centroid[1] + dE)
        for r in range(GRIDSIZE):
            for c in range(GRIDSIZE):
                ar = int(round(r - dN / GRIDSCALE))
                ac = int(round(c - dE / GRIDSCALE))
                try: tpmap[r][c] = s.objects['sub'].pmap[ar][ac]
                except: tpmap[r][c] = gaussian(0, s.objects['sub'].error)(toPolar(ncentroid[0] - r*GRIDSCALE, ncentroid[1] - c*GRIDSCALE)[0])
        tpmap /= tpmap.sum()
        s.objects['sub'].pmap = tpmap
        s.objects['sub'].rcCentroid()

    def getEHErr(s):
        s.herr.rcCentroid()
        return s.herr.centroid
    def getEDErr(s):
        s.derr.rcCentroid()
        return s.derr.centroid
    def getHeading(obj):
        return toPolar(obj.centroid[0] - objects['sub'].centroid[0], obj.centroid[1] - objects['sub'].centroid[1])[1]
    def getDistance(obj):
        return toPolar(obj.centroid[0] - objects['sub'].centroid[0], obj.centroid[1] - objects['sub'].centroid[1])[0]
    def update(s):
        for k in s.objects.keys(): 
            if k is not 'sub': s.objects[k].recalc(); s.objects[k].rcCentroid()
