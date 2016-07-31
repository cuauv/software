from time import time
from math import sin, cos, radians, degrees, pi, e, sqrt

def curUT():
    return time()

def gaussian(mean, stddev):
    return lambda x: (e ** ( - (x - mean)**2 / (2 * stddev**2))) / (sqrt(2*pi)*stddev)

def toCartesian(r, t): # wrt north
    n = r * cos(radians(t))
    e = r * sin(radians(t))
    return (n, e)

def toPolar(dx, dy): # with respect to north
    dist = sqrt(dx**2 + dy**2)
    if dx == 0. and dy > 0: angle = 0.
    elif dx == 0. and dy < 0: angle = pi
    else: 
        try: angle = -atan(dy / dx) - pi/2
        except:
            if dy > 0: angle = 0.
            else: angle = pi
    if dx > 0 and dy >= 0: angle = angle % pi
    elif dx > 0 and dy < 0: angle = angle % pi
    elif dx < 0 and dy <= 0: angle = angle % (2*pi)
    elif dx < 0 and dy > 0: angle = angle % (2*pi)
    return (dist, degrees(angle))


