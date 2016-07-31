from math import e, pi, sqrt, atan, degrees

def gaussian(mean, stddev):
    return lambda x: (e ** ( - (x - mean)**2 / (2 * stddev**2))) / (sqrt(2*pi)*stddev)

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

class DefaultDict:
    d = dict()
    def __init__(s, d):
        s.d = d
    def __getitem__(s, i):
        if i in s.d.keys(): return s.d.get(i)
        else: return None
