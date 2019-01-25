# -*- coding: utf-8 -*-
from math import sqrt, acos, pi
import numpy as np
'''
Implementation of Discrete Contour Evolution algorithm (Longin Jan Latecki, Rolf Lakämper, Convexity
Rule for Shape Decomposition Based on Discrete Contour Evolution, In Computer Vision and Image
Understanding, Volume 73, Issue 3, 1999, Pages 441-454, ISSN 1077-3142,
https://doi.org/10.1006/cviu.1998.0738.).
Almost a direct port of the existing implementation in MATLAB.
For the most part, naming choices and comments are replicated.
'''

def evolution(slist, number, max_value=None, keep_endpoints=False, process_until_convex=False, display=False):
    # EVOLUTION(slist, number,<maxvalue>,<keepEndpoints>,<processUntilConvex><display>)
    #  discrete curve evolution of slist to n points
    # input: slist, number of vertices in resulting list
    #        optional: maxvalue: stop criterion,if >0.0 value overrides number
    #        optional: keepEndpoints. if set to 1, endpoints are NOT deleted
    #        optional: processUntilConvex. if set to 1, evolution continues until
    #                  shape is convex
    #        optional: display: displayflag
    # output: s=simplificated list
    #         value= vector containing values of remaining vertices in point-order
    #         delval= vector containing values of deleted vertices in order of deletion


    # this function is not speed-optimized, it is near a simple brute force
    # implementation !

    # blocking of vertices is taken into account.

    if max_value is not None and max_value > 0:
        number = 3
    else:
        max_value = float("inf")

    if process_until_convex:
        number = 3
        max_value = float("inf")

    del_val = np.array([])
    s = slist
    # initialize value vector (containing the information value of each vertex)
    value = np.zeros((len(s), 1))

    if number < 3 or len(slist) <= 3:
        print("WARNING (evolution): less than 3 vertices")
        return (s, value, del_val)

    peri = poly_perimeter(np.append(slist, [slist[0]], axis=0))

    for i in range(len(s)):
        value[i] = relevance(s, i, peri, keep_endpoints)

    m = -1

    while True:
        if display:
            print("Value of deleted vertex: {}".format(m))

        if process_until_convex and is_convex(s):
            break

        if number >= len(s):
            break

        i = np.argmin(value)
        m = value[i][0]
        if m > max_value:
            break

        # test blocking
        if m > 0:
            bf = blocked(s, i)

            # if vertex is blocked, find first non-blocked vertex
            # this procedure is separated from the 'usual min-case'
            # for speed-reasons (sort is needed instead of min)
            if bf:
                ind = np.argsort(value, axis=0)
                rel = value[ind]
                j = 2
                m = 1e16
                while j < len(s):
                    i = ind[j][0]
                    bf = blocked(s, i)
                    if not bf:
                        m = rel[j][0][0]
                        break
                    j += 1

                if m > max_value:
                    break

        # delete vertex
        px = s[i, 0]
        py = s[i, 1]
        s = np.delete(s, i, axis=0)
        value = np.delete(value, i, axis=0)

        del_val = np.append(del_val, [m], axis=0)

        # neighboring vertices
        i0 = i - 1
        i1 = i
        if i0 < 0:
            i0 = len(s) - 1
        elif i1 >= len(s):
            i1 = 0

        value[i0] = relevance(s, i0, peri, keep_endpoints)
        value[i1] = relevance(s, i1, peri, keep_endpoints)
    return s, value, del_val

def relevance(s, index, peri, keep_endpoints):
    if keep_endpoints:
        if index == 0 or index == len(s[:, 0]) - 1:
            return float("inf")
    # vertices
    i0 = index - 1
    i1 = index
    i2 = index + 1
    if i0 < 0:
        i0 = len(s) - 1
    elif i2 >= len(s):
        i2 = 0

    # segments
    seg1x = s[i1, 0] - s[i0, 0]
    seg1y = s[i1, 1] - s[i0, 1]
    seg2x = s[i1, 0] - s[i2, 0]
    seg2y = s[i1, 1] - s[i2, 1]

    l1 = sqrt(seg1x * seg1x + seg1y * seg1y)
    l2 = sqrt(seg2x * seg2x + seg2y * seg2y)

    # turning angle (0-180)
    a = 180 - acos((seg1x * seg2x + seg1y * seg2y) / l1 / l2) * 180 / pi

    # relevance measure
    v = a * l1 * l2 / (l1 + l2)

    v = v / peri # normalize
    return v

def seglength(p1x, p1y, p2x, p2y):
    dx = p2x - p1x
    dy = p2y - p1y
    l = sqrt(dx * dx + dy * dy)
    return l

def blocked(s, i):
    # find neighbouring vertices
    i0 = i - 1
    i1 = i + 1
    if i0 < 0:
        i0 = len(s) - 1
    elif i1 >= len(s):
        i1 = 0

    # bounding box
    minx = min([s[i0, 0], s[i, 0], s[i1, 0]])
    miny = min([s[i0, 1], s[i, 1], s[i1, 1]])
    maxx = max([s[i0, 0], s[i, 0], s[i1, 0]])
    maxy = max([s[i0, 1], s[i, 1], s[i1, 1]])

    # check if any boundary-vertex is inside bounding box
    # first create index-set v=s\(i0,i,i1)

    if i0 < i1:
        k = [i1, i, i0]
    elif i0 > i:
        k = [i0, i1, i]
    elif i1 < i:
        k = [i, i0, i1]

    v = np.arange(len(s))
    v = np.delete(v, k[0], axis=0)
    v = np.delete(v, k[1], axis=0)
    v = np.delete(v, k[2], axis=0)

    for k in range(len(v)):
        px = s[v[k], 0]
        py = s[v[k], 1]

        # vertex px, py inside boundary-box?
        b = False
        if not (px < minx or py < miny or px > maxx or py > maxy):
            # inside, now test triangle
            a = s[i, :] - s[i0, :] # a = i0 to i
            b = s[i1, :] - s[i, :] # b = i to i1
            c = s[i0, :] - s[i1, :] # c = i1 to i0

            e0 = s[i0, :] - np.array([px, py])
            e1 = s[i, :] - np.array([px, py])
            e2 = s[i1, :] - np.array([px, py])

            d0 = np.linalg.det(np.append([a], [e0], axis=0))
            d1 = np.linalg.det(np.append([b], [e1], axis=0))
            d2 = np.linalg.det(np.append([c], [e2], axis=0))

            # INSIDE?
            b = (d0 > 0 and d1 > 0 and d2 > 0) or (d0 < 0 and d1 < 0 and d2 < 0)

        if b:
            break
    return b

# check if shape is convex (or concave)
def is_convex(s):
    con = True
    if len(s[:, 0]) < 4:
        return con
    # get direction of first curve
    for i in range(1, len(s[:, 0]) - 1):
        curv = curvature_direction(s, i)
        if curv != 0:
            break

    # check if there's a curve oppositely directed
    for j in range(i+1, len(s[:, 0] - 1)):
        curv1 = curvature_direction(s, j)
        if curv1 != 0 and curv1 != curv:
            con = False
            break
    return con

def curvature_direction(s, i):
    a = s[i-1, :]
    b = s[i, :]
    c = s[i+1, :]

    d1 = b - a
    d2 = c - b

    d = np.sign(np.linalg.det(np.append(d1, d2, axis=0)))
    return d

def poly_perimeter(s):
    return np.sum(np.sqrt(np.diff(s[:, 0]) ** 2 + np.diff(s[:, 1]) ** 2))

if __name__ == "__main__":
    slist = np.array([
    [6.0000,    5.8000],
    [8.4189,    5.8000],
   [10.8378,    5.8000],
   [12.8425,    6.8000],
   [15.2614,    6.8000],
   [17.6803,    6.8000],
   [19.7773,    7.5773],
   [22.1039,    7.8000],
   [24.1086,    6.8000],
   [26.5275,    6.8000],
   [28.9464,    6.8000],
   [30.9511,    7.8000],
   [33.2616,    8.0616],
   [35.3747,    8.8000],
   [37.3794,    9.8000],
   [39.7983,    9.8000],
   [42.1536,    9.6464],
   [43.8640,    7.9360],
   [46.1602,    7.9602],
   [48.2313,    8.8000],
   [50.4597,    9.2597],
   [52.6548,    9.8000],
   [54.6595,   10.8000],
   [57.0555,   10.8555],
   [59.0831,   11.8000],
   [61.0878,   12.8000],
   [63.3583,   13.1583],
   [65.3616,   13.4384],
   [67.1019,   11.8000],
   [69.5208,   11.8000],
   [71.9397,   11.8000],
   [73.9444,   12.8000],
   [75.9640,   13.7640],
   [78.3680,   13.8000],
   [80.3727,   14.8000],
   [82.5597,   15.3597],
   [84.7963,   15.8000],
   [86.8593,   16.6593],
   [89.1555,   16.9555],
   [91.1588,   17.9588],
   [92.2000,   19.9464],
   [93.2000,   21.9511],
   [93.2000,   24.3700],
   [94.2000,   26.3747],
   [94.7611,   28.5611],
   [95.2000,   30.7983],
   [94.0657,   32.2000],
   [91.6468,   32.2000],
   [89.2279,   32.2000],
   [86.8090,   32.2000],
   [84.8616,   31.0616],
   [82.7996,   30.2000],
   [80.5621,   29.7621],
   [78.5587,   28.7587],
   [76.3713,   28.2000],
   [74.3666,   27.2000],
   [73.2000,   28.9209],
   [70.9430,   29.2000],
   [68.6635,   28.8635],
   [66.6602,   27.8602],
   [64.5147,   27.2000],
   [62.5100,   26.2000],
   [60.5053,   25.2000],
   [58.3540,   24.5540],
   [56.4960,   23.2000],
   [54.3474,   22.5474],
   [52.0724,   22.2000],
   [50.0479,   21.2479],
   [48.0445,   20.2445],
   [46.2000,   21.2245],
   [45.6394,   23.2000],
   [43.2205,   23.2000],
   [41.2158,   22.2000],
   [39.2111,   21.2000],
   [37.1460,   20.3460],
   [35.1426,   19.3426],
   [33.1393,   18.3393],
   [30.8431,   18.0431],
   [28.7734,   17.2000],
   [26.7687,   18.2000],
   [24.5403,   17.7403],
   [22.5370,   16.7370],
   [20.7547,   15.2000],
   [18.7500,   14.2000],
   [16.7453,   13.2000],
   [14.7406,   12.2000],
   [12.7359,   11.2000],
   [10.8099,   10.0099],
    [8.7265,    9.2000],
    [6.8033,    8.0033]
    ])

    number = 50
    max_value = 1.5
    keep_endpoints = False
    process_until_convex = False
    display = False
    print(evolution(slist, number, max_value, keep_endpoints, process_until_convex, display))
