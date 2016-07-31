#!/usr/bin/env python

from ctypes import *
laslam = CDLL("libaslam.so")

class State:
    def __init__(s, n, e, err):
        s.sptr = laslam.s_new(c_double(n), c_double(e), c_double(err))
    def addObject(s, oid, n, e, err):
        laslam.s_addObject(s.sptr, c_char_p(str.encode(oid)), c_double(n), c_double(e), c_double(err))
    def hObs(s, o1id, o2id, h, err):
        laslam.s_hObs(s.sptr, c_char_p(str.encode(o1id)), c_char_p(str.encode(o2id)), c_double(h), c_double(err))
    def dObs(s, o1id, o2id, d, err):
        laslam.s_dObs(s.sptr, c_char_p(str.encode(o1id)), c_char_p(str.encode(o2id)), c_double(d), c_double(err))
    def move(s, dN, dE, err):
        laslam.s_move(s.sptr, c_double(dN), c_double(dE), c_double(err))
    def getHeading(s, o1id, o2id):
        f = laslam.s_getHeading
        f.restype = c_double
        return f(s.sptr, c_char_p(str.encode(o1id)), c_char_p(str.encode(o2id)))
    def getDistance(s, o1id, o2id):
        f = laslam.s_getDistance
        f.restype = c_double
        return f(s.sptr, c_char_p(str.encode(o1id)), c_char_p(str.encode(o2id)))
    def getNE(s, o1id, o2id):
        f = laslam.s_getN
        f.restype = c_double
        n = f(s.sptr, c_char_p(str.encode(o1id)), c_char_p(str.encode(o2id)))
        f = laslam.s_getE
        f.restype = c_double
        e = f(s.sptr, c_char_p(str.encode(o1id)), c_char_p(str.encode(o2id)))
        return (n, e)
    def update(s):
        laslam.s_update(s.sptr)

if __name__ == '__main__':
    s = State(25., 25., 2.)
    s.addObject("pipe", 30., 30., 1.)
    s.update()
    print(s.getHeading("sub", "pipe"))
    print(s.getDistance("sub", "pipe"))
    s.move(5., 5., 0.5)
    s.update()
    print(s.getHeading("sub", "pipe"))
    print(s.getDistance("sub", "pipe"))
    print(s.getNE("sub", "pipe"))
    
