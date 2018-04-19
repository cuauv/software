#!/usr/bin/env python2

import timeit
import numpy as np
import random
from control.optimizer import Optimizer

N = 100
o = Optimizer()
o.mapper = o.tm.thrusts_to_outputs()
thrusts = np.array([8.45, 1.12, -0.15, -12.2, 6.4, 4.4])
desires = np.array([123, 45, -13, -31.123, 31, 90])
def timed():
    o.objective(thrusts, desires)

if __name__ == "__main__":
    print("Value of objective function: %f" % o.objective(thrusts, desires))
    t = timeit.timeit("timed()", setup="from __main__ import timed", number=N)
    print("Time for %d calls: %f" % (N, t))

