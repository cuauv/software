#!/usr/bin/env auv-python2

import numpy as np

p_dvl = np.poly1d([-4.70324060e-22, -1.48344261e-18, 3.00715028e-15, -2.08496981e-12, 7.37936110e-10, -1.46799049e-07, 1.65189693e-05, -1.00401898e-03, 3.47378787e-02, -2.07774276e-01, 1.83824651e+00])

def linearize(x):
    return p_dvl(x)
