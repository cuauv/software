#!/usr/bin/env python3

# Calculates a hyper-ellipsoid approximation of the forces and torques that the
# AUV can exert. 
# Also outputs thruster limits.

import math
import numpy as np
from control.thruster_manager import ThrusterManager
import shm

# average magnitude
def amag(pair):
    return 0.5 * (math.fabs(pair[0]) + math.fabs(pair[1]))

# calculate returns a pair (a, b) of vectors.
# The elements of a are the radii of a six-dimensional hyper-ellipsoid,
# corresponding to the averaged magnitudes of the limits of the forces that the
# AUV can exert along the X, Y, and Z axes, followed by the averaged magnitudes
# of the limits of the torques that the AUV can exert on the same axes.  The
# hyper-ellipsoid is intended to be used for fast approximate clamping of AUV
# control wrenches. The actual control process consists of a bunch of linear
# algebra and optimization that is relatively heavy on the CPU. This is
# appropriate and necessary for realtime control.For simulations, however, where
# we don't even need to decide on power outputs to physical thrusters, we assume
# that taking the desires wrench as-is and then clamping as necessary is good
# enough.
# The elements of b are the 2-tuples output by tm.max_forces, with the same
# ordering of forces/torques as in b.
def calculate():
    tm = ThrusterManager()

    # tm.max_forces(axis, torquep) returns the force/torque along the given
    # axis as [min, max]. We average |min| and |max| so we can treat it as
    # the major axis length in that direction.

    # Set depth to well below surface of the water so that thruster maximums are computed
    # without any artificial throttling based on depth
    shm.kalman.depth.set(2.0)

    fx = tm.max_forces(np.array([1, 0, 0]), torque=False)
    fy = tm.max_forces(np.array([0, 1, 0]), torque=False)
    fz = tm.max_forces(np.array([0, 0, 1]), torque=False)

    tx = tm.max_forces(np.array([1, 0, 0]), torque=True)
    ty = tm.max_forces(np.array([0, 1, 0]), torque=True)
    tz = tm.max_forces(np.array([0, 0, 1]), torque=True)

    fxa = amag(fx)
    fya = amag(fy)
    fza = amag(fz)

    txa = amag(tx)
    tya = amag(ty)
    tza = amag(tz)

    return ([fxa, fya, fza, txa, tya, tza], [fx, fy, fz, tx, ty, tz])

if __name__ == "__main__":
    result = calculate()
    # radii of hyper-ellipsoid
    print("[" + ", ".join([str(x) for x in result[0]]) + "]")
    # screw minimums
    print("[" + ", ".join([str(x[0]) for x in result[1]]) + "]")
    # screw maximums
    print("[" + ", ".join([str(x[1]) for x in result[1]]) + "]")
