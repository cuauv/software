#!/usr/bin/env python3
import time
import shm
import ctypes

from conf.vehicle import cameras

from vision.modules.base import ModuleBase, _PsuedoOptionsDict
from vision import options

directions = cameras.keys()

opts = []

DEFAULT_DOUBLE_MAX = 100.0
DEFAULT_DOUBLE_MIN = 0.0
DEFAULT_INT_MAX = 50
DEFAULT_INT_MIN = 0

def build_opts():
    for o, t in shm.camera_calibration._fields:
        print(o)
        if t == ctypes.c_double:
            opts.append(options.DoubleOption(o,
                                             getattr(shm.camera_calibration, o).get(),
                                             DEFAULT_DOUBLE_MIN, DEFAULT_DOUBLE_MAX))
        elif t == ctypes.c_int:
            opts.append(options.IntOption(o,
                                          getattr(shm.camera_calibration, o).get(),
                                          DEFAULT_INT_MIN, DEFAULT_INT_MAX))
    return opts


class Calibrate(ModuleBase):
    def __init__(self, directions):
        super().__init__(directions, build_opts())

        self.prev = {}

    def process(self, *mats):
        for o, t in shm.camera_calibration._fields:
            opt_val = self.options[o]
            if not o in self.prev or not opt_val == self.prev[o]:
                getattr(shm.camera_calibration, o).set(opt_val)
                self.prev[o] = opt_val

        for d, m in zip(directions, mats):
            self.post(d, m)


if __name__ == '__main__':
    Calibrate(directions)()
