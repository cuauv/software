#!/usr/bin/env python3

import shm
from vision.modules.base import ModuleBase
from vision.modules import buoy_common
from vision import options

options = [options.IntOption('hls_h_min', 63, 0, 255),
           options.IntOption('hls_h_max', 207, 0, 255),
           options.IntOption('lab_a_min', 123, 0, 255),
           options.IntOption('lab_a_max', 167, 0, 255),
           options.IntOption('lab_b_min', 87, 0, 255),
           options.IntOption('lab_b_max', 142, 0, 255),
           options.IntOption('min_area', 100, 0, 1000000),
           options.IntOption('blur_size', 4, 1, 50),
           options.IntOption('min_heuristic_score', 0, 0, 1000),
           options.DoubleOption('min_circularity', 0.5, 0, 1),
           options.BoolOption('verbose', False)
          ]

class RedBuoy(ModuleBase):
    def process(self, mat):
        buoy_common.process(self, mat, shm.red_buoy_results)

if __name__ == '__main__':
    RedBuoy('forward', options)()
