#!/usr/bin/env python3

import shm
from vision.modules import buoy_common
from vision.modules.base import ModuleBase
from vision import options

options = [options.IntOption('hls_h_min', 85, 0, 255),
           options.IntOption('hls_h_max', 111, 0, 255),
           options.IntOption('lab_a_min', 64, 0, 255),
           options.IntOption('lab_a_max', 121, 0, 255),
           options.IntOption('lab_b_min', 106, 0, 255),
           options.IntOption('lab_b_max', 123, 0, 255),
           options.IntOption('min_area', 100, 0, 1000000),
           options.IntOption('blur_size', 4, 1, 50),
           options.IntOption('min_heuristic_score', 0, 0, 1000),
           options.DoubleOption('min_circularity', 0.1, 0, 1),
           options.BoolOption('verbose', False)
          ]

class YellowBuoy(ModuleBase):
    def process(self, mat):
        buoy_common.process(self, mat, shm.yellow_buoy_results)

if __name__ == '__main__':
    YellowBuoy('forward', options)()
