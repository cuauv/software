import time

import cv2

from vision.modules import ModuleBase
import gui_options
import buoy_common


capture_source = 'forward'

vision_options = [gui_options.IntOption('l_min', 39, 0, 255), gui_options.IntOption('l_max', 152, 0, 255),
                  gui_options.IntOption('a_min', 45, 0, 255), gui_options.IntOption('a_max', 107, 0, 255),
                  gui_options.IntOption('b_min', 128, 0, 255), gui_options.IntOption('b_max', 184, 0, 255),
                  gui_options.IntOption('min_area', 200),
                  gui_options.IntOption('blur_size', 11, 1, 255, lambda x: x % 2 == 1),
                  gui_options.FloatOption('min_circularity', .35, 0, 150), gui_options.FloatOption('heuristicPower', 5),
                  gui_options.FloatOption('dead_band', 1.5), gui_options.BooleanOption('debugging', True)]


class GreenBuoy(ModuleBase.ModuleBase):
    def __init__(self):
        super(GreenBuoy, self).__init__(True)
        self.vsp_data = None
        self.times = []

    def process(self, mat):
        self.times.insert(0, time.time())
        while time.time() - self.times[-1] > 5:
            self.times.pop()
        print('fps: {}, blur_size: {}'.format(len(self.times) / 5., self.options["blur_size"]))

        self.post('orig', mat)

        lab_image = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        lab_split = cv2.split(lab_image)

        buoy_common.find_buoy('green', self.post, self.options, lab_split, mat)
