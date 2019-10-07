#!/usr/bin/env python3
from vision.modules.base import ModuleBase
from vision import options
from vision.stdlib import *
import shm
import math
import time
import cv2 as cv2
import numpy as np
from collections import namedtuple

from cash_in_shared import *



module_options = get_shared_options(is_forward=False) + [
]



class CashInDownward(ModuleBase):
    last_run = 0

    def process(self, img):
        curr_time = time.time()
        if curr_time - self.last_run < shm.vision_module_settings.time_between_frames.get():
            print("skipping")
            # self.last_run = curr_time
            return
        self.last_run = curr_time

        img = img[::2, ::2, :]
        # uimg = cv2.UMat(img)
        h, w, _ = img.shape

        shm.camera.downward_height.set(h)
        shm.camera.downward_width.set(w)

        self.post("Original", img)

        set_shared_globals(is_forward=False, options=self.options, post=self.post, img=img)

        preprocessed_image = preprocess(img)
        threshed = threshold(preprocessed_image)
        # contours = find_contours(threshed)
        # bins = find_bins(threshed, contours)
        bins = find_bins(threshed)

        final = img.copy()

        for binn in bins:
            # shm_group = shm._eval("recovery_vision_downward_{}".format(name))
            # output = shm_group.get()

            # output.area = binn.area
            # output.center_x = binn.x
            # output.center_y = binn.y
            # output.probability = binn.probability

            # shm_group.set(output)

            cv2.circle(final, (int(binn.x), int(binn.y)), int(binn.area), COLORS["BLUE"], 5)
            # cv2.putText(final, name, (int(binn.x), int(binn.y) - 20), cv2.FONT_HERSHEY_SIMPLEX, 1, COLORS["BLUE"], 2)

        self.post("Final", final)


if __name__ == '__main__':
    CashInDownward("downward", module_options)()
