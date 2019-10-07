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



module_options = [
        options.IntOption('gaussian_kernel', 2, 1, 40),
        options.IntOption('gaussian_stdev', 10, 0, 40),
        options.IntOption('min_area', 100, 1, 2000),
        options.DoubleOption('min_circularity', 0.4, 0, 1),
]



class RandomPinger(ModuleBase):
    last_run = 0

    def process(self, img):
        curr_time = time.time()
        print(curr_time - self.last_run)
        if curr_time - self.last_run < .2:
            print("skipping")
            # self.last_run = curr_time
            return

        self.last_run = curr_time

        img = img[::2, ::2, :]
        # uimg = cv2.UMat(img)
        h, w, _ = img.shape

        shm.camera.downward_height.set(h)
        shm.camera.downward_width.set(w)

        # self.post("Original", img)

        luv = cv2.cvtColor(img, cv2.COLOR_BGR2LUV)
        (luv_l, luv_u, luv_v) = cv2.split(luv)

        lab = cv2.cvtColor(img, cv2.COLOR_BGR2LAB)
        (lab_l, lab_a, lab_b) = cv2.split(lab)

        k_size = self.options["gaussian_kernel"]
        k_std = self.options["gaussian_stdev"]
        blurred = cv2.GaussianBlur(lab_l, (k_size * 2 + 1, k_size * 2 + 1), k_std, k_std)

        p1 = 70
        canny = cv2.Canny(blurred, p1, p1 / 2)
        # self.post("Canny", canny)

        circles = cv2.HoughCircles(blurred, cv2.HOUGH_GRADIENT, 1, 50, param1=p1, param2=50, minRadius=15, maxRadius=175)


        final = img.copy()

        found = False

        if circles is not None:
            for circle in circles[0, :]:
                if circle[2]:
                    found = True
                print(circle[2])
                # draw the outer circle
                cv2.circle(final, (circle[0], circle[1]), circle[2], (0, 255, 0), 2)
                # draw the center of the circle
                cv2.circle(final, (circle[0], circle[1]), 2, (0, 0, 255), 3)


        found_img = img.copy()
        found_img[:, :] = (COLORS["RED"], COLORS["GREEN"])[found]
        self.post("found", found_img)


        self.post("Final", final)


if __name__ == '__main__':
    RandomPinger("downward", module_options)()
