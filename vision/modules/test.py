#!/usr/bin/env python3

import cv2
import numpy as np

from vision import options
from vision.modules.base import ModuleBase

opts = [
    options.IntOption('lab_l_min', 51, 0, 255),
    options.IntOption('lab_l_max', 136, 0, 255),
    options.IntOption('lab_a_min', 86, 0, 255),
    options.IntOption('lab_a_max', 117, 0, 255),
    options.IntOption('lab_b_min', 115, 0, 255),
    options.IntOption('lab_b_max', 152, 0, 255),
    options.IntOption('brightness', 127, 0, 255),
    options.DoubleOption('min_area', 100, 0, 500),
    options.DoubleOption('max_arclength', 4, 0, 500),
]

class Test(ModuleBase):
    def process(self, mat):
        o = lambda name: self.options[name]

        self.post('original', mat)

        lab = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        l_a_b = cv2.split(lab)

        threshed_chans = []
        for i, chan in enumerate('lab'):
            min = o('lab_{}_min'.format(chan))
            max = o('lab_{}_max'.format(chan))
            threshed = cv2.inRange(l_a_b[i], min, max)
            threshed[threshed > 0] = o('brightness')

            threshed_chans.append(threshed)
            self.post('threshed_{}'.format(chan), threshed)

        final_threshed = cv2.bitwise_and(
            threshed_chans[0],
            threshed_chans[1],
            mask=threshed_chans[2],
        )
        self.post('threshed', final_threshed)

        morphed = cv2.morphologyEx(final_threshed, cv2.MORPH_OPEN, np.ones((5, 5)))

        _, contours, heierarchy = cv2.findContours(morphed.copy(), cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        orig_contours = mat.copy()
        cv2.drawContours(orig_contours, contours, -1, (127, 255, 0))
        self.post('orig_contours', orig_contours)

        if len(contours) > 0:
            scored_contours = [cv2.contourArea(c) / cv2.arcLength(c, True) for c in contours]
            sorted_pairs = sorted(zip(contours, scored_contours), key=lambda x: -x[1])
            sorted_contours = list(zip(*sorted_pairs))[0]
            rect_bounds = [(cv2.boundingRect(c)) for c in contours]

            cv2.drawContours(mat, sorted_contours[:9], -1, (0, 255, 255))
            self.post('good_contours', mat)

if __name__ == '__main__':
    Test('forward', opts)()
