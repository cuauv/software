import time
from collections import namedtuple
import math

import cv2
import shm
import numpy as np

from vision.modules import ModuleBase
from vision import options


capture_source = 'forward'

options = [options.IntOption('lab_b_min',  41, (0, 255)),
           options.IntOption('lab_b_max', 128, (0, 255)),
           options.IntOption('yuv_v_min', 127, (0, 255)),
           options.IntOption('yuv_v_max', 210, (0, 255)),
           options.IntOption('hls_h_min',  73, (0, 255)),
           options.IntOption('hls_h_max', 128, (0, 255)),
           options.IntOption('min_area', 200),
           options.IntOption('blur_size',  11, (1, 255), lambda x: x % 2 == 1),
           options.DoubleOption('min_circularity', .35, (0, 150)),
           options.DoubleOption('heuristicPower', 15),
           options.IntOption('ideal_height', 510, (0, 1020)),
           options.BoolOption('debugging', True)]


class RedBuoy(ModuleBase.ModuleBase):
    def __init__(self):
        super(RedBuoy, self).__init__(options, True)
        self.vsp_data = None
        self.times = []


    def process(self, mat):
        self.times.insert(0, time.time())
        while time.time() - self.times[-1] > 5:
            self.times.pop()
            #print('fps: {}, blur_size: {}'.format(len(self.times) / 5., self.options["blur_size"]))

        self.post('orig', mat)
        results = shm.red_buoy_results

        lab_image = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        lab_split = cv2.split(lab_image)
        yuv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2YUV)
        yuv_split = cv2.split(yuv_image)
        hls_image = cv2.cvtColor(mat, cv2.COLOR_BGR2HLS)
        hls_split = cv2.split(hls_image)

        lab_bthreshed = cv2.inRange(lab_split[2], self.options["lab_b_min"], self.options["lab_b_max"])
        yuv_vthreshed = cv2.inRange(yuv_split[2], self.options["yuv_v_min"], self.options["yuv_v_max"])
        hls_hthreshed = cv2.inRange(hls_split[0], self.options["hls_h_min"], self.options["hls_h_max"])

        finalThreshed = lab_bthreshed & yuv_vthreshed & hls_hthreshed

        blurred = cv2.medianBlur(finalThreshed, self.options["blur_size"])

        if self.options["debugging"]:
            self.post('lab b Threshed', lab_bthreshed)
            self.post('yuv v Threshed', yuv_vthreshed)
            self.post('hls h Threshed', hls_hthreshed)
            self.post("Threshed", finalThreshed)
            self.post("Masked", cv2.bitwise_and(mat, mat, mask=finalThreshed))
            self.post("Blurred", blurred.copy())

        _, contours, hierarchy = cv2.findContours(blurred, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

        if hierarchy is None:
            print('Cannoy find any buoy contours')
            results.probability.set(0)
            return

        if self.options['debugging']:
            allContoursDrawing = np.copy(mat)
            cv2.drawContours(allContoursDrawing, [c for c in contours if cv2.contourArea(c)], -1, (255, 255, 0), 2)
            self.post("All contours", allContoursDrawing)

        contour_info = namedtuple("contour_info", ["contour", "area", "center", "radius", "heuristic_score", "circularity"])
        contour_data = []

        hierarchy = hierarchy[0]

        outermost_contour, outermost_index = hierarchy[0], 0
        while outermost_contour[3] >= 0:
            outermost_contour, outermost_index = hierarchy[outermost_contour[3]], outermost_contour[3]
        while outermost_contour[1] >= 0:
            outermost_contour, outermost_index = hierarchy[outermost_contour[1]], outermost_contour[1]

        outer_contours = [outermost_index]

        while outermost_contour[0] >= 0:
            outermost_contour, outermost_index = hierarchy[outermost_contour[0]], outermost_contour[0]
            outer_contours.append(outermost_index)


        for c in (x for (i, x) in enumerate(contours) if i in outer_contours):
            area = cv2.contourArea(c)
            if area < self.options['min_area']:
                continue

            center, radius = cv2.minEnclosingCircle(c)
            circularity = area / (math.pi * radius ** 2)
            ideal_height = self.options['ideal_height']
            x = contour_info(c, area, center, radius, area * (circularity ** self.options['heuristicPower']) * ((700-abs(700 - center[1])) / 700 + 1), circularity)

            contour_data.append(x)

        if len(contour_data) > 0:
            best = max(contour_data, key=lambda x: x.heuristic_score)
            center = tuple(map(int, best.center))
            radius = int(best.radius)

            if self.options['debugging']:
                img = np.copy(mat)
                cv2.circle(img, center, radius, 0xFF0000, 2)
                self.post('final', img)

            # Output values to shm!
            results.center_x.set(center[0])
            results.center_y.set(center[1])
            results.area.set(math.pi * radius ** 2)
            results.heuristic_score.set(best.heuristic_score)
            results.probability.set(best.circularity)
        else:
            results.heuristic_score.set(0)
            results.probability.set(0)
