#!/usr/bin/env python3

import math
import cv2

import shm
from mission.constants.config import navigate as constants
from vision.modules.base import ModuleBase
from vision import options

options = [
        options.IntOption('adaptive_thresh_block_size', constants.block_size, 1, 2500),
        options.IntOption('adaptive_thresh_c', constants.thresh_c, 0, 20),

        # options.IntOption('cmyk_y_min', 20, 0, 255),
        # options.IntOption('cmyk_y_max', 255, 0, 255),
        # options.IntOption('hsv_h_min', 18, 0, 255),
        # options.IntOption('hsv_h_max', 59, 0, 255),
        # options.IntOption('hsv_s_min', 0, 0, 255),
        # options.IntOption('hsv_s_max', 255, 0, 255),
        # options.IntOption('hsv_v_min', 0, 0, 255),
        # options.IntOption('hsv_v_max', 255, 0, 255),

        options.DoubleOption('min_area', constants.min_area, 0, 1000),
        options.IntOption('kernel_size', constants.kernel_size, 1, 255),
        options.DoubleOption('min_aspect_ratio', 20, 1, 40),
        options.BoolOption('debugging', constants.debugging)
]

def tag(mat, text, pos):
    cv2.putText(mat, text, pos, cv2.FONT_HERSHEY_DUPLEX, 1, (255, 50, 255), thickness=2)

class Bar:
    def __init__(self, contour):
        min_rect = cv2.boxPoints(cv2.minAreaRect(contour))
        dist1 = self.distance(min_rect[1], min_rect[0])
        dist2 = self.distance(min_rect[1], min_rect[2])
        self.area = dist1 * dist2

        if dist1 < dist2:
            self.aspect_ratio = dist2 / dist1
            self.end1 = self.avg_pt(min_rect[1], min_rect[0])
            self.end2 = self.avg_pt(min_rect[2], min_rect[3])
        else:
            self.aspect_ratio = dist1 / dist2
            self.end1 = self.avg_pt(min_rect[1], min_rect[2])
            self.end2 = self.avg_pt(min_rect[0], min_rect[3])

    def distance(self, p1, p2):
        return ((p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2) ** 0.5

    def avg_pt(self, p1, p2):
        return ((p1[0] + p2[0]) / 2, (p1[1] + p2[1]) / 2)

    def is_horiz(self):
        ratio = abs((self.end1[1] - self.end2[1]) / (self.end1[0] - self.end2[0]))
        return math.atan(ratio) < math.pi / 4

    def center(self):
        return self.avg_pt(self.end1, self.end2)

    def draw(self, mat, label):
        color = (255, 255, 0) if self.is_horiz() else (127, 255, 127)
        int_pt = lambda pt: (int(pt[0]), int(pt[1]))
        cv2.line(mat, int_pt(self.end1), int_pt(self.end2), color, thickness=3)

        tag(mat, label, int_pt(self.center()))

def vision_experimentation():
    """ Experimenting with alternate colorspaces and different combinations of
    adaptive and hard thresholding.
    """
    # def bgr_to_cmyk_y(mat):
        # norm = np.float32(mat) / 255
        # b = cv2.split(norm)[0]
        # k = 1 - np.max(norm, axis=2)
        # cmyk_y = np.uint8((-b - k + 1) / (-k + 1.0001) * 255)
        # return cmyk_y

    # cmyk_y = bgr_to_cmyk_y(mat)
    # if self.options['debugging']:
        # self.post('cmyk_y', cmyk_y)
    # cmyk_y_threshed = cv2.inRange(cmyk_y, self.options['cmyk_y_min'], self.options['cmyk_y_max'])
    # if self.options['debugging']:
        # self.post('cmyk_y_threshed', cmyk_y_threshed)
    # cmyk_y_adap = cv2.adaptiveThreshold(
        # cmyk_y, 255,
        # cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY,
        # self.options['adaptive_thresh_block_size'] * 2 + 1,
        # self.options['adaptive_thresh_c'],
    # )
    # if self.options['debugging']:
        # self.post('cmyk_y_adapt', cmyk_y_adap)

    # bgr_b = cv2.split(mat)[0]
    # if self.options['debugging']:
        # self.post('bgr_b', bgr_b)
    # bgr_b_threshed = cv2.adaptiveThreshold(
        # bgr_b, 255,
        # cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY,
        # self.options['adaptive_thresh_block_size'] * 2 + 1,
        # self.options['adaptive_thresh_c'],
    # )
    # if self.options['debugging']:
        # self.post('bgr_b_threshed', bgr_b_threshed)


    # hsv = cv2.cvtColor(mat, cv2.COLOR_BGR2HSV)
    # hsv_h, hsv_s, hsv_v = cv2.split(hsv)
    # hsv_h_threshed = cv2.inRange(hsv_h, self.options['hsv_h_min'], self.options['hsv_h_max'])
    # hsv_s_threshed = cv2.inRange(hsv_s, self.options['hsv_s_min'], self.options['hsv_s_max'])
    # hsv_v_threshed = cv2.inRange(hsv_v, self.options['hsv_v_min'], self.options['hsv_v_max'])
    # hsv_threshed = cv2.bitwise_and(hsv_h_threshed, hsv_s_threshed, mask=hsv_v_threshed)

    # if self.options['debugging']:
        # self.post('h threshed', hsv_h_threshed)
        # self.post('s threshed', hsv_s_threshed)
        # self.post('v threshed', hsv_v_threshed)
        # self.post('hsv threshed', hsv_threshed)

    pass

class Navigate(ModuleBase):
    def process(self, mat):
        results = shm.navigate_results.get()
        camera_w, camera_h = mat.shape[:2]

        self.post('original', mat)

        lab = cv2.cvtColor(mat, cv2.COLOR_RGB2LAB)
        lab_a = cv2.split(lab)[1]
        if self.options['debugging']:
            self.post('lab a', lab_a)
        lab_a_threshed = cv2.adaptiveThreshold(
            lab_a, 255,
            cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV,
            self.options['adaptive_thresh_block_size'] * 2 + 1,
            self.options['adaptive_thresh_c'],
        )
        if self.options['debugging']:
            self.post('lab a threshed', lab_a_threshed)

        # Erode/dilate with both horiz and vert kernels to avoid erasing the
        # thin navigate bars
        # kernel_size = self.options['kernel_size'] * 2 + 1
        # horiz_kernel = np.ones((1, kernel_size), np.uint8)
        # vert_kernel = np.ones((kernel_size, 1), np.uint8)
        # horiz_opened = cv2.morphologyEx(lab_a_threshed, cv2.MORPH_OPEN, horiz_kernel)
        # vert_opened = cv2.morphologyEx(lab_a_threshed, cv2.MORPH_OPEN, vert_kernel)
        # opened = cv2.bitwise_or(horiz_opened, vert_opened)
        # if self.options['debugging']:
            # self.post('horiz opened', horiz_opened)
            # self.post('vert opened', vert_opened)
            # self.post('opened', opened)

        blurred = cv2.medianBlur(lab_a_threshed, self.options['kernel_size'] * 2 + 1)
        if self.options['debugging']:
            self.post('blurred', blurred)
        _, contours, hierarchy = cv2.findContours(blurred.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        if self.options['debugging']:
            contours_mat = mat.copy()
            for c in contours:
                cv2.drawContours(contours_mat, c, -1, (255, 255, 0), thickness=2)
            self.post('contours', contours_mat)

        bars = []
        for c in contours:
            bar = Bar(c)
            if bar.area >= self.options['min_area'] and \
                    bar.aspect_ratio >= self.options['min_aspect_ratio']:
                bars.append(bar)
        bars = sorted(bars, key=lambda x: -x.area)

        # Identify left, right, and/or horizontal bars
        hbar, vbar1, vbar2 = None, None, None
        for bar in bars[:3]:
            if bar.is_horiz():
                if hbar is None:
                    hbar = bar

            else:
                if vbar1 is None:
                    vbar1 = bar
                elif vbar2 is None:
                    vbar2 = bar

        def set_results(bar, loc):
            vars = None
            if bar is not None:
                vars = {
                    '{}_x1'.format(loc): bar.end1[0],
                    '{}_y1'.format(loc): bar.end1[1],
                    '{}_x2'.format(loc): bar.end2[0],
                    '{}_y2'.format(loc): bar.end2[1],
                    '{}_area'.format(loc): bar.area,
                    '{}_prob'.format(loc): 1,
                }
            else:
                vars = {'{}_prob'.format(loc): 0}

            results.update(**vars)

        if hbar is not None:
            # Align vertical bars relative to horizontal bar
            left_bar, right_bar = None, None

            for bar in [vbar1, vbar2]:
                if bar is not None:
                    if bar.center()[0] < hbar.center()[0]:
                        left_bar = bar
                    else:
                        right_bar = bar
            vbar1, vbar2 = left_bar, right_bar

        else:
            # Align vertical bars relative to each other
            if vbar1 is not None:
                if vbar2 is not None:
                    # Orient two bars
                    if vbar1.center()[0] > vbar2.center()[0]:
                        vbar1, vbar2 = vbar2, vbar1
                elif vbar1.center()[0] > camera_w / 2:
                    # Move lone bar to side of camera that it's on
                    vbar1, vbar2 = vbar2, vbar1

        bars_mat = mat.copy()
        if hbar is not None: hbar.draw(bars_mat, 'bottom bar')
        if vbar1 is not None: vbar1.draw(bars_mat, 'left bar')
        if vbar2 is not None: vbar2.draw(bars_mat, 'right bar')
        self.post('barlines', bars_mat)

        # Set bar locations, if found, to shm
        set_results(hbar, 'bottom')
        set_results(vbar1, 'left')
        set_results(vbar2, 'right')
        self.fill_single_camera_direction(results)
        shm.navigate_results.set(results)

if __name__ == '__main__':
    Navigate(options=options)()
