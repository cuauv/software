#!/usr/bin/env python3

import math
import cv2

import shm
from mission.constants.config import wire as constants
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
        options.DoubleOption('min_aspect_ratio', 6, 1, 40),
        options.BoolOption('debugging', constants.debugging),
        options.BoolOption('lab a', True),
        options.BoolOption('lab b', False),
        options.BoolOption('ycrcb cb', False),
        options.BoolOption('yellow_debug', False),
        options.IntOption('yellow_hls_h_min', 58, 0, 255),
        options.IntOption('yellow_hls_h_max', 100, 0, 255),
        options.IntOption('yellow_ycrcb_cb_min', 111, 0, 255),
        options.IntOption('yellow_ycrcb_cb_max', 145, 0, 255),
        options.IntOption('yellow_lab_a_min', 0, 0, 255),
        options.IntOption('yellow_lab_a_max', 101, 0, 255),
        options.IntOption('yellow_lab_b_min', 95, 0, 255),
        options.IntOption('yellow_lab_b_max', 152, 0, 255)
]

DOWNSCALE_RATIO = .33

def tag(mat, text, pos):
    cv2.putText(mat, text, pos, cv2.FONT_HERSHEY_DUPLEX, 1, (255, 50, 255), thickness=2)

class Bar:
    def __init__(self, contour):
        min_rect = cv2.boxPoints(cv2.minAreaRect(contour))
        dist1 = self.distance(min_rect[1], min_rect[0]) *int(1 / DOWNSCALE_RATIO)
        dist2 = self.distance(min_rect[1], min_rect[2]) *int(1 / DOWNSCALE_RATIO)
        self.area = dist1 * dist2

        if dist1 < dist2:
            self.aspect_ratio = dist2 / dist1
            self.end1 = self.avg_pt(min_rect[1], min_rect[0]) * int(1 / DOWNSCALE_RATIO)
            self.end2 = self.avg_pt(min_rect[2], min_rect[3]) * int(1 / DOWNSCALE_RATIO)
        else:
            self.aspect_ratio = dist1 / dist2
            self.end1 = self.avg_pt(min_rect[1], min_rect[2]) * int(1 / DOWNSCALE_RATIO)
            self.end2 = self.avg_pt(min_rect[0], min_rect[3]) *int(1 / DOWNSCALE_RATIO)


    def distance(self, p1, p2):
        return (((p1[0] - p2[0]) ** 2 + (p1[1] - p2[1]) ** 2) ** 0.5) * int(1 / DOWNSCALE_RATIO)

    def avg_pt(self, p1, p2):
        return ((p1[0] + p2[0]) / 2, (p1[1] + p2[1]) / 2) * int(1 / DOWNSCALE_RATIO)

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

class Wire(ModuleBase):
    def process(self, mat):
        results = shm.wire_results.get()
        camera_w, camera_h = mat.shape[:2]

        mat = cv2.resize(mat, (int(mat.shape[1] * DOWNSCALE_RATIO), int(mat.shape[0] * DOWNSCALE_RATIO)))

        self.post('original', mat)

        lab = cv2.cvtColor(mat, cv2.COLOR_RGB2LAB)
        ycrcb = cv2.cvtColor(mat, cv2.COLOR_RGB2YCR_CB)
        hls = cv2.cvtColor(mat, cv2.COLOR_RGB2HLS)
        hls_split = cv2.split(hls)
        lab_split = cv2.split(lab)
        ycrcb_split = cv2.split(ycrcb)
        lab_a = cv2.split(lab)[1]
        lab_b = cv2.split(lab)[2]
        ycrcb_cb = cv2.split(ycrcb)[2]


        if self.options['debugging']:
            self.post('lab a', lab_a)
            self.post('lab b', lab_b)
            self.post('ycrcb cb', ycrcb_cb)

        if self.options['lab a']:
            threshed = cv2.adaptiveThreshold(
                lab_a, 255,
                cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV,
                self.options['adaptive_thresh_block_size'] * 2 + 1,
                self.options['adaptive_thresh_c'],
            )
        elif self.options['lab b']:
            threshed = cv2.adaptiveThreshold(
                lab_b, 255,
                cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV,
                self.options['adaptive_thresh_block_size'] * 2 + 1,
                self.options['adaptive_thresh_c'],
            )
        elif self.options['ycrcb cb']:
            threshed = cv2.adaptiveThreshold(
                ycrcb_cb, 255,
                cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY,
                self.options['adaptive_thresh_block_size'] * 2 + 1,
                self.options['adaptive_thresh_c'],
            )
        else:
            yellow_a_threshed = cv2.inRange(lab_split[1], self.options['yellow_lab_a_min'],
                                                   self.options['yellow_lab_a_max'])
            if self.options['yellow_debug']:
                self.post('yellow_a_threshed', yellow_a_threshed)

            yellow_b_threshed = cv2.inRange(lab_split[2], self.options['yellow_lab_b_min'],
                                                   self.options['yellow_lab_b_max'])
            if self.options['yellow_debug']:
                self.post('yellow_b_threshed', yellow_b_threshed)

            yellow_cb_threshed = cv2.inRange(ycrcb_split[2], self.options['yellow_ycrcb_cb_min'],
                                                      self.options['yellow_ycrcb_cb_max'])
            if self.options['yellow_debug']:
                self.post('yellow_cb_threshed', yellow_cb_threshed)

            yellow_h_threshed = cv2.inRange(hls_split[0], self.options['yellow_hls_h_min'],
                                                   self.options['yellow_hls_h_max'])
            if self.options['yellow_debug']:
                self.post('yellow_h_threshed', yellow_h_threshed)

            threshed = yellow_a_threshed & yellow_b_threshed & yellow_h_threshed & yellow_cb_threshed

        if self.options['debugging']:
            self.post('threshed', threshed)


        blurred = cv2.medianBlur(threshed, self.options['kernel_size'] * 2 + 1)
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
                    '{}_x1'.format(loc): bar.end1[0] *int(1 / DOWNSCALE_RATIO),
                    '{}_y1'.format(loc): bar.end1[1] *int(1 / DOWNSCALE_RATIO),
                    '{}_x2'.format(loc): bar.end2[0] *int(1 / DOWNSCALE_RATIO),
                    '{}_y2'.format(loc): bar.end2[1]*int(1 / DOWNSCALE_RATIO),
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
        shm.wire_results.set(results)

if __name__ == '__main__':
    Wire('forward', options=options)()
