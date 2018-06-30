#!/usr/bin/env python3
from vision.modules.base import ModuleBase
from vision import options
import shm
import math
from enum import Enum
import cv2 as cv2
import numpy as np
from scipy.spatial.distance import pdist, squareform

black_left = False
gate_shm_group = shm.bicolor_gate_vision

# SHM:
#  - general, black, and red centers
#  - confidence

module_options = [
    options.BoolOption('debug', True),
    options.IntOption('erode_size', 1, 1, 40),
    options.IntOption('gaussian_kernel', 1, 1, 40),
    options.IntOption('gaussian_stdev', 2, 0, 40),
    options.IntOption('thresh_size', 90, 1, 100),
    options.IntOption('min_area', 700, 1, 2000),
    options.IntOption('center_dist', 100, 1, 2000),
    options.IntOption('luv_l_thresh_min', 1, 1, 254),
    options.IntOption('luv_l_thresh_max', 76, 1, 254),
    options.IntOption('hough_votes', 700, 100, 1000000),
    options.IntOption('rho', 7, 1, 100),
    options.IntOption('bin_dist_threshold', 50, 0, 1000),
    options.IntOption('min_length', 50, 1, 100000),
    options.IntOption('max_gap', 100, 1, 100000),
]


WHITE = (255, 255, 255)
RED = (0, 0, 255)
BLUE = (255, 0, 0)
GREEN = (0, 255, 0)
ORANGE = (255, 165, 0)
CYAN = (0, 255, 255)
MAGENTA = (255, 0, 255)
YELLOW = (255, 255, 0)
BLACK = (0, 0, 0)

def get_kernel(size):
    return cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (size * 2 + 1, size * 2 + 1), (size, size))

def clean(img, erode_size):
    kernel = get_kernel(erode_size)

    erode2 = cv2.erode(img, kernel)
    dilate2 = cv2.dilate(erode2, kernel)

    return dilate2

def thresh(img, kernel, stdev, size, thresh_type, c):
    blurred = cv2.GaussianBlur(img, (kernel * 2 + 1, kernel * 2 + 1), stdev, stdev)
    return cv2.adaptiveThreshold(blurred, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, thresh_type, size * 2 + 1, c)

class BicolorGate(ModuleBase):
    def process(self, img):
        debug = self.options['debug']
        height, width, channels = img.shape

        if debug:
            self.post('raw', img)

        if debug:
            self.post('rotated', img)

        # We use the L from LUV and the S from HLS
        (luv_l, luv_u, luv_v) = cv2.split(cv2.cvtColor(img, cv2.COLOR_BGR2LUV))
        (hls_h, hls_l, hls_s) = cv2.split(cv2.cvtColor(img, cv2.COLOR_BGR2HLS))
        (ycrcb_y, ycrcb_cr, ycrcb_cb) = cv2.split(cv2.cvtColor(img, cv2.COLOR_BGR2YCR_CB))

        g_b_k = self.options['gaussian_kernel']
        g_b_sd = self.options['gaussian_stdev']
        a_t_s = self.options['thresh_size']

        erode_size = self.options['erode_size']

        # Thresholds
        luv_u_thresh = clean(thresh(luv_u, g_b_k, g_b_sd, a_t_s, cv2.THRESH_BINARY, -1), erode_size)
        hls_h_thresh = clean(thresh(hls_h + 180, g_b_k, g_b_sd, a_t_s, cv2.THRESH_BINARY_INV, 2), erode_size)
        ycrcb_cr_thresh = clean(thresh(ycrcb_cr, g_b_k, g_b_sd, a_t_s, cv2.THRESH_BINARY, -1), erode_size)

        # luv_l_thresh = clean(cv2.inRange(luv_l, self.options['luv_l_thresh_min'], self.options['luv_l_thresh_max']), erode_size)

        if debug:
            self.post('luv_u_thresh', luv_u_thresh)
            self.post('hls_h_thresh', hls_h_thresh)
            self.post('ycrcb_cr_thresh', ycrcb_cr_thresh)

        # Combine them
        norm = (luv_u_thresh / 255) + (hls_h_thresh / 255) + (ycrcb_cr_thresh / 255)
        comp = (norm >= 2) * 255
        comp = comp.astype("uint8")

        if debug:
            self.post('comp', comp)

        # Clean up
        # comp_clean = cv2.erode(clean(comp, self.options['erode_size']), get_kernel(3))
        comp_clean = comp

        if debug:
            self.post('comp_clean', comp_clean)

        # edge_img = comp_clean.copy()
        # edges = cv2.Canny(edge_img, 50, 150, apertureSize = 3)

        lines = cv2.HoughLinesP(comp_clean, self.options["rho"], np.pi / 30, self.options["hough_votes"], minLineLength=self.options["min_length"], maxLineGap=self.options["max_gap"])

        horiz_lines = []
        vert_lines = []

        results = {}

        if lines is None:
            results["total_poles"] = 0
            results["gate_center_prob"] = 0.0
            results["red_center_prob"] = 0.0
            results["black_center_prob"] = 0.0
        elif debug:
            lines_img = img.copy()
            for line in lines:
                (x1, y1, x2, y2) = line[0]

                delta = np.pi / 12
                theta = math.atan2(y2 - y1, x2 - x1) % np.pi

                vert_angles = np.array([np.pi / 2])
                horiz_angles = np.array([0, np.pi])

                vert_difs = np.abs(vert_angles - theta)
                horiz_difs = np.abs(horiz_angles - theta)

                if np.min(vert_difs) < delta:
                    vert_lines.append((x1, y1, x2, y2))
                elif np.min(horiz_difs) < delta:
                    horiz_lines.append((x1, y1, x2, y2))
                else:
                    continue

                cv2.line(lines_img, (x1, y1), (x2, y2), (0, 0, 255), 2)

            self.post("lines", lines_img)

            binned = img.copy()

            has_horiz = len(horiz_lines) > 0
            has_vert = len(vert_lines) > 0

            if has_horiz:
                avg_horiz_line = tuple(np.array(horiz_lines).mean(axis=0, dtype=int))
                avg_horiz_y = (avg_horiz_line[1] + avg_horiz_line[3]) // 2

                cv2.line(binned, (0, avg_horiz_y), (img.shape[1], avg_horiz_y), RED, 2)

            if has_vert:
                verts = np.array(vert_lines)
                avgs = verts[:, (0, 2)].mean(axis=1)
                # mid = int(np.median(avgs))
                mid = int(np.max(avgs) + np.min(avgs)) // 2

                if np.ptp(avgs) > width / 3:
                    results["total_poles"] = 2
                    results["gate_center_prob"] = 1.0
                    results["red_center_prob"] = 1.0
                    results["black_center_prob"] = 1.0

                    lefts = avgs[np.where(avgs <= mid)]
                    rights = avgs[np.where(avgs >= mid)]

                    left_pole = int(np.median(lefts))
                    right_pole = int(np.median(rights))

                    center_mid = (left_pole + right_pole) // 2

                    left_mid = (left_pole + center_mid) // 2
                    right_mid = (center_mid + right_pole) // 2

                    left_color = "black" if black_left else "red"
                    right_color = "red" if black_left else "black"

                    if abs(left_mid - right_mid) < width / 5:
                        results["total_poles"] = 0
                    else:
                        results["gate_center_x"] = center_mid
                        results["{}_center_x".format(left_color)] = float(left_mid)
                        results["{}_center_x".format(right_color)] = float(right_mid)

                        cv2.line(binned, (left_pole, 0), (left_pole, img.shape[0]), GREEN, 2)
                        cv2.line(binned, (right_pole, 0), (right_pole, img.shape[0]), GREEN, 2)
                        cv2.line(binned, (center_mid, 0), (center_mid, img.shape[0]), YELLOW, 2)

                        if black_left:
                            cv2.line(binned, (left_mid, 0), (left_mig, img.shape[0]), BLACK, 2)
                            cv2.line(binned, (right_mid, 0), (right_mid, img.shape[0]), RED, 2)
                        else:
                            cv2.line(binned, (left_mid, 0), (left_mid, img.shape[0]), RED, 2)
                            cv2.line(binned, (right_mid, 0), (right_mid, img.shape[0]), BLACK, 2)
                else:
                    results["total_poles"] = 1
                    results["gate_center_prob"] = 1.0
                    results["red_center_prob"] = 0.0
                    results["black_center_prob"] = 0.0
                    cv2.line(binned, (mid, 0), (mid, img.shape[0]), BLUE, 2)

            self.post('binned', binned)

            #print(results)

            group = gate_shm_group.get()

            for key, value in results.items():
                if key.endswith('_x'):
                    value = self.normalized(value, 1)
                elif key.endswith('_y'):
                    value = self.normalized(value, 0)
                setattr(group, key, value)

            gate_shm_group.set(group)


if __name__ == '__main__':
    BicolorGate(None, module_options)()
