#!/usr/bin/env python3
from vision.modules.base import ModuleBase
from vision import options
import shm
import math
from enum import Enum
import cv2 as cv2
import numpy as np
from scipy.spatial.distance import pdist, squareform

ENABLE_UMAT_POST = True

black_left = True
gate_shm_group = shm.bicolor_gate_vision

# SHM:
#  - general, black, and red centers
#  - confidence

module_options = [
    options.BoolOption('debug', True),
    #options.IntOption('erode_size', 1, 1, 40),
    #options.IntOption('gaussian_kernel', 1, 1, 40),
    #options.IntOption('gaussian_stdev', 2, 0, 40),
    #options.IntOption('thresh_size', 7, 1, 100),

    #options.IntOption('luv_u_canny_min', 0, 0, 255),
    #options.IntOption('luv_u_canny_max', 255, 0, 255),
    #options.IntOption('hls_h_canny_min', 0, 0, 255),
    #options.IntOption('hls_h_canny_max', 255, 0, 255),
    options.IntOption('ycrcb_cr_canny_min', 5, 0, 255),
    options.IntOption('ycrcb_cr_canny_max', 13, 0, 255),

    options.IntOption('min_area', 700, 1, 2000),
    options.IntOption('center_dist', 100, 1, 2000),
    #options.IntOption('luv_l_thresh_min', 1, 1, 254),
    #options.IntOption('luv_l_thresh_max', 76, 1, 254),
    options.IntOption('hough_votes', 250, 100, 1000000),
    options.IntOption('rho', 7, 1, 100),
    options.IntOption('bin_dist_threshold', 50, 0, 1000),
    options.IntOption('min_length', 50, 1, 100000),
    options.IntOption('max_gap', 50, 1, 100000),
    options.IntOption('min_y', 0, 0, 1024),
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

def clean(img, erode_size):
    kernel = get_kernel(erode_size)

    erode2 = cv2.erode(img, kernel)
    dilate2 = cv2.dilate(erode2, kernel)

    return dilate2

def thresh(img, kernel, stdev, size, thresh_type, c):
    blurred = cv2.GaussianBlur(img, (kernel * 2 + 1, kernel * 2 + 1), stdev, stdev)
    return cv2.adaptiveThreshold(blurred, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, thresh_type, size * 2 + 1, c)

class BicolorGate(ModuleBase):
    umat = cv2.UMat()

    def blur(self, img, kernel, stdev):
        return cv2.GaussianBlur(img, (kernel * 2 + 1, kernel * 2 + 1), stdev, stdev)

    def get_kernel(self, size):
        return cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (size * 2 + 1, size * 2 + 1), (size, size))

    def postu(self, name, img):
        if type(img) == type(BicolorGate.umat):
            # For some reason this seems to wreck visiongui
            # The more UMat posts, the more CPU
            if ENABLE_UMAT_POST:
                self.post(name, cv2.UMat.get(img))
        else:
            self.post(name, img)

    def process(self, img):
        # only deal with half of the pixels
        img = img[::2,::2,:]

        debug = self.options['debug']
        height, width, channels = img.shape

        img = cv2.UMat(img)

        if debug:
            self.postu('raw', img)

        #if debug:
        #    self.post('rotated', img)

        # We use the L from LUV and the S from HLS
        #(luv_l, luv_u, luv_v) = cv2.split(cv2.cvtColor(img, cv2.COLOR_BGR2LUV))
        #(hls_h, hls_l, hls_s) = cv2.split(cv2.cvtColor(img, cv2.COLOR_BGR2HLS))
        (ycrcb_y, ycrcb_cr, ycrcb_cb) = cv2.split(cv2.cvtColor(img, cv2.COLOR_BGR2YCR_CB))

        ycrcb_cr_blur = self.blur(ycrcb_cr, 3, 3)

        #ycrcb_cr_trunc = cv2.inRange(ycrcb_cr_blur, 0, 63)

        ycrcb_combined = ycrcb_cr_blur #& ~ycrcb_cr_trunc

        if debug:
            #self.postu('luv_u', luv_u)
            #self.postu('hls_h', hls_h)
            self.postu('ycrcb_cr', ycrcb_cr)
            self.postu('ycrcb_cr_blur', ycrcb_cr_blur)
            #self.postu('trunc', ycrcb_cr_trunc)

        #g_b_k = self.options['gaussian_kernel']
        #g_b_sd = self.options['gaussian_stdev']
        #a_t_s = self.options['thresh_size']

        #erode_size = self.options['erode_size']

        # Thresholds
        #luv_u_thresh = clean(thresh(luv_u, g_b_k, g_b_sd, a_t_s, cv2.THRESH_BINARY, -1), erode_size)
        #hls_h_thresh = clean(thresh(hls_h + 180, g_b_k, g_b_sd, a_t_s, cv2.THRESH_BINARY_INV, 2), erode_size)s
        #ycrcb_cr_thresh = clean(thresh(ycrcb_cr, g_b_k, g_b_sd, a_t_s, cv2.THRESH_BINARY, -1), erode_size)

        # luv_l_thresh = clean(cv2.inRange(luv_l, self.options['luv_l_thresh_min'], self.options['luv_l_thresh_max']), erode_size)

        #luv_u_edges = cv2.Canny(self.blur(luv_u, 1, 1), self.options['luv_u_canny_min'], self.options['luv_u_canny_max'])
        #hls_h_edges = cv2.Canny(self.blur(hls_h, 1, 1), self.options['hls_h_canny_min'], self.options['hls_h_canny_max'])
        ycrcb_cr_edges = cv2.Canny(ycrcb_combined, threshold1=self.options['ycrcb_cr_canny_min'], threshold2=self.options['ycrcb_cr_canny_max'])

        if debug:
            #self.postu('luv_u_edges', luv_u_edges)
            #self.postu('hls_h_edges', hls_h_edges)
            self.postu('ycrcb_cr_edges', ycrcb_cr_edges)

        comp = cv2.dilate(ycrcb_cr_edges, self.get_kernel(1))

        # Ignore top section of pickles
        cv2.rectangle(comp, (0, 0), (2000, self.options['min_y']), 0, thickness=-1)

        if debug:
            self.postu('comp', comp)

        # Combine them
        #norm = (luv_u_thresh / 255) + (hls_h_thresh / 255) + (ycrcb_cr_thresh / 255)
        #comp = (norm >= 2) * 255
        #comp = comp.astype("uint8")

        # Clean up
        # comp_clean = cv2.erode(clean(comp, self.options['erode_size']), get_kernel(3))
        #comp_clean = comp

        #if debug:
        #    self.post('comp_clean', comp_clean)

        # edge_img = comp_clean.copy()
        # edges = cv2.Canny(edge_img, 50, 150, apertureSize = 3)

        lines = cv2.HoughLinesP(comp, self.options["rho"], np.pi / 30, self.options["hough_votes"], minLineLength=self.options["min_length"], maxLineGap=self.options["max_gap"])

        horiz_lines = []
        vert_lines = []

        results = {}

        if lines is None:
            results["total_poles"] = 0
            results["gate_center_prob"] = 0.0
            results["red_center_prob"] = 0.0
            results["black_center_prob"] = 0.0
        else:
            lines_img = img
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

            self.postu("lines", lines_img)

            binned = img

            has_horiz = len(horiz_lines) > 0
            has_vert = len(vert_lines) > 0

            if has_horiz:
                avg_horiz_line = tuple(np.array(horiz_lines).mean(axis=0, dtype=int))
                avg_horiz_x = (avg_horiz_line[0] + avg_horiz_line[2]) // 2
                avg_horiz_y = (avg_horiz_line[1] + avg_horiz_line[3]) // 2

                cv2.line(binned, (0, avg_horiz_y), (width, avg_horiz_y), RED, 2)

            if has_vert:
                verts = np.array(vert_lines)
                avgs = verts[:, (0, 2)].mean(axis=1)
                # mid = int(np.median(avgs))
                mid = int(np.max(avgs) + np.min(avgs)) / 2

                left_color = "black" if black_left else "red"
                right_color = "red" if black_left else "black"

                bins = []
                BIN_DIST = 100

                for line in verts:
                    cx = (line[0] + line[2]) / 2
                    for bin in bins:
                        if abs(cx - bin[0]) < BIN_DIST:
                            bin = ((bin[0] * bin[1] + cx) / (bin[1] + 1), bin[1] + 1)
                            break
                    else:
                        bins.append((cx, 1))

                bins_sorted = sorted(bins, key=lambda bin: -bin[1])

                sortsort = img #.copy()

                for bin in bins_sorted[:2]:
                    cv2.line(sortsort, (int(bin[0]), 0), (int(bin[0]), 1024), (255, 255, 0), 2)

                self.post('sortsort', sortsort.get())

                if np.ptp(avgs) > width / 3:
                    lefts = avgs[np.where(avgs <= mid)]
                    rights = avgs[np.where(avgs >= mid)]

                    left_pole = np.median(lefts)
                    right_pole = np.median(rights)

                    center_mid = (left_pole + right_pole) / 2

                    left_mid = (left_pole + center_mid) / 2
                    right_mid = (center_mid + right_pole) / 2

                    if abs(left_mid - right_mid) < width / 5:
                        results["total_poles"] = 0
                        results["gate_center_prob"] = 0.0
                        results["red_center_prob"] = 0.0
                        results["black_center_prob"] = 0.0
                        results["width"] = 0.0
                    else:
                        results["total_poles"] = 2
                        results["gate_center_prob"] = 1.0
                        results["red_center_prob"] = 1.0
                        results["black_center_prob"] = 1.0
                        results["gate_center_x"] = center_mid
                        results["{}_center_x".format(left_color)] = left_mid
                        results["{}_center_x".format(right_color)] = right_mid
                        results["width"] = abs(right_pole - left_pole) / width

                        cv2.line(binned, (int(left_pole), 0), (int(left_pole), height), GREEN, 2)
                        cv2.line(binned, (int(right_pole), 0), (int(right_pole), height), GREEN, 2)
                        cv2.line(binned, (int(center_mid), 0), (int(center_mid), height), YELLOW, 2)

                        if black_left:
                            cv2.line(binned, (int(left_mid), 0), (int(left_mid), height), BLACK, 2)
                            cv2.line(binned, (int(right_mid), 0), (int(right_mid), height), RED, 2)
                        else:
                            cv2.line(binned, (int(left_mid), 0), (int(left_mid), height), RED, 2)
                            cv2.line(binned, (int(right_mid), 0), (int(right_mid), height), BLACK, 2)
                elif has_horiz:
                    results["total_poles"] = 1
                    results["gate_center_prob"] = 1.0

                    seen_right = mid > avg_horiz_x
                    seen_color = right_color if seen_right else left_color
                    unseen_color = left_color if seen_right else right_color
                    line_color = BLACK if seen_right ^ black_left else RED

                    seen_center = mid / 2 if seen_right else mid + (width - mid) / 2

                    # This is gross... but we need it
                    results["gate_center_x"] = seen_center

                    results["{}_center_x".format(seen_color)] = seen_center
                    results["{}_center_prob".format(seen_color)] = 1.0
                    results["{}_center_prob".format(unseen_color)] = 0.0
                    results["width"] = (mid if seen_right else width - mid) / width

                    cv2.line(binned, (int(mid), 0), (int(mid), height), BLUE, 2)
                    cv2.line(binned, (int(seen_center), 0), (int(seen_center), height), line_color, 2)
                else:
                    results["total_poles"] = 1
                    results["gate_center_prob"] = 0.0
                    results["red_center_prob"] = 0.0
                    results["black_center_prob"] = 0.0
                    results["width"] = 0.0
                    cv2.line(binned, (int(mid), 0), (int(mid), height), BLUE, 2)

            self.postu('binned', binned)

            #print(results)

            group = gate_shm_group.get()

            for key, value in results.items():
                if key.endswith('_x'):
                    value = (value - width / 2) / (width / 2)
                elif key.endswith('_y'):
                    value = (value - height / 2) / (height / 2)
                setattr(group, key, value)

            gate_shm_group.set(group)


if __name__ == '__main__':
    BicolorGate(None, module_options)()
