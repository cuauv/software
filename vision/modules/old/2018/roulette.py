#!/usr/bin/env python3

# Written by Chesley Tan.
# Tweaked by Will Smith.

import traceback
import time
import sys
import math
import itertools
import cv2
import numpy as np
import shm

from vision.modules.base import ModuleBase
from vision.framework.color import bgr_to_lab, elementwise_color_dist, range_threshold
from vision.framework.draw import draw_line, draw_circle, draw_contours
from vision.framework.helpers import to_umat, to_odd
from vision.framework.feature import outer_contours, contour_centroid, contour_area, simple_canny, line_polar_to_cartesian, find_lines
from vision.framework.transform import erode, rect_kernel, simple_gaussian_blur
from vision import options

from vision.modules.will_common import find_best_match

options = [
    options.BoolOption('debug', False),
    options.IntOption('red_lab_a_min', 140, 0, 255),
    options.IntOption('red_lab_a_max', 255, 0, 255),
    options.IntOption('black_lab_l_min', 0, 0, 255),
    options.IntOption('black_lab_l_max', 150, 0, 255),
    options.IntOption('green_lab_a_min', 0, 0, 255),
    options.IntOption('green_lab_a_max', 120, 0, 255),
    options.IntOption('color_dist_min_green_funnel', 0, 0, 255),
    options.IntOption('color_dist_max_green_funnel', 50, 0, 255),
    options.IntOption('blur_kernel', 17, 0, 255),
    options.IntOption('erode_kernel', 5, 0, 255),
    options.IntOption('black_erode_iters', 5, 0, 100),
    options.IntOption('hough_lines_rho', 5, 1, 1000),
    options.IntOption('hough_lines_theta', 1, 1, 1000),
    options.IntOption('hough_lines_thresh', 100, 0, 1000),
    options.IntOption('contour_min_area', 1000, 0, 100000),
    options.DoubleOption('percent_red_thresh', 0.05, 0, 1),
]

POST_UMAT = True

ROTATION_PREDICTION_ANGLE = 20
DOWNWARD_CAM_WIDTH = shm.camera.downward_width.get()
DOWNWARD_CAM_HEIGHT = shm.camera.downward_height.get()

# 30 for green, 75 for red
TARGET_ANGLE = 30


def within_camera(x, y):
    return 0 <= x < DOWNWARD_CAM_WIDTH and 0 <= y < DOWNWARD_CAM_HEIGHT


def predict_xy(board_center_x, board_center_y, x, y):
    translated_x = x - board_center_x
    translated_y = y - board_center_y
    predicted_x = translated_x * np.cos(np.radians(ROTATION_PREDICTION_ANGLE)) - translated_y * np.sin(np.radians(ROTATION_PREDICTION_ANGLE))
    predicted_y = translated_x * np.sin(np.radians(ROTATION_PREDICTION_ANGLE)) + translated_y * np.cos(np.radians(ROTATION_PREDICTION_ANGLE))
    predicted_x = int(predicted_x + board_center_x)
    predicted_y = int(predicted_y + board_center_y)
    return predicted_x, predicted_y


def calc_diff(new_centers, old_centers):
    return sum([dist(new, old) for (new, old) in zip(new_centers, old_centers)])


def dist(a, b):
    return math.sqrt((a[0]-b[0])**2 + (a[1]-b[1])**2)


def angle_diff(a, b):
    return math.atan2(math.sin(b - a), math.cos(b - a))


# This is the same as the function in vision/modules/will_common, but I'm too
# lazy to change this right now
def assign_bins(contours, bins_data, module_context):
    # Keeps track of which bin is which by sorting all possible lists by the sum of the
    # differences in the old and new centroids

    # Find new and old centers of the two bins
    old_centers = [(bin.shm_group.centroid_x.get(), bin.shm_group.centroid_y.get()) for bin in bins_data]
    new_centers = [contour_centroid(contour) for contour in contours]

    permutations = itertools.permutations(new_centers)

    diffs = [(centers, calc_diff(centers, old_centers)) for centers in permutations]

    sorted_diffs = sorted(diffs, key=lambda tup: tup[1])

    best_permutation = sorted_diffs[0][0]
    for i in range(min(len(bins_data), len(best_permutation))):
        bins_data[i].visible = True
        bins_data[i].centroid_x = best_permutation[i][0]
        bins_data[i].centroid_y = best_permutation[i][1]


class RouletteBoardData:
    def __init__(self, shm_group):
        self.shm_group = shm_group
        self.reset()

    def reset(self):
        self.visible = False
        self.center_x = 0
        self.center_y = 0

    def commit(self):
        results = self.shm_group.get()
        results.board_visible = self.visible
        results.center_x = self.center_x
        results.center_y = self.center_y
        self.shm_group.set(results)


class BinsData:
    def __init__(self, shm_group):
        self.shm_group = shm_group
        self.reset()

    def reset(self):
        self.visible = False
        self.centroid_x = 0
        self.centroid_y = 0
        self.predicted_location = False
        self.predicted_x = 0
        self.predicted_y = 0
        self.angle = 0

    def commit(self):
        results = self.shm_group.get()
        results.visible = self.visible
        results.centroid_x = self.centroid_x
        results.centroid_y = self.centroid_y
        results.predicted_location = self.predicted_location
        results.predicted_x = self.predicted_x
        results.predicted_y = self.predicted_y
        results.angle = self.angle
        self.shm_group.set(results)


ROULETTE_BOARD = RouletteBoardData(shm.bins_vision)
GREEN_BINS = [BinsData(shm.bins_green0), BinsData(shm.bins_green1)]
RED_BINS = [BinsData(shm.bins_red0), BinsData(shm.bins_red1)]
BLACK_BINS = [BinsData(shm.bins_black0), BinsData(shm.bins_black1)]
ALL_BINS = GREEN_BINS + RED_BINS + BLACK_BINS
ALL_SHM = [ROULETTE_BOARD] + ALL_BINS


class Roulette(ModuleBase):
    last_run = 0

    def process(self, mat):
        global DOWNWARD_CAM_WIDTH, DOWNWARD_CAM_HEIGHT

        curr_time = time.time()
        if curr_time - self.last_run < shm.vision_module_settings.time_between_frames.get():
            return
        self.last_run = curr_time

        DOWNWARD_CAM_WIDTH = DOWNWARD_CAM_WIDTH or mat.shape[1]
        DOWNWARD_CAM_HEIGHT = DOWNWARD_CAM_HEIGHT or mat.shape[0]

        mat = to_umat(mat)

        debug = self.options['debug']

        try:
            ## Reset SHM output
            #for s in ALL_SHM:
            #    s.reset()
            for s in ALL_SHM:
                s.visible = False

            lab, lab_split = bgr_to_lab(mat)

            # detect green section
            dist_from_green = elementwise_color_dist(lab, [185, 55, 196])

            if debug:
                self.post('green_dist', np.abs(dist_from_green).astype('uint8'))

            green_threshed = range_threshold(
                dist_from_green,
                self.options["color_dist_min_green_funnel"],
                self.options["color_dist_max_green_funnel"],
            )

            erode_kernel = rect_kernel(to_odd(self.options['erode_kernel']))

            green_threshed = erode(green_threshed, erode_kernel)

            # detect red section
            red_threshed = range_threshold(lab_split[1],
                    self.options['red_lab_a_min'],
                    self.options['red_lab_a_max'])
            red_threshed = erode(red_threshed, erode_kernel)

            # detect black section
            black_threshed = range_threshold(lab_split[0],
                    self.options['black_lab_l_min'],
                    self.options['black_lab_l_max'])
            black_threshed = erode(black_threshed,
                                   erode_kernel,
                                   iterations=self.options['black_erode_iters'])

            if debug and POST_UMAT:
                self.post('green_threshed', green_threshed)
                self.post('red_threshed', red_threshed)
                self.post('black_threshed', black_threshed)

            #comp = red_threshed & ~green_threshed
            comp = green_threshed

            if debug:
                self.post('comp', comp)

            percent_red = cv2.countNonZero(red_threshed) / \
                cv2.countNonZero(red_threshed | green_threshed | black_threshed)
            percent_red_thresh = self.options['percent_red_thresh']

            # Find center using hough lines
            blurred = simple_gaussian_blur(comp, to_odd(self.options['blur_kernel']), 0)

            edges = simple_canny(blurred, use_mean=True)
            if debug and POST_UMAT:
                self.post('edges', edges)
            lines_cart, lines_polar = find_lines(edges,
                    self.options['hough_lines_rho'],
                    np.radians(self.options['hough_lines_theta']),
                    self.options['hough_lines_thresh'])

            found_center = False
            thetas = []

            THETA_DIFF = math.radians(15)

            if lines_cart:

                lines_groups_mat = mat

                # Group lines into bins
                bins = []
                for line_polar, line_cart in zip(lines_polar, lines_cart):
                    for (idx, bin) in enumerate(bins):
                        # Multiple by 2 because we're in [0, 180], not [0, 360]
                        if abs(angle_diff(line_polar[1] * 2, bin[0][1] * 2)) < THETA_DIFF * 2:
                            bins[idx] = (bin[0], bin[1] + 1)
                            break
                    else:
                        bins.append((line_polar, 1))
                        draw_line(lines_groups_mat,
                                  (line_cart[0], line_cart[1]),
                                  (line_cart[2], line_cart[3]),
                                  thickness=2)

                if debug:
                    self.post('lines_groups', lines_groups_mat)

                # Pick top four - we sometimes get the ends of the bins as lines as well
                lines_unpicked = [line for line, count in sorted(bins, key=lambda bin: bin[1], reverse=True)[:4]]

                if len(lines_unpicked) >= 2:
                    target_angle = math.radians(TARGET_ANGLE)

                    # Find two lines that are about 30 degrees apart
                    # Find the pairing of lines with the angle difference closest to 30 degrees
                    pairs = itertools.combinations(lines_unpicked, 2)
                    # We double angles because we're in [0, 180] and not [0, 360]
                    lines = sorted(pairs, key=lambda pair: abs(target_angle * 2 - abs(angle_diff(pair[0][1] * 2, pair[1][1] * 2))))[0]

                    delta = math.degrees(abs(target_angle * 2 - abs(angle_diff(lines[0][1] * 2, lines[1][1] * 2))))

                    MAX_ANGLE_DIFF = 30

                    if delta <= MAX_ANGLE_DIFF:
                        line_equations = []
                        lines_mat = mat #mat.copy()
                        for (rho, theta) in lines:
                            thetas.append(theta)

                            (x1, y1, x2, y2) = line_polar_to_cartesian(rho, theta)
                            draw_line(lines_mat, (x1, y1), (x2, y2), (255, 0, 0), thickness=2)
                            line_equations.append((x1, x2, y1, y2))

                        if debug and POST_UMAT:
                            self.post('lines', lines_mat)

                        found_center = len(line_equations) >= 2 and percent_red >= percent_red_thresh
                        if found_center:
                            # calculate intersection of diameters of green section
                            [x01, x02, y01, y02] = line_equations[0]
                            [x11, x12, y11, y12] = line_equations[1]

                            # This is stupid but it works
                            if x02 == x01:
                                x01 += 0.01
                            if x12 == x11:
                                x11 += 0.01
                            b1 = (y02 - y01) / (x02 - x01)
                            b2 = (y12 - y11) / (x12 - x11)

                            if b1 == b2:
                                print('ovelapping')
                                found_center = False
                            else:
                                intersection_x = (b1 * x01 - b2 * x11 + y11 - y01) / (b1 - b2)

                            if math.isinf(intersection_x):
                                if abs(x02 - x01) < 0.2:
                                    intersection_x = x02
                                elif abs(x12 - x11) < 0.2:
                                    intersection_x = x12

                            intersection_y = (b1 * (intersection_x - x01) + y01)

                            intersection_x = int(intersection_x)
                            intersection_y = int(intersection_y)

                            center_x, center_y = intersection_x, intersection_y
                    else:
                        found_center = False

            if found_center:
                center_mat = mat # mat.copy()
                draw_circle(center_mat, (center_x, center_y), 7, (255, 255, 255), thickness=-1)
                if POST_UMAT:
                    self.post('center', center_mat)
                ROULETTE_BOARD.visible = True
                (ROULETTE_BOARD.center_x, ROULETTE_BOARD.center_y) = (center_x, center_y)

                if len(thetas) == 2:
                    x = 0
                    y = 0
                    # We multiply angle by 2 for calculating average because
                    # we want it to be in the range [0,180] instead of [0,360]
                    for theta in thetas:
                        if theta > math.pi:
                            theta -= math.pi*2
                        x += math.cos(theta*2)
                        y += math.sin(theta*2)
                    avg_heading = math.atan2(y, x) * 180 / math.pi / 2
                    GREEN_BINS[0].angle = avg_heading

            # draw centroids of green sections and predict location ~3 seconds later
            contours = outer_contours(green_threshed)
            contours = sorted(contours, key=contour_area, reverse=True)
            bin_index = 0
            for contour in contours[:len(GREEN_BINS)]:
                centroid_x, centroid_y = contour_centroid(contour)
                draw_contours(mat, [contour], (0, 255, 0), thickness=2)
                draw_circle(mat, (centroid_x, centroid_y), 7, (255, 255, 255), thickness=-1)
            #    #self.post('centroids', mat)
            #    GREEN_BINS[bin_index].visible = True
            #    GREEN_BINS[bin_index].centroid_x = centroid_x
            #    GREEN_BINS[bin_index].centroid_y = centroid_y
            #    if found_center:
            #        predicted_x, predicted_y = predict_xy(center_x, center_y, centroid_x, centroid_y)
            #        if within_camera(predicted_x, predicted_y):
            #            cv2.circle(mat, (predicted_x, predicted_y), 7, (255, 0, 0), -1)
            #            GREEN_BINS[bin_index].predicted_location = True
            #            GREEN_BINS[bin_index].predicted_x = predicted_x
            #            GREEN_BINS[bin_index].predicted_y = predicted_y
            #    bin_index += 1

            assign_bins(contours[:len(GREEN_BINS)], GREEN_BINS, self)
            if debug and POST_UMAT:
                self.post('centroids', mat)

            # # draw centroids for red sections and predict location ~3 seconds later
            # _, contours, _ = cv2.findContours(red_threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            # contours = sorted(contours, key=lambda cont: cv2.contourArea(cont), reverse=True)
            # bin_index = 0
            # for contour in contours[:len(RED_BINS)]:
            #     centroid_x, centroid_y = contour_centroid(contour)
            #     cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
            #     cv2.circle(mat, (centroid_x, centroid_y), 7, (255, 255, 255), -1)

            # assign_bins(contours[:len(RED_BINS)], RED_BINS, self)
            # if POST_UMAT:
            #     self.post('centroids', mat)

            # # draw centroids for black sections and predict location ~3 seconds later
            # _, contours, _ = cv2.findContours(black_threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            # contours = sorted(contours, key=lambda cont: cv2.contourArea(cont), reverse=True)
            # bin_index = 0
            # for contour in contours[:len(BLACK_BINS)]:
            #     centroid_x, centroid_y = contour_centroid(contour)
            #     cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
            #     cv2.circle(mat, (centroid_x, centroid_y), 7, (255, 255, 255), -1)

            # assign_bins(contours[:len(BLACK_BINS)], BLACK_BINS, self)
            # if POST_UMAT:
                self.post('centroids', mat)

        except Exception:
            traceback.print_exc(file=sys.stdout)
        finally:
            for s in ALL_SHM:
                s.commit()


if __name__ == '__main__':
    Roulette('downward', options)()

