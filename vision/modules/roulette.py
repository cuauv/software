#!/usr/bin/env python3

# Written by Chesley Tan.
# Tweaked by Will Smith.

import traceback
import sys
import math
import itertools
import cv2
import numpy as np
import shm

from vision.modules.base import ModuleBase
from vision import options
options = [
    options.IntOption('red_lab_a_min', 129, 0, 255),
    options.IntOption('red_lab_a_max', 255, 0, 255),
    options.IntOption('black_lab_l_min', 0, 0, 255),
    options.IntOption('black_lab_l_max', 110, 0, 255),
    options.IntOption('green_lab_a_min', 0, 0, 255),
    options.IntOption('green_lab_a_max', 88, 0, 255),
    options.IntOption('blur_kernel', 8, 0, 255),
    options.IntOption('erode_kernel', 2, 0, 255),
    options.IntOption('black_erode_iters', 5, 0, 100),
    options.IntOption('canny_low_thresh', 100, 0, 1000),
    options.IntOption('canny_high_thresh', 200, 0, 1000),
    options.IntOption('hough_lines_rho', 5, 1, 1000),
    options.IntOption('hough_lines_theta', 10, 1, 1000),
    options.IntOption('hough_lines_thresh', 90, 0, 1000),
    options.IntOption('hough_circle_blur_kernel', 10, 0, 255),
    options.IntOption('hough_circles_dp', 1, 0, 255),
    options.IntOption('hough_circles_minDist', 50, 0, 1000),
    options.IntOption('hough_circles_param1', 5, 0, 255),
    options.IntOption('hough_circles_param2', 30, 0, 255),
    options.IntOption('hough_circles_minRadius', 50, 0, 1000),
    options.IntOption('hough_circles_maxRadius', 1000, 0, 1000),
    options.IntOption('contour_min_area', 1000, 0, 100000)
]

ROTATION_PREDICTION_ANGLE = 20
DOWNWARD_CAM_WIDTH = shm.camera.downward_width.get()
DOWNWARD_CAM_HEIGHT = shm.camera.downward_height.get()


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


def calculate_centroid(contour):
    moments = cv2.moments(contour)
    centroid_x = int(moments['m10'] / max(1e-10, moments['m00']))
    centroid_y = int(moments['m01'] / max(1e-10, moments['m00']))
    return centroid_x, centroid_y


def calc_diff(new_centers, old_centers):
    return sum([dist(new, old) for (new, old) in zip(new_centers, old_centers)])

def dist(a, b):
    return math.sqrt((a[0]-b[0])**2 + (a[1]-b[1])**2)

def assign_bins(contours, bins_data, module_context):
    # Keeps track of which bin is which by sorting all possible lists by the sum of the
    # differences in the old and new centroids

    # Find new and old centers of the two bins
    old_centers = [(bin.shm_group.centroid_x.get(), bin.shm_group.centroid_y.get()) for bin in bins_data]
    new_centers = [calculate_centroid(contour) for contour in contours]

    permutations = itertools.permutations(new_centers)

    diffs = [(centers, calc_diff(centers, old_centers)) for centers in permutations]

    sorted_diffs = sorted(diffs, key=lambda tup: tup[1])

    best_permutation = sorted_diffs[0][0]

    for i in range(len(bins_data)):
        bins_data[i].visible = True
        bins_data[i].centroid_x = best_permutation[i][0]
        bins_data[i].centroid_y = best_permutation[i][1]


class RouletteBoardData:
    def __init__(self, shm_group):
        self.shm_group = shm_group
        self.reset()

    def reset(self):
        self.board_visible = False
        self.center_x = 0
        self.center_y = 0

    def commit(self):
        results = self.shm_group.get()
        results.board_visible = self.board_visible
        results.center_x = self.center_x
        results.center_y = self.center_y
        self.shm_group.set(results)


# TODO add angles so that we can align heading
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

    def commit(self):
        results = self.shm_group.get()
        results.visible = self.visible
        results.centroid_x = self.centroid_x
        results.centroid_y = self.centroid_y
        results.predicted_location = self.predicted_location
        results.predicted_x = self.predicted_x
        results.predicted_y = self.predicted_y
        self.shm_group.set(results)


ROULETTE_BOARD = RouletteBoardData(shm.bins_vision)
GREEN_BINS = [BinsData(shm.bins_green0), BinsData(shm.bins_green1)]
RED_BINS = [BinsData(shm.bins_red0), BinsData(shm.bins_red1)]
BLACK_BINS = [BinsData(shm.bins_black0), BinsData(shm.bins_black1)]
ALL_BINS = GREEN_BINS + RED_BINS + BLACK_BINS
ALL_SHM = [ROULETTE_BOARD] + ALL_BINS


class Roulette(ModuleBase):

    def process(self, mat):
        global DOWNWARD_CAM_WIDTH, DOWNWARD_CAM_HEIGHT

        DOWNWARD_CAM_WIDTH = DOWNWARD_CAM_WIDTH or mat.shape[1]
        DOWNWARD_CAM_HEIGHT = DOWNWARD_CAM_HEIGHT or mat.shape[0]

        mat = cv2.rotate(mat, cv2.ROTATE_90_CLOCKWISE)

        mat = cv2.UMat(mat)

        try:
            # Reset SHM output
            for s in ALL_SHM:
                s.reset()

            lab = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
            lab_split = cv2.split(lab)
            #self.post('lab_a_split', lab_split[1])

            # detect green section
            green_threshed = cv2.inRange(lab_split[1],
                    self.options['green_lab_a_min'],
                    self.options['green_lab_a_max'])
            green_threshed = cv2.erode(green_threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1))
            #self.post('green_threshed', green_threshed)

            # detect red section
            red_threshed = cv2.inRange(lab_split[1],
                    self.options['red_lab_a_min'],
                    self.options['red_lab_a_max'])
            red_threshed = cv2.erode(red_threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1))
            #self.post('red_threshed', red_threshed)

            # detect black section
            black_threshed = cv2.inRange(lab_split[0],
                    self.options['black_lab_l_min'],
                    self.options['black_lab_l_max'])
            black_threshed = cv2.erode(black_threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1),
                    iterations=self.options['black_erode_iters'])
            #self.post('black_threshed', black_threshed)

            all_threshed = green_threshed | red_threshed | black_threshed
            all_threshed = cv2.GaussianBlur(all_threshed,
                    (2 * self.options['hough_circle_blur_kernel'] + 1,
                    2 * self.options['hough_circle_blur_kernel'] + 1), 0)
            #self.post('all_threshed', cv2.UMat.get(all_threshed))

            #circle_blurred = cv2.GaussianBlur(all_threshed,
            #           (2 * self.options['blur_kernel'] + 1,
            #            2 * self.options['blur_kernel'] + 1), 0)
            #circle_edges = cv2.Canny(circle_blurred,
            #            threshold1=self.options['canny_low_thresh'],
            #            threshold2=self.options['canny_high_thresh'])

            #self.post('circle_edges', circle_edges)

            #circles = cv2.HoughCircles(circle_edges, cv2.HOUGH_GRADIENT, self.options['hough_circles_dp'],
            #                           self.options['hough_circles_minDist'], param1=self.options['hough_circles_param1'],
            #                           param2=self.options['hough_circles_param2'], minRadius=self.options['hough_circles_minRadius'],
            #                           maxRadius=self.options['hough_circles_maxRadius'])

            # Hough circles aren't working. So don't use them.
            found_center = False # circles is not None
            if found_center:
                circle_mask = np.zeros(mat.shape, np.uint8)
                for circle in circles[0, :]:
                    cv2.circle(circle_mask, (circle[0], circle[1]), circle[2], (255, 255, 255), -1)
                #self.post("circle_mask", circle_mask)
                circle = circles[0][0]
                center_x, center_y = circle[0], circle[1]
            if not found_center:
                # Fall back to hough lines if cannot find center using hough circles
                blurred = cv2.GaussianBlur(green_threshed,
                        (2 * self.options['blur_kernel'] + 1,
                        2 * self.options['blur_kernel'] + 1), 0)
                #self.post('blurred', blurred)

                edges = cv2.Canny(blurred,
                        threshold1=self.options['canny_low_thresh'],
                        threshold2=self.options['canny_high_thresh'])
                self.post('edges', edges)
                lines = cv2.HoughLines(edges,
                        self.options['hough_lines_rho'],
                        self.options['hough_lines_theta'] * np.pi / 180,
                        self.options['hough_lines_thresh'])
                if lines is not None:
                    lines = [(idx, line[0]) for (idx, line) in enumerate(lines[:2])]
                    line_equations = []
                    lines_mat = mat #mat.copy()
                    for (i, (rho, theta)) in lines:
                        a = np.cos(theta)
                        b = np.sin(theta)
                        x0 = a*rho
                        y0 = b*rho
                        x1 = int(x0 + 1000*(-b))
                        y1 = int(y0 + 1000*(a))
                        x2 = int(x0 - 1000*(-b))
                        y2 = int(y0 - 1000*(a))
                        cv2.line(lines_mat, (x1, y1), (x2, y2), (0, 0, 255), 2)
                        line_equations.append((float(x1), float(x2), float(y1), float(y2)))
                    self.post('lines', cv2.UMat.get(lines_mat))
                    found_center = len(line_equations) >= 2
                    if found_center:
                        # calculate intersection of diameters of green section
                        [x01, x02, y01, y02] = line_equations[0]
                        [x11, x12, y11, y12] = line_equations[1]
                        b1 = (y02 - y01) / max(1e-10, x02 - x01)
                        b2 = (y12 - y11) / max(1e-10, x12 - x11)
                        intersection_x = int((b1 * x01 - b2 * x11 + y11 - y01) / (b1 - b2))
                        intersection_y = int(b1 * (intersection_x - x01) + y01)
                        center_x, center_y = intersection_x, intersection_y

            if found_center:
                center_mat = mat # mat.copy()
                cv2.circle(center_mat, (center_x, center_y), 7, (255, 255, 255), -1)
                self.post('center', cv2.UMat.get(center_mat))
                ROULETTE_BOARD.board_visible = True
                (ROULETTE_BOARD.center_x, ROULETTE_BOARD.center_y) = (center_x, center_y)

            # draw centroids of green sections and predict location ~3 seconds later
            _, contours, _ = cv2.findContours(green_threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            contours = sorted(contours, key=lambda cont: cv2.contourArea(cont), reverse=True)
            bin_index = 0
            for contour in contours[:len(GREEN_BINS)]:
                centroid_x, centroid_y = calculate_centroid(contour)
                cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
                cv2.circle(mat, (centroid_x, centroid_y), 7, (255, 255, 255), -1)
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
            self.post('centroids', cv2.UMat.get(mat))

            # draw centroids for red sections and predict location ~3 seconds later
            _, contours, _ = cv2.findContours(red_threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            contours = sorted(contours, key=lambda cont: cv2.contourArea(cont), reverse=True)
            bin_index = 0
            for contour in contours[:len(RED_BINS)]:
                centroid_x, centroid_y = calculate_centroid(contour)
                cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
                cv2.circle(mat, (centroid_x, centroid_y), 7, (255, 255, 255), -1)

            assign_bins(contours[:len(RED_BINS)], RED_BINS, self)
            self.post('centroids', cv2.UMat.get(mat))

            # draw centroids for black sections and predict location ~3 seconds later
            _, contours, _ = cv2.findContours(black_threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            contours = sorted(contours, key=lambda cont: cv2.contourArea(cont), reverse=True)
            bin_index = 0
            for contour in contours[:len(BLACK_BINS)]:
                centroid_x, centroid_y = calculate_centroid(contour)
                cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
                cv2.circle(mat, (centroid_x, centroid_y), 7, (255, 255, 255), -1)

            assign_bins(contours[:len(BLACK_BINS)], BLACK_BINS, self)
            self.post('centroids', cv2.UMat.get(mat))

        except Exception as e:
            traceback.print_exc(file=sys.stdout)
        finally:
            for s in ALL_SHM:
                s.commit()


if __name__ == '__main__':
    Roulette('downward', options)()
