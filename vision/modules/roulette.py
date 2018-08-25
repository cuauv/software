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
    options.IntOption('blur_kernel', 8, 0, 255),
    options.IntOption('erode_kernel', 2, 0, 255),
    options.IntOption('black_erode_iters', 5, 0, 100),
    options.IntOption('canny_low_thresh', 100, 0, 1000),
    options.IntOption('canny_high_thresh', 200, 0, 1000),
    options.IntOption('hough_lines_rho', 5, 1, 1000),
    options.IntOption('hough_lines_theta', 1, 1, 1000),
    options.IntOption('hough_lines_thresh', 200, 0, 1000),
    options.IntOption('hough_circle_blur_kernel', 10, 0, 255),
    options.IntOption('hough_circles_dp', 1, 0, 255),
    options.IntOption('hough_circles_minDist', 50, 0, 1000),
    options.IntOption('hough_circles_param1', 5, 0, 255),
    options.IntOption('hough_circles_param2', 30, 0, 255),
    options.IntOption('hough_circles_minRadius', 50, 0, 1000),
    options.IntOption('hough_circles_maxRadius', 1000, 0, 1000),
    options.IntOption('contour_min_area', 1000, 0, 100000),
    options.DoubleOption('percent_red_thresh', 0.005, 0, 1),
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


def calculate_centroid(contour):
    moments = cv2.moments(contour)
    centroid_x = int(moments['m10'] / max(1e-10, moments['m00']))
    centroid_y = int(moments['m01'] / max(1e-10, moments['m00']))
    return centroid_x, centroid_y


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
    new_centers = [calculate_centroid(contour) for contour in contours]

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

        # With new camera we are no longer rotated
        #mat = cv2.rotate(mat, cv2.ROTATE_90_CLOCKWISE)

        mat = cv2.UMat(mat)

        debug = self.options['debug']

        try:
            ## Reset SHM output
            #for s in ALL_SHM:
            #    s.reset()
            for s in ALL_SHM:
                s.visible = False

            lab = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
            lab_split = cv2.split(lab)
            #self.post('lab_a_split', lab_split[1])

            if False:
                self.post('lab', lab.get())

            # detect green section
            #dist_from_green = np.linalg.norm(lab.get()[:, :, 1:].astype(int) - [131, 165, 125][1:], axis=2).astype(int)
            dist_from_green = np.linalg.norm(lab.get()[:, :, :].astype(int) - [255, 129, 128], axis=2).astype(int)

            if debug:
                self.post('yellow_dist', np.abs(dist_from_green).astype('uint8'))

            green_threshed = cv2.inRange(
                dist_from_green,
                self.options["color_dist_min_green_funnel"],
                self.options["color_dist_max_green_funnel"],
            )

            # green_threshed = cv2.inRange(lab_split[1],
            #         self.options['green_lab_a_min'],
            #         self.options['green_lab_a_max'])
            green_threshed = cv2.erode(green_threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1))
            if debug and POST_UMAT:
                self.post('green_threshed', green_threshed)

            # detect red section
            red_threshed = cv2.inRange(lab_split[1],
                    self.options['red_lab_a_min'],
                    self.options['red_lab_a_max'])
            red_threshed = cv2.erode(red_threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1))
            if debug and POST_UMAT:
                self.post('red_threshed', red_threshed)

            # detect black section
            black_threshed = cv2.inRange(lab_split[0],
                    self.options['black_lab_l_min'],
                    self.options['black_lab_l_max'])
            black_threshed = cv2.erode(black_threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1),
                    iterations=self.options['black_erode_iters'])
            if debug and POST_UMAT:
                self.post('black_threshed', black_threshed)

            #comp = red_threshed & ~green_threshed
            comp = green_threshed

            if debug:
                self.post('comp', comp)

            #green_threshed = green_threshed & ~red_threshed & ~black_threshed

            #self.post('comp', green_threshed)

            # all_threshed = green_threshed | red_threshed | black_threshed
            # all_threshed = cv2.GaussianBlur(all_threshed,
            #         (2 * self.options['hough_circle_blur_kernel'] + 1,
            #         2 * self.options['hough_circle_blur_kernel'] + 1), 0)
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

            percent_red = cv2.countNonZero(red_threshed) / (DOWNWARD_CAM_WIDTH * DOWNWARD_CAM_HEIGHT)
            percent_red_thresh = self.options['percent_red_thresh']

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
                blurred = cv2.GaussianBlur(comp,
                        (2 * self.options['blur_kernel'] + 1,
                        2 * self.options['blur_kernel'] + 1), 0)
                #self.post('blurred', blurred)

                edges = cv2.Canny(blurred,
                        threshold1=self.options['canny_low_thresh'],
                        threshold2=self.options['canny_high_thresh'])
                if debug and POST_UMAT:
                    self.post('edges', edges)
                lines = cv2.HoughLines(edges,
                        self.options['hough_lines_rho'],
                        self.options['hough_lines_theta'] * np.pi / 180,
                        self.options['hough_lines_thresh'])

                thetas = []

                THETA_DIFF = math.radians(20)

                if lines is not None:
                    # Remove duplicates
                    lines_unfiltered = set([(line[0][0], line[0][1]) for line in lines])

                    lines_groups_mat = mat

                    # Group lines into bins
                    bins = []
                    for line in lines_unfiltered:
                        for bin in bins:
                            # Multiple by 2 because we're in [0, 180], not [0, 360]
                            if abs(angle_diff(line[1] * 2, bin[0][1] * 2)) < THETA_DIFF * 2:
                                bin = (bin[0], bin[1] + 1)
                                break
                        else:
                            bins.append((line, 1))

                            (rho, theta) = line

                            a = np.cos(theta)
                            b = np.sin(theta)
                            x0 = a*rho
                            y0 = b*rho
                            x1 = (x0 + 1500*(-b))
                            y1 = (y0 + 1500*(a))
                            x2 = (x0 - 1500*(-b))
                            y2 = (y0 - 1500*(a))
                            cv2.line(lines_groups_mat, (int(x1), int(y1)), (int(x2), int(y2)), (255, 0, 0), 2)

                    if debug:
                        self.post('lines_groups', cv2.UMat.get(lines_groups_mat))

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

                        MAX_ANGLE_DIFF = 15

                        if delta <= MAX_ANGLE_DIFF:
                            line_equations = []
                            lines_mat = mat #mat.copy()
                            for (rho, theta) in lines:
                                thetas.append(theta)

                                a = np.cos(theta)
                                b = np.sin(theta)
                                x0 = a*rho
                                y0 = b*rho
                                x1 = (x0 + 1500*(-b))
                                y1 = (y0 + 1500*(a))
                                x2 = (x0 - 1500*(-b))
                                y2 = (y0 - 1500*(a))
                                cv2.line(lines_mat, (int(x1), int(y1)), (int(x2), int(y2)), (0, 0, 255), 2)
                                line_equations.append((x1, x2, y1, y2))

                            if debug and POST_UMAT:
                                self.post('lines', cv2.UMat.get(lines_mat))

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
                cv2.circle(center_mat, (center_x, center_y), 7, (255, 255, 255), -1)
                if POST_UMAT:
                    self.post('center', cv2.UMat.get(center_mat))
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
            if debug and POST_UMAT:
                self.post('centroids', cv2.UMat.get(mat))

            # # draw centroids for red sections and predict location ~3 seconds later
            # _, contours, _ = cv2.findContours(red_threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            # contours = sorted(contours, key=lambda cont: cv2.contourArea(cont), reverse=True)
            # bin_index = 0
            # for contour in contours[:len(RED_BINS)]:
            #     centroid_x, centroid_y = calculate_centroid(contour)
            #     cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
            #     cv2.circle(mat, (centroid_x, centroid_y), 7, (255, 255, 255), -1)

            # assign_bins(contours[:len(RED_BINS)], RED_BINS, self)
            # if POST_UMAT:
            #     self.post('centroids', cv2.UMat.get(mat))

            # # draw centroids for black sections and predict location ~3 seconds later
            # _, contours, _ = cv2.findContours(black_threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            # contours = sorted(contours, key=lambda cont: cv2.contourArea(cont), reverse=True)
            # bin_index = 0
            # for contour in contours[:len(BLACK_BINS)]:
            #     centroid_x, centroid_y = calculate_centroid(contour)
            #     cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
            #     cv2.circle(mat, (centroid_x, centroid_y), 7, (255, 255, 255), -1)

            # assign_bins(contours[:len(BLACK_BINS)], BLACK_BINS, self)
            # if POST_UMAT:
                self.post('centroids', cv2.UMat.get(mat))

        except Exception as e:
            traceback.print_exc(file=sys.stdout)
        finally:
            for s in ALL_SHM:
                s.commit()


if __name__ == '__main__':
    Roulette('downward', options)()
