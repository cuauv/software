#!/usr/bin/env python3

import math
import time
import cv2
from collections import namedtuple
from functools import reduce
from itertools import permutations, starmap
import numpy as np

import shm

from mission.constants.config import buoys as constants
from mission.constants.config import buoysVision as bv

from vision.modules.base import ModuleBase
from vision.vision_common import green, red, white, yellow
from vision import options
from vision.modules.color_balance import balance

options = [options.BoolOption('debug', False),
           options.BoolOption('red_debug', False),
           options.BoolOption('green_debug', False),
           options.BoolOption('yellow_debug', False),
           options.BoolOption('use_color_correction', False),
           options.IntOption('hls_l_min', bv.hls_l_min, 0, 255),
           options.IntOption('hls_l_max', bv.hls_l_max, 0, 255),
           options.IntOption('red_hls_h_min', bv.red_hls_h_min, 0, 255),
           options.IntOption('red_hls_h_max', bv.red_hls_h_max, 0, 255),
           options.IntOption('red_ycrcb_cb_min', bv.red_ycrcb_cb_min, 0, 255),
           options.IntOption('red_ycrcb_cb_max', bv.red_ycrcb_cb_max, 0, 255),
           options.IntOption('red_lab_a_min', bv.red_lab_a_min, 0, 255),
           options.IntOption('red_lab_a_max', bv.red_lab_a_max, 0, 255),
           options.IntOption('red_lab_b_min', bv.red_lab_b_min, 0, 255),
           options.IntOption('red_lab_b_max', bv.red_lab_b_max, 0, 255),
           options.IntOption('red_erode_kernel_size', bv.red_erode_kernel_size, 1, 151),
           options.IntOption('red_dilate_kernel_size', bv.red_dilate_kernel_size, 1, 151),
           options.IntOption('green_lab_l_min', bv.green_lab_l_min, 0, 255),
           options.IntOption('green_lab_l_max', bv.green_lab_l_max, 0, 255),
           options.IntOption('green_hsv_v_min', bv.green_hsv_v_min, 0, 255),
           options.IntOption('green_hsv_v_max', bv.green_hsv_v_max, 0, 255),
           options.IntOption('green_hsv_h_min', bv.green_hsv_h_min, 0, 255),
           options.IntOption('green_hsv_h_max', bv.green_hsv_h_max, 0, 255),
           options.IntOption('green_ycrcb_y_min', bv.green_ycrcb_y_min, 0, 255),
           options.IntOption('green_ycrcb_y_max', bv.green_ycrcb_y_max, 0, 255),
           options.IntOption('green_erode_kernel_size', bv.green_erode_kernel_size, 1, 151),
           options.IntOption('green_dilate_kernel_size', bv.green_dilate_kernel_size, 1, 151),
           options.IntOption('yellow_hls_h_min', bv.yellow_hls_h_min, 0, 255),
           options.IntOption('yellow_hls_h_max', bv.yellow_hls_h_max, 0, 255),
           options.IntOption('yellow_ycrcb_cb_min', bv.yellow_ycrcb_cb_min, 0, 255),
           options.IntOption('yellow_ycrcb_cb_max', bv.yellow_ycrcb_cb_max, 0, 255),
           options.IntOption('yellow_lab_a_min', bv.yellow_lab_a_min, 0, 255),
           options.IntOption('yellow_lab_a_max', bv.yellow_lab_a_max, 0, 255),
           options.IntOption('yellow_lab_b_min', bv.yellow_lab_b_min, 0, 255),
           options.IntOption('yellow_lab_b_max', bv.yellow_lab_b_max, 0, 255),
           options.IntOption('yellow_erode_kernel_size', bv.yellow_erode_kernel_size, 1, 151),
           options.IntOption('yellow_dilate_kernel_size', bv.yellow_dilate_kernel_size, 1, 151),
           options.IntOption('blur_kernel', bv.blur_kernel, 1, 151),
           options.IntOption('max_error', bv.max_error, 100, 100000),
           options.IntOption('max_buoy_error', bv.max_buoy_error, 100, 100000),
           options.DoubleOption('min_circularity', bv.min_circularity, 0.1, 0.95),
           options.DoubleOption('min_percent_frame', bv.min_percent_frame, 0, 0.01),
           options.IntOption('ex', bv.ex, 0, 1600),
           options.IntOption('ey', bv.ey, 0, 1200),
           options.IntOption('max_y_diff', bv.max_y_diff, 0, 500),
           options.IntOption('min_x_diff', bv.min_x_diff, 0, 500)]

CONTOUR_CIRCULARITY_HEURISTIC_LIMIT = 50
CONTOUR_SCALED_HEURISTIC_LIMIT = 20
CONTOUR_CLASSIFICATION_LIMIT = 6
ContourAreaData = namedtuple('ContourAreaData', ['contour', 'area'])
ContourScoreData = namedtuple('ContourScoreData', ['contour', 'area', 'circularity', 'score', 'center', 'radius'])

INVALID_BUOY_ERROR = 1e99


class BuoyData:
    def __init__(self, shm_group, color, reference_color):
        self.shm_group = shm_group
        self.results = shm_group.get()
        self.color = color
        self.reference_color = reference_color
        self.reset_frame_state()

    def reset_frame_state(self):
        self.contour = None

    def zero_results(self):
        self.results.heuristic_score = 0
        self.results.probability = 0
        self.results.area = 0
        self.results.percent_frame = 0

    def set_shm_group(self):
        self.shm_group.set(self.results)


RED_BUOY = BuoyData(shm.red_buoy_results, red, constants.RED_REF)
GREEN_BUOY = BuoyData(shm.green_buoy_results, green, constants.GREEN_REF)
YELLOW_BUOY = BuoyData(shm.yellow_buoy_results, yellow, constants.YELLOW_REF)
BUOYS = [RED_BUOY, GREEN_BUOY, YELLOW_BUOY]


def tag(mat, text, pos):
    position = (pos[0] - 100, pos[1])
    cv2.putText(mat, text, position, cv2.FONT_HERSHEY_DUPLEX, 1, (0, 0, 0), thickness=2)


class Buoys(ModuleBase):

    def set_buoy_results(self, mat, buoy, contour_list):
        contour = buoy.contour
        assert contour is not None  # This must be called after contour has been set.
        results = buoy.results
        results.center_x = self.normalized(contour.center[0] / constants.DOWNSCALE_RATIO, 0)
        results.center_y = self.normalized(contour.center[1] / constants.DOWNSCALE_RATIO, 1)
        results.top_x = self.normalized(contour.center[0] / constants.DOWNSCALE_RATIO, 0)
        results.top_y = self.normalized(contour.center[1] / constants.DOWNSCALE_RATIO - contour.radius / 2, 1)
        results.bottom_y = self.normalized(contour.center[1] / constants.DOWNSCALE_RATIO + contour.radius, 1)
        results.r_side_x = self.normalized(contour.center[0] / constants.DOWNSCALE_RATIO + contour.radius, 0)
        results.area = contour.area
        results.heuristic_score = contour.score
        results.percent_frame = 100 * contour.area / (mat.shape[1] * mat.shape[0])
        results.frame_width = mat.shape[1]
        results.frame_height = mat.shape[0]
        results.probability = contour.score / reduce(lambda acc, x: acc + x.score, contour_list, 0)
        # TODO XXX HAX Please fix this probabilities.
        results.probability = max(results.probability, 0.5)

    def calculate_error(self, buoy, candidate):
        if buoy is None:
            return INVALID_BUOY_ERROR
        # Check if area is 0
        if candidate[2] == 0:
            return INVALID_BUOY_ERROR
        error_vec = np.array(buoy.reference_color) - np.array(candidate[1])
        buoy_error = error_vec.dot(error_vec)
        # Prefer larger contours
        size_error = 1000 / candidate[2]
        # Prefer lower contours
        position_error = 1000 - candidate[0].center[1]
        if buoy_error > self.options['max_buoy_error']:
            return INVALID_BUOY_ERROR
        else:
            return buoy_error + size_error + position_error

    def check_pos(self, mapping, classification):
        """
        Discard classifications that result in buoys that are too far or too
        close to each other
        """
        coords = []
        for buoy in mapping:
            # if classification[buoy] is None and buoy is not None:
            if classification[buoy] is not None:
                for c in coords:
                    if abs(classification[buoy].center[1] - c[1]) > self.options['max_y_diff']:
                        return False
                    if abs(classification[buoy].center[0] - c[0]) < self.options['min_x_diff']:
                        return False
                coords.append(classification[buoy].center)
        return True

    def classify_buoys(self, buoys, candidates):
        """
        buoys: list of BuoyData objects
        candidates: list of (candidate, color) tuples
        """
        # Possible mappings for the # of candidates to the 3 buoys
        buoys = list(buoys)
        while len(candidates) > len(buoys):
            buoys.append(None)
        possible_mappings = permutations(buoys, len(candidates))
        assignments = []
        max_buoys_classified = 0
        for mapping in possible_mappings:
            total_error = 0
            classification = {buoy: None for buoy in mapping if buoy is not None}
            buoys_classified = 0
            # Iterate through one-to-one mappings of buoys to candidates
            for i in range(len(candidates)):
                error = self.calculate_error(mapping[i], candidates[i])
                if error >= INVALID_BUOY_ERROR:
                    classification[mapping[i]] = None
                else:
                    classification[mapping[i]] = candidates[i][0]
                    # print("{}".format(classification[mapping[i]]))
                    total_error += error
                    buoys_classified += 1
            # # UNCOMMENT THIS WHEN TUNING BUOY REFERENCE COLORS
            # print("Considering: " + str([buoy.color if\
            #        classification[buoy] is not None else (-1, -1, -1) for buoy in mapping]))
            # print(total_error)

            if buoys_classified > 0 and total_error < self.options['max_error'] and \
               buoys_classified >= max_buoys_classified and self.check_pos(mapping, classification):
                max_buoys_classified = buoys_classified
                assignments.append((total_error, buoys_classified, classification))

        # Consider assignments with the most number of classified buoys
        assignments = filter(lambda x: x[1] == max_buoys_classified, assignments)
        min_error = self.options['max_error']
        optimal_assignment = (0, 0, {buoy: None for buoy in buoys})
        for assignment in assignments:
            if assignment[0] < min_error:
                min_error = assignment[0]
                optimal_assignment = assignment
        # print(len(optimal_assignment[2]), min_error)
        for (k, v) in optimal_assignment[2].items():
            if v is not None:
                for candidate in enumerate(candidates):
                    if candidate[1][0].center == v.center:
                        # print(k.color, candidate[1][1])
                        break
        return optimal_assignment

    def process(self, mat):
        start_time = time.time()

        # Downscale original image for performance
        mat = cv2.resize(mat, (int(mat.shape[1] * constants.DOWNSCALE_RATIO),
                               int(mat.shape[0] * constants.DOWNSCALE_RATIO)))
        image_size = mat.shape[1] * mat.shape[0]

        if self.options['use_color_correction']:
            mat = balance(mat)

        self.post('orig', mat)
        # Apply blur for noise reduction
        blurred = cv2.GaussianBlur(mat,
                                   (self.options_dict['blur_kernel'].value * 2 + 1,
                                    self.options_dict['blur_kernel'].value * 2 + 1),
                                   0)

        # Ignore anything outside of the center viewcone
        mask_e = np.zeros((mat.shape[0], mat.shape[1]), np.uint8)
        cv2.ellipse(mask_e,
                    (int(mask_e.shape[1] / 2), int(mask_e.shape[0] / 2)),
                    (self.options['ex'], self.options['ey']),
                    0, 0, 360, (255, 255, 255), -1)

        if self.options['debug']:
            self.post('mask', mask_e)

        hls = cv2.cvtColor(blurred, cv2.COLOR_BGR2HLS)
        hls_split = cv2.split(hls)
        lab = cv2.cvtColor(blurred, cv2.COLOR_BGR2LAB)
        lab_split = cv2.split(lab)
        ycrcb = cv2.cvtColor(blurred, cv2.COLOR_BGR2YCrCb)
        ycrcb_split = cv2.split(ycrcb)
        hsv = cv2.cvtColor(blurred, cv2.COLOR_BGR2HSV)
        hsv_split = cv2.split(hsv)

        def morph(channels_and_names, channel_set_name,
                                    debug=False):
            """
            channels_and_names: [(channel, name), ...] - The mat will be be
                thresholded according to the parameters identified by `name`
                and `channel_set_name`.
            channel_set_name: Name of the channel set (prepend to all posts)
            debug: Enable debugging output (posting images)
            """
            # Apply thresholds for segmentation
            channels_and_bounds = list(starmap(
                lambda channel, name: (channel,
                                       self.options["{}_{}_min".format(channel_set_name, name)],
                                       self.options["{}_{}_max".format(channel_set_name, name)]),
                channels_and_names))
            thresheds = list(starmap(cv2.inRange, channels_and_bounds))
            threshed = reduce(cv2.bitwise_and, thresheds, mask_e)

            # Erode to reduce noise and smoothen boundaries
            erode_size = self.options["{}_erode_kernel_size".format(channel_set_name)]
            dilate_size = self.options["{}_dilate_kernel_size".format(channel_set_name)]
            eroded = cv2.erode(threshed, cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (erode_size, erode_size)))
            morphed = cv2.dilate(eroded, cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (dilate_size, dilate_size)))

            if debug:
                for thresh, (_, channel_name) in zip(thresheds, channels_and_names):
                    self.post("{}_{}_threshed".format(channel_set_name, channel_name), thresh)
                self.post("{}_threshed".format(channel_set_name), threshed)
                self.post("{}_morphed".format(channel_set_name), morphed)

            return morphed

        red_channels_and_names = [
            (lab_split[1],   "lab_a"),
            (lab_split[2],   "lab_b"),
            (ycrcb_split[2], "ycrcb_cb"),
            (hls_split[0],   "hls_h"),
        ]
        red_morphed = morph(
            red_channels_and_names,
            "red",
            debug=self.options["red_debug"])

        yellow_channels_and_names = [
            (lab_split[1],   "lab_a"),
            (lab_split[2],   "lab_b"),
            (ycrcb_split[2], "ycrcb_cb"),
            (hls_split[0],   "hls_h"),
        ]
        yellow_morphed = morph(
            yellow_channels_and_names,
            "yellow",
            debug=self.options["yellow_debug"])

        green_channels_and_names = [
            (lab_split[0],   "lab_l"),
            (hsv_split[2],   "hsv_v"),
            (hsv_split[0],   "hsv_h"),
            (ycrcb_split[0], "ycrcb_y"),
        ]
        green_morphed = morph(
            green_channels_and_names,
            "green",
            debug=self.options["green_debug"])

        # De-conflict green buoy with yellow and red
        green_morphed = green_morphed - yellow_morphed - red_morphed

        total_morphed = green_morphed | red_morphed | yellow_morphed

        _, contours, _ = cv2.findContours(total_morphed.copy(),
                                          cv2.RETR_EXTERNAL,
                                          cv2.CHAIN_APPROX_SIMPLE)

        # Use hough circle transform as heuristic for finding buoy contours
        circles_mat = lab_split[2].copy()
        circles_mat = cv2.medianBlur(circles_mat, 11)
        circles = cv2.HoughCircles(circles_mat, cv2.HOUGH_GRADIENT, 1, 50,
                                   param1=100, param2=30, minRadius=1, maxRadius=200)
        if self.options['debug']:
            self.post("circles_mat", circles_mat)

        if circles is not None:
            circle_mask = np.zeros(mat.shape, np.uint8)
            for circle in circles[0, :]:
                cv2.circle(circle_mask, (circle[0], circle[1]), circle[2], (255, 255, 255), -1)
            morphed_hough_heuristic = red_morphed.copy() & cv2.cvtColor(circle_mask, cv2.COLOR_BGR2GRAY)
            _, heuristic_contours, _ = cv2.findContours(morphed_hough_heuristic.copy(),
                                                        cv2.RETR_EXTERNAL,
                                                        cv2.CHAIN_APPROX_SIMPLE)
            contours.extend(heuristic_contours)

            if self.options['debug']:
                self.post("circle_mask", circle_mask)
                self.post("img_mat", mat.copy() & circle_mask)
                self.post("morphed_hough_heuristic", morphed_hough_heuristic)

        contourAreas = []
        for contour in contours:
            contourArea = cv2.contourArea(contour)
            if contourArea >= image_size * self.options['min_percent_frame']:
                contourAreas.append(ContourAreaData(contour, contourArea))
        # Sort contours in descending order by their areas
        contourAreas = sorted(contourAreas, key=lambda x: -x.area)[:CONTOUR_CIRCULARITY_HEURISTIC_LIMIT]

        contourScores = []
        for contourArea in contourAreas:
            center, radius = cv2.minEnclosingCircle(contourArea.contour)
            circularity = contourArea.area / (math.pi * radius ** 2)
            if circularity >= self.options['min_circularity']:
                # Calculate a heuristic score defined by
                # (contour cirularity)^1.25 * (contour area)
                heuristic_score = (circularity**1.25) * contourArea.area
                contourScores.append(ContourScoreData(contourArea.contour,
                                     contourArea.area, circularity, heuristic_score, center, radius))
        # Sort contours in descending order by their heuristic score
        contourScores = sorted(contourScores, key=lambda x: -x.score)[:CONTOUR_SCALED_HEURISTIC_LIMIT]

        if contourScores:
            if self.options['debug']:
                contoursMat = mat.copy()
                cv2.drawContours(contoursMat, [cand.contour for cand in contourScores], -1, white, 3)
                self.post('contours', contoursMat)
                avgMat = contoursMat.copy()

            buoyContours = sorted(contourScores, key=lambda x: -x.score)[:CONTOUR_CLASSIFICATION_LIMIT]
            total_mask = np.zeros(mat.shape, np.uint8)
            buoy_results = []
            # Only look at top contour candidates.
            for i, buoy_candidate in enumerate(buoyContours):
                # Calculate average L, A, and B values for each contour
                mask = np.zeros(lab.shape, np.uint8)
                cv2.drawContours(mask, [buoy_candidate.contour], 0, white, cv2.FILLED)
                if i == 0:
                    cv2.drawContours(total_mask, [buoy_candidate.contour], 0, green, 1)
                else:
                    cv2.drawContours(total_mask, [buoy_candidate.contour], 0, white, 1)

                just_buoy = cv2.bitwise_and(lab, mask)
                just_buoy_split = cv2.split(just_buoy)

                l_ch = just_buoy_split[0]
                a_ch = just_buoy_split[1]
                b_ch = just_buoy_split[2]

                l_ch_nonzero = l_ch[l_ch != 0]
                a_ch_nonzero = a_ch[a_ch != 0]
                b_ch_nonzero = b_ch[b_ch != 0]

                # Clip outliers
                l_min_max = np.percentile(l_ch_nonzero, [0.2, 99.8])
                a_min_max = np.percentile(a_ch_nonzero, [0.2, 99.8])
                b_min_max = np.percentile(b_ch_nonzero, [0.2, 99.8])

                l_ch_nonzero = np.clip(l_ch_nonzero, l_min_max[0], l_min_max[1])
                a_ch_nonzero = np.clip(a_ch_nonzero, a_min_max[0], a_min_max[1])
                b_ch_nonzero = np.clip(b_ch_nonzero, b_min_max[0], b_min_max[1])

                l_buoy_area = max(l_ch_nonzero.size, 1)
                a_buoy_area = max(a_ch_nonzero.size, 1)
                b_buoy_area = max(b_ch_nonzero.size, 1)
                avg_l = np.sum(l_ch_nonzero) / l_buoy_area
                avg_a = np.sum(a_ch_nonzero) / a_buoy_area
                avg_b = np.sum(b_ch_nonzero) / b_buoy_area

                if self.options['debug']:
                    int_pt = lambda pt: (int(pt[0]), int(pt[1]))
                    tag(avgMat, str("({},{},{})".format(int(avg_l), int(avg_a), int(avg_b))), int_pt(buoy_candidate.center))

                # print(avg_l, avg_a, avg_b)
                buoy_results.append((buoy_candidate, (avg_l, avg_a, avg_b), buoy_candidate.area))

            if self.options['debug']:
                self.post('avg LAB', avgMat)

            # Best fit classification
            [buoy.reset_frame_state() for buoy in BUOYS]
            total_error, buoys_classified, classification = self.classify_buoys(BUOYS, buoy_results)
            # print(buoys_classified)
            for (buoy, contour) in classification.items():
                if buoy is None:
                    continue
                if contour is not None:
                    buoy.contour = contour
                    self.set_buoy_results(mat, buoy, contourScores)
                else:
                    buoy.reset_frame_state()
                    buoy.zero_results()

            buoy_contours_mat = mat.copy()
            for buoy in BUOYS:
                if buoy.contour is not None:
                    cv2.drawContours(buoy_contours_mat, [buoy.contour.contour], -1, buoy.color, 6)

            self.post("total_mask", total_mask)
            self.post("All buoys", buoy_contours_mat)

        else:
            [buoy.zero_results() for buoy in BUOYS]

        for buoy in BUOYS:
            self.fill_single_camera_direction(buoy.results)
            buoy.set_shm_group()
        end_time = time.time()


if __name__ == '__main__':
    Buoys('forward', options)()
