#!/usr/bin/env python3

import math

from collections import namedtuple
from functools import reduce

import cv2
import numpy as np

import shm

from vision.modules import buoy_common
from vision.modules.base import ModuleBase
from vision.vision_common import green, red, white, yellow
from vision import options

options = [ options.IntOption('c_thresh', -25, -100, 100), 
            options.IntOption('red_c_thresh', 30, -100, 100), 
            options.IntOption('green_c_thresh', 40, -100, 100), 
            options.IntOption('cr_c_thresh', 25, -100, 100), 
            options.IntOption('h_c_thresh', -5, -100, 100), 
            options.IntOption('filter_p1', 5, 1, 100),
            options.IntOption('filter_p2', 75, 1, 1000),
            options.IntOption('filter_p3', 75, 1, 1000),
            options.IntOption('block_size', 551, 1, 1500),
            options.IntOption('red_block_size', 551, 1, 1500),
            options.IntOption('green_block_size', 551, 1, 1500),
            options.IntOption('cr_block_size', 551, 1, 1500),
            options.IntOption('h_block_size', 311, 1, 1500),
            options.IntOption('blur_size', 1, 1, 150, lambda x: x % 2 == 1),
            options.IntOption('kernel_size', 31, 1, 151),
            options.IntOption('min_heuristic_score', 1, 0, 1000),
            options.DoubleOption('min_circularity', 0.25, 0.1, 0.95),
            options.DoubleOption('min_percent_frame', 0.0002, 0, 0.01) ]

CONTOUR_CIRCULARITY_HEURISTIC_LIMIT = 10
CONTOUR_SCALED_HEURISTIC_LIMIT = 5
CONTOUR_CLASSIFICATION_LIMIT = 3
ContourAreaData = namedtuple('ContourAreaData', ['contour', 'area'])
ContourScoreData = namedtuple('ContourScoreData', ['contour', 'area', 'circularity', 'score', 'center', 'radius'])

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

    def set_results(self, image_size, contour_list):
        assert self.contour is not None # This must be called after contour has been set.
        self.results.center_x = int(self.contour.center[0])
        self.results.center_y = int(self.contour.center[1])
        self.results.top_x = int(self.contour.center[0])
        self.results.top_y = int(self.contour.center[1] - self.contour.radius / 2)
        self.results.area = self.contour.area
        self.results.heuristic_score = self.contour.score
        self.results.percent_frame = 100 * self.contour.area / image_size
        self.results.probability = self.contour.score / reduce(lambda acc, x: acc + x.score, contour_list, 0)
        # TODO XXX HAX Please fix this probabilities.
        self.results.probability = max(self.results.probability, 0.5)

    def set_shm_group(self):
        self.shm_group.set(self.results)

# RED, GREEN, BLUE
red_ref = (130, 100, 50)
green_ref = (60, 120, 90)
yellow_ref = (10, 200, 40)

RED_BUOY = BuoyData(shm.red_buoy_results, red, red_ref)
GREEN_BUOY = BuoyData(shm.green_buoy_results, green, green_ref)
YELLOW_BUOY = BuoyData(shm.yellow_buoy_results, yellow, yellow_ref)
buoys = [RED_BUOY, GREEN_BUOY, YELLOW_BUOY]

def objective_function(buoys, mapping):
    error = 0
    for buoy in buoys:
        if buoy in mapping:
          error_vec = np.array((buoy.reference_color)) - \
                      np.array((mapping[buoy][1]))
          error += error_vec.dot(error_vec)
    return error

def classify_helper(buoys, mapping, candidates):
    buoys_left = len([buoy for buoy in buoys if buoy not in mapping])
    if not candidates or not buoys_left:
        return mapping, objective_function(buoys, mapping)

    best_mapping = None
    min_score = None

    for buoy in buoys:
      next_cand = candidates[0]
      if buoy not in mapping:
        new_mapping = mapping.copy()
        new_mapping[buoy] = next_cand
        finished_mapping, score = classify_helper(buoys, new_mapping, candidates[1:])
        if min_score is None or score < min_score:
            min_score = score
            best_mapping = finished_mapping

    return best_mapping, min_score

def classify_buoys(buoys, candidates):
    """
        buoys is a list of BuoyData objects
        candidates is list of (candidate, color) tuples
    """
    mapping, score = classify_helper(buoys, {}, candidates)
    for buoy, cand in mapping.items():
        buoy.contour = cand[0]
    return score

class Buoys(ModuleBase):
    def process(self, mat):
        cspace = cv2.cvtColor(mat, cv2.COLOR_RGB2YCrCb)
        red, green, blue = cv2.split(mat)
        red = cv2.bilateralFilter(red, self.options['filter_p1'], self.options['filter_p2'], self.options['filter_p3'])
        rthreshed = cv2.adaptiveThreshold(red, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, self.options['red_block_size'], self.options['red_c_thresh']) 
        green = cv2.bilateralFilter(green, self.options['filter_p1'], self.options['filter_p2'], self.options['filter_p3'])
        gthreshed = cv2.adaptiveThreshold(green, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, self.options['green_block_size'], self.options['green_c_thresh'])
        y, cr, cb = cv2.split(cspace)
        cr = cv2.bilateralFilter(cr, self.options['filter_p1'], self.options['filter_p2'], self.options['filter_p3'])
        cthreshed = cv2.adaptiveThreshold(cr, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, self.options['cr_block_size'], self.options['cr_c_thresh'])
        hsv = cv2.cvtColor(mat, cv2.COLOR_RGB2HSV)
        h, s, v = cv2.split(hsv)
        h = cv2.bilateralFilter(h, self.options['filter_p1'], self.options['filter_p2'], self.options['filter_p3'])
        hthreshed = cv2.adaptiveThreshold(h, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, self.options['h_block_size'], self.options['h_c_thresh'])
        self.post('hthresh', hthreshed)
        self.post('cthresh', cthreshed)
        self.post('rthresh', rthreshed)
        self.post('gthresh', gthreshed)
        cspace_split = cv2.split(cspace)
        red_channel  = cv2.split(mat)[0]
        blue = cv2.bilateralFilter(blue, self.options['filter_p1'], self.options['filter_p2'], self.options['filter_p3'])
        bthreshed = cv2.adaptiveThreshold(blue, 255,
                                         cv2.ADAPTIVE_THRESH_MEAN_C,
                                         cv2.THRESH_BINARY,
                                         self.options['block_size'],
                                         self.options['c_thresh'])

        self.post('bthreshed', bthreshed)
        threshed = cv2.bitwise_and(hthreshed, cv2.bitwise_or(cthreshed, cv2.bitwise_and(rthreshed, cv2.bitwise_or(bthreshed, gthreshed))))

        kernel = np.ones((self.options['kernel_size'],
                          self.options['kernel_size']), np.uint8)
        #morphed = cv2.morphologyEx(threshed, cv2.MORPH_OPEN, kernel)
        morphed = cv2.medianBlur(threshed, self.options['kernel_size'])

        _, contours, hierarchy = cv2.findContours(morphed.copy(), cv2.RETR_EXTERNAL,
                                                    cv2.CHAIN_APPROX_SIMPLE)

        image_size = mat.shape[0] * mat.shape[1]

        contourAreas = []
        for contour in contours:
            contourArea = cv2.contourArea(contour)
            if contourArea >= image_size * self.options['min_percent_frame']:
                contourAreas.append(ContourAreaData(contour, contourArea))
        contourAreas = sorted(contourAreas, key=lambda x: -x.area)[:CONTOUR_CIRCULARITY_HEURISTIC_LIMIT]

        contourScores = []
        for contourArea in contourAreas:
            center, radius = cv2.minEnclosingCircle(contourArea.contour)
            circularity = contourArea.area / (math.pi * radius ** 2)
            heuristic_score = circularity * contourArea.area / center[0] # stupid Teagle hack
            if circularity >= self.options['min_circularity'] and\
               heuristic_score >= self.options['min_heuristic_score']:
                contourScores.append(ContourScoreData(contourArea.contour,
                    contourArea.area, circularity, heuristic_score, center, radius))
        contourScores = sorted(contourScores, key=lambda x: -x.score)[:CONTOUR_SCALED_HEURISTIC_LIMIT]

        if contourScores:
            topContour = min(contourScores, key=lambda x: x.center[1]) # Zero is top-left of image
            topContour = topContour._replace(score=topContour.score / 2) # Reduce score of top contour
            #try:
            #contourScores = [x if x != topContour else _topContour for x in contourScores]
            #except:
            #    print(topContour)

            contoursMat = mat.copy()
            cv2.drawContours(contoursMat, [cand.contour for cand in contourScores], -1, white, 3)

            buoyContours = sorted(contourScores, key=lambda x: x.score)
            total_mask = np.zeros(mat.shape, np.uint8)
            buoy_results = []
            # Only look at top contour candidates.
            for i, buoy_candidate in enumerate(buoyContours[:CONTOUR_CLASSIFICATION_LIMIT]):
                mask = np.zeros(mat.shape, np.uint8)
                cv2.drawContours(mask, [buoy_candidate.contour], 0, white, -1)
                cv2.drawContours(total_mask, [buoy_candidate.contour], 0, white, -1)

                just_buoy = cv2.bitwise_and(mat, mask)
                just_buoy_split = cv2.split(just_buoy)

                red_ch = just_buoy_split[2]
                green_ch = just_buoy_split[1]
                blue_ch = just_buoy_split[0]

                red_buoy = red_ch[np.nonzero(red_ch)]
                green_buoy = green_ch[np.nonzero(green_ch)]
                blue_buoy = blue_ch[np.nonzero(blue_ch)]
                total_area = red_buoy.shape[0]

                avg_red = sum(red_buoy) / total_area
                avg_green = sum(green_buoy) / total_area
                avg_blue = sum(blue_buoy) / total_area
                print(avg_red, avg_green, avg_blue)
                buoy_results.append((buoy_candidate, (avg_red, avg_green, avg_blue)))

            # OLD CLASSIFICATION THAT USES HARDCODED THRESHOLDS
            # Here we must classify buoys as red, green, or yellow.
            # If there is only one buoy, we use a sketchy threshold!
            #if len(buoy_results) == 1:
            #  if buoy_results[0][1][0] > self.options['red_channel_classification_threshold']:
            #    RED_BUOY.contour = buoy_results[0][0]
            #  else:
            #    GREEN_BUOY.contour = buoy_results[0][0]

            ## If there is more than one, it is a bit easier.
            #elif len(buoy_results) > 1:
            #  sorted_results = sorted(buoy_results, key=lambda res: res[1][0])
            #  RED_BUOY.contour = sorted_results[-1][0]
            #  green_result = sorted_results[0]
            #  if green_result[1][0] < self.options['green_buoy_max_red']:
            #    GREEN_BUOY.contour = green_result[0]

            # NEW CLASSIFICATION THAT USES BEST FIT!
            [buoy.reset_frame_state() for buoy in buoys]
            score = classify_buoys(buoys, buoy_results)

            for buoy in buoys:
              if buoy.contour is not None:
                buoy.set_results(mat.shape[0] * mat.shape[1], contourScores)
              else:
                buoy.zero_results()

            buoy_contoursMat = mat.copy()
            for buoy in buoys:
              if buoy.contour is not None:
                cv2.drawContours(buoy_contoursMat, [buoy.contour.contour], -1, buoy.color, 6)

            self.post("All buoys", buoy_contoursMat)

        else:
            [buoy.zero_results() for buoy in buoys]

        self.post('orig', mat)
        self.post('threshed', threshed)
        self.post('morphed', morphed)
        if contourScores:
            self.post('contours', contoursMat)

        for buoy in buoys:
            self.fill_single_camera_direction(buoy.results)
            buoy.set_shm_group()

if __name__ == '__main__':
    Buoys('forward', options)()
