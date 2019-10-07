#!/usr/bin/env python3
import os
import sys
import shm
import cv2
import numpy as np
from collections import namedtuple

from conf.vehicle import VEHICLE, is_mainsub
from vision import options
from vision.modules.base import ModuleBase
from vision.framework.feature import outer_contours, contour_area, contour_centroid, min_enclosing_circle, min_enclosing_rect
from vision.framework.transform import resize, simple_gaussian_blur, morph_remove_noise, morph_close_holes, dilate, erode, rect_kernel
from vision.framework.helpers import to_umat, from_umat, to_odd
from vision.framework.color import bgr_to_lab, gray_to_bgr, range_threshold
from vision.framework.draw import draw_contours, draw_circle, draw_text


CUAUV_LOCALE = os.environ['CUAUV_LOCALE']

OPTS_ODYSSEUS = [
    options.IntOption('lab_l_ref', 255, 0, 255),
    options.IntOption('lab_a_ref', 163, 0, 255),
    options.IntOption('lab_b_ref', 180, 0, 255),
    options.IntOption('color_dist_thresh', 45, 0, 255),
    options.IntOption('blur_kernel', 3, 0, 255),
    options.IntOption('blur_std', 10, 0, 500),
    options.DoubleOption('resize_width_scale', 0.5, 0, 1),
    options.DoubleOption('resize_height_scale', 0.5, 0, 1),
    options.IntOption('dilate_kernel', 1, 0, 255),
    options.IntOption('erode_kernel', 1, 0, 255),
    options.IntOption('min_contour_area', 30, 0, 500),
    options.DoubleOption('min_contour_rect', 0.4, 0, 1),
    options.DoubleOption('min_contour_ratio', 4.5, 0, 10),
    options.DoubleOption('max_angle_from_vertical', 15, 0, 90),
    options.DoubleOption('min_length', 15, 0, 500),
    options.IntOption('auto_distance_percentile', 25, 0, 100),
    options.IntOption('nonblack_thresh', 900, 0, 10000),
    options.IntOption('water_a_thresh', 20, 0, 255),
    options.IntOption('water_b_thresh', 25, 0, 255),
    options.BoolOption('debug', True),
]

OPTS_AJAX = [
    options.IntOption('lab_l_ref', 255, 0, 255),
    options.IntOption('lab_a_ref', 175, 0, 255),
    options.IntOption('lab_b_ref', 169, 0, 255),
    options.IntOption('color_dist_thresh', 40, 0, 255),
    options.IntOption('blur_kernel', 3, 0, 255),
    options.IntOption('blur_std', 10, 0, 500),
    options.DoubleOption('resize_width_scale', 0.25, 0, 1),
    options.DoubleOption('resize_height_scale', 0.25, 0, 1),
    options.IntOption('dilate_kernel', 1, 0, 255),
    options.IntOption('erode_kernel', 1, 0, 255),
    options.IntOption('min_contour_area', 30, 0, 500),
    options.DoubleOption('min_contour_ratio', 4.5, 0, 10),
    options.DoubleOption('min_contour_rect', 0.4, 0, 1),
    options.DoubleOption('max_angle_from_vertical', 15, 0, 90),
    options.DoubleOption('min_length', 15, 0, 500),
    options.IntOption('auto_distance_percentile', 25, 0, 100),
    options.IntOption('nonblack_thresh', 600, 0, 10000),
    options.IntOption('water_a_thresh', 10, 0, 255),
    options.IntOption('water_b_thresh', 10, 0, 255),
    options.BoolOption('debug', True),
]

#OPTS_SIM = [
#    options.IntOption('lab_l_ref', 0, 0, 255),
#    options.IntOption('lab_a_ref', 170, 0, 255),
#    options.IntOption('lab_b_ref', 180, 0, 255),
#    options.IntOption('color_dist_thresh', 35, 0, 255),
#    options.IntOption('blur_kernel', 3, 0, 255),
#    options.IntOption('blur_std', 10, 0, 500),
#    options.DoubleOption('resize_width_scale', 0.5, 0, 1),
#    options.DoubleOption('resize_height_scale', 0.5, 0, 1),
#    options.IntOption('dilate_kernel', 1, 0, 255),
#    options.IntOption('erode_kernel', 1, 0, 255),
#    options.IntOption('min_contour_area', 30, 0, 500),
#    options.DoubleOption('min_contour_rect', 0.4, 0, 1),
#    options.DoubleOption('min_contour_ratio', 5, 0, 10),
#    options.DoubleOption('max_angle_from_vertical', 15, 0, 90),
#    options.DoubleOption('min_length', 15, 0, 500),
#    options.IntOption('auto_distance_percentile', 15, 0, 100),
#    options.IntOption('nonblack_thresh', 1000, 0, 10000),
#    options.IntOption('water_a_thresh', 20, 0, 255),
#    options.IntOption('water_b_thresh', 25, 0, 255),
#    options.BoolOption('debug', True),
#]

OPTS_SIM = OPTS_ODYSSEUS if VEHICLE == 'odysseus' else OPTS_AJAX

REFERENCE_BRIGHTNESS = 190 if is_mainsub else 190
CUTOFF_SCALAR = 10 if is_mainsub else 7

ContourFeats = namedtuple('ContourFeats', ['contour', 'area', 'x', 'y', 'rect', 'angle', 'length', 'ratio'])


def try_index(arr, idx):
    if idx < len(arr):
        return arr[idx]
    return None


def thresh_color_distance(split, color, distance, auto_distance_percentile=None, ignore_channels=[], weights=[1, 1, 1]):
    for idx in ignore_channels:
        weights[idx] = 0
    weights /= np.linalg.norm(weights)
    dists = np.zeros(split[0].shape, dtype=np.float32)
    for i in range(3):
        if i in ignore_channels:
            continue
        dists += weights[i] * (np.float32(split[i]) - color[i])**2
    if auto_distance_percentile:
        distance = min(np.percentile(dists, auto_distance_percentile), distance**2)
    else:
        distance = distance**2
    return range_threshold(dists, 0, distance), np.uint8(np.sqrt(dists))


def filter_duplicates_sorted_by_x(contour_feats):
    MIN_DIST_BETWEEN_PIPES = 30
    res = []
    last_x = -MIN_DIST_BETWEEN_PIPES
    last_len = 0
    for c in contour_feats:
        if c.x - last_x > MIN_DIST_BETWEEN_PIPES:
            last_x = c.x
            last_len = c.length
            res.append(c)
        elif last_len < c.length:
            last_x = c.x
            last_len = c.length
            if res:
                res.pop(-1)
            res.append(c)
    return res


class Gate(ModuleBase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def post_contours(self, name, h, w, contour_feats):
        if not self.options['debug']:
            return
        tmp = np.zeros((h, w, 3))
        draw_contours(tmp, [c.contour for c in contour_feats], color=(255, 0, 0), thickness=-1)
        self.post(name, tmp)

    def process(self, *mats):
        results = shm.gate_vision.get()
        h, w, _ = mats[0].shape
        h = int(h * self.options['resize_height_scale'])
        w = int(w * self.options['resize_width_scale'])
        results.img_height = h
        results.img_width = w
        mat = resize(mats[0], w, h)
        #print(np.mean(mat))
        avg_brightness_ratio = np.mean(mat) / REFERENCE_BRIGHTNESS
        nonblack_thresh_dist = self.options['nonblack_thresh'] * avg_brightness_ratio

        lab, lab_split = bgr_to_lab(mat)
        median_a = np.median(lab_split[1])
        median_b = np.median(lab_split[2])
        median_filter_a = range_threshold(lab_split[1], median_a - self.options['water_a_thresh'], median_a + self.options['water_a_thresh'])
        median_filter_b = range_threshold(lab_split[2], median_b - self.options['water_b_thresh'], median_b + self.options['water_b_thresh'])
        if self.options['debug']:
            self.post('median filter a', median_filter_a)
            self.post('median filter b', median_filter_b)
        nonwater_mask, _ = gray_to_bgr(255 - (median_filter_a & median_filter_b))
        self.post('nonwater', nonwater_mask)
        # Tuned for a 320x256 image
        vehicle_depth = shm.kalman.depth.get()
        reflection_cutoff = min(h, int(max(0, 3 - vehicle_depth)**2 * CUTOFF_SCALAR))
        mat[:reflection_cutoff] *= 0
        tmp = mat.copy()
        draw_text(tmp, 'Depth: {:.2f}'.format(vehicle_depth), (30, 30), 0.5, color=(255, 255, 255))
        self.post('mat', tmp)
        #lab, lab_split = bgr_to_lab(mat)
        #nonblack_mask, _ = gray_to_bgr(np.uint8(255 * (lab_split[0] > self.options['nonblack_thresh'])))
        nonblack_mask, _ = gray_to_bgr(np.uint8(255 * (np.var(mat, axis=2) > nonblack_thresh_dist)))
        self.post('nonblack', nonblack_mask)
        mat &= nonblack_mask
        mat &= nonwater_mask
        mat = to_umat(mat)
        mat = simple_gaussian_blur(mat, to_odd(self.options['blur_kernel']),
                                   self.options['blur_std'])
        lab, lab_split = bgr_to_lab(mat)
        threshed, dists = thresh_color_distance([lab_split[0], lab_split[1], lab_split[2]],
                                                [self.options['lab_l_ref'], self.options['lab_a_ref'],
                                                     self.options['lab_b_ref']],
                                         self.options['color_dist_thresh'], auto_distance_percentile=self.options['auto_distance_percentile'],
                                         ignore_channels=[0], weights=[2, 0, 15])
        if self.options['debug']:
            self.post('threshed', threshed)
            self.post('dists', dists)
        dilated = dilate(threshed, rect_kernel(self.options['dilate_kernel']))
        if self.options['debug']:
            self.post('dilated', dilated)
        eroded = erode(dilated, rect_kernel(self.options['erode_kernel']))
        if self.options['debug']:
            self.post('eroded', eroded)
        contours = outer_contours(eroded)
        areas = [*map(contour_area, contours)]
        centroids = [*map(contour_centroid, contours)]
        xs = [c[0] for c in centroids]
        ys = [c[1] for c in centroids]
        rects = [*map(min_enclosing_rect, contours)]
        lengths = [max(r[1]) for r in rects]
        ratios = [max(r[1]) / (1e-30 + min(r[1])) for r in rects]
        vehicle_roll = shm.kalman.roll.get()
        lines = [cv2.fitLine(c, cv2.DIST_L2, 0, 0.01, 0.01) for c in contours]
        angles = [np.degrees(np.arctan2(line[1], line[0]))[0] for line in lines]
        angles = [min(abs(90 - a - vehicle_roll), abs(-90 - a - vehicle_roll)) for a in angles]
        rectangularities = [a / (1e-30 + rect[1][0] * rect[1][1]) for (c, a, rect) in zip(contours, areas, rects)]
        contours = [ContourFeats(*feats) for feats in zip(contours, areas, xs, ys, rectangularities, angles, lengths, ratios)]
        contours = [*filter(lambda c: c.area > self.options['min_contour_area'], contours)]
        self.post_contours('area', h, w, contours)
        contours = [*filter(lambda c: c.angle < self.options['max_angle_from_vertical'], contours)]
        self.post_contours('angle', h, w, contours)
        contours = [*filter(lambda c: c.length > self.options['min_length'], contours)]
        self.post_contours('length', h, w, contours)
        #contours = [*filter(lambda c: c.rect > self.options['min_contour_rect'], contours)]
        #self.post_contours('rect', h, w, contours)
        contours = [*filter(lambda c: c.ratio > self.options['min_contour_ratio'], contours)]
        self.post_contours('ratio', h, w, contours)
        contours = sorted(contours, key=lambda c: c.area)[:6]
        contours_by_x = sorted(contours, key=lambda c: c.x)
        contours_by_x = filter_duplicates_sorted_by_x(contours_by_x)
        leftmost = try_index(contours_by_x, 0)
        middle = try_index(contours_by_x, 1)
        rightmost = try_index(contours_by_x, 2)
        tmp = np.zeros((h, w, 3))
        results.leftmost_visible = leftmost is not None
        results.middle_visible = middle is not None
        results.rightmost_visible = rightmost is not None
        draw_text(tmp, 'Roll: {:.2f}'.format(vehicle_roll), (30, 30), 0.5, color=(255, 255, 255))
        if leftmost is not None:
            draw_contours(tmp, [leftmost.contour], color=(255, 0, 0), thickness=-1)
            draw_circle(tmp, (leftmost.x, leftmost.y), 5, color=(255, 255, 255), thickness=-1)
            results.leftmost_x = leftmost.x
            results.leftmost_y = leftmost.y
            results.leftmost_len = leftmost.length
        if middle is not None:
            draw_contours(tmp, [middle.contour], color=(0, 255, 0), thickness=-1)
            draw_circle(tmp, (middle.x, middle.y), 5, color=(255, 255, 255), thickness=-1)
            results.middle_x = middle.x
            results.middle_y = middle.y
            results.middle_len = middle.length
        if rightmost is not None:
            draw_contours(tmp, [rightmost.contour], color=(0, 0, 255), thickness=-1)
            draw_circle(tmp, (rightmost.x, rightmost.y), 5, color=(255, 255, 255), thickness=-1)
            results.rightmost_x = rightmost.x
            results.rightmost_y = rightmost.y
            results.rightmost_len = rightmost.length
        shm.gate_vision.set(results)
        self.post('contours', tmp)


if __name__ == '__main__':
    Gate('forward', OPTS_SIM if CUAUV_LOCALE == 'simulator' else OPTS_ODYSSEUS if VEHICLE == 'odysseus' else OPTS_AJAX)()
