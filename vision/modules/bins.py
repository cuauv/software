#!/usr/bin/env python3

from collections import namedtuple

import cv2
import numpy as np

import shm

from vision import options
from vision.vision_common import red, green, blue, white, cyan, yellow, \
                                 draw_angled_arrow, \
                                 get_angle_from_rotated_rect, \
                                 zero_vision_group
from vision.modules.base import ModuleBase


CONTOUR_HEURISTIC_LIMIT = 5
CONTOUR_SCALED_HEURISTIC_LIMIT = 2

options = [
     options.IntOption('block_size_cover', 501, 0, 5500, lambda x: x % 2 == 1),
     options.IntOption('c_thresh_cover', 26, -100, 100),
     options.IntOption('blur_size_cover',  5, 1, 255, lambda x: x % 2 == 1),
     options.IntOption('block_size_cutout', 401, 0, 1500),
     options.IntOption('c_thresh_cutout', 10, -100, 100),
     options.IntOption('blur_size_cutout',  7, 1, 255, lambda x: x % 2 == 1),
     options.IntOption('kernel_size',  51, 1, 101, lambda x: x % 2 == 1),
     options.DoubleOption('min_area_percent', 0.001, 0, 0.15),
     options.DoubleOption('min_rectangularity', 0.6, 0, 1),
     options.BoolOption('debugging', True)
]

class ShmGroup:
  def __init__(self, shm_group):
    self.shm_group = shm_group
    self.cached_group = shm_group.get()

  def set(self):
    self.shm_group.set(self.cached_group)

class Bins(ModuleBase):
  def process(self, mat):
    self.post('orig', mat)

    lab_image = cv2.cvtColor(mat, cv2.COLOR_RGB2LAB)
    lab_split = cv2.split(lab_image)

    lab_athreshed = cv2.adaptiveThreshold(lab_split[1], 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV,
                                          self.options['block_size_cutout'], self.options['c_thresh_cutout'])
    lab_bthreshed = cv2.adaptiveThreshold(lab_split[2], 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV,
                                          self.options['block_size_cover'], self.options['c_thresh_cover'])

    finalaThreshed =  lab_athreshed
    finalbThreshed =  lab_bthreshed

    if self.options['debugging']:
      self.post('lab a', lab_split[1])
      self.post('lab b', lab_split[2])
      self.post('finalaThreshed', finalaThreshed)
      self.post('finalbThreshed', finalbThreshed)

    blurreda = cv2.medianBlur(finalaThreshed, self.options['blur_size_cutout'])
    blurredb = cv2.medianBlur(finalbThreshed, self.options['blur_size_cover'])
    self.post('blurred_a', blurreda)
    self.post('blurred_b', blurredb)

    kernel = np.ones((self.options['kernel_size'],
                      self.options['kernel_size']), np.uint8)
    morphed = cv2.morphologyEx(blurredb, cv2.MORPH_CLOSE, kernel)
    self.post("cover_morphed", morphed)

    # TODO Maybe do eroding and dilating / morphology here?
    _, contours_cutout, __ = cv2.findContours(blurreda.copy(),
                                     cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

    image_size = mat.shape[0] * mat.shape[1]

    big_contours = []
    for contour in contours_cutout:
      area = cv2.contourArea(contour)
      if area >= self.options['min_area_percent'] * image_size:
        big_contours.append((contour, area))

    candidate_cutouts = sorted(big_contours, key=lambda x: -x[1])[:2]

    _, contours, __ = cv2.findContours(morphed.copy(),
                               cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

    if self.options['debugging']:
      contoursMat = mat.copy()
      cv2.drawContours(contoursMat, contours, -1, green, 5)
      self.post("All contours", contoursMat)

    contourAreas = []
    for contour in contours:
      contourArea = cv2.contourArea(contour)
      if contourArea >= self.options['min_area_percent'] * image_size:
        contourAreas.append([contour, contourArea])
    contourAreas = sorted(contourAreas, key=lambda x: -x[1])[:CONTOUR_HEURISTIC_LIMIT]

    contour_info = namedtuple("contour_info", ["contour", "area", "rectangularity", "rect_area_prod", "center", "angle", "rotrect"])

    contourScores = []
    for c, a in contourAreas:
      rotrect = cv2.minAreaRect(c)
      center, (h, w), angle = rotrect

      rectangularity = a / (w*h)
      if rectangularity >= self.options['min_rectangularity']:
        contourScores.append(contour_info(c, a, rectangularity, rectangularity * a, center, angle, rotrect))
    contourScores = sorted(contourScores, key=lambda x: -x.rect_area_prod)[:CONTOUR_SCALED_HEURISTIC_LIMIT]

    shm_groups = [ShmGroup(shm.bin_cover),
                  ShmGroup(shm.bin_yellow_1), ShmGroup(shm.bin_yellow_2)]
    results_cover = shm_groups[0].cached_group
    results_yellow = shm_groups[1].cached_group, shm_groups[2].cached_group

    for shm_group in shm_groups:
      zero_vision_group(shm_group.cached_group)

    if contourScores:
      filteredContoursMat = mat.copy()
      binContour = max(contourScores, key=lambda x: x.center[1]) # Zero is top-left of image

      cv2.drawContours(filteredContoursMat, [binContour.contour], -1, green, 5)
      bin_center = int(binContour.center[0]), int(binContour.center[1])
      cv2.circle(filteredContoursMat, bin_center, 5, white, -1)

      shm_angle = get_angle_from_rotated_rect(binContour.rotrect)

      draw_angled_arrow(filteredContoursMat, bin_center, shm_angle)

      self.post("Filtered contours", filteredContoursMat)

      results_cover.probability = 1.0
      results_cover.center_x = bin_center[0]
      results_cover.center_y = bin_center[1]
      results_cover.angle = shm_angle

    contoursMat2 = mat.copy()
    for i, (contour, area) in enumerate(candidate_cutouts):
      moms = cv2.moments(contour)
      center = int(moms["m10"] / moms["m00"]), int(moms["m01"] / moms["m00"])

      cv2.drawContours(contoursMat2, [contour], -1, yellow, 5)
      cv2.circle(contoursMat2, center, 5, white, -1)

      results_yellow[i].probability = 1.0
      results_yellow[i].angle = 0.0
      results_yellow[i].center_x = center[0]
      results_yellow[i].center_y = center[1]

    self.post("Cutout contours", contoursMat2)

    for shm_group in shm_groups:
      self.fill_single_camera_direction(shm_group.cached_group)
      shm_group.set()

if __name__ == '__main__':
    Bins('downward', options)()
