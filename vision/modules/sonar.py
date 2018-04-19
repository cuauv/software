#!/usr/bin/env python3

import cv2
import time
import math
import shm
import functools
import numpy as np
import auvlog.client

from vision import options
from vision.modules.base import ModuleBase

log = lambda m: auvlog.client.log.vision.sonar(m, copy_to_stdout = True)

options = [
  options.IntOption('accumulation_count', 10, 1, 40),
  options.IntOption('filter_p1', 10, 1, 100),
  options.IntOption('filter_p2', 75, 1, 1000),
  options.IntOption('thresh_c', 150, 1, 255),
  options.IntOption('erode_kernel_size', 9, 1, 50),
  options.DoubleOption('circularity_threshold', 7.0, 1.0, 20.0),
  options.DoubleOption('radius_min_meters', 0.05, 0.0, 1.0),
  options.DoubleOption('radius_max_meters', 0.20, 0.0, 1.0)
]

normalize = lambda x: x.astype(np.float32) / np.sum(x)


class Sonar(ModuleBase):
  def __init__(self, *args, **kwargs):
    super().__init__(*args, **kwargs)
    self.prev = []

  def process(self, mat):

    c0, c1, c2 = cv2.split(mat)
    mat = c1
    self.post('orig', mat)

    self.prev.append(mat)
    if len(self.prev) < self.options['accumulation_count']:
      log('Skipping image (added to accumulator)')
      return
    else:
        accum = functools.reduce(lambda x, y: normalize(x.astype(np.float32) * y.astype(np.float32)), self.prev)
        self.prev = []

    accum = (accum / np.sum(accum)) * 254. * accum.shape[0] * accum.shape[1]
    accum = np.minimum(accum, 254.)
    accum = np.floor(accum).astype(np.uint8)

    self.post('accum', accum)

    eroded = cv2.erode(accum, np.ones(self.options['erode_kernel_size'], np.uint8))
    self.post('eroded', eroded)

    filtered = cv2.bilateralFilter(eroded, self.options['filter_p1'], self.options['filter_p2'], self.options['filter_p2'])
    self.post('filtered', filtered)

    _, threshed = cv2.threshold(filtered, self.options['thresh_c'], 255, cv2.THRESH_BINARY)
    self.post('threshed', threshed)

    contours = threshed.copy()
    self.post('contours', contours)
    _, contours, hierarchy = cv2.findContours(contours, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

    data = [(c, cv2.contourArea(c), cv2.minAreaRect(c), cv2.minEnclosingCircle(c)) for c in contours]

    sonar = shm.sonar.get()
    print('Min angle: {} Max angle: {} Range res: {} Bearing res: {}'.format(sonar.fov_min_angle, sonar.fov_max_angle, sonar.range_resolution, sonar.bearing_resolution))
    
    '''
    Buoys
    '''

    selected = [(c, c[3], math.pi * c[3][1] ** 2. / c[1]) for c in data if c[1] > 0]
    selected = sorted(selected, key = lambda x:x[2])
    selected = [x for x in selected if x[2] < self.options['circularity_threshold'] and self.options['radius_min_meters'] < x[1][1] * sonar.range_resolution < self.options['radius_max_meters']]
    selected = [(x[0], x[1]) for x in selected]
    
    colored = np.zeros((threshed.shape[0], threshed.shape[1], 3), np.uint8)
    for (_, circle) in selected:
      cv2.circle(colored, (int(circle[0][0]), int(circle[0][1])), int(circle[1]), (255, 0, 0), 5)
    self.post('colored', colored)

    buoys = []
    for (_, ((ctr_x, ctr_y), rad)) in selected:
        dx, dy = ctr_x - sonar.origin_col, ctr_y - sonar.origin_row
        # angular_range = math.radians(sonar.fov_max_angle - sonar.fov_min_angle)
        # radians_per_px = angular_range / mat.shape[1]
        # log('Degrees per x pixel: {}'.format(math.degrees(radians_per_px)))
        ang = math.atan2(dx, -dy)
        dist = sonar.range_resolution * ((dx ** 2. + dy ** 2.) ** (0.5))
        buoys.append(( dist * math.cos(ang), dist * math.sin(ang), rad * sonar.range_resolution ))
        log('Got a buoy! Angle: {} deg, distance: {}m. Converted to dX: {}, dY: {}, radius: {}m'.format(math.degrees(ang), dist, buoys[-1][0], buoys[-1][1], buoys[-1][2]))

    for (delta_x, delta_y, radius) in buoys:
        grp = shm.sonar_buoy_results.get()
        grp.delta_x = delta_x
        grp.delta_y = delta_y
        grp.probability = 0.5
        shm.sonar_buoy_results.set(grp)
        time.sleep(0.1)

    '''
    Walls
    '''

if __name__ == '__main__':
    Sonar('sonar', options)()
