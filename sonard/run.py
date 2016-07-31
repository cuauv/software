#!/usr/bin/env python2.7

from __future__ import division

METERS_PER_PIXEL = 7.5 # EMPIRICAL

import cv2, matplotlib, time, numpy, shm, subprocess, math
from scipy import ndimage

sonar = subprocess.Popen(['./stream2'], stdin = None, stdout = open('stream.out', 'wb'), stderr = open('stream.err', 'wb'))

matplotlib.use('WX')

from matplotlib import pyplot as plt

plt.ion()

prev = None

def get():
  i = cv2.imread('rdy.pgm', cv2.IMREAD_ANYDEPTH)
  maxv = i.max()
  i *= 255 / maxv
  i = numpy.uint8(i)
  i = cv2.bilateralFilter(i, 9, 75, 75)
  # i = cv2.adaptiveThreshold(i, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 11, 5)
  plt.imshow(i)
  plt.waitforbuttonpress()
  return i

prev = get()
prev_t = time.time()
p0 = cv2.goodFeaturesToTrack(prev, mask = None, maxCorners = 100, qualityLevel = 0.7, minDistance = 7, blockSize = 7)

while 1:
  i = get()
  
  '''
  # FLOW v1
  p1, st, err = cv2.calcOpticalFlowPyrLK(prev, i, p0, None, winSize = (15, 15), maxLevel = 3, criteria = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT, 20, 0.01))
  p0, p1 = p0[st == 1], p1[st == 1]
  delta = p1 - p0 
  flow_1 = numpy.sum(delta, axis = 0) / delta.shape[0]
  p0 = cv2.goodFeaturesToTrack(i, mask = None, maxCorners = 100, qualityLevel = 0.3, minDistance = 7, blockSize = 7)
  '''

  # FLOW v2
  far = cv2.calcOpticalFlowFarneback(prev, i, flow = None, pyr_scale = 0.5, levels = 2, winsize = 30, iterations = 10, poly_n = 7, poly_sigma = 1.5, flags = cv2.OPTFLOW_FARNEBACK_GAUSSIAN)
  far = numpy.sum(far, axis = 0) / far.shape[0] / far.shape[1]
  far = numpy.sum(far, axis = 0)
  flow_2 = far

  aff = cv2.estimateRigidTransform(prev, i, fullAffine = False)
  # Affine 2x3 2x2 heading, 1x2 linear. ~ Ax + b.
  print(aff)
  if aff is not None:
    x, y = aff[0][0], aff[1][0]
    print(math.atan2(y, x))
    dx, dy = aff[0][2], aff[1][2]
    print(dx, dy)
   
  # Scaling
  dt = time.time() - prev_t
  scale = METERS_PER_PIXEL / dt
  # flow_1 *= scale
  flow_2 *= scale

  if flow_2.sum() != 0:
    print('Flow : {0}'.format(flow_2))
    shm.sonar.vel_x.set(flow_2[1])
    shm.sonar.vel_y.set(flow_2[0])
    # shm.sonar.confidence.set(len(delta))

  prev, prev_t = i, time.time()
