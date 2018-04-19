import json
import struct
import os
import sys
import subprocess
import time
import collections
import io
import cv2

from math import radians

import numpy as np

import shm

red = (0, 0, 255)
green = (0, 255, 0)
blue = (255, 0, 0)
white = (255, 255, 255)
cyan = (255, 255, 0)
yellow = (0, 255, 255)
purple = (127, 0, 127)

_all_vision_modules = [(x[0], getattr(shm.vision_modules, x[0])) for x in shm.vision_modules._fields]
_all_vision_modules_dict = {name: (name, shm) for (name, shm) in _all_vision_modules}
_all_vision_modules_lower_dict = {name.lower(): (name, shm) for (name, shm) in _all_vision_modules}

def all_vision_modules():
    return _all_vision_modules[:]

def module_by_name(module_name, case_sensitive=False):
    if case_sensitive:
        return _all_vision_modules_dict[module_name]
    return _all_vision_modules_lower_dict[module_name.lower()]

def fork(target, args=None, kwargs=None):
    pid = os.fork()
    if pid == 0:
        os.setsid()
        os.umask(0)

        pid = os.fork()
        if pid != 0:
            os._exit(0)

        dirname = os.path.dirname(os.path.realpath(__file__))

        open('{}/pids/{}.pid'.format(dirname, os.getpid()), 'w').close()

        if args:
            if kwargs:
                target(*args, **kwargs)
            else:
                target(*args)
        else:
            if kwargs:
                target(**kwargs)
            else:
                target()
        cleanup_pid()
        sys.exit(0)


def cleanup_pid(*args, **kwargs):
    print('{} exiting'.format(os.getpid()))
    dirname = os.path.dirname(os.path.realpath(__file__))
    try:
        os.remove('{}/pids/{}.pid'.format(dirname, os.getpid()))
    except OSError:
        pass


class NTee(io.IOBase):
    def __init__(self, *dest):
        super(NTee, self).__init__(os.devnull, 'w')
        self._dest = dest

    def write(self, str):
        for dest in self._dest:
            dest.write(str)

    def writelines(self, sequence_of_strings):
        for string in sequence_of_strings:
            self.write('{}\n'.format(string))



def flatten(l):
    for el in l:
        if isinstance(el, collections.Iterable) and not isinstance(el, str):
            for sub in flatten(el):
                yield sub
        else:
            yield el

def extract_features(image):
  import cv2
  import numpy as np

  feature_vector = []
  _, contours, hierarchy = cv2.findContours(image, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

  if hierarchy is None:
    return []

  hierarchy = hierarchy[0]
  contour_info = hierarchy[0]
  outer_index = 0
  while contour_info[3] >= 0:
    outer_index, contour_info = contour_info[3], hierarchy[contour_info[3]]

  while contour_info[1] >= 0:
    outer_index, contour_info = contour_info[1], hierarchy[contour_info[1]]

  outer_contours = [(outer_index, contours[outer_index])]

  while contour_info[0] >= 0:
    outer_index, contour_info = contour_info[0], hierarchy[contour_info[0]]
    outer_contours.append((outer_index, contours[outer_index]))

  outer_contours = [(index, contour, cv2.contourArea(contour), cv2.boundingRect(contour)) for (index, contour) in outer_contours]

  outer_contours.sort(key=lambda x: x[2], reverse=True)

  outer_contours = outer_contours[:2]

  outer_contours.sort(key=lambda x: x[3][0])

  for index, contour, area, bounding_rect in outer_contours[:2]:
    moments = cv2.moments(contour)
    hu_moments = cv2.HuMoments(moments)

    feature_vector.append(hu_moments)
    feature_vector.append(area)
    feature_vector.append(bounding_rect[2:])

    inner_contour_area = 0

    outer_index = hierarchy[outer_index][2]

    while outer_index >= 0:
      inner_contour_area += cv2.contourArea(contours[outer_index])
      outer_index, contour_info = contour_info[0], hierarchy[contour_info[0]]

    feature_vector.append(inner_contour_area)

  return np.array(list(flatten(feature_vector)))

def resize_keep_ratio(image, desired_size):
    if len(image.shape) == 2:
        height, width = image.shape
    else:
        height, width, _ = image.shape
    div_ratio = 1
    if height > desired_size:
        div_ratio = height / desired_size
    if width > desired_size:
        div_ratio = max(div_ratio, width / desired_size)
    if abs(1 - div_ratio) < 0.01:
        return image
    des_height = int(height // div_ratio)
    des_width = int(width // div_ratio)
    return cv2.resize(image, (des_width, des_height))

def get_angle_from_rotated_rect(rotrect):
    """
        Computes the relative angle to the sub needed to align
        to the rectangle along its long axis.
    """
    # True if taller than wide
    if rotrect[1][0] < rotrect[1][1]:
        return rotrect[2]
    return rotrect[2] + 90

def get_angle_from_ellipse(ellipse):
    """
        Computes the relative angle to the sub needed to align
        to the ellipse along its long axis.
    """
    return (get_angle_from_rotated_rect(ellipse) + 90) % 180 - 90

def draw_angled_arrow(image, center, angle):
    """
        Draws a double sided arrow on image centered at center
        at an angle of angle degrees.
    """
    sin, cos = np.sin(radians(angle)), np.cos(radians(angle))
    rotated_dir = np.array(((cos, -sin), (sin, cos))).dot(
                  np.array((0, -1)))

    line_length = min(image.shape[0], image.shape[1]) * 0.17
    line_start = np.array((center)) + rotated_dir * line_length
    line_end = np.array((center)) - rotated_dir * line_length
    def get_tup(vec):
        return int(vec[0]), int(vec[1])

    cv2.arrowedLine(image, get_tup(line_start),
                    get_tup(line_end), (255, 255, 0), 2)
    cv2.arrowedLine(image, get_tup(line_end),
                    get_tup(line_start), (255, 255, 0), 2)

def zero_vision_group(group):
    group.center_x = 0
    group.center_y = 0
    group.probability = 0

def post_colorspace(module, original, colorspace):
  cspace_map = { cv2.COLOR_BGR2HSV : "hsv",
                 cv2.COLOR_BGR2LAB : "lab",
                 cv2.COLOR_BGR2YUV : "yuv",
                 cv2.COLOR_BGR2YCrCb: "ycrcb",
                 cv2.COLOR_BGR2LUV : "luv",
                 cv2.COLOR_BGR2XYZ : "xyz" }

  conv = cv2.cvtColor(original, colorspace)
  split = cv2.split(conv)
  pre = cspace_map[colorspace]
  module.post(pre + " " + pre[0], split[0])
  module.post(pre + " " + pre[int(len(pre) / 2)], split[1])
  module.post(pre + " " + pre[-1], split[2])

class Hierarchy:
    def __init__(self, hierarchy):
        self.hierarchy = hierarchy[0]

    def next(self, i):
        return self.hierarchy[i][0] if self._in_range(i) else -1

    def prev(self, i):
        return self.hierarchy[i][1] if self._in_range(i) else -1

    def first_child(self, i):
        return self.hierarchy[i][2] if self._in_range(i) else -1

    def children(self, i):
        return self.siblings(self.first_child(i))

    def parent(self, i):
        return self.hierarchy[i][3] if self._in_range(i) else -1

    def siblings(self, i):
        if i == -1:
            return
        while self.prev(i) != -1:
            i = self.prev(i)

        while i != -1:
            yield i
            i = self.next(i)

    def num_sibs(self, i):
        num = 0
        for i in self.siblings(i):
            num += 1
        return num

    def outermost(self):
        if len(self.hierarchy) == 0:
            return []

        i = 0
        
        while self.parent(i) != -1:
            i = self.parent(i)
        return self.siblings(i)

    def _in_range(self, i):
        return -1 < i < len(self.hierarchy)

def is_clipping(mat, contour):
    """
    Returns if any points on the contour are close to an edge of the camera view
    """
    cam_height, cam_width = mat.shape[:2]
    distance = 5
    x, y, w, h = cv2.boundingRect(contour)
    return x <= distance or y <= distance or \
            cam_width - w - x <= distance or \
            cam_height - h - y <= distance

def fill_ratio(mat, contour, threshed):
    fill_mask = np.zeros(mat.shape[:2], dtype=np.uint8)
    cv2.drawContours(fill_mask, [contour], -1, 255, thickness=-1)
    fill_masked = cv2.bitwise_and(threshed, threshed, mask=fill_mask)
    hull_area = cv2.contourArea(cv2.convexHull(contour))
    fill = np.sum(fill_masked) / 255 / hull_area
    return fill

def contour_center(contour):
    moments = cv2.moments(contour)
    return (moments['m10'] / moments['m00'], moments['m01'] / moments['m00'])
