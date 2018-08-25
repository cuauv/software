#!/usr/bin/env python3

from collections import namedtuple
from math import radians, atan2, sqrt, sin, cos, atan

import cv2
import numpy as np

import shm
import auvlog.client

import time
import traceback

from conf.vehicle import VEHICLE

from vision.vision_common import draw_angled_arrow, \
                                 get_angle_from_rotated_rect
from vision.modules.base import ModuleBase
from vision import options as gui_options


log = auvlog.client.log.vision.pipes

# vision_options = {
#     'pollux': [
#         gui_options.BoolOption('debugging', False),
# #        gui_options.IntOption('lab_a_thresh_min', 0, 0, 255),
# #        gui_options.IntOption('lab_a_thresh_max', 127, 0, 255),
# #        gui_options.IntOption('hsv_h_min', 61, 0, 255),
# #        gui_options.IntOption('hsv_h_max', 255, 0, 255),
# #        gui_options.IntOption('erode_size', 4, 0, 50),
# #        gui_options.IntOption('dilate_size', 4, 0, 50),
#         gui_options.IntOption('canny1', 50, 0, 200),
#         gui_options.IntOption('canny2', 150, 0, 200),
# #        gui_options.IntOption('canny_aperture_size', 3, 0, 20),
#         gui_options.DoubleOption('min_angle_diff', np.pi/8, 0, np.pi*2),
# #        gui_options.DoubleOption('min_line_dist', 100, 0, 1000),
#         # A circle with radius 1 inscribed in a square has
#         # 'rectangularity' pi/4 ~ 0.78.
#         gui_options.DoubleOption('min_line_hough_length',45, 0, 500),
#         gui_options.IntOption('color_dist_min', 0, 0, 255),
#         gui_options.IntOption('color_dist_max', 20, 0, 255),

#         # Preprocess
# #        gui_options.IntOption('gaussian_kernel', 5, 1, 40),
# #        gui_options.IntOption('gaussian_stdev', 20, 0, 40),
#     ],
#     'castor': [
#         gui_options.BoolOption('debugging', True),
# #        gui_options.IntOption('lab_a_thresh_min', 0, 0, 255),
# #        gui_options.IntOption('lab_a_thresh_max', 114, 0, 255),
# #        gui_options.IntOption('hsv_h_min', 142, 0, 255),
# #        gui_options.IntOption('hsv_h_max', 255, 0, 255),
# #        gui_options.IntOption('erode_size', 4, 0, 50),
# #        gui_options.IntOption('dilate_size', 4, 0, 50),
#         gui_options.IntOption('canny1', 50, 0, 200),
#         gui_options.IntOption('canny2', 150, 0, 200),
# #        gui_options.IntOption('canny_aperture_size', 3, 0, 20),
#         gui_options.DoubleOption('min_angle_diff', np.pi/8, 0, np.pi*2),
# #        gui_options.DoubleOption('min_line_dist', 100, 0, 1000),
#         # A circle with radius 1 inscribed in a square has
#         # 'rectangularity' pi/4 ~ 0.78.
#         gui_options.DoubleOption('min_line_hough_length',45, 0, 500),
#         gui_options.IntOption('color_dist_min', 0, 255),
#         gui_options.IntOption('color_dist_max', 20, 0, 255),

#         # Preprocess
# #        gui_options.IntOption('gaussian_kernel', 5, 1, 40),
# #        gui_options.IntOption('gaussian_stdev', 20, 0, 40),
#     ],
# }

vision_options = [
    gui_options.BoolOption('debugging', False),
    gui_options.IntOption('canny1', 50, 0, 200),
    gui_options.IntOption('canny2', 150, 0, 200),
    gui_options.DoubleOption('min_angle_diff', np.pi/8, 0, np.pi*2),
    gui_options.DoubleOption('min_line_hough_length',45, 0, 500),
    gui_options.IntOption('color_dist_min', 0, 0, 255),
    gui_options.IntOption('color_dist_max', 20, 0, 255),
    gui_options.IntOption('min_angle_range', 35, 0, 180),
    gui_options.IntOption('max_angle_range', 55, 0, 180),
]

segment_info = namedtuple("segment_info", ["x1", "y1", "x2", "y2", "angle", "id", "updated"])
line_info = namedtuple("line_info", ["x1", "y1", "x2", "y2", "angle", "length", "id"])

INVALID_ERROR = 1e99

from conf.vehicle import VEHICLE

is_mainsub = VEHICLE == 'castor'

class Pipes(ModuleBase):
    tracked_lines = []


    def angle(self, x1, y1, x2, y2):
        a = atan( (x2-x1) / (y2-y1) )
        #a = atan2(y2-y1, x2-x1)
        return a

    def angle_diff(self, a1, a2):
        # a=min((2*np.pi) - abs(a1-a2), abs(a1-a2))
        a = atan(sin(a1-a2)/cos(a1-a2))
        return abs(a)

    def threshold(self, mat):
        threshes = {}

        lab = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        if False:
            self.post('lab', lab)

        dist_from_orange = np.linalg.norm(lab[:, :, :].astype(int) - [90, 144, 131], axis=2).astype(int)
        if self.options['debugging']:
            self.post('orange_dist', np.abs(dist_from_orange).astype('uint8'))

        orange_threshed = cv2.inRange(dist_from_orange, self.options["color_dist_min"], self.options["color_dist_max"])

        morphed = orange_threshed
        threshes['morphed'] = morphed

        #k_size = self.options["gaussian_kernel"]
        #k_std = self.options["gaussian_stdev"]
        #mat = cv2.GaussianBlur(mat, (k_size * 2 + 1, k_size * 2 + 1), k_std, k_std)


        #if self.options['debugging']:
        #    self.post("preprocessed", mat)

        # hsv = cv2.cvtColor(mat, cv2.COLOR_RGB2HSV)
        # hsv_h, hsv_s, hsv_v = cv2.split(hsv)
        # # s_threshed = cv2.adaptiveThreshold(hsv_s, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, 1 + 2 * self.options['hsv_s_block_size'], self.options['hsv_s_c_thresh'])
        # # v_threshed = cv2.adaptiveThreshold(hsv_v, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, 1 + 2 * self.options['hsv_v_block_size'], self.options['hsv_v_c_thresh'])
        # h_threshed = cv2.inRange(hsv_v, self.options['hsv_h_min'], self.options['hsv_h_max'])

        # lab = cv2.cvtColor(mat, cv2.COLOR_RGB2LAB)

        # lab_l, lab_a, lab_b = cv2.split(lab)

        # lab_a_threshed = cv2.inRange(lab_a, self.options['lab_a_thresh_min'], self.options['lab_a_thresh_max'])
        # # lab_l_threshed = cv2.inRange(lab_l, self.options['lab_l_thresh_min'], self.options['lab_l_thresh_max'])
        # # lab_b_threshed = cv2.inRange(lab_b, self.options['lab_b_thresh_min'], self.options['lab_b_thresh_max'])

        # if self.options['debugging']:
        #     self.post('lab_a_thresh', lab_a_threshed)

        # final_threshed = h_threshed & ~lab_a_threshed
        # #final_threshed = ~h_threshed & ~lab_a_threshed

        # #if self.options['debugging']:
        # #    self.post('final_threshed',final_threshed)

        # # threshes["hsv_s"] = s_threshed
        # # threshes["hsv_v"] = v_threshed
        # threshes["hsv_h"] = h_threshed
        # threshes["final"] = final_threshed

        # dilate_size = self.options['dilate_size']
        # erode_size = self.options['erode_size']
        # erode_element = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (erode_size * 2 + 1, erode_size * 2 + 1), (erode_size, erode_size))
        # dilate_element = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (dilate_size * 2 + 1, dilate_size * 2 + 1), (dilate_size, dilate_size))
        # morphed = cv2.erode(final_threshed, erode_element)
        # morphed = cv2.dilate(morphed, dilate_element)

        # threshes["morphed"] = cv2.inRange(final_threshed, 200, 255)

        # hack hack hack for simulator
        sat = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2HSV))[1]
        morphed = cv2.inRange(sat, 20, 255)

        edges = cv2.Canny(morphed,threshold1=self.options['canny1'],threshold2=self.options['canny2'],apertureSize=3) #self.options['canny_aperture_size'])
        threshes["edges"] = edges

        if self.options['debugging']:
            for name, image in threshes.items():
                self.post("threshed_{}".format(name), image)

        return threshes

    def find_lines(self, mat, thresh):
        #if self.options['debugging']:
        #    self.post("thres_test",thresh)
        minLineLength = self.options['min_line_hough_length']

        theta_res = radians(2)

        lines = cv2.HoughLines(thresh,1,theta_res,int(minLineLength))

        if lines is None:
            return []

        if self.options["debugging"]:
            line_image = np.copy(mat)

            for line in lines:
                rho, theta = line[0]
                a = np.cos(theta)
                b = np.sin(theta)
                x0 = a * rho
                y0 = b * rho
                x1 = int(x0 + 1000*(-b))
                y1 = int(y0 + 1000*(a))
                x2 = int(x0 - 1000*(-b))
                y2 = int(y0 - 1000*(a))
                cv2.line(line_image,(x1,y1),(x2,y2),(0,255,0),10)


        if self.options['debugging']:
            self.post("all_houghs",line_image)

        return lines

    #A list of segment_infos sorted in order of best
    def average_lines(self,lines,mat):
        if lines is None:
            return lines

        info = []
        final = []

        for l in lines:
            rho, theta = l[0]
            a = np.cos(theta)
            b = np.sin(theta)
            x0 = a * rho
            y0 = b * rho
            x1 = int(x0 + 1000*(-b))
            y1 = int(y0 + 1000*(a))
            x2 = int(x0 - 1000*(-b))
            y2 = int(y0 - 1000*(a))
            angle = self.angle(x1, y1, x2, y2)
            length = sqrt((y2-y1)**2 + (x2-x1)**2)

            x = line_info(x1,y1,x2,y2, angle, length, 0)
            info.append(x)

        # Pick big lines first
        info.sort(key=lambda x: x.length, reverse=True)

        # Put into buckets
        buckets = []
        for line in info:
            for bucket in buckets:
                if self.angle_diff(line.angle, bucket[0].angle) < self.options['min_angle_diff']:
                    bucket.append(line)
                    break
            else:
                buckets.append([line])

        # Average each bucket
        final = [line_info(*[sum([line[x] for line in bucket]) / len(bucket) for x in range(len(bucket[0]))]) for bucket in buckets]

        if self.options["debugging"]:
            line_image = np.copy(mat)

            for line in final:
                x1,y1,x2,y2 = line.x1,line.y1,line.x2,line.y2
                cv2.line(line_image,(int(x1),int(y1)),(int(x2),int(y2)),(0, 0, 255),15)
            for line in info:
                x1,y1,x2,y2 = line.x1,line.y1,line.x2,line.y2
                cv2.line(line_image,(int(x1),int(y1)),(int(x2),int(y2)),(0, 255, 0),5)

        return final

    def get_intersection(self, lines):
      x1 = lines[0].x1
      x2 = lines[0].x2
      y1 = lines[0].y1
      y2 = lines[0].y2

      x3 = lines[1].x1
      x4 = lines[1].x2
      y3 = lines[1].y1
      y4 = lines[1].y2

      px= ( (x1*y2-y1*x2)*(x3-x4)-(x1-x2)*(x3*y4-y3*x4) ) / ( (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4) )
      py= ( (x1*y2-y1*x2)*(y3-y4)-(y1-y2)*(x3*y4-y3*x4) ) / ( (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4) )

      return [px, py]


    def process(self, mat):
      try:

        image_size = mat.shape[0]*mat.shape[1]

        #if self.options['debugging']:
        #    self.post('orig', mat)
        threshes = self.threshold(mat)


        lineMat = np.copy(mat)

        lines = self.find_lines(mat, threshes["edges"])

        error = namedtuple("error", ["lines", "error"])

        linesI = self.average_lines(lines, mat)

        path_angle = []

        for line in linesI:
          path_angle.append(line.angle)

        second_line = False
        flipped = False

        if len(linesI) == 2:
          px, py = self.get_intersection(linesI)

        if len(linesI) == 2:
          old_angle_1 = shm.path_results.angle_1.get()
          old_angle_2 = shm.path_results.angle_2.get()

          angle_diff_1 = self.angle_diff(old_angle_1, path_angle[0])
          angle_diff_2 = self.angle_diff(old_angle_2, path_angle[0])

          angle_diff_3 = self.angle_diff(old_angle_1, path_angle[1])
          angle_diff_4 = self.angle_diff(old_angle_2, path_angle[1])

          angle_diffs = [angle_diff_1, angle_diff_2, angle_diff_3, angle_diff_4]

          angle_diffs.sort()

          if angle_diffs[0] == angle_diff_2 or angle_diffs[0] == angle_diff_3 :
            path_angle[0], path_angle[1] = path_angle[1], path_angle[0]
            flipped = True

          shm.path_results.angle_1.set(path_angle[0])
          shm.path_results.angle_2.set(path_angle[1])
          shm.path_results.visible_1.set(True)
          shm.path_results.visible_2.set(True)
          shm.path_results.center_x.set(self.normalized(px,1))
          shm.path_results.center_y.set(self.normalized(py,0))
          shm.path_results.num_lines.set(2)

        elif len(linesI) == 1:
          old_angle_1 = shm.path_results.angle_1.get()
          old_angle_2 = shm.path_results.angle_2.get()
          angle_diff_1 = self.angle_diff(old_angle_1, path_angle[0])
          angle_diff_2 = self.angle_diff(old_angle_2, path_angle[0])

          if angle_diff_1 < angle_diff_2:
            shm.path_results.angle_1.set(path_angle[0])
            shm.path_results.visible_1.set(True)
            shm.path_results.visible_2.set(False)
          elif angle_diff_1 > angle_diff_2:
            shm.path_results.angle_2.set(path_angle[0])
            shm.path_results.visible_1.set(False)
            shm.path_results.visible_2.set(True)
            second_line = True

          shm.path_results.num_lines.set(1)

        else:
          shm.path_results.visible_1.set(False)
          shm.path_results.visible_2.set(False)
          shm.path_results.num_lines.set(0)


        line_image = np.copy(mat)
        if flipped:
            linesI[0], linesI[1] = linesI[1], linesI[0]

        for i, line in enumerate(linesI):
          x1,y1,x2,y2 = line.x1,line.y1,line.x2,line.y2
          if i == 0 and not second_line:
            cv2.line(line_image,(int(x1),int(y1)),(int(x2),int(y2)),(255,0,0),5)
          elif i == 1 or second_line:
            cv2.line(line_image,(int(x1),int(y1)),(int(x2),int(y2)),(0,255,0),5)

        self.post("final_final",line_image)

      except Exception as e:
          traceback.print_exc()

if __name__ == '__main__':
    Pipes('downward', vision_options)() #[VEHICLE])()
