#!/usr/bin/env python3

from collections import namedtuple
from math import radians

import cv2
import numpy as np

import shm
import auvlog.client

from vision.vision_common import draw_angled_arrow, \
                                 get_angle_from_rotated_rect
from vision.modules.base import ModuleBase
from vision import options as gui_options

log = auvlog.client.log.vision.pipes

#vision_options = [gui_options.IntOption('lab_b_min', 41, 0, 255),
#                  gui_options.IntOption('lab_b_max', 150, 0, 255),
#                  gui_options.IntOption('yuv_v_min', 107, 0, 255),
#                  gui_options.IntOption('yuv_v_max', 210, 0, 255),
#                  gui_options.IntOption('hsv_h_min', 49, 0, 255),
#                  gui_options.IntOption('hsv_h_max', 128, 0, 255),
#                  gui_options.IntOption('erode_size', 2, 0, 50),
#                  gui_options.IntOption('dilate_size', 2, 0, 50),
#                  gui_options.IntOption('min_pipe_area', 100),
#                  gui_options.DoubleOption('min_rectangularity', 1000),
#                  gui_options.DoubleOption('heuristic_power', 5),
#                  gui_options.BoolOption('debugging', False)]

vision_options = [ gui_options.IntOption('block_size', 1001, 0, 1500),
                   gui_options.IntOption('c_thresh', -13, -100, 100),
                   gui_options.IntOption('erode_size', 9, 0, 50),
                   gui_options.IntOption('dilate_size', 19, 0, 50),
                   gui_options.DoubleOption('min_percent_frame', 0.0002, 0, 0.1),
                   gui_options.DoubleOption('min_rectangularity', 1000),
                   gui_options.DoubleOption('heuristic_power', 5),
                   gui_options.BoolOption('debugging', True)
                 ]

def get_zero_pipe_group():
  pipe_group = shm.pipe_results.get()
  pipe_group.angle = 0
  pipe_group.center_x = 0
  pipe_group.center_y = 0
  pipe_group.heuristic_score = 0
  pipe_group.rectangularity = 0
  return pipe_group

class Pipes(ModuleBase):
    def process(self, mat):
        pipe_group = get_zero_pipe_group()

        self.post('orig', mat)

        lab_image = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        lab_split = cv2.split(lab_image)
        #yuv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2YUV)
        #yuv_split = cv2.split(yuv_image)
        #hsv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2HSV)
        #hsv_split = cv2.split(hsv_image)

        channel = lab_split[2]

        lab_bthreshed = cv2.adaptiveThreshold(channel, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, self.options['block_size'], self.options['c_thresh'])
        #lab_bthreshed = cv2.inRange(lab_split[2], self.options["lab_b_min"], self.options["lab_b_max"])
        #yuv_vthreshed = cv2.inRange(yuv_split[2], self.options["yuv_v_min"], self.options["yuv_v_max"])
        #hsv_hthreshed = cv2.inRange(hsv_split[0], self.options["hsv_h_min"], self.options["hsv_h_max"])

        #final_threshed = lab_bthreshed & yuv_vthreshed & hsv_hthreshed
        final_threshed = lab_bthreshed

        erode_size = self.options['erode_size']
        dilate_size = self.options['dilate_size']
        erode_element = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (erode_size * 2 + 1, erode_size * 2 + 1),
                                                  (erode_size, erode_size))
        dilate_element = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (dilate_size * 2 + 1, dilate_size * 2 + 1),
                                                   (dilate_size, dilate_size))

        eroded = cv2.erode(final_threshed, erode_element)
        dilated = cv2.dilate(eroded, dilate_element)

        if self.options['debugging']:
            self.post('lab b', lab_split[2])
            #self.post('yuv v', yuv_split[2])
            #self.post('hsv h', hsv_split[0])
            #self.post('lab b Threshed', lab_bthreshed)
            #self.post('yuv v Threshed', yuv_vthreshed)
            #self.post('hsv h Threshed', hsv_hthreshed)
            self.post("Threshed", final_threshed)
            self.post("Masked", cv2.bitwise_and(mat, mat, mask=final_threshed))
            self.post("Eroded", eroded)
            self.post("Eroded_Dilated", dilated.copy())

        _, contours, _ = cv2.findContours(dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        if contours is None:
            print("None returned from findContours")
            shm.pipe_results.set(pipe_group)
            return

        if self.options['debugging']:
            allContoursDrawing = np.copy(mat)
            cv2.drawContours(allContoursDrawing, [c for c in contours if cv2.contourArea(c)], -1, (255, 255, 0), 2)
            self.post("All contours", allContoursDrawing)

        contour_info = namedtuple("contour_info",
                      ["contour", "rectangle_contour", "rect", "angle", "area", "center", "rectangularity", "heuristic_score"])

        image_size = mat.shape[0] * mat.shape[1]

        contour_data = []
        for c in contours:
            area = cv2.contourArea(c)
            if area < image_size * self.options['min_percent_frame']:
                continue

            rotated_rect = cv2.minAreaRect(c)
            contour = np.array(cv2.boxPoints(rotated_rect)).astype(int)
            rectangularity = area / (rotated_rect[1][0] * rotated_rect[1][1])
            center = rotated_rect[0]
            heuristic_score = rectangularity ** self.options["heuristic_power"] * cv2.contourArea(contour)
            x = contour_info(c, contour, rotated_rect, rotated_rect[2], area, center, rectangularity, heuristic_score)

            contour_data.append(x)

        if len(contour_data) == 0:
            shm.pipe_results.set(pipe_group)
            return

        best = max(contour_data, key=lambda x: x.heuristic_score)
        pipe_center = int(best.center[0]), int(best.center[1])

        shm_angle = get_angle_from_rotated_rect(best.rect)

        if self.options["debugging"]:
            log("rect:{}".format(best.rect))
            log("width:{}".format(best.rect[1][0]))
            log("height:{}".format(best.rect[1][1]))
            log("angle:{}".format(shm_angle))

        contours_to_draw = [x.rectangle_contour for x in contour_data]
        good_contours_drawing = np.copy(mat)
        cv2.drawContours(good_contours_drawing, contours_to_draw, -1, (255, 0, 0), 4)
        cv2.circle(good_contours_drawing, pipe_center, 5, (0, 0, 255), 2)
        draw_angled_arrow(good_contours_drawing, pipe_center, shm_angle)

        self.post("Rectangular contours", good_contours_drawing)

        pipe_group.center_x = pipe_center[0]
        pipe_group.center_y = pipe_center[1]
        pipe_group.angle = shm_angle
        pipe_group.rectangularity = best.rectangularity
        pipe_group.heuristic_score = best.heuristic_score
        self.fill_single_camera_direction(pipe_group)
        shm.pipe_results.set(pipe_group)

if __name__ == '__main__':
    Pipes('downward', vision_options)()
