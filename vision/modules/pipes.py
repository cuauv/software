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

vision_options = [ gui_options.IntOption('block_size', 1001, 0, 1500),
                   gui_options.IntOption('c_thresh', -4, -200, 100),
                   gui_options.IntOption('erode_size', 5, 0, 50),
                   gui_options.IntOption('dilate_size', 9, 0, 50),
                   gui_options.DoubleOption('min_percent_frame', 0.01, 0, 0.1),
                   gui_options.DoubleOption('max_percent_frame', 0.2, 0, 1),
                   # A circle with radius 1 inscribed in a square has
                   # 'rectangularity' pi/4 ~ 0.78.
                   gui_options.DoubleOption('aspect_ratio_threshold', 3, 0.5, 5),
                   gui_options.DoubleOption('min_rectangularity', 0.48),
                   gui_options.DoubleOption('heuristic_power', 5),
                   gui_options.IntOption('orange_lab_l_min', 152, 0, 255),
                   gui_options.IntOption('orange_lab_l_max', 255, 0, 255),
                   gui_options.IntOption('orange_hsv_v_min', 0, 0, 255),
                   gui_options.IntOption('orange_hsv_v_max', 255, 0, 255),
                   gui_options.IntOption('orange_rgb_r_min', 180, 0, 255),
                   gui_options.IntOption('orange_rgb_r_max', 255, 0, 255),
                   gui_options.IntOption('orange_ycrcb_y_min', 125, 0, 255),
                   gui_options.IntOption('orange_ycrcb_y_max', 255, 0, 255),

                   gui_options.IntOption('dark_lab_l_min', 54, 0, 255),
                   gui_options.IntOption('dark_lab_l_max', 255, 0, 255),
                   gui_options.IntOption('dark_hsv_v_min', 0, 0, 255),
                   gui_options.IntOption('dark_hsv_v_max', 255, 0, 255),
                   gui_options.IntOption('dark_rgb_r_min', 85, 0, 255),
                   gui_options.IntOption('dark_rgb_r_max', 255, 0, 255),
                   gui_options.IntOption('dark_ycrcb_y_min', 32, 0, 255),
                   gui_options.IntOption('dark_ycrcb_y_max', 255, 0, 255),

                   gui_options.IntOption('light_lab_l_min', 200, 0, 255),
                   gui_options.IntOption('light_lab_l_max', 255, 0, 255),
                   gui_options.IntOption('light_hsv_v_min', 172, 0, 255),
                   gui_options.IntOption('light_hsv_v_max', 255, 0, 255),
                   gui_options.IntOption('light_rgb_r_min', 250, 0, 255),
                   gui_options.IntOption('light_rgb_r_max', 255, 0, 255),
                   gui_options.IntOption('light_ycrcb_y_min', 207, 0, 255),
                   gui_options.IntOption('light_ycrcb_y_max', 255, 0, 255),

                   gui_options.IntOption('max_redness', 254, 0, 255),
                   gui_options.IntOption('min_redness', 0, 0, 255),
                   gui_options.BoolOption('debugging', False),
                   gui_options.BoolOption('orange_debug', False),
                   gui_options.BoolOption('dark_debug', False),
                   gui_options.BoolOption('light_debug', False)
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

        #rgb r
        #lab l
        #hvs s
        #ycrcb cb

        #lab b

        lab = cv2.cvtColor(mat, cv2.COLOR_RGB2LAB)
        ycrcb = cv2.cvtColor(mat, cv2.COLOR_RGB2YCR_CB)
        hls = cv2.cvtColor(mat, cv2.COLOR_RGB2HLS)
        hls_split = cv2.split(hls)
        hsv = cv2.cvtColor(mat, cv2.COLOR_RGB2HSV)
        hsv_split = cv2.split(hls)
        lab_split = cv2.split(lab)
        ycrcb_split = cv2.split(ycrcb)
        lab_a = cv2.split(lab)[1]
        lab_b = cv2.split(lab)[2]
        ycrcb_cb = cv2.split(ycrcb)[2]

        lab_image = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        lab_split = cv2.split(lab_image)
        ycrcb_split = cv2.split(ycrcb)
        rgb_split = cv2.split(mat)
        #yuv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2YUV)
        #yuv_split = cv2.split(yuv_image)
        #hsv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2HSV)
        #hsv_split = cv2.split(hsv_image)

        channel = hsv_split[0]

        lab_bthreshed = cv2.adaptiveThreshold(channel, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, self.options['block_size'], self.options['c_thresh'])
        #lab_bthreshed = cv2.inRange(lab_split[2], self.options["lab_b_min"], self.options["lab_b_max"])
        #yuv_vthreshed = cv2.inRange(yuv_split[2], self.options["yuv_v_min"], self.options["yuv_v_max"])
        #hsv_hthreshed = cv2.inRange(hsv_split[0], self.options["hsv_h_min"], self.options["hsv_h_max"])

        #final_threshed = lab_bthreshed & yuv_vthreshed & hsv_hthreshed


        erode_size = self.options['erode_size']
        dilate_size = self.options['dilate_size']
        erode_element = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (erode_size * 2 + 1, erode_size * 2 + 1),
                                                  (erode_size, erode_size))
        dilate_element = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (dilate_size * 2 + 1, dilate_size * 2 + 1),
                                                   (dilate_size, dilate_size))

        #NORMAL
        orange_l_threshed = cv2.inRange(lab_split[0], self.options['orange_lab_l_min'],
                                                     self.options['orange_lab_l_max'])
        if self.options['orange_debug']:
            self.post('orange_l_threshed', orange_l_threshed)

        orange_v_threshed = cv2.inRange(hsv_split[2], self.options['orange_hsv_v_min'],
                                                     self.options['orange_hsv_v_max'])
        if self.options['orange_debug']:
            self.post('orange_v_threshed', orange_v_threshed)

        orange_r_threshed = cv2.inRange(rgb_split[2], self.options['orange_rgb_r_min'],
                                                     self.options['orange_rgb_r_max'])
        if self.options['orange_debug']:
            self.post('orange_r_threshed', orange_r_threshed)

        max_r_threshed = cv2.inRange(rgb_split[2], self.options['max_redness'],
                                                     255)
        if self.options['orange_debug']:
            self.post('max_r_threshed', max_r_threshed)

        min_r_threshed = cv2.inRange(rgb_split[2], self.options['min_redness'],
                                                     255)
        if self.options['orange_debug']:
            self.post('nmin_r_threshed', min_r_threshed)

        orange_y_threshed = cv2.inRange(ycrcb_split[0], self.options['orange_ycrcb_y_min'],
                                                       self.options['orange_ycrcb_y_max'])
        if self.options['orange_debug']:
            self.post('orange_y_threshed', orange_y_threshed)

        orange_threshed = orange_l_threshed & orange_v_threshed & orange_y_threshed
        max_threshed = orange_threshed & max_r_threshed
        min_threshed = orange_threshed & min_r_threshed
        orange_threshed = orange_threshed & orange_r_threshed


        orange_eroded = cv2.erode(orange_threshed, erode_element)
        orange_dilated = cv2.dilate(orange_eroded, dilate_element)
        o_max_eroded = cv2.erode(max_threshed, erode_element)
        o_max_dilated = cv2.dilate(o_max_eroded, dilate_element)
        o_min_eroded = cv2.erode(min_threshed, erode_element)
        o_min_dilated = cv2.dilate(o_min_eroded, dilate_element)

        #END NORMAL


        #DARK
        dark_l_threshed = cv2.inRange(lab_split[0], self.options['dark_lab_l_min'],
                                                     self.options['dark_lab_l_max'])
        if self.options['dark_debug']:
            self.post('dark_l_threshed', dark_l_threshed)

        dark_v_threshed = cv2.inRange(hsv_split[2], self.options['dark_hsv_v_min'],
                                                     self.options['dark_hsv_v_max'])
        if self.options['dark_debug']:
            self.post('dark_v_threshed', dark_v_threshed)

        dark_r_threshed = cv2.inRange(rgb_split[2], self.options['dark_rgb_r_min'],
                                                     self.options['dark_rgb_r_max'])
        if self.options['dark_debug']:
            self.post('dark_r_threshed', dark_r_threshed)

        dark_y_threshed = cv2.inRange(ycrcb_split[0], self.options['dark_ycrcb_y_min'],
                                                       self.options['dark_ycrcb_y_max'])
        if self.options['dark_debug']:
            self.post('dark_y_threshed', dark_y_threshed)

        dark_threshed = dark_l_threshed & dark_v_threshed & dark_y_threshed & dark_r_threshed

        dark_eroded = cv2.erode(dark_threshed, erode_element)
        dark_dilated = cv2.dilate(dark_eroded, dilate_element)

        #END DARK

        #LIGHT
        light_l_threshed = cv2.inRange(lab_split[0], self.options['light_lab_l_min'],
                                                     self.options['light_lab_l_max'])
        if self.options['light_debug']:
            self.post('light_l_threshed', light_l_threshed)

        light_v_threshed = cv2.inRange(hsv_split[2], self.options['light_hsv_v_min'],
                                                     self.options['light_hsv_v_max'])
        if self.options['light_debug']:
            self.post('light_v_threshed', light_v_threshed)

        light_r_threshed = cv2.inRange(rgb_split[2], self.options['light_rgb_r_min'],
                                                     self.options['light_rgb_r_max'])
        if self.options['light_debug']:
            self.post('light_r_threshed', light_r_threshed)

        light_y_threshed = cv2.inRange(ycrcb_split[0], self.options['light_ycrcb_y_min'],
                                                       self.options['light_ycrcb_y_max'])
        if self.options['light_debug']:
            self.post('light_y_threshed', light_y_threshed)

        light_threshed = light_l_threshed & light_v_threshed & light_y_threshed & light_r_threshed

        light_eroded = cv2.erode(light_threshed, erode_element)
        light_dilated = cv2.dilate(light_eroded, dilate_element)

        #END LIGHT

        if self.options['debugging']:
            self.post("Threshed", orange_threshed)
            self.post("Dark Threshed", dark_threshed)
            self.post("LIght Threshed", light_threshed)
            #self.post("Masked", cv2.bitwise_and(mat, mat, mask=final_threshed))
            #self.post("Eroded", eroded)
            #self.post("Eroded_Dilated", dilated.copy())

        _, o_contours, _ = cv2.findContours(orange_dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        _, d_contours, _ = cv2.findContours(dark_dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        _, l_contours, _ = cv2.findContours(light_dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        _, max_contours, _ = cv2.findContours(o_max_dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        _, min_contours, _ = cv2.findContours(o_min_dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

        contours = o_contours + d_contours + l_contours + max_contours + min_contours
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
            if area < image_size * self.options['min_percent_frame'] or area > image_size * self.options['max_percent_frame']:
                continue

            rotated_rect = cv2.minAreaRect(c)
            contour = np.array(cv2.boxPoints(rotated_rect)).astype(int)

            side_ratio = (rotated_rect[1][0]/rotated_rect[1][1])
            if 1 / self.options['aspect_ratio_threshold'] < side_ratio and side_ratio < self.options['aspect_ratio_threshold']:
                continue

            rectangularity = area / (rotated_rect[1][0] * rotated_rect[1][1])
            if rectangularity < self.options['min_rectangularity']:
                continue

            center = rotated_rect[0]
            heuristic_score = rectangularity ** self.options["heuristic_power"] * cv2.contourArea(contour)
            x = contour_info(c, contour, rotated_rect, rotated_rect[2], area, center, rectangularity, heuristic_score)

            contour_data.append(x)

        if len(contour_data) == 0:
            pipe_group.heuristic_score = 0
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
