#!/usr/bin/env python3

import traceback
import sys

import cv2
import numpy as np
import shm

from vision.modules.base import ModuleBase
from vision import options

options = [options.IntOption('red_lab_a_min', 200, 0, 255),
           options.IntOption('red_lab_a_max', 255, 0, 255),
           options.IntOption('black_lab_l_min', 0, 0, 255),
           options.IntOption('black_lab_l_max', 50, 0, 255),
           options.IntOption('green_lab_a_min', 0, 0, 255),
           options.IntOption('green_lab_a_max', 100, 0, 255),
           options.IntOption('blur_kernel', 1, 0, 255),
           options.IntOption('erode_kernel', 2, 0, 255),
           options.IntOption('black_erode_iters', 5, 0, 100),
           options.IntOption('canny_low_thresh', 100, 0, 1000),
           options.IntOption('canny_high_thresh', 200, 0, 1000),
           options.IntOption('hough_lines_rho', 5, 1, 1000),
           options.IntOption('hough_lines_theta', 10, 1, 1000),
           options.IntOption('hough_lines_thresh', 70, 0, 1000),
          ]

class Roulette(ModuleBase):

    def process(self, mat):
        try:
            lab = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
            lab_split = cv2.split(lab)
            self.post('lab_a_split', lab_split[1])

            # detect red section
            threshed = cv2.inRange(lab_split[1],
                    self.options['red_lab_a_min'],
                    self.options['red_lab_a_max'])
            threshed = cv2.erode(threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1))
            self.post('red_threshed', threshed)

            # draw centroids for red sections
            _, contours, _ = cv2.findContours(threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            for contour in contours:
                moments = cv2.moments(contour)
                cX = int(moments['m10'] / moments['m00'])
                cY = int(moments['m01'] / moments['m00'])
                cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
                cv2.circle(mat, (cX, cY), 7, (255, 255, 255), -1)
            self.post('centroids', mat)

            # detect black section
            threshed = cv2.inRange(lab_split[0],
                    self.options['black_lab_l_min'],
                    self.options['black_lab_l_max'])
            threshed = cv2.erode(threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1),
                    iterations=self.options['black_erode_iters'])
            self.post('black_threshed', threshed)

            # draw centroids for black sections
            _, contours, _ = cv2.findContours(threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            for contour in contours:
                moments = cv2.moments(contour)
                cX = int(moments['m10'] / moments['m00'])
                cY = int(moments['m01'] / moments['m00'])
                cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
                cv2.circle(mat, (cX, cY), 7, (255, 255, 255), -1)
            self.post('centroids', mat)

            # detect green section
            threshed = cv2.inRange(lab_split[1],
                    self.options['green_lab_a_min'],
                    self.options['green_lab_a_max'])
            threshed = cv2.erode(threshed,
                    (2 * self.options['erode_kernel'] + 1,
                    2 * self.options['erode_kernel'] + 1))
            self.post('green_threshed', threshed)

            # TODO: Use red section to determine center, rather than green
            # detect diameters of green section to calculate location of center
            # of roulette board
            blurred = cv2.GaussianBlur(threshed,
                    (2 * self.options['blur_kernel'] + 1,
                    2 * self.options['blur_kernel'] + 1), 0)
            self.post('blurred', blurred)

            edges = cv2.Canny(blurred,
                    self.options['canny_low_thresh'],
                    self.options['canny_high_thresh'])
            self.post('edges', edges)
            lines = cv2.HoughLines(edges,
                    self.options['hough_lines_rho'],
                    self.options['hough_lines_theta'] * np.pi / 180,
                    self.options['hough_lines_thresh'])

            lines = [(idx, line[0]) for (idx, line) in enumerate(lines[:2])]
            line_equations = []
            lines_mat = mat.copy()
            for (i, (rho,theta)) in lines:
                a = np.cos(theta)
                b = np.sin(theta)
                x0 = a*rho
                y0 = b*rho
                x1 = int(x0 + 1000*(-b))
                y1 = int(y0 + 1000*(a))
                x2 = int(x0 - 1000*(-b))
                y2 = int(y0 - 1000*(a))
                cv2.line(lines_mat,(x1,y1),(x2,y2),(0,0,255),2)
                line_equations.append((float(x1), float(x2), float(y1), float(y2)))

            self.post('lines', lines_mat)

            # calculate intersection of diameters of green section
            [x01, x02, y01, y02] = line_equations[0]
            [x11, x12, y11, y12] = line_equations[1]
            b1 = (y02 - y01) / max(1e-10, x02 - x01)
            b2 = (y12 - y11) / max(1e-10, x12 - x11)
            intersection_x = int((b1 * x01 - b2 * x11 + y11 - y01) / (b1 - b2))
            intersection_y = int(b1 * (intersection_x - x01) + y01)
            center_mat = mat.copy()
            cv2.circle(center_mat, (intersection_x, intersection_y), 7, (255, 255, 255), -1)
            self.post('center', center_mat)

            # draw centroids of green sections and predict location ~3 seconds later
            _, contours, _ = cv2.findContours(threshed.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            for contour in contours:
                moments = cv2.moments(contour)
                cX = int(moments['m10'] / max(1e-10, moments['m00']))
                cY = int(moments['m01'] / max(1e-10, moments['m00']))
                cv2.drawContours(mat, [contour], -1, (0, 255, 0), 2)
                cv2.circle(mat, (cX, cY), 7, (255, 255, 255), -1)
                self.post('centroids', mat)
                translated_x = cX - intersection_x
                translated_y = cY - intersection_y
                predicted_x = translated_x * np.cos(np.radians(20)) - translated_y * np.sin(np.radians(20))
                predicted_y = translated_x * np.sin(np.radians(20)) + translated_y * np.cos(np.radians(20))
                predicted_x = int(predicted_x + intersection_x)
                predicted_y = int(predicted_y + intersection_y)
                cv2.circle(mat, (predicted_x, predicted_y), 7, (255, 0, 0), -1)
            self.post('predicted_green', mat)
        except Exception as e:
            traceback.print_exc(file=sys.stdout)

if __name__ == '__main__':
    Roulette('downward', options)()
