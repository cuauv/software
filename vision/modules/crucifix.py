#!/usr/bin/env python3
from math import pi, sqrt
import time

import cv2
import numpy as np

import shm

from vision.modules.base import ModuleBase
from vision.framework.color import bgr_to_lab
from vision.framework.transform import dilate, erode, rect_kernel, resize
from vision.framework.feature import find_lines
from vision.framework.draw import draw_line

from vision.modules.attilus_garbage import garlic_crucifix_opts as opts, lines_to_angles, angle_to_line, find_yellow_circle, intersect_circles, crop_by_mask, kmeans_mask, outline_mask
from vision.modules.gate import thresh_color_distance

from auv_python_helpers.angles import average_headings_degrees, heading_sub_degrees




COLORSPACE = 'lab'




class Crucifix(ModuleBase):
    def process(self, mat):
        self.post('org', mat)
        mat = resize(mat, mat.shape[1]//2, mat.shape[0]//2)
        print(mat.shape)
        shm.recovery_crucifix.cam_x.set(mat.shape[1]//2)
        shm.recovery_crucifix.cam_y.set(mat.shape[0]//2)
        cvtmat, split = bgr_to_lab(mat)
        self.circles = find_yellow_circle(split,
                                          color=[self.options['yellow_{}'.format(s)] for s in COLORSPACE],
                                          distance=self.options['circle_color_distance'],
                                          erode_kernel=self.options['circle_erode_kernel'],
                                          erode_iterations=self.options['circle_erode_iterations'],
                                          dilate_kernel=self.options['circle_dilate_kernel'],
                                          dilate_iterations=self.options['circle_dilate_iterations'],
                                          min_contour_size=self.options['circle_min_contour_size'],
                                          min_circularity=self.options['circle_min_circularity'],
                                          radius_offset=self.options['crucifix_circle_r_offset'])
        cv2.drawContours(mat, [c['contour'] for c in self.circles], 0, (255, 0, 0), 10)
        for c in self.circles:
            cv2.circle(mat, *c['circle'], (0, 255, 0), 10)
        self.post('circle', mat)
        self.find_crucifix(cvtmat, split)

    def find_crucifix(self, cvtmat, split):
        t = time.time()
        color = [self.options['green_{}'.format(s)] for s in COLORSPACE]
        distance = self.options['crucifix_color_distance']
        mask, _ = thresh_color_distance(split, color, distance, weights=[0.5, 2, 2])
        print('a', time.time() - t)
        t = time.time()
        # mask = erode(mask, rect_kernel(self.options['crucifix_erode_kernel']), iterations=self.options['crucifix_erode_iterations'])
        # mask = dilate(mask, rect_kernel(self.options['crucifix_dilate_kernel']), iterations=self.options['crucifix_dilate_iterations'])
        mask = erode(mask, rect_kernel(3), iterations=1)
        mask = dilate(mask, rect_kernel(3), iterations=1)
        print('b', time.time() - t)
        t = time.time()
        self.post('crucifix', mask)
        circle_id, mask_c = intersect_circles(self.circles, mask, min_size=self.options['crucifix_size_min'])
        print('c', time.time() - t)
        t = time.time()
        if circle_id is not None:
            shm.recovery_crucifix.visible.set(True)
            (x, y), r = self.circles[circle_id]['circle']
            shm.recovery_crucifix.center_x.set(x)
            shm.recovery_crucifix.center_y.set(y)
            shm.recovery_crucifix.offset_x.set(int(x + self.options['crucifix_offset_x'] * r))
            shm.recovery_crucifix.offset_y.set(y)
            shm.recovery_crucifix.size.set(pi * r**2)
            only_circle = cv2.bitwise_and(cvtmat, cvtmat, mask=mask_c)
            self.post('hmmm', only_circle)
            only_circle = crop_by_mask(cvtmat, mask_c, x, y, r)
            print('d', time.time() - t)
            t = time.time()
            cross = kmeans_mask(only_circle, x, y, r,
                                target_centeroid=(self.options['green_l'], self.options['green_a'], self.options['green_b']),
                                centeroids=3, remove_noise=False,
                                morph_kernel=self.options['kmeans_morph_kernel'],
                                morph_iterations=self.options['kmeans_morph_iterations'])
            # cross = dilate(cross, rect_kernel(self.options['kmeans_morph_kernel']))
            # cross = erode(cross, rect_kernel(self.options['kmeans_morph_kernel']))
            print('e', time.time() - t)
            t = time.time()
            self.post('crus', cross)
            cross = outline_mask(cross, simplify=False)
            lines, angle = self.find_crucifix_angles(cross)
            print('f', time.time() - t)
            t = time.time()
            cross = cv2.cvtColor(cross, cv2.COLOR_GRAY2BGR)
            for l in lines:
                draw_line(cross, (int(l[0]), int(l[1])), (int(l[2]), int(l[3])), thickness=5)

            draw_line(cvtmat, *angle_to_line(self.options['manipulator_angle'], origin=(x, y)), color=(0, 255, 0), thickness=4)

            draw_line(cvtmat, *angle_to_line(angle, origin=(x,y)), color=(255, 0, 0), thickness=5)

            self.post('angles', cvtmat)
            self.post('hmm', cross)
        else:
            shm.recovery_crucifix.visible.set(False)
        print('done')

    def find_crucifix_angles(self, garlic_mask):
        lines = find_lines(garlic_mask, 2, pi/180, self.options['garlic_line_threshold'])[0]
        # print(lines)
        center = garlic_mask.shape[0]//2, garlic_mask.shape[1]//2

        def distance_from_center(line):
            num = abs((line[3]-line[1])*center[1]-(line[2]-line[0])*center[0] + line[2]*line[1]-line[3]*line[0])
            denom = sqrt((line[3]-line[1])**2 + (line[2]-line[0])**2)
            return num/denom

        lines = list(filter(lambda x: distance_from_center(x) < 20, lines))
        angles = np.array([lines_to_angles(l)*180/pi for l in lines], dtype=np.float32)
        average = average_headings_degrees(angles) + 90
        # print(average)

        shm.recovery_crucifix.angle_offset.set(heading_sub_degrees(self.options['manipulator_angle'], average))

        return lines, average




if __name__ == '__main__':
    Crucifix('downward', opts)()
