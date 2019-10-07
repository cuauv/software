#!/usr/bin/env python3
from math import pi, atan, sin, cos, sqrt
from functools import reduce

import cv2
import numpy as np

import shm

from vision.modules.base import ModuleBase
from vision.framework.color import bgr_to_lab, elementwise_color_dist, range_threshold, color_dist
from vision.framework.transform import elliptic_kernel, dilate, erode, rect_kernel, morph_remove_noise, simple_gaussian_blur, resize
from vision.framework.feature import outer_contours, find_lines
from vision.framework.draw import draw_line

from vision.modules.attilus_garbage import garlic_crucifix_opts as opts, KMEANS_ITER, lines_to_angles, vectors_to_degrees, angle_to_unit_circle, angle_to_line, find_yellow_circle, intersect_circles, crop_by_mask, kmeans_mask, outline_mask

from auv_python_helpers.angles import abs_heading_sub_degrees, heading_sub_degrees
from vision.modules.gate import thresh_color_distance



COLORSPACE = 'lab'




class Recovery(ModuleBase):
    def process(self, mat):
        self.post('org', mat)
        mat = resize(mat, mat.shape[1]//2, mat.shape[0]//2)
        shm.bins_garlic.center_x.set(mat.shape[0]//2)
        shm.bins_garlic.center_y.set(mat.shape[1]//2)
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
                                          radius_offset=self.options['garlic_circle_r_offset'])
        cv2.drawContours(mat, [c['contour'] for c in self.circles], 0, (255, 0, 0), 10)
        for c in self.circles:
            cv2.circle(mat, *c['circle'], (0, 255, 0), 10)
        self.post('circle', mat)
        self.find_red_garlic(cvtmat, split)

    def find_red_garlic(self, cvtmat, split):
        color = [self.options['red_{}'.format(s)] for s in COLORSPACE]
        distance = self.options['garlic_color_distance']
        mask, _ = thresh_color_distance(split, color, distance, ignore_channels=[0])
        mask = erode(mask, rect_kernel(self.options['garlic_erode_kernel']), iterations=self.options['garlic_erode_iterations'])
        mask = dilate(mask, rect_kernel(self.options['garlic_dilate_kernel']), iterations=self.options['garlic_dilate_iterations'])
        self.post('garlic', mask)
        circle_id, mask_c = intersect_circles(self.circles, mask, min_size=self.options['garlic_size_min'])
        if circle_id is not None:
            (x, y), r = self.circles[circle_id]['circle']
            shm.bins_garlic.center_x.set(x)
            shm.bins_garlic.center_y.set(y)
            only_circle = cv2.bitwise_and(cvtmat, cvtmat, mask=mask_c)
            self.post('hmmm', only_circle)
            only_circle = crop_by_mask(cvtmat, mask_c, x, y, r)  # TODO: move this to a big function in recovery common
            cross = kmeans_mask(only_circle, x, y, r,
                                target_centeroid=(self.options['red_l'], self.options['red_a'], self.options['red_b']),
                                centeroids=3, remove_noise=False,
                                morph_kernel=self.options['kmeans_morph_kernel'],
                                morph_iterations=self.options['kmeans_morph_iterations'])
            # cross = dilate(cross, rect_kernel(self.options['kmeans_morph_kernel']))
            # cross = erode(cross, rect_kernel(self.options['kmeans_morph_kernel']))
            self.post('crus', cross)
            cross = outline_mask(cross, simplify=False)
            lines, vectors = self.find_garlic_angles(cross)
            cross = cv2.cvtColor(cross, cv2.COLOR_GRAY2BGR)
            for l in lines:
                draw_line(cross, (int(l[0]), int(l[1])), (int(l[2]), int(l[3])), thickness=5)
            for a in vectors:
                draw_line(cvtmat, (x, y), (x+int(a[0]*1000), y+int(a[1]*1000)), thickness=5)

            angles = [vectors_to_degrees(a) for a in vectors]
            # print(angles)

            manipulator_vector = angle_to_unit_circle(self.options['manipulator_angle']/180*pi)
            # draw_line(cvtmat, (x, y), (x + int(manipulator_vector[0]*1000), y + int(manipulator_vector[1]*1000)), color=(0, 255, 0), thickness=4)
            draw_line(cvtmat, *angle_to_line(self.options['manipulator_angle'], origin=(x,y)), color=(0, 255, 0), thickness=4)

            closest = self.find_closest_angle(self.options['manipulator_angle'], *angles, post=True)
            if closest is not None:
                closest_unit_vector = angle_to_unit_circle(closest/180*pi)
                # draw_line(cvtmat, (x, y), (x + int(closest_unit_vector[0]*1000), y + int(closest_unit_vector[1]*1000)), color=(255, 0, 0), thickness=5)
                draw_line(cvtmat, *angle_to_line(closest, origin=(x,y)), color=(255, 0, 0), thickness=5)

                self.post('angles', cvtmat)
                self.post('hmm', cross)


    def find_garlic_angles(self, garlic_mask):
        lines = find_lines(garlic_mask, 2, pi/180, self.options['garlic_line_threshold'])[0]
        print(lines)

        center = garlic_mask.shape[0]//2, garlic_mask.shape[1]//2
        def distance_from_center(line):
            num = abs((line[3]-line[1])*center[1]-(line[2]-line[0])*center[0] + line[2]*line[1]-line[3]*line[0])
            denom = sqrt((line[3]-line[1])**2 + (line[2]-line[0])**2)
            return num/denom


        lines = list(filter(lambda x: distance_from_center(x) < 80, lines))
        angles = np.array([angle_to_unit_circle(lines_to_angles(l)) for l in lines], dtype=np.float32)
        if len(angles) < 2: return lines, angles

        criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
        compactness, label, centeroid = cv2.kmeans(angles, 2, None, criteria, KMEANS_ITER, cv2.KMEANS_RANDOM_CENTERS)

        return lines, centeroid

    def find_closest_angle(self, target, *angles, post=False):
        if len(angles) == 0: return
        # print(angles)
        closest = min(angles, key=lambda x: abs_heading_sub_degrees(target, x))
        print(closest)
        if post:
            shm.bins_garlic.angle_offset.set(heading_sub_degrees(target, closest))
        return closest





if __name__ == '__main__':
    Recovery('downward', opts)()
