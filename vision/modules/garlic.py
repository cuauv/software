#!/usr/bin/env python3
from math import pi, atan, sin, cos
from functools import reduce
import time

import cv2
import numpy as np

from vision.modules.base import ModuleBase
from vision.framework.color import bgr_to_lab, range_threshold, color_dist
from vision.framework.transform import elliptic_kernel, dilate, erode, rect_kernel, morph_remove_noise, simple_gaussian_blur, resize
from vision.framework.feature import outer_contours, find_lines
from vision.framework.draw import draw_line
from vision.options import IntOption, DoubleOption

from auv_python_helpers.angles import abs_heading_sub_degrees, heading_sub_degrees
import shm

opts = [
        IntOption('yellow_l', 186, 0, 255),  # 224
        IntOption('yellow_a', 144, 0, 255),
        IntOption('yellow_b', 185, 0, 255),
        IntOption('red_l', 39, 0, 255),  # 224
        IntOption('red_a', 174, 0, 255),
        IntOption('red_b', 154, 0, 255),
        IntOption('lever_color_distance', 24, 0, 255),
        IntOption('contour_size_min', 50, 0, 1000),
        IntOption('intersection_size_min', 20, 0, 1000),
        IntOption('erode_kernel_size', 3, 0, 100),  # 18
        IntOption('dilate_yellow_kernel_size', 5, 0, 100),  # 18
        IntOption('dilate_red_kernel_size', 3, 0, 100),  # 18
        IntOption('dilate_iterations', 10, 0, 20),
        IntOption('line_thresh', 130, 0, 5000),
        IntOption('manipulator_angle', 25, -90, 90),
        IntOption('kmeans_size', 300, 0, 2000),
        DoubleOption('circularity_thresh', 0.5, 0, 1),
]

COLORSPACE = "lab"


def lines_to_angles(line):
    try:
        return atan((line[1]-line[3])/(line[0]-line[2]))
    except ZeroDivisionError:
        return pi/2
def vectors_to_degrees(vectors):
    degrees = atan(vectors[1]/vectors[0])*180/pi
    return degrees if degrees > 0 else 360+degrees
def angle_to_unit_circle(angle):
    return cos(angle), sin(angle)


class Color(ModuleBase):
    def process(self, mat):
        t = time.perf_counter()
        self.post('org', mat)
        shm.recovery_garlic.cam_x.set(mat.shape[1]//2)
        shm.recovery_garlic.cam_y.set(mat.shape[0]//2)
        # print(mat.shape)
        d = self.options['lever_color_distance']
        color = [self.options["yellow_%s" % c] for c in COLORSPACE]
        # yellow = self.find_color(mat, color, d, iterations=self.options['dilate_iterations'], rectangular=True)
        yellow = self.find_color(mat, color, d, erode_mask=self.options['erode_kernel_size'], dilate_mask=self.options['dilate_yellow_kernel_size'], iterations=self.options['dilate_iterations'], rectangular=True)
        yellow = erode(yellow, rect_kernel(self.options['dilate_yellow_kernel_size']), iterations=self.options['dilate_iterations'])
        self.post('yellow', yellow)
        yellow_contours = self.contours_and_filter(yellow, self.options['contour_size_min'])
        yellow_contours = self.filter_circles(yellow_contours, self.options['circularity_thresh'])

        tt = time.perf_counter()
        print("y %f" % (tt-t))

        color = [self.options["red_%s" % c] for c in COLORSPACE]
        red = self.find_color(mat, color, d, use_first_channel=True, erode_mask=self.options['erode_kernel_size'], dilate_mask=self.options['dilate_red_kernel_size'], iterations=10, rectangular=True)
        # red = dilate(red, elliptic_kernel(self.options['dilate_kernel_size']), iterations=1)
        red = erode(red, rect_kernel(self.options['dilate_red_kernel_size']), iterations=1)
        self.post('red', red)
        red_contours = outer_contours(red)

        t = time.perf_counter()
        print("yy %f " % (t-tt))

        platform, garlic = self.red_in_circle(mat, yellow_contours, red_contours)
        platform_center = None

        if platform is not None:
            platform_center = self.center(platform[0], post=True)
            garlic_mask = np.zeros((mat.shape[0], mat.shape[1]), dtype=np.uint8)
            garlic_mask = cv2.drawContours(garlic_mask, garlic, -1, (255, 0, 0), 10)
            self.post('garlic', garlic_mask)
            mat = cv2.circle(mat, platform_center, 4, (255, 255, 0), thickness=4)
            lines, garlic_vectors = self.find_garlic_angle(mat, garlic_mask)

            for l in lines:
                draw_line(mat, (l[0], l[1]), (l[2], l[3]), thickness=5)
            for v in garlic_vectors:
                draw_line(mat, platform_center, tuple([platform_center[i] + int(v[i] * 1000) for i in range(len(v))]), color=(0, 255, 0), thickness=5)
            manipulator_vector = angle_to_unit_circle(self.options['manipulator_angle']/180 * pi)
            draw_line(mat, platform_center, tuple([platform_center[i] + int(manipulator_vector[i]*1000) for i in range(len(platform_center))]), color=(255, 0, 0), thickness=4)
            closest = self.find_closest_angle(self.options['manipulator_angle'], *(vectors_to_degrees(v) for v in garlic_vectors), post=True)
            if closest is not None:
                closest_vector = angle_to_unit_circle(closest/180 * pi)
                draw_line(mat, platform_center, tuple([platform_center[i] + int(closest_vector[i] * 1000) for i in range(len(platform_center))]), color=(255, 255, 0), thickness=5)

        self.post('line', mat)

        mat = cv2.drawContours(mat, yellow_contours, -1, (0, 255, 0), 10)
        self.post('yellow_contours', mat)

        tt = time.perf_counter()
        print("yyy %f" % (tt-t))

    def find_color(self, mat, color, distance, use_first_channel=False, erode_mask=False, dilate_mask=False, iterations=1, rectangular=False):
        _, split = bgr_to_lab(mat)
        threshed = [range_threshold(split[i],
                    color[i] - distance, color[i] + distance)
                    for i in range(int(not use_first_channel), len(color))]
        combined = reduce(lambda x, y: cv2.bitwise_and(x, y), threshed)
        if dilate_mask or erode_mask:
            if erode_mask:
                kernel = elliptic_kernel(erode_mask) if not rectangular \
                        else rect_kernel(erode_mask)
                combined = erode(combined, kernel, iterations=1)
            if dilate_mask:
                kernel = elliptic_kernel(dilate_mask) if not rectangular \
                        else rect_kernel(dilate_mask)
                combined = dilate(combined, kernel, iterations=iterations)
        # _, contours, _ = cv2.findContours(combined, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        # filtered = [i for i in contours if cv2.contourArea(i) > self.options['contour_size_min']]
        return combined

    def contours_and_filter(self, mat, size_thresh):
        _, contours, _ = cv2.findContours(mat, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        filtered = [i for i in contours if cv2.contourArea(i) > size_thresh]
        return filtered

    def filter_circles(self, contours, circularity_thresh):
        def _check_circle(contour):
            circle_area = pi * cv2.minEnclosingCircle(contour)[1]**2
            area = cv2.contourArea(contour)
            return area/circle_area > circularity_thresh
        return [c for c in contours if _check_circle(c)]

    def red_in_circle(self, mat, yellow_contours, red_contours):
        mask = np.zeros((mat.shape[0], mat.shape[1]), dtype=np.uint8)
        circle_mat = mat.copy()
        for i in range(len(yellow_contours)):
            mask_y = mask.copy()
            mask_r = mask.copy()
            # mask_y = cv2.fillPoly(mask_y, [yellow_contours[i]], 255)
            (x, y), r = cv2.minEnclosingCircle(yellow_contours[i])
            mask_y = cv2.circle(mask_y, (int(x), int(y)), int(r), 255, -1)
            circle_mat = cv2.circle(circle_mat, (int(x), int(y)), 255, -1)
            self.post('mask_y%d' % i, mask_y)
            mask_r = cv2.fillPoly(mask_r, red_contours, 255)
            self.post('mask_r%d' % i, mask_r)
            intersection = cv2.bitwise_and(mask_y, mask_r)
            self.post('intersection%d' % i, intersection)
            if any(map(lambda x: cv2.contourArea(x) > self.options['intersection_size_min'], outer_contours(intersection))):
                self.post('circle_mat', circle_mat)
                return [yellow_contours[i]], self.kmeans(mat, yellow_contours[i])
                # dilate(intersection, rect_kernel(self.options['dilate_red_kernel_size']), iterations=6)
                # return [yellow_contours[i]], outer_contours(intersection)
        return None, None

    def kmeans(self, mat, yellow_contour):
        (x, y), r = cv2.minEnclosingCircle(yellow_contour)
        mat, _ = bgr_to_lab(mat)
        mat = mat[:, :, 1:]
        # mat = mat[int(y-r):int(y+r), int(x-r):int(x+r), :]
        self.post('hmm', mat)
        mask = np.zeros((mat.shape[0], mat.shape[1]), dtype=np.uint8)
        mask_y = cv2.circle(mask, (int(x), int(y)), int(r), 255, -1)
        masked = cv2.bitwise_and(mat, mat, mask=mask_y)
        self.post('masked', masked)
        masked = masked[int(y-r):int(y+r), int(x-r):int(x+r), :]
        size = min(self.options['kmeans_size'], int(2*r))
        masked = resize(masked, size, size)
        simple_gaussian_blur(masked, 7, 3)
        if not all(masked.shape): return
        print(masked.shape)
        # color = masked[int(r), 5]
        # masked = np.where(masked==(0,0), color, masked)
        array = masked.reshape((-1, 2))
        array = np.float32(array)
        # print(array)
        criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
        compactness, labels, centers = cv2.kmeans(array, 3, None, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)
        array = np.uint8(array)
        # print(array.shape)
        labels = labels.ravel()
        # array = np.where(labels==0, (255, 0, 0), array)
        # array = np.where(labels==1, (0, 255, 0), array)
        # array = np.where(labels==0, (0, 0, 255), array)
        mean = [(np.mean(array[labels.ravel()==i], axis=0), i) for i in range(3)]
        print(mean)
        mean.sort(key=lambda x: color_dist((x[0][0], x[0][1], 0), (self.options['red_a'], self.options['red_b'], 0)))
        red_label = mean[0][1]
        print(red_label)
        post = np.full((array.shape[0], ), 0, dtype=np.uint8)
        # post[labels.ravel()!=red_label] = 0
        post[labels.ravel()==red_label] = 255
        # array[labels.ravel()==2] = (0, 255)
        # labels = np.uint8(labels)
        post = post.reshape((masked.shape[0], masked.shape[1]))
        print(post.shape)
        # r = self.options['kmeans_size'] if self.options['kmeans_size'] < r*2 else r*2
        print(r)
        post = post[int(r*0.3):-int(r*0.3), int(r*0.3):-int(r*0.3)]
        another_mask = np.zeros((post.shape[0], post.shape[1]), dtype=np.uint8)
        cv2.circle(another_mask, (post.shape[0]//2, post.shape[1]//2), post.shape[0]//2, 255, -1)
        post = cv2.bitwise_and(post, post, mask=another_mask)
        post = morph_remove_noise(post, rect_kernel(5))
        # print(post)
        self.post('array', post)
        print(centers)
        print(post.shape)
        return outer_contours(post)




    def center(self, contour, post=False):
        M = cv2.moments(contour)
        ret = (int(M['m10']/M['m00']), int(M['m01']/M['m00']))
        if post:
            shm.recovery_garlic.center_x.set(ret[0])
            shm.recovery_garlic.center_y.set(ret[1])
        return ret

    def find_garlic_angle(self, mat, garlic_mask):
        try:
            lines = find_lines(garlic_mask, 1, pi/180, self.options['line_thresh'])

            angles = np.array([angle_to_unit_circle(lines_to_angles(l)) for l in lines[0]], dtype=np.float32)
            if len(angles) < 2:
                return [], []

            criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
            compactness, label, centeroid = cv2.kmeans(angles, 2, None, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)

            return lines[0], centeroid  # , [atan(c[1]/c[0]) for c in center]

        except cv2.error as e:
            print(e)
            return [], []

    def find_closest_angle(self, target, *angles, post=False):
        if len(angles) == 0: return
        closest = min(angles, key=lambda x: abs_heading_sub_degrees(target, x))
        if post:
            shm.recovery_garlic.angle_offset.set(heading_sub_degrees(target, closest))
        return closest

if __name__ == '__main__':
    Color('downward', opts)()
