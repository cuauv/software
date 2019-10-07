#!/usr/bin/env python3
from math import pi, atan, sin, cos
from auv_python_helpers.angles import heading_sub_degrees
from functools import reduce

import cv2
import numpy as np
import time
import shm

from vision.modules.base import ModuleBase
from vision.framework.color import bgr_to_lab, range_threshold
from vision.framework.transform import elliptic_kernel, dilate, erode, rect_kernel, resize
from vision.framework.feature import outer_contours, find_lines, contour_centroid, contour_area
from vision.framework.draw import draw_line
from vision.options import IntOption, DoubleOption

from vision.modules.attilus_garbage import thresh_color_distance, filter_contour_size, filter_shapularity, angle_to_line, MANIPULATOR_ANGLE
from vision.modules.gate import thresh_color_distance

opts = [
        IntOption('yellow_l', 186, 0, 255),  # 224
        IntOption('yellow_a', 130, 0, 255),
        IntOption('yellow_b', 200, 0, 255),
        IntOption('purple_l', 39, 0, 255),  # 224
        IntOption('purple_a', 160, 0, 255),
        IntOption('purple_b', 80, 0, 255),
        IntOption('yellow_color_distance', 20, 0, 255),
        IntOption('vampire_color_distance', 30, 0, 255),
        IntOption('contour_size_min', 1700, 0, 1000),
        IntOption('intersection_size_min', 20, 0, 1000),
        IntOption('erode_kernel_size', 3, 0, 100),
        IntOption('erode_iterations', 1, 0, 100),
        IntOption('dilate_kernel_size', 3, 0, 100),
        IntOption('dilate_iterations', 1, 0, 10),
        IntOption('line_thresh', 210, 0, 5000),
        IntOption('manipulator_angle', MANIPULATOR_ANGLE, 0, 359),
        DoubleOption('rectangle_padding', 0.7, -1, 1),
        DoubleOption('rectangularity_thresh', 0.8, 0, 1),
        DoubleOption('closed_offset', 0.27, 0, 2),
        DoubleOption('open_offset', 0.27, 0, 2),
        DoubleOption('vert_offset', -0.1, -2, 2),
]

COLORSPACE = "lab"


class Vampire(ModuleBase):
    def process(self, mat):
        t = time.perf_counter()
        self.post('org', mat)
        mat = resize(mat, mat.shape[1]//2, mat.shape[0]//2)
        shm.recovery_vampire.cam_x.set(mat.shape[1]//2)
        shm.recovery_vampire.cam_y.set(mat.shape[0]//2)
        # tt = time.perf_counter()
        # print('1 %f' % (tt - t))
        # print(mat.shape)
        _, split = bgr_to_lab(mat)
        d = self.options['vampire_color_distance']
        color = [self.options["yellow_%s" % c] for c in COLORSPACE]
        self.rectangles = self.find_yellow_rectangle(split, color, d, self.options['erode_kernel_size'],
                                                self.options['erode_iterations'],
                                                self.options['dilate_kernel_size'],
                                                self.options['dilate_iterations'],
                                                self.options['contour_size_min'],
                                                self.options['rectangularity_thresh'],
                                                -self.options['rectangle_padding'])
        # t = time.perf_counter()
        # print('2 %f' % (t - tt))

        for y in self.rectangles:
            rectangle = cv2.boxPoints(y['rectangle'])
            mat = cv2.drawContours(mat, [np.int0(rectangle)], 0, (0, 0, 255), 10)

        color = [self.options["purple_%s" % c] for c in COLORSPACE]
        # purple = self.find_color(mat, color, d, use_first_channel=False, erode_mask=True, dilate_mask=True, iterations=3, rectangular=False)
        # self.post('purple', purple)
        # purple_contours = self.contours_and_filter(purple, self.options['contour_size_min'])
        self.find_vampire(mat, split, color, d)

        # tt = time.perf_counter()
        # print('3 %f' % (tt-t))

        mat = cv2.drawContours(mat, [r['contour'] for r in self.rectangles], -1, (0, 255, 0), 10)
        # mat = cv2.drawContours(mat, purple_contours, -1, (0, 255, 0), 10)
        self.post('yellow_contours', mat)
        # print('4 %f' % (time.perf_counter() - tt))


    def find_yellow_rectangle(self, split, color, distance, erode_kernel, erode_iterations,
                              dilate_kernel, dilate_iterations, min_contour_size,
                              min_rectangularity, padding_offset):
        # mask = thresh_color_distance(split, color, distance, use_first_channel=False)
        # t = time.perf_counter()
        mask, _ = thresh_color_distance(split, color, distance, ignore_channels=[0])
        mask = erode(mask, rect_kernel(erode_kernel), iterations=erode_iterations)
        mask = dilate(mask, rect_kernel(dilate_kernel), iterations=dilate_iterations)
        self.post('mask', mask)
        # tt = time.perf_counter()
        # print('f %f' % (tt-t))

        contours = outer_contours(mask)
        contours = filter_contour_size(contours, min_contour_size)

        # ttt = time.perf_counter()
        # print('fo %f' % (ttt-tt))

        def box_area(contour):
            r = cv2.minAreaRect(contour)
            return r[1][0] * r[1][1]

        contours = filter_shapularity(box_area, contours, min_rectangularity)
        # print('foo %f' % (time.perf_counter() - ttt))

        def rectangle_with_offset(contour, offset=padding_offset):
            r = cv2.minAreaRect(contour)
            return r[0], (max(r[1][0] * (1-offset), 0), max(r[1][1] * (1-offset), 0)), r[2]

        return [{'contour': c, 'rectangle': rectangle_with_offset(c), 'rectangle_org': rectangle_with_offset(c, offset=0.2)} for c in contours]


    def find_vampire(self, mat, split, color, distance):
        # mask = thresh_color_distance(split, color, distance)
        mask, _ = thresh_color_distance(split, color, distance, weights=[0.5, 2, 2])
        self.post('purple', mask)
        rects = self.intersect_rectangles(self.rectangles, mask, self.options['intersection_size_min'])

        if self.rectangles:
            empty = max([r['rectangle'] for r in self.rectangles], key=lambda r: r[1][0] * r[1][1])

            align_angle = empty[2] + 90 if empty[1][1] > empty[1][0] else empty[2]
            align_angle = 360 + align_angle if align_angle < 0 else align_angle
            shm.recovery_vampire.empty_visible.set(True)
            shm.recovery_vampire.empty_x.set(int(empty[0][0]))
            shm.recovery_vampire.empty_y.set(int(empty[0][1]))
            shm.recovery_vampire.empty_offset_x.set(int(empty[0][0] + min(empty[1]) * self.options['open_offset']))
            shm.recovery_vampire.empty_offset_y.set(int(empty[0][1]))
            shm.recovery_vampire.empty_angle_offset.set(heading_sub_degrees(self.options['manipulator_angle'], align_angle))
            shm.recovery_vampire.empty_size.set(empty[1][0] * empty[1][1])
            cv2.circle(mat, (int(empty[0][0]), int(empty[0][1])), 5, color=(255, 255, 255), thickness=-1)


        opened = []
        closed = []

        for j in range(len(rects)):
            i, mask_r = rects[j]
            self.post('rectangle_%d' % j, mask_r)

            purple = cv2.bitwise_and(mask, mask_r)
            purple_contours = outer_contours(purple)

            purple_center = contour_centroid(max(purple_contours, key=contour_area))
            # print(purple_center)

            align_angle = self.rectangles[i]['rectangle'][2] if self.rectangles[i]['rectangle'][1][1] > self.rectangles[i]['rectangle'][1][0] else self.rectangles[i]['rectangle'][2] + 90
            align_angle = 360 + align_angle if align_angle < 0 else align_angle

            def point_in_rectangle(point, rect):
                contour = np.float32([cv2.boxPoints(rect)]).reshape(-1, 1, 2)
                return cv2.pointPolygonTest(contour, point, measureDist=False)

            if point_in_rectangle(purple_center, self.rectangles[i]['rectangle_org']) > 0:
                color = (0, 0, 255)
                opened.append({'center': purple_center, 'align': align_angle, 'size': self.rectangles[i]['rectangle'][1][0] * self.rectangles[i]['rectangle'][1][1], 'offset': (int(min(self.rectangles[i]['rectangle'][1]) * self.options['open_offset']), int(max(self.rectangles[i]['rectangle'][1]) * self.options['vert_offset']))})
            else:
                color = (255, 0, 0)
                direction = 1 if self.rectangles[i]['rectangle'][0][0] > purple_center[0] else -1
                closed.append({'center': purple_center, 'align': ((align_angle + 180) % 360) if direction == 1 else align_angle, 'size': self.rectangles[i]['rectangle'][1][0] * self.rectangles[i]['rectangle'][1][1], 'offset': (int(min(self.rectangles[i]['rectangle'][1]) * self.options['closed_offset']), int(max(self.rectangles[i]['rectangle'][1]) * self.options['vert_offset'])), 'direction': direction})

            # cv2.circle(mat, purple_center, 20, color=color, thickness=-1)
            # draw_line(mat, *angle_to_line(self.options['manipulator_angle'], origin=purple_center), thickness=5)
            # draw_line(mat, *angle_to_line(align_angle, origin=purple_center), color=color, thickness=5)

        opened = max(opened, key=lambda x: x['size']) if opened else None
        closed = max(closed, key=lambda x: x['size']) if closed else None


        if opened:
            cv2.circle(mat, opened['center'], 5, color=(0, 0, 255), thickness=-1)
            draw_line(mat, *angle_to_line(opened['align'], origin=opened['center']), color=(0, 0, 255), thickness=5)
            draw_line(mat, *angle_to_line(self.options['manipulator_angle'], origin=opened['center']), thickness=5)
            # print('nani %d' % opened['align'])
            shm.recovery_vampire.open_visible.set(True)
            shm.recovery_vampire.open_handle_x.set(opened['center'][0])
            shm.recovery_vampire.open_handle_y.set(opened['center'][1])
            shm.recovery_vampire.open_offset_x.set(opened['center'][0] + opened['offset'][0])
            shm.recovery_vampire.open_offset_y.set(opened['center'][1] + opened['offset'][1])
            shm.recovery_vampire.open_angle_offset.set(heading_sub_degrees(self.options['manipulator_angle'], opened['align']))
            shm.recovery_vampire.open_size.set(opened['size'])
        else:
            shm.recovery_vampire.open_visible.set(False)
        if closed:
            draw_line(mat, *angle_to_line(closed['align'], origin=closed['center']), color=(0, 255, 0), thickness=5)
            draw_line(mat, *angle_to_line(self.options['manipulator_angle'], origin=closed['center']), thickness=5)
            # print('what %d' % closed['align'])
            cv2.circle(mat, closed['center'], 5, color=(0, 255, 0), thickness=-1)
            shm.recovery_vampire.closed_visible.set(True)
            shm.recovery_vampire.closed_handle_x.set(closed['center'][0])
            shm.recovery_vampire.closed_handle_y.set(closed['center'][1])
            shm.recovery_vampire.closed_handle_direction.set(closed['direction'])
            shm.recovery_vampire.closed_offset_x.set(closed['center'][0] + closed['offset'][0])
            shm.recovery_vampire.closed_offset_y.set(closed['center'][1])
            shm.recovery_vampire.closed_angle_offset.set(heading_sub_degrees(self.options['manipulator_angle'], closed['align']))
            shm.recovery_vampire.closed_size.set(closed['size'])
        else:
            shm.recovery_vampire.closed_visible.set(False)

        self.post('hmm', mat)


    def intersect_rectangles(self, rectangles, mask, min_size):
        ret = []
        for i in range(len(rectangles)):
            c = rectangles[i]['rectangle']
            mask_c = np.zeros(mask.shape, dtype=np.uint8)
            mask_c = cv2.fillPoly(mask_c, [np.int0(cv2.boxPoints(c))], color=255)
            # self.post('mask_%d'%i, mask_c)
            intersect = cv2.bitwise_and(mask, mask_c)
            # self.post('intersect_%d'%i, intersect)
            if any(map(lambda x: cv2.contourArea(x) > min_size, outer_contours(intersect))):
                ret.append((i, intersect))
        return ret



if __name__ == '__main__':
    Vampire('downward', opts)()
