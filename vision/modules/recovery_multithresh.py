#!/usr/bin/env python3

import math
import time
from collections import namedtuple
import cv2
import numpy as np
import shm
from mission.constants.config import recovery as constants
from vision.modules.base import ModuleBase
from vision.options import IntOption, DoubleOption, BoolOption

mark_opts = []
for color in ['red', 'green']:
    for thresh_info in getattr(constants, '{}_mark_threshes'.format(color)):
        opt_name = '{} mark {} c'.format(color, thresh_info.colorspace)
        opt = IntOption(opt_name, thresh_info.c, -40, 40)
        mark_opts.append(opt)

opts = [
    BoolOption('debugging', constants.debugging),
    IntOption('max fps', 15, 1, 120),

    IntOption('table block size', 673, 0, 2000),
    IntOption('table c', 6, -20, 20),
    IntOption('table blur size', 9, 0, 100),
    IntOption('table morph size', 20, 0, 100),
    IntOption('table min area', 20000, 0, 200000),
    IntOption('max clipping distance', 5, 0, 50),

    IntOption('red c', constants.red_c, -20, 20),
    IntOption('green c', constants.green_c, -40, 40),

    IntOption('mark block size', 400, 1, 2000),
    IntOption('min mark contour points', 5, 0, 100),
    IntOption('min mark area', 400, 0, 2000),
    DoubleOption('min mark ellipticality', 0.9, 0, 2),
    DoubleOption('max mark ellipticality', 1.1, 0, 2),
    DoubleOption('min mark inner radius ratio', 0.8, 0, 1),
    IntOption('mark mask dilate kernel size', 3, 1, 50),

    IntOption('stack block size', 200, 1, 2000),
    IntOption('stack morph size', 7, 0, 50),
    IntOption('min stack area', constants.min_stack_area, 0, 2000),
    IntOption('min stack width', constants.min_stack_width, 0, 2000),
    DoubleOption('min horizontal stack ratio', 3, 0, 10),
    DoubleOption('min stack fill ratio', 0.7, 0, 1),
    DoubleOption('left crop', 0.2, 0, 1),
    DoubleOption('min stack blocking width ratio', 0.1, 0, 1),

    IntOption('min region size', 200, 0, 2000),
] + mark_opts

class Recovery(ModuleBase):
    # Crop the left edge of the camera so we don't see stacks we pick up
    x_props = [
        'stack_1_x',
        'stack_2_x',
        'stack_3_x',
        'stack_4_x',
        'green_mark_x',
        'red_mark_x',
        'green_region_x',
        'red_region_x',
        'table_x',
    ]

    def process(self, mat):
        start_time = time.time()

        if shm.recovery_state.grabber_stack_present.get():
            self.left_crop_pixels = int(self.options['left crop'] * mat.shape[1])
        else:
            self.left_crop_pixels = 0
        self.uncropped = mat
        mat = mat[:,self.left_crop_pixels:]

        self.results = shm.recovery_vision.get()
        self.fill_single_camera_direction(self.results)

        self.post('original', mat)

        self.lab_l, self.lab_a, self.lab_b = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2LAB))
        self.post('lab a', self.lab_a)
        self.post('lab b', self.lab_b)

        self.luv_l, self.luv_u, self.luv_v = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2LUV))
        self.post('luv u', self.luv_u)

        self.bgr_b, self.bgr_g, self.bgr_r = cv2.split(mat)
        self.post('bgr b', self.bgr_b)

        # table_mask = self.table(mat)
        table_mask = np.zeros(mat.shape[:2], np.uint8)
        mark_mask = self.marks(mat)
        tower_mask = cv2.bitwise_not(cv2.bitwise_or(table_mask, mark_mask))
        self.tower(mat, tower_mask)

        # Offset image results by crop
        for prop in self.x_props:
            setattr(self.results, prop, getattr(self.results, prop) + self.left_crop_pixels)
        shm.recovery_vision.set(self.results)

        runtime = time.time() - start_time
        min_runtime = 1 / self.options['max fps']
        if min_runtime > runtime:
            time.sleep(min_runtime - runtime)
            runtime = min_runtime
        print('FPS: {}'.format(1 / (runtime)))

    def left_crop_pixels(self, mat):
        if shm.recovery_state.grabber_stack_present.get():
            return int(self.left_crop * mat.shape[1])
        else:
            return 0

    def post(self, tag, mat):
        if not self.options['debugging']:
            return

        out_mat = None
        if self.left_crop_pixels > 0:
            if mat.ndim == 3:
                out_mat = np.empty_like(self.uncropped)
                out_mat[:, self.left_crop_pixels:] = mat

                cropped_hls = cv2.cvtColor(self.uncropped[:,:self.left_crop_pixels], cv2.COLOR_BGR2HLS)
                cropped_hls[..., 1] = np.uint8(cropped_hls[..., 1] * 0.5)
                out_mat[:, :self.left_crop_pixels] = cv2.cvtColor(cropped_hls, cv2.COLOR_HLS2BGR)
            else:
                height, width = self.uncropped.shape[:2]
                out_mat = np.zeros((height, width), dtype=np.uint8)
                out_mat[:, self.left_crop_pixels:] = mat
        else:
            out_mat = mat

        super().post(tag, out_mat)

    def is_clipping(self, mat, contour):
        """
        Returns if any points on the contour are close to an edge of the camera view
        """
        height, width = mat.shape[:2]
        distance = self.options['max clipping distance']
        for pt in contour:
            if not distance <= pt[0][0] <= (width - distance) or \
                    not distance <= pt[0][1] <= (height - distance):
                return True
        return False

    def draw_text(self, mat, text, pos):
        cv2.putText(mat, text, pos, cv2.FONT_HERSHEY_DUPLEX, 1, (255, 50, 255), thickness=2)

    def draw_contours(self, mat, *contours):
        cv2.drawContours(mat, contours, -1, (0, 127, 255), thickness=3)

    def contour_center(self, contour):
        moments = cv2.moments(contour)
        return (moments['m10'] / moments['m00'], moments['m01'] / moments['m00'])

    def table(self, mat):
        """
        Detect anything (including multiple things) that looks remotely
        like a table, just to create a mask of where to not look for stacks or
        the tower.
        """
        self.results.table_visible = False

        # blurred = cv2.GaussianBlur(self.luv_v, (self.options['table blur size'] * 2 + 1,) * 2, 0)
        # self.post('table blurred', blurred)
        adapted = cv2.adaptiveThreshold(
            self.luv_v,
            255,
            cv2.ADAPTIVE_THRESH_MEAN_C,
            cv2.THRESH_BINARY_INV,
            self.options['table block size'] * 2 + 1,
            self.options['table c'],
        )
        self.post('table adapted', adapted)

        # morph_kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (self.options['table morph size'],) * 2)
        # morphed = cv2.morphologyEx(adapted, cv2.MORPH_ELLIPSE, morph_kernel)
        # self.post('table morphed', morphed)

        _, contours, hierarchy = cv2.findContours(adapted.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        table_mask = np.zeros(mat.shape[:2], np.uint8)
        if len(contours) > 0:
            for contour in contours:
                if cv2.contourArea(contour) < self.options['table min area']:
                    continue
                hull = cv2.convexHull(contour)
                cv2.fillConvexPoly(table_mask, hull, 255)

        self.post('table mask', table_mask)
        return table_mask

    def marks(self, mat):
        """
        Finds the marks on the table. Returns a mask of the found marks.

        A mark is considered a contour of a red / green threshed image
        that is highly elliptical, has a large area, and has a corresponding
        'hole' contour that is also highly elliptical and has similar major/minor
        axes lengths.
        """
        def is_ellipse_valid(contour, ellipse):
            contour_area = cv2.contourArea(contour)
            if contour_area < self.options['min mark area']:
                return False

            ellipse_a, ellipse_b = ellipse[1]
            if ellipse_a < ellipse_b:
                ellipse_a, ellipse_b = ellipse_b, ellipse_a
            ellipse_area = math.pi * (ellipse_a / 2) * (ellipse_b / 2)
            ellipticality = ellipse_area / contour_area
            if ellipticality < self.options['min mark ellipticality'] or \
                    ellipticality > self.options['max mark ellipticality']:
                return False

            return True

        def set_results(color, name, props):
            for prop in props:
                setattr(self.results, '{}_{}_{}'.format(color, name, prop), props[prop])

        for color in ['red', 'green']:
            final_marks, final_regions = [], []

            for thresh_info in getattr(constants, '{}_mark_threshes'.format(color)):
                threshed = cv2.adaptiveThreshold(
                    getattr(self, thresh_info.colorspace),
                    255,
                    cv2.ADAPTIVE_THRESH_MEAN_C,
                    cv2.THRESH_BINARY if not thresh_info.invert else cv2.THRESH_BINARY_INV,
                    self.options['mark block size'] * 2 + 1,
                    self.options['{} mark {} c'.format(color, thresh_info.colorspace)],
                )
                self.post('{} marks {} threshed'.format(color, thresh_info.colorspace), threshed)

                _, contours, hierarchy = cv2.findContours(threshed.copy(), cv2.RETR_CCOMP, cv2.CHAIN_APPROX_SIMPLE)
                if len(contours) > 0:
                    HullInfo = namedtuple('HullInfo', ['contour', 'area'])
                    # Find the region of this color with the largest convex hull
                    # area; if we've above the table, this should detect marks even
                    # if they have stacks on them.
                    all_region_infos = []
                    for c in contours:
                        hull = cv2.convexHull(c)
                        area = cv2.contourArea(hull)
                        all_region_infos.append(HullInfo(hull, area))
                    region_info = max(all_region_infos, key=lambda x: x.area)
                    if region_info.area >= self.options['min region size']:
                        final_regions.append(region_info.contour)
                    ContourInfo = namedtuple('ContourInfo', ['contour', 'area'])
                    contour_infos = []
                    for i, hierarchy_info in enumerate(hierarchy[0]):
                        # Find the largest inner contour, if any. Don't just take
                        # the first inner contour, becasome sometimes is actually
                        # just a small contour inside the circle ring, not the one
                        # that goes around the inside of the circle ring.
                        hole_index = hierarchy_info[2]
                        inner_contours = []
                        while hole_index != -1:
                            inner_contours.append(contours[hole_index])
                            hole_index = hierarchy[0][hole_index][0]
                        if len(inner_contours) == 0:
                            continue

                        outer_contour = contours[i]
                        inner_contour = max(inner_contours, key=lambda x: cv2.contourArea(x))
                        if len(outer_contour) < self.options['min mark contour points'] or \
                                len(inner_contour) < self.options['min mark contour points']:
                            continue

                        outer_ellipse, inner_ellipse = cv2.fitEllipse(outer_contour), cv2.fitEllipse(inner_contour)
                        if not is_ellipse_valid(outer_contour, outer_ellipse) or \
                                not is_ellipse_valid(inner_contour, inner_ellipse):
                            continue

                        if inner_ellipse[1][0] / outer_ellipse[1][1] < self.options['min mark inner radius ratio'] or \
                                inner_ellipse[1][0] / outer_ellipse[1][1] < self.options['min mark inner radius ratio']:
                            continue
                        contour_infos.append(ContourInfo(outer_contour, outer_ellipse[2]))

                    if len(contour_infos) > 0:
                        best_contour = max(contour_infos, key=lambda x: x.area)
                        final_marks.append(best_contour)

            set_results(color, 'mark', {'visible': False})
            set_results(color, 'region', {'visible': False})

            contours_mat = mat.copy()
            mark_mask = np.zeros(mat.shape[:2], np.uint8)

            if len(final_marks) > 0:
                final_mark = final_marks[0]
                self.draw_contours(contours_mat, final_mark)
                cv2.drawContours(mark_mask, [final_mark], -1, 255, thickness=-1)
                center = self.contour_center(final_mark)
                area = cv2.contourArea(final_mark)

                set_results(color, 'mark', {
                    'visible': True,
                    'x': center[0],
                    'y': center[1],
                    'area': area,
                })

            if len(final_regions) > 0:
                final_region = final_regions[0]
                center = self.contour_center(final_region)
                area = cv2.contourArea(final_region)
                set_results(color, 'region', {
                    'visible': True,
                    'x': center[0],
                    'y': center[1],
                    'area': area,
                })

        dilate_kernel = cv2.getStructuringElement(
            cv2.MORPH_ELLIPSE,
            (self.options['mark mask dilate kernel size'],) * 2
        )
        cv2.dilate(mark_mask, dilate_kernel)

        self.post('mark contours', contours_mat)
        self.post('marks mask', mark_mask)

        return mark_mask

    def marks_old(self, mat, table_contour):
        MarkInfo = namedtuple('MarkInfo', ['threshed', 'color_name', 'color_val'])
        threshed_marks = [
            MarkInfo(
                cv2.adaptiveThreshold(
                    self.lab_a,
                    255,
                    cv2.ADAPTIVE_THRESH_MEAN_C,
                    cv2.THRESH_BINARY,
                    self.options['mark block size'] * 2 + 1,
                    self.options['mark c'],
                ),
                'green',
                (0, 180, 0),
            ),
            MarkInfo(
                cv2.adaptiveThreshold(
                    self.lab_a,
                    255,
                    cv2.ADAPTIVE_THRESH_MEAN_C,
                    cv2.THRESH_BINARY_INV,
                    self.options['mark block size'] * 2 + 1,
                    self.options['mark c'],
                ),
                'red',
                (0, 0, 180),
            ),
        ]

        table_mask = np.zeros(mat.shape[:2], np.uint8)
        cv2.fillConvexPoly(table_mask, table_contour, 255)
        table_area = cv2.contourArea(table_contour)
        contours_mat = mat.copy()

        def set_result(color, prop, val):
            setattr(self.results, '{}_mark_{}'.format(color, prop), val)

        for mark in threshed_marks:
            masked = cv2.bitwise_and(mark.threshed, mark.threshed, mask=table_mask)
            self.post('{} mark threshed'.format(mark.color_name), masked)

            _, contours, hierarchy = cv2.findContours(masked.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            if len(contours) > 0:
                ContourInfo = namedtuple('ContourInfo', ['contour', 'hull', 'hull_area'])
                hulled_contours = []
                for c in contours:
                    hull = cv2.convexHull(c)
                    area = cv2.contourArea(hull)
                    hulled_contours.append(ContourInfo(c, hull, area))

                sorted_hulls = sorted(hulled_contours, key=lambda x: -x.hull_area)
                max_hull = sorted_hulls[0]
                if max_hull.hull_area / table_area >= self.options['min mark table area ratio']:
                    self.draw_contours(contours_mat, max_hull.hull)

                    set_result(mark.color_name, 'visible', True)
                    set_result(mark.color_name, 'area', max_hull.hull_area)
                    set_result(mark.color_name, 'clipping', self.is_clipping(mat, max_hull.hull))
                    center = self.contour_center(mark.contour)
                    set_result(mark.color_name, 'x', center[0])
                    set_result(mark.color_name, 'y', center[1])

                else:
                    set_result(mark.color_name, 'visible', False)

    def tower(self, mat, mask):
        threshed_colors = []

        morph_kernel = cv2.getStructuringElement(
            cv2.MORPH_ELLIPSE,
            (self.options['stack morph size'],) * 2
        )
        for color in ['red', 'green']:
            # channel = self.lab_a if color == 'red' else self.luv_v
            threshed = cv2.adaptiveThreshold(
                self.lab_a,
                255,
                cv2.ADAPTIVE_THRESH_MEAN_C,
                cv2.THRESH_BINARY if color == 'red' else cv2.THRESH_BINARY_INV,
                self.options['stack block size'] * 2 + 1,
                self.options['{} c'.format(color)],
            )
            morphed = cv2.morphologyEx(threshed, cv2.MORPH_OPEN, morph_kernel)
            threshed_colors.append((color, morphed))
            self.post('{} stacks morphed'.format(color), morphed)

        StackInfo = namedtuple('StackInfo', ['color', 'contour', 'min_rect', 'area'])
        stack_infos = []
        blocker_infos = []

        for color, threshed in threshed_colors:
            masked = cv2.bitwise_and(threshed, threshed, mask=mask)

            # blurred = cv2.medianBlur(masked, self.options['stack blur size'] * 2 + 1)
            # self.post('{} stacks blurred'.format(color), blurred)

            candidate_stack_infos = []
            candidate_blocker_infos = []
            _, contours, hierarchy = cv2.findContours(masked.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            for c in contours:
                area = cv2.contourArea(c)
                area_ok = area >= self.options['min stack area']

                # Don't detect poles of the tower
                min_rect = cv2.minAreaRect(c)
                width_ok = min(min_rect[1]) >= self.options['min stack width']

                # Don't detect contours that clip with the edge of the camera
                clipping = self.is_clipping(mat, c)

                fill_mask = np.zeros(mat.shape[:2], dtype=np.uint8)
                cv2.drawContours(fill_mask, [c], -1, 255, thickness=-1)
                fill_masked = cv2.bitwise_and(masked, masked, mask=fill_mask)
                hull_area = cv2.contourArea(cv2.convexHull(c))
                fill = np.sum(fill_masked) / 255 / hull_area
                fill_ok = fill > self.options['min stack fill ratio']

                rect_x, rect_y, rect_width, rect_height = cv2.boundingRect(c)
                blocking = rect_x < self.options['max clipping distance'] and \
                    rect_width >= mat.shape[1] * self.options['min stack blocking width ratio']

                if area_ok and width_ok and fill_ok:
                    stack = StackInfo(color, c, min_rect, area)
                    if blocking:
                        candidate_blocker_infos.append(stack)
                    elif not clipping:
                        candidate_stack_infos.append(stack)

            setattr(self.results, '{}_stack_blocking'.format(color), len(candidate_blocker_infos) > 0)
            blocker_infos.extend(candidate_blocker_infos)

            if len(candidate_stack_infos) > 0:
                sorted_stacks = sorted(candidate_stack_infos, key=lambda x: -x.area)
                stack_infos.extend(sorted_stacks[:2])

        contours_mat = mat.copy()
        for stack in stack_infos:
            self.draw_contours(contours_mat, stack.contour)
        self.post('stack contours', contours_mat)

        blockers_mat = mat.copy()
        for blocker in blocker_infos:
            self.draw_contours(blockers_mat, blocker.contour)
        self.post('blocker contours', blockers_mat)

        def set_result(i, prop, val):
            setattr(self.results, 'stack_{}_{}'.format(i + 1, prop), val)

        # Let mission handle deciding orientation/relative placement of stacks
        for i, stack in enumerate(stack_infos):
            set_result(i, 'visible', True)
            set_result(i, 'red', stack.color == 'red')
            center = self.contour_center(stack.contour)
            set_result(i, 'x', center[0])
            set_result(i, 'y', center[1])
            set_result(i, 'area', stack.area)

            length, width = stack.min_rect[1]
            aspect_ratio = None
            angle = None
            if length < width:
                aspect_ratio = width / length
                angle = stack.min_rect[2] + 180
            else:
                aspect_ratio = length / width
                angle = stack.min_rect[2] + 90
            set_result(i, 'aspect_ratio', aspect_ratio)
            set_result(i, 'angle', angle)

        for i in range(len(stack_infos), 4):
            set_result(i, 'visible', False)

if __name__ == '__main__':
    Recovery(options=opts)()
