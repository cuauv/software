#!/usr/bin/env python3

import time
import math
import cv2
import numpy as np
import shm
from mission.constants.config import recovery as constants
from vision.modules.base import ModuleBase
from vision.options import IntOption, DoubleOption, BoolOption
from vision.vision_common import (
    is_clipping,
    get_angle_from_rotated_rect,
    get_angle_from_ellipse,
    contour_center,
)

opts = [
    BoolOption('debug', False),
    IntOption('max_fps', 30, 1, 120),

    IntOption('blur_size', 5, 1, 100, lambda x: x % 2 == 1),
    IntOption('c0', constants.c0, -127, 127),
    IntOption('c1', constants.c1, -255, 255),
    IntOption('c2', constants.c2, -255, 255),
    IntOption('block_size', 301, 1, 401, lambda x: x % 2 == 1),

    IntOption('e_c0', 8, -255, 255),
    IntOption('e_c1', 8, -255, 255),
    IntOption('tube_morph', 5, 1, 255),

    DoubleOption('min_tube_len', 0.2, 0, 2),
    DoubleOption('min_tube_width', 0.02, 0, 2),
    DoubleOption('min_tube_rectangularity', 0.6, 0, 1),
    DoubleOption('min_tube_aspect_ratio', 4, 1, 15),

    IntOption('table_thresh', constants.table_thresh, 0, 255),
    IntOption('table_erode', 11, 1, 41, lambda x: x % 2 == 1),
    DoubleOption('min_table_area_ratio', 0.03, 0, 1),

    IntOption('blue_ellipse_thresh', constants.blue_ellipse_thresh, 0, 255),
    DoubleOption('min_ellipse_width', 0.04, 0, 2),
    DoubleOption('min_ellipticality', 0.7, 0, 1),
    DoubleOption('min_ellipse_aspect_ratio', 1.5, 1, 5),
]

NUM_TUBES = 4
NUM_ELLIPSES = NUM_TUBES

class Recovery(ModuleBase):
    def post(self, *args, **kwargs):
        if self.options['debug']:
            super().post(*args, **kwargs)

    def draw_contours(self, mat, *contours):
        cv2.drawContours(mat, contours, -1, (0, 127, 255), thickness=3)

    def avg_color(self, contour, lab, ycrcb):
        """
        Get the average (lab_a, lab_b, ycrcb_cr, ycrcb_cb) color under the
        contour.
        """

        mask = np.zeros(lab.shape[:2], dtype=np.uint8)
        cv2.drawContours(mask, [contour], -1, 255, thickness=-1)
        avg_lab = cv2.mean(lab, mask=mask)
        avg_ycrcb = cv2.mean(ycrcb, mask=mask)
        return (
            int(avg_lab[1]),
            int(avg_lab[2]),
            int(avg_ycrcb[1]),
            int(avg_ycrcb[2]),
        )

    def process(self, mat):
        start_time = time.time()

        self.post('original', mat)

        blurred = cv2.medianBlur(mat, self.options['blur_size'])
        lab = cv2.cvtColor(blurred, cv2.COLOR_BGR2LAB)
        ycrcb = cv2.cvtColor(blurred, cv2.COLOR_BGR2YCR_CB)
        hsv = cv2.cvtColor(blurred, cv2.COLOR_BGR2HSV)
        lab_sp, ycrcb_sp = cv2.split(lab), cv2.split(ycrcb)
        hsv_sp = cv2.split(hsv)

        self.post('lab_a', lab_sp[1])
        self.post('ycrcb_cb', ycrcb_sp[2])
        self.post('hxv_s', hsv_sp[1])

        self.tubes(mat, lab, ycrcb, lab_sp, hsv_sp)
        self.table(mat, lab, ycrcb)
        shm.recovery_vision.clock.set(not shm.recovery_vision.clock.get())

        runtime = time.time() - start_time
        min_runtime = 1 / self.options['max_fps']
        if min_runtime > runtime:
            time.sleep(min_runtime - runtime)
            runtime = min_runtime

        print('FPS: {}'.format(1 / (runtime)))

    def tubes(self, mat, lab, ycrcb, lab_sp, hsv_sp):
        results = [shm._eval('recovery_tube{}'.format(i)).get() for i in range(NUM_TUBES)]
        for result in results:
            result.visible = False

        thresh0 = cv2.adaptiveThreshold(
            lab_sp[1],
            255,
            cv2.ADAPTIVE_THRESH_MEAN_C,
            cv2.THRESH_BINARY,
            self.options['block_size'],
            self.options['c0'],
        )
        thresh1 = cv2.adaptiveThreshold(
            lab_sp[1],
            255,
            cv2.ADAPTIVE_THRESH_MEAN_C,
            cv2.THRESH_BINARY_INV,
            self.options['block_size'],
            self.options['c1'],
        )
        thresh2 = cv2.adaptiveThreshold(
            lab_sp[2],
            255,
            cv2.ADAPTIVE_THRESH_MEAN_C,
            cv2.THRESH_BINARY_INV,
            self.options['block_size'],
            self.options['c2'],
        )

        self.post('tube thresh0', thresh0)
        self.post('tube thresh1', thresh1)
        self.post('tube thresh2', thresh2)

        # Find contours on single binary image to prevent redundant contours
        threshed = cv2.bitwise_or(thresh0, cv2.bitwise_or(thresh1, thresh2))
        morph_kernel = cv2.getStructuringElement(
            cv2.MORPH_ELLIPSE,
            (self.options['tube_morph'],) * 2
        )
        morphed = cv2.morphologyEx(threshed, cv2.MORPH_OPEN, morph_kernel)
        self.post('tubes morphed', morphed)
        _, contours, _ = cv2.findContours(
            morphed.copy(),
            cv2.RETR_EXTERNAL,
            cv2.CHAIN_APPROX_SIMPLE,
        )

        infos = []
        for contour in contours:
            info = {'contour': contour}

            if is_clipping(mat, contour):
                continue

            info['min_rect'] = cv2.minAreaRect(contour)
            info['width'], info['len'] = self.normalized_size(sorted(info['min_rect'][1]))
            if info['len'] < self.options['min_tube_len']:
                continue
            if info['width'] < self.options['min_tube_width']:
                continue

            aspect_ratio = info['len'] / info['width']
            if aspect_ratio < self.options['min_tube_aspect_ratio']:
                continue

            info['rect_area'] = np.prod(info['min_rect'][1])
            infos.append(info)

        if self.options['debug']:
            contours_mat = mat.copy()
            self.draw_contours(contours_mat, *[i['contour'] for i in infos])
            self.post('tube contours', contours_mat)

        for result, info in zip(results, sorted(infos, key=lambda x: -x['rect_area'])):
            result.visible = True
            result.x, result.y = self.normalized(info['min_rect'][0])
            result.angle = get_angle_from_rotated_rect(info['min_rect'])
            result.length = info['len']

            color = self.avg_color(info['contour'], lab, ycrcb)
            result.lab_a, result.lab_b, result.ycrcb_cr, result.ycrcb_cb = color

        for i, result in enumerate(results):
            shm._eval('recovery_tube{}'.format(i)).set(result)

    def table(self, mat, lab, ycrcb):
        bgr_sp = cv2.split(mat)
        # _, threshed = cv2.threshold(bgr_sp[0], self.options['table_thresh'], 255, cv2.THRESH_BINARY)
        _, threshed = cv2.threshold(bgr_sp[0], 0, 255, cv2.THRESH_BINARY | cv2.THRESH_OTSU)

        morph_kernel = cv2.getStructuringElement(
            cv2.MORPH_ELLIPSE,
            (self.options['table_erode'],) * 2
        )
        eroded = cv2.erode(threshed, morph_kernel)
        self.post('table eroded', eroded)

        _, contours, _ = cv2.findContours(
            eroded.copy(),
            cv2.RETR_EXTERNAL,
            cv2.CHAIN_APPROX_SIMPLE,
        )

        # Find table and ellipses inside
        infos = []
        for contour in contours:
            info = {'contour': contour}

            info['area'] = cv2.contourArea(contour)
            area_ratio = info['area'] / np.prod(mat.shape[:2])
            if area_ratio < self.options['min_table_area_ratio']:
                continue
            info['clipping'] = is_clipping(mat, contour)

            infos.append(info)

        if self.options['debug']:
            contours_mat = mat.copy()
            self.draw_contours(contours_mat, *[info['contour'] for info in infos])
            self.post('table contours', contours_mat)

        if len(infos) > 0:
            table_contour = max(infos, key=lambda x: x['area'])['contour']
        else:
            table_contour = None

        # Pass threshed instead of eroded since it will appear dilated when
        # inverted
        self.ellipses(mat, lab, ycrcb, table_contour, threshed, bgr_sp, morph_kernel)

    def ellipses(self, mat, lab, ycrcb, table_contour, table_thresh, bgr_sp, morph_kernel):
        results = [shm._eval('recovery_ellipse{}'.format(i)).get() for i in range(NUM_ELLIPSES)]
        for result in results:
            result.visible = False

        infos = []

        if table_contour is not None:
            table_mask = np.zeros(mat.shape[:2], dtype=np.uint8)
            cv2.drawContours(table_mask, [table_contour], -1, 255, thickness=-1)

            table_masked = cv2.bitwise_and(bgr_sp[0], table_mask)
            # levelled_blue = cv2.bitwise_or(outer_mat, table_masked)
            self.post('levelled image', table_masked)

            thresh0 = cv2.adaptiveThreshold(
                bgr_sp[1],
                255,
                cv2.ADAPTIVE_THRESH_MEAN_C,
                cv2.THRESH_BINARY_INV,
                self.options['block_size'],
                self.options['e_c0'],
            )
            thresh1 = cv2.adaptiveThreshold(
                bgr_sp[0],
                255,
                cv2.ADAPTIVE_THRESH_MEAN_C,
                cv2.THRESH_BINARY_INV,
                self.options['block_size'],
                self.options['e_c1'],
            )
            self.post('e thresh0', thresh0)
            self.post('e thresh1', thresh1)

            threshed = cv2.bitwise_or(
                thresh0,
                thresh1,
                mask=table_mask if constants.detect_table else None,
            )

            # morphed = cv2.morphologyEx(threshed, cv2.MORPH_CLOSE, morph_kernel)
            morphed = cv2.erode(threshed, morph_kernel)
            self.post('total ellipses morphed', morphed)

            _, contours, _ = cv2.findContours(
                morphed.copy(),
                cv2.RETR_EXTERNAL if constants.detect_table else cv2.RETR_LIST,
                cv2.CHAIN_APPROX_SIMPLE,
            )

            for contour in contours:
                info = {'contour': contour}

                if is_clipping(mat, contour):
                    continue
                if len(contour) < 5:
                    continue

                if len(info['contour']) < 5:
                    continue
                info['ellipse'] = cv2.fitEllipse(contour)
                info['width'], info['len'] = self.normalized_size(sorted(info['ellipse'][1]))
                if info['width'] < self.options['min_ellipse_width']:
                    continue

                ellipse_area = math.pi * np.prod(info['ellipse'][1]) / 4
                info['area'] = cv2.contourArea(contour)
                ellipticality = ellipse_area / info['area']
                if ellipticality > 1:
                    ellipticality = 1 / ellipticality
                if ellipticality < self.options['min_ellipticality']:
                    continue

                minor, major = sorted(info['ellipse'][1])
                if major / minor < self.options['min_ellipse_aspect_ratio']:
                    continue

                infos.append(info)

        if self.options['debug']:
            ellipses_mat = mat.copy()
            for info in infos:
                cv2.ellipse(ellipses_mat, info['ellipse'], (0, 127, 255), 4)
            self.post('ellipses', ellipses_mat)

        for result, info in zip(results, sorted(infos, key=lambda x: -x['area'])):
            result.visible = True
            result.x, result.y = self.normalized(info['ellipse'][0])
            result.angle = get_angle_from_ellipse(info['ellipse'])
            result.length = info['len']

            color = self.avg_color(info['contour'], lab, ycrcb)
            result.lab_a, result.lab_b, result.ycrcb_cr, result.ycrcb_cb = color

        for i, result in enumerate(results):
            shm._eval('recovery_ellipse{}'.format(i)).set(result)

if __name__ == '__main__':
    Recovery('downward', options=opts)()
