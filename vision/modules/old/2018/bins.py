#!/usr/bin/env python3

import time
import cv2
import numpy as np

import shm
from vision import options
from vision.vision_common import (
    draw_angled_arrow,
    get_angle_from_rotated_rect,
    Hierarchy,
    is_clipping,
)
from vision.modules.base import ModuleBase

from mission.constants.config import bins as constants

CONTOUR_HEURISTIC_LIMIT = 5
CONTOUR_SCALED_HEURISTIC_LIMIT = 2

options = [
    options.BoolOption('clipping_guard', constants.clipping_guard),
    options.BoolOption('debug', False),
    options.IntOption('max_fps', 30, 0, 30),
    options.IntOption('border_bgr_compon', constants.border_bgr_compon, 0, 2),
    options.IntOption('block_size', 401, 0, 4000, lambda x: x % 2 == 1),
    # options.IntOption('border_thresh', constants.border_thresh, 0, 255),
    options.IntOption('morph_size', 25, 1, 30, lambda x: x % 2 == 1),
    options.DoubleOption('min_size', 0.1, 0, 2), # Min length of min length side
    options.DoubleOption('min_rectangularity', 0.7, 0, 1),
    options.DoubleOption('min_inner_outer_ratio', 0.3, 0, 1),
    options.DoubleOption('min_cover_diff', 25, 0, 255),
    options.IntOption('blur', 27, 0, 100, lambda x: x % 2 == 1),
]

class Bins(ModuleBase):
    def post(self, *args, **kwargs):
        if self.options['debug']:
            super().post(*args, **kwargs)

    def draw_contours(self, mat, *contours):
        cv2.drawContours(mat, contours, -1, (0, 127, 255), thickness=3)

    def process(self, mat):
        start_time = time.time()

        self.process_bins(mat)
        shm.bins_vision.clock.set(not shm.bins_vision.clock.get())

        runtime = time.time() - start_time
        min_runtime = 1 / self.options['max_fps']
        if min_runtime > runtime:
            time.sleep(min_runtime - runtime)
            runtime = min_runtime
        print('FPS: {}'.format(1 / (runtime)))

    def process_bins(self, mat):
        results = [shm.bins_bin0.get(), shm.bins_bin1.get()]
        for result in results:
            result.visible = False

        self.post('orig', mat)

        self.bgr_sp = cv2.split(mat)

        _, threshed = cv2.threshold(
            self.bgr_sp[self.options['border_bgr_compon']],
            0,
            255,
            cv2.THRESH_BINARY | cv2.THRESH_OTSU,
        )
        self.post('threshed', threshed)

        morph_kernel = cv2.getStructuringElement(
            cv2.MORPH_ELLIPSE,
            (self.options['morph_size'],) * 2,
        )
        # Get rid of small things
        morphed = cv2.erode(threshed, morph_kernel)
        self.post('morphed', morphed)

        _, contours, hierarchy = cv2.findContours(
            morphed.copy(),
            cv2.RETR_TREE,
            cv2.CHAIN_APPROX_SIMPLE,
        )

        if hierarchy is None:
            hierarchy = [[]]
        hier = Hierarchy(hierarchy)

        all_contours = [{'i': i, 'contour': contours[i]} for i in range(len(contours))]

        big_rects = []
        for info in all_contours:
            info['rect'] = cv2.minAreaRect(info['contour'])

            if hier.first_child(info['i']) == -1:
                continue

            if min(info['rect'][1]) / len(mat[1]) < self.options['min_size']:
                continue

            rectangularity = cv2.contourArea(info['contour']) / np.prod(info['rect'][1])
            if rectangularity < self.options['min_rectangularity']:
                continue

            if is_clipping(mat, info['contour']) and self.options['clipping_guard']:
                continue

            big_rects.append(info)

        concentric_rects = []
        for info in big_rects:
            max_child_i = max(
                hier.siblings(hier.first_child(info['i'])),
                key=lambda x: cv2.contourArea(contours[x])
            )

            info['inner_contour'] = contours[max_child_i]
            info['inner_rect'] = cv2.minAreaRect(info['inner_contour'])

            info['inner_width'], info['inner_length'] = sorted(info['inner_rect'][1])
            if info['inner_width'] < self.options['min_size'] * self.options['min_inner_outer_ratio']:
                continue

            inner_area = cv2.contourArea(info['inner_contour'])
            rectangularity = inner_area / (info['inner_rect'][1][0] * info['inner_rect'][1][1])
            if rectangularity < self.options['min_rectangularity']:
                continue

            inner_mask = np.zeros(mat.shape[:2], dtype=np.uint8)
            cv2.drawContours(inner_mask, [info['inner_contour']], -1, 255, -1)
            info['average_cover'] = cv2.mean(self.bgr_sp[2], inner_mask)[0]
            info['angle'] = get_angle_from_rotated_rect(info['rect'])

            concentric_rects.append(info)

        concentric_rects.sort(key=lambda x: -x['rect'][1][0] * x['rect'][1][1])

        for info in concentric_rects:
            info['covered'] = False

        if len(concentric_rects) >= 2:
            r0, r1 = tuple(concentric_rects[:2])
            diff = r0['average_cover'] - r1['average_cover']
            if abs(diff) >= self.options['min_cover_diff']:
                r0_covered = diff > 0
                r0['covered'], r1['covered'] = r0_covered, not r0_covered

        for info, result in zip(concentric_rects, results):
            result.visible = True
            result.clipping = False
            result.x, result.y = self.normalized(info['inner_rect'][0])
            result.width, result.length = self.normalized_size(sorted(info['rect'][1]))
            result.angle = info['angle']
            result.covered = info['covered']

        shm.bins_bin0.set(results[0])
        shm.bins_bin1.set(results[1])

        if self.options['debug']:
            contours_mat = mat.copy()
            self.draw_contours(contours_mat, *[info['contour'] for info in concentric_rects])
            self.draw_contours(contours_mat, *[info['inner_contour'] for info in concentric_rects])
            for info in concentric_rects:
                draw_angled_arrow(contours_mat, info['rect'][0], info['angle'])
                if info['covered']:
                    cv2.drawContours(
                        contours_mat,
                        [info['inner_contour']],
                        -1,
                        (20, 255, 57),
                        thickness=10,
                    )
            self.post('contours', contours_mat)

            all_con = mat.copy()
            self.draw_contours(all_con, *[info['contour'] for info in big_rects])
            self.post('all outer', all_con)

if __name__ == '__main__':
    Bins('downward', options)()
