#!/usr/bin/env python3

import time
import math
import os
import cv2
from collections import namedtuple
from itertools import combinations
import numpy as np

import shm
from vision.modules.base import ModuleBase
from vision.options import IntOption, DoubleOption, BoolOption
from vision import vision_common

options = [
    BoolOption('debug', False),
    IntOption('max_fps', 30, 0, 30),

    DoubleOption('scale_factor', 4, 1, 10),
    DoubleOption('template_scale_factor', 5, 1, 50),
    IntOption('flann_checks', 50, 1, 200),
    IntOption('min_matches', 10, 4, 100),
]

def resize_shrink(mat, factor):
    return cv2.resize(
        mat,
        (0, 0),
        fx=factor,
        fy=factor,
        interpolation=cv2.INTER_AREA,
    )

Template = namedtuple('Template', [
    'mat',
    'points_map',
])

Match = namedtuple('Match', [
    'points_map',
    'matches_mat',
])

class FeatureMatcher:
    FeaturesResult = namedtuple('FeaturesResult', [
        'kp', # Keypoints
        'des', # Descriptors
        'kp_mat', # Keypoints image
    ])

    TemplateRender = namedtuple('TemplateRender', [
        'template',
        'features_result',
    ])

    FLANN_NEAREST_NEIGHBORS = 2

    def __init__(self, templates, flann_checks, min_matches, gray_func=None, sanity_check=True):
        self.sift = cv2.xfeatures2d.SIFT_create()

        FLANN_INDEX_KDTREE = 1
        index_params = dict(algorithm=FLANN_INDEX_KDTREE, trees=5)
        search_params = dict(checks=flann_checks)
        self.flann = cv2.FlannBasedMatcher(index_params, search_params)
        self.min_matches = min_matches

        if gray_func is None:
            gray_func = lambda mat: cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2YCR_CB))[0]
        self.gray_func = gray_func

        self.template_renders = []
        for t in templates:
            tr = self.TemplateRender(t, self.features(t.mat, True))
            if len(tr.features_result.kp) < self.FLANN_NEAREST_NEIGHBORS:
                raise ValueError('Template images must contain > {} features')

            self.template_renders.append(tr)

        self.sanity_check = sanity_check

    def features(self, mat, draw_keypoints):
        gray = self.gray_func(mat)
        kp, des = self.sift.detectAndCompute(gray, None)

        kp_mat = None
        if draw_keypoints:
            kp_mat = cv2.drawKeypoints(gray, kp, mat, flags=cv2.DRAW_MATCHES_FLAGS_DRAW_RICH_KEYPOINTS)

        return self.FeaturesResult(kp, des, kp_mat)

    def match(self, mat, debug=False):
        matches = [None for _ in self.template_renders]

        live_features = self.features(mat, debug)
        if len(live_features.kp) < self.FLANN_NEAREST_NEIGHBORS:
            return matches

        for i, tem_rend in enumerate(self.template_renders):
            flann_matches = self.flann.knnMatch(
                tem_rend.features_result.des,
                live_features.des,
                k=self.FLANN_NEAREST_NEIGHBORS,
            )

            # Ratio test to find good matches
            matches_mask = [[0, 0] for _ in flann_matches]
            good_matches = []
            for (m, n), match_mask in zip(flann_matches, matches_mask):
                if m.distance < 0.7 * n.distance:
                    match_mask[0] = 1
                    good_matches.append(m)

            if len(good_matches) < self.min_matches:
                continue

            matches_mat = None
            if debug:
                matches_mat = cv2.drawMatchesKnn(
                    tem_rend.features_result.kp_mat,
                    tem_rend.features_result.kp,
                    live_features.kp_mat,
                    live_features.kp,
                    flann_matches,
                    None,

                    matchColor=(0, 255, 0),
                    singlePointColor=(255, 0, 0),
                    matchesMask=matches_mask,
                    flags=0,
                )

            # For some reason, OpenCV likes wrapped points like [[x, y]]
            src_pts = np.float32([tem_rend.features_result.kp[m.queryIdx].pt for m in good_matches]).reshape(-1, 1, 2)
            dst_points = np.float32([live_features.kp[m.trainIdx].pt for m in good_matches]).reshape(-1, 1, 2)
            transform, mask = cv2.findHomography(src_pts, dst_points, cv2.RANSAC, 5.0)
            if transform is None:
                continue

            template_ptmap_list = list(sorted(tem_rend.template.points_map.items()))
            template_pt_names, template_pts = tuple(zip(*template_ptmap_list))
            template_pts = np.float32(template_pts).reshape(-1, 1, 2)
            transformed = cv2.perspectiveTransform(template_pts, transform)
            result_ptmap = {name: pt[0] for name, pt in zip(template_pt_names, transformed)}

            if not self.sanity_check or not self._sanity_check_points(tem_rend.template.points_map, result_ptmap):
                continue

            matches[i] = Match(result_ptmap, matches_mat)

        return matches

    def _sanity_check_points(self, template_pts, live_pts):
        """
        Check that the transformed points are in a reasonable place relative
        to those in the template.
        """

        for pt1, pt2 in combinations(template_pts.keys(), 2):
            template_vec = np.array(template_pts[pt2]) - np.array(template_pts[pt1])
            live_vec = np.array(live_pts[pt2] - live_pts[pt1])
            if template_vec.dot(live_vec) < 0:
                return False

        return True

TENT_CUTOUTS = {
    'large_cutout': (1040, 3528),
    'small_cutout': (988, 732),
}
SQUID_CUTOUTS = {
    'large_cutout': (1252, 1172),
    'small_cutout': (724, 3864),
}

class Torpedoes(ModuleBase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        software_path = os.environ['CUAUV_SOFTWARE']
        template_scale_down = 1 / self.options['template_scale_factor']

        def read_torp_image(name):
            path = '{}vision/data/Torpedoes/{}-transdec.png'.format(
                software_path,
                name,
            )
            mat = cv2.imread(path)
            return resize_shrink(mat, template_scale_down)

        tent_mat = read_torp_image('tentacle')
        squid_mat = read_torp_image('squid')

        def extract_corners(mat):
            """ In OpenCV coordinates """

            x_max = mat.shape[1] - 1
            y_max = mat.shape[0] - 1

            return {
                'top_right_corner': (x_max, 0),
                'top_left_corner': (0, 0),
                'bottom_left_corner': (0, y_max),
                'bottom_right_corner': (x_max, y_max),
            }

        tent_points = extract_corners(tent_mat)
        tent_points.update(self.scale_points(TENT_CUTOUTS, template_scale_down))
        squid_points = extract_corners(squid_mat)
        squid_points.update(self.scale_points(SQUID_CUTOUTS, template_scale_down))

        templates = [
            Template(tent_mat, tent_points),
            Template(squid_mat, squid_points),
        ]

        self.matcher = FeatureMatcher(
            templates,
            self.options['flann_checks'],
            self.options['min_matches'],
        )

    def scale_points(self, points, scale):
        return {name: (
            coord[0] * scale,
            coord[1] * scale,
        ) for name, coord in points.items()}

    def post(self, name, mat, *args, **kwargs):
        if self.options['debug']:
            orig_area = self.mat.shape[0] * self.mat.shape[1]
            height, width = tuple(mat.shape[:2])

            scale_factor = math.sqrt(orig_area) / math.sqrt(height * width)
            new_size = (int(scale_factor * width), int(scale_factor * height))
            resized = cv2.resize(mat, new_size)

            super().post(name, resized, *args, **kwargs)

    def draw_board(self, mat, points_map):
        drawn_mat = mat.copy()

        outline_pts = [
            points_map['top_right_corner'],
            points_map['top_left_corner'],
            points_map['bottom_left_corner'],
            points_map['bottom_right_corner'],
        ]
        path = np.int32(outline_pts).reshape(-1, 1, 2)
        cv2.polylines(drawn_mat, [path], True, vision_common.purple, 5)

        small, large = points_map['small_cutout'], points_map['large_cutout']
        cv2.circle(
            drawn_mat,
            (int(small[0]), int(small[1])),
            self.denormalized_size(0.04, round=True),
            (50, 200, 100),
            -1,
        )
        cv2.circle(
            drawn_mat,
            (int(large[0]), int(large[1])),
            self.denormalized_size(0.08, round=True),
            (200, 50, 100),
            -1,
        )

        return drawn_mat

    def process(self, mat):
        start_time = time.time()

        self.mat = mat

        shrunk = resize_shrink(mat, 1 / self.options['scale_factor'])

        for name, match in zip(['tentacle', 'squid'], self.matcher.match(shrunk, self.options['debug'])):
            results_g = shm._eval('torpedoes_{}'.format(name))
            results = results_g.get()
            results.visible = False

            drawn_mat = mat

            if match is not None:
                orig_scale_pts = self.scale_points(match.points_map, self.options['scale_factor'])
                if self.options['debug']:
                    drawn_mat = self.draw_board(mat, orig_scale_pts)
                    self.post('{} matches'.format(name), match.matches_mat)

                results.visible = True

                top_right = np.array(orig_scale_pts['top_right_corner'])
                top_left = np.array(orig_scale_pts['top_left_corner'])
                bottom_left = np.array(orig_scale_pts['bottom_left_corner'])
                bottom_right = np.array(orig_scale_pts['bottom_right_corner'])

                left_height = np.linalg.norm(top_left - bottom_left)
                right_height = np.linalg.norm(top_right - bottom_right)
                avg_height = (left_height + right_height) / 2
                results.height = self.normalized_size(avg_height)

                top_width = np.linalg.norm(top_left - top_right)
                bottom_width = np.linalg.norm(bottom_left - bottom_right)
                avg_width = (top_width + bottom_width) / 2
                results.width = self.normalized_size(avg_width)

                skew = (right_height - left_height) / avg_height
                results.skew = skew if results.height > 0.3 else 0

                center = (top_right + top_left + bottom_left + bottom_right) / 4
                results.x, results.y = self.normalized(center)

                small = orig_scale_pts['small_cutout']
                large = orig_scale_pts['large_cutout']
                results.small_cutout_x, results.small_cutout_y = self.normalized(small)
                results.large_cutout_x, results.large_cutout_y = self.normalized(large)

            self.post('{} board'.format(name), drawn_mat)

            results_g.set(results)

        shm.torpedoes_vision.clock.set(not shm.torpedoes_vision.clock.get())

        runtime = time.time() - start_time
        min_runtime = 1 / self.options['max_fps']
        if min_runtime > runtime:
            time.sleep(min_runtime - runtime)
            runtime = min_runtime
        print('FPS: {}'.format(1 / (runtime)))

if __name__ == '__main__':
    Torpedoes('forward', options)()
