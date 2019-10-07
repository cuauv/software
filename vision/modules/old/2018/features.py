#!/usr/bin/env python3
from vision.modules.base import ModuleBase
from vision.modules import features_utils
from vision import options
import sys

import cv2
import numpy as np

module_options = [
        options.IntOption('source_hess_thresh', 645, 10, 1000),
        options.IntOption('video_hess_thresh', 645, 10, 1000),
        options.DoubleOption('ratio_thresh', 0.7, 0.1, 1),
]

Detector = cv2.xfeatures2d.SURF_create
Detector = cv2.xfeatures2d.SIFT_create

class Features(ModuleBase):
    def __init__(self, source_image_name, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self._feature_finder = features_utils.FeatureFinder(Detector(), ('torpedo', source_image_name))

    def process(self, img):
        res = self._feature_finder.match_against(img)
        if 'torpedo' not in res:
            print('none found :(')
            return
        result = res['torpedo']

        print('Score: {}'.format(result.score()))

        h, w, _ = result.template_image.shape
        src_corners = np.float32([[0,0],[0,h-1],[w-1,h-1],[w-1,0]]).reshape(-1,1,2)
        dst_corners = cv2.perspectiveTransform(src_corners, result.homography)
        img = cv2.polylines(img,[np.int32(dst_corners)], True, 255, 3, cv2.LINE_AA)
        res = cv2.drawMatches(img, result.image_kp, result.template_image, result.template_kp, result.matches, None)
        self.post('res', res)

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Must provide an argument for the source image')
        sys.exit(1)

    im = sys.argv[1]
    sys.argv[1:] = sys.argv[2:]

    Features(im, ['forward'], module_options)()
