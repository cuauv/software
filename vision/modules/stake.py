#!/usr/bin/env python3
import shm
import cv2
from functools import reduce
from math import pi
from pickle import load

# import time

import numpy as np
from vision.modules.base import ModuleBase  # , UndefinedModuleOption
from vision.framework.transform import resize, simple_gaussian_blur
from vision.framework.feature import outer_contours, contour_centroid, contour_area, simple_canny
from vision.framework.helpers import to_umat  # , from_umat
from vision.framework.draw import draw_circle
from vision.framework.color import bgr_to_lab, range_threshold

from vision.modules.gate import thresh_color_distance


from mission.missions.stake import MOVE_DIRECTION

from vision import options

opts =    [options.DoubleOption('rectangular_thresh', 0.8, 0, 1),
           options.DoubleOption('source_x_scale_board', 0.1, 0, 1),
           options.DoubleOption('source_y_scale_board', 0.1, 0, 1),
           options.DoubleOption('downsize_camera', 0.5, 0, 1),
           options.IntOption('min_match_count', 3, 0, 255),
           options.DoubleOption('good_ratio', 0.8, 0, 1),
           options.BoolOption('show_keypoints', False),
           options.IntOption('board_separation', 450, 0, 4000),
           options.IntOption('board_horizontal_offset', 70, -1000, 1000),
           options.IntOption('lever_position_x', -500 if MOVE_DIRECTION==1 else 2700, -3000, 3000),
           options.IntOption('lever_position_y', 2500, 0, 6000),
           options.IntOption('heart_offset_x', -307, -3000, 3000),
           options.IntOption('heart_offset_y', 0, -3000, 3000),
           options.IntOption('belt_offset_x', 0, -3000, 3000),
           options.IntOption('belt_offset_y', 0, -3000, 3000),
           options.IntOption('left_circle_offset_x', -284, -3000, 3000),
           options.IntOption('left_circle_offset_y', 565, -3000, 3000),
           options.IntOption('right_circle_offset_x', -519, -3000, 3000),
           options.IntOption('right_circle_offset_y', 565, -3000, 3000),
           options.IntOption('lever_l', 129, 0, 255),
           options.IntOption('lever_a', 201, 0, 255),
           options.IntOption('lever_b', 183, 0, 255),
           options.IntOption('lever_color_distance', 50, 0, 255),
           options.IntOption('contour_size_min', 5, 0, 1000),
           options.IntOption('lever_endzone_left', 1793 if MOVE_DIRECTION==1 else 750, 0, 6000),
           options.IntOption('lever_gutter_top', 2238, 0, 6000),
           options.IntOption('lever_gutter_bot', 2887, 0, 6000),
           options.IntOption('color_l', 170, 0, 255),
           options.IntOption('color_a', 129, 0, 255),
           options.IntOption('color_b', 155, 0, 255),
           options.IntOption('color_distance', 30, 0, 255),
           options.IntOption('close_offset_x', -70, -255, 255),
           options.IntOption('close_offset_y', -10, -255, 255),
           options.DoubleOption('min_eccentricity', 0.6, 0, 1),
           options.DoubleOption('max_eccentricity', 0.8, 0, 1),
           options.DoubleOption('min_elllipsish_thing', 0.95, 0, 1),
           options.IntOption('water_a', 10, 0, 100),
           options.IntOption('water_b', 10, 0, 100),
           options.IntOption('water_distance', 10, 0, 255),
           options.DoubleOption('sigma_canny', 0.33, 0, 1),
           ]


PADDING = 50
GUTTER_PAD = 500
BLUR_KERNEL = 3
BLUR_SD = 1

HEART = (1356, 3250)
BELT = (1174, 3700)
LEFT_CIRCLE = (390, 562)
RIGHT_CIRCLE = (1900, 570)

# MOVE_DIRECTION=1  # 1 if lever on left else -1 if on right

heart_original = load(open('/home/software/cuauv/software/vision/modules/heart', 'rb'))
# from vision.modules.heart import heart as heart_original

class Stake(ModuleBase):

    def heart(self):
        return (HEART[0] + self.options['heart_offset_x'], HEART[1] + self.options['heart_offset_y'])
    def belt(self):
        return (BELT[0] + self.options['belt_offset_x'], BELT[1] + self.options['belt_offset_y'])
    def left_circle(self):
        return (LEFT_CIRCLE[0] + self.options['left_circle_offset_x'], LEFT_CIRCLE[1] + self.options['left_circle_offset_y'])
    def right_circle(self):
        return (RIGHT_CIRCLE[0] + self.options['right_circle_offset_x'], RIGHT_CIRCLE[1] + self.options['right_circle_offset_y'])

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.detector = cv2.xfeatures2d.SIFT_create()
        FLANN_INDEX_KDTREE = 0
        index_params = dict(algorithm=FLANN_INDEX_KDTREE, trees=5)  # For SIFT
        search_params = dict(checks=50)

        self.flann = cv2.FlannBasedMatcher(index_params, search_params)
        self.past_var = {"lever_origin_upper": np.zeros((5, 2)), "lever_origin_lower": np.zeros((5, 2))}
        self.static = {}


    def static_process(self, image1, image2, name="board"):

        def find_key_descriptors(im):
            if self.options['source_x_scale_%s' % image] != 0 and self.options['source_y_scale_%s' % image] != 0:
                scaledim = resize(im, int(im.shape[1]*self.options['source_x_scale_%s' % image]),
                                      int(im.shape[0]*self.options['source_y_scale_%s' % image]))
                scaledim = np.pad(scaledim, ((PADDING, PADDING), (PADDING, PADDING)), 'constant', constant_values=255)
                rx = self.options['source_x_scale_%s' % image]
                ry = self.options['source_y_scale_%s' % image]
            else:
                scaledim = im
                rx = 1
                ry = 1
            scaledim = simple_gaussian_blur(scaledim, BLUR_KERNEL, BLUR_SD)
            kp, des = self.detector.detectAndCompute(scaledim, None)
            self.static[image] = {"name": image, "org": im,
                                  "img":  scaledim, "rx":  rx, "ry":  ry, "kp": kp, "des": des,
                                  "separation": self.options['board_separation']}
            keypoints = cv2.drawKeypoints(scaledim.copy(), kp, None, (0, 0, 255), flags=0)
            self.post(image, keypoints)
            return self.static[image]

        def stitch(im1, im2):
            im = np.full((im1.shape[0] + im2.shape[0] + self.options['board_separation'], im1.shape[1]), 255, dtype=np.uint8)
            im[:im1.shape[0], :im1.shape[1]] = im1
            im[-im2.shape[0]:, -im2.shape[1]:] = im2
            return im

        image = name

        if image in self.static:
            if self.static[image]['rx'] == self.options['source_x_scale_%s' % image] and \
                    self.static[image]['ry'] == self.options['source_y_scale_%s' % image] and \
                    self.static[image]['separation'] == self.options['board_separation']:
                return self.static[image]
            else:
                if self.static[image]['separation'] != self.options['board_separation']:
                    im = stitch(self.static[image1]["org"], self.static[image2]["org"])
                else:
                    im = self.static[image]["org"]
                return find_key_descriptors(im)
        else:
            im1 = cv2.imread('/home/software/cuauv/software/vision/modules/stake_images/%s.png' % image1, 0)
            im2 = cv2.imread('/home/software/cuauv/software/vision/modules/stake_images/%s.png' % image2, 0)
            self.static[image1] = {"org": im1}
            self.static[image2] = {"org": im2}
            im = stitch(im1, im2)

            assert im is not None
            return find_key_descriptors(im)


    def match(self, im1, im2, output, color):
        MIN_MATCH_COUNT = self.options['min_match_count']
        RECTANGULARITY_THRESH = self.options['rectangular_thresh']
        kp1 = im1["kp"]
        kp2 = im2["kp"]
        des1 = im1["des"]
        des2 = im2["des"]
        img1 = im1["img"]
        img2 = im2["img"]

        self.dst = None
        try:
            matches = self.flann.knnMatch(des1, des2, k=2)
        except cv2.error as e:
            matches = []
            print(e)

        if output is None: output = img2

        # store all the good matches as per Lowe's ratio test.
        good = []
        for m, n in (x for x in matches if len(x) == 2):
            if m.distance < self.options['good_ratio']*n.distance:
                good.append(m)

        if len(good) > MIN_MATCH_COUNT:
            src_pts = np.float32([kp1[m.queryIdx].pt for m in good]).reshape(-1, 1, 2)
            dst_pts = np.float32([kp2[m.trainIdx].pt for m in good]).reshape(-1, 1, 2)

            M, mask = cv2.findHomography(src_pts, dst_pts, cv2.RANSAC, 5.0)
            # matchesMask = mask.ravel().tolist()
            # print("m" + str(M))
            # print("ma" + str(mask))
            # print("mm" + str(matchesMask))

            h, w = img1.shape
            pts = np.float32([[PADDING, PADDING], [PADDING, h-1-PADDING], [w-1-PADDING, h-PADDING-1], [w-PADDING-1, PADDING]]).reshape(-1, 1, 2)

            try:
                dst = cv2.perspectiveTransform(pts, M)
                area = cv2.contourArea(dst)
                rarea = cv2.minAreaRect(dst)
                rarea = rarea[1][0]*rarea[1][1]
                if area/rarea < RECTANGULARITY_THRESH:
                    # print("box lower than rectangularity threshold")
                    return output, None

                def e_length(pt1, pt2):
                    return ((pt1[0] - pt2[0])**2 + (pt1[1]-pt2[1])**2)**(0.5)

                def norm_length_diff(dst, line1, line2):
                    l1 = e_length(dst[line1[0]][0], dst[line1[1]][0])
                    l2 = e_length(dst[line2[0]][0], dst[line2[1]][0])
                    return (l2-l1)

                Moments = cv2.moments(dst)

                getattr(shm.torpedoes_stake, "%s_align_h" % im1["name"]).set(norm_length_diff(dst, (0,1), (2,3)))
                # getattr(shm.torpedoes_stake, " % s_align_v" % im1["name"]).set(norm_length_diff(dst, (0,2), (1,3)))
                getattr(shm.torpedoes_stake, "%s_size" % im1["name"]).set(area)
                getattr(shm.torpedoes_stake, "%s_center_x" % im1["name"]).set(Moments["m10"]/Moments['m00'])
                getattr(shm.torpedoes_stake, "%s_center_y" % im1["name"]).set(Moments["m01"]/Moments['m00'])

                self.dst = dst
                output = cv2.polylines(output, [np.int32(dst)], True, color, 3, cv2.LINE_AA)

                return output, M
            except ZeroDivisionError:
                print('what')
            except cv2.error as e:
                print(e)

        # else:
            # print ("Not enough matches are found for %s - %d/%d" % (im1["name"], len(good), MIN_MATCH_COUNT))
        return output, None


    def locate_source_point(self, image, mask, point, output=None, color=(0, 0, 255)):
        i = self.static[image]
        pt = np.float32([[[int(point[0]*i['rx'] + PADDING), int(point[1]*i['ry'] + PADDING)]]])
        # print(mask)
        pt = cv2.perspectiveTransform(pt, mask)
        draw_circle(output, tuple(pt[0][0]), 1, color, thickness=3)
        return pt


    def post_shm(self, mat, p, M):
        shm.torpedoes_stake.camera_x.set(p.shape[1]//2)
        shm.torpedoes_stake.camera_y.set(p.shape[0]//2)

        colorspace = "lab"
        _, split = bgr_to_lab(mat)

        if M is not None:
            try:
                left_hole = self.locate_source_point('board', M, self.left_circle(), p)
                right_hole = self.locate_source_point('board', M, self.right_circle(), p)
                shm.torpedoes_stake.left_hole_x.set(left_hole[0][0][0])
                shm.torpedoes_stake.left_hole_y.set(left_hole[0][0][1])
                shm.torpedoes_stake.right_hole_x.set(right_hole[0][0][0])
                shm.torpedoes_stake.right_hole_y.set(right_hole[0][0][1])
                shm.torpedoes_stake.board_visible.set(True)

                lever_origin = (self.options['lever_position_x'], self.options['lever_position_y'])
                lever_origin_board = self.locate_source_point('board', M, lever_origin, p, color=(255, 0, 255))
                shm.torpedoes_stake.lever_origin_x.set(lever_origin_board[0][0][0])
                shm.torpedoes_stake.lever_origin_y.set(lever_origin_board[0][0][1])

                heart = self.locate_source_point('board', M, self.heart(), p)
                shm.torpedoes_stake.heart_x.set(heart[0][0][0])
                shm.torpedoes_stake.heart_y.set(heart[0][0][1])
                belt = self.locate_source_point('board', M, self.belt(), p)
                shm.torpedoes_stake.belt_x.set(belt[0][0][0])
                shm.torpedoes_stake.belt_y.set(belt[0][0][1])
                shm.torpedoes_stake.lever_finished.set(self.lever_finished(mat, split, 'board', M, p, colorspace))
            except cv2.error as e:
                print(e)
        else:
            shm.torpedoes_stake.board_visible.set(False)

        self.close_point(mat, split, colorspace)
        # print(self.lever_finished(mat, 'board', M, p))
        self.post('contours', mat)

    def lever_finished(self, mat, split, image, mask, output, colorspace="lab"):
        shape = self.static[image]['org'].shape[1] + GUTTER_PAD
        lever_gutter = ((-GUTTER_PAD, self.options['lever_gutter_top']),
                        (-GUTTER_PAD, self.options['lever_gutter_bot']),
                        (shape, self.options['lever_gutter_bot']),
                        (shape, self.options['lever_gutter_top']),
                        )
        gutter_transformed = [self.locate_source_point(image, mask, pt, output, (0, 255, 0))[0][0] for pt in lever_gutter]
        gutter_transformed = np.float32(gutter_transformed).reshape(-1, 1, 2)
        gutter_mask = np.zeros((mat.shape[0], mat.shape[1]), dtype=np.uint8)
        cv2.fillPoly(gutter_mask, [np.int32(gutter_transformed)], 255)
        self.post('gutter', gutter_mask)


        d = self.options['lever_color_distance']
        # threshed = [range_threshold(split[i],
        #             self.options['lever_%s' % colorspace[i]] - d, self.options['lever_%s' % colorspace[i]] + d)
        #             for i in range(1, len(colorspace))]

        threshed = thresh_color_distance(split, [self.options['lever_%s' % colorspace[i]] for i in range(len(colorspace))], d, weights=[0.5, 2, 2,])

        combined = reduce(lambda x, y: cv2.bitwise_and(x, y), threshed)
        combined = cv2.bitwise_and(combined, gutter_mask)
        self.post('thresh', combined)
        lever_endzone_left = self.locate_source_point(image, mask, (self.options['lever_endzone_left'], self.options['lever_gutter_top']), output, (0, 0, 0,))
        _, contours, _ = cv2.findContours(combined, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        filtered = [i for i in contours if cv2.contourArea(i) > self.options['contour_size_min']]
        cv2.drawContours(mat, filtered, -1, (0, 255, 0), 3)


        def contour_in_endzone(contour, p):
            M = cv2.moments(contour)

            center = (M['m10']/M['m00'], M['m01']/M['m00'])
            print(center)
            draw_circle(p, (int(center[0]), int(center[1])), 1, (0, 255, 0), thickness=3)

            return center[0] > lever_endzone_left[0][0][0] if MOVE_DIRECTION==1 else lever_endzone_left[0][0][0] > center[0]


        ret = any(map(lambda x: contour_in_endzone(x, output), filtered))

        return ret

    def close_point(self, mat, split, colorspace):
        color = [self.options['color_%s' % s] for s in colorspace]
        distance = self.options['water_distance']
        # threshed = [range_threshold(split[i],
        #             color[i] - distance, color[i] + distance)
        #             for i in range(1, len(color))]
        # mask = reduce(lambda x, y: cv2.bitwise_and(x, y), threshed)

        # mask, _ = thresh_color_distance(split, color, distance, weights=[0.5, 2, 2])

        mask = np.full((mat.shape[0], mat.shape[1]), 0, dtype=np.int32)
        if self.dst is not None:
            print(len(self.dst))
            cv2.fillPoly(mask, [np.int32(self.dst)], 1)
            if np.sum(mask) < mat.shape[0] * mat.shape[1] // 4 * 3:
                mask = None
        median_a = np.median(np.ma.array(split[1], mask=mask))
        median_b = np.median(np.ma.array(split[2], mask=mask))
        mask, _ = thresh_color_distance(split, [0, median_a, median_b], distance, weights=[0, 1, 1])
        # thresh_a = cv2.bitwise_not(range_threshold(split[1], median_a-self.options['water_a'], median_a+self.options['water_a']))
        # thresh_b = cv2.bitwise_not(range_threshold(split[2], median_b-self.options['water_a'], median_b+self.options['water_b']))
        # self.post('a', thresh_a)
        # self.post('b', thresh_b)
        # mask = reduce(lambda x, y: cv2.bitwise_and(x, y), [mask, thresh_a, thresh_b])
        #
        # contours = outer_contours(mask)
        _, contours, _ = cv2.findContours(mask, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)
        self.post('something', mask)
        #


        # canny_post = np.zeros(mat.shape)
        # canny = simple_canny(mat, sigma=self.options['sigma_canny'], use_mean=True)
        # contours = outer_contours(canny)
        # for i in range(len((contours))):
        #     cv2.drawContours(canny_post, contours, i, (i*20, i*20, i*20))
        # self.post('canny', canny)
        if contours is not None and len(contours) > 0:
            # shm.torpedoes_stake.close_visible.set(True)

            def filter_ellipses(contour):
                if len(contour) < 5: return False
                if contour_area(contour) < 200: return False
                _, (MA, ma), _ = cv2.fitEllipse(contour)
                try:
                    return (self.options['max_eccentricity'] > 1-(MA**2)/(ma**2))**(1/2) > self.options['min_eccentricity'] and contour_area(contour)/(MA * ma / 4 * pi) > self.options['min_elllipsish_thing']
                except ZeroDivisionError:
                    return False

            close = list(filter(filter_ellipses, contours))
            close.sort(key=contour_area)

            close1 = None
            close2 = None
            try:
                close1 = {'contour': close[0], 'centroid': contour_centroid(close[0]), 'area': contour_area(close[0])}
                close2 = {'contour': close[1], 'centroid': contour_centroid(close[1]), 'area': contour_area(close[1])}

                if close1['centroid'][0] > close2['centroid'][0]:
                    temp = close2
                    close2 = close1
                    close1 = temp

            except IndexError:
                pass

            if close1 is not None:
                shm.torpedoes_stake.close_left_x.set(close1['centroid'][0] + self.options['close_offset_x'])
                shm.torpedoes_stake.close_left_y.set(close1['centroid'][1] + self.options['close_offset_y'])
                shm.torpedoes_stake.close_left_size.set(close1['area'])
                shm.torpedoes_stake.close_left_visible.set(True)

                if close2 is not None:
                    shm.torpedoes_stake.close_right_x.set(close2['centroid'][0] + self.options['close_offset_x'])
                    shm.torpedoes_stake.close_right_y.set(close2['centroid'][1] + self.options['close_offset_y'])
                    shm.torpedoes_stake.close_right_size.set(close2['area'])
                    shm.torpedoes_stake.close_right_visible.set(True)
                else:
                    shm.torpedoes_stake.close_right_visible.set(False)
            else:
                shm.torpedoes_stake.close_left_visible.set(False)
                shm.torpedoes_stake.close_right_visible.set(False)

            # _, contours, _ = cv2.findContours(canny, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

            heart = [c for c in contours if cv2.contourArea(c) > 300]
            if heart:
                heart = min(heart, key=lambda c:cv2.matchShapes(heart_original, c, cv2.CONTOURS_MATCH_I1, 0))
                print(cv2.matchShapes(heart_original, heart, cv2.CONTOURS_MATCH_I1, 0))
                if not cv2.matchShapes(heart_original, heart, cv2.CONTOURS_MATCH_I1, 0) > 0.25:
                    h_centroid = contour_centroid(heart)
                    shm.torpedoes_stake.close_heart_x.set(h_centroid[0] + self.options['close_offset_x'])
                    shm.torpedoes_stake.close_heart_y.set(h_centroid[1] + self.options['close_offset_y'])
                    shm.torpedoes_stake.close_heart_size.set(contour_area(heart))
                    shm.torpedoes_stake.close_heart_visible.set(True)
                    cv2.drawContours(mat, [heart], -1, (0, 0, 255), thickness=5)
            else:
                    shm.torpedoes_stake.close_heart_visible.set(False)

            try:
                cv2.drawContours(mat, [close[0]], -1, (255, 0, 0), thickness=5)
                cv2.drawContours(mat, [close[1]], -1, (255, 0, 0), thickness=5)
            except IndexError:
                pass

        else:
            # shm.torpedoes_stake.close_visible.set(False)
            pass

    def process(self, *mats):
        # x = time.perf_counter()
        DOWNSIZE_CAMERA = self.options['downsize_camera']

        mat = cv2.cvtColor(mats[0], cv2.COLOR_BGR2GRAY)

        img2 = resize(to_umat(mat), int(mat.shape[1]*DOWNSIZE_CAMERA), int(mat.shape[0]*DOWNSIZE_CAMERA)) if DOWNSIZE_CAMERA else mat  # trainImage

        board = self.static_process('upper', 'lower')

        # find the keypoints and descriptors with SIFT
        kp2, des2 = self.detector.detectAndCompute(img2, None)
        cam = {"img": img2, "kp": kp2, "des": des2}

        p = resize(mats[0], int(mats[0].shape[1] * self.options['downsize_camera']), int(mats[0].shape[0] * self.options['downsize_camera']))
        p_mat = p.copy()

        p, M = self.match(board, cam, p, (255, 0, 0))
        if self.options['show_keypoints']: p = cv2.drawKeypoints(p, kp2, None, (255, 255, 0))

        assert p is not None

        self.post_shm(p_mat, p, M)

        self.post("outline", p)

        # print(time.perf_counter() - x)



if __name__ == '__main__':
    Stake('forward', opts)()
