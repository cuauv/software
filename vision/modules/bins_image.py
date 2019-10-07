#!/usr/bin/env python3
import shm
import cv2
from functools import reduce
import traceback
import sys

import time

import numpy as np
from vision.modules.base import ModuleBase  # , UndefinedModuleOption
from vision.framework.transform import resize, simple_gaussian_blur
from vision.framework.helpers import to_umat  # , from_umat
from vision.framework.draw import draw_circle
from vision.framework.color import bgr_to_lab, range_threshold


from vision import options

opts = [
    options.DoubleOption('rectangular_thresh', 0.8, 0, 1),
    options.DoubleOption('source_x_scale_bat', 0.1, 0, 1),
    options.DoubleOption('source_y_scale_bat', 0.1, 0, 1),
    options.DoubleOption('source_x_scale_wolf', 0.1, 0, 1),
    options.DoubleOption('source_y_scale_wolf', 0.1, 0, 1),
    options.DoubleOption('camera_scale', 0.35, 0, 1),
    options.IntOption('min_match_count', 10, 0, 255),
    options.DoubleOption('good_ratio', 0.8, 0, 1),
    options.BoolOption('show_keypoints', True),
    options.IntOption('min_gray', 83, 0, 255),
    #options.IntOption('img_l_trg', 71, 0, 255),
    #options.IntOption('img_a_trg', 94, 0, 255),
    #options.IntOption('img_b_trg', 164, 0, 255),
    #options.IntOption('img_d_thresh', 96, 0, 255), # 128
]


def make_poly(d1, d2):
    #print(d1, d2)
    return np.complex64([0, d1, (d1 + d2), d2])[:,np.newaxis]
PADDING = 50
GUTTER_PAD = 500
BLUR_KERNEL = 7
BLUR_SD = 1

HEART = (1356, 3250)
LEFT_CIRCLE = (390, 562)
RIGHT_CIRCLE = (1900, 570)

MOVE_DIRECTION=1  # 1 if lever on left else -1 if on right
kernel = np.ones((5, 5), np.uint8)

class BinsImage(ModuleBase):

    def heart(self):
        return (HEART[0] + self.options['heart_offset_x'], HEART[1] + self.options['heart_offset_y'])
    def left_circle(self):
        return (LEFT_CIRCLE[0] + self.options['left_circle_offset_x'], LEFT_CIRCLE[1] + self.options['left_circle_offset_y'])
    def right_circle(self):
        return (RIGHT_CIRCLE[0] + self.options['right_circle_offset_x'], RIGHT_CIRCLE[1] + self.options['right_circle_offset_y'])

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.detector = cv2.xfeatures2d.SIFT_create(contrastThreshold=.04)
        FLANN_INDEX_KDTREE = 0
        index_params = dict(algorithm=FLANN_INDEX_KDTREE, trees=5)  # For SIFT
        search_params = dict(checks=50)

        self.flann = cv2.FlannBasedMatcher(index_params, search_params)
        self.past_var = {"lever_origin_upper": np.zeros((5, 2)), "lever_origin_lower": np.zeros((5, 2))}
        self.static = {}


    def static_process(self, image1, name=None):
        if name is None: name = image1

        def find_key_descriptors(im):
            scaledim = resize(im, int(im.shape[1]*self.options['source_x_scale_%s' % image]),
                                  int(im.shape[0]*self.options['source_y_scale_%s' % image]))

            #mean = 0
            #sigma = 20
            #gauss = np.random.normal(mean,sigma,scaledim.shape)
            #scaledim = scaledim.astype(np.int16) + gauss.astype(np.int8)
            #np.clip(scaledim, 0, 255, out=scaledim)
            #scaledim = scaledim.astype(np.uint8)

            scaledim = np.pad(scaledim, ((PADDING, PADDING), (PADDING, PADDING)), 'constant', constant_values=255)
            rx = self.options['source_x_scale_%s' % image]
            ry = self.options['source_y_scale_%s' % image]
            scaledim = simple_gaussian_blur(scaledim, BLUR_KERNEL, BLUR_SD)
            kp, des = self.detector.detectAndCompute(scaledim, None)
            self.static[image] = {"name": image, "org": im,
                                  "img":  scaledim, "rx":  rx, "ry":  ry, "kp": kp, "des": des}
            keypoints = cv2.drawKeypoints(scaledim.copy(), kp, None, (0, 0, 255), flags=0)
            self.post(image, keypoints)
            return self.static[image]

        image = name

        if image in self.static:
            if self.static[image]['rx'] == self.options['source_x_scale_%s' % image] and \
                    self.static[image]['ry'] == self.options['source_y_scale_%s' % image]:
                return self.static[image]
            else:
                im1 = self.static[image]["org"]
                return find_key_descriptors(im1)
        else:
            im1 = cv2.imread('bins_images/%s.png' % image1, 0)
            self.static[image1] = {"org": im1}

            assert im1 is not None
            return find_key_descriptors(im1)


    def match(self, im1, im2, output, color, contour_segment, n_segs):
        MIN_MATCH_COUNT = self.options['min_match_count']
        RECTANGULARITY_THRESH = self.options['rectangular_thresh']
        kp1 = im1["kp"]
        kp2 = im2["kp"]
        des1 = im1["des"]
        des2 = im2["des"]
        img1 = im1["img"]
        img2 = im2["img"]

        #if des1.size == 0 or des2.size == 0: return (output if output is not None else img2), None
        try:
            matches = self.flann.knnMatch(des1, des2, k=2)
        except cv2.error as e:
            matches = []
            #print(e)

        if output is None: output = img2

        # store all the good matches as per Lowe's ratio test.
        good = [m for (m, n) in matches if m.distance < self.options['good_ratio'] * n.distance]
        #pts = np.int0([x.pt for x in kp2])
        #labels = msk[pts]
        #good = [m for m in good if kp
        #good = []
        #for m, n in matches:#(x for x in matches if len(x) == 2):
        #    if m.distance < self.options['good_ratio']*n.distance:
        #        good.append(m)


        if len(good) > MIN_MATCH_COUNT:
            #matchesView = cv2.drawMatchesKnn(img1, kp1, img2, kp2, matches, None)
            #self.post('match_' + im1['name'], matchesView)
            src_pts = np.float32([kp1[m.queryIdx].pt for m in good]).reshape(-1, 1, 2)
            dst_pts = np.float32([kp2[m.trainIdx].pt for m in good]).reshape(-1, 1, 2)

            lpts = np.int0(dst_pts)[:,0]
            #lpts = lpts[lpts[:,0] < contour_segment
            #print(lpts)
            lbs = contour_segment[lpts[:,1], lpts[:,0]]
            #print(lbs)
            #print(lbs.shape)
            if n_segs < 1:
                print('n_segs < 1')
                return output, None
            msks = [lbs == i for i in range(1, n_segs + 1)]
            mx = max(msks, key=lambda x: x.sum())
            #print(msks)
            #print(mx)
            src_pts = src_pts[mx]
            dst_pts = dst_pts[mx]
            g2 = [g for i, g in enumerate(good) if mx[i]]
            if mx.sum() == 0:
                print('mx.sum() == 0')
                return output, None
            #pts = np.int0([x.pt for x in kp2])
            #labels = msk[pts]

            #src_pts_2 = [kp1[m.queryIdx] for m in good]).reshape(-1, 1, 2)
            #dst_pts_2 = [kp2[m.trainIdx] for m in good]).reshape(-1, 1, 2)
            matchesView = cv2.drawMatches(img1, kp1, img2, kp2, good, None)
            self.post('match_' + im1['name'], matchesView)

            M, mask = cv2.findHomography(src_pts, dst_pts, cv2.RANSAC, 3.0)
            if M is None:
                print('No homography found', file=sys.stderr)
                return output, None
            g3 = [g2[i] for i, v in enumerate(mask) if v]
            matchesView2 = cv2.drawMatches(img1, kp1, img2, kp2, g3, None)
            self.post('inliers_' + im1['name'], matchesView2)
            # matchesMask = mask.ravel().tolist()
            # print("m" + str(M))
            # print("ma" + str(mask))
            # print("mm" + str(matchesMask))

            h, w = img1.shape
            pts = np.float32([[PADDING, PADDING], [PADDING, h-1-PADDING], [w-1-PADDING, h-PADDING-1], [w-PADDING-1, PADDING]]).reshape(-1, 1, 2)

            try:
                #print(pts, M)
                dst = cv2.perspectiveTransform(pts, M)
                area = cv2.contourArea(dst)
                rarea = cv2.minAreaRect(dst)
                rarea = rarea[1][0]*rarea[1][1]
                if area/rarea < RECTANGULARITY_THRESH:
                    print("box lower than rectangularity threshold")
                    return output, None

                def e_length(pt1, pt2):
                    return ((pt1[0] - pt2[0])**2 + (pt1[1]-pt2[1])**2)**(0.5)

                def norm_length_diff(dst, line1, line2):
                    l1 = e_length(dst[line1[0]][0], dst[line1[1]][0])
                    l2 = e_length(dst[line2[0]][0], dst[line2[1]][0])
                    return (l2-l1)

                Moments = cv2.moments(dst)

                #getattr(shm.torpedoes_stake, "%s_align_h" % im1["name"]).set(norm_length_diff(dst, (0,1), (2,3)))
                ## getattr(shm.torpedoes_stake, " % s_align_v" % im1["name"]).set(norm_length_diff(dst, (0,2), (1,3)))
                #getattr(shm.torpedoes_stake, "%s_size" % im1["name"]).set(area)
                #getattr(shm.torpedoes_stake, "%s_center_x" % im1["name"]).set(Moments["m10"]/Moments['m00'])
                #getattr(shm.torpedoes_stake, "%s_center_y" % im1["name"]).set(Moments["m01"]/Moments['m00'])

                output = cv2.polylines(output, [np.int32(dst)], True, color, 3, cv2.LINE_AA)

                return output, M
            except ZeroDivisionError:
                print('Division by zero')
            except cv2.error as e:
                #traceback.print_exception(type(e), e)
                traceback.print_exc()
                #print(e)

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

    def process(self, *mats):
        # x = time.perf_counter()
        CAMERA_SCALE = self.options['camera_scale']
        t = time.perf_counter()
        print('a', time.perf_counter() - t); t = time.perf_counter()


        mat = resize(mats[0], int(mats[0].shape[1]*CAMERA_SCALE), int(mats[0].shape[0]*CAMERA_SCALE)) if CAMERA_SCALE else mats[0]
        temp = mat.astype(np.uint16) * 2#self.options_dict['PPX_contrast'].value
        mat = np.clip(temp, 0, 255).astype(np.uint8)
        p = mat.copy()
        #rv, ccs = cv2.findChessboardCorners(mat, (1, 1))
        #print(rv)
        #l_mat = cv2.cvtColor(mat, cv2.COLOR_BGR2Lab)
        #mm = l_mat.astype(np.int16)
        #dst = np.abs(mm[:,:,0] - self.options['img_l_trg']) + \
        #    np.abs(mm[:,:,1] - self.options['img_a_trg']) + \
        #    np.abs(mm[:,:,2] - self.options['img_b_trg'])
        #self.post('yellowness', (dst // 3).astype(np.uint8))
        #np.clip(dst, 0, 255, out=dst)
        #dst = dst.astype(np.uint8)
        #res, yellow_mask = cv2.threshold(dst, self.options['img_d_thresh'], 255, cv2.THRESH_BINARY_INV)
        #self.post('yellow_mask', yellow_mask)

        img2 = cv2.cvtColor(to_umat(mat), cv2.COLOR_BGR2GRAY)
        print('b', time.perf_counter() - t); t = time.perf_counter()
        #corners = cv2.cornerHarris(img2, 2, 3, .04)
        #print(corners.get().dtype)
        #cg = corners.get()
        #cg[cg <= 0] = cg[cg > 0].min()
        #print(cg.max())
        #print(cg.min(), cg.max())
        #ll = np.log(cg)
        #print(ll.min(), ll.max())
        #self.post('harris', np.clip((ll * 5 + 128), 0, 255).astype(np.uint8))
        #img2 = to_umat(np.pad(cv2.cvtColor(mat, cv2.COLOR_BGR2GRAY), ((PADDING, PADDING), (PADDING, PADDING)), 'constant', constant_values=255))
        #mat = to_umat(np.pad(img2.get(), ((PADDING, PADDING), (PADDING, PADDING)), 'constant', constant_values=255))

        #img2 = resize(to_umat(mat), int(mat.shape[1]*camera_scale), int(mat.shape[0]*camera_scale)) if DOWNSIZE_CAMERA else mat  # trainImage

        print('h', time.perf_counter() - t); t = time.perf_counter()
        bat = self.static_process('bat')
        wolf = self.static_process('wolf')
        print('i', time.perf_counter() - t); t = time.perf_counter()

        #black_areas = img2 < self.options['min_gray']
        res, black_areas = cv2.threshold(img2, self.options['min_gray'], 255, cv2.THRESH_BINARY_INV)
        black_areas = cv2.erode(black_areas, kernel)
        black_areas = cv2.dilate(black_areas, kernel, iterations=2)
        self.post('black_areas', black_areas)
        img, contours, hierarchy = cv2.findContours(black_areas, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        msk = np.zeros(mat.shape[:-1], dtype=np.uint8)
        #szcs = sorted(contours, key=cv2.contourArea, reverse=True)
        print('j', time.perf_counter() - t); t = time.perf_counter()
        
        for i, x in enumerate(contours):#szcs[:2]:
            pts = cv2.boxPoints(cv2.minAreaRect(x))
            #mm = np.mean(x, axis=0)
            mpp = np.mean(pts, axis=0)
            #print(pts)
            #print(np.mean(pts, axis=0))
            cv2.drawContours(msk, [np.int0(pts - (pts - (pts * 4 + mpp) / 5) * .1)], -1, i+1, -1)
            #print(x * 9 + mm, mm)
           # print(x)
            #cv2.drawContours(msk, [np.int0((x * 2 + mm) / 3)], -1, i+1, -1)

        #for p in szcs[0:2]:
        #    msk = cv2.fillPoly(msk, p, 255)
        colors = np.uint8([(0, 0, 0), (0, 0, 255), (0, 255, 0), (255, 0, 0), (0, 255, 255), (255, 255, 0), (255, 0, 255), (255, 255, 255)])
        self.post('blk_fill', colors[np.clip(msk, 0, 7)])
        print('k', time.perf_counter() - t); t = time.perf_counter()
                

        #print(szcs)
        #black_areas = black_areas.astype(np.uint8)
        #target_area = cv2.dilate(black_areas, kernel, iterations=10)
        #target_area = target_area.get()
        #self.post('target_area', target_area)

        #target_area = cv2.erode(yellow_mask, kernel)
        #target_area = cv2.dilate(target_area, kernel, iterations=10)
        #target_area &= msk
        target_area = msk
        #self.post('target_area', target_area)

        # find the keypoints and descriptors with SIFT

        #img2 = cv2.UMat(np.pad(img2.get(), ((PADDING, PADDING), (PADDING, PADDING)), 'constant', constant_values=255)) # boo
        print('l', time.perf_counter() - t); t = time.perf_counter()
        kp2, des2 = self.detector.detectAndCompute(img2, None)
        print('l1', time.perf_counter() - t); t = time.perf_counter()
        if des2 is None:
            print('No points found')
            return
        dg = des2.get()
        if dg is None:
            print('No points found')
            return
        #cv2.UMat(des2, [0, 1, 2])
        #print(dir(des2))
        #print(des2.get([0, 1, 2]))
        #print(kp2)
        #print(max(x.pt[1] for x in kp2))
        #print(max(x.pt[0] for x in kp2))
        #p = resize(mats[0], int(mats[0].shape[1] * self.options['camera_scale']), int(mats[0].shape[0] * self.options['camera_scale']))
        #p = cv2.copyMakeBorder(p, PADDING, PADDING, PADDING, PADDING, cv2.BORDER_CONSTANT, None, (255, 255, 255))
        if self.options['show_keypoints']:
            p = cv2.drawKeypoints(p, kp2, None, (0, 255, 255))
        #print([x.pt for x in kp2])
        idxs = [i for (i, x) in enumerate(kp2) if (0 < x.pt[1] < target_area.shape[0]) and (0 < x.pt[0] < target_area.shape[1]) and target_area[int(x.pt[1]), int(x.pt[0])]]
        kp2 = [kp2[i] for i in idxs]
        des2 = cv2.UMat(dg[idxs])
        #print(kp2[0].pt)
        cam = {"img": img2, "kp": kp2, "des": des2}

        if self.options['show_keypoints']:
            p = cv2.drawKeypoints(p, kp2, None, (255, 255, 0))
        p_mat = p.copy()

        def get_center_ang(img, mat):
            ii = img['img']
            pts = np.float32([[ii.shape[1] / 2, ii.shape[0] / 2], [ii.shape[1] / 2, 0]]).reshape(-1, 1, 2) + PADDING
            middle, top = cv2.perspectiveTransform(pts, mat)[:,0,:]
            up_vec = top - middle
            return middle, np.arctan2(up_vec[1], up_vec[0])

        if kp2:
            print('m', time.perf_counter() - t); t = time.perf_counter()
            p, M1 = self.match(bat, cam, p, (0, 0, 255), msk, len(contours))
            p, M2 = self.match(wolf, cam, p, (0, 255, 0), msk, len(contours))
            if M1 is not None:
                ctr, ang = get_center_ang(bat, M1)
                shm.bins_status.bat_x.set(ctr[0] / mat.shape[1] - .5)
                shm.bins_status.bat_y.set((ctr[1] - mat.shape[0] / 2) / mat.shape[1])
                shm.bins_status.bat_angle.set(ang)
                shm.bins_status.bat_visible_frames.set(shm.bins_status.bat_visible_frames.get()+1)
            shm.bins_status.bat_visible.set(M1 is not None)

            if M2 is not None:
                ctr, ang = get_center_ang(bat, M2)
                shm.bins_status.wolf_x.set(ctr[0] / mat.shape[1] - .5)
                shm.bins_status.wolf_y.set((ctr[1] - mat.shape[0] / 2) / mat.shape[1])
                shm.bins_status.wolf_angle.set(ang)
                shm.bins_status.wolf_visible_frames.set(shm.bins_status.wolf_visible_frames.get()+1)
            shm.bins_status.wolf_visible.set(M2 is not None)

            
        print('n', time.perf_counter() - t); t = time.perf_counter()

        assert p is not None

        #try:
        #    self.post_shm(p_mat, p, M)
        #except cv2.error as e:
        #    print(e)

        self.post("outline", p)

        # print(time.perf_counter() - x)



if __name__ == '__main__':
    BinsImage('downward', opts)()
