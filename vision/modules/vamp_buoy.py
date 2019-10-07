#!/usr/bin/env python3
import shm
import cv2

import time

import numpy as np
from vision.modules.base import ModuleBase
from vision.framework.transform import resize, simple_gaussian_blur
from vision.framework.helpers import to_umat, from_umat

from vision import options

opts =    [options.DoubleOption('rectangular_thresh', 0.7, 0, 1),
           options.DoubleOption('source_x_scale_draugr', 1, 0, 1),
           options.DoubleOption('source_y_scale_draugr', 1, 0, 1),
           options.DoubleOption('source_x_scale_vetalas', 1, 0, 1),
           options.DoubleOption('source_y_scale_vetalas', 1, 0, 1),
           options.DoubleOption('source_x_scale_aswang', 0.69, 0, 1), #CHANGE BACK TO 1 IF COMPETITION BUOY IS NOT STRETCHED
           options.DoubleOption('source_y_scale_aswang', 1, 0, 1),
           options.DoubleOption('source_x_scale_jiangshi', 1, 0, 1),
           options.DoubleOption('source_y_scale_jiangshi', 1, 0, 1),
           options.DoubleOption('downsize_camera', 0.25, 0, 1),
           options.IntOption('min_match_count', 10, 0, 255),
           options.DoubleOption('good_ratio', 0.8, 0, 1),
           options.BoolOption('show_keypoints', False)]

PADDING = 100

class VampBuoy(ModuleBase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        #self.source = cv2.imread('buoy_images/draugr.png',0)
        # self.orb = cv2.ORB_create(nfeatures=250, WTA_K=3)
        self.detector = cv2.xfeatures2d.SIFT_create()
        FLANN_INDEX_KDTREE = 0
        #FLANN_INDEX_LSH = 6
        index_params = dict(algorithm = FLANN_INDEX_KDTREE, trees = 5) #For SIFT
        # index_params= dict(algorithm = FLANN_INDEX_LSH,
        #            table_number = 12, #6, # 12
        #            key_size = 20, #12,     # 20
        #            multi_probe_level = 2) #1) #2 #For ORB
        search_params = dict(checks = 50)

        self.flann = cv2.FlannBasedMatcher(index_params, search_params)
        #self.flann = cv2.BFMatcher(normType=cv2.NORM_HAMMING2, crossCheck=False)
        #self.flann = cv2.BFMatcher()
        self.static = {}
        self.visible = {}

    def static_process(self, image):

        def find_key_descriptors(im):
            if self.options['source_x_scale_%s'%image] !=0 and self.options['source_y_scale_%s'%image] != 0:
                scaledim = resize(im, int(im.shape[1]*self.options['source_x_scale_%s'%image]),
                                      int(im.shape[0]*self.options['source_y_scale_%s'%image]))
                scaledim = np.pad(scaledim, ((PADDING, PADDING), (PADDING, PADDING)), 'constant', constant_values=255)
                rx = self.options['source_x_scale_%s'%image]
                ry = self.options['source_y_scale_%s'%image]
            else:
                scaledim = im
                rx = 1
                ry = 1
            kp, des = self.detector.detectAndCompute(scaledim,None)
            self.static[image] = {"name": image, "org": im, "img": scaledim, "rx": rx, "ry":ry, "kp":kp, "des":des}
            self.post(image, cv2.drawKeypoints(scaledim, kp, None, (0,0,255), flags=0))
            return self.static[image]

        if image in self.static:
            if self.static[image]['rx'] == self.options['source_x_scale_%s'%image] and \
               self.static[image]['ry'] == self.options['source_y_scale_%s'%image]:
                return self.static[image]
            else:
                im = self.static[image]["org"]
                return find_key_descriptors(im)
        else:
            im = simple_gaussian_blur(cv2.imread('buoy_images/%s.png' %image,0), 11, 3)
            im = resize(im, im.shape[1]//2, im.shape[0]//2)
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
        self.visible[im1['name']] = False

        try:
            matches = self.flann.knnMatch(des1,des2,k=2)
        except cv2.error as e:
            matches = []
            print(e)

        if output is None: output = img2

        # store all the good matches as per Lowe's ratio test.
        good = []
        for m,n in (x for x in matches if len(x) == 2) :
            if m.distance < self.options['good_ratio']*n.distance:
                good.append(m)

        if len(good)>MIN_MATCH_COUNT:
            src_pts = np.float32([ kp1[m.queryIdx].pt for m in good ]).reshape(-1,1,2)
            dst_pts = np.float32([ kp2[m.trainIdx].pt for m in good ]).reshape(-1,1,2)

            M, mask = cv2.findHomography(src_pts, dst_pts, cv2.RANSAC,5.0)
            matchesMask = mask.ravel().tolist()

            h,w = img1.shape
            pts = np.float32([ [PADDING,PADDING],[PADDING,h-1-PADDING],[w-1-PADDING,h-PADDING-1],[w-PADDING-1,PADDING] ]).reshape(-1,1,2)


            try:
                dst = cv2.perspectiveTransform(pts,M)
                area = cv2.contourArea(dst)
                mar = cv2.minAreaRect(dst)
                rarea = mar[1][0]*mar[1][1]
                mom = cv2.moments(dst)
                if area/rarea < RECTANGULARITY_THRESH:
                    print("box lower than rectangularity threshold")
                    return output
                output = cv2.polylines(output,[np.int32(dst)],True,color,3, cv2.LINE_AA)
                self.visible[im1['name']] = ((mom["m10"]/mom["m00"], mom["m01"]/mom["m00"]), area)
                self.post_shm_align(im1['name'], dst)
            except ZeroDivisionError:
                print('what')
            except cv2.error as e:
                print(e)

        else:
            print ("Not enough matches are found for %s - %d/%d" % (im1["name"], len(good),MIN_MATCH_COUNT))
            matchesMask = None
        return output

    def process(self, *mats):
        x = time.perf_counter()
        DOWNSIZE_CAMERA = self.options['downsize_camera']

        img2 = resize(to_umat(mats[0]), int(mats[0].shape[1]*DOWNSIZE_CAMERA), int(mats[0].shape[0]*DOWNSIZE_CAMERA)) if DOWNSIZE_CAMERA else mats[0] # trainImage

        #img2 = cv2.copyMakeBorder(img2, PADDING,PADDING,PADDING,PADDING, cv2.BORDER_REPLICATE)

        draugr = self.static_process('draugr')
        vetalas = self.static_process('vetalas')
        aswang = self.static_process('aswang')
        jiangshi = self.static_process('jiangshi')

        # find the keypoints and descriptors with SIFT
        kp2, des2 = self.detector.detectAndCompute(img2,None)
        cam = {"img": img2, "kp":kp2, "des": des2}


        p = self.match(draugr, cam, None, (255,0,0))
        p = self.match(vetalas, cam, p, (0, 255, 0))
        p = self.match(aswang, cam, p, (0,0,255))
        p = self.match(jiangshi, cam, p, (255,0,255))

        if self.options['show_keypoints']:
            p = cv2.drawKeypoints(p, kp2, None, (255,255,0))

        p = from_umat(p)

        self.post_shm()
        shm.vamp_buoy_results.camera_x.set(p.shape[1]//2)
        shm.vamp_buoy_results.camera_y.set(p.shape[0]//2)

        self.post("outline", p)
        print(time.perf_counter() - x)

    def post_shm_align(self, image, dst):
        def e_length(pt1, pt2):
            return ((pt1[0] - pt2[0])**2 + (pt1[1]-pt2[1])**2)**(0.5)

        def norm_length_diff(dst, line1, line2):
            l1 = e_length(dst[line1[0]][0], dst[line1[1]][0])
            l2 = e_length(dst[line2[0]][0], dst[line2[1]][0])
            return (l2-l1)

        getattr(shm.vamp_buoy_results, "%s_align_h"%image).set(int(norm_length_diff(dst, (0,1), (2,3))))
        getattr(shm.vamp_buoy_results, "%s_align_v"%image).set(int(norm_length_diff(dst, (0,2), (1,3))))

    def post_shm(self):
        for k, v in self.visible.items():
            getattr(shm.vamp_buoy_results, "%s_visible"%k).set(bool(v))
            if v:
                getattr(shm.vamp_buoy_results, "%s_center_x"%k).set(int(v[0][0]))
                getattr(shm.vamp_buoy_results, "%s_center_y"%k).set(int(v[0][1]))
                getattr(shm.vamp_buoy_results, "%s_size"%k).set(v[1])


if __name__ == '__main__':
    VampBuoy('forward', opts)()
