#!/usr/bin/env python3
from vision.modules.base import ModuleBase
from vision.options import *
import shm
import cv2
import numpy as np
from vision.framework.transform import resize

opts = [
        IntOption('t_l_trg', 151, 0, 255),
        IntOption('t_a_trg', 207, 0, 255),
        IntOption('t_b_trg', 174, 0, 255),
        #IntOption('t_d_thresh', 49, 0, 255), # 128
        IntOption('b_l_trg', 54, 0, 255),
        IntOption('b_a_trg', 173, 0, 255),
        IntOption('b_b_trg', 161, 0, 255),
        #IntOption('b_d_thresh', 34, 0, 255), # 128
        IntOption('combined_thresh', 66, 0, 255),
        DoubleOption('compactness', .05, 0, .1)
]

def dst_thresh(img, trg):
        dsi = np.linalg.norm(img - trg, axis=2)
        #print(dst.shape)
        #np.abs(col[:,:,0] - self.options['l_trg']) + \
        #      np.abs(col[:,:,1] - self.options['a_trg']) + \
        #      np.abs(col[:,:,2] - self.options['b_trg'])
        np.clip(dsi, 0, 255, out=dsi)
        dsi = dsi.astype(np.uint8)
        return dsi#, (dsi < dst)

kernel = np.ones((5, 5), np.uint8)
class BinsLever(ModuleBase):
    def process(self, mat):
        mat = resize(mat, mat.shape[1] // 4, mat.shape[0] // 4)

        col = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB).astype(np.int16)
        dst1 = dst_thresh(col, (self.options['t_l_trg'], self.options['t_a_trg'], self.options['t_b_trg']))
        dst2 = dst_thresh(col, (self.options['b_l_trg'], self.options['b_a_trg'], self.options['b_b_trg']))
        cdst = dst1.astype(np.uint16) * dst2.astype(np.uint16)
        t3 = cdst < (self.options['combined_thresh'] ** 2)
        self.post('dst1', dst1)
        self.post('dst2', dst2)
        self.post('cdst', (cdst ** .5).astype(np.uint8))
        #t2 = dst < self.options['d_thresh']
        #t1 = t1.astype(np.uint8) * 255
        #t2 = t2.astype(np.uint8) * 255 
        t3 = t3.astype(np.uint8) * 255 
        #self.post('t1', t1)
        #self.post('t2', t2)
        self.post('t3', t3)
        img, contours, hierarchy = cv2.findContours(t3, cv2.RETR_CCOMP, cv2.CHAIN_APPROX_SIMPLE)
        if contours is None or hierarchy is None: 
            shm.bins_status.lever_visible.set(False)
        else:
            valid_contours =  (hierarchy[0,:,3] < 0) # (hierarchy[0,:,2] < 0)
            #print(valid_contours.shape, contours.shape)
            #print(hierarchy)
            #print(len(contours), valid_contours)
            #contours = contours[valid_contours]
            #print([(cv2.contourArea(x), cv2.arcLength(x, True) ** 2 / 2) for x in contours])
            contours = [x for i, x in enumerate(contours) if valid_contours[i] and cv2.contourArea(x) > cv2.arcLength(x, True) ** 2 / 2 * self.options['compactness']]
            #print(contours)
            if contours:
                mx = max(contours, key=cv2.contourArea)
                mm = cv2.moments(mx)
                if mm['m00'] != 0:
                    cx = mm['m10'] / mm['m00']
                    cy = mm['m01'] / mm['m00']
                    cv2.drawContours(mat, [mx], -1, (0, 255, 0), 2)
                    norm_x = cx / mat.shape[1] - .5
                    norm_y = (cy - mat.shape[0] / 2) / mat.shape[1]
                    shm.bins_status.lever_x.set(norm_x)
                    shm.bins_status.lever_y.set(norm_y)
                    shm.bins_status.lever_sz.set(cv2.contourArea(mx) ** .5)
                    shm.bins_status.lever_visible.set(True)
                else:
                    shm.bins_status.lever_visible.set(False)
            else:
                shm.bins_status.lever_visible.set(False)
        self.post('mat', mat)



        #t1 = cv2.erode(t1, kernel)
        #t1 = cv2.dilate(t1, kernel, iterations=4)
        #t2 = cv2.erode(t2, kernel)
        #t2 = cv2.dilate(t2, kernel, iterations=4)
        #self.post('b', t1 & t2)

        

if __name__ == '__main__':
    BinsLever('forward', opts)()
