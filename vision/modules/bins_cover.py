#!/usr/bin/env python3
from vision import options
import numpy as np
from vision.modules.base import ModuleBase  # , UndefinedModuleOption
from vision.framework.helpers import to_umat  # , from_umat
from vision.framework.transform import resize, simple_gaussian_blur
import cv2
import shm

opts = [
    options.DoubleOption('camera_scale', 0.35, 0, 1),
    options.IntOption('lid_l_trg', 198, 0, 255),
    options.IntOption('lid_a_trg', 119, 0, 255),
    options.IntOption('lid_b_trg', 196, 0, 255),
    options.IntOption('lid_d_thresh', 40, 0, 255), # 128
    options.IntOption('canny1', 459, 0, 1000), # 25
    options.IntOption('canny2', 496, 0, 1000), # 93
    options.IntOption('houghness', 51, 0, 1000),
    options.DoubleOption('min_cross_score', .15, 0, 1),
    options.BoolOption('debug', False)
]

def make_poly(d1, d2):
    #print(d1, d2)
    return np.complex64([0, d1, (d1 + d2), d2])[:,np.newaxis]

class BinsCover(ModuleBase):
    def process(self, mat):
        camera_scale = self.options['camera_scale']
        debug = self.options['debug']
        if camera_scale != 0:
            mat = resize(mat, int(mat.shape[1]*camera_scale), int(mat.shape[0]*camera_scale))

        l_mat = cv2.cvtColor(mat, cv2.COLOR_BGR2Lab)
        if debug:
            mm = l_mat.astype(np.int32)
            dst_lid = (mm[:,:,0] - self.options['lid_l_trg']) ** 2 + \
                (mm[:,:,1] - self.options['lid_a_trg']) ** 2 + \
                (mm[:,:,2] - self.options['lid_b_trg']) ** 2
            self.post('yellowness_lid', ((dst_lid / 3) ** .5).astype(np.uint8))
            #np.clip(dst_lid, 0, 255, out=dst_lid)
            #dst_lid = dst_lid.astype(np.uint8)
            #res, yellow_mask_lid = cv2.threshold(to_umat(dst_lid), self.options['lid_d_thresh'], 255, cv2.THRESH_BINARY_INV)
            yellow_mask_lid = (dst_lid < self.options['lid_d_thresh'] ** 2).astype(np.uint8) * 255
            self.post('yellow_mask_lid', yellow_mask_lid)
        img2 = cv2.cvtColor(to_umat(mat), cv2.COLOR_BGR2GRAY)
        edg = cv2.Canny(img2, self.options['canny1'], self.options['canny2'], apertureSize=3)
        #yellow_edg_msk = cv2.erode(yellow_mask_lid, kernel)
        #yellow_edg_msk = cv2.dilate(yellow_edg_msk, kernel, iterations=4)
        #self.post('edg0', edg)
        #edg = cv2.bitwise_and(edg, yellow_edg_msk)
        self.post('edg', edg)

        lines = cv2.HoughLines(edg, 1, np.pi/180, self.options['houghness']).get()
        clrs = [(0, 0, 255), (0, 255, 0), (255, 0, 0), (0, 255, 255), (255, 255, 0), (255, 0, 255), (0, 0, 128), (0, 128, 0), (128, 0, 0)]

        if lines is None:
            self.post('lines', mat)
            shm.bins_status.cover_visible.set(False)
            return

        lines[lines[:,0,0] < 0,:,1] += np.pi
        lines[:,:,0] = np.abs(lines[:,:,0])
        lvecs = np.exp(1j * lines[:,:,1])
        mlvecs = lvecs ** 2
        #print(lvecs)
        if debug:
            for i, (dst, vect) in enumerate(zip(lines[:,0,0], lvecs[:,0])):
                ctr = vect * dst
                vc = vect * 1j
                p1 = ctr + 1000 * vc
                p2 = ctr - 1000 * vc
                mat = cv2.line(mat, (int(p1.real), int(p1.imag)), (int(p2.real), int(p2.imag)), (0, 0, 200), 2)
            #print(vect, dst)
        m = cv2.BFMatcher(cv2.NORM_L2)
        cc = mlvecs.astype(np.complex64).view(np.float32)
        centers = []
        if len(mlvecs) > 1:
            dsst = 2.83 - shm.kalman.depth.get()
            plen = 75 / dsst if dsst > 0 else 1000
            res = m.match(cc, -cc)
            for m in res:
                if m.distance > .05: continue
                d1 = lvecs[m.trainIdx,0], lines[m.trainIdx,0,0]
                d2 = lvecs[m.queryIdx,0], lines[m.queryIdx,0,0]
                try:
                    center = np.linalg.solve(np.complex64([d1[0] / abs(d1[0]), d2[0] / abs(d2[0])]).view(np.float32).reshape(2,-1), [[d1[1]], [d2[1]]])[:,0].astype(np.float64)#.view(np.complex64)[0]
                except np.linalg.linalg.LinAlgError:
                    print('singular matrix')
                    continue
                cC = complex(*center)
                mz = np.zeros(mat.shape[:-1], dtype=np.uint8)
                mz2 = np.zeros(mat.shape[:-1], dtype=np.uint8)
                poly = make_poly(d1[0], d2[0])
                rpoly = make_poly(d1[0], -d2[0])

                mz = cv2.fillConvexPoly(mz, (cC + plen * poly).view(np.float32).astype(np.int32), 255)
                mz = cv2.fillConvexPoly(mz, (cC - plen * poly).view(np.float32).astype(np.int32), 255)
                mz2 = cv2.fillConvexPoly(mz2, (cC + plen * rpoly).view(np.float32).astype(np.int32), 255)
                mz2 = cv2.fillConvexPoly(mz2, (cC - plen * rpoly).view(np.float32).astype(np.int32), 255)
                m1, sd1 = cv2.meanStdDev(img2, mask=mz)
                m2, sd2 = cv2.meanStdDev(img2, mask=mz2)

                m1, sd1, m2, sd2 = (x.get()[0,0] for x in (m1, sd1, m2, sd2))
                score = abs(m1 - m2) / (sd1 * sd2)
                if score < self.options['min_cross_score']: continue
                
                no_swap = np.cross(*np.complex128([[d1[0]], [d2[0]]]).view(np.float64)) > 0
                d1, d2 = (d1, d2) if no_swap else (d2, d1)
                long_axis, short_axis = (d1[0], d2[0]) if m1 < m2 else (d2[0], d1[0])
                sm = mz if (m1 > m2) else mz2 #  ^ no_swap
                m_color = cv2.mean(l_mat, mask=sm)[0:3]
                passes_color = cv2.norm(np.float32(m_color), np.float32([self.options['lid_l_trg'], self.options['lid_a_trg'], self.options['lid_b_trg']])) < self.options['lid_d_thresh']
                if passes_color:
                    centers.append((cC, d1, d2, score, long_axis, short_axis, sm))
                try:
                    mat = cv2.circle(mat, (int(center[0]), int(center[1])), 10, (255, 0, 0) if passes_color else (0, 200, 200), 2)
                except (ValueError, OverflowError):
                    pass
            if centers:
                mx = max(centers, key=lambda h: h[3])
                center, d1, d2, score, long_axis, short_axis, sm = mx
                #dsst = 4.26 - shm.kalman.depth.get() - .762
                pts = (np.complex64([long_axis + short_axis, long_axis - short_axis, -long_axis - short_axis, -long_axis + short_axis])[:,np.newaxis] * plen + center).view(np.float32).astype(np.int32)
                print(pts)
                mat = cv2.polylines(mat, [pts], True, (255, 255, 255))
                print(score, shm.kalman.depth.get())
                if long_axis.imag < 0: long_axis *= -1
                if short_axis.real < 0: short_axis *= -1
                #self.post('sm', sm)
                shm.bins_status.cover_x.set(center.real / mat.shape[1] - .5)
                shm.bins_status.cover_y.set((center.imag - mat.shape[0] / 2) / mat.shape[1])
                shm.bins_status.cover_maj_x.set(long_axis.real)
                shm.bins_status.cover_maj_y.set(long_axis.imag)
                shm.bins_status.cover_min_x.set(short_axis.real)
                shm.bins_status.cover_min_y.set(short_axis.imag)
                shm.bins_status.cover_visible.set(True)

                for ax, clr in ((short_axis, (255, 0, 0)), (long_axis, (0, 255, 0))):
                    p1 = center + ax * 1000
                    p2 = center - ax * 1000
                    mat = cv2.line(mat, (int(p1.real), int(p1.imag)), (int(p2.real), int(p2.imag)), clr, 2)
                mat = cv2.circle(mat, (int(center.real), int(center.imag)), 10, (0, 255, 0), 2)
            else:
                shm.bins_status.cover_visible.set(False)
        self.post('lines', mat)
    
if __name__ == '__main__':
    BinsCover('downward', opts)()
