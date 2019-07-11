from math import radians, atan2, sqrt, sin, cos, atan

import cv2
import numpy as np
from vision.modules.base import ModuleBase
from vision.options import *
import math
import cmath
import shm
import itertools

#static_img = cv2.imread('path1.png')
static_img = None
opts = [
        #IntOption('l_min', 0, 0, 255),
        #IntOption('l_max', 255, 0, 255),
        #IntOption('a_min', 0, 0, 255),
        #IntOption('a_max', 255, 0, 255),
        #IntOption('b_min', 0, 0, 255),
        #IntOption('b_max', 255, 0, 255),
        IntOption('l_trg', 75, 0, 255),
        IntOption('a_trg', 201, 0, 255),
        IntOption('b_trg', 183, 0, 255),
        #IntOption('l_trg', 78, 0, 255), # 129
        #IntOption('a_trg', 171, 0, 255), # 201
        #IntOption('b_trg', 173, 0, 255), # 183
        IntOption('houghness', 154, 0, 1000),
        IntOption('canny1', 47, 0, 1000), # 25
        IntOption('canny2', 66, 0, 1000), # 93
        #IntOption('canny1', 25, 0, 1000),
        #IntOption('canny2', 93, 0, 1000),
        IntOption('d_thresh', 31, 0, 255), # 128
        IntOption('angle_min', 125, 90, 180),
        IntOption('angle_max', 145, 90, 180),
        IntOption('circle_x', 864//2, 0, 1500),
        IntOption('circle_y', 600//2, 0, 1500),
        IntOption('circle_rad', 800//2, 0, 1500),
]

kernel = np.ones((5, 5), np.uint8)

def uv_norm(s):
    return s / abs(s)

class Path(ModuleBase):
    def process(self, mat):
        mat = cv2.resize(mat, (mat.shape[1]//2,mat.shape[0]//2))
        indices_y, indices_x = np.indices(mat.shape[:-1])
        cmsk = (indices_y - self.options['circle_y']) ** 2 + \
              (indices_x - self.options['circle_x']) ** 2 < \
              (self.options['circle_rad'] ** 2)
        cmsk = cmsk.astype(np.uint8) * 255

        self.post('cmsk', cmsk)#cmsk.astype(np.uint8) * 255)
        self.post('img', mat)
        if static_img is not None: mat = static_img.copy()
        mat = cv2.GaussianBlur(mat,(13,13),0)
        cc = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB).astype(np.int16)
        z = np.abs(cc[:,:,0] - self.options['l_trg']) # + \
        #    np.abs(cc[:,:,1] - self.options['a_trg']) + \
        #    np.abs(cc[:,:,2] - self.options['b_trg'])
        #z //= 3
        np.clip(z, 0, 255, out=z)
        z = z.astype(np.uint8)
        t2 = z < self.options['d_thresh']
        centr_y = np.mean(indices_y[t2])
        centr_x = np.mean(indices_x[t2])
        near_target = cv2.dilate(t2.astype(np.uint8) * 255, kernel, iterations=2) & cmsk
        self.post('dist', z)
        edg = cv2.Canny(z, self.options['canny1'], self.options['canny2'], apertureSize=3)
        edg2 = cv2.Canny(near_target, self.options['canny1'], self.options['canny2'], apertureSize=3)
        edg = cv2.dilate(edg, kernel)
        edg2 = cv2.dilate(edg2, kernel)
        self.post('edg2', edg2)
        edg &= edg2
        edg = cv2.dilate(edg, kernel)
        #edg = cv2.erode(edg, kernel)
        self.post('near_target', near_target)
        edg &= near_target
        self.post('edg', edg)
        lines = cv2.HoughLines(edg, 1, np.pi/180, self.options['houghness'])
        #print(lines)
        if lines is None or len(lines) < 2:
            if np.sum(t2) < .01 * mat.shape[1] * mat.shape[0]:
                shm.path_results.num_lines.set(-1)
            else:
                shm.path_results.num_lines.set(0)
                shm.path_results.center_x.set((centr_x / mat.shape[1]) - .5)
                shm.path_results.center_y.set((centr_y - mat.shape[0] / 2) / mat.shape[1])
            shm.path_results.visible_1.set(False)
            shm.path_results.visible_2.set(False)
            return
        #print(lines)
        lines[lines[:,0,0] < 0,:,1] += np.pi
        lines[:,:,0] = np.abs(lines[:,:,0])
        pkv = np.exp(1j * lines[:,:,1])
        #kvalues = (pkv ** 2).astype(np.complex64).view(np.float32)
        kvalues = lines[:,0,0]
        #kvalues = kv1
        #kvalues = np.hstack((kv1, lines[:,:,0] / 100))
        #kvalues = kvalues[:,np.newaxis,:]
        #print(kvalues)
        criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
        ret, label, center = cv2.kmeans(kvalues, 2, None, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)
        s = np.int64(sorted(range(2), key=lambda i: center[i]))#math.atan2(center[i][1], center[i][0])))
        center = center[s]
        label = s[label]
        #print(center)
        label = label[:,0]
        ll = []
        clrs = [(0, 0, 255), (0, 255, 0), (255, 0, 0), (0, 255, 255), (255, 255, 0), (255, 0, 255), (0, 0, 128), (0, 128, 0), (128, 0, 0)]
        superlabels = label * 2
        supercenters = []
        m2 = mat.copy()
        avg, = (center[0] + center[1]) / 2
        avg_vect = np.mean(pkv)
        print(avg, avg_vect)
        superlabels = list(superlabels)
        superlabels.append(8)
        d2 = (1j, mat.shape[0] / 2)
        try:
            center = np.linalg.solve(np.complex64([avg_vect / abs(avg_vect), d2[0] / abs(d2[0])]).view(np.float32).reshape(2,-1), [[avg], [d2[1]]])[:,0].astype(np.float64)#.view(np.complex64)[0]
        except np.linalg.linalg.LinAlgError:
            print('singular matrix')
            return
        print(center)
        m2 = cv2.circle(mat, (int(center[0]), int(center[1])), 10, (255, 255, 255), 2)

        for i, ((r, th),) in enumerate(itertools.chain(lines, (((avg, cmath.log(avg_vect).imag),),))):
            #print(r, th)
            
            dr = np.exp(1j*th)
            cent = dr * r
            drx = dr * 1j
            #print(cent, end='\t')
            p1 = cent + 1000 * drx
            p2 = cent + -1000 * drx
            #print(p1, p2)
            #print(label[i])
            m2 = cv2.line(m2, (int(p1.real), int(p1.imag)), (int(p2.real), int(p2.imag)), clrs[superlabels[i]], 2)
        self.post('m2', m2)
        shm.path_results.center_x.set((center[0] / mat.shape[1]) - .5)
        shm.path_results.center_y.set((center[1] - mat.shape[0] / 2) / mat.shape[1])
        shm.path_results.angle_1.set(cmath.log(avg_vect * 1j).imag)
        return
        #for i in range(2):
        #    matches = lines[label == i,:,0]
        #    if len(matches) < 2: return
        #    r2, l2, c2 = cv2.kmeans(matches, 2, None, criteria, 10, cv2.KMEANS_RANDOM_CENTERS)
        #    s2 = np.int64(sorted(range(2), key=lambda i: c2[i,0]))
        #    c2 = c2[s2]
        #    l2 = s2[l2]
        #    for j, in c2:
        #        supercenters.append((np.mean(pkv[label == i,0], axis=0), j))
        #    superlabels[label == i] += l2[:,0]

        #supercenters = np.float32(supercenters)
        #d1 = (uv_norm(supercenters[0][0] + supercenters[1][0]), (supercenters[0][1] + supercenters[1][1]) / 2)
        #d2 = (uv_norm(supercenters[2][0] + supercenters[3][0]), (supercenters[2][1] + supercenters[3][1]) / 2)
        d1, d2 = ((np.mean(pkv[label == i,0]), np.mean(lines[label == i,:,0])) for i in range(2))
        d1, d2 = ((a / abs(a), b) for a, b in (d1, d2))
        #print(d1, d2)

        ddot = d1[0].real * d2[0].real + d1[0].imag * d2[0].imag
        if ddot < 0:
            d1 = -d1[0], -d1[1]#((d1[1] + math.pi) % (2*math.pi))
            ddot *= -1
        try:
            center = np.linalg.solve(np.complex64([d1[0] / abs(d1[0]), d2[0] / abs(d2[0])]).view(np.float32).reshape(2,-1), [[d1[1]], [d2[1]]])[:,0].astype(np.float64)#.view(np.complex64)[0]
        except np.linalg.linalg.LinAlgError:
            print('singular matrix')
            if np.sum(t2) < .01 * mat.shape[1] * mat.shape[0]:
                shm.path_results.num_lines.set(-1)
            else:
                shm.path_results.num_lines.set(0)
                shm.path_results.center_x.set((centr_x / mat.shape[1]) - .5)
                shm.path_results.center_y.set((centr_y - mat.shape[0] / 2) / mat.shape[1])
            shm.path_results.visible_1.set(False)
            shm.path_results.visible_2.set(False)
            return
        shm.path_results.center_x.set((center[0] / mat.shape[1]) - .5)
        shm.path_results.center_y.set((center[1] - mat.shape[0] / 2) / mat.shape[1])
        #print(center)
        d3 = (uv_norm(d1[0] + d2[0]), (d1[1] + d2[1]) / 2)
        d3 = d3[0], np.dot(center, np.complex64([d3[0]]).view(np.float32))
        try:
            mat = cv2.circle(mat, (int(center[0]), int(center[1])), 10, (255, 0, 0), 2)
        except (ValueError, OverflowError):
            print('overflow')
            if np.sum(t2) < .01 * mat.shape[1] * mat.shape[0]:
                shm.path_results.num_lines.set(-1)
            else:
                shm.path_results.num_lines.set(0)
            shm.path_results.visible_1.set(False)
            shm.path_results.visible_2.set(False)
            return
        msk2 = (indices_y * d3[0].imag + indices_x * d3[0].real) < d3[1]
        #self.post('msk2', msk2.astype(np.uint8) * 255)
        try:
            angle = math.acos(-ddot)
        except ValueError:
            angle = 0
        if not ((self.options['angle_min'] * math.pi / 180) < angle < (self.options['angle_max'] * math.pi / 180)):
            mat[0:20,0:20] = (0, 0, 255)
            print(angle * 180 / math.pi)
            if angle < .2:
                shm.path_results.num_lines.set(1)
            else:
                shm.path_results.num_lines.set(0)
        else:
            shm.path_results.num_lines.set(2)
            #self.post('mat', mat)
            #return
        #print(center)
        #d3 = center, (d1[1] + d2[1])

        # get intersection, split on middle of narrow wedge 
        side = near_target[msk2].sum() > near_target[~msk2].sum()
        norm = -d3[0] if side else d3[0]
        def k(x):
            return x.real * norm.real + x.imag * norm.imag
        v1 = d1[0] * 1j#(1j if side else -1j)
        v2 = d2[0] * 1j#(-1j if side else 1j)
        v1 = max((v1, -v1), key=k)
        v2 = max((v2, -v2), key=k)
        v1, v2 = (v1, v2) if np.cross(*np.complex128([[v1], [v2]]).view(np.float64)) > 0 else (v2, v1)
        #v1, v2 = sorted((v1, v2), key=lambda x: -x.imag)
        shm.path_results.visible_1.set(True)
        shm.path_results.visible_2.set(True)
        #c1, c2 = (x.conjugate() * -1j for x in (v1, v2))
        c1, c2 = v1, v2
        shm.path_results.angle_1.set(cmath.log(c1).imag)
        shm.path_results.angle_2.set(cmath.log(c2).imag)

            
        print(center)
        Ccenter = center.view(np.complex128)[0]
        for i, v in enumerate((v1, v2)):
            #print(Ccenter, v)
            ds = Ccenter + v * 200
            #mat = cv2.line(mat, (int(Ccenter.real), int(Ccenter.imag)), (int(ds.real), int(ds.imag)), clrs[i], 2)
            mat = cv2.circle(mat, (int(ds.real), int(ds.imag)), 10, clrs[i*2+1], 2)

        for i, (v, dst) in enumerate((d1, d2, d3)):
            v /= abs(v)
            ctr = v * dst
            vc = v * 1j
            p1 = ctr + 1000 * vc
            p2 = ctr - 1000 * vc
            mat = cv2.line(mat, (int(p1.real), int(p1.imag)), (int(p2.real), int(p2.imag)), clrs[i], 2)
        #for i, (v, dst) in enumerate(supercenters):
        #    v /= abs(v)
        #    ctr = v * dst
        #    vc = v * 1j
        #    p1 = ctr + 1000 * vc
        #    p2 = ctr - 1000 * vc
        #    m2 = cv2.line(m2, (int(p1.real), int(p1.imag)), (int(p2.real), int(p2.imag)), clrs[i], 2)
        #print(supercenters)
        #print(angles)
        #l2 = np.complex64([np.exp(2j * th) for (r, th), in lines]
        #dirs = [dir ** 2 for cent, dir in ll]

        self.post('mat', mat)

if __name__ == '__main__':
    Path('downward', opts)()
