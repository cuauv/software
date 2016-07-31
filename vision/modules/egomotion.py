#!/usr/bin/env python3

from vision.modules.base import ModuleBase

import shm
import numpy as np
import cv2
from time import clock

lk_params = dict( winSize  = (15, 15),
                  maxLevel = 2,
                  criteria = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT, 10, 0.03))

feature_params = dict( maxCorners = 500,
                       qualityLevel = 0.01,
                       minDistance = 7,
                       blockSize = 7 )

def draw_str(dst, target, s):
    x, y = target
    cv2.putText(dst, s, (x+1, y+1), cv2.FONT_HERSHEY_PLAIN, 1.0, (0, 0, 0), thickness = 2, lineType=cv2.LINE_AA)
    cv2.putText(dst, s, (x, y), cv2.FONT_HERSHEY_PLAIN, 1.0, (255, 255, 255), lineType=cv2.LINE_AA)

class Egomotion(ModuleBase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.vsp_data = None
        self.times = []

        self.track_len = 10
        self.detect_interval = 5
        self.tracks = []
        self.frame_idx = 0
        self.max_depth = 2.85
        self.focal_length = 733.184
        self.prev_time = 0.0
        self.curr_time = 0.0

    def process(self, mat):
        self.curr_time = clock()

        frame_gray = cv2.cvtColor(mat, cv2.COLOR_BGR2GRAY)
        vis = mat.copy()

        if len(self.tracks) > 0:
            img0, img1 = self.prev_gray, frame_gray
            p0 = np.float32([tr[-1] for tr in self.tracks]).reshape(-1, 1, 2)
            p1, st, err = cv2.calcOpticalFlowPyrLK(img0, img1, p0, None, **lk_params)

            # Back track to verify match
            p0r, st, err = cv2.calcOpticalFlowPyrLK(img1, img0, p1, None, **lk_params)
            d = abs(p0-p0r).reshape(-1, 2).max(-1)
            good = d < 1

            new_tracks = []
            bad_indicies = []
            for i, (tr, (x, y), good_flag) in enumerate(zip(self.tracks, p1.reshape(-1, 2), good)):
                # Clip points that fail back-tracking
                if not good_flag:
                    bad_indicies.append(i)
                    continue

                # Clip points that are near the edges
                height, width = img0.shape
                if x < 20 or  x > (width - 20) or y < 20 or y > (height - 20):
                    bad_indicies.append(i)
                    continue

                # Clip points near the center
                #if x > width / 4 and x < (3 * width) / 4 and y > height / 4 and y < (3 * height) / 4:
                #    bad_indicies.append(i)
                #    continue

                tr.append((x, y))
                if len(tr) > self.track_len:
                    del tr[0]

                new_tracks.append(tr)
                cv2.circle(vis, (x, y), 2, (0, 255, 0), -1)

            self.tracks = new_tracks
            cv2.polylines(vis, [np.int32(tr) for tr in self.tracks], False, (0, 255, 0))
            draw_str(vis, (20, 20), 'track count: %d' % len(self.tracks))

            np.delete(p0, bad_indicies, 0)
            np.delete(p1, bad_indicies, 0)
            bad_indicies = []

            # Check if there are more than 5 points detected
            if p0.shape[0] > 5 and p1.shape[0] > 5:
                # Calculate displacement
                s = np.subtract(p1, p0)
                s_av = np.mean(s, axis=0)

                # Replace with real value!
                camera_pos = np.array([0, 0, 0])

                # Calculate x and y displacement of sub (in meters), dx and dy
                dxs = []
                dys = []
                t = 0.0
                for v in s:
                    t = (1) / self.focal_length

                    dx = v[0][0] * t
                    dy = v[0][1] * t

                    dxs.append(dx)
                    dys.append(dy)


                # No outliner detection for now, just average the x and y displacements
                dt = self.curr_time - self.prev_time
                y_vel = sum(dxs) / (len(dxs) * dt)
                x_vel = sum(dys) / (len(dys) * dt)
                # y_vel = median(dxs) / dt
                # x_vel = median(dys) / dt

                shm.egm.x_vel.set(x_vel)
                shm.egm.y_vel.set(y_vel)
                # shm.egm.z_vel.set(t[2][0])

                shm.egm.s_avg_x.set(s_av[0][0])
                shm.egm.s_avg_y.set(s_av[0][1])

        # Detect new features at the specified interval
        if self.frame_idx % self.detect_interval == 0:
            mask = np.zeros_like(frame_gray)
            mask[:] = 255
            for x, y in [np.int32(tr[-1]) for tr in self.tracks]:
                cv2.circle(mask, (x, y), 5, 0, -1)
            p = cv2.goodFeaturesToTrack(frame_gray, mask = mask, **feature_params)
            if p is not None:
                for x, y in np.float32(p).reshape(-1, 2):
                    self.tracks.append([(x, y)])

        self.frame_idx += 1
        self.prev_gray = frame_gray
        self.post('flow', vis)

        self.prev_time = self.curr_time

if __name__ == '__main__':
    Egomotion()()
