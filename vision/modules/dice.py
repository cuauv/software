#!/usr/bin/env python3

# Written by Will Smith.

import traceback
import sys
import math

import cv2
import numpy as np
import shm

from vision.modules.base import ModuleBase
from vision import options

#import slam.slam_util as slam
from will_common import find_best_match

options = [
    options.BoolOption('debug', False),
    options.IntOption('hsv_thresh_c', 35, 0, 100),
    options.IntOption('canny_a', 50, 0, 255),
    options.IntOption('canny_b', 110, 0, 255),
    options.IntOption('contour_area_thresh', 10, 1, 200),
    options.DoubleOption('perim_thresh', 1.5, 1, 10),
    options.DoubleOption('circle_thresh', 3, 1, 10),
    options.DoubleOption('ellipse_thresh', 1.2, 1, 10),
    options.DoubleOption('max_pip_dist_ratio', 4, 0, 10),
    options.DoubleOption('max_pip_radius_ratio', 1.2, 1, 10),
    options.IntOption('min_y', 200, 0, 1024),
]

FORWARD_CAM_WIDTH = shm.camera.forward_width.get()
FORWARD_CAM_HEIGHT = shm.camera.forward_height.get()

PIP_THRESH = 5

class Dice(ModuleBase):
    def post_umat(self, tag, img):
        self.post(tag, cv2.UMat.get(img))

    def kernel(self, size):
        return cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (size * 2 + 1, size * 2 + 1), (size, size))

    def dist(self, p1, p2):
        return np.sqrt((p2[0] - p1[0])**2 + (p2[1] - p1[1])**2)

    def norm_x(self, x):
        return (x - FORWARD_CAM_WIDTH / 2) / (FORWARD_CAM_WIDTH / 2)

    def norm_y(self, y):
        return (y - FORWARD_CAM_HEIGHT / 2) / (FORWARD_CAM_HEIGHT / 2)

    def norm_xy(self, tup):
        x, y = tup
        return (self.norm_x(x), self.norm_y(y))

    def process(self, mat):
        # for simulator
        shm.camera.forward_width.set(mat.shape[1])
        shm.camera.forward_height.set(mat.shape[0])
        global FORWARD_CAM_WIDTH, FORWARD_CAM_HEIGHT
        FORWARD_CAM_WIDTH = mat.shape[1]
        FORWARD_CAM_HEIGHT = mat.shape[0]

        try:
            debug = self.options['debug']

            if debug:
                self.post('raw', mat)

            hsv_h, hsv_s, hsv_v = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2HSV))
            #luv_l, luv_u, luv_v = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2LUV))
            #y_y, y_cr, y_cb = cv2.split(cv2.cvtColor(mat, cv2.COLOR_BGR2YCR_CB))

            if debug:
                self.post('hsv_v', hsv_v)
                #self.post('luv_u', luv_u)
                #self.post('luv_v', luv_v)
                #self.post('y_cr', y_cr)
                #self.post('y_cb', y_cb)

            #hsv_v_thresh = cv2.adaptiveThreshold(hsv_v, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, 99, self.options['hsv_thresh_c'])
            #luv_u_thresh = cv2.adaptiveThreshold(luv_u, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, 99, 5)
            #luv_v_thresh = cv2.adaptiveThreshold(luv_v, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, 199, 5)
            #y_cr_thresh = cv2.adaptiveThreshold(y_cr, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, 99, 5)
            #y_cb_thresh = cv2.adaptiveThreshold(y_cb, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, 11, 5)

            hsv_v_edges = cv2.Canny(hsv_v, self.options['canny_a'], self.options['canny_b'])

            DILATE_VALS = [0, 1]
            ACT_PIP_THRESH = PIP_THRESH * math.sqrt(len(DILATE_VALS))

            # We want to dilate by both 0 and 1
            hsv_v_c = [cv2.dilate(hsv_v_edges, self.kernel(foo)) for foo in DILATE_VALS]
            #luv_u_c = cv2.erode(luv_u_thresh, self.kernel(0))
            #luv_v_c = cv2.erode(luv_v_thresh, self.kernel(0))
            #y_cr_c = cv2.erode(y_cr_thresh, self.kernel(0))
            #y_cb_c = cv2.erode(y_cb_thresh, self.kernel(0))

            if debug:
                for i, bar in enumerate(hsv_v_c):
                    self.post('hsv_v_c_{}'.format(i), bar)
                #self.post('luv_u_c', luv_u_c)
                #self.post('luv_v_c', luv_v_c)
                #self.post('y_cr_c', y_cr_c)
                #self.post('y_cb_c', y_cb_c)

            # Find the dots in hsv_v

            contours = []
            for bar in hsv_v_c:
                contours += cv2.findContours(bar, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)[1]

            dots = []

            # Problem is that this depends on the distance
            # When the dice are really far away the dots are tiny
            min_area = self.options['contour_area_thresh']

            for contour in contours:
                M = cv2.moments(contour)
                center_y = M['m01'] / M['m00'] if M['m00'] != 0 else 0

                if center_y > self.options['min_y']:
                    hull = cv2.convexHull(contour)
                    # Use convex hull to fill in circle, otherwise we just get edge
                    area = cv2.contourArea(hull)

                    # Make sure that we have a mostly-closed contour
                    perim = cv2.arcLength(contour, closed=False)
                    hull_perim = cv2.arcLength(hull, closed=True)
                    if area > min_area and max([perim / hull_perim, hull_perim / perim]) < self.options['perim_thresh']:
                        # Bound with circle
                        x, circ_r = cv2.minEnclosingCircle(contour)
                        circ_area = math.pi * circ_r**2

                        if max(circ_area / area, area / circ_area) < self.options['circle_thresh']:
                            # problems if contour isn't big enough
                            try:
                                # Bound with ellipse
                                (ell_cx, ell_cy), (ell_w, ell_h), theta = cv2.fitEllipse(contour)

                                ell_area = math.pi * ell_w * ell_h / 4

                                if max(ell_area / area, area / ell_area) < self.options['ellipse_thresh']:
                                    dots.append((contour, (ell_cx, ell_cy), (ell_w + ell_h) / 2))
                            except cv2.error:
                                pass

            # Use Hough circles as well

            #circled = mat

            #circles = cv2.HoughCircles(hsv_v, cv2.HOUGH_GRADIENT, 5, 50, param1=100, param2=10, minRadius=3, maxRadius=10)

            #if circles is not None:
            #    for circle in circles[0,:]:
            #        print(circle)
            #        cv2.circle(circled, (circle[0], circle[1]), circle[2], (0, 255, 0), 3)

            dotted = mat.copy()
            density_dots = np.zeros(mat.shape, np.uint8)

            #self.post('circled', circled)

            for dot in dots:
                x, y, w, h = cv2.boundingRect(dot[0])
                cv2.rectangle(dotted, (x, y), (x+w, y+h), 0, thickness=2)

            cv2.drawContours(density_dots, [dot[0] for dot in dots], -1, (255, 255, 255), 3)
            if debug:
                self.post('density_dots', density_dots)
                self.post('dotted', dotted)

            # Group the dots into buckets

            # Each dot on the same die should be close together and have a similar area

            # Start out with all dots in separate groups and then combine them
            groups = [set([(dot[1], dot[2])]) for dot in dots]

            # This iteration avoids checking the same pair twice
            for i, dot1 in enumerate(dots):
                for dot2 in dots[i+1:]:
                    dist = self.dist(dot2[1], dot1[1])
                    radius_ratio = max(dot1[2], dot2[2]) / min(dot1[2], dot2[2])
                    avg_radius = (dot1[2] + dot2[2]) / 2

                    if dist < avg_radius * self.options['max_pip_dist_ratio'] and radius_ratio < self.options['max_pip_radius_ratio']:
                        # They should be in the same group

                        # Find the two groups
                        group1 = None
                        group2 = None
                        for group in groups:
                            if (dot1[1], dot1[2]) in group:
                                group1 = group
                            if (dot2[1], dot2[2]) in group:
                                group2 = group
                            if (group1 is not None) and (group2 is not None):
                                break

                        if group1 != group2:
                            # Combine the groups
                            group1 |= group2
                            groups.remove(group2)

            groups_out = dotted.copy()

            # Sort by size - most dots first
            groups = sorted(groups, key=lambda group: len(group), reverse=True)

            shm_values = []

            SHM_VARS = [shm.dice0, shm.dice1] #, shm.dice2, shm.dice3]

            #count = 0
            for group in groups:
                gcx = int(sum([dot[0] for dot, rad in group]) / len(group))
                gcy = int(sum([dot[1] for dot, rad in group]) / len(group))
                #rad = max([self.dist(dot1, dot2) for dot1 in group for dot2 in group]) / 2
                # This radius is the average of all of the dots' radii
                rad = sum([rad for dot, rad in group]) / len(group)
                dist = None if rad == 0 else 0.11 * FORWARD_CAM_WIDTH / rad # hella sketch, be ware or be dare

                shm_values.append((gcx, gcy, len(group), rad, dist))

                cv2.circle(groups_out, (gcx, gcy), len(group) * 10, (0, 0, 255) if len(group) >= ACT_PIP_THRESH else (0, 0, 0), thickness=(2 * len(group)))
                if len(group) >= ACT_PIP_THRESH:
                    cv2.circle(groups_out, (gcx, gcy), int(rad), (255, 255, 0), thickness=5)
                #if dist is not None:
                #    x, y = slam.sub_to_vision(slam.slam_to_sub(slam.sub_to_slam(slam.vision_to_sub(gcx, gcy, dist, 0))), 0)
                #    count += 1
                #    print(count, x, y)
                #    cv2.circle(groups_out, (int(x), int(y)), 30, (0, 255, 255), thickness=10)

            #print()
            #print('-----')
            #print()

            #print(shm_values)

            if debug:
                self.post('groups_out', groups_out)

            if len(shm_values) < 2:
                # Want at least two values
                shm_values += [None] * (len(SHM_VARS) - min(len(shm_values), len(SHM_VARS)))
                data = shm_values[:2]
            else:
                old_data = [(var.center_x.get(), var.center_y.get()) for var in SHM_VARS]
                new_data = shm_values[:2]

                def comp(new, old):
                    if new[2] < ACT_PIP_THRESH or new is None or old is None:
                        return np.inf
                    # Distance between centers
                    dist = self.dist(self.norm_xy(new[:2]), old[:2])
                    
                    return dist

                # Try to line up the data with the old one for consistency
                # We have no guarantees of alignment from frame to frame, though            
                data = find_best_match(old_data, new_data, comp)

            slam_out = groups_out.copy()

            #for (key, datum) in zip(slam_keys, data):
            #    (cx, cy, count, radius, dist) = datum
            #    if count >= 5:
            #        slam.observe(key, cx, cy, dist, 'f')

            #roulette_coords = slam.sub_to_vision(slam.slam_to_sub(np.array([5 - shm.kalman.north.get(), -6 - shm.kalman.east.get(), 3.5 - shm.kalman.depth.get()])), 0)
            #cv2.circle(slam_out, (int(roulette_coords[0]), int(roulette_coords[1])), 100, (255, 255, 0), thickness=10)

            #self.post('slam_out', slam_out)

            for i, (val, var) in enumerate(zip(data, SHM_VARS)):
                new_var = var.get()

                if val is None:
                    new_var.visible = False
                else:
                    (cx, cy, count, radius, dist) = val

                    #dx, dy, d = slam.request_pos(slam_keys[i])

                    #slammed_coords = slam.request(slam_keys[i], 'f')

                    #cv2.circle(slam_out, (int(slammed_coords[0]), int(slammed_coords[1])), 100, (0, 255, 255), 20)

                    new_var.visible = count >= ACT_PIP_THRESH
                    new_var.center_x = self.norm_x(cx)
                    new_var.center_y = self.norm_y(cy)
                    new_var.count = count
                    new_var.radius = radius
                    new_var.radius_norm = radius / FORWARD_CAM_WIDTH if FORWARD_CAM_WIDTH != 0 else -1

                    #print(dx, dy, d, np.arctan2(dy, dx))

                    #new_var.theta = np.degrees(np.arctan2(dy, dx))
                    #new_var.depth = d
                    # Assuming depth is level already
                    #new_var.dist = np.sqrt(dx**2 + dy**2)

                var.set(new_var)

            cv2.line(slam_out, (int(FORWARD_CAM_WIDTH / 2), 0), (int(FORWARD_CAM_WIDTH / 2), int(FORWARD_CAM_HEIGHT)), (0, 0, 0))
            cv2.line(slam_out, (0, int(FORWARD_CAM_HEIGHT / 2)), (int(FORWARD_CAM_WIDTH), int(FORWARD_CAM_HEIGHT / 2)), (0, 0, 0))

            self.post('slam_out', slam_out)
        except Exception as e:
            traceback.print_exc(file=sys.stdout)
if __name__ == '__main__':
    Dice('forward', options)()
