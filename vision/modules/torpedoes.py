#!/usr/bin/env python3

import time

import cv2
import shm
import numpy as np

from vision.modules.base import ModuleBase
from vision import options
from math import hypot
from collections import namedtuple
from enum import Enum
from itertools import permutations

import pickle
import math
import os

from sklearn.cluster import KMeans

SAVE_MOMENTS = False

class TorpedoesMode(Enum):
    board_inital = 0
    board_subsequent = 1
    track_target = 2

class CutoutSize(Enum):
    def __int__(self):
        return self.value

    def __str__(self):
        return self.name.title()

    small = 0
    large = 1

class CutoutName(Enum):
    top_left = 0
    top_right = 1
    bottom_left = 2
    bottom_right = 3

class Cutout:
    def __init__(self, *, contour, area, bounding_rect, center, min_area_rect, polygon):
        self.contour = contour
        self.area = area
        self.bounding_rect = bounding_rect
        self.center = center
        self.min_area_rect = min_area_rect
        self.polygon = polygon

class LocatedCutout:
    def __init__(self, *, group=None, name, cutout=None, 
                 avg_color_dist=0, avg_color=0, covered=False,
                 expected_board_offset, name_value):
        self.group = group
        self.name = name
        self.name_value = name_value
        self.track = []
        self.cutout = cutout
        self.avg_color_dist = avg_color_dist
        self.avg_color = avg_color
        self.covered = covered
        self.expected_board_offset = expected_board_offset
        self.visible = False
        self.diff_from_small = 0
        self.diff_from_large = 0
        self.size = 0
        self.letter_count = [[], [], [], []]
        self.letter_hist = []
        self.letter = None
        self.hu_moments = None
        self.unskewed_rect = None

class CutoutLayout:
    def __init__(self):
        self.top_left = \
            LocatedCutout(name="Top Left", expected_board_offset=(-0.5, -0.5), 
                          name_value=CutoutName.top_left.value)
        self.top_right = \
            LocatedCutout(name="Top Right", expected_board_offset=(0.5, -0.5),
                          name_value=CutoutName.top_right.value)
        self.bottom_left = \
            LocatedCutout(name="Bottom Left", expected_board_offset=(-0.5, 0.5),
                          name_value=CutoutName.bottom_left.value)
        self.bottom_right = \
            LocatedCutout(name="Bottom Right", expected_board_offset=(0.5, 0.5),
                          name_value=CutoutName.bottom_right.value)
        self.located_cutouts = [self.top_left, self.top_right, 
                                self.bottom_left, self.bottom_right]
        self.visible_cutouts = []



def label_rect(img, rect, text, color=(0,0,0)):
    text_origin = (rect[0], rect[1] - 10)
    scale = 0.00127890625 * img.shape[0]
    cv2.putText(img, text, text_origin, cv2.FONT_HERSHEY_SIMPLEX, scale, (0,0,0), thickness=3)
    cv2.putText(img, text, text_origin, cv2.FONT_HERSHEY_SIMPLEX, scale, (255,255,255))

def distance(pt1, pt2):
    return hypot(pt1[0] - pt2[0], pt1[1] - pt2[1])

options = [options.IntOption('board_blocksize', 901, 0, 2000, lambda x: x % 2 == 1),
           options.IntOption('board_C_u', 5, -255, 255), 
           options.IntOption('board_C_b', -5, -255, 255),
           options.IntOption('cutouts_blocksize', 501, 0, 2000, lambda x: x % 2 == 1),
           options.IntOption('cutouts_C', 0, -255, 255),
           options.IntOption('black_blocksize', 255, 0, 2000, lambda x: x % 2 == 1),
           options.IntOption('black_C', 45, -255, 255),
           options.IntOption('min_area', 200),
           options.IntOption('board_morph_iter', 1, 1, 10),
           options.IntOption('board_morph_size', 9, 0, 20),
           options.DoubleOption('board_min_area', 0.02, 0, 1.0),
           options.DoubleOption('board_min_poly_score', 0.7, 0, 1.0),
           options.IntOption('blur_size',  11, 1, 255, lambda x: x % 2 == 1),
           options.DoubleOption('min_circularity', .35, 0, 150),
           options.DoubleOption('heuristicPower', 15),
           options.IntOption('ideal_height', 510, 0, 1020),
           options.BoolOption('verbose', True),
           options.DoubleOption('min_cutout_area', 0.005, 0, 1.0),
           options.DoubleOption('min_cutout_rect', 0.8, 0, 1.0),
           options.DoubleOption('max_cutout_color_dist', 5.0, 0, 20.00),
           options.IntOption('max_target_accel', 100, 0, 100),
           options.BoolOption('post', True),
           options.DoubleOption("poly_epsilon_gain", 0.07, 0, 1.0),
           options.DoubleOption("min_board_aspect_ratio", 0.8, 0, 1.0),
           options.DoubleOption("max_board_aspect_ratio", 5.0, 0, 10.0)
           ]


class Torpedoes(ModuleBase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.vsp_data = None
        self.times = [] 
        self.cutout_layout = CutoutLayout()

        file_dir = os.path.dirname(__file__) 
        filename = "moments.p"
        path = os.path.join(file_dir, filename)

        self.moments = pickle.load(open(path, "rb"))
        self.avg_colors = []
        self.kmeans = KMeans(n_clusters = 2)

    def post_if_enabled(self, name, img):
        if self.options["post"]:
            self.post(name, img)
    
    SplitImage = namedtuple('SplitImage', ['lab', 'yuv', 'hls'])
    Board = namedtuple('Board', ['bounding_rect', 'center', 'height', 'width',
                       'skew', 'skew_valid', 'contour', 'area', 'outline',
                       'unskewed', 'unskew_mat'])
    target = None
    prev_target_dist = 0
    first_track_target = False
    prev_mode = -1
    letter_for_id = ['S', 'N', 'E', 'W']
    id_for_letter = {'S': 0, 'N': 1, 'E': 2, 'W': 3}
    avg_covered_color = None
    avg_uncovered_color = None
    prev_board_visible = []

    def scale(self, val):
        return int(val * self.img_height)
    
    def get_split_image(self, mat):
        lab_image = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        lab_split = cv2.split(lab_image)
        yuv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2YUV)
        yuv_split = cv2.split(yuv_image)
        hls_image = cv2.cvtColor(mat, cv2.COLOR_BGR2HLS)
        hls_split = cv2.split(hls_image)

        return self.SplitImage(lab=lab_split, yuv=yuv_split, hls=hls_split)

    def find_yellow_contours(self, split_image):
        lab_bthreshed_board = cv2.adaptiveThreshold(split_image.lab[2], 255, 
            cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, 
            self.options["board_blocksize"], self.options["board_C_b"])
        yuv_uthreshed_board = cv2.adaptiveThreshold(split_image.yuv[2], 255, 
            cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, 
            self.options["board_blocksize"], self.options["board_C_u"])
        yuv_uthreshed_board = cv2.bitwise_not(yuv_uthreshed_board)
        finalThreshed = lab_bthreshed_board & yuv_uthreshed_board

        # Erode and dilate thresholded images
        morph_size = self.options["board_morph_size"]
        erode_element = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (morph_size * 2 + 1, morph_size * 2 + 1),
                                                  (morph_size, morph_size))
        dilate_element = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (morph_size * 2 + 1, morph_size * 2 + 1),
                                                   (morph_size, morph_size))
        eroded = cv2.erode(finalThreshed,erode_element, iterations = self.options["board_morph_iter"])
        finalThreshed = cv2.dilate(eroded, dilate_element, iterations = self.options["board_morph_iter"])
        finalThreshed = cv2.dilate(finalThreshed, dilate_element, iterations = self.options["board_morph_iter"])
        finalThreshed = cv2.erode(finalThreshed, erode_element, iterations = self.options["board_morph_iter"])

        self.post_if_enabled('yellow_lab_bthreshed', lab_bthreshed_board)
        self.post_if_enabled('yellow_yuv_uthreshed', yuv_uthreshed_board)
        self.post_if_enabled('yellow_binary_image', finalThreshed)

        _, contours, hierarchy = cv2.findContours(np.copy(finalThreshed), 
            cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

        return contours, hierarchy

    def poly_fit(self, c):
        min_area_rect = cv2.minAreaRect(c)
        epsilon = max([min_area_rect[1][0], min_area_rect[1][1]]) * self.options["poly_epsilon_gain"]
        poly = cv2.approxPolyDP(cv2.convexHull(c), epsilon, True) 
        # poly = cv2.approxPolyDP(c, epsilon, True) 
        return poly

    def num_edge_points(self, poly):
        lines = [((98, 0), (0, 95), True), ((1502, 0), (1600, 93), True), ((0, 840), (282, 1200), False), ((1600, 840), (1317, 1200), False)]

        # print(poly.shape[0])
        num_edge_points = 0
        for arr in poly:
            point = tuple(arr[0])
            on_edge = False
            if (point[0] <= 5 or point[0] >= self.img_width - 6 or point[1] <= 5 
                or point[1] >= self.img_height - 6):
                num_edge_points += 1
                cv2.circle(self.poly_drawing, point, self.scale(0.006), (0, 0, 255), thickness=self.scale(0.0083))
                continue

            for idx, line in enumerate(lines):
                x = point[0]
                m = float(line[1][1] - line[0][1]) / float(line[1][0] - line[0][0])
                y = m * (x - line[0][0]) + line[0][1]
              
                inside = False
                if y > point[1]:
                    inside = False
                else:
                    inside = True

                if line[2]:
                    inside = not inside

                if inside:
                    num_edge_points += 1
                    cv2.circle(self.poly_drawing, point, self.scale(0.006), (0,0,0), thickness=self.scale(0.0083))
                    on_edge = True
                    break

            if not on_edge:
                cv2.circle(self.poly_drawing, point, self.scale(0.006), (0, 255, 0), thickness=self.scale(0.0083))

        return num_edge_points

    def contour_center(self, contour):
        M = cv2.moments(contour)
        cx = int(M['m10']/M['m00'])
        cy = int(M['m01']/M['m00'])
        
        return (cx, cy)

    def find_board(self, yellow_contours):
        self.poly_drawing = np.copy(self.mat)
        def valid_board_candidate(c):
            poly = self.poly_fit(c)
            # poly = cv2.approxPolyDP(c, 30, True) 
            poly_area = cv2.contourArea(poly)
            cv2.drawContours(self.poly_drawing, [poly], -1, (255,0,0), 2)
            if poly_area == 0.0:
                return False

            # If part of the polygon approx is cut off, note that it could
            # still be a 4 sided polygon, ignore the side that is cut off
            num_edge_points = self.num_edge_points(poly)

            cutoff_edges = (num_edge_points + 1) / 2
            # cutoff_edges = 0
            # cutoff_edges = (num_edge_points + 1) / 2

            rel_area = poly_area / self.img_area

            bounding_rect = cv2.boundingRect(c)
            if bounding_rect[2] == 0:
                aspect_ratio = 10.0
            else:
                aspect_ratio = bounding_rect[3] / bounding_rect[2]

            poly_score = cv2.contourArea(c) / poly_area
            return (poly.shape[0] >= 4 and poly.shape[0] <= 4 + cutoff_edges and 
                   poly_score > self.options["board_min_poly_score"] and 
                   rel_area > self.options["board_min_area"] and
                   aspect_ratio > self.options["min_board_aspect_ratio"] and
                   aspect_ratio < self.options["max_board_aspect_ratio"])

        self.post_if_enabled('poly', self.poly_drawing)

        yellow_contours = [c for c in yellow_contours if valid_board_candidate(c) ]

        if len(yellow_contours) < 1:
            self.prev_board_visible.append(False)
            if len(self.prev_board_visible) > 5:
                del self.prev_board_visible[0]

            return None
        else:
            self.prev_board_visible.append(True)
            if len(self.prev_board_visible) > 5:
                del self.prev_board_visible[0]

            
            for b in self.prev_board_visible:
                if not b:
                    return None
  
        # The largest contour is the torpedoes board, more robust logic to be added
        # Hack to make lower contours preferred
        board_contour_index, board_contour = max(enumerate(yellow_contours), 
            key=lambda x: cv2.contourArea(x[1]) + 10*self.contour_center(x[1])[1])

        mask = np.zeros((self.mat.shape[0], self.mat.shape[1]), dtype=np.uint8)
        board_hull = self.poly_fit(board_contour)

        cv2.drawContours(mask, [board_hull], -1, (255,255,255), -1)
        self.post_if_enabled('board_mask', mask)

        # Find bounding rectangle of board contour, to be used in skew
        # calculation and determining "center" of board
        # Format is list of x, y, width, height in that order
        bounding_rect = cv2.boundingRect(board_contour)

        # Find positions of the corners clockwise from top left
        corners = [(bounding_rect[0], bounding_rect[1]), 
                   (bounding_rect[0] + bounding_rect[2], bounding_rect[1]),
                   (bounding_rect[0] + bounding_rect[2], bounding_rect[1] + bounding_rect[3]),
                   (bounding_rect[0], bounding_rect[1] + bounding_rect[3])]

        # List of four points corresponding to corners of the bounding rectangle
        board_center = (int(bounding_rect[0] + bounding_rect[2]/2), 
                        int(bounding_rect[1] + bounding_rect[3]/2))

        self.results.board_center_x = board_center[0]
        self.results.board_center_y = board_center[1]

        # Report board height, used as heursitic to determine distance to the board
        self.results.board_height = bounding_rect[3]
        board_width = bounding_rect[2]
        
        # Draw bounding rectangle with dot at the center (for debugging)
        cv2.drawContours(self.final_contour, [np.asarray(corners)], -1, (255, 255, 255), 2)
        cv2.circle(self.final_contour, board_center, 2, (255, 255, 255), thickness=self.scale(0.0016))
        cv2.drawContours(self.final_contour, [board_contour], -1, (0, 255, 255), 2)

        board_poly = self.poly_fit(board_contour)
        skew_str = ""
        outline = None
        unskew_mat = None
        unskewed = None
        if board_poly.shape[0] == 4:
            top_left_index, top_left = min(enumerate(board_poly), 
                                         key=lambda c: distance(c[1][0], (0, 0)))

            top_right_index = (top_left_index + 1) % 4
            bottom_right_index = (top_left_index + 2) % 4
            bottom_left_index = (top_left_index + 3) % 4

            top_left = tuple(top_left[0])
            top_right = tuple(board_poly[top_right_index][0])
            bottom_right = tuple(board_poly[bottom_right_index][0])
            bottom_left = tuple(board_poly[bottom_left_index][0])

            left_side = distance(top_left, bottom_left)
            right_side = distance(top_right, bottom_right)

            if left_side > right_side:
                self.results.board_skew = -((left_side / right_side) - 1.0) * 100
            else:
                self.results.board_skew = ((right_side / left_side) - 1.0) * 100

            cv2.line(self.final_contour, top_left, bottom_left, (255,0,0), 4)
            cv2.line(self.final_contour, top_right, bottom_right, (0,255,0), 4)

            self.results.skew_valid = True
            skew_str = str(self.results.board_skew)
            
            outline = [board_poly[top_left_index][0], board_poly[top_right_index][0], 
                       board_poly[bottom_right_index][0], board_poly[bottom_left_index][0]]

            # Get perspective transformation
            board_square = np.float32([[0,0],[self.UNSKEWED_SIDE_LEN,0],[self.UNSKEWED_SIDE_LEN,self.UNSKEWED_SIDE_LEN],[0,self.UNSKEWED_SIDE_LEN]])
            board_skewed = np.float32(outline)

            unskew_mat = cv2.getPerspectiveTransform(board_skewed, board_square)
            unskewed = cv2.warpPerspective(self.mat, unskew_mat, (self.UNSKEWED_SIDE_LEN, self.UNSKEWED_SIDE_LEN), flags=cv2.INTER_LINEAR)
        else:
            self.results.skew_valid = False
            skew_str = "Unknown"

        text = "Skew: {}".format(skew_str)
        img = self.final_contour
        text_origin = (10, 30)
        cv2.putText(img, text, text_origin, cv2.FONT_HERSHEY_SIMPLEX, 1.0, (0,0,0), thickness=self.scale(0.005))
        cv2.putText(img, text, text_origin, cv2.FONT_HERSHEY_SIMPLEX, 1.0, (255,255,255))

        board_area = cv2.contourArea(board_contour)

        return self.Board(center=board_center, bounding_rect=bounding_rect, 
               height=bounding_rect[3], width=bounding_rect[2], skew=self.results.board_skew, 
               skew_valid=self.results.skew_valid, contour=board_contour, area=board_area,
               outline=outline, unskewed=unskewed, unskew_mat=unskew_mat)

    def find_red_contours(self, split_image, post_name):
        binary_img_red = cv2.adaptiveThreshold(split_image.lab[1], 255, 
            cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, 
            self.options["cutouts_blocksize"], self.options["cutouts_C"])
         
        # Erode and dilate to remove noise
        kernel = np.ones((5,5), np.uint8)
        eroded = cv2.erode(binary_img_red, kernel, iterations = 1)
        binary_img_red = cv2.dilate(eroded, kernel, iterations = 1)
        
        self.post_if_enabled(post_name, binary_img_red)

        _, contours, _ = cv2.findContours(np.copy(binary_img_red), cv2.RETR_EXTERNAL, 
                                          cv2.CHAIN_APPROX_SIMPLE)

        return contours
    
    def find_cutout_candidates(self, red_contours):
        self.candidates_drawing = np.copy(self.mat)
        
        # Eliminate some contours based on how much they look like cutouts
        cutout_candidates = []
        for contour in red_contours:
            cutout_bounding_rect = cv2.boundingRect(contour)
            area = cv2.contourArea(contour)
        
            min_area_rect = cv2.minAreaRect(contour)
            cutout_center = (int(min_area_rect[0][0]), int(min_area_rect[0][1]))
        
            box = np.int0(cv2.boxPoints(min_area_rect))
            rectangularity = area / cv2.contourArea(box)
            
            epsilon = max([min_area_rect[1][1], min_area_rect[1][0]]) * 0.1
            polygon = cv2.approxPolyDP(contour, epsilon, True)
         
            if self.mode == TorpedoesMode.track_target.value:
                min_rect = 0.5
            else:
                min_rect = self.options["min_cutout_rect"]

            if rectangularity > min_rect: # and area / self.img_area > self.options["min_cutout_area"]:
            # if polygon.shape[0] == 4:
                cutout_candidates.append(Cutout(contour=contour, area=area,
                    bounding_rect=cutout_bounding_rect, center=cutout_center, 
                    min_area_rect=min_area_rect, polygon=polygon))
                cv2.drawContours(self.candidates_drawing, [contour], -1, (255, 255, 255), 3)
                # cv2.drawContours(self.candidates_drawing, [polygon], -1, (255,255,255), 2)

        self.post_if_enabled('cutout candidates', self.candidates_drawing)

        return cutout_candidates
         
    SMALL_CUTOUT_RELATIVE_AREA = 0.0212
    LARGE_CUTOUT_RELATIVE_AREA = 0.0625
    def find_cutouts_in_board(self, split_image, board, cutout_candidates):
        # Copy the list so we don't modify it when tracking is going on
        # Or we could just not call this function when we're tracking...
        cutout_candidates = list(cutout_candidates)

        valid_cutout_candidate = lambda c: (cv2.pointPolygonTest(board.contour, c.center, False) == 1.0 and
            c.area / board.area > self.options["min_cutout_area"])
        cutout_candidates = [c for c in cutout_candidates if valid_cutout_candidate(c)]
        
        # Find the four candidates most likely to be cutouts  based on their expected 
        # positions on the board 
        located_cutouts = []
        self.cutout_layout.visible_cutouts = []
        small_cutouts = []
        large_cutouts = []
        for loc_cutout in self.cutout_layout.located_cutouts:
            o = (loc_cutout.expected_board_offset[0] * board.bounding_rect[2] / 2, 
                 loc_cutout.expected_board_offset[1] * board.bounding_rect[3] / 2)
            expected_center = (int(board.center[0] + o[0]), int(board.center[1] + o[1])) 
             
            cv2.circle(self.final_contour, expected_center, int(board.width / 8), (255,255, 255), 
                      thickness=self.scale(0.003))
            
            if len(cutout_candidates) == 0:
                print("Ran out of cutout candidates!")
                loc_cutout.visible = False
                loc_cutout.group.visible =  False
                continue
            
            # Select the candidate closest to the expected position
            cutout_index, cutout = min(enumerate(cutout_candidates), 
                                      key=lambda c: distance(c[1].center, expected_center))
            loc_cutout.cutout = cutout
            loc_cutout.visible = True
            loc_cutout.group.visible = True
            self.cutout_layout.visible_cutouts.append(loc_cutout)

            # Reset covered flag, will be properly set later
            loc_cutout.covered = False

            del cutout_candidates[cutout_index]

            # Create mask for mean color calculation
            mask = np.zeros((self.mat.shape[0], self.mat.shape[1]), dtype=np.uint8)
            cutout_hull = cv2.convexHull(cutout.contour)
            cv2.drawContours(mask, [cutout_hull], -1, (255,255,255), -1)
            
            # Calculate mean color
            mean_a = cv2.mean(split_image.lab[1], mask)
            # mean_b = cv2.mean(split_image.lab[2], mask)
            mean_b = [0]
            loc_cutout.avg_color = (mean_a[0], mean_b[0])

            self.avg_colors.append(loc_cutout.avg_color)
            if len(self.avg_colors) > 20:
                del self.avg_colors[0]
         
            # Draw cutout in red with white dot in center
            center_int = (int(cutout.center[0]), int(cutout.center[1]))
            cv2.circle(self.final_contour, center_int, 2, (0, 0, 255), thickness=2)

            cv2.drawContours(self.final_contour, [cutout.contour], -1, (0, 0, 255), 2)

            # Area of cutout relative to board
            relative_area = loc_cutout.cutout.area / board.area
            loc_cutout.diff_from_small = abs(relative_area - self.SMALL_CUTOUT_RELATIVE_AREA)
            loc_cutout.diff_from_large = abs(relative_area - self.LARGE_CUTOUT_RELATIVE_AREA)

            loc_cutout.group.x = loc_cutout.cutout.center[0]
            loc_cutout.group.y = loc_cutout.cutout.center[1]
            loc_cutout.group.width = int(loc_cutout.cutout.min_area_rect[1][0])
            loc_cutout.group.height = int(loc_cutout.cutout.min_area_rect[1][1])

            if loc_cutout.diff_from_small < loc_cutout.diff_from_large:
                small_cutouts.append(loc_cutout)
                loc_cutout.size = CutoutSize.small
            else:
                large_cutouts.append(loc_cutout)
                loc_cutout.size = CutoutSize.large

            loc_cutout.group.size = loc_cutout.size
            
            if (loc_cutout.cutout.polygon is not None and 
                loc_cutout.cutout.polygon.shape[0] == 4 and
                board.unskew_mat is not None):
                poly = np.float32(loc_cutout.cutout.polygon)
                poly_transformed = cv2.perspectiveTransform(poly, board.unskew_mat)
                loc_cutout.unskewed_rect = cv2.boundingRect(poly_transformed)

        # Enforce the condidtion that there must be at most 2 large and 2 small cutouts
        if len(large_cutouts) > 2:
            while len(large_cutouts) > 2:
                small_cutout = min(large_cutouts, key=lambda c: c.diff_from_small)
                large_cutouts.remove(small_cutout)
                small_cutout.size = CutoutSize.small
        if len(small_cutouts) > 2:
            while len(small_cutouts) > 2:
                large_cutout = min(small_cutouts, key=lambda c: c.diff_from_large)
                small_cutouts.remove(large_cutout)
                large_cutout.size = CutoutSize.large

        for loc_cutout in self.cutout_layout.visible_cutouts:
            # Draw cutout names
            loc_cutout.group.size = loc_cutout.size.value
            label_rect(self.final_contour, loc_cutout.cutout.bounding_rect, loc_cutout.name + " (" + str(loc_cutout.size) + ")")

    def identify_cover(self):
        if len(self.avg_colors) < 2:
            print("Not enough observations to identify the cover!")
            return

        # Assume one of the cutouts is covered, find which one
        X = np.array(self.avg_colors)
        labels = self.kmeans.fit_predict(X)

        colors = np.zeros_like(self.mat)
        cluster_size = [0,0]
        for idx, c in enumerate(self.avg_colors):
            cluster_size[labels[idx]] += 1

        cover_cluster, _ = min(enumerate(cluster_size), key=lambda x: x[1])

        for idx, c in enumerate(self.avg_colors):
            if labels[idx] == cover_cluster:
                color = (0, 165, 255)
            else:
                color = (255, 0, 0)

            cv2.circle(colors, (int(c[0] * (self.mat.shape[0] / 255)), int(c[1] * (self.mat.shape[1] / 255))), 2, color, thickness=2)
        
        if self.options["post"]:
            self.post_if_enabled('cutout avg colors', colors )

        self.avg_covered_color = self.kmeans.cluster_centers_[cover_cluster]
        self.avg_uncovered_color = self.kmeans.cluster_centers_[(cover_cluster + 1) % 2]
    
    def assume_no_cover(self):
        for c in self.cutout_layout.located_cutouts:
            c.covered = False
            c.group.covered = False
        
    def classify_cutouts(self, assume_cover_present):
        if self.avg_covered_color is None or self.avg_uncovered_color is None:
            print("Can't classify cutouts when clustering hasn't been done!")
            return

        covered_layouts = [
            [True,  False, False, False],
            [False, True,  False, False],
            [False, False, True,  False],
            [False, False, False, True ]
        ]
        uncovered_layout = [[False, False, False, False]]
              
        if assume_cover_present:
            possible_layouts = covered_layouts
        else:
            possible_layouts = covered_layouts + uncovered_layout

        errors = [0] * len(possible_layouts) 
        for l_idx, l in enumerate(possible_layouts):
            for c_idx, c in enumerate(self.cutout_layout.located_cutouts):
                if not c.visible:
                    continue

                if l[c_idx]:
                    expected = self.avg_covered_color
                else:
                    expected = self.avg_uncovered_color

                errors[l_idx] += distance(c.avg_color, expected)

        layout_idx, _ = min(enumerate(errors), key=lambda x: x[1])
        correct_layout = possible_layouts[layout_idx]

        for i, c in enumerate(self.cutout_layout.visible_cutouts):
            c.covered = correct_layout[i]
            c.group.covered = c.covered

            if c.covered:
                cv2.drawContours(self.final_contour, [c.cutout.contour], -1, (0, 165, 255), 4)

    def percent_diff(self, y2, y1):
        diff =  (abs(float(y2) - float(y1)) / y1) * 100.0
        # print("Percent diff", diff)
        return diff
    
    BOARD_HEIGHT_TO_LETTER_BOX_HEIGHT = 0.098
    BOARD_WIDTH_TO_LETTER_BOX_WIDTH = 0.11
    BOARD_SIDE_INCHES = 48
    SMALL_CUTOUT_SIDE_INCHES = 7
    LARGE_CUTOUT_SIDE_INCHES = 12
    SMALL_CUTOUT_TO_LETTER_BOX_WIDTH = (BOARD_SIDE_INCHES / SMALL_CUTOUT_SIDE_INCHES) * BOARD_WIDTH_TO_LETTER_BOX_WIDTH
    LARGE_CUTOUT_TO_LETTER_BOX_WIDTH = (BOARD_SIDE_INCHES / LARGE_CUTOUT_SIDE_INCHES) * BOARD_WIDTH_TO_LETTER_BOX_WIDTH
    SMALL_CUTOUT_TO_LETTER_BOX_HEIGHT = (BOARD_SIDE_INCHES / SMALL_CUTOUT_SIDE_INCHES) * BOARD_HEIGHT_TO_LETTER_BOX_HEIGHT
    LARGE_CUTOUT_TO_LETTER_BOX_HEIGHT = (BOARD_SIDE_INCHES / LARGE_CUTOUT_SIDE_INCHES) * BOARD_HEIGHT_TO_LETTER_BOX_HEIGHT
    def track_target(self, cutout_candidates, split_image):
        if self.target is None or self.target.cutout is None:
            print("Trying to track target but there is no target set!")
            return 
        
        self.cutout_track = np.copy(self.mat)
          
        image_diagonal = distance((0,0), (self.img_width, self.img_height))
        # print("Image Diagonal", image_diagonal)
        
        if len(cutout_candidates) == 0:
            for cutout in self.cutout_layout.located_cutouts:
                cutout.group.visible = False
            print("Trying to track cutouts but there are no cutout candidates!")
             
        if len(cutout_candidates) == 0:
            #print("Ran out of cutout candiates!")
            pass
             
        # Find the cutout candidate which is most likely to be the located cutout
        closest_dist = image_diagonal 
        # print("Image diagonal is ", image_diagonal)
        #print('Previous Target dist', self.prev_target_dist)
        match = None
        for cutout in cutout_candidates:
            dist = distance(self.target.cutout.center, cutout.center)
            #print("Distance is", dist)
                
            if (dist > closest_dist):
                #print("No closer than next best,", closest_dist, "continuing")
                continue
                
            if (abs(dist - self.prev_target_dist) > self.options["max_target_accel"] and not self.first_track_target):
                 #print("Moved too far, continuing search")
                 continue
              
            if (self.percent_diff(cutout.min_area_rect[1][0], self.target.group.width) < 20):
                #print("Found match better than previous best")
                match = cutout
                closest_dist = dist
                self.prev_target_dist = dist
            else:
                #print("Size changed by too much, continuing search")
                continue
             
        if match == None:
            #print("Can't find match for {} cutout".format(self.target.name))
            self.target.group.visible = False
            return
        else:
            #print("Done searching, best dist", closest_dist)
            self.prev_target_dist = closest_dist
        
        # Update internal cutout model
        self.target.cutout = match
        
        # Update shm
        g = self.target.group
        g.x = match.center[0]
        g.y = match.center[1]
        g.width = int(match.min_area_rect[1][0])
        g.height = int(match.min_area_rect[1][1])
        g.visible = True
         
        self.target.track.append(match.center)

         
        # The cutout track can be at most 10 points long
        if len(self.target.track) > 10:
            del self.target.track[0]
         
        label_rect(self.cutout_track, match.bounding_rect, 
                   self.target.name + "(" + str(CutoutSize(self.target.group.size)) + ")")
         
        # Draw minimum area enclosing rectangle contour
        box = cv2.boxPoints(match.min_area_rect)
        box = np.int0(box)
        cv2.drawContours(self.cutout_track, [box], -1, (0,0,255), 2)
        
        # Draw red track
        cv2.polylines(self.cutout_track, [np.int32(self.target.track)], False, (0, 0, 255), thickness=2)
         
        # Draw white dot at center
        cv2.circle(self.cutout_track, (match.center[0], match.center[1]), 2, (255, 255, 255), thickness=2)

        # Classify the letter above the cutout
        if self.target.size == CutoutSize.small:
            wl = g.width * self.SMALL_CUTOUT_TO_LETTER_BOX_WIDTH
            hl = g.height * self.SMALL_CUTOUT_TO_LETTER_BOX_HEIGHT
        elif self.target.size == CutoutSize.large:
            wl = g.width * self.LARGE_CUTOUT_TO_LETTER_BOX_WIDTH
            hl = g.height * self.LARGE_CUTOUT_TO_LETTER_BOX_HEIGHT
        else:
            self.post_if_enabled('target track', self.cutout_track)
            print("Invalid cutout size!")
            return
            
        xl = int(g.x - (wl / 2))
        yl = int(g.y - (g.height / 2) - hl)
        hl = int(hl)
        wl = int(wl)
         
        mask = np.zeros_like(split_image.lab[0])
        cv2.rectangle(mask, (xl,yl), (xl+wl,yl+hl), (255,255,255), -1)

        binary_img_black = cv2.adaptiveThreshold(split_image.lab[0], 255, 
            cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, 
            self.options["black_blocksize"], self.options["black_C"])

        masked = binary_img_black & mask

        _, contours, _ = cv2.findContours(masked, cv2.RETR_EXTERNAL, 
                                          cv2.CHAIN_APPROX_SIMPLE)


        if len(contours) == 0:
            print("Failed to threshold for letter in expected location")
            self.post_if_enabled('target track', self.cutout_track)
            return

        letter_contour = max(contours, key=cv2.contourArea)
        cv2.drawContours(self.cutout_track, [letter_contour], -1, (255, 255, 255), 2)

        moments = cv2.moments(letter_contour)
        hu_moments = cv2.HuMoments(moments)
            
        if self.direction is None:
            print("Can't classify letter during tracking because direction is invalid!")
        else:
            valid_letter_ids = [self.id_for_letter[l] for l in self.direction]
            errors = [[letter_id, 0] for letter_id in valid_letter_ids]
            for error in errors:
                letter_id = error[0]
                error[1] = self.compare_hu_moments(hu_moments, self.moments[letter_id])
                                                        
            letter_id, _ = min(errors, key=lambda x: x[1])
                                                                
            self.target.letter_hist.append(letter_id)
                
            if len(self.target.letter_hist) > 10:
                del self.target.letter_hist[0]
              
            # Find most common letter id in list
            count = [0] * 4
            for e in self.target.letter_hist:
                count[e] += 1
            best_letter_id, _ = max(enumerate(count), key=lambda x: x[1])
             
            letter = self.letter_for_id[best_letter_id]
            cv2.putText(self.cutout_track, letter, (g.x, g.y), cv2.FONT_HERSHEY_SIMPLEX, 2, (0,0,0), thickness=6)

            g.letter = letter.encode('utf-8')

        self.post_if_enabled('target track', self.cutout_track)

    def find_black_contours(self, split_image):
        binary_img_black = cv2.adaptiveThreshold(split_image.lab[0], 255, 
            cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, 
            self.options["black_blocksize"], self.options["black_C"])

        
        self.post_if_enabled('black_binary_image', binary_img_black)

        _, contours, _ = cv2.findContours(np.copy(binary_img_black), cv2.RETR_EXTERNAL, 
                                          cv2.CHAIN_APPROX_SIMPLE)

        return contours
    
    def get_possible_letter_layouts(self):
        # Bad way to handle invalid direction...
        if self.direction is None:
            print("Got invalid direction. Is the mission running??")
            return list(permutations(['N', 'W', 'S', 'E'])) 
             
        layouts = [[
                self.direction[0], self.direction[1], 
                self.direction[0], self.direction[1]
            ], [
                self.direction[1], self.direction[1],
                self.direction[0], self.direction[0]
            ], [
                self.direction[0], self.direction[0],
                self.direction[1], self.direction[1]
            ], [
                self.direction[1], self.direction[0],
                self.direction[1], self.direction[0]
            ]
        ]

        valid_layouts = []
        known_letters = self.settings.known_letters.decode('utf-8')
        for layout in layouts:
            for idx, letter in enumerate(layout):   
                if known_letters[idx] != "?" and known_letters[idx] != layout[idx]:
                   continue 
            valid_layouts.append(layout)

        return valid_layouts

    def compare_hu_moments(self, hu1, hu2):
        error = 0
        for i in range(0, 7):
            m1 = hu1[i][0]
            m2 = hu2[i][0]
             
            if m1 != 0.0:
                m1 = math.log(abs(m1))
            if m2 != 0.0:
                m2 = math.log(abs(m2))
             
            error += (m1 - m2)**2
        return error
            
    def classify_letters(self, board):
        unskewed = board.unskewed
        unskewed_split = self.get_split_image(unskewed)
        
        binary_img_black = cv2.adaptiveThreshold(unskewed_split.lab[0], 255, 
            cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, 
            self.options["black_blocksize"], self.options["black_C"])
        
        # ret2,binary_img_black = cv2.threshold(unskewed_split.lab[0],0,255,cv2.THRESH_BINARY+cv2.THRESH_OTSU)
        # binary_img_black = cv2.bitwise_not(binary_img_black)
            
        # Transform the cutout polygons
        polys = []
        letter_contours = []
        moments_dict = {}

        cutouts = []
        layouts = self.get_possible_letter_layouts()
        
        if len(layouts) == 0:
            print("No possible letter layouts! This should never happen! Reporting no knowledge of the letters to mission.")
            for loc_cutout in self.cutout_layout.located_cutouts:
                loc_cutout.letter = None
                loc_cutout.group.letter_visible = False
            return

        layout_errors = [0] * len(layouts)
        for loc_cutout in self.cutout_layout.visible_cutouts:
            if loc_cutout.cutout.polygon.shape[0] == 4:
                rect = loc_cutout.unskewed_rect
                x,y,w,h = rect
                cv2.rectangle(unskewed,(x,y),(x+w,y+h),(255,255,255),2)

                hl = int(self.UNSKEWED_SIDE_LEN * self.BOARD_HEIGHT_TO_LETTER_BOX_HEIGHT)
                wl = int(self.UNSKEWED_SIDE_LEN * self.BOARD_WIDTH_TO_LETTER_BOX_WIDTH)
                xl = int(x + (w/2) - (wl/2))
                yl = int(y - hl -1)

                mask = np.zeros_like(binary_img_black)
                cv2.rectangle(mask,(xl,yl),(xl+wl,yl+hl),(255,255,255),-1)

                masked = binary_img_black & mask

                _, contours, _ = cv2.findContours(masked, cv2.RETR_EXTERNAL, 
                                                  cv2.CHAIN_APPROX_SIMPLE)

                if len(contours) == 0:
                    print("Failed to threshold for letter in expected location")
                    continue

                letter_contour = max(contours, key=cv2.contourArea)
                letter_contours.append(letter_contour)
        
                moments = cv2.moments(letter_contour)
                hu_moments = cv2.HuMoments(moments)
         
                if SAVE_MOMENTS:
                    moments_dict[loc_cutout.name_value] = hu_moments
         
                cutouts.append(loc_cutout)
                loc_cutout.hu_moments = hu_moments
            else:
                print("Cutout cannot be approximated as a quadrilateral, so cannot find the letter")
          
        for layout_idx, layout in enumerate(layouts):
            layout_error = 0
            for loc_cutout in cutouts:
                letter_from_layout = layout[loc_cutout.name_value]
                letter_id = self.id_for_letter[letter_from_layout]
              
                hu1 = loc_cutout.hu_moments
                hu2 = self.moments[letter_id]
              
                layout_error += self.compare_hu_moments(hu1, hu2)
                
            layout_errors[layout_idx] = layout_error
        
        correct_layout_idx, _ = min(enumerate(layout_errors), key=lambda x: x[1])
        correct_layout = layouts[correct_layout_idx]
         
        for loc_cutout in self.cutout_layout.located_cutouts:
            o = (int(loc_cutout.expected_board_offset[0] * self.UNSKEWED_SIDE_LEN / 2 + self.UNSKEWED_SIDE_LEN / 2), 
                 int(loc_cutout.expected_board_offset[1] * self.UNSKEWED_SIDE_LEN / 2 + self.UNSKEWED_SIDE_LEN / 2))
            
            letter = correct_layout[loc_cutout.name_value]
            
            loc_cutout.letter = letter
            loc_cutout.group.letter = bytes(letter, encoding='utf-8')
            loc_cutout.group.letter_visible = True
            
            cv2.putText(unskewed, letter, o, cv2.FONT_HERSHEY_SIMPLEX, 2, (0,0,0), thickness=6)
        
        if SAVE_MOMENTS:
            pickle.dump(moments_dict, open("moments.p", "wb"))

             
        cv2.drawContours(unskewed, letter_contours, -1, (255, 255, 255), 2)
            
        self.post_if_enabled('unskewed', unskewed)
             
    def print_fps(self):
        self.times.insert(0, time.time())
        while time.time() - self.times[-1] > 5:
            self.times.pop()
            print('fps: {}'.format(len(self.times) / 5.))

    def process(self, mat):
        self.mat = mat
        self.final_contour = np.copy(self.mat)

        self.UNSKEWED_SIDE_LEN = min(mat.shape[0], mat.shape[1])
            
        # Report the fps to the terminal
        # self.print_fps()
        
        self.img_width = len(mat[0])
        self.img_height = len(mat)
        self.img_area = self.img_width * self.img_height
        
        self.results = shm.torpedoes_results.get()
        self.results_cutout_top_left = shm.torpedoes_cutout_top_left.get()
        self.results_cutout_top_right = shm.torpedoes_cutout_top_right.get()
        self.results_cutout_bottom_left = shm.torpedoes_cutout_bottom_left.get()
        self.results_cutout_bottom_right = shm.torpedoes_cutout_bottom_right.get()
        self.settings = shm.torpedoes_settings.get()
        self.fill_single_camera_direction(self.results)

        # Initalize cuout groups
        self.cutout_layout.top_left.group = self.results_cutout_top_left
        self.cutout_layout.top_right.group = self.results_cutout_top_right
        self.cutout_layout.bottom_left.group = self.results_cutout_bottom_left
        self.cutout_layout.bottom_right.group = self.results_cutout_bottom_right

        # Handle invalid direction
        self.direction = self.settings.direction.decode('utf-8')
        if len(self.direction) != 2:
            self.direction = None
        else:
            for l in self.direction:
                if l != "N" and l != "W" and l != "S" and l != "E":
                    self.direction = None
                    break

        self.post_if_enabled('orignal', mat)

        split_image = self.get_split_image(mat)

        self.mode = self.settings.mode

        red_contours = self.find_red_contours(split_image, "red binary")
        yellow_contours, hierarchy = self.find_yellow_contours(split_image)

        # Draw yellow contours
        yellow_contours_drawing = np.copy(mat)
        cv2.drawContours(yellow_contours_drawing, yellow_contours, -1, (0, 255, 255), 2)
        self.post_if_enabled('yellow contours', yellow_contours_drawing)
        
        cutout_candidates = self.find_cutout_candidates(red_contours)

        if self.options["verbose"]:
            self.post_if_enabled('lab_b', split_image.lab[2])
            self.post_if_enabled('lab_a', split_image.lab[1])
            self.post_if_enabled('lab_l', split_image.lab[0])
        
        if self.mode == TorpedoesMode.board_inital.value:
            board = self.find_board(yellow_contours)
        
            if board == None:
                self.results.board_prob = 0.0
            else:
                self.results.board_prob = 1.0
                self.find_cutouts_in_board(split_image, board, cutout_candidates)
                self.identify_cover()

                if self.settings.cover_initally_present:
                    self.classify_cutouts(assume_cover_present=True)
                else:
                    self.assume_no_cover()

                if board.skew_valid:
                    self.classify_letters(board)
                else:
                    for loc_cutout in self.cutout_layout.located_cutouts:
                        loc_cutout.letter = None
                        loc_cutout.group.letter_visible = False

        elif self.mode == TorpedoesMode.board_subsequent.value:
            board = self.find_board(yellow_contours)
        
            if board == None:
                self.results.board_prob = 0.0
            else:
                self.results.board_prob = 1.0
                self.find_cutouts_in_board(split_image, board, cutout_candidates)

                if self.settings.cover_initally_present:
                    self.classify_cutouts(assume_cover_present=False)
                else:
                    self.assume_no_cover()

        elif self.mode == TorpedoesMode.track_target.value:
            if self.prev_mode != TorpedoesMode.track_target.value:
                self.first_track_target = True

                print("Entering target track")

                # Set these values so they don't get used after tracking exits
                self.results.board_prob = 0.0
                self.results.skew_valid = 0

                # Set the target based on shm
                found = False
                for loc_cutout in self.cutout_layout.located_cutouts:
                    if loc_cutout.name_value == self.settings.target:
                        self.target = loc_cutout
                        self.target.track = []
                        found = True
                if not found:
                    print("Error: Unknown target cutout!")
            else:
                self.first_track_target = False
        
            self.track_target(cutout_candidates, split_image) 
        
        self.prev_mode = self.mode

        self.post_if_enabled('final', self.final_contour)
        shm.torpedoes_results.set(self.results)
        shm.torpedoes_cutout_top_left.set(self.results_cutout_top_left)
        shm.torpedoes_cutout_top_right.set(self.results_cutout_top_right)
        shm.torpedoes_cutout_bottom_right.set(self.results_cutout_bottom_right)
        shm.torpedoes_cutout_bottom_left.set(self.results_cutout_bottom_left)

if __name__ == '__main__':
    Torpedoes('forward', options)()
