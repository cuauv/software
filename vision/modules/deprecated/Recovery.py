from collections import namedtuple
import pickle
from math import sin, cos, radians
import os
import time

import cv2
import numpy as np

import shm
from vision.modules import ModuleBase, gui_options

capture_source = 'downward'

vision_options = [gui_options.BooleanOption('debugging', True), gui_options.IntOption('min_rect_area', 1000, 0, 30000),
                  gui_options.IntOption('train_lab_a_min', 53, 0, 255), gui_options.IntOption('train_lab_a_max', 88, 0, 255),
                  gui_options.IntOption('train_yuv_u_min', 0, 0, 255), gui_options.IntOption('train_yuv_u_max', 98, 0, 255),
                  gui_options.IntOption('train_hls_h_min', 60, 0, 255), gui_options.IntOption('train_hls_h_max', 104, 0, 255),


                  gui_options.IntOption('car_lab_b_min', 41, 0, 255), gui_options.IntOption('car_lab_b_max', 150, 0, 255),
                  gui_options.IntOption('car_yuv_v_min', 107, 0, 255), gui_options.IntOption('car_yuv_v_max', 210, 0, 255),
                  gui_options.IntOption('car_hls_h_min', 64, 0, 255), gui_options.IntOption('car_hls_h_max', 128, 0, 255),


                  gui_options.IntOption('rail_l_min', 0, 0, 255), gui_options.IntOption('rail_l_max', 73, 0, 255),
                  gui_options.IntOption('rail_a_min', 114, 0, 255), gui_options.IntOption('rail_a_max', 152, 0, 255),
                  gui_options.IntOption('rail_b_min', 0, 0, 255), gui_options.IntOption('rail_b_max', 166, 0, 255)]


def distance(pt1, pt2):
    return hypot(pt1[0] - pt2[0], pt1[1] - pt2[1])

def center(rect):
    return rect[0] + rect[2] / 2, rect[1] + rect[3] / 2

def point_in_rect(point, rect):
    return point[0] > rect[0] and point[1] > rect[1] and point[0] < rect[0] + rect[2] and point[1] < rect[1] + rect[3]

class Recovery(ModuleBase.ModuleBase):
    def __init__(self):
        super(Recovery, self).__init__(True)

    def find_train(self, mat, color_splits):
        lab_split = color_splits['lab']
        yuv_split = color_splits['yuv']
        hls_split = color_splits['hls']
        
        train_yuv_uthreshed = cv2.inRange(yuv_split[1], self.options['train_yuv_u_min'], self.options['train_yuv_u_max'])
        train_lab_athreshed = cv2.inRange(lab_split[1], self.options['train_lab_a_min'], self.options['train_lab_a_max'])
        train_hls_hthreshed = cv2.inRange(hls_split[0], self.options['train_hls_h_min'], self.options['train_hls_h_max'])
        train_threshed = train_yuv_uthreshed & train_lab_athreshed & train_hls_hthreshed
        
        train_eroded = cv2.erode(train_threshed, np.ones((10, 10)))
        train_dilated = cv2.dilate(train_eroded, np.ones((100, 100)))

        if self.options['debugging']:
            self.post('train_lab_athreshed', train_lab_athreshed)
            self.post('train_yuv_uthreshed', train_yuv_uthreshed)
            self.post('train_hls_hthreshed', train_hls_hthreshed)
            self.post('train_threshed', train_threshed)
            self.post('train_eroded', train_eroded.copy())
            self.post('train_dilated', train_dilated.copy())

        _, train_threshed_contours, train_threshed_hierarchy = cv2.findContours(train_eroded, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

        if train_threshed_hierarchy is None:
            print('Cannot find any train contours in the picture')
            shm.recovery_results.train_heuristic_score.set(0)
            return
        
        _, train_dilated_contours, train_dilated_hierarchy = cv2.findContours(train_dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

        
        train_dilated_contour_areas = map(lambda (i, x): [cv2.contourArea(x), i, cv2.boundingRect(x), x],
                                   enumerate(train_dilated_contours))
        train_bounding_contour = max(train_dilated_contour_areas, key=lambda x: x[0])
        
        train_threshed_contours = filter(lambda x: point_in_rect(center(cv2.boundingRect(x)), train_bounding_contour[2]), train_threshed_contours)

        
        if self.options['debugging']:
            all_contours_drawing = np.copy(mat)
            cv2.drawContours(all_contours_drawing, [c for c in train_threshed_contours if cv2.contourArea(c)], -1,
                (255, 255, 0), 2)
            cv2.drawContours(all_contours_drawing, [c for c in train_dilated_contours if cv2.contourArea(c)], -1,
                (255, 0, 0), 2)
            self.post('train_threshed_contours', all_contours_drawing)

        if self.options['debugging']:
            all_contours_drawing = np.copy(mat)
            cv2.drawContours(all_contours_drawing, [c for c in train_threshed_contours if cv2.contourArea(c)], -1,
                (255, 255, 0), 2)
            cv2.drawContours(all_contours_drawing, [c for c in train_dilated_contours if cv2.contourArea(c)], -1,
                (255, 0, 0), 2)
            self.post('train_threshed_contours', all_contours_drawing)
            
        train_threshed_contour_areas = map(lambda (i, x): [cv2.contourArea(x), i, cv2.boundingRect(x), x],
                                           enumerate(train_threshed_contours))

        train_threshed_contour_areas.sort(key=lambda x: x[0], reverse=True)
        train_threshed_contour_areas = train_threshed_contour_areas[:4]
        
        train_threshed_contour_areas_by_x = sorted(train_threshed_contour_areas, key=lambda x: center(x[2])[0])
        biggest_block_index_by_x, biggest_block = max(enumerate(train_threshed_contour_areas_by_x), key=lambda x: x[1][0])
        biggest_block_rotated_rect = list(cv2.minAreaRect(biggest_block[3]))

        if biggest_block_rotated_rect[1][0] >  biggest_block_rotated_rect[1][1]:
            biggest_block_rotated_rect[1] = biggest_block_rotated_rect[1][1], biggest_block_rotated_rect[1][0]
            biggest_block_rotated_rect[2] += 90
    
        train_threshed_contour_areas_by_y = sorted(train_threshed_contour_areas, key=lambda x: center(x[2])[1])
        biggest_block_index_by_y, _ = max(enumerate(train_threshed_contour_areas_by_y), key=lambda x: x[1][0])
        
        vertical = False
        
        if len(train_threshed_contour_areas) == 4:
            if biggest_block_index_by_x > 1:
                biggest_block_rotated_rect[2] += 180
                
            if biggest_block_index_by_y > 1:
                vertical = True
        elif len(train_threshed_contour_areas) == 3:
            if biggest_block_index_by_x == 2:
                biggest_block_rotated_rect[2] += 180
            elif biggest_block_index_by_x == 1:
                if train_threshed_contour_areas_by_x[0][0] < train_threshed_contour_areas_by_x[2][0]:
                    biggest_block_rotated_rect[2] += 180
                    
            if biggest_block_index_by_y == 2:
                vertical = True
            elif biggest_block_index_by_y == 1:
                if train_threshed_contour_areas_by_y[0][0] < train_threshed_contour_areas_by_y[2][0]:
                    vertical = True
        elif len(train_threshed_contour_areas) == 2:
            if biggest_block_index_by_x == 1:
                biggest_block_rotated_rect[2] += 180
                
            if biggest_block_index_by_y == 1:
                vertical = True

        biggest_block_rotated_rect[2] %= 360
        biggest_block_rotated_rect[2] = 360 - biggest_block_rotated_rect[2]

        if 65 < biggest_block_rotated_rect[2] < 115 and not vertical:
            biggest_block_rotated_rect[2] += 180
        elif 245 < biggest_block_rotated_rect[2] < 294 and vertical:
            biggest_block_rotated_rect[2] -= 180
        
        
        if self.options['debugging']:
            arrow_distance = 200
            angle = 360 - biggest_block_rotated_rect[2]
            arrow_x_dist = int(arrow_distance * cos(radians(angle)))
            arrow_y_dist = int(arrow_distance * sin(radians(angle)))
            
            arrow_start = tuple(map(int, biggest_block_rotated_rect[0]))
            arrow_end = (arrow_start[0] + arrow_x_dist, arrow_start[1] + arrow_y_dist)
            
            cv2.arrowedLine(all_contours_drawing, arrow_start, arrow_end, (255, 0, 0), 4)
    
        shm.recovery_results.train_center_x.set(int(biggest_block_rotated_rect[0][0]))
        shm.recovery_results.train_center_y.set(int(biggest_block_rotated_rect[0][1]))
        shm.recovery_results.train_angle.set(biggest_block_rotated_rect[2])
        shm.recovery_results.train_heuristic_score.set(train_bounding_contour[0])
        
        
    def find_car(self, mat, color_splits):
        lab_split = color_splits['lab']
        yuv_split = color_splits['yuv']
        hls_split = color_splits['hls']
        
        car_lab_bthreshed = cv2.inRange(lab_split[2], self.options['car_lab_b_min'], self.options['car_lab_b_max'])
        car_yuv_vthreshed = cv2.inRange(yuv_split[2], self.options['car_yuv_v_min'], self.options['car_yuv_v_max'])
        car_hls_hthreshed = cv2.inRange(hls_split[0], self.options['car_hls_h_min'], self.options['car_hls_h_max'])
        car_threshed = car_yuv_vthreshed & car_lab_bthreshed & car_hls_hthreshed
        
        car_eroded = cv2.erode(car_threshed, np.ones((10, 10)))
        car_dilated = cv2.dilate(car_eroded, np.ones((100, 100)))

        if self.options['debugging']:
            self.post('car_lab_bthreshed', car_lab_bthreshed)
            self.post('car_yuv_vthreshed', car_yuv_vthreshed)
            self.post('car_hls_hthreshed', car_hls_hthreshed)
            self.post('car_threshed', car_threshed)
            self.post('car_eroded', car_eroded.copy())
            self.post('car_dilated', car_dilated.copy())

        _, car_threshed_contours, car_threshed_hierarchy = cv2.findContours(car_eroded, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

        if car_threshed_hierarchy is None:
            print('Cannot find any car contours in the picture')
            shm.recovery_results.car_heuristic_score.set(0)
            return
        
        _, car_dilated_contours, car_dilated_hierarchy = cv2.findContours(car_dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        
        if self.options['debugging']:
            all_contours_drawing = np.copy(mat)
            cv2.drawContours(all_contours_drawing, [c for c in car_threshed_contours if cv2.contourArea(c)], -1,
                (255, 255, 0), 2)
            cv2.drawContours(all_contours_drawing, [c for c in car_dilated_contours if cv2.contourArea(c)], -1,
                (255, 0, 0), 2)
            self.post('car_threshed_contours', all_contours_drawing)

        
        car_dilated_contour_areas = map(lambda (i, x): [cv2.contourArea(x), i, cv2.boundingRect(x), x],
                                   enumerate(car_dilated_contours))
        car_bounding_contour = max(car_dilated_contour_areas, key=lambda x: x[0])
        
        car_threshed_contours = filter(lambda x: point_in_rect(center(cv2.boundingRect(x)), car_bounding_contour[2]), car_threshed_contours)
        
        car_threshed_contour_areas = map(lambda (i, x): [cv2.contourArea(x), i, cv2.boundingRect(x), x],
                                           enumerate(car_threshed_contours))
        
        car_threshed_contour_areas.sort(key=lambda x: x[0], reverse=True)
        car_threshed_contour_areas = car_threshed_contour_areas[:4]
        
        car_threshed_contour_areas_by_x = sorted(car_threshed_contour_areas, key=lambda x: center(x[2])[0])
        biggest_block_index_by_x, biggest_block = max(enumerate(car_threshed_contour_areas_by_x), key=lambda x: x[1][0])
        biggest_block_rotated_rect = list(cv2.minAreaRect(biggest_block[3]))

        if biggest_block_rotated_rect[1][0] >  biggest_block_rotated_rect[1][1]:
            biggest_block_rotated_rect[1] = biggest_block_rotated_rect[1][1], biggest_block_rotated_rect[1][0]
            biggest_block_rotated_rect[2] += 90
    
        car_threshed_contour_areas_by_y = sorted(car_threshed_contour_areas, key=lambda x: center(x[2])[1])
        biggest_block_index_by_y, _ = max(enumerate(car_threshed_contour_areas_by_y), key=lambda x: x[1][0])
        
        vertical = False
        if len(car_threshed_contour_areas) == 2:
            if biggest_block_index_by_x == 1:
                biggest_block_rotated_rect[2] += 180
                
            if biggest_block_index_by_y == 1:
                vertical = True
        elif len(car_threshed_contour_areas) == 1:
            shm.recovery_results.car_heuristic_score.set(0)
            return

        biggest_block_rotated_rect[2] %= 360
        biggest_block_rotated_rect[2] = 360 - biggest_block_rotated_rect[2]

        if 65 < biggest_block_rotated_rect[2] < 115 and not vertical:
            biggest_block_rotated_rect[2] += 180
        elif 245 < biggest_block_rotated_rect[2] < 294 and vertical:
            biggest_block_rotated_rect[2] -= 180

        
        if self.options['debugging']:
            arrow_distance = 200
            angle = 360 - biggest_block_rotated_rect[2]
            arrow_x_dist = int(arrow_distance * cos(radians(angle)))
            arrow_y_dist = int(arrow_distance * sin(radians(angle)))
            
            arrow_start = tuple(map(int, biggest_block_rotated_rect[0]))
            arrow_end = (arrow_start[0] + arrow_x_dist, arrow_start[1] + arrow_y_dist)
            
            cv2.arrowedLine(all_contours_drawing, arrow_start, arrow_end, (255, 0, 0), 4)
            
        shm.recovery_results.car_center_x.set(int(biggest_block_rotated_rect[0][0]))
        shm.recovery_results.car_center_y.set(int(biggest_block_rotated_rect[0][1]))
        shm.recovery_results.car_angle.set(biggest_block_rotated_rect[2])
        shm.recovery_results.car_heuristic_score.set(car_bounding_contour[0])

            
    def find_rail(self, mat, color_splits):
        lab_split = color_splits['lab']
        rail_lthreshed = cv2.inRange(lab_split[0], self.options['rail_l_min'], self.options['rail_l_max'])
        rail_athreshed = cv2.inRange(lab_split[1], self.options['rail_a_min'], self.options['rail_a_max'])
        rail_bthreshed = cv2.inRange(lab_split[2], self.options['rail_b_min'], self.options['rail_b_max'])
        rail_threshed = rail_lthreshed & rail_athreshed & rail_bthreshed
        
        rail_eroded = cv2.erode(rail_threshed, np.ones((5, 5)))

        if self.options['debugging']:
            self.post('rail_lthreshed', rail_lthreshed)
            self.post('rail_athreshed', rail_athreshed)
            self.post('rail_bthreshed', rail_bthreshed)
            self.post('rail_threshed', rail_threshed)
            self.post('rail_eroded', rail_eroded.copy())

        _, rail_threshed_contours, rail_threshed_hierarchy = cv2.findContours(rail_eroded, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

        if rail_threshed_hierarchy is None:
            print('Cannot find any rail contours in the picture')
            shm.recovery_results.rail_heuristic_score.set(0)
            return
        rail_threshed_hierarchy = rail_threshed_hierarchy[0]

        if self.options['debugging']:
            all_contours_drawing = np.copy(mat)
            cv2.drawContours(all_contours_drawing, [c for c in rail_threshed_contours if cv2.contourArea(c)], -1,
                (255, 255, 0), 2)
            self.post('rail_threshed_contours', all_contours_drawing)
        
        rail_threshed_contour_areas = map(lambda (i, x): [cv2.contourArea(x), i, cv2.boundingRect(x), x], enumerate(rail_threshed_contours))

        def contour_heuristic(index):
            heuristic_value = 0
            
            child_index = rail_threshed_hierarchy[index][2]
            while child_index >= 0:
                rotated_rect = cv2.minAreaRect(rail_threshed_contours[child_index])
                rotated_rect_area = rotated_rect[1][0] * rotated_rect[1][1]
                if rotated_rect_area == 0:
                    child_index = rail_threshed_hierarchy[child_index][0]
                    continue
                heuristic_value += rail_threshed_contour_areas[child_index][0] * ((rail_threshed_contour_areas[child_index][0] / rotated_rect_area) ** 5)
                child_index = rail_threshed_hierarchy[child_index][0]
                
            return heuristic_value

        contour_indices = []
        curr_index = 0
        while curr_index >= 0:
            contour_indices.append(curr_index)
            curr_index = rail_threshed_hierarchy[curr_index][0]

        largest_index = max(contour_indices, key=contour_heuristic)

        if self.options['debugging']:
            cv2.drawContours(all_contours_drawing, [rail_threshed_contours[largest_index]], -1, (255, 0, 0), 4)

        min_rect = cv2.minAreaRect(rail_threshed_contour_areas[largest_index][3])
        shm.recovery_results.rail_center_x.set(int(min_rect[0][0]))
        shm.recovery_results.rail_center_y.set(int(min_rect[0][1]))
        if min_rect[1][0] < min_rect[1][1]:
            shm.recovery_results.rail_angle.set(min_rect[2])
        else:
            shm.recovery_results.rail_angle.set(min_rect[2] + 90)
        shm.recovery_results.rail_heuristic_score.set(contour_heuristic(largest_index))


    def process(self, mat):
        self.post('orig', mat)
        lab_image = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        lab_split = cv2.split(lab_image)
        yuv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2YUV)
        yuv_split = cv2.split(yuv_image)
        hls_image = cv2.cvtColor(mat, cv2.COLOR_BGR2HLS)
        hls_split = cv2.split(hls_image)
        color_splits = {'lab': lab_split, 'yuv': yuv_split, 'hls': hls_split}
        self.find_train(mat, color_splits)
        self.find_car(mat, color_splits)
        self.find_rail(mat, color_splits)
