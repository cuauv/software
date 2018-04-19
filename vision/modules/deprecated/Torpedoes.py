import os
import time
from math import hypot, sin, cos, radians
import functools
import itertools
import traceback

import shm
import cv2
import numpy as np
import sys
from skimage import color
import skimage
try:
    import cPickle as pickle
except ImportError:
    import pickle

from vision.modules import ModuleBase
import gui_options
from caffew import caffe_classifier

capture_source = 'forward'

vision_options = [gui_options.IntOption('yellow_a_min', 38, 0, 255), gui_options.IntOption('yellow_a_max', 107, 0, 255),
                  gui_options.IntOption('yellow_lu_min', 0, 0, 255), gui_options.IntOption('yellow_lu_max', 111, 0, 255),
                  gui_options.IntOption('yellow_yu_min', 56, 0, 255), gui_options.IntOption('yellow_yu_max', 110, 0, 255),
                  
                  gui_options.IntOption('orangered_lab_b_min', 41, 0, 255), gui_options.IntOption('orangered_lab_b_max', 128, 0, 255),
                  gui_options.IntOption('orangered_yuv_v_min', 127, 0, 255), gui_options.IntOption('orangered_yuv_v_max', 210, 0, 255),
                  gui_options.IntOption('orangered_hls_h_min', 73, 0, 255), gui_options.IntOption('orangered_hls_h_max', 128, 0, 255),
                  
                  gui_options.BooleanOption('debugging', True),
                  gui_options.BooleanOption('write numbers', False)]

results = shm.torpedo_results


def distance(pt1, pt2):
    return hypot(pt1[0] - pt2[0], pt1[1] - pt2[1])


def center(rect):
    return rect[0] + rect[2] / 2, rect[1] + rect[3] / 2

def point_in_rect(point, rect):
    return point[0] > rect[0] and point[1] > rect[1] and point[0] < rect[0] + rect[2] and point[1] < rect[1] + rect[3]


MAXIMUM_HOLE_MOVE_DISTANCE_PER_FRAME = 100
MINIMUM_LARGE_ORANGERED_BOUNDING_BOX_AREA = 100 * 100
FPS_WEIGHT = 0.1

########################################################
YEAR = 1955
########################################################

SVM_FILENAME = '{}/../data/Torpedoes/{}'.format(os.path.dirname(os.path.realpath(__file__)),
                                                            'svm_latest_{}.pkl'.format(YEAR))
class Torpedoes(ModuleBase.ModuleBase):
    def __init__(self):
        super(Torpedoes, self).__init__(True)
        self.vsp_data = None
        self.time_per_frame = 0
        self.last_time = time.time()
        
        self.correct_guesses = [0., 0., 0., 0.]
        self.count_attempts = [0, 0, 0, 0]
        self.known_numbers = [55, 55, 55, 55]
        
        self.dirname = os.path.dirname(os.path.realpath(__file__))
        self.number_index = 0
        with open(SVM_FILENAME) as svm_file:
            self.svm = pickle.load(svm_file)

    def process(self, mat):
        self.time_per_frame = self.time_per_frame * (1 - FPS_WEIGHT) + (time.time() - self.last_time) * FPS_WEIGHT
        self.last_time = time.time()

        print('Torpedoes fps: {}'.format(1 / self.time_per_frame))
        def get_percentage(i):
            if self.count_attempts[i] != 0:
                return '{}: {}'.format(self.known_numbers[i], self.correct_guesses[i] / self.count_attempts[i])
            else:
                return '{}: N/A'.format(self.known_numbers[i])
        print([get_percentage(i) for i in range(4)])

        self.post('orig', mat)

        lab_image = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        lab_split = cv2.split(lab_image)
        luv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2LUV)
        luv_split = cv2.split(luv_image)
        yuv_image = cv2.cvtColor(mat, cv2.COLOR_BGR2YUV)
        yuv_split = cv2.split(yuv_image)
        hls_image = cv2.cvtColor(mat, cv2.COLOR_BGR2HLS)
        hls_split = cv2.split(hls_image)

        yellow_athreshed = cv2.inRange(lab_split[1], self.options["yellow_a_min"], self.options["yellow_a_max"])
        yellow_luthreshed = cv2.inRange(hls_split[1], self.options["yellow_lu_min"], self.options["yellow_lu_max"])
        yellow_yuthreshed = cv2.inRange(hls_split[1], self.options["yellow_yu_min"], self.options["yellow_yu_max"])

        yellow_final_threshed = (yellow_athreshed & yellow_luthreshed) | (yellow_athreshed & yellow_yuthreshed) | (yellow_luthreshed & yellow_yuthreshed)
        yellow_dilated = cv2.dilate(yellow_final_threshed, np.ones((5, 5)))
        yellow_eroded = yellow_dilated
        #yellow_eroded = cv2.erode(yellow_dilated, np.ones((10, 10)))

        orangered_lab_bthreshed = cv2.inRange(lab_split[2], self.options["orangered_lab_b_min"], self.options["orangered_lab_b_max"])
        orangered_yuv_vthreshed = cv2.inRange(yuv_split[2], self.options["orangered_yuv_v_min"], self.options["orangered_yuv_v_max"])
        orangered_hls_hthreshed = cv2.inRange(hls_split[0], self.options["orangered_hls_h_min"], self.options["orangered_hls_h_max"])

        orangered_final_threshed = orangered_lab_bthreshed & orangered_yuv_vthreshed & orangered_hls_hthreshed

        if self.options["debugging"]:
            self.post('Yellow a Threshed', yellow_athreshed)
            self.post('Yellow lu Threshed', yellow_luthreshed)
            self.post('Yellow yu Threshed', yellow_yuthreshed)
            self.post("Yellow Threshed", yellow_final_threshed)
            self.post("Yellow Eroded", yellow_eroded.copy())

            self.post('Orangered lab bThreshed', orangered_lab_bthreshed)
            self.post('Orangered yuv bThreshed', orangered_yuv_vthreshed)
            self.post('Orangered hls hThreshed', orangered_hls_hthreshed)
            self.post("Orangered Threshed", orangered_final_threshed)

        _, yellow_contours, yellow_hierarchy = cv2.findContours(yellow_eroded, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)

        _, orangered_contours, orangered_hierarchy = cv2.findContours(orangered_final_threshed, cv2.RETR_TREE,
                                                                      cv2.CHAIN_APPROX_SIMPLE)

        if yellow_hierarchy is None or orangered_hierarchy is None:
            print('Cannot find any contours in the picture')
            results.target_center_x.set(-1)
            results.target_center_y.set(-1)
            results.target_size.set(-1)
            return

        yellow_hierarchy = yellow_hierarchy[0]
        yellow_curr_contour_info = yellow_hierarchy[0]

        orangered_hierarchy = orangered_hierarchy[0]

        outer_index = 0
        # find the first outermost yellow contour
        while yellow_curr_contour_info[3] >= 0:
            outer_index, yellow_curr_contour_info = yellow_curr_contour_info[3], yellow_hierarchy[yellow_curr_contour_info[3]]

        while yellow_curr_contour_info[1] >= 0:
            outer_index, yellow_curr_contour_info = yellow_curr_contour_info[1], yellow_hierarchy[yellow_curr_contour_info[1]]

        if self.options['debugging']:
            all_contours_drawing = np.copy(mat)
            cv2.drawContours(all_contours_drawing, [c for c in yellow_contours if cv2.contourArea(c)], -1,
                             (255, 255, 0), 2)
            self.post("All yellow_contours", all_contours_drawing)

        yellow_contour_areas = map(lambda (i, x): [cv2.contourArea(x), i, cv2.boundingRect(x), x],
                                   enumerate(yellow_contours))

        orangered_contour_areas = map(lambda (i, x): [cv2.contourArea(x), i, cv2.boundingRect(x), x],
                                      enumerate(orangered_contours))

        outer_yellow_contours = [yellow_contour_areas[outer_index]]
        while yellow_curr_contour_info[0] >= 0:
            outer_index, yellow_curr_contour_info = yellow_curr_contour_info[0], yellow_hierarchy[yellow_curr_contour_info[0]]
            outer_yellow_contours.append(yellow_contour_areas[outer_index])

        outer_yellow_contours = filter(lambda x: x[0] > 30000, outer_yellow_contours)
        
        if len(outer_yellow_contours) > 0:
            # find the best contour in the yellow contours
            full_target = max(outer_yellow_contours, key=lambda x: x[0] * x[2][1] * (x[0] / (x[2][2] * x[2][3])))

            center_rect = full_target[2]

            # find positions of the corners clockwise from top left (in order to do various contour analyses on them)
            corners = [(center_rect[0], center_rect[1]), (center_rect[0] + center_rect[2], center_rect[1]),
                       (center_rect[0] + center_rect[2], center_rect[1] + center_rect[3]),
                       (center_rect[0], center_rect[1] + center_rect[3])]

            # calculate skew by finding the minimum distance from each corner to a point in the contour
            corner_distances = []
            for corner in corners:
                dist = min(map(lambda x: distance(corner, x[0]), full_target[3]))
                corner_distances.append(dist)
            # the skew (left-right) is the sum of the left two corner distances minus the right two corner distances.
            # If this is poisitive, go right, if it's negative, go left
            skew = corner_distances[0] + corner_distances[3] - corner_distances[1] - corner_distances[2]

            # calculate the area of the detected contour's children
            children_areas = []
            index = yellow_hierarchy[full_target[1]][2]

            while index >= 0:
                children_areas.append(yellow_contour_areas[index])
                index = yellow_hierarchy[index][0]

            children_areas.sort(key=lambda x: x[0], reverse=True)

            # find the biggest four-ish children of the best contour
            torpedo_holes = filter(lambda x: x[0] > full_target[0] / 100, children_areas[:4])
            num_torpedo_holes = len(torpedo_holes)
            for _ in range(len(torpedo_holes), 4):
                torpedo_holes.append(None)
            # find the next couple biggest holes, which are likely the numbers
            possible_number_boxes = filter(lambda x: x[0] > 1000,
                                           children_areas[num_torpedo_holes:num_torpedo_holes * 2])

            torpedoes = {}
            cover_index = -1
            cover_probability = 0
            cover_probability_max = 1

            def check_fit(torpedo_holes_permutation):
                m_sum = 0
                for index, torpedo_hole in enumerate(torpedo_holes_permutation):
                    if torpedo_hole is None:
                        continue

                    m_center = center(torpedo_hole[2])
                    m_sum += distance(m_center, corners[index])

                    for other_torpedo_hole in torpedo_holes_permutation:
                        if other_torpedo_hole is None or other_torpedo_hole is torpedo_hole:
                            continue
                        if index == 0:
                            m_sum += max(0, torpedo_hole[2][0] - other_torpedo_hole[2][0])
                            m_sum += max(0, torpedo_hole[2][1] - other_torpedo_hole[2][1])
                        elif index == 1:
                            m_sum += max(0, -torpedo_hole[2][0] + other_torpedo_hole[2][0])
                            m_sum += max(0, torpedo_hole[2][1] - other_torpedo_hole[2][1])
                        elif index == 2:
                            m_sum += max(0, -torpedo_hole[2][0] + other_torpedo_hole[2][0])
                            m_sum += max(0, -torpedo_hole[2][1] + other_torpedo_hole[2][1])
                        elif index == 3:
                            m_sum += max(0, torpedo_hole[2][0] - other_torpedo_hole[2][0])
                            m_sum += max(0, -torpedo_hole[2][1] + other_torpedo_hole[2][1])
                return m_sum

            torpedo_map = min(itertools.permutations(torpedo_holes[index] for index in range(4)), key=check_fit)

            for index, torpedo_hole in enumerate(torpedo_map):
                if torpedo_hole is None:
                    continue
                m_center = center(torpedo_hole[2])

                torpedoes[index] = {}
                torpedoes[index]['bounding box'] = torpedo_hole[2]
                torpedoes[index]['size'] = torpedo_hole[0]

                number_rotated_rect = None
                rotated_rect = cv2.minAreaRect(torpedo_hole[3])
                torpedoes[index]['rotated rect'] = rotated_rect
                width, height = rotated_rect[1]
                rotation = rotated_rect[2]

                if rotation > -30:
                    rotation -= 90
                extra = abs(height - width)

                for number_box in possible_number_boxes:
                    n_center = center(number_box[2])
                    if m_center[1] > n_center[1] > m_center[1] - torpedo_hole[2][3] and number_box[2][0] > \
                            torpedo_hole[2][0] and number_box[2][0] + number_box[2][2] < torpedo_hole[2][0] + \
                            torpedo_hole[2][2]:
                        number_rotated_rect = (n_center, (number_box[2][2], number_box[2][3]), 0)
                        break

                if number_rotated_rect is None and extra > 20 or True:
                    # number box up in hurr
                    torpedoes[index]['bounding box'] = (
                        torpedo_hole[2][0], torpedo_hole[2][1] + extra, torpedo_hole[2][2], torpedo_hole[2][3] - extra)
                    xoffset = (height / 2) * cos(radians(-rotation))
                    yoffset = (height / 2) * sin(radians(-rotation))
                    n_center = (rotated_rect[0][0] + xoffset, rotated_rect[0][1] - yoffset - 10)

                    number_rotated_rect = (n_center, (torpedo_hole[2][2]/3, torpedo_hole[2][3]/3), rotation)

                torpedoes[index]['number bounding box'] = number_rotated_rect

            possible_outer_orangered_contours = []
            orangered_index = 0
            while orangered_index >= 0:
                possible_outer_orangered_contours.append(orangered_contour_areas[orangered_index])
                orangered_index = orangered_hierarchy[orangered_index][0]

            for index in torpedoes:
                m_center = center(torpedoes[index]['bounding box'])
                try:
                    orangered_index, _ = min(enumerate(map(functools.partial(distance, m_center), (center(c[2]) for c in possible_outer_orangered_contours))), key=lambda x: x[1])
                except ValueError:
                    continue

                m_cover_probability, orangered_index, _, _ = possible_outer_orangered_contours[orangered_index]
                m_child_index = orangered_hierarchy[orangered_index][2]
                while m_child_index >= 0:
                    m_cover_probability -= orangered_contour_areas[m_child_index][0]
                    m_child_index = orangered_hierarchy[m_child_index][0]

                cover_probability_max = orangered_contour_areas[orangered_index][2][2] * \
                                        orangered_contour_areas[orangered_index][2][3]
                m_cover_probability /= cover_probability_max
                if m_cover_probability > cover_probability:
                    cover_index = index
                    cover_probability = m_cover_probability

            if cover_index != -1 and cover_probability > 0.4:
                torpedoes[cover_index]['number bounding box'] = ((torpedoes[cover_index]['number bounding box'][0][0], torpedoes[cover_index]['number bounding box'][0][1] - 10), torpedoes[cover_index]['number bounding box'][1], torpedoes[cover_index]['number bounding box'][2])
            sorted_sizes = sorted(((index, pow(min(torpedoes[index]['rotated rect'][1]), 2)) for index in torpedoes), key=lambda x: x[1])

            if len(torpedoes) == 4:
                torpedoes[sorted_sizes[0][0]]['small'] = True
                torpedoes[sorted_sizes[1][0]]['small'] = True
            elif len(torpedoes) == 3:
                first_dist = abs(sorted_sizes[0][1] - sorted_sizes[1][1])
                second_dist = abs(sorted_sizes[0][1] - sorted_sizes[2][1])
                third_dist = abs(sorted_sizes[1][1] - sorted_sizes[2][1])
                if first_dist > second_dist:
                    # index 1 is outlier
                    if sorted_sizes[0][1] < sorted_sizes[1][1]:
                        torpedoes[sorted_sizes[0][0]]['small'] = True
                        torpedoes[sorted_sizes[2][0]]['small'] = True
                    else:
                        torpedoes[sorted_sizes[1][0]]['small'] = True
                elif first_dist > third_dist:
                    # index 0 is outlier
                    if sorted_sizes[0][1] < sorted_sizes[1][1]:
                        torpedoes[sorted_sizes[0][0]]['small'] = True
                    else:
                        torpedoes[sorted_sizes[1][0]]['small'] = True
                        torpedoes[sorted_sizes[2][0]]['small'] = True
                else:
                    # index 2 is outlier
                    if sorted_sizes[0][1] < sorted_sizes[2][1]:
                        torpedoes[sorted_sizes[0][0]]['small'] = True
                        torpedoes[sorted_sizes[1][0]]['small'] = True
                    else:
                        torpedoes[sorted_sizes[2][0]]['small'] = True


                        
            if self.options['debugging']:
                for index in range(4):
                    if index in torpedoes:
                        if torpedoes[index]['number bounding box'] is not None:
                            rotated_rect = torpedoes[index]['number bounding box']
                            box = cv2.boxPoints(rotated_rect)
                            minx = int(min(x[0] for x in box))
                            miny = int(min(x[1] for x in box))
                            maxx = int(max(x[0] for x in box))
                            maxy = int(max(x[1] for x in box))
                            subimage = mat[miny:maxy, minx:maxx]
                            #self.post('{} subimage'.format(index), subimage)
                            if self.options['write numbers']:
                                cv2.imwrite('{}/../data/Torpedoes/{}/{}.png'.format(self.dirname, self.known_numbers[index],
                                                                                    self.number_index), subimage)
                                self.number_index += 1

            index_names = ['top_left', 'top_right', 'bottom_right', 'bottom_left']

            c = center(full_target[2])
            results.target_center_x.set(c[0])
            results.target_center_y.set(c[1])
            results.target_size.set(int(full_target[0] * full_target[2][1] * (full_target[0] / (full_target[2][2] * full_target[2][3]))))
            results.skew.set(int(skew))

            def write_torpedo(torpedo, index):
                if torpedo is not None:
                    m_center = center(torpedo['bounding box'])
                    getattr(results, '{}_center_x'.format(index_names[index])).set(int(m_center[0]))
                    getattr(results, '{}_center_y'.format(index_names[index])).set(int(m_center[1]))

                    rect = torpedo['number bounding box']
                    if rect:
                        try:
                            box = cv2.boxPoints(rect)
                            minx = int(min(x[0] for x in box))
                            miny = int(min(x[1] for x in box))
                            maxx = int(max(x[0] for x in box))
                            maxy = int(max(x[1] for x in box))
                            subimage = lab_split[0][miny:maxy, minx:maxx]
                            subimage = cv2.adaptiveThreshold(subimage, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, 15, 16)
                            self.post('{} subimage'.format(index), subimage.copy())
                            subimage = cv2.resize(subimage, (40, 40))
                            subimage = subimage.flatten()
                            prediction = self.svm.predict(subimage)
                            #print('{}: {}, {}'.format(self.known_numbers[index], self.svm.decision_function(subimage), prediction[0]))
                            number = prediction[0]
                            if index == 0 or index == 1:
                                number = 20
                            else:
                                number = 15
                            getattr(results, '{}_number'.format(index_names[index])).set(number)
                        except Exception as e:
                            traceback.print_exc(e)
                            getattr(results, '{}_number'.format(index_names[index])).set(-1)
                    else:
                        getattr(results, '{}_number'.format(index_names[index])).set(-1)
                        
                    getattr(results, '{}_small'.format(index_names[index])).set('small' in torpedo)
                    getattr(results, '{}_size'.format(index_names[index])).set(torpedo['size'])
                else:
                    getattr(results, '{}_center_x'.format(index_names[index])).set(-1)
                    getattr(results, '{}_center_y'.format(index_names[index])).set(-1)
                    getattr(results, '{}_number'.format(index_names[index])).set(-1)
                    getattr(results, '{}_small'.format(index_names[index])).set(False)
                    getattr(results, '{}_size'.format(index_names[index])).set(-1)

            
            if len(torpedoes) >= 3:
                for index in torpedoes.keys():
                    write_torpedo(torpedoes[index], index)
            elif len(torpedoes) == 2:
                indices = torpedoes.keys()
                if (min(indices) + 1) % 4 == max(indices) or (0 in indices and 3 in indices):
                    if sum(indices) == 3:
                        if 0 in indices:
                            write_torpedo(torpedoes[0], 0)
                            write_torpedo(torpedoes[0], 1)
                            write_torpedo(torpedoes[3], 2)
                            write_torpedo(torpedoes[3], 3)
                        else:
                            write_torpedo(torpedoes[1], 0)
                            write_torpedo(torpedoes[1], 1)
                            write_torpedo(torpedoes[2], 2)
                            write_torpedo(torpedoes[2], 3)
                    else:
                        if 0 in indices:
                            write_torpedo(torpedoes[0], 0)
                            write_torpedo(torpedoes[0], 3)
                            write_torpedo(torpedoes[1], 1)
                            write_torpedo(torpedoes[1], 2)
                        else:
                            write_torpedo(torpedoes[2], 1)
                            write_torpedo(torpedoes[2], 2)
                            write_torpedo(torpedoes[3], 3)
                            write_torpedo(torpedoes[3], 0)
                else:
                    for index in torpedoes.keys():
                        write_torpedo(torpedoes[index], index)
            elif len(torpedoes) == 1:
                index = torpedoes.keys()[0]
                write_torpedo(torpedoes[index], 0)
                write_torpedo(torpedoes[index], 1)
                write_torpedo(torpedoes[index], 2)
                write_torpedo(torpedoes[index], 3)
            else:
                write_torpedo(None, 0)
                write_torpedo(None, 1)
                write_torpedo(None, 2)
                write_torpedo(None, 3)

            results.torpedo_cover_index.set(cover_index)
            results.torpedo_cover_probability.set(cover_probability)
            if self.options['debugging']:
                img = np.copy(mat)
                cv2.rectangle(img, (center_rect[0], center_rect[1]),
                              (center_rect[0] + center_rect[2], center_rect[1] + center_rect[3]), (255, 0, 0), 4)

                colors = [(0, 255, 255), (0, 0, 255), (255, 255, 0), (255, 0, 255)]
                for i in range(4):
                    if i in torpedoes:
                        rect = map(int, torpedoes[i]['bounding box'])
                        cv2.rectangle(img, (rect[0], rect[1]), (rect[0] + rect[2], rect[1] + rect[3]), colors[i],
                                      6 if i != cover_index else -1)

                        rect = torpedoes[i]['number bounding box']
                        if rect:
                            box = cv2.boxPoints(rect)
                            box = np.int0(box)
                            minx = min(x[0] for x in box)
                            miny = min(x[1] for x in box)
                            cv2.drawContours(img, [box], 0, colors[i], 2)
                            number = getattr(results, '{}_number'.format(index_names[i])).get()
                            cv2.putText(img, '{}'.format(number), (minx, miny), cv2.FONT_HERSHEY_SIMPLEX, 0.8,
                                        (255, 255, 255))
                            if len(torpedoes) > 2:
                                self.correct_guesses[i] += number == self.known_numbers[i]
                                self.count_attempts[i] += 1
                self.post('Final', img)
        else:
            results.target_center_x.set(-1)
            results.target_center_y.set(-1)
            results.target_size.set(-1)
                                

