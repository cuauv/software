from collections import namedtuple
from math import sin, cos, radians
import os
import time

import cv2
import numpy as np

from caffew import caffe_classifier
import shm
from vision.modules import ModuleBase, gui_options

capture_source = 'downward'

vision_options = [gui_options.BooleanOption('debugging', False), gui_options.BooleanOption('cover_debugging', False),
                  gui_options.IntOption('l_min', 180, 0, 255), gui_options.IntOption('l_max', 255, 0, 255),
                  gui_options.IntOption('a_min', 0, 0, 255), gui_options.IntOption('a_max', 52, 0, 255),
                  gui_options.IntOption('b_min', 221, 0, 255), gui_options.IntOption('b_max', 255, 0, 255),
                  gui_options.IntOption('cover_l_min', 52, 0, 255), gui_options.IntOption('cover_l_max', 126, 0, 255),
                  gui_options.IntOption('cover_a_min', 126, 0, 255), gui_options.IntOption('cover_a_max', 138, 0, 255),
                  gui_options.IntOption('cover_b_min', 64, 0, 255), gui_options.IntOption('cover_b_max', 119, 0, 255),
                  gui_options.IntOption('erode_size', 2, 0, 50),
                  gui_options.IntOption('dilate_size', 2, 0, 50),
                  gui_options.IntOption('cover_min_area', 5000),
                  gui_options.IntOption('min_area', 5000),
                  gui_options.IntOption('min_invader_area', 1000),
                  gui_options.BooleanOption('training', False)]

shm_groups = (shm.shape_banana, shm.shape_lightning, shm.shape_bijection, shm.shape_soda)
shape_names = ("banana", "lightning", "bijection", "soda")
shm_group_dictionary = {"banana": shm.shape_banana,
                        "lightning": shm.shape_lightning,
                        "bijection": shm.shape_bijection,
                        "soda": shm.shape_soda}
OutputData = namedtuple("OutputData", ("p", "x", "y", "hd"))

data_dirname = os.path.dirname(os.path.realpath(__file__)) + "/../data/Bins/"
classifier_bson_filename = data_dirname + "/bins_caffe_classifier.bson"
svm_filename = data_dirname + "/bins_svm_classifier.pkl"


class Bins(ModuleBase.ModuleBase):
    def __init__(self):
        super(Bins, self).__init__(True)

        # try:
        #     self.svm = pickle.load(open(svm_filename, "r"))
        # except IOError:
        #     print("Sklearn pickle file could not be loaded")
        #     self.svm = None
        
        print(classifier_bson_filename)
        try:
            self.classifier = caffe_classifier.CaffeClassifier(classifier_bson_filename)
        except IOError as e:
            print()
            print(e)
            print()
            print("Caffe bson file could not be loaded.")
            self.classifier = None

    def process(self, mat):
        self.post('orig', mat)
        final = mat.copy()
        original = cv2.cvtColor(mat, cv2.COLOR_BGR2LAB)
        LabSpace = cv2.split(original)

        lthreshed = cv2.inRange(LabSpace[0], self.options['l_min'], self.options['l_max'])
        athreshed = cv2.inRange(LabSpace[1], self.options['a_min'], self.options['a_max'])
        bthreshed = cv2.inRange(LabSpace[2], self.options['b_min'], self.options['b_max'])

        cover_lthreshed = cv2.inRange(LabSpace[0], self.options['cover_l_min'], self.options['cover_l_max'])
        cover_athreshed = cv2.inRange(LabSpace[1], self.options['cover_a_min'], self.options['cover_a_max'])
        cover_bthreshed = cv2.inRange(LabSpace[2], self.options['cover_b_min'], self.options['cover_b_max'])

        finalThreshed = lthreshed & athreshed & bthreshed
        cover_finalThreshed = cover_athreshed & cover_bthreshed & cover_lthreshed

        erodeSize = self.options['erode_size']
        dilateSize = self.options['dilate_size']
        erodeElement = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (erodeSize * 2 + 1, erodeSize * 2 + 1),
                                                 (erodeSize, erodeSize))
        dilateElement = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (dilateSize * 2 + 1, dilateSize * 2 + 1),
                                                  (dilateSize, dilateSize))

        cover_erodeSize = 2
        cover_dilateSize = 8
        cover_erodeElement = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (cover_erodeSize * 2 + 1, cover_erodeSize * 2 + 1),
                                                 (cover_erodeSize, cover_erodeSize))
        cover_dilateElement = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (cover_dilateSize * 2 + 1, cover_dilateSize * 2 + 1),
                                                  (cover_dilateSize, cover_dilateSize))

        dilated = cv2.dilate(finalThreshed, dilateElement)
        eroded = cv2.erode(dilated, erodeElement)

        cover_eroded = cv2.erode(cover_finalThreshed, cover_erodeElement)
        cover_dilated = cv2.dilate(cover_eroded, cover_dilateElement)

        if self.options['debugging']:
            self.post("L", LabSpace[0])
            self.post("a", LabSpace[1])
            self.post("b", LabSpace[2])
            self.post('L Threshed', lthreshed)
            self.post('a Threshed', athreshed)
            self.post('b Threshed', bthreshed)
            self.post("Threshed", finalThreshed)
            self.post("Masked", cv2.bitwise_and(mat, mat, mask=finalThreshed))
            self.post("Dilated", dilated)
            self.post("Dilated/Eroded", eroded.copy())

        if self.options['cover_debugging']:
            self.post('cover_L Threshed', cover_lthreshed)
            self.post('cover_a Threshed', cover_athreshed)
            self.post('cover_b Threshed', cover_bthreshed)
            self.post("cover_Threshed", cover_finalThreshed)
            self.post("cover_Masked", cv2.bitwise_and(mat, mat, mask=cover_finalThreshed))
            self.post("cover_Eroded", cover_eroded)
            self.post("cover_Eroded/Dilated", cover_dilated.copy())

        _, cover_contours, _ = cv2.findContours(cover_dilated, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        if cover_contours:
            if self.options['cover_debugging']:
                cover_allContoursDrawing = np.copy(mat)
                cv2.drawContours(cover_allContoursDrawing, cover_contours, -1, (255, 255, 0), 2)
                self.post("Cover contours", cover_allContoursDrawing)

            good_cover_contours = []
            cover_contour = namedtuple("cover_contour", ("contour", "area", "rotated_rect", "probability"))

            for c in cover_contours:
                cover_area = cv2.contourArea(c)
                if cover_area < self.options['cover_min_area']:
                    continue

                rrect = cv2.minAreaRect(c)

                probability = cover_area / (rrect[1][0] * rrect[1][1])

                if probability > .85:
                    good_cover_contours.append(cover_contour(c, cover_area, rrect, probability))

            if good_cover_contours:
                best_cover_contour = max(good_cover_contours, key=lambda c: c.probability)
                rrect = best_cover_contour.rotated_rect

                if rrect[1][0] > rrect[1][1]:
                    rrect = (rrect[0], (rrect[1][1], rrect[1][0]), rrect[2] + 90)

                if self.options['cover_debugging']:
                    cover_good_contours_drawing = np.copy(mat)
                    cv2.drawContours(cover_good_contours_drawing, [best_cover_contour.contour], -1, (255, 0, 0), 3)

                    scale = rrect[1][1] / 4
                    offset = (scale * sin(radians(rrect[2])), -scale * cos(radians(rrect[2])))
                    p1 = (int(offset[0] + rrect[0][0]), int(offset[1] + rrect[0][1]))
                    p2 = (int(-offset[0] + rrect[0][0]), int(-offset[1] + rrect[0][1]))
                    cv2.line(cover_good_contours_drawing, p1, p2, (0, 255), 2)
                    self.post("Good cover contours", cover_good_contours_drawing)

                cv2.drawContours(final, [best_cover_contour.contour], -1, (255, 0, 0), 3)

                scale = rrect[1][1] / 4
                offset = (scale * sin(radians(rrect[2])), -scale * cos(radians(rrect[2])))
                p1 = (int(offset[0] + rrect[0][0]), int(offset[1] + rrect[0][1]))
                p2 = (int(-offset[0] + rrect[0][0]), int(-offset[1] + rrect[0][1]))
                cv2.line(final, p1, p2, (0, 255), 2)

                shm_group_cover = shm.shape_handle
                shm_group_cover.p.set(best_cover_contour.probability)
                shm_group_cover.x.set(int(rrect[0][0]))
                shm_group_cover.y.set(int(rrect[0][1]))
                shm_group_cover.hd.set(rrect[2])
            else:
                shm.shape_handle.p.set(0)
        else:
            shm.shape_handle.p.set(0)

        _, contours, _ = cv2.findContours(eroded, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
        #
        # unmatched_bins = {"banana": shm.shape_banana,
        #                   "lightning": shm.shape_lightning,
        #                   "bijection": shm.shape_bijection,
        #                   "soda": shm.shape_soda}

        output_datas = [OutputData(0, 0, 0, 0)] * 4
        contour_groups = [shm.bins_contour4, shm.bins_contour3, shm.bins_contour2, shm.bins_contour1]

        if contours:
            if self.options['debugging']:
                allContoursDrawing = np.copy(mat)
                cv2.drawContours(allContoursDrawing, contours, -1, (255, 255, 0), 2)
                self.post("All contours", allContoursDrawing)

            i = 0
            for c in contours:
                area = cv2.contourArea(c)
                if area < self.options['min_area']:
                    continue

                rrect = cv2.minAreaRect(c)

                probability = area / (rrect[1][0] * rrect[1][1])

                if probability > .92:
                    if len(contour_groups):
                        shm_group_bin = contour_groups.pop()
                        shm_group_bin.x.set(int(rrect[0][0]))
                        shm_group_bin.y.set(int(rrect[0][1]))
                        shm_group_bin.hd.set(rrect[2])
                        shm_group_bin.p.set(probability)

                    cv2.drawContours(final, [c], -1, (255, 255, 0), 3)
                    i += 1

                    rrect = (tuple(map(int, rrect[0])), tuple(map(int, rrect[1])), int(rrect[2]))

                    if rrect[1][0] < rrect[1][1]:
                        rrect = (rrect[0], (rrect[1][1], rrect[1][0]), rrect[2] + 90)

                    M = cv2.getRotationMatrix2D(rrect[0], rrect[2], 1)
                    rotated = cv2.warpAffine(mat, M, (mat.shape[1], mat.shape[0]))
                    # cv2.rectangle(rotated, (rrect[0][0] - rrect[1][0] / 2, rrect[0][1] - rrect[1][1] / 2),
                    #               (rrect[0][0] + rrect[1][0] / 2, rrect[0][1] + rrect[1][1] / 2), (255, 0, 255), 3)
                    bin = cv2.getRectSubPix(rotated, tuple(map(int, rrect[1])),
                                            tuple(map(int, rrect[0])))
                    self.post("Bin {}".format(i), bin)
                    self.post("Bin-rotated {}".format(i), rotated)

                    if self.options['training']:
                        filename = data_dirname + "untrained/bin-{}.png".format(time.time())
                        print("Bin {} image written to {}".format(i, filename))
                        cv2.imwrite(filename, bin)

                    if self.classifier:
                        tmp_bin_filename = data_dirname + "/tmp_bin.png"
                        cv2.imwrite(tmp_bin_filename, bin)

                        classification = self.classifier.classify(tmp_bin_filename)
                        shape_name, probability = classification[0]
                        shm_group = shm_group_dictionary[shape_name]

                        if probability < .7:
                            cv2.putText(final, shape_name, tuple(map(int, rrect[0])), cv2.FONT_HERSHEY_DUPLEX, 1,
                                        (255, 0, 0))
                            print("Best probability was {} for shape {}.".format(probability, shape_name))
                            continue

                        cv2.putText(final, shape_name, tuple(map(int, rrect[0])), cv2.FONT_HERSHEY_DUPLEX, 1,
                                    (0, 255, 0))

                        shape_index = shape_names.index(shape_name)
                        old_output_data = output_datas[shape_index]

                        if old_output_data.p > probability:
                            if rrect[2] > 0:
                                output_datas[shape_index] = OutputData(probability, int(rrect[0][0]), int(rrect[0][1]),
                                                                       rrect[2] - 90)
                            else:
                                output_datas[shape_index] = OutputData(probability, int(rrect[0][0]), int(rrect[0][1]),
                                                                       rrect[2] + 90)

                                # if self.svm:
                                #     moments = []
                                #     for channel in cv2.split(bin):
                                #         moments.extend(cv2.HuMoments(cv2.moments(channel)).reshape(7))
                                #     probabilities = self.svm.predict_proba(moments).reshape(4)
                                #
                                #     best_index, best_probability = max(enumerate(probabilities), key=lambda x: x[1])
                                #     if best_probability > output_datas[best_index].p:
                                #         output_datas[best_index] = OutputData(best_probability, int(rrect[0][0]), int(rrect[0][1]),
                                #                                               rrect[2])

        # for shm_group in unmatched_bins.values():
        #     if shm_group:
        #         shm_group.p.set(0)

        for shm_group, shape_name, output_data in zip(shm_groups, shape_names, output_datas):
            shm_group.p.set(output_data.p)

            if output_data.p:
                shm_group.x.set(output_data.x)
                shm_group.y.set(output_data.y)
                shm_group.hd.set(output_data.hd)
                cv2.putText(final, shape_name, (output_data.x, output_data.y), cv2.FONT_HERSHEY_DUPLEX, 1,
                            (0, 255, 0))
                cv2.putText(final, str(output_data.p), (output_data.x, output_data.y + 30), cv2.FONT_HERSHEY_DUPLEX, 1,
                            (0, 255, 0))

        for shm_group in contour_groups:
            shm_group.p.set(0)

        self.post("Final", final)
