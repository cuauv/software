from collections import namedtuple
import math

import cv2 as cv2
import numpy as np

import shm
from vision import options
from vision.stdlib import *



def get_shared_options(is_forward):
    # (Downward, Forward)
    return [
        # Global
        options.BoolOption('in_simulator', False),
        options.BoolOption('preprocess_debug', False),
        options.BoolOption('thresh_debug', True),
        options.BoolOption('contour_debug', False),
        options.BoolOption('bins_debug', True),
        options.BoolOption('funnels_debug', False),

        # Preprocess
        options.IntOption('gaussian_kernel', (5, 5)[is_forward], 1, 40),
        options.IntOption('gaussian_stdev', 20, 0, 40),

        # Threshing
        options.IntOption('erode_size', (3, 5)[is_forward], 1, 40),
        options.IntOption('thresh_size', (15, 15)[is_forward], 1, 100),
        options.IntOption("lab_b_min_red_bin", 140, 0, 255),
        options.IntOption("lab_b_max_red_bin", 250, 0, 255),
        options.IntOption("color_dist_min_red_funnel", (137, 0)[is_forward], 0, 255),
        options.IntOption("color_dist_max_red_funnel", (250, 35)[is_forward], 0, 255),
        options.IntOption("color_dist_min_yellow_funnel", (0, -1)[is_forward], 0, 255),
        options.IntOption("color_dist_max_yellow_funnel", (25, -1)[is_forward], 0, 255),

        # Contouring
        options.IntOption('min_area', (50, 100)[is_forward], 1, 2000),
        options.IntOption('min_y', (0, 200)[is_forward], 0, 2000),
        options.DoubleOption('min_circularity', (0.8, 0.1)[is_forward], 0, 1),
        options.DoubleOption('max_rectangularity', 0.9, 0, 1),

        # Binning
        options.DoubleOption('max_joining_dist', 120, 0, 500),
    ]

def copy_mat(mat):
    if type(mat) == type(cv2.UMat()):
        return mat.get()
    else:
        return mat.copy()


dist_multiple = (None, None, 1, 2 + math.sqrt(2), 4 + 2 * math.sqrt(2), 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)


class Shared:
    def __init__(self, is_forward, options, post, img):
        self.is_forward = is_forward
        self.options = options
        def p(name, mat):
            if type(mat) == type(cv2.UMat()):
                post(name, mat.get())
            else:
                post(name, mat)
        self.post = p
        self.img = img


def set_shared_globals(*args, **kwargs):
    global shared
    shared = Shared(*args, **kwargs)

Bin = namedtuple("Bin", ["x", "y", "area", "probability"])
FeaturedContour = namedtuple("FeaturedContour", ["x", "y", "area", "contour", "circularity"])


# def calc_contour_features(contour):
#     moments = cv2.moments(contour)
#     x = int(moments['m10']/moments['m00'])
#     y = int(moments['m01']/moments['m00'])

#     area = cv2.contourArea(contour)

#     return FeaturedContour(x, y, area, contour)


def fc_togetherness_rating(fc1, fc2):
    dist = math.hypot(fc1.x - fc2.x, fc1.y - fc2.y)

    if dist < 1:
        return float('Inf')

    return dist


def avg_fc(*fcs):
    total_area = sum(fc.area for fc in fcs)
    x = sum(fc.x * fc.area for fc in fcs) / total_area
    y = sum(fc.y * fc.area for fc in fcs) / total_area
    area = total_area

    return FeaturedContour(x, y, area, None, None)


def preprocess(img):
    debug = shared.options["preprocess_debug"]

    k_size = shared.options["gaussian_kernel"]
    k_std = shared.options["gaussian_stdev"]
    blurred = cv2.GaussianBlur(img, (k_size * 2 + 1, k_size * 2 + 1), k_std, k_std)

    if debug:
        shared.post("preprocessed", blurred)

    return blurred


def threshold(img):
    debug = shared.options["thresh_debug"]

    threshes = {}
    debugs = {}

    luv = cv2.cvtColor(img, cv2.COLOR_BGR2LUV)
    (luv_l, luv_u, luv_v) = cv2.split(luv)

    lab = cv2.cvtColor(img, cv2.COLOR_BGR2LAB)
    (lab_l, lab_a, lab_b) = cv2.split(lab)

    hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
    (hsv_h, hsv_s, hsv_v) = cv2.split(hsv)

    debugs["luv"] = luv
    debugs["lab"] = lab

    debugs["luv_u"] = luv_u
    debugs["luv_l"] = luv_l
    debugs["lab_a"] = lab_a
    debugs["lab_b"] = lab_b

    if shared.options["in_simulator"]:
        # dist_from_green = np.linalg.norm(luv.astype(int) - [150, 90, 103], axis=2)

        # threshes["green"] = cv2.inRange(
        #     dist_from_green,
        #     0,
        #     40,
        # )

        if shared.is_forward:
            threshes["green"] = cv2.adaptiveThreshold(
                lab_a,
                255,
                cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                cv2.THRESH_BINARY_INV,
                shared.options["thresh_size"] * 2 + 1,
                5,
            )

            threshes["red"] = cv2.adaptiveThreshold(
                luv_u,
                255,
                cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                cv2.THRESH_BINARY,
                shared.options["thresh_size"] * 2 + 1,
                -3,
            )

        else:
            threshes["bin_green"] = cv2.adaptiveThreshold(
                lab_a,
                255,
                cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                cv2.THRESH_BINARY_INV,
                shared.options["thresh_size"] * 2 + 1,
                5,
            )

            threshes["bin_red"] = cv2.adaptiveThreshold(
                luv_u,
                255,
                cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                cv2.THRESH_BINARY,
                shared.options["thresh_size"] * 2 + 1,
                -3,
            )

    else:
        if shared.is_forward:
            # shared.post("l", lab)

            dist_from_red_top = np.linalg.norm(lab[:, :, :].astype(int) - [150, 171, 151], axis=2).astype(int)
            dist_from_red_bottom = np.linalg.norm(lab[:, :, :].astype(int) - [107, 134, 130], axis=2).astype(int)

            rf = threshes["red"] = cv2.inRange(
                dist_from_red_top,
                shared.options["color_dist_min_red_funnel"],
                shared.options["color_dist_max_red_funnel"],
            ) | cv2.inRange(
                dist_from_red_bottom,
                shared.options["color_dist_min_red_funnel"],
                shared.options["color_dist_max_red_funnel"],
            )

        else:

            # threshes["bin_red"] = cv2.adaptiveThreshold(
            #     lab_a,
            #     255,
            #     cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
            #     cv2.THRESH_BINARY_INV,
            #     shared.options["thresh_size"] * 2 + 1,
            #     3,
            # )

            dist_from_red = np.linalg.norm(img.astype(int) - [147, 0, 31], axis=2).astype(int)

            rf = threshes["red_funnel"] = cv2.inRange(
                dist_from_red,
                shared.options["color_dist_min_red_funnel"],
                shared.options["color_dist_max_red_funnel"],
            )

            funnel = np.sum(rf != 0) / (rf.shape[0] * rf.shape[1])

            shm.recovery_vision_downward_red.probability.set(funnel)


            dist_from_yellow = np.linalg.norm(lab[:, :, 1:].astype(int) - [248, 111, 173][1:], axis=2).astype(int)
            # dist_from_yellow = np.linalg.norm(lab[:, :, :2].astype(int) - [149, 115, 170][:2], axis=2).astype(int)
            # dist_from_yellow = np.linalg.norm(lab[:, :, 0].astype(int) - [248, 111, 173][0], axis=2).astype(int)

            threshes["all_bins"] = cv2.inRange(
                dist_from_yellow,
                shared.options["color_dist_min_yellow_funnel"],
                shared.options["color_dist_max_yellow_funnel"],
            )

            a, b = cv2.threshold(luv_u, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
            a, b = cv2.threshold(lab_a, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
            a, b = cv2.threshold(lab_l, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
            threshes["otsu"] = b
            threshes["all_bins"] = b

            threshes["all_bins"] = cv2.adaptiveThreshold(
                lab_a,
                255,
                cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                cv2.THRESH_BINARY_INV,
                shared.options["thresh_size"] * 2 + 1,
                2,
            )

            # threshes["all_bins"] = cv2.inRange(
            #     lab_b,
            #     # hsv_s,
            #     shared.options["lab_b_min_red_bin"],
            #     shared.options["lab_b_max_red_bin"],
            # )



    e_size = shared.options["erode_size"]
    e_kernel = cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (e_size * 2 + 1, e_size * 2 + 1), (e_size, e_size))

    cleaned = {
        name: cv2.dilate(cv2.erode(image, e_kernel), e_kernel)
        # name: cv2.dilate(image, e_kernel)
        # name: cv2.erode(image, e_kernel)
        # name: image
        for name, image in threshes.items()
    }

    if debug:
        for name, image in debugs.items():
            shared.post("thresh_debug_{}".format(name), image)

        for name, image in threshes.items():
            shared.post("thresh_{}".format(name), image)

        for name, image in cleaned.items():
            shared.post("thresh_cleaned_{}".format(name), image)

    # return threshes
    return cleaned





def find_contours(images):
    debug = shared.options["contour_debug"]

    all_contours = {
        name: cv2.findContours(image, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)[1]
        for name, image in images.items()
    }

    contours = {}

    for name, all_cs in all_contours.items():
        cs = []

        for contour in all_cs:
            area = cv2.contourArea(contour)

            if area < shared.options["min_area"]:
                continue

            circle = cv2.minEnclosingCircle(contour)
            rect = cv2.boundingRect(contour)

            circularity = area / (math.pi * circle[1] ** 2)

            if circularity < shared.options["min_circularity"]:
                continue

            if area / (rect[2] * rect[3]) > shared.options["max_rectangularity"]:
                continue


            moments = cv2.moments(contour)
            x = int(moments['m10']/moments['m00'])
            y = int(moments['m01']/moments['m00'])

            if y < shared.options["min_y"]:
                continue

            cs.append(FeaturedContour(x, y, area, contour, circularity))

        contours[name] = cs

    if debug:
        for name, fcs in contours.items():
            image = copy_mat(shared.img)
            cs = [fc.contour for fc in fcs]
            cv2.drawContours(image, cs, -1, COLORS["BLUE"], 2)
            shared.post("contours_{}".format(name), image)

    return contours




def find_funnels(featured_contours):
    debug = shared.options["funnels_debug"]

    bins = {}

    for name, fcs in featured_contours.items():
        if len(fcs) < 2:
            bins[name] = Bin(0, 0, 0, 0)
            continue

        contours_to_draw = []

        # total_area = sum(fc.area for fc in fcs)
        # x = sum(fc.x * fc.area for fc in fcs) / total_area
        # y = sum(fc.y * fc.area for fc in fcs) / total_area
        # area = total_area

        if shared.is_forward:
            best_fc = max(fcs, key=lambda fc: fc.area)
        else:
            best_fc = max(fcs, key=lambda fc:  fc.circularity)


        area = best_fc.area
        x = best_fc.x
        y = best_fc.y

        contours_to_draw.append((best_fc.contour, COLORS["RED"]))

        result_fc = best_fc

        if len(fcs) >= 2:
            fc2 = min(fcs, key=lambda fc: fc_togetherness_rating(best_fc, fc))

            if fc_togetherness_rating(best_fc, fc2) < shared.options["max_joining_dist"]:
                result_fc = avg_fc(best_fc, fc2)

                contours_to_draw.append((fc2.contour, COLORS["MAGENTA"]))
            else:
                continue

        binn = bins[name] = Bin(result_fc.x, result_fc.y, result_fc.area, 1)

        if debug:
            image = copy_mat(shared.img)
            cv2.circle(image, (int(binn.x), int(binn.y)), int(math.sqrt(binn.area)), COLORS["BLUE"], 5)

            for contour, color  in contours_to_draw:
                cv2.drawContours(image, [contour], -1, color, 2)

            shared.post("bin_{}".format(name), image)

    return bins

FeaturedContour2 = namedtuple("FeaturedContour2", ["x", "y", "area", "contour", "circularity", "index"])

def calc_features(index, contour):
    area = cv2.contourArea(contour)

    if area < shared.options["min_area"]:
        return

    circle = cv2.minEnclosingCircle(contour)
    rect = cv2.boundingRect(contour)

    circularity = area / (math.pi * circle[1] ** 2)

    if circularity < shared.options["min_circularity"]:
        return

    if area / (rect[2] * rect[3]) > shared.options["max_rectangularity"]:
        return


    moments = cv2.moments(contour)
    x = int(moments['m10']/moments['m00'])
    y = int(moments['m01']/moments['m00'])

    return FeaturedContour2(x, y, area, contour, circularity, index)



def find_bins(images):
    debug = shared.options["bins_debug"]

    all_img = images["all_bins"]
    _, all_contours, hierarchy = cv2.findContours(all_img, cv2.RETR_TREE, cv2.CHAIN_APPROX_SIMPLE)
    if hierarchy is None:
        return []
    hierarchy = hierarchy[0]
    all_features = [calc_features(*args) for args in enumerate(all_contours)]
    good_features = [fc for fc in all_features if fc]

    tops = [fc for fc in good_features if hierarchy[fc.index][3] < 0]
    childs = {}

    for fc in tops:
        i = fc.index
        childs[i] = []
        child_i = hierarchy[i][2]

        while child_i >= 0:
            childs[i].append(child_i)
            child_i = hierarchy[child_i][0]

    if len(tops) > 2:
        print("Too many bins??")

    bins = []

    for top in tops:
        children = [all_features[i] for i in childs[top.index] if all_features[i]]

        if 2 <= len(children) <= 7:
            total_area = sum(fc.area for fc in children)
            x = sum(fc.x * fc.area for fc in children) / total_area
            y = sum(fc.y * fc.area for fc in children) / total_area


            total_dist = 0

            for i, fc1 in enumerate(children):
                for fc2 in children[i + 1:]:
                    total_dist += math.hypot(fc1.x - fc2.x, fc1.y - fc2.y)

            area = total_dist / dist_multiple[len(children)]

            bins.append(Bin(x, y, area, 1))

    if len(good_features) > 0:
        total_area = sum(fc.area for fc in good_features)
        x = sum(fc.x * fc.area for fc in good_features) / total_area
        y = sum(fc.y * fc.area for fc in good_features) / total_area

        dots = []

        for gf in good_features:
            if math.hypot(gf.x - x, gf.y - y) < shared.options["max_joining_dist"]:
                dots.append(gf)

        if len(dots) >= 3:
            total_area = sum(fc.area for fc in dots)
            x = sum(fc.x * fc.area for fc in dots) / total_area
            y = sum(fc.y * fc.area for fc in dots) / total_area


            total_dist = 0

            for i, fc1 in enumerate(dots):
                for fc2 in dots[i + 1:]:
                    total_dist += math.hypot(fc1.x - fc2.x, fc1.y - fc2.y)

            area = total_dist / dist_multiple[len(dots)]

            bins.append(Bin(x, y, area, 1))



    shm_groups = [shm.recovery_vision_downward_bin_red, shm.recovery_vision_downward_bin_green]
    output1 = shm_groups[0].get()
    output2 = shm_groups[1].get()

    if len(bins):
        binn = bins[0]

        output1.area = binn.area
        output1.center_x = binn.x
        output1.center_y = binn.y
        output1.probability = binn.probability

    else:
        # output1.area = 0
        # output1.center_x = 0
        # output1.center_y = 0
        output1.probability = 0

    if len(bins) >= 2:
        binn = bins[1]

        output2.area = binn.area
        output2.center_x = binn.x
        output2.center_y = binn.y
        output2.probability = binn.probability

    else:
        # output2.area = 0
        # output2.center_x = 0
        # output2.center_y = 0
        output2.probability = 0

    shm_groups[0].set(output1)
    shm_groups[1].set(output2)


    if debug:
        image = shared.img.copy()
        for fc in tops:
            cv2.drawContours(image, [fc.contour], -1, COLORS["ORANGE"], 4)
            cv2.putText(image, str(len(childs[fc.index])), (fc.x, fc.y), cv2.FONT_HERSHEY_SIMPLEX, 1, COLORS["MAGENTA"], 2)

            for child_i in childs[fc.index]:
                c = all_contours[child_i]
                cv2.drawContours(image, [c], -1, COLORS["GREEN"], 2)

        for binn in bins:
            # cv2.putText(image, str(binn), (int(binn.x), int(binn.y)), cv2.FONT_HERSHEY_SIMPLEX, 1, COLORS["MAGENTA"], 2)
            cv2.circle(image, (int(binn.x), int(binn.y)), int(binn.area), COLORS["BLUE"], 5)

        shared.post("bins_all", image)

    return bins
