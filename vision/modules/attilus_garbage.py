import cv2
import numpy as np

from functools import reduce
from math import pi, atan, sin, cos

from vision.options import IntOption, DoubleOption
from vision.framework.color import range_threshold, color_dist
from vision.framework.feature import outer_contours, contour_approx, contour_area
from vision.framework.transform import erode, dilate, rect_kernel, morph_remove_noise, morph_close_holes, resize, elliptic_kernel
from vision.framework.helpers import to_umat

from vision.modules.gate import thresh_color_distance

MANIPULATOR_ANGLE = 25

garlic_crucifix_opts = [
        IntOption('yellow_l', 255, 0, 255),  # 224 186
        IntOption('yellow_a', 128, 0, 255),  # 130 144
        IntOption('yellow_b', 129, 0, 255),  #     185
        IntOption('circle_color_distance', 24, 0, 255),
        IntOption('circle_erode_kernel', 5, 1, 50),
        IntOption('circle_dilate_kernel', 5, 1, 50),
        IntOption('circle_erode_iterations', 1, 1, 50),
        IntOption('circle_dilate_iterations', 17, 1, 50),
        IntOption('circle_min_contour_size', 50, 0, 1000),
        DoubleOption('circle_min_circularity', 0.75, 0, 1),
        DoubleOption('garlic_circle_r_offset', 0.5, 0, 1),
        DoubleOption('crucifix_circle_r_offset', 0.5, 0, 1),
        IntOption('red_l', 39, 0, 255),  # 224
        IntOption('red_a', 174, 0, 255),
        IntOption('red_b', 154, 0, 255),
        IntOption('garlic_color_distance', 24, 0, 255),
        IntOption('garlic_erode_kernel', 5, 1, 50),
        IntOption('garlic_dilate_kernel', 5, 1, 50),
        IntOption('garlic_erode_iterations', 4, 1, 50),
        IntOption('garlic_dilate_iterations', 1, 1, 50),
        IntOption('garlic_size_min', 100, 0, 1000),
        IntOption('kmeans_morph_kernel', 5, 0, 50),
        IntOption('kmeans_morph_iterations', 3, 1, 50),
        IntOption('kmeans_shrink_size', 300, 1, 1000),
        IntOption('garlic_line_threshold', 35, 0, 5000),
        IntOption('manipulator_angle', MANIPULATOR_ANGLE, 0, 359),
        IntOption('green_l', 80, 0, 255),
        IntOption('green_a', 119, 0, 255),
        IntOption('green_b', 147, 0, 255),
        IntOption('crucifix_color_distance', 12, 0, 255),
        IntOption('crucifix_erode_kernel', 5, 1, 50),
        IntOption('crucifix_dilate_kernel', 5, 1, 50),
        IntOption('crucifix_erode_iterations', 4, 1, 50),
        IntOption('crucifix_dilate_iterations', 1, 1, 50),
        IntOption('crucifix_size_min', 100, 0, 1000),
        DoubleOption('crucifix_offset_x', 1.29, 0, 10),
]

KMEANS_ITER = 5

# NOTE: Please don't use these for anything other than the 2019 recovery mission. These implementations uses atan instead of atan2 and thus never returns the angle in the correct quadrant (even if it is sufficient for that mission)
def lines_to_angles(line):
    try:
        return atan((line[1]-line[3])/(line[0]-line[2]))
    except ZeroDivisionError:
        return pi/2

def vectors_to_degrees(vectors):
    degrees = atan(vectors[1]/vectors[0])*180/pi
    return degrees if degrees > 0 else 360+degrees

def angle_to_unit_circle(angle):
    return cos(angle), sin(angle)

def angle_to_line(angle, origin=(0, 0), length=1000):
    segment = angle_to_unit_circle(angle/180*pi)
    return origin, tuple([origin[i] + int(segment[i] * length) for i in range(len(origin))])


# def thresh_color_distance(split, color, distance, use_first_channel=False):
#     threshed = [range_threshold(split[i],
#                 color[i] - distance, color[i] + distance)
#                 for i in range(int(not use_first_channel), len(color))]
#     combined = reduce(lambda x, y: cv2.bitwise_and(x, y), threshed)
#     return combined

def filter_contour_size(contours, size):
    return [c for c in contours if cv2.contourArea(c) > size]

def filter_shapularity(area_function, contours, thresh):
    return [c for c in contours if cv2.contourArea(c)/area_function(c) > thresh]


def find_yellow_circle(split, color, distance, erode_kernel, erode_iterations,
                       dilate_kernel, dilate_iterations, min_contour_size, min_circularity, radius_offset):
    mask, _ = thresh_color_distance(split, color, distance, weights=[0.5, 2, 2])
    mask = erode(mask, rect_kernel(erode_kernel), iterations=erode_iterations)
    mask = dilate(mask, rect_kernel(dilate_kernel), iterations=dilate_iterations)
    # mask = erode(mask, rect_kernel(self.options['circle_erode_kernel']),
    #         iterations=self.options['circle_dilate_iterations']-self.options['circle_erode_iterations'])
    contours = outer_contours(mask)
    contours = filter_contour_size(contours, min_contour_size)
    contours = filter_shapularity(
            lambda c: pi * cv2.minEnclosingCircle(c)[1]**2, contours, min_circularity)

    def circle_with_offset(contour):
        c = cv2.minEnclosingCircle(contour)
        return (int(c[0][0]), int(c[0][1])), max(int((1-radius_offset)*c[1]), 0)

    return [{'contour': c, 'circle': circle_with_offset(c)} for c in contours]


def intersect_circles(circles, mask, min_size):
    for i in range(len(circles)):
        c = circles[i]['circle']
        mask_c = np.zeros(mask.shape, dtype=np.uint8)
        mask_c = cv2.circle(mask_c, *c, 255, -1)
        intersect = cv2.bitwise_and(mask, mask_c)
        if any(map(lambda x: cv2.contourArea(x) > min_size, outer_contours(intersect))):
            return i, mask_c
    return None, None

def crop_by_mask(cvtmat, mask, x, y, r, shrink=False):
    import time
    t = time.time()
    cvtmat = cvtmat[:, :, 1:]  # TODO: make this adjustable
    print('foo a', time.time() - t)
    t = time.time()
    mask = mask[y-r:y+r, x-r:x+r]
    print('foo b', time.time() - t)
    t = time.time()
    cropped = cvtmat[y-r:y+r, x-r:x+r]
    print('foo c', time.time() - t)
    t = time.time()
    cropped = cv2.bitwise_and(cropped, cropped, mask=mask)
    print('foo d', time.time()-t)
    if shrink:
        size = min(cropped.shape[0], cropped.shape[1], shrink)
        cropped = resize(cropped, size, size)
    return cropped

def kmeans_mask(mat, x, y, r, target_centeroid, centeroids=3, remove_noise=True, morph_kernel=5, morph_iterations=3):
    array = mat.reshape((-1, 2))
    array = np.float32(array)
    criteria = (cv2.TERM_CRITERIA_EPS + cv2.TERM_CRITERIA_MAX_ITER, 10, 1.0)
    compactness, labels, centers = cv2.kmeans(array, centeroids, None, criteria, KMEANS_ITER, cv2.KMEANS_RANDOM_CENTERS)
    array = np.uint8(array)
    labels = labels.ravel()
    centers = list(zip(range(len(centers)), centers))
    centers.sort(key=lambda x: color_dist((0, x[1][0], x[1][1]), (0, target_centeroid[1], target_centeroid[2])))
    target_label = centers[0][0]
    post = np.zeros(labels.shape, dtype=np.uint8)
    post[labels == target_label] = 255
    post = post.reshape((mat.shape[0], mat.shape[1]))
    if remove_noise:
        post = morph_remove_noise(post, rect_kernel(morph_kernel), iterations=morph_iterations)
        # post = morph_close_holes(post, rect_kernel(morph_kernel), iterations=morph_iterations)
        pass
    return post

def outline_mask(mask, simplify=True):
    ret = np.zeros(mask.shape, dtype=np.uint8)
    contour = outer_contours(mask)
    contour = max(contour, key=contour_area)
    if simplify:
        contour = contour_approx(contour, epsilon=contour_area(contour)*0.01)
    cv2.drawContours(ret, [contour], -1, 255)
    return ret
