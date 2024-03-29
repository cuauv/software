from math import sqrt
import ctypes
import cv2
import numpy as np

from auv_python_helpers import load_library
from vision.framework.helpers import as_mat


_lib_color_balance = load_library('libauv-color-balance.so')


def _convert_colorspace(conv_type):
    """
    :param conv_type: the type of colorspace conversion
    :return: a function that takes an input image and returns (converted image, target colorspace split)
    """
    def _inner(mat):
        conv = cv2.cvtColor(mat, conv_type)
        return conv, cv2.split(conv)
    return _inner


_conversions = [cv2.COLOR_BGR2LAB, cv2.COLOR_BGR2HSV, cv2.COLOR_BGR2HLS, cv2.COLOR_BGR2YCrCb,
                cv2.COLOR_BGR2LUV, cv2.COLOR_BGR2GRAY, cv2.COLOR_GRAY2BGR]
bgr_to_lab, bgr_to_hsv, bgr_to_hls, bgr_to_ycrcb, bgr_to_luv, bgr_to_gray, gray_to_bgr = [_convert_colorspace(c) for c in _conversions]


def color_dist(c1, c2):
    """
    :param c1: first color (3-dimensional)
    :param c2: second color (3-dimensional)
    :return: euclidean distance between color values
    """
    return sqrt((c1[0] - c2[0])**2 + (c1[1] - c2[1])**2 + (c1[2] - c2[2])**2)


def elementwise_color_dist(mat, c):
    """
    Calculates the elementwise distance between the pixels in an image matrix and a given color
    :param mat: image matrix
    :param c: target color
    :return: a matrix of distance values for each pixel in the image
    """
    return np.linalg.norm(as_mat(mat) - c, axis=2)

def thresh_color_distance(split, color, distance, auto_distance_percentile=None, ignore_channels=[], weights=[1, 1, 1]):
    """
    thresholds the image according to the weighted distance of each pixel to the color
    :param split: a list of monocolored images (see _convert_colorspace)
    :param color: the target color of the threshold
    :param distance: the target distance of the threshold.
    :param auto_distance_percentile: the percentile used for distance
    :param ignore_channels: specifies which color channels should be ignored
    :param weights: the weights to calculated the weighted color distance of each channel
    :return: A tuple of the input image with all pixel values with weighted
             color distance to color larger than distance set to zero and all other
             values set to 255, and a matrix of distances of the point to the specified color.
    """
    for idx in ignore_channels:
        weights[idx] = 0
    weights /= np.linalg.norm(weights)
    dists = np.zeros(split[0].shape, dtype=np.float32)
    for i in range(3):
        if i in ignore_channels:
            continue
        dists += weights[i] * (np.float32(split[i]) - color[i])**2
    if auto_distance_percentile:
        distance = min(np.percentile(dists, auto_distance_percentile), distance**2)
    else:
        distance = distance**2
    return range_threshold(dists, 0, distance), np.uint8(np.sqrt(dists))

def range_threshold(mat, min, max):
    """
    :param mat: input image
    :param min: minimum threshold for pixel value
    :param max: maximum threshold for pixel value
    :return: the input image with all pixel values outside of the range [min, max] set to zero and all values within the
        range set to 255
    """
    return cv2.inRange(mat, min, max)


def binary_threshold(mat, threshold):
    """
    :param mat: input image
    :param threshold: threshold value
    :return: values above the threshold are set to 255 and values less than or equal to the threshold are set to zero
    """
    return cv2.threshold(mat, threshold, 255, cv2.THRESH_BINARY)[1]


def binary_threshold_inv(mat, threshold):
    """
    :param mat: input image
    :param threshold: threshold value
    :return: values above the threshold are set to zero and values less than or equal to the threshold are set to 255
    """
    return cv2.threshold(mat, threshold, 255, cv2.THRESH_BINARY_INV)[1]


def max_threshold(mat, threshold):
    """
    :param mat: input image
    :param threshold: threshold value
    :return: values above the threshold are set to threshold and all other values are unchanged
    """
    return cv2.threshold(mat, threshold, 0, cv2.THRESH_TRUNC)[1]


def above_threshold(mat, threshold):
    """
    :param mat: input image
    :param threshold: threshold value
    :return: values above the threshold are unchanged, and values less than or equal to the threshold are set to zero
    """
    return cv2.threshold(mat, threshold, 0, cv2.THRESH_TOZERO)[1]


def below_threshold(mat, threshold):
    """
    :param mat: input image
    :param threshold: threshold value
    :return: values above the threshold are set to zero, and values less than or equal to the threshold are unchanged
    """
    return cv2.threshold(mat, threshold, 0, cv2.THRESH_TOZERO_INV)[1]


def otsu_threshold(mat):
    """
    Thresholds the image using a threshold value calculated automatically using Otsu's Binarization
    method. The input image must be bimodal, because Otsu's Binarization method automatically
    calculates a threshold value based on the bimodal values.
    :param mat: input image (should be bimodal)
    :return: (threshold value, thresholded image)
    """
    return cv2.threshold(mat, 0, 255, cv2.THRESH_OTSU)


def adaptive_threshold_mean(mat, neighborhood_size, bias=0):
    """
    Applies an adaptive threshold value, calculated as the mean of neighboring values. Values above the threshold are
    set to 255 and values less than or equal to the threshold are set to zero.
    :param mat: input image
    :param neighborhood_size: size of neighborhood for threshold calculation (must be odd)
    :param bias: a constant offset to the calculated mean value
    :return: the thresholded image
    """
    return cv2.adaptiveThreshold(mat, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, neighborhood_size, bias)


def adaptive_threshold_mean_inv(mat, neighborhood_size, bias=0):
    """
    Applies an adaptive threshold value, calculated as the mean of neighboring values. Values above the threshold are
    set to zero and values less than or equal to the threshold are set to 255.
    :param mat: input image
    :param neighborhood_size: size of neighborhood for threshold calculation (must be odd)
    :param bias: a constant offset to the calculated mean value
    :return: the thresholded image
    """
    return cv2.adaptiveThreshold(mat, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, neighborhood_size, bias)


def adaptive_threshold_gaussian(mat, neighborhood_size, bias=0):
    """
    Applies an adaptive threshold value, calculated as a weighted sum of neighboring values. The weights are a Gaussian
    window. Values above the threshold are set to 255 and values less than or equal to the threshold are set to zero.
    :param mat: input image
    :param neighborhood_size: size of neighborhood for threshold calculation (must be odd)
    :param bias: a constant offset to the calculated weighted mean value
    :return: the thresholded image
    """
    return cv2.adaptiveThreshold(mat, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, neighborhood_size, bias)


def adaptive_threshold_gaussian_inv(mat, neighborhood_size, bias=0):
    """
    Applies an adaptive threshold value, calculated as a weighted sum of neighboring values. The weights are a Gaussian
    window. Values above the threshold are set to zero and values less than or equal to the threshold are set to 255.
    :param mat: input image
    :param neighborhood_size: size of neighborhood for threshold calculation (must be odd)
    :param bias: a constant offset to the calculated weighted mean value
    :return: the thresholded image
    """
    return cv2.adaptiveThreshold(mat, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, neighborhood_size, bias)


def kmeans(mat, num_centeroids, iterations=10, epsilon=1.0):
    """
    Performs kmeans clustering on the colors in an image. It clusters the
    colors as three dimensional points and separates them into different clusteroids.

    The function returns a measure of how close the samples are to each center,
    the best suited label for each point on the image, and the centeroids of
    each label.

    :param mat: The image to be clustered
    :param num_centeroids: The number of clusters/centeroids to be used when
                           running the algorithm
    :param iterations: The number of iterations to run the kmeans algorithm
    :param epsilon: The epsilon to terminate the algorithm when it is reached
    """
    array = mat.reshape((-1, 2))
    array = np.float32(array)
    criteria = (cv2.TERM_CRITERIA_MAX_ITER + cv2.TERM_CRITERIA_EPS, 10, 1.0)
    compactness, labels, centers = cv2.kmeans(array, num_centeroids, None, criteria, KMEANS_ITER, cv2.KMEANS_RANDOM_CENTERS)
    compactness =  np.reshape((mat.shape[0], mat.shape[1]))
    labels = np.reshape((mat.shape[0], mat.shape[1]))
    return compactness, labels, centers


def mask_from_labels(labels, centers):
    """
    Generates masks from labels generated from kmeans.

    Generates one mask from each centeroid in centers. Basically, all points in
    the same label is set to 255, while all other points are set to 0.
    :param labels: The labels generated by kmeans.
    :param centers: The centeroids generated by kmeans.
    """
    acc = []
    for i, c in enumerate(centers):
        mask = np.zeros(labels.shape, dtype=np.uint8)
        mask[labels==target_label] = 255
        acc.append(mask)
    return acc

def mask_from_labels_target_color(labels, centers, target_color, distance_func=color_dist):
    """
    Generates masks from labels generated from kmeans.

    Generates one mask based on the label closest to target_color. Basically,
    all points in the same label is set to 255, while all other points are set
    to 0.  Generates one mask from each centeroid in centers.
    :param labels: The labels generated by kmeans.
    :param centers: The centeroids generated by kmeans.
    :param target_color: The target color used to choose the label generating the masks
    :param distance_func: The function that takes in two 3-channel colors to
                          calculate the color distance of the colors. (ie, you can specify a custom
                          function to ignore channels, weight different channels, etc.)
    """
    target_label = min(enumerate(centers), key= lambda x: distance_fun(x, target_color))
    ret = np.zeros(lables.shape, dtype=np.uint8)
    ret[labels==target_label] = 255
    return ret


def color_correct(mat, equalize_rgb=True, rgb_contrast_correct=False,
                  hsv_contrast_correct=True, hsi_contrast_correct=False,
                  rgb_extrema_clipping=True, adaptive_cast_correction=False,
                  horizontal_blocks=1, vertical_blocks=1):
    """
    Applies color correction (color cast correction and contrast correction)
    :param mat: input image
    :param equalize_rgb: whether to apply color cast correction
    :param rgb_contrast_correct: whether to apply RGB-based contrast correction
    :param hsv_contrast_correct: whether to apply HSV-based contrast correction
    :param hsi_contrast_correct: whether to apply HSI-based contrast correction
    :param rgb_extrema_clipping: whether to clip outliers in terms of RGB values
    :param adaptive_cast_correction: whether to adaptively calculate gains for color cast correction
    :param horizontal_blocks: number of horizontal blocks used for color cast correction
    :param vertical_blocks: number of vertical blocks used for color cast correction
    :return:
    """
    rows = mat.shape[0]
    cols = mat.shape[1]
    depth = 3

    # Convert to one-dimensional uint8 array and pass into C++ module
    c_uint8_p = ctypes.POINTER(ctypes.c_int8)
    data = mat.flatten()
    data_p = data.ctypes.data_as(c_uint8_p)
    _lib_color_balance.process_frame(data_p, rows, cols, depth, equalize_rgb,
                                     rgb_contrast_correct, hsv_contrast_correct, hsi_contrast_correct,
                                     rgb_extrema_clipping, adaptive_cast_correction, horizontal_blocks, vertical_blocks)
    # Convert to matrix of original shape
    mat = np.ctypeslib.as_array(data_p, (rows, cols, depth)).astype(np.uint8)
    return mat
