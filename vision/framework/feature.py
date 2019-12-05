import cv2
import numpy as np


def outer_contours(mat):
    """
    Extracts all outermost contours in the image
    :param mat: input image; the image should be grayscale
    :return: contours in the image
    """
    contours, _ = cv2.findContours(mat, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
    return contours


def simple_canny(mat, sigma=0.33, use_mean=False):
    """
    Performs Canny edge detection on the image using automatically computed lower and upper
    thresholds for hysteresis.

    Canny edge detection identifies edges in an image via a sequence of Gaussian noise filtering,
    calculation of an intensity gradient over the image, filtering out any pixels which do not lie
    on an edge (are not a local maximum in the direction of the gradient), and finally identifying
    edges via a dual-threshold hysteresis on the intensity gradient values. Any pixels connected to
    pixels that are above the high threshold are assumed to be part of the edge. Any pixels below
    the low threshold are discarded.

    Source: https://www.pyimagesearch.com/2015/04/06/zero-parameter-automatic-canny-edge-detection-with-python-and-opencv/
    :param mat: input image
    :param sigma: deviation from median used to compute upper and lower thresholds for hysteresis
    :param use_mean: whether to calculate deviation from arithmetic mean instead of median
    :return: None
    """
    mid = np.mean(mat) if use_mean else np.median(mat)
    lower = int(max(0, (1.0 - sigma) * mid))
    upper = int(min(255, (1.0 + sigma) * mid))
    return cv2.Canny(mat, lower, upper)


def find_corners(mat, max_corners, quality_thresh=0.01, min_distance=10):
    """
    Detects corners in the image using the Shi-Tomasi method, which is a variant of the Harris
    Corner Detector. The Harris Corner Detector identifies corners by locating rectangular
    subregions of an image that would introduce large changes into the image when moved. Corners are
    considered good features for tracking, because corners are invariant to translation, rotation,
    and illumination.

    :param mat: input image; the image should be grayscale
    :param max_corners: maximum number of corners to detect
    :param quality_thresh: minimum quality threshold for corners
    :param min_distance: minimum distance between detected corners
    :return: detected corners
    """
    return cv2.goodFeaturesToTrack(mat, max_corners, quality_thresh, min_distance)


def find_circles(mat, res_ratio, min_distance, canny_thresh=100, circle_thresh=100, min_radius=0, max_radius=0):
    """
    Find circles using the circle Hough Transform. The circle Hough Transform detects circles by
    voting for different coordinates within the Hough parameter space ((x, y, r) for a circle center
    and radius) and identifying local maxima in the accumulator.

    :param mat: input image; it is recommended to apply noise reduction beforehand
    :param res_ratio: inverse ratio of accumulator resolution to image resolution. A higher value indicates smaller
    accumulator resolution. At smaller accumulator resolutions, nearby circle centers may be combined into a single one.
    :param min_distance: minimum distance between circle centers
    :param canny_thresh: value used to scale upper and lower thresholds for Canny detector
    :param circle_thresh: accumulator threshold for circle centers
    :param min_radius: minimum radius of circles
    :param max_radius: maximum radius of circles
    :return: detected circles (a list of (x, y, radius))
    """
    return cv2.HoughCircles(mat, cv2.HOUGH_GRADIENT, res_ratio, min_distance, param1=canny_thresh, param2=circle_thresh,
                            minRadius=min_radius, maxRadius=max_radius)


def line_polar_to_cartesian(rho, theta):
    """
    Converts a line in polar coordinates to a pair of points in cartesian coordinates
    :return: (x1, y1, x2, y2) corresponding to two points (x1, y1) and (x2, y2) on the line
    """
    a = np.cos(theta)
    b = np.sin(theta)
    x0 = a*rho
    y0 = b*rho
    x1 = int(x0 + 1000*(-b))
    y1 = int(y0 + 1000*a)
    x2 = int(x0 - 1000*(-b))
    y2 = int(y0 - 1000*a)
    return (x1, y1, x2, y2)


def find_lines(mat, rho, theta, threshold):
    """
    Finds lines using the standard Hough Transform.  The standard Hough Transform detects lines by
    voting for different coordinates within the Hough parameter space ((r, theta) for the polar
    coordinates of the line) and identifying local maxima in the accumulator.

    :param mat: input image
    :param rho: distance resolution of the accumulator (in pixels)
    :param theta: angle resolution of the accumulator (in radians)
    :param threshold: accumulator threshold for lines
    :return: detected lines (a list of (x1, y1, x2, y2) and a list of (r, theta), where each line
    (segment) is represented by a pair of points with cartesian coordinates (x1, y1) and (x2, y2) on
    the line as well as a pair of polar coordinates (r, theta))
    """
    cartesian_points = []
    polar_points = []
    lines = cv2.HoughLines(mat, rho, theta, threshold)
    if lines is not None:
        for line in lines:
            (rho, theta) = line[0]
            cartesian_points.append(line_polar_to_cartesian(rho, theta))
            polar_points.append((rho, theta))
    return cartesian_points, polar_points


def find_line_segments(mat, rho, theta, threshold, min_line_length=0, max_line_gap=0):
    """
    Finds line segments using the Probabilistic Hough Transform, a more efficient implementation of
    the standard Hough Transform.
    :param mat: input image
    :param rho: distance resolution of the accumulator (in pixels)
    :param theta: angle resolution of the accumulator (in radians)
    :param threshold: accumulator threshold for lines
    :param min_line_length: minimum line length
    :param max_line_gap: maximum allowed gap between points on the same line
    :return: detected line segments (a list of (x1, y1, x2, y2), where each line segment is represented by its endpoints
        (x1, y1) and (x2, y2)
    """
    return cv2.HoughLinesP(mat, rho, theta, threshold, minLineLength=min_line_length, maxLineGap=max_line_gap)


def contour_centroid(contour):
    """
    :param contour:
    :return: the centroid (center of mass) of the contour
    """
    moments = cv2.moments(contour)
    m00 = max(1e-10, moments['m00'])
    return int(moments['m10'] / m00), int(moments['m01'] / m00)


def contour_area(contour):
    """
    :param contour:
    :return: the area of the contour
    """
    return cv2.contourArea(contour, oriented=False)


def contour_perimeter(contour, closed=True):
    """
    :param contour:
    :param closed: whether the contour is closed
    :return: the perimeter of the contour if it is closed; otherwise, the arclength of the contour
    """
    return cv2.arcLength(contour, closed)


def contour_approx(contour, epsilon=None, closed=True):
    """
    Returns an approximation of the given contour using fewer vertices
    :param contour:
    :param epsilon: maximum distance between approximated contour and original contour
    :param closed: whether the contour is closed
    :return: the approximated contour
    """
    if epsilon is None:
        epsilon = 0.1 * contour_perimeter(contour, closed)
    return cv2.approxPolyDP(contour, epsilon, closed)


def min_enclosing_rect(contour):
    """
    Returns a minimum area enclosing rectangle of the contour
    :param contour:
    :return: ((center_x, center_y), (width, height), rotation angle in degrees) of the rectangle
    """
    return cv2.minAreaRect(contour)


def min_enclosing_circle(contour):
    """
    Returns a minimum area enclosing circle of the contour
    :param contour:
    :return: ((center x, center y), radius) of the circle
    """
    return cv2.minEnclosingCircle(contour)


def min_enclosing_ellipse(contour):
    """
    Returns an ellipse in which the contour is inscribed. The ellipse is fitted to the contour using a least-squares
    approximation, so the contour may not be entirely enclosed.
    :param contour:
    :return: ((center x, center y), (radius x, radius y), rotation angle in degrees) of the ellipse
    """
    return cv2.fitEllipse(contour)

