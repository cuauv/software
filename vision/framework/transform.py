import cv2
import numpy as np


def simple_gaussian_blur(mat, kernel_size: int, std_dev: float):
    """
    Blurs an image using a Gaussian filter with a square kernel
    :param mat: input image
    :param kernel_size: size of convolution kernel (must be odd)
    :param std_dev: standard deviation of Gaussian kernel in the x- and y-axes
    :return: blurred image
    """
    return cv2.GaussianBlur(mat, (kernel_size, kernel_size), std_dev)


def elliptic_kernel(x, y=None):
    """
    Returns an elliptic kernel for use in morphological transforms
    :param x: length of the x-axis (must be odd)
    :param y: length of the y-axis (must be odd); if None, `x` is used
    :return: the kernel
    """
    if y is None:
        y = x
    return cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (x, y))


def rect_kernel(x, y=None):
    """
    Returns a rectangular kernel for use in morphological transforms
    :param x: length of the x-axis (must be odd)
    :param y: length of the y-axis (must be odd); if None, `x` is used
    :return: the kernel
    """
    if y is None:
        y = x
    return cv2.getStructuringElement(cv2.MORPH_RECT, (x, y))


def erode(mat, kernel, iterations=1):
    """
    Applies an erosion to the input image, decreasing the size of white objects at their boundaries
    :param mat: input image
    :param kernel: kernel for morphological transforms
    :param iterations: number of iterations to apply erosion
    :return: eroded image
    """
    return cv2.erode(mat, kernel, iterations=iterations)


def dilate(mat, kernel, iterations=1):
    """
    Applies a dilation to the input image, increasing the size of white objects at their boundaries
    :param mat: input image
    :param kernel: kernel for morphological transforms
    :param iterations: number of iterations to apply dilation
    :return: dilated image
    """
    return cv2.dilate(mat, kernel, iterations=iterations)


def morph_remove_noise(mat, kernel, iterations=1):
    """
    Applies an opening operation (erosion followed by dilation) to the input image, removing noise
    :param mat: input image
    :param kernel: kernel for morphological transforms
    :param iterations: number of iterations to apply opening
    :return: opened image
    """
    return cv2.morphologyEx(mat, cv2.MORPH_OPEN, kernel, iterations=iterations)


def morph_close_holes(mat, kernel, iterations=1):
    """
    Applies a closing operation (dilation followed by erosion) to the input image, closing small holes in the foreground
    object (i.e., black points surrounded by white)
    :param mat: input image
    :param kernel: kernel for morphological transforms
    :param iterations: number of iterations to apply closing
    :return: closed image
    """
    return cv2.morphologyEx(mat, cv2.MORPH_CLOSE, kernel, iterations=iterations)


def morph_borders(mat, kernel, iterations=1):
    """
    Computes the difference between a dilation and an erosion of the input image, returning a morphological gradient,
    i.e., the border of the foreground object(s).
    :param mat:
    :param kernel:
    :param iterations:
    :return: the morphological gradient
    """
    return cv2.morphologyEx(mat, cv2.MORPH_GRADIENT, kernel, iterations=iterations)


def resize(mat, width, height):
    """
    Resize the input image to width x height
    :param mat: input image
    :param width: new width
    :param height: new height
    :return: resized image
    """
    return cv2.resize(mat, (width, height))


def rotate(mat, degrees):
    """
    Rotates the input image by a given number of degrees about its center.
    Border pixels are extrapolated by replication.
    :param mat: input image
    :param degrees: number of degrees to rotate (positive is counter-clockwise)
    :return: rotated image
    """
    rot_mat = cv2.getRotationMatrix2D((mat.shape[1] / 2, mat.shape[0] / 2), degrees, 1)
    return cv2.warpAffine(mat, rot_mat, (mat.shape[1], mat.shape[0]),
                          borderMode=cv2.BORDER_REPLICATE)


def translate(mat, x, y):
    """
    Translates the input image
    :param mat: input image
    :param x: amount to translate in the x-axis
    :param y: amount to translate in the y-axis
    :return: translated image
    """
    trans_mat = np.float32([[1, 0, x],
                            [0, 1, y]])
    return cv2.warpAffine(mat, trans_mat, (mat.shape[1], mat.shape[0]))

