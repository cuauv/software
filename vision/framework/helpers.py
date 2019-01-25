import cv2


def to_odd_linear(n):
    """
    Converts a number to an odd number using a linear function
    :param n: the number
    :return: and odd number obtained by applying a linear function to the input
    """
    return 2 * n + 1


def to_odd(n):
    """
    Converts a number to an odd number by getting the nearest odd number (greater than or equal to the input)
    :param n: the number
    :return: and odd number
    """
    return n if n % 2 == 1 else n + 1


def to_umat(mat):
    """
    Converts an image matrix to a unified matrix in the Transparent API for transparent access to
    both CPU/OpenCL-accelerated codepaths.
    :param mat: image
    :return: UMat for image
    """
    return cv2.UMat(mat)


def from_umat(umat):
    """
    Converts a unified image matrix to a standard image matrix.
    :param umat: image
    :return: matrix for image
    """
    return cv2.UMat.get(umat)


def as_mat(mat):
    """
    Converts an image matrix (either in standard form or unified form) to a standard image matrix
    :param mat: image
    """
    return from_umat(mat) if isinstance(mat, cv2.UMat) else mat

