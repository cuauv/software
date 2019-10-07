import cv2


def draw_circle(mat, center, radius, color=(0, 0, 255), thickness=1):
    """
    Draws a circle on the input image. The input image is modified.
    :param mat: input image
    :param center: center of the circle
    :param radius: radius of the circle
    :param color: color of the circle
    :param thickness: thickness of circle boundary; if negative, a filled circle is drawn
    :return: None
    """
    cv2.circle(mat, center, radius, color, thickness=thickness)


def draw_ellipse(mat, radius_x, radius_y, angle, color=(0, 0, 255), thickness=1):
    """
    Draws an ellipse on the input image. The input image is modified.
    :param mat: input image
    :param radius_x: radius in the x-axis
    :param radius_y: radius in the y-axis
    :param angle: rotation angle of the ellipse, in degrees
    :param color: color of the ellipse
    :param thickness: thickness of ellipse boundary; if negative, a filled ellipse is drawn
    :return: None
    """
    cv2.ellipse(mat, (radius_x, radius_y), angle, 0, 0, color, thickness=thickness)


def draw_line(mat, pt1, pt2, color=(0, 0, 255), thickness=1):
    """
    Draws a line on the input image. The input image is modified.
    :param mat: input image
    :param pt1: first point on the line
    :param pt2: second point on the line
    :param color: color of the line
    :param thickness: thickness of the line
    :return: None
    """
    cv2.line(mat, pt1, pt2, color, thickness=thickness)


def draw_arrow(mat, from_pt, to_pt, color=(0, 0, 255), thickness=1):
    """
    Draws an arrow on the input image. The input image is modified.
    :param mat: input image
    :param from_pt: point at the base of the arrow
    :param to_pt: point at the tip of the arrow
    :param color: color of the arrow
    :param thickness: thickness of the arrow
    :return: None
    """
    cv2.arrowedLine(mat, from_pt, to_pt, color, thickness=thickness)


def draw_rect(mat, pt1, pt2, color=(0, 0, 255), thickness=1):
    """
    Draws a rectangle on the input image. The input image is modified.
    :param mat: input image
    :param pt1: vertex of the rectangle
    :param pt2: vertex of the rectangle opposite from pt1
    :param color: color of the rectangle
    :param thickness: thickness of the borders of the rectangle; if negative, a filled rectangle is drawn
    :return: None
    """
    cv2.rectangle(mat, pt1, pt2, color, thickness=thickness)


def draw_text(mat, s, origin, scale, color=(0, 0, 255), thickness=1):
    """
    Draws text on the input image. The input image is modified.
    :param mat: input image
    :param s: text to draw
    :param origin: coordinate of bottom-left corner of text
    :param scale: font scaling factor
    :param color: color of font
    :param thickness: thickness of font
    :return: None
    """
    cv2.putText(mat, s, origin, cv2.FONT_HERSHEY_SIMPLEX, scale, color, thickness=thickness)


def draw_contours(mat, contours, color=(0, 0, 255), thickness=1):
    """
    Draws contours on the input image. The input image is modified.
    :param mat: input image
    :param contours: contours to draw
    :param color: color of contours
    :param thickness: thickness of contours, filled if -1
    :return: None
    """
    cv2.drawContours(mat, contours, -1, color, thickness=thickness)

