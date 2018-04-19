import math


def heading_sub_degrees(a, b, mod=360):
    """
    Calculate the difference from -mod to mod between two angles.

    a > b will return positive difference, a < b will return negative difference
    (this example does not account for all quadrant. Consider this example in
    the first quadrant for clarity).

    :param a: First angle.
    :param b: Second angle.
    :return: The angle from -mod to mod between a and b.
    """

    diff = (a - b) % mod
    if diff > mod / 2:
        return diff - mod
    else:
        return diff


def abs_heading_sub_degrees(a, b, mod=360):
    """
    Returns the absolute (non-negative) difference in heading from 0 to 180
    between angles a and b.

    :param a: First angle.
    :param b: Second angle.
    :return: The angle from 0 to 180 between a and b.
    """

    return abs(heading_sub_degrees(a, b, mod))


def average_headings_degrees(h_list):
    """ Averages a list of headings """
    x = sum([math.sin(math.radians(h)) for h in h_list])
    y = sum([math.cos(math.radians(h)) for h in h_list])
    return math.degrees(math.atan2(x, y)) % 360
