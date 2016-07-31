import math


def heading_sub_degrees(a: float, b: float) -> float:
    """Calculate the difference from -180 to 180 between two angles.

    a > b will return positive difference, a < b will return negative difference (this example does not account for
    all quadrant. Consider this example in the first quadrant for clarity).

    :param a: First angle.
    :param b: Second angle.
    :return: The angle from -180 to 180 between a and b.
    """

    diff = (a - b) % 360
    if diff > 180:
        return diff - 360
    else:
        return diff


def abs_heading_sub_degrees(a: float, b: float) -> float:
    """
    Returns the absolute (non-negative) difference in heading from 0 to 180 between angles a and b.

    :param a: First angle.
    :param b: Second angle.
    :return: The angle from 0 to 180 between a and b.
    """

    return abs(heading_sub_degrees(a, b))


def average_headings_degrees(h_list):
    # Averages a list of headings
    x = sum([math.sin(math.radians(h)) for h in h_list])
    y = sum([math.cos(math.radians(h)) for h in h_list])
    return math.degrees(math.atan2(x, y)) % 360
