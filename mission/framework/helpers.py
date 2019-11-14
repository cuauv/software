import numpy as np

import shm

from auv_math.quat import Quaternion
from auv_python_helpers.angles import abs_heading_sub_degrees
from conf.vehicle import cameras

def get_camera(results_group):
    direction = results_group.camera.decode('utf-8')
    if direction not in cameras:
        # TODO Maybe exceptions are better here?
        return None

    return cameras[direction]

def get_camera_size(results_group):
    """
        Returns the size of the camera used by the vision module that
        produced the passed in results group as (width, height).
    """
    cam = get_camera(results_group)
    if cam is None:
        return None

    return cam['width'], cam['height']

def get_camera_center(results_group):
    """
        Returns the center of the camera used by the vision module that
        produced the passed in results group as (pixel_x, pixel_y).
    """
    size = get_camera_size(results_group)
    if size is None:
        return None

    return size[0] / 2.0, size[1] / 2.0

def get_sub_quaternion(kalman=None):
    if kalman is None:
      kalman = shm.kalman.get()
    return Quaternion(q=[kalman.q0, kalman.q1, kalman.q2, kalman.q3])

def get_sub_position(kalman=None):
    if kalman is None:
      kalman = shm.kalman.get()
    return np.array((kalman.north, kalman.east, kalman.depth))

def call_if_function(value):
    # TODO: Split iterable version into separate function?
    """Get a usable value.

    If the passed in argument is a function, its result is returned. If the passed in argument is iterable,
    a tuple of the values is returned with this function recursively applied to each element.

    Args:
        value: The value to be processed.

    Returns: A value that is ether the argument, the result of calling of the argument or a tuple of values that have
    been called if the element is a function.
    """
    if type(value) == str:
        return value
    if callable(value):
        return value()
    elif hasattr(value, "__iter__"):
        return tuple(map(call_if_function, value))
    else:
        return value


def within_deadband(a, b, deadband, use_mod_error):
    """Check if two values are close enough, even on a circle.

    Args:
        a (float): The first value to compare.
        b (float): The second value to compare.
        deadband (float): The acceptable deadband for the deadband. This is exclusive.
        use_mod_error (bool): If True, the calculation is performed using modular arithmetic respecting the mod
            argument. Otherwise, the calculation is performed ignoring the mod argument.
        use_mod_error (float): If use_mod_error is True, the calculation is performed mod 360.

    Returns:
        (bool): A boolean that represents if the two values are within the deadband of each other.
    """

    if use_mod_error:
        return abs_heading_sub_degrees(a, b) < deadband
    else:
        return abs(a - b) < deadband


def should_run(task, finite):
    return not finite or not task.has_ever_finished

def should_finish(task, finite):
    return task.finished or (finite and task.has_ever_finished)


def dict_join(d1, d2):
    """
    Join two dictionaries using a shallow copy and prioritizing the first dictionary.

    :param d1: The first dictionary
    :param d2: The second dictionary
    :return: The combined dictionary
    """

    new = dict(d1)
    for k in d2:
        if k not in new:
            new[k] = d2[k]
    return new
