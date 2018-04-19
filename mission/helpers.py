"""Helper functions common to all mission tsaks"""

import itertools
from math import radians, sqrt, cos, sin, tan, atan, degrees, atan2

import numpy
import scipy.stats

import shm

from mission.framework.helpers import get_sub_position

# Function returns a list where elements are sorted by their distance from element t in list x
# Used for pipe finding logic
def priority_list_gen(x, t):
    i = x.index(t)
    l1 = x[0:i]
    l1.reverse()
    l2 = x[i + 1:]

    def pr(a, b):
        if not a:
            return b
        else:
            return [a[0]] + pr(b, a[1:])

    return [t] + pr(l1, l2)


# Used for determining offsets while approaching a target and holding it in a certain spot in the camera view.
# Returns x and y offsets which scale based on distance from the target, keeping a roughly constant real world offset
# between the axis of the camera and of the target.
#
# final_offsets are the desired offsets (x,y) in pixels when current_area == final_area.
# Will not return offsets larger than final_offsets.
def scaled_offsets(vehicle, final_area, final_offsets, current_area, forward=True):
    if (final_area == 0) or (current_area == 0) or (final_offsets == (0, 0)):
        return 0, 0

    screen_width = 0
    screen_height = 0
    if forward:
        screen_width = vehicle.sensors.forward_width
        screen_height = vehicle.sensors.forward_height
    else:
        screen_width = vehicle.sensors.downward_width
        screen_height = vehicle.sensors.downward_height
    screen_area = screen_width * screen_height
    if screen_area is 0:
        return 0, 0

    # View angle from center of screen to edge of screen.
    max_angle_x = 0
    max_angle_y = 0
    if forward:
        max_angle_x = 35
        max_angle_y = 26.25
    else:
        max_angle_x = 27.5
        max_angle_y = 20.625

    final_x, final_y = final_offsets

    # Calculates the constant 'real' offsets and area based off of the given final conditions.
    #
    # All 'real' values are in arbitrary units of distance and area based off the distance variable (below) being
    # equal to 1 when current_area = final_area.
    real_x = tan(radians(2 * max_angle_x * (float(final_x) / float(screen_width))))
    real_y = tan(radians(2 * max_angle_y * (float(final_y) / float(screen_height))))
    real_area = (screen_area / final_area) / (4 * tan(radians(max_angle_x)) * tan(radians(max_angle_y)))

    # Distance is 1 when (current_area = final_area). Distance decreases as current_area approaches screen_area (as
    # we get closer).
    distance = sqrt(
        (screen_area / current_area) / (real_area * (4 * tan(radians(max_angle_x)) * tan(radians(max_angle_y)))))

    # Subfunction to calculate an individual offset.
    def get_offset(screen_height_or_width, real_offset, distance, max_angle, final_offset):
        offset = ((.5 * screen_height_or_width) * degrees(atan2(real_offset, distance)) / max_angle)
        # Do not return an offset larger than the final offset.
        if abs(offset) > abs(final_offset):
            return final_offset
        return offset

    # Returns the x and y offsets.
    x_offset = get_offset(screen_width, real_x, distance, max_angle_x, final_x)
    y_offset = get_offset(screen_height, real_y, distance, max_angle_y, final_y)
    offsets = (x_offset, y_offset)
    return offsets


# Will return a speed between min and max speeds based on value. Can also be used for other variables besides speed.
def scaled_speed(final_value=0, initial_value=0, final_speed=0, initial_speed=0, current_value=0):
    # Check to make sure reasonable values were passed in.
    if initial_value > final_value:
        temp = initial_value
        initial_value = final_value
        final_value = temp
        temp = initial_speed
        initial_speed = final_speed
        final_speed = temp

    output_speed = 0
    if current_value >= final_value:
        # If we are past final value, go at final speed.
        output_speed = final_speed
    elif current_value <= initial_value:
        # If we are below initial value, go at initial speed.
        output_speed = initial_speed
    else:
        # Find the fraction of the value range which we've covered
        value_range = final_value - initial_value + 0.0
        value_fraction = ((current_value - initial_value + 0.0) / (value_range * 1.0))
        # Then multiply that fraction by the speed range to get the speed difference to add to the minimum speed.
        speed_range = final_speed - initial_speed + 0.0
        output_speed = (speed_range * value_fraction) + initial_speed + 0.0

    return output_speed


def downward_position(camera_position=(0, 0)):
    """
    distance = (objsize) * (camsize) / (image size * tan(theta)

    very similar to distance_from_size
    """

    # TODO: add pitch and roll compensation

    pos = get_sub_position()[:2]
    altitude = shm.dvl.savg_altitude.get()

    heading_rad = radians(shm.kalman.heading.get())
    heading_v = numpy.array((cos(heading_rad), sin(heading_rad)))
    heading_yv = numpy.array((-heading_v[1], heading_v[0]))

    screen_width = shm.camera.downward_width.get()
    screen_height = shm.camera.downward_height.get()
    screen_area = screen_width * screen_height
    if screen_area == 0:
        return numpy.array((0, 0))

    from locator.camera import compute_fov_from_lens_params, DOWNWARD_LENS_PARAMS
    h_fov, v_fov = compute_fov_from_lens_params(*DOWNWARD_LENS_PARAMS)

    max_angle_x = 2 * tan(h_fov / 2)
    max_angle_y = 2 * tan(v_fov / 2)

    camera_x, camera_y = camera_position
    camera_x -= .5 * screen_width
    camera_y -= .5 * screen_height

    x_scale = (camera_x / screen_width) * max_angle_x
    y_scale = (camera_y / screen_height) * max_angle_y

    x_distance = -1 * altitude * y_scale
    y_distance = altitude * x_scale

    return pos + (x_distance * heading_v) + (y_distance * heading_yv)


def distance_from_size(camera_size=0, field_of_view=0, object_size=0, image_size=0):
    """ Determines distance in real world units (meters) from size in pixels.

    Can be used for both height and width field of view is in degrees camera_height and image_size are in pixels
    object_height is in any real world length unit returns: distance to object in the real world length unit
    """
    return (object_size * camera_size) / \
           (image_size * 2 * tan(radians(field_of_view / 2.0)))


# Uses downward_position because something is wrong :(
# But should be revisited perhaps after this summer (2013)
# to take into account pitch and roll
def screen_point_to_world_point(x, y):
    """
    Converts a point (x,y) in the downward camera to a point (north,east) on the ground.

    Takes into account altitude and attitude (heading, pitch, roll)
    """
    # Measured by inspection of bins
    pitch_fov = radians(41.)
    roll_fov = radians(32.5)
    screen_height = shm.camera.downward_height.get()
    screen_width = shm.camera.downward_width.get()

    # Centers the coordinates, flips y
    x = (x - screen_width / 2.)
    y = (screen_height / 2. - y)

    # Angle from sub to the point
    # (opp/adj)/(opp/adj) =
    # tan(angle)/tan(fov) = x/(half screen size)
    # so angle = atan( x*tan(fov)*2/screen size)
    eff_pitch = shm.kalman.pitch.get() + degrees(atan(y * tan(pitch_fov) * 2 / screen_height))
    eff_roll = -shm.kalman.roll.get() + degrees(atan(x * tan(roll_fov) * 2 / screen_width))

    # Calculate displacements in vehicle frame
    h = shm.dvl.savg_altitude.get()
    forward = tan(radians(eff_pitch)) * h
    sway = tan(radians(eff_roll)) * h

    heading = shm.kalman.heading.get()
    n = shm.kalman.north.get() + forward * cos(heading) - sway * sin(heading)
    e = shm.kalman.east.get() + forward * sin(heading) + sway * cos(heading)
    return numpy.array((n, e))


def cluster(point_list, std, chance_correct):
    """
    Given a list of points (x,y), the standard deviation of the point position measurements,
    and the chance that the point is correctly identified as part of the group,
    this returns the expectation point of where the object is

    This is useful for the following situation:

    We are getting points, say from vision, that are either approximately the correct point or are something being
    mis-identified.

    We don't want to simply take the average of the points, since that would include the 'bad' points along with the
    good. This gives a way to average only the good points, assuming that the bad points are infrequent enough.

    This was originally written for SlagathorIV, the bins 2013 task, but was eventually replaced
    with it's 'binning' approach.
    """

    # Probability time!
    # We assume points are either 'correct' or 'wrong'
    # if they're correct then they're drawn from a Gaussian distribution
    # with standard deviation std

    # We try each possible combination of 'correct' points and take the
    # 'true position' to be the average of correct points
    # then test how likely it is to get that collection about this true point
    best_probability = 0
    best = (0, 0)

    # N is total number of points, K is number of 'correct' points
    N = len(point_list)
    for K in range(N, 0, -1):
        # Chance of getting this many 'correct' points
        p_H = chance_correct ** K * (1 - chance_correct) ** (N - K)

        # Short-circuit if we're doing better than we can ever
        # possibly do with fewer 'correct' points
        if best_probability > p_H:
            break

        # try all subsets of possible 'correct' points
        for subset in itertools.combinations(point_list, K):
            # std of average is std/sqrt(k), so
            std_total = sqrt((1. + 1. / K)) * std

            avg_n = float(sum([n for n, e in subset])) / len(subset)
            avg_e = float(sum([e for n, e in subset])) / len(subset)

            dists = [sqrt((n - avg_n) ** 2 + (e - avg_e) ** 2) for n, e in subset]
            z_scores = [d / std_total for d in dists]
            ps = [2 * scipy.stats.norm.sf(z) for z in z_scores]

            p_E_H = numpy.product(ps)
            p_H_E = p_H * p_E_H

            if p_H_E > best_probability:
                best_probability = p_H_E
                best = (avg_n, avg_e)
    return best


# must be used with continuous data streams
class ProgressiveMedianFilterer:
    def __init__(self, n=10, t=0.2):
        self.n, self.t, self.vals = n, t, []

    def call(self, val):
        res = val
        if len(self.vals) < self.n:
            self.vals.append(res)
            return res
        else:
            dev = abs((res / numpy.mean(self.vals)) - 1)
            if dev < self.t:
                self.vals.pop()
                self.vals.insert(0, res)
                return res
            else:
                return self.vals[self.n - 1]


def filtered(func):
    f = ProgressiveMedianFilterer()

    def wrapper(*args, **kwargs):
        return f.call(func(*args, **kwargs))

    return wrapper
