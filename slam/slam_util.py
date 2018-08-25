# Utilities for using SLAM

import numpy as np
import shm

from slam.slam_client import SlamClient

from mission.framework.targeting import ForwardTarget, DownwardTarget

# Vision coords: camera_x, camera_y, dist
# Sub coords: X, Y, Z
# Slam coords: north, east, depth

CAMERA_DIMS = [
    (shm.camera.forward_width.get(), shm.camera.forward_height.get()),
    (shm.camera.downward_width.get(), shm.camera.downward_height.get()),
]

CAMERA_CENTERS = [(CAMERA_DIM[0] / 2, CAMERA_DIM[1] / 2) for CAMERA_DIM in CAMERA_DIMS]

# True for rectangular, False for spherical
# Note: spherical is dumb broke yo, don't use it
RECTANGULAR = True

CAMERA_VIEWING_ANGLES = [
    #[73.7, 73.7],
    #[76.75, 74.6], # measured
    #[103.6, 76.6], # X, Y forward
    [51.8, 38.3],
    [100, 100], # X. Y downward TODO this isn't an actual value
]

def vision_to_sub(x, y, dist, camera):
    theta = np.radians((x - CAMERA_CENTERS[camera][0]) / CAMERA_CENTERS[camera][0] * CAMERA_VIEWING_ANGLES[camera][0])
    phi = np.radians((y - CAMERA_CENTERS[camera][1]) / CAMERA_CENTERS[camera][1] * CAMERA_VIEWING_ANGLES[camera][1])

    if RECTANGULAR:
        dx = dist * np.cos(phi) * np.cos(theta)
        dy = dist * np.cos(phi) * np.sin(theta)
        dz = dist * np.cos(theta) * np.sin(phi)
    else:
        dy = dist * np.sin(theta)
        dz = dist * np.sin(phi)
        dx = np.sqrt(dist**2 - dy**2 - dz**2)

    return np.array([dx, dy, dz])

# Returns just X and Y
def sub_to_vision(sub_coords, camera):
    if RECTANGULAR:
        theta = np.degrees(np.arctan2(sub_coords[1], sub_coords[0]))
        phi = np.degrees(np.arctan2(sub_coords[2], sub_coords[0]))
    else:
        theta = np.degrees(np.arctan2(sub_coords[1], np.sqrt(sub_coords[0]**2 + sub_coords[2]**2)))
        phi = np.degrees(np.arctan2(sub_coords[2], np.sqrt(sub_coords[0]**2 + sub_coords[1]**2)))

    return (theta * CAMERA_CENTERS[camera][0] / CAMERA_VIEWING_ANGLES[camera][0] + CAMERA_CENTERS[camera][0],
            phi * CAMERA_CENTERS[camera][1] / CAMERA_VIEWING_ANGLES[camera][1] + CAMERA_CENTERS[camera][1])

make_rotate = lambda theta: np.array([
    [np.cos(theta), -np.sin(theta), 0],
    [np.sin(theta), np.cos(theta), 0],
    [0, 0, 1],
])

# Convert sub to slam coords. Deals in np arrays.
def sub_to_slam(sub_coords):
    kalman = shm.kalman.get()

    rotate = make_rotate(-np.radians(kalman.heading))

    return np.dot(sub_coords.T, rotate).T

# Convert slam to sub coords. Deals in np arrays.
def slam_to_sub(slam_coords):
    kalman = shm.kalman.get()

    rotate = make_rotate(np.radians(kalman.heading))
    rel_pos = np.array(slam.request_position())

    return np.dot(slam_coords.T - rel_pos, rotate).T

slam = SlamClient()

def check_camera(camera):
    return 1 if camera in ['downward', 'down', 'd'] else 0

def observe(name, x, y, dist, camera=0):
    out = sub_to_slam(vision_to_sub(x, y, dist, check_camera(camera)))
    return slam.observe_landmark(name, *out, uncertainty=1)

def request(name, camera):
    return sub_to_vision(slam_to_sub(np.array(slam.request_landmark(name)[0])), check_camera(camera))

def request_pos_rel(name):
    return tuple(slam_to_sub(np.array(slam.request_landmark(name)[0])))

def request_pos(name):
    pos = list(slam.request_position())
    pos[2] = 0 # don't subtract heading
    return tuple(np.array(slam.request_landmark(name)[0]) - np.array(pos))
