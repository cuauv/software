import numpy as np

import shm

from conf.vehicle import buoyancy_force, center_of_buoyancy, gravity_force, \
                         I, drag_planes

mass = gravity_force / 9.81 # in KG

def get_buoyancy_force(depth):
    """
        Returns the buoyancy force on the sub at a given depth.
        Piecewise linear: constant below the surface, 0 above a threshold depth
        and interpolates between the two when the sub is partially above water
    """

    return buoyancy_force
    """
    # causes problems in the pool; floors thrusters at the surface
    if depth > 0:
        return buoyancy_force
    elif depth < -sub_height:
        return 0

    else:
        _m = buoyancy_force / sub_height
        return _m * depth + buoyancy_force
    """

def passive_forces(kalman, tm):
    """
        Given a kalman group, returns the passive forces on the vehicle
        as a (force vector, torque vector) tuple in SUB SPACE
    """
    passives = np.zeros(6)

    if shm.settings_control.buoyancy_forces.get():
        # Calculate passive forces due to buoyancy
        buoyancy_mag = get_buoyancy_force(kalman.depth)
        force_hat = tm.orientation.conjugate() * np.array((0, 0, 1))
        passives[:3] += (gravity_force - buoyancy_mag) * force_hat
        passives[3:] += np.cross(center_of_buoyancy, -buoyancy_mag * force_hat)

    # Calculate passive forces due to drag for each drag plane
    if shm.settings_control.drag_forces.get():
        velocity = np.array((kalman.velx, kalman.vely, kalman.depth_rate))
        # Bring velocities into sub space.
        # We are assuming here that kalman velocities are adjusted for pitch
        # and roll, i.e. in the north east plane always
        # this is probably not the case at all
        velocity = tm.spitz_to_sub_quat * velocity

        # We are taking this to be in the model frame / sub space.
        ang_velocity = np.radians(np.array((kalman.roll_rate,
                       kalman.pitch_rate, kalman.heading_rate)))

        for plane in drag_planes:
            plane_velocity = velocity + np.cross(ang_velocity, plane.pos)
            mag = plane_velocity.dot(plane.n)
            force_mag = 0.5 * 1000 * mag ** 2 * plane.cD * plane.area
            if mag > 0:   # the flow is hitting the backside of the plane
                force_mag = -force_mag

            passives[:3] += force_mag * plane.n
            passives[3:] += force_mag * plane.torque_hat

    return passives
