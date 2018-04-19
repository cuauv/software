#!/usr/bin/env python3
import time
import math
import shm
from auv_math.math_utils import inverse_clamp
from pid import DynamicPID

NAVIGATION_OPTIMIZE_DEADBAND = 2

def main(freq=30):
    settings = shm.navigation_settings.get()

    # Init position -> velocity controllers
    vel_x_out = DynamicPID(settings.x_pid_p, settings.x_pid_i,
                           settings.x_pid_d)
    vel_y_out = DynamicPID(settings.y_pid_p, settings.y_pid_i,
                           settings.y_pid_d)

    rate = 1/freq
    start = time.time()
    while True:
        if (time.time() - start > rate):
            desires = shm.desires.get()
            nd = shm.navigation_desires.get()
            kalman = shm.kalman.get()
            settings = shm.navigation_settings.get()

            desires.heading = nd.heading
            desires.pitch = nd.pitch
            desires.roll = nd.roll
            desires.depth = nd.depth

            # Position specific trajectories, navigation
            if settings.position_controls:
                delta_n = kalman.north - nd.north
                delta_e = kalman.east - nd.east

                heading = shm.kalman.heading.get()
                delta_x = math.cos(math.radians(heading))*delta_n + \
                          math.sin(math.radians(heading))*delta_e
                delta_y = -math.sin(math.radians(heading))*delta_n + \
                          math.cos(math.radians(heading))*delta_e

                x_out = vel_x_out.tick(delta_x, 0, p=settings.x_pid_p,
                                                   i=settings.x_pid_i,
                                                   d=settings.x_pid_d)
                y_out = vel_y_out.tick(delta_y, 0, p=settings.y_pid_p,
                                                   i=settings.y_pid_i,
                                                   d=settings.y_pid_d)

                def condition(error, value, liveband):
                    if abs(error) > settings.deadband:
                        return inverse_clamp(value, liveband)
                    return 0

                x_out = condition(delta_x, x_out, settings.min_x_speed)
                y_out = condition(delta_y, y_out, settings.min_y_speed)


                # Only adjust speed components if it matters
                initial_speed = math.sqrt(x_out**2 + y_out**2)
                max_speed = settings.max_speed
                if max_speed < initial_speed:
                    x_out *= max_speed / initial_speed
                    y_out *= max_speed / initial_speed

                desires.speed = x_out
                desires.sway_speed = y_out

                if settings.optimize:
                    mag = math.sqrt(delta_n**2 + delta_e**2)
                    if mag > NAVIGATION_OPTIMIZE_DEADBAND:
                        direction = math.atan2(-delta_e, -delta_n)
                        direction = math.degrees(direction)
                        shm.navigation_desires.heading.set(direction)

            # Otherwise, "naively" let velocity desires fall through
            else:
                desires.speed = nd.speed
                desires.sway_speed = nd.sway_speed
                shm.navigation_desires.north.set(kalman.north)
                shm.navigation_desires.east.set(kalman.east)

            shm.desires.set(desires)

            start = time.time()
        else:
            time.sleep(rate/2)

if __name__ == "__main__":
    main()
