from webserver import BaseHandler, APIHandler
import shm

SHM_GROUP = shm.navigation_desires

def bound(value, negative_max, positive_min):
    if value < 0:
        return max(value, negative_max)
    else:
        return min(value, positive_min)

class DriveHandler(BaseHandler):

    def get(self):
        self.write(self.render_template("drive.html"))

class ZeroHandler(APIHandler):

    def post(self, zero):
        SHM_GROUP.speed.set(0)
        SHM_GROUP.sway_speed.set(0)
        SHM_GROUP.depth.set(shm.kalman.depth.get())
        self.respond_success("Sub zero'd")

class MovementHandler(APIHandler):

    def post(self, axis, value):
        try:
            value = float(value)
        except ValueError:
            self.respond_failure("Invalid value for movement")
            return

        if axis == "z":
            if abs(value) < .2:
                current_depth = SHM_GROUP.depth.get()
                SHM_GROUP.depth.set(current_depth + value)
                self.respond_success("Depth desire at {}".format(SHM_GROUP.depth.get()))
            else:
                self.respond_failure("Depth change too large! Must be <= .1")
        elif axis == "h":
            if abs(value) < 5.1:
                current_heading = SHM_GROUP.heading.get()
                SHM_GROUP.heading.set(current_heading + value)
                self.respond_success("Heading desire at {}".format(SHM_GROUP.heading.get()))
            else:
                self.respond_failure("Heading change too large! Must be <= 5")
        else:
            self.respond_failure("Unsupported as of now")

class VelocityHandler(APIHandler):

    def post(self, axis, value):
        try:
            value = float(value)
        except ValueError:
            self.respond_failure("Invalid value for velocity")
            return

        if axis == "x":
            # bound value to [-1.0, 1.0]
            value = bound(value, -1.0, 1.0)
            SHM_GROUP.speed.set(value)
            self.respond_success("{} speed set to {}".format(axis, value))
        elif axis == "y":
            current_sway_velocity = SHM_GROUP.sway_speed.get()
            # limit to speed changes of 0.1
            sway_delta = 0.1 if value > 0 else -0.1
            new_value = bound(current_sway_velocity + sway_delta, -1.0, 1.0)
            SHM_GROUP.sway_speed.set(new_value)
            self.respond_success("{} speed set to {}".format(axis, SHM_GROUP.sway_speed.get()))
