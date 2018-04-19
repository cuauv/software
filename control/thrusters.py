import numpy as np
import os
import shm

from math import sqrt
from control.thrust_vectoring import VectoringThrusterData

from auv_math import quat
from auvlog.client import log
from conf import vehicle

# *************************************
#
# thrusters.py --
#   Complete description of thrusters
#   attached to the vehicle. Adjust
#   settings here when thrusters change.
#
# *************************************

MODEL_DIR = os.path.join(os.environ['CUAUV_SOFTWARE'], 'control', 'bollard',
                         '2015-06-03')

# Keys of this dictionary should match names given to thrusters below
# Values should be a tuple of filenames for the forward and reverse models
# in the directory specified by MODEL_DIR above
# The suffix ".polynomial" is automatically appended to each filename given
# Files given should match a specific format, see GenericThruster.parse_model
# TODO: make sure that the bollard pull polynomial model for each actually works from the test on one thruster
thruster_models = {
    'aft_port' : ("2015-06-03_17.14.30_legolas_forward", \
                  "2015-06-03_17.03.11_legolas_reverse"),
    'fore_port' : ("2015-06-03_17.14.30_legolas_forward", \
                  "2015-06-03_17.03.11_legolas_reverse"),
    'aft_starboard' : ("2015-06-03_17.14.30_legolas_forward", \
                  "2015-06-03_17.03.11_legolas_reverse"),
    'fore_starboard' : ("2015-06-03_17.14.30_legolas_forward", \
                  "2015-06-03_17.03.11_legolas_reverse")
}


# These are polynomial fits from the bollard pull test in 2012
# Old and should only be used if newer bollard pull data is not available
A = 0.000307276253082
B = -0.01525678631260
C = 0.204972883002
SEABOTIX_FORWARD1 = [A,B,C]

A = 0.000394823970677
B = 0.0323005697662
C = 0.753939329621
SEABOTIX_REVERSE1 = [-A,-B,-C]

A = 0.000272106522688
B = 0.0146228928113
C = -0.100585915806
VIDEORAY_FORWARD1 = [A,B,C]

A = 6.74058780851e-05
B = -0.0156209666281
C = -0.340788996155
VIDEORAY_REVERSE1 = [-A,-B,-C]

# From Blue Robotics site.
# Polynomial fit on 12V thrust data.
A = 6.00612189e-05 # ORIGINAL BEFORE BOLLARD PULL
B = 1.60088580e-01 # ORIGINAL BEFORE BOLLARD PULL
#C = 3.65838579e-01
# Multiplied by 10; works wonders!
#C = 3.65838579e-00
C = 2.65838579e-00 # ORIGINAL BEFORE BOLLARD PULL
# TODO Run bollard pull!
# Below after shifting by 64 (accounting for firmware deadband removal).
# This is wrong!
#A = 6.00612189e-05
#B = 1.6769e-1
#C = 6.118e-1

# IT'S VERY IMOPRTANT FOR THE NEGATIVE CURVE TO ALWAYS GIVE NEGATIVE
# THRUST WHEN GIVEN A NEGATIVE PWM!
BLUEROBOTICS_T200 = [A, B, C]
BLUEROBOTICS_T200_REV = [A, B, -C]
#BLUEROBOTICS_T200_REV = [3*A, 3*B, 1.8*C]

# From Blue Robotics site
# 24.6N at full forward, linear.
# BLUEROBOTICS_T100 = [0, 24.6 / 255.0, 0]


# BlueRobotics T200, 16V
# A = 0.000291659454377
# B = 0.123845579636882
# C = 0
# BLUEROBOTICS_T200 = [A, B, C]

# A = -0.000238104822255
# B = 0.104961999605853
# C = 0
# BLUEROBOTICS_T200_REV = [A, B, C]


# BlueRobotics T100, 16V, from extrapolation:
A = 0.000295908999078
B = 0.078499761258298
C = 0
BLUEROBOTICS_T100 = [A, B, C]

A = -0.000269659532703
B = 0.037883323208733
C = 0
BLUEROBOTICS_T100_REV = [A, B, C]

# BlueRobotics T100, 14.8V, from extrapolation:
# A = 0.000253187137336
# B = 0.072612279163925
# C = 0
# BLUEROBOTICS_T100 = [A, B, C]

# A = -0.000230727437669
# B = 0.035042073968078
# C = 0
# BLUEROBOTICS_T100_REV = [A, B, C]

desires = shm.motor_desires

class GenericThruster(object):
    max_pwm = 255
    min_pwm = 0 # turn on value
    def __init__(self, yaw, pitch, position, min_neg_pwm, min_pos_pwm, drag=1.0, link=None, name="", reversed_polarity=False, broken=False, vector=False):
        """
            Yaw, and pitch are counter-clockwise angles (deg) in a right hand
            coordinate system where +z is down (yaw axis) and
                                    +y is starboard (pitch axis)
            A yaw of 0 indicates forward thrust is in -x direction
            (propels sub forward), a yaw of 90 indicates propulsion to the left
            a pitch of 90 indicates upward propulsion
            Roll is irrelevant (axis of prop)

            position is a 3 element tuple giving the position of the thruster
            in the cartesian coordinate system of the sub in the order x, y, z.
            +x is forward, +y is starboard, +z is down
            The origin is considered to be the center of rotation.

            Drag is a property of how well the thruster can deliver power.
            e.g. an obstructed thruster might have a drag of 0.5 (1/2 power)

            link is a string indicating the name of the shared memory variable
            holding the thruster's PWM inside the desires group

            reversed_polarity=True essentially indicates that the thruster's
            wiring has been flipped, i.e. a negative PWM drives it forwards

            vector is non False iff the thruster is "vectored".
            A vectored thruster is specified by a 4-tuple that denotes the axis
            of the thruster's vectorization and the range, as well as its shm
            bindings:
              (axis, theta, shm_current_value, shm_desired_value)
            The axis should be orthogonal to the thruster's axis of thrust and
            theta is the range of rotation CCW from start_vector about axis.
        """
        assert type(self) != GenericThruster
        assert -90.0 <= pitch <= 90
        assert 0 <= drag <= 1.0

        self.reversed_polarity = reversed_polarity
        self.broken = broken
        self.pos = np.array((position))

        self.drag = drag

        # Link to shared memory variable
        self.set = desires.__getattribute__(link).set
        self.get = desires.__getattribute__(link).get

        self.name = name
        self.link = link
        #self.set_models()

        self.q = quat.Quaternion(hpr=(yaw % 360, pitch, 0.0))
        self.calculate_force_and_torque_hat(self.q)

        self.vectored = vector is not False
        if self.vectored:
          self.thrust_vectoring_data = \
            VectoringThrusterData(self.pos, vector[0], self.force_hat, vector[1])

          self.vector_angle = shm._eval(vector[2])
          self.vector_desire = shm._eval(vector[3])

        self._thrust_memo = {} # pwm -> thrust mappings
                               # is this even worth it?, 32Kb of data...

        assert(min_neg_pwm < 0 and min_pos_pwm > 0)
        self.min_pos_pwm = min_pos_pwm
        self.min_neg_pwm = min_neg_pwm

        self.max_thrust = self.pwm_to_thrust(self.max_pwm)
        self.max_neg_thrust = self.pwm_to_thrust(-self.max_pwm)

        self.min_thrust = self.pwm_to_thrust(self.min_pos_pwm)
        self.min_neg_thrust = self.pwm_to_thrust(self.min_neg_pwm)

        # Variables used for quadratic equation in thrust_to_pwm
        self._qvars = [self.get_qvars(self.curve_reverse),
                       self.get_qvars(self.curve_forward)]

    def get_qvars(self, curve):
        class Qdata:
            pass

        qvars = Qdata()
        qvars.a = self.drag * curve[0]
        qvars.b = self.drag * curve[1]
        qvars.b_sq = qvars.b**2
        qvars.a4 = 4*qvars.a
        return qvars

    def update_shm_group(self, g, pwm):
        """
            Populates motor desires group g with a pwm for this thruster
        """
        g.update(**{ self.link : pwm })

    def pwm_to_thrust(self, pwm=None):
        """
            Returns thrust that a given pwm will provide
            on this thruster.

            Needs to handle lower bound. (min possible thrust, turn-on point)
            Does not need to handle max value
        """
        if pwm is None:
            pwm = self.get()

        if pwm in self._thrust_memo:
            return self._thrust_memo[pwm]

        #Handle lower bound
        if pwm >= 0 and pwm < self.min_pos_pwm:
            thrust = 0
        elif pwm < 0 and pwm > self.min_neg_pwm:
            thrust = 0

        else:
            #drag
            d = self.drag

            #Thrust calculation
            curve = [self.curve_reverse, self.curve_forward][pwm > 0]
            # TODO: why only the first two terms are multiplied by d?
            thrust = d * curve[0] * pwm**2 + d * curve[1] * pwm + curve[2]

        #thrust *= SCALE_FACTOR
        self._thrust_memo[pwm] = thrust
        return thrust

    def thrust_to_pwm(self, thrust):
        """
            Returns the integer pwm required to achieve a given thrust
            for this thruster.

            Needs to handle lower bound. (min possible thrust, turn-on point)
            Does not need to handle max value
        """
        # If thrust is within dead region of the thruster,
        # pick the one of min_neg_pwm, 0, min_pos_pwm which is the closest
        if thrust >= 0 and thrust < self.min_thrust:
            if self.min_thrust < 2 * thrust:
                return self.min_pos_pwm
            else:
                return 0

        elif thrust < 0 and thrust > self.min_neg_thrust:
            if self.min_neg_thrust > 2 * thrust:
                return self.min_neg_pwm
            else:
                return 0

        #PWM calculation
        if thrust > 0:
            curve = self.curve_forward
            qvars = self._qvars[1]

        else:
            curve = self.curve_reverse
            qvars = self._qvars[0]

        c = curve[2] - thrust# / SCALE_FACTOR
        D = qvars.b_sq - qvars.a4*c

        # we use a robust quadratic formula to avoid precision loss
        # see Numerical recipes on Quadratic formula
        try:
            if qvars.b > 0:
                z = -0.5*(qvars.b + sqrt(D))
                pwm = c/z
            else:
                z = -0.5*(qvars.b - sqrt(D))
                pwm = z/qvars.a

        except ValueError: #sqrt negative number, possible for small values of x
            return 0       #small enough to return 0 anyway

        return int(round(pwm))

    def get_thrust_vectoring_offset(self):
        angle = self.thrust_vectoring_data.angle_from_value(self.vector_angle.get())
        return quat.quat_from_axis_angle(self.thrust_vectoring_data.vector_axis, angle)

    def vector(self, desired_output_s):
        angle = self.thrust_vectoring_data.get_angle(desired_output_s)
        self.vector_desire.set(self.thrust_vectoring_data.value_from_angle(angle))

        self.calculate_force_and_torque_hat(self.get_thrust_vectoring_offset() * self.q)

    def calculate_force_and_torque_hat(self, thruster_orientation):
        """
            Thruster orientation should be relative to the sub.
        """
        self.force_hat = thruster_orientation * np.array((1, 0, 0))
        self.torque_hat = np.cross(self.pos, self.force_hat)

    def torque_about(self, axis_hat, thrust=1.0):
        """
            Returns the scalar torque generated by thruster about axis_hat
        """
        return thrust * axis_hat.dot(self.torque_hat)

    def thrust_in(self, axis_hat, thrust=1.0):
        """
            Returns the scalar force provided by thruster in axis_hat direction
        """
        return thrust * self.force_hat.dot(axis_hat)

    def parse_model(self, filename):
        """
            Returns a list of coefficients from a polynomial model file
            We are assuming a quadratic fit ax^2 + bx + c
            Returns: [a, b, c]
        """
        f = open(filename)
        lines = f.readlines()
        f.close()

        return [float(line.split()[1]) for line in lines[2:]]

    def set_models(self):
        if self.name in thruster_models:
            tags = thruster_models[self.name]
            self.curve_forward, self.curve_reverse = \
       [self.parse_model("%s/%s.polynomial" % (MODEL_DIR, tag)) for tag in tags]

            self.curve_reverse = [-x for x in self.curve_reverse]

        else:
            log("No model for %s thruster, defaulting to VideoRay!" % self.name)

class VideoRay(GenericThruster):
    max_pwm = 255
    min_pwm = 26
    curve_forward = VIDEORAY_FORWARD1
    curve_reverse = VIDEORAY_REVERSE1

class SeaBotix(GenericThruster):
    max_pwm = 255 # TODO: forward and reverse the same?
    min_pwm = 35
    curve_forward = SEABOTIX_FORWARD1
    curve_reverse = SEABOTIX_REVERSE1

class EllenThruster(GenericThruster):
    max_pwm = 255
    min_pwm = 45 # TODO: arbitrary at the moment

class T100(GenericThruster):
    max_pwm = 255
    min_pwm = 30  # TODO verify
    curve_forward = BLUEROBOTICS_T100
    curve_reverse = BLUEROBOTICS_T100_REV

class T200(GenericThruster):
    max_pwm = 255
    min_pwm = 1
    curve_forward = BLUEROBOTICS_T200
    curve_reverse = BLUEROBOTICS_T200_REV

class Simple(GenericThruster):
    max_pwm = 100
    min_pwm = 1
    curve_forward = [0, 0.1, 0]
    curve_reverse = [0, 0.1, 0]

# List of all thrusters on the vehicle (including possibly broken thrusters)
# Used for thruster tests
all_thrusters = []

for thruster in vehicle.thrusters:
    def get_default(key, default):
        return thruster[key] if key in thruster else default

    h, p = thruster['heading_pitch']
    reversed_polarity = get_default('reversed', False)
    broken = get_default('broken', False)
    t_class_s = get_default('type', 'VideoRay')
    vector = get_default('vector', False)

    if t_class_s not in globals():
        log("Invalid type \"%s\" for thruster %s!" % (t_class_s, thruster['name']),
                copy_to_stdout=True)
        raise Exception()

    t_class = globals()[t_class_s]

    min_pos_pwm = get_default('min_pos_pwm', t_class.min_pwm)
    min_neg_pwm = get_default('min_neg_pwm', -t_class.min_pwm)

    thruster_name = thruster.get('real_name', thruster['name'])
    thruster = t_class(yaw=h, pitch=p, position=thruster['pos'],
                       link=thruster['name'], name=thruster_name, vector=vector,
                       reversed_polarity=reversed_polarity, broken=broken,
                       min_pos_pwm=min_pos_pwm, min_neg_pwm=min_neg_pwm)

    all_thrusters.append(thruster)

# Thrusters contains an array of all non-broken thrusters
# to be used for control
thrusters = [x for x in all_thrusters if not x.broken]

