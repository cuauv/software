import time
import sys

try:
    import numpy as np
    from scipy.optimize import fmin_cobyla, fmin_tnc, fmin_l_bfgs_b, fmin_slsqp
except ImportError:
    print("## ERROR: controld3 requires NumPy and SciPy for optimization ##")
    sys.exit(1)

from shm import control_internal_opt_errors, control_internal_wrench, \
                control_internal_outs, control_internal_priority as priorities
from shm.control_internal_depth import out as depth_out
from shm.control_internal_heading import out as heading_out
from shm.control_internal_pitch import out as pitch_out
from shm.control_internal_velx import out as velx_out
from shm.control_internal_vely import out as vely_out
from shm.control_internal_roll import out as roll_out
from shm.settings_control import quat_pid as quat_pid_enabled
from shm.settings_control import velx_active, vely_active, depth_active, \
                                 heading_active, pitch_active, roll_active

from control import vehicle
from control.thrusters import thrusters
from control.util import set_all_motors_from_seq, set_shm_wrench
from control.pid import PIDLoop

class Optimizer:
    """ Class to set motors using optimization techniques (see set_motors) """ 
    def __init__(self):
        self.thrusters = thrusters

        # this is multiplied by the errors so we prioritize certain DOFs
        self.error_scale = np.ones(6)

        self.quat_pid = PIDLoop()

        # other options
        self.DEBUG = False

    def get_thrusters(self):
        got_thrusters = thrusters()

        # constraint functions always have to be positive for allowed thrusts
        self.constraints = []
        # bounds is an array of min, max pairs for each thruster
        self.bounds = []
        for i, t in enumerate(got_thrusters):
            self.constraints.append(lambda x, m=t.max_thrust, i=i: m-x[i])
            self.constraints.append(lambda x, mn=t.max_neg_thrust, i=i: x[i]-mn)
            self.bounds.append((t.max_neg_thrust, t.max_thrust))

        # calculate some "starting step" guesses for the optimization algorithm
        # by using 1/4 of the average max thrust
        # good start guesses are important and make the algorithm run faster
        self.rhobeg = 0.25 * sum([t.max_thrust - t.max_neg_thrust \
                          for t in got_thrusters]) / (len(got_thrusters) * 2)

        # initial guess for optimizing function, currently static; 0 on all
        self.initial_guess = np.array((0,) * len(got_thrusters))

        return got_thrusters

    def update_error_scale(self):
        """
            Reads shared memory for the weight of each DOF
        """
        #self.error_scale[DOFSet.yaw_i] = priorities.heading.get()
        #self.error_scale[DOFSet.pitch_i] = priorities.pitch.get()
        #self.error_scale[DOFSet.roll_i] = priorities.roll.get()
        #self.error_scale[DOFSet.forward_i] = priorities.forward.get()
        #self.error_scale[DOFSet.sway_i] = priorities.sway.get()
        #self.error_scale[DOFSet.depth_i] = priorities.depth.get()
        # TODO figure out how to do priorities for individual components
        # of orientation
        self.error_scale[3] = priorities.torque.get()
        self.error_scale[4] = priorities.torque.get()
        self.error_scale[5] = priorities.torque.get()

        self.error_scale_sq = self.error_scale * self.error_scale

    def get_error(self, thrusts):
        return self.thrusts_output_mat_s.dot(thrusts) - self.desired_output_s

    def objective(self, thrusts):
        """
            Takes in a thrust for each motor and calculates our "error"
            We want to minimize this.
        """
        error = self.get_error(thrusts)
        error *= self.error_scale
        return error.dot(error)

    def derivative(self, thrusts):
        """
            The derivative of the objective function with respect to
            the thrusts. Greatly speeds up optimization.
        """
        return 2 * self.thrusts_output_mat_s.T.dot(self.get_error(thrusts) * \
                                               self.error_scale_sq)

    def optimize(self, A, b):
        """
            Attempts to find the motor values that best achieve the desired
            thruster response. First tries the exact solution, then resorts to
            a black box optimization routine if the solution will saturate
            thrusters.
        """
        # After this step, the error should always be zero.
        x = A.dot(b)

        self.get_thrusters() # init self.bounds

        good = True
        for bound, v in zip(self.bounds, x):
            if bound[0] > v or bound[1] < v:
                good = False
                break

        # If the zero error point is outside the thruster bounds,
        # initiate black box optimization!
        if not good:
            # Update priorities - possibly remove after tuning is finalized
            self.update_error_scale()

            x = fmin_slsqp(self.objective, self.initial_guess,
                           bounds=self.bounds, fprime=self.derivative,
                           disp=int(self.DEBUG))

        return x

    def set_motors(self, tm, ft_passive_s):
        """
            Sets motors using the PID control loop outputs by a sketchy
            multivariable optimization technique (aka. control voodoo v3.0)

            Takes in:
                a ThrusterManager
                a wrench of passive forces and passive torques in SUB SPACE
                the moment of inertia tensor
        """
        ##########
        # FORCES #
        ##########

        # Get the desired linear forces from the 3 linear PID loops
        # These are returned in "sub" space that is adjusted for heading
        # but not pitch and roll
        desired_accel = np.array((velx_out.get(), vely_out.get(),
                                  depth_out.get()))

        # Convert to sub space
        desired_force = tm.spitz_to_sub_quat * (vehicle.mass * desired_accel)

        ###########
        # TORQUES #
        ###########

        if quat_pid_enabled.get():
            # Get the quaternion desired angular accel in world coordinates
            desired_ang_accel = self.quat_pid.quat_pid()

        else:
            # If we are using individual PID loops, we need to do a bit of work
            # to get them all correctly into world space
            desired_ang_accel = tm.hpr_to_world(
                heading_out.get(), pitch_out.get(), roll_out.get()
            )

        # Convert to sub space
        desired_ang_accel_s = tm.orientation.conjugate() * desired_ang_accel

        # Convert desired angular accel into desired torque using inertia tensor
        desired_torque = vehicle.I.dot(desired_ang_accel_s)

        ###############################################################

        # Assimilate desired forces and torques into a vector of length 6
        self.desired_output_s = np.hstack((desired_force, desired_torque))

        # Subtract forces and torques already on the vehicle from the desires.
        # Our thrusters need only provide the forces and torques not already
        # provided by passives such as those due to buoyancy and drag.
        # This greatly reduces the complexities the PID loops have to handle.
        self.desired_output_s -= ft_passive_s

        # We should make sure that any DISABLED degrees of freedom are actually
        # disabled; PID loops will output 0 but passive forces may not!
        disables = [ (velx_active, tm.spitz_to_sub_quat * np.array((1, 0, 0)), False),
                     (vely_active, tm.spitz_to_sub_quat * np.array((0, 1, 0)), False),
                     (depth_active, tm.orientation.conjugate() * np.array((0, 0, 1)), False),
                     (heading_active, tm.orientation.conjugate() * np.array((0, 0, 1)), True),
                     (pitch_active, tm.spitz_to_sub_quat * np.array((0, 1, 0)), True),
                     (roll_active, np.array((1, 0, 0)), True) ]

        for enable, direction, is_torque in disables:
            if not enable.get():
                if is_torque:
                    direction = np.hstack((np.array((0, 0, 0)), direction))
                else:
                    direction = np.hstack((direction, np.array((0, 0, 0))))

                self.desired_output_s -= self.desired_output_s.dot(direction) * direction

        # limit final desired forces in sub space
        # This prevents an aggressive PID loop from destabilizing the sub
        # TODO This is not implemented.

        # Output final desires to shared memory.
        set_shm_wrench(control_internal_outs, self.desired_output_s)

        tm.vector_all(self.desired_output_s)

        ################
        # OPTIMIZATION #
        ################
        # Get the matrix that calculates outputs from thrusts (6 x 8)
        # This is needed for the optimization routine, which uses get_error.
        self.thrusts_output_mat_s = tm.thrusts_to_sub

        got_thrusters = self.get_thrusters()

        x = self.optimize(tm.sub_to_thrusts, self.desired_output_s)

        actual_output_s = self.thrusts_output_mat_s.dot(x)
        set_shm_wrench(control_internal_wrench, actual_output_s)

        # Output errors to shared memory.
        error = actual_output_s - self.desired_output_s
        set_shm_wrench(control_internal_opt_errors, error)

        # Finally, we convert thrusts to PWM and output values to shared memory
        out = [0] * len(got_thrusters)
        for i, t in enumerate(got_thrusters):
            # this only happens when desires are wild and slsqp fails
            # at that point we don't really care about accuracy and just
            # truncate for safety.
            if x[i] < t.max_neg_thrust:
                x[i] = t.max_neg_thrust
            elif x[i] > t.max_thrust:
                x[i] = t.max_thrust

            out[i] = t.thrust_to_pwm(x[i])
            if t.reversed_polarity():
              out[i] = -out[i]

        set_all_motors_from_seq(out, got_thrusters)
