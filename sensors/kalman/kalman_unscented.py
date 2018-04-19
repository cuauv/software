from numpy.linalg import inv
import numpy as np
from unscented_tools import calculate_sigmas, unscented_transform
'''
In this implementation of the UKF, the following variables are used,
in order to be consistent with most documentation for Kalman Filtering:

x_hat - an array storing the system state. I use the convention x_hat to
        store all forms of state (documentation often distinguishes x, x_minus,
        x_hat, and x_hat_minus... this gets annoying!)

Nx - size of the system state, ie. the number of observed variables

P - the covariance matrix of the system's variables (summa in some docs for UKF)

F - state transfer function -- used to calculate predicted state from current

z - an array storing all sensor measurements (only needed in update step)

Nz - size of measurement state, ie. the number of sensor readings

R - noise matrix corresponding to each sensor in z

H - measurement transfer function -- brings state variables into measurement space

Q - process noise matrix, determined by the order of the measurements

kappa - spread of sigma points (sometimes lambda, but we can't use that)
        (generally, a value of 3 - Nx for gaussian distributions)

weights - the weights of each of the sigma points

dt - the time step between readings

sp, sp_f, sp_h - sigma points calculated normally, in the state space, and
                 measurement space, respectively

K - kalman gain, calculated in the update step
'''


class UnscentedKalmanFilter(object):

    def __init__(self, state_size, state_transfer, measurement_size,
                 measurement_transfer, time_step, sigma_spread,
                 difference = lambda x, y: x - y):

        self.x_hat = np.zeros(state_size)
        self.Nx = state_size
        self.P = np.eye(state_size)
        self.F = state_transfer
        self.Nz = measurement_size
        self.R = np.eye(measurement_size)
        self.H = measurement_transfer
        self.Q = np.eye(state_size)
        self.kappa = sigma_spread
        self.weights = np.full(2*state_size+1, .5 / (state_size+sigma_spread))
        self.weights[0] = sigma_spread / (state_size+sigma_spread)
        self.difference = difference
        self.num_sp = 2*state_size + 1
        self.sp = np.zeros((self.num_sp, self.Nx))
        self.sp_f = np.zeros((self.num_sp, self.Nx))
        self.sp_h = np.zeros((self.num_sp, self.Nz))
        self.dt = time_step

    def predict(self):

        self.sp = calculate_sigmas(self.x_hat, self.Nx, self.P, self.kappa)

        # Transform sigmas into predicted state space
        for i in range(self.num_sp):
            self.sp_f[i] = self.F(self.sp[i], self.dt)

        # Normalize through unscented transform
        self.x_hat, self.P = unscented_transform(self.sp_f, self.weights, self.Q)

    def update(self, z):

        # Transform sigmas into measurement space
        for i in range(self.num_sp):
            self.sp_h[i] = self.H(self.sp_f[i])

        # Mean and covariance of the state in the measurement space
        Hx_bar, PHx = unscented_transform(self.sp_h, self.weights, self.R)
        # Cross variance of Fx and Hx -- used to calculate K
        cross_var = np.zeros((self.Nx, self.Nz))

        for i in range(self.num_sp):
            cross_var += self.weights[i] * np.outer(self.sp_f[i] - self.x_hat,
                                                    self.sp_h[i] - Hx_bar)
        # Calculate K and measurement residual
        K = np.dot(cross_var, inv(PHx))
        residual = self.difference(z, Hx_bar)
        # Update predicted to new values for state and variance
        self.x_hat += np.dot(K, residual)
        self.P -= np.dot(K, np.dot(PHx, K.T))
