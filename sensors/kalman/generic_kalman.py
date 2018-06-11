'''
A generic Kalman filter.
Based on the paper "An Introduction to the Kalman Filter" Welch, Bishop
www.cs.unc.edu/~welch/media/pdf/kalman_intro.pdf
'''
from cupy import transpose, identity, dot, linalg
''' 
The following variables are used throught the program:
n - size of state vector
m - size of input vector
b - size of control vector

x - n-by-1 state vector
xHatMinus - n-by-1 a priori state vector
xHatPrev - n-by-1 a posteriori state estimate of last iteration
PPrev - n-by-n a posteriori estimate error covariance
xHat - n-by-1 a posteriori state estimate of this iteration
u - l-by-1 input reading
z - n-by-1 state reading

A - n-by-n relates previous state to next state
B - n-by-l relates control inputs to state
H - m-by-n relates state to measurement
P - n-by-n a posteriori estimate error covariance
R - m-by-m measurement noise covariance
'''

class KalmanFilter(object):
    def __init__(self, n,m,l, xStart, P,  A, B, H, R, Q  ):
        #Parameters
        self.n = n
        self.m = m
        self.l = l

        #State
        self.xHat = xStart
        self.P = P.reshape(n,n)

        #System Matrices
        self.A = A.reshape(n,n)
        self.B = B.reshape(n,l)
        self.H = H.reshape(m,n)
        self.R = R.reshape(m,m)
        self.Q = Q.reshape(n,n)
        self.I = identity(n)

    def Predict(self, uPrev):
        '''For use in Iterate'''
        xHatMinus = dot(self.A, self.xHat) + dot(self.B, uPrev)
        PMinus = dot(dot(self.A, self.P), self.A.T) + self.Q

        return xHatMinus,PMinus

    def Correct(self, xHatMinus, PMinus, z, active_measurements=None):
        '''For use in Iterate'''
        if active_measurements is not None:
            H = self.H * active_measurements
        else:
            H = self.H

        K = dot(PMinus, dot(H.T, linalg.inv(dot(H, dot(PMinus, H.T)) + self.R)))
        xHat = xHatMinus + dot(K,(z - dot(H, xHatMinus)))
        P = dot((self.I - dot(K, H)),PMinus)

        return K, xHat, P

    def Iterate(self, u, z, active_measurements=None):
        '''Steps the filter one step further
        active measurements is a boolean (m,1) array used as a mask for 
        which measurements should be used'''
        xHatMinus, PMinus = self.Predict(u)
        self.K, self.xHat, self.P = self.Correct(xHatMinus, PMinus, z, active_measurements)
        return self.xHat, self.P
