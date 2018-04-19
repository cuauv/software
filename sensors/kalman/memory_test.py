import generic_kalman
from numpy import *

#State size 
n = 6 #Heading, rate, pitch, pitch rate, roll, roll rate
#Input size
l = 1
#Measurement size
m = 7

#Settings
dt = 0.02 #Simulation timestep
hdgSigma = 0.4 #Measurement of heading error
rateSigma = 2 #Measurement of rate error
pitchSigma = 0.1
pitchRateSigma = 1
aSigma = 5000 #Acceleration selection

#A n-by-n relates one state to the next
A = array([ [1, dt, 0, 0, 0, 0],
            [0, 1, 0 ,0, 0, 0],
            [0, 0, 1, dt, 0, 0],
            [0, 0, 0, 1, 0, 0],
            [0, 0, 0, 0, 1, dt],
            [0, 0, 0, 0, 0, 1]]).reshape(n,n) #States are constant
ATrans = transpose(A)
#B n-by-l relates control input to the state
B = array([0,0,0,0,0,0]).reshape(n,l) #No Input
BTrans = transpose(B)
#H m-by-n relates the state to the measurement
H = array([ [1,0,0,0,0,0], 
            [0,1,0,0,0,0], 
            [0,1,0,0,0,0],#we use two sensors for heading rate
            [0,0,1,0,0,0],
            [0,0,0,1,0,0],
            [0,0,0,0,1,0],
            [0,0,0,0,0,1]])#.reshape(m,n)
HTrans = transpose(H)
#Q n-by-n process noise covariance
Q = aSigma*aSigma*array([[dt**4/4, dt**3/2, 0, 0, 0, 0],
                         [dt**3/2, dt**2, 0, 0, 0, 0],
                         [0, 0, dt**4/4, dt**3/2, 0, 0],
                         [0, 0, dt**3/2, dt**2, 0, 0],
                         [0, 0, 0, 0, dt**4/4, dt**3/2],
                         [0, 0, 0, 0, dt**3/2, dt**2]]).reshape(n,n)

#R m-by-m measurement noise covariance
R = array([ [hdgSigma**2, 0, 0, 0, 0, 0, 0],
            [0, rateSigma**2, 0, 0, 0, 0, 0],#3dmg
            [0, 0, rateSigma**2, 0, 0, 0, 0],#imu sensor
            [0, 0, 0, pitchSigma**2, 0, 0, 0],
            [0, 0, 0, 0, pitchRateSigma**2, 0, 0],
            [0, 0, 0, 0, 0, hdgSigma**2, 0],
            [0, 0, 0, 0, 0, 0, rateSigma**2] ])

xHat = array([0,1,2,3,4,5]).reshape(n,1)
P = array([ [1,0.5,0,0,0,0],
            [0.5,1,0,0,0,0],
            [0,0,1,0.5,0,0],
            [0,0,0.5,1,0,0],
            [0,0,0,0,1,0.5],
            [0,0,0,0,0.5,1]]).reshape(n,n)


kalman_filter = generic_kalman.KalmanFilter( n,m,l, xHat,P, A,B,H,R,Q)

from auval.shmem import SharedVar
#Shared Variables
input_var = SharedVar("/sensors/dvl/linear_heading")

for i in range(int(1e6)):
    #kalman_filter.Predict([[0]])
    #kalman_filter.Correct(xHat, P, [[0]])
    #kalman_filter.Iterate([[0]],[[0]])
    x = input_var.get()
    input_var.set(255)


raw_input("done....")
