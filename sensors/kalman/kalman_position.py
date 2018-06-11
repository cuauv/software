import generic_kalman

import shm

from settings import dt
from conf.vehicle import gravity_force as vehicle_weight 
from conf.vehicle import dvl_present
from conf.vehicle import measurement_error

from auv_math.quat import Quaternion

from cupy import array, radians, sin, cos, zeros, eye

class PositionFilter(generic_kalman.KalmanFilter):
    def __init__(self, xHatStart):
        #State size
        n = 8 #Vel N, Acc N, Vel E, Acc E, Bias X Acc, Bias Y Acc, Depth, Depth Rate
              #Velocities relative to north-east coordinate system
              #This means that velocities are independent of rotation
        #Input size
        l = n
        #Measurement size
        m = 5

        #Settings
        velSigma = measurement_error['velSigma'] #5e-3 #Measurement of velocity error
        depthSigma = measurement_error['depthSigma'] #1 #Measurement of depth error 100
        accSigma = measurement_error['accSigma'] #1e3 #Measurement of acceleration error
        aSigma = 1e1 #Acceleration selection
        bSigma = 1e-2/(aSigma) #Bias error
        drag = 0.995
        #drag = 0.0

        #A n-by-n relates one state to the next
        #TODO: accelerations aren't being added to velocity!
        # This is intentional since they seem to mess things up
        # when we lose the DVL and they don't seem to help out much
        # when we do have the DVL to rely upon
        # At some point, this should be redone!
        A = array([ [drag, 0, 0, 0, 0, 0, 0, 0], #Vel N
                    [0, 1, 0, 0, 0, 0, 0, 0], #Acc N
                    [0, 0, drag, 0, 0, 0, 0, 0], #Vel E
                    [0, 0, 0, 1, 0, 0, 0, 0], #Acc e
                    [0, 0, 0, 0, 1, 0, 0, 0], #Bias X Acc
                    [0, 0, 0, 0, 0, 1, 0, 0], #Bias Y Acc
                    [0, 0, 0, 0, 0, 0, 1, dt], #Depth
                    [0, 0, 0, 0, 0, 0, 0, 1] #Depth rate
                    ]).reshape(n,n)

        #B n-by-l relates control input to the state
        #B = array([0,0,0,0,0,0,0,0]).reshape(n,l) #No Input
        B = eye( n )

        #H m-by-n relates the state to the measurement
        #TODO: biases are commented out - relating to the fact
        # that they cannot be calculated while acc is commented out
        # in A (see above todo)
        has_dvl = 1 if dvl_present else 0
        H = array([ [has_dvl, 0, 0, 0, 0, 0, 0, 0],
                    [0, 1, 0, 0, 0, 0, 0, 0], #[0, 1, 0, 0, 1, 0, 0, 0],
                    [0, 0, has_dvl, 0, 0, 0, 0, 0],
                    [0, 0, 0, 1, 0, 0, 0, 0], #[0, 0, 0, 1, 0, 1, 0, 0],
                    [0, 0, 0, 0, 0, 0, 1, 0] ]).reshape(m,n)

        #Q n-by-n process noise covariance
        Q = aSigma*aSigma*array([[dt**4/4, dt**3/2, 0, 0, 0, 0, 0, 0], [dt**3/2, dt**2, 0, 0, 0, 0, 0, 0], [0, 0, dt**4/4, dt**3/2, 0, 0, 0, 0],
                                 [0, 0, dt**3/2, dt**2, 0, 0, 0, 0],
                                 [0, 0, 0, 0, bSigma**2, 0, 0, 0],
                                 [0, 0, 0, 0, 0, bSigma**2, 0, 0],
                                 [0, 0, 0, 0, 0, 0, dt**4/4, dt**3/2],
                                 [0, 0, 0, 0, 0, 0, dt**3/2, dt**2] ] ).reshape(n,n)

        #R m-by-m measurement noise covariance
        R = array([ [velSigma**2, 0, 0, 0, 0],
                    [0, accSigma**2, 0, 0, 0],
                    [0, 0, velSigma**2, 0, 0],
                    [0, 0, 0, accSigma**2, 0],
                    [0, 0, 0, 0, depthSigma**2] ]).reshape(m,m)

        P = array([ [1, 0, 0, 0, 0, 0, 0, 0],
                    [0, 1, 0, 0, 0, 0, 0, 0],
                    [0, 0, 1, 0, 0, 0, 0, 0],
                    [0, 0, 0, 1, 0, 0, 0, 0],
                    [0, 0, 0, 0, 1, 0, 0, 0],
                    [0, 0, 0, 0, 0, 1, 0, 0],
                    [0, 0, 0, 0, 0, 0, 1, 0],
                    [0, 0, 0, 0, 0, 0, 0, 1] ]).reshape(n,n)

        super(PositionFilter, self).__init__( n,m,l, array(xHatStart), P, A,B,H,R,Q)

        #We also do position integration here
        self.forward = 0.0
        self.sway = 0.0
        self.north = 0.0
        self.east = 0.0

        self.dt = dt

    def update(self, heading, x_vel, x_acc, y_vel, y_acc, depth, wench,
                    active_measurements=None,
                    thruster_vals=None,
                    pitch=None,
                    roll=None):
        if thruster_vals is None:
            thruster_vals = []
        if active_measurements is not None:
            active_measurements = array(active_measurements)

        #TODO: use active_measurements

        heading = radians(heading)
        pitch = radians(pitch)
        roll = radians(roll)

        #Data relative to north-east
        z = zeros((5,1))
        z[0] = x_vel*cos(heading) - y_vel*sin(heading)
        z[1] = x_acc*cos(heading) - y_acc*sin(heading)
        z[2] = x_vel*sin(heading) + y_vel*cos(heading)
        z[3] = y_acc*sin(heading) + y_acc*cos(heading)
        z[4] = depth
        z = z.reshape(self.m,1) 
        # z = array([ x_vel*cos(heading) - y_vel*sin(heading),
        #             x_acc*cos(heading) - y_acc*sin(heading),
        #             x_vel*sin(heading) + y_vel*cos(heading),
        #             y_acc*sin(heading) + y_acc*cos(heading),
        #             depth
        #             ]).reshape(self.m,1)
        if active_measurements is not None:
            z *= active_measurements

        u = zeros( (8,1) )
        if len(thruster_vals) > 0:
            # Resolve sub/world model discrepancies for x/y/z -> n/e forces
            f_x = wench[0]
            f_y = wench[1]
            f_z = wench[2]

            kalman_g = shm.kalman.get()
            sub_quat = Quaternion(q=[kalman_g.q0, kalman_g.q1, kalman_g.q2, kalman_g.q3])

            # Convert from body or sub space to world space.
            world_space_forces = sub_quat * array((f_x, f_y, f_z))

            north_force = world_space_forces[0]
            east_force = world_space_forces[1]

            # Force*dt/mass = delta_v
            u[0] = 0
            u[2] = 0
            #u[0] = north_force * self.dt / (vehicle_weight/9.8)
            #u[2] = east_force * self.dt / (vehicle_weight/9.8)

        #H m-by-n relates the state to the measurement
        # Okay this takes some explaining
        # We want to be able to measure a bias in x,y accelerations
        # but are storing our state in north-east coordinates
        has_dvl = 1 if dvl_present else 0
        c = cos(heading)
        s = sin(heading)
        # self.H = array([ [has_dvl, 0, 0, 0, 0, 0, 0, 0],
        #                  [0, 1, 0, 0, c,-s, 0, 0],
        #                  [0, 0, has_dvl, 0, 0, 0, 0, 0],
        #                  [0, 0, 0, 1, s, c, 0, 0],
        #                  [0, 0, 0, 0, 0, 0, 1, 0] ]).reshape(self.m,self.n)
        # 
        self.H = array([ [has_dvl, 0, 0, 0, 0, 0, 0, 0],
                         [0, 1, 0, 0, 0, 0, 0, 0],
                         [0, 0, has_dvl, 0, 0, 0, 0, 0],
                         [0, 0, 0, 1, 0, 0, 0, 0],
                         [0, 0, 0, 0, 0, 0, 1, 0] ])
        self.H[1][4] = c
        self.H[1][5] = -s
        self.H[3][4] = s
        self.H[3][5] = c
        self.H = self.H.reshape(self.m, self.n)
        if active_measurements is not None:
            self.H *= active_measurements
            #self.H = array([ [0, 0, 0, 0, 0, 0, 0, 0],
            #                 [0, 1, 0, 0, c,-s, 0, 0],
            #                 [0, 0, 0, 0, 0, 0, 0, 0],
            #                 [0, 0, 0, 1, s, c, 0, 0],
            #                 [0, 0, 0, 0, 0, 0, 1, 0] ]).reshape(self.m,self.n)

        # Iterate
        xHat, P = self.Iterate(u,z)

        vel_n = xHat[0][0]
        acc_n = xHat[1][0]
        vel_e = xHat[2][0]
        acc_e = xHat[3][0]
        depth = xHat[6][0]
        depth_rate = xHat[7][0]

        vel_x =  vel_n * c + vel_e * s
        vel_y = -vel_n * s + vel_e * c
        acc_x =  acc_n * c + acc_e * s
        acc_y = -acc_n * s + acc_e * c

        #print "biases: %0.2e %0.2e"%(xHat[4][0], xHat[5][0])

        #We need to convert back to local (sub) coordinates here
        self.forward += self.dt*vel_x
        self.sway += self.dt*vel_y

        #Already in north-east coordinates, no need to convert
        self.north += self.dt*vel_n
        self.east += self.dt*vel_e

        return dict( north = self.north, east = self.east,
                     forward = self.forward, sway = self.sway,
                     velx = vel_x, vely = vel_y,
                     accelx = acc_x, accely = acc_y,
                     depth = depth, depth_rate = depth_rate)

