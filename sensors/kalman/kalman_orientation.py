import pickle
import os
import numpy as np
from collections import deque
import shm
from settings import dt

from variable_filter import VarFilter

# Number of thrusters values back to record to use in prediction
PREDICT_COUNT = 200
# Number of steps (0.02 seconds each) to advance the prediction
# based off of thruster impulse responses
PREDICT_FORWARD = 0

thrusters = ['port', 'starboard', 'sway_fore', 'sway_aft']

# Load thruster impulse response curves
impulse_response_file = os.path.join(os.path.dirname(__file__), "util/impulse_responses.pickle")
impulse_responses = pickle.load(open(impulse_response_file))

class OrientationFilter(object):
    def __init__(self, angle_sensors, rate_sensors, angle_biases, rate_biases, bias_guess):
        ''' Takes in dictionaries of variable:sensors pairs and constructs
        Kalman filters for each of these groups
        '''

        self.angle_sensors = angle_sensors
        self.rate_sensors = rate_sensors
        self.angle_biases = angle_biases
        self.rate_biases = rate_biases
        assert( set(angle_sensors.keys()) == set(['heading', 'roll', 'pitch']) )
        assert( set(rate_sensors.keys()) == set(['heading', 'roll', 'pitch']) )
        assert( set(angle_biases.keys()) == set(['heading', 'roll', 'pitch']) )
        assert( set(rate_biases.keys()) == set(['heading', 'roll', 'pitch']) )

        self.heading = VarFilter( angle_sensors['heading'],
                                   rate_sensors['heading'],
                                   angle_biases['heading'],
                                   rate_biases['heading'],
                                   bias_guess['heading'],
                                   varSigma = 3.0e0,
                                   aSigma = 1.0e1,
                                   rateSigma = 1.0e0,
                                   biasSigma = 1e-5,
                                   vel_drag = 1.0, #0.993,
                                   mod360=True)

        self.pitch = VarFilter( angle_sensors['pitch'],
                                   rate_sensors['pitch'],
                                   angle_biases['pitch'],
                                   rate_biases['pitch'],
                                   bias_guess['pitch'],
                                   #varSigma = 0.3e0,
                                   #aSigma = 3.0e0,
                                   #rateSigma = 3.0e0,
                                   biasSigma = 1e-10,
                                   mod360=True)
        self.roll = VarFilter( angle_sensors['roll'],
                                   rate_sensors['roll'],
                                   angle_biases['roll'],
                                   rate_biases['roll'],
                                   bias_guess['roll'],
                                   varSigma = 3e-1,
                                   aSigma = 3e1,
                                   rateSigma = 1e0,
                                   biasSigma = 1e-10,
                                   mod360=True)

        self.prev_prediction = 0

        #self.thruster_values = dict((t,deque([0]*PREDICT_COUNT, \
        #                             maxlen=PREDICT_COUNT)) for t in thrusters)

    def update(self, angle_sensors, rate_sensors):
        soft_kill = shm.switches.soft_kill.get()
        curr_thrusters = dict((t,(1-soft_kill)*shm.motor_desires.__getattribute__(t).get()) for t in thrusters)

        '''
        heading_rate = 0
        for t in thrusters:
            self.thruster_values[t].appendleft(curr_thrusters[t])

            # TODO: why is this factor of 2 necessary?
            # Response from past thrusters
            heading_rate += impulse_responses[t][PREDICT_FORWARD:PREDICT_COUNT+PREDICT_FORWARD].dot(self.thruster_values[t])/2
            # Response assuming current thrust for the immediate future
            heading_rate += sum(impulse_responses[t][:PREDICT_FORWARD]*self.thruster_values[t][0])/2.
        '''

        input = np.zeros(  (self.heading.n,1) )
        #input[0] = heading_rate*dt/2.
        #input[1] = (heading_rate - self.prev_prediction)
        input[1] = (0.00128 *curr_thrusters['port']
                    - 0.00122 * curr_thrusters['starboard']
                    + 0.00194 * curr_thrusters['sway_aft']
                    - 0.00218 * curr_thrusters['sway_fore'])/2.
        input[0] = input[1] * dt/2.
        #print input[1]
        #self.prev_prediction = heading_rate

        hdg, hdg_rate, bias = self.heading.update(angle_sensors['heading'], rate_sensors['heading'], input=input)
        #print bias

        pitch, pitch_rate,bias = self.pitch.update(angle_sensors['pitch'], rate_sensors['pitch'])
        roll, roll_rate,bias = self.roll.update(angle_sensors['roll'], rate_sensors['roll'])


        return dict(heading=hdg, heading_rate=hdg_rate,
                    pitch=pitch, pitch_rate=pitch_rate,
                    roll=roll, roll_rate=roll_rate)

errors = []
