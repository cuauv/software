'''Utility file for using the logplot to tune the Kalman filter
Gives a way to run the Kalman filter on the data from logs in the logplot
and then to show the result.'''

import numpy as np

def kalman_orientation(output, variables, **kwargs):
    '''output is the desired output from the kalman filter, so something like
    'hdg' or 'rate' or 'pitch_rate'
    
    variables is the logplot variables dictionary and provides
    default data for all the inputs that kalman_orientation takes.

    Pass in as keywords any values to override, say with tweaked data
    or different sensors'''

    time = variables["t"]


    translation_dict = dict( hdg = "dvl.linear_heading",
                             rate = "threedmg.heading_rate",
                             rate_imu = "threedmg.heading_rate", #Currently ignored
                             pitch = "threedmg.pitch",
                             pitch_rate = "threedmg.pitch_rate",
                             roll = "dvl.roll",
                             roll_rate = "threedmg.roll_rate"
                             )
    required_args = ["hdg", "rate", "rate_imu", "pitch", "pitch_rate", "roll", "roll_rate"]
    state = ["hdg", "rate", "pitch", "pitch_rate", "roll", "roll_rate"]

    zeros = np.zeros( time.shape )
    #Pull arguments from variables into the expected inputs for kalman_orientation
    def lookup(value):
        group, var= tuple(value.split("."))
        group = variables.get(group, None)
        if group:
            out = getattr(group, var, zeros)
            return out
        return zeros
    arguments = dict([(key,lookup(value))
                        for key,value in translation_dict.items()])
    #Arguments passed in explicitly override all default values
    arguments.update(**kwargs)

    #The actual data to send to the filter
    reading_vectors = np.array([arguments[x] for x in required_args])

    #Starting vector - use initial values from the arguments
    xHatStart = np.array([arguments[x][0] for x in state])[:,np.newaxis]

    ###Run the Kalman Filter
    from sensors.kalman import kalman_orientation as orientation
    orientation.initialize(xHatStart)

    #Determine what output we are looking for
    output_index = required_args.index(output)

    prev_time = time[0]
    out = np.empty( (len(time)) )
    out[0] = xHatStart[output_index]
    for i in xrange(1,len(time)):
        if prev_time + orientation.dt > time[i]:
            #Only process once every 'dt' seconds
            out[i] = out[i-1]
            continue
        xHat = orientation.update(*reading_vectors[:,i])
        out[i] = xHat[output_index]
        prev_time = time[i]

    return out

#Curry some inputs for easy calling in logplot
def make_funcs(variables):
    def orient(output, **kwargs):
        return kalman_orientation(output, variables, **kwargs)
    return dict(orientation=orient)

