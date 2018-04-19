import numpy
from numpy import arctan2, pi, max, array, maximum, minimum, radians, \
                sum, sqrt, sin, cos
import scipy.stats

STEP_SIGMA = 0.02
RESAMPLE_SIGMA = 0.2
RESAMPLE_THRESH = 0.30
STARTING_SIGMA = 1.5

STATE_SIZE = 8

STATE_N1 = 0
STATE_E1 = 1
STATE_D1 = 2
STATE_N2 = 3
STATE_E2 = 4
STATE_D2 = 5
STATE_PHASE1 = 6
STATE_PHASE2 = 7

DEPTH_GUESS = 1.0 #TODO: determine
DEPTH_MEASURE_STDDEV = 0.3
DEPTH_START_STDDEV = 0.5

PERIOD = 15
GREEN_TIME = 5
COLOR_CORRECT_CHANCE = 0.85

def starting_samples( x1,y1,d1, x2,y2,d2, sigma, N ):
    state = numpy.random.normal( array([x1,y1,d1, x2,y2,d2]).reshape(-1,1),
                                 array([sigma,sigma,DEPTH_START_STDDEV,
                                      sigma,sigma,DEPTH_START_STDDEV]).reshape(-1,1),
                                 (STATE_SIZE-2,N) )
    phase = PERIOD*numpy.random.random( (2,N) )
    state = numpy.concatenate( (state, phase) )
    weights = numpy.ones( (N,) ) / N
    return state, weights

def angle_from( x, y, angle_offset ):
    theta = (arctan2(y,x) - angle_offset) % (2*pi)
    return theta - 2*pi * (theta > pi)
def angle_clamp( angle, coefficient ):
    return maximum( minimum( 1.05 - coefficient * abs(angle), 1), 0)

def evidence(state, center, angle_offset, angle_y, min_dist, max_dist, width, time, color, in_weight, out_weight):
    n,e,d = center
    angle_offset = radians(angle_offset)
    def p_E_H( x1, y1, d1, phase ):
        # Probability of evidence given a buoy position
        angle = angle_from( x1-n, y1-e, angle_offset )
        evidence = angle_clamp(angle, width)
        dist = (x1-n)*(x1-n) + (y1-e)*(y1-e)
        evidence *= (dist > min_dist**2) * (dist < max_dist**2)

        p = (in_weight-out_weight)*evidence + out_weight

        # probabillity contribution from depth
        expected_d = dist*numpy.tan(radians(-angle_y)) + d
        d_difference = numpy.min((numpy.abs(d1 - expected_d),numpy.abs(-d1 - expected_d)))
        cdf = scipy.stats.norm.cdf(-d_difference / DEPTH_MEASURE_STDDEV)
        p *= 2*cdf

        # Probability contribution from color
        # 1 if not-green, 0 if green
        expected_color = ((time + phase) % PERIOD) > GREEN_TIME
        p *= (COLOR_CORRECT_CHANCE * (expected_color == color)
                + (1-COLOR_CORRECT_CHANCE) * (expected_color != color))

        return p

    x1,y1,d1, x2,y2,d2, phase1,phase2 = state
    p1 = p_E_H( x1, y1, d1, phase1 )
    p2 = p_E_H( x2, y2, d2, phase2 )
    # So if either buoy position matches the evidence
    # then call it good!
    return maximum(p1, p2)

def step( states, weights, _evidence ):
    # Wiggle jiggle
    states += numpy.random.normal( numpy.zeros( (STATE_SIZE,1) ), STEP_SIGMA, states.shape)
    # Apply evidence to weights
    weights *= evidence( states, *_evidence)
    # Renormalize
    weights /= sum(weights)

    # See if we are improverished in our points
    N_eff = 1./ sum(weights * weights) 
    #print "N_eff:", N_eff
    if N_eff < RESAMPLE_THRESH * len(states[0,:]):
        #print "Resamplin'"
        # Please, sir, may I have more (good) points?
        indices = resample(weights)
        states = states[:,indices]
        weights = weights[:,indices]
        weights /= sum(weights)
        states += numpy.random.normal( numpy.zeros( (STATE_SIZE,1) ), RESAMPLE_SIGMA, states.shape)
    return states, weights

def fold( states ):
    ''' takes each (x1,y1,x2,y2) state to the equivalent state such that x1 < x2
    This is a 'symmetry-breaking' move - the buoys are idistinguishable so
    each possible state (x1,y1,x2,y2) is equivalent to (x2,y2, x1,y1).
    Think of this as folding a paper along the x1=x2 line - ie. we declare that
    buoy 1 is to the 'left' of buoy 2 so the swapping-symmetry is broken
    '''
    swap_indices = states[STATE_N1] > states[STATE_N2]
    swapped = numpy.vstack( (states[STATE_N2], states[STATE_E2], states[STATE_D2],
                             states[STATE_N1], states[STATE_E1], states[STATE_D1],
                             states[STATE_PHASE2], states[STATE_PHASE1]) )
    # also we make depths positive always
    swapped[STATE_D1] = abs(swapped[STATE_D1])
    swapped[STATE_D2] = abs(swapped[STATE_D2])
    return states*(1-swap_indices) + swapped*swap_indices

def resample(weights):
    ''' pick points randomly among all of them, choosing a point
    proportionally to its weight '''
    #n = len(weights)
    #indices = []
    ##C = [0.] + [sum(weights[:i+1]) for i in range(n)]
    #C = numpy.concatenate( ([0.], numpy.cumsum(weights)) )
    #u0, j = numpy.random.random(), 0
    #for u in [(u0+i)/n for i in range(n)]:
    #    while u > C[j]:
    #        j+=1
    #    indices.append(j-1)
    indices = numpy.cumsum(weights).searchsorted(numpy.random.sample(len(weights)))
    return indices

def get_layout():
    import pickle
    try:
        state = pickle.loads(shm.layout.state.get())
    except:
        print "Invalid mission layout state! Cannot initialize buoy positions."
        print "You might need to start running a mission log."
        print "Using a default guess of 5 meters infront of sub"
        n_off = shm.kalman.north.get()
        e_off = shm.kalman.east.get()
        hdg = shm.kalman.heading.get()
        n = 5*cos(radians(hdg))
        e = 5*sin(radians(hdg))
        return [(n_off + n, e_off + e),
                (n_off + n, e_off + e),
                (n_off + n, e_off + e)]

    #Gather buoy positions
    n_off = state[0]
    e_off = state[1]
    positions = state[2]

    out = [(positions[2*i,0]-n_off, positions[2*i+1,0]-e_off) for i in range(2,5)]
    return out

if __name__ == '__main__':
    import shm
    import time
    import camera
    import pylab
    import distance.calc_pos_to_objs as calc_pos
    ####Configuration

    # Number of samples in the filter
    display = True
    N = 100000
    NUM_DRAWN = 3000

    POSITIVE_WEIGHT = 0.80 #For same-color evidence (eg: seeing orange, looking for orange)
    POSITIVE_WIDTH = 6 # inversely proportional to width of region for positive updates

    #Size of buoy in meters
    #used to estimate buoy distance
    BUOY_RADIUS = 0.16

    #Factors to give minimum and maximum distance from approximated distance
    MIN_DIST_FACTOR = 0.4
    MAX_DIST_FACTOR = 1.8

    north = shm.kalman.north
    east = shm.kalman.east
    depth = shm.kalman.depth
    hdg = shm.kalman.heading

    buoy_x = dict(orange=shm.orange_results.center_x,
                  green=shm.green_results.center_x,
                  yellow=shm.yellow_results.center_x,
                  led1=shm.led_buoy_results.center_x,
                  led2=shm.led_buoy_results2.center_x)
    buoy_y = dict(orange=shm.orange_results.center_y,
                  green=shm.green_results.center_y,
                  yellow=shm.yellow_results.center_y,
                  led1=shm.led_buoy_results.center_y,
                  led2=shm.led_buoy_results2.center_y)
    buoy_prob = dict(orange=shm.orange_results.probability,
                      green=shm.green_results.probability,
                      yellow=shm.yellow_results.probability,
                      led1=shm.led_buoy_results.probability,
                      led2=shm.led_buoy_results2.probability)
    buoy_area = dict(orange=shm.orange_results.area,
                      green=shm.green_results.area,
                      yellow=shm.yellow_results.area,
                      led1=shm.led_buoy_results.area,
                      led2=shm.led_buoy_results2.area)
    buoy_watch_group = dict(orange=shm.orange_results,
                            green=shm.green_results,
                            yellow=shm.yellow_results,
                            led1=shm.led_buoy_results,
                            led2=shm.led_buoy_results2)
    buoy_color = dict(led1=shm.led_buoy_results.color,
                      led2=shm.led_buoy_results2.color)
    name = dict(orange="orange_buoy", # Used for calc_pos_to_objs
                yellow="orange_buoy",
                green="green_buoy",
                led1="led_buoy1",
                led2="led_buoy2")

    buoys = ['led1', 'led2']

    distance_calculator = calc_pos.PositionToObject()

    last_update = time.time()

    # TODO update layout to reflect the new buoy configurations
    (N1,E1), (N2,E2), (_,_) = get_layout()
    print "Starting locator at %s, %s" % ((N1,E1), (N2, E2))

    states, weights = starting_samples( N1, E1,DEPTH_GUESS,  N2, E2,DEPTH_GUESS, STARTING_SIGMA, N)
    watcher = shm.watchers.watcher()
    [watcher.watch(buoy_watch_group[b]) for b in buoys]

    likely_depths = [] #list of depth guesses, locator probability

    if display:
        pylab.ion()
        figure = pylab.figure()
        temp_states = states[:,:NUM_DRAWN]
        buoy1_plot = pylab.plot(temp_states[STATE_E1,:], temp_states[STATE_N1,:], ",b")[0]
        buoy2_plot = pylab.plot(temp_states[STATE_E1,:], temp_states[STATE_N2,:], ",g")[0]
        avg1_plot = pylab.plot([],[], 'bo')[0] #Plot of expectations
        avg2_plot = pylab.plot([],[], 'go')[0] #Plot of expectations

        sub_pos = pylab.plot([], [], "r-")[0] #Plot sub position over time as red line
        current_pos = pylab.plot([],[], "ro")[0] #Plot of current position

    while(True):
        heading = hdg.get()

        #Get the sub's positions
        curr_east, curr_north = east.get(), north.get()
        curr_depth = depth.get()

        delta_time = max((time.time() - last_update, 1))

        if not watcher.has_changed():
            time.sleep(0.05)
            continue


        for buoy in buoys:
            if buoy_prob[buoy].get() > 0.5:
                x,y = buoy_x[buoy].get(), buoy_y[buoy].get()
                angle_x, angle_y = camera.screen_to_angle(x,y)
                #print "Angle Y: %0.2f" % angle_y

                area = buoy_area[buoy].get()

                color = (buoy_color[buoy].get() != "green")

                #dist = distance_calculator.get_distance("led_buoy1")
                try:
                    dist = distance_calculator.get_distance(name[buoy])
                except ZeroDivisionError:
                    # Just skip this since
                    # area must have been zero
                    print "oops! got zero area"
                    continue

                min_dist = dist*MIN_DIST_FACTOR
                max_dist = dist*MAX_DIST_FACTOR
                min_dist = 0.1
                max_dist = 10.0
                weight = POSITIVE_WEIGHT
                #weight = weight_attenuate(weight,delta_time)
                width = POSITIVE_WIDTH
                run = True

                if run:
                    # Perform the actual update
                    _evidence = ( (curr_north, curr_east, curr_depth),
                                  angle_x + heading,
                                  angle_y,
                                  min_dist, max_dist,
                                  width,
                                  weight,
                                  time.time(),
                                  color,
                                  1-weight )

                    states, weights = step( states, weights, _evidence )

        folded = fold(states)
        avg = sum(folded*weights, axis=1)
        stddev = sqrt(sum( (folded - avg.reshape((-1,1)))**2*weights, axis = 1))

        shm.locator1.target_north.set(avg[STATE_N1])
        shm.locator1.target_east.set(avg[STATE_E1])
        shm.locator1.target_depth.set(avg[STATE_D1])
        shm.locator1.stddev.set(sqrt(stddev[STATE_N1]**2 + stddev[STATE_E1]**2))
        shm.locator1.depth_stddev.set(stddev[STATE_D1])

        shm.locator2.target_north.set(avg[STATE_N2])
        shm.locator2.target_east.set(avg[STATE_E2])
        shm.locator2.target_depth.set(avg[STATE_D2])
        shm.locator2.stddev.set(sqrt(stddev[STATE_N2]**2 + stddev[STATE_E2]**2))
        shm.locator2.depth_stddev.set(stddev[STATE_D2])
        print "Depths: %0.2f, %0.2f" %(avg[STATE_D1], avg[STATE_D2])
        #print "Phases: %0.1f, %0.1f" % (avg[STATE_PHASE1], avg[STATE_PHASE2])
        t = time.time()
        delay1 = (avg[STATE_PHASE1] - t) % (PERIOD)
        delay2 = (avg[STATE_PHASE2] - t) % (PERIOD)
        #print "Next green: %0.1f, %0.1f"% (delay1, delay2)

        if display:
            folded = folded[:,:NUM_DRAWN]
            #Draw the sub's position
            xs, ys = sub_pos.get_data()
            sub_pos.set_data(list(xs)+[curr_east],list(ys)+[curr_north])
            current_pos.set_data( [curr_east], [curr_north] )

            buoy1_plot.set_data( folded[STATE_E1,:], folded[STATE_N1,:] )
            buoy2_plot.set_data( folded[STATE_E2,:], folded[STATE_N2,:] )

            avg1_plot.set_data([avg[STATE_E1]], [avg[STATE_N1]])
            avg2_plot.set_data([avg[STATE_E2]], [avg[STATE_N2]])

            figure.canvas.draw()


        last_update = time.time()
