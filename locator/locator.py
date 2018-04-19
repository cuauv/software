'''
This file runs analysis on the vision output from buoys (or other things)
to determine where the buoy is most likely to be.
This outputs as the mission's tagged location the location where it is
most likely to find the buoy.

We keep a grid storing a top-down view of the area where we think the buoy is.
Vision results are used as evidence of where the buoy is likely to be found.
Positive results increase the likley in a region by where we think the buoy is.
This region is approximated by the angle to the buoy in the screen and
a rough distance estimate from the size in pixels of the buoy.

Seeing a buoy other than the target buoy helps, too, since that is used to give
an 'inferred' position of the target buoy, through the use of visual estimates
of buoy relative positions. These estimates (in Configuration) need only be
fairly rough (on the order of half a meter accuracy) to still be useful.

Not seeing the target buoy is also used as evidence, but in this case
it decreases the probability of finding the buoy in the region where it
could be seen. That is, in a region bounded by the visible angles of the cameras
and by the distances at which we could see the buoy.

More specifically, this program performs Bayesian updates of the probability
distribution function (approximated in a grid) through use of evidence from
the vision system. We compute the probability of the buoy being at
any given location by Bayes theorem and an estimated distribution of
the likelihood that the reading would be gotten if the buoy is at that
location.
This gives a structured means of incorporating a wide variety of data.
This program uses approximate knowledge of:
-relative buoy positions from visual observations
-Sub movements from DVL readings
-Vision success or failure in any of the three buoy colors
'''

####Configuration

POSITIVE_WEIGHT = 0.80 #For same-color evidence (eg: seeing orange, looking for orange)
POSITIVE_WIDTH = 6 # inversely proportional to width of region for positive updates

INFERRED_WEIGHT = 0.52 #For off-color evidence (eg: seeing green, looking for orange)
INFERRED_WIDTH = 3.5 # inversely proportional to width of region fo inferred updates

NEGATIVE_WEIGHT = 0.1 # Gives little weight to not-observing
NEGATIVE_WIDTH = 3 # Gives a very wide region for not-observing
NEGATIVE_MIN_DIST = 0.25 # Closest point at which we can see a buoy
NEGATIVE_MAX_DIST = 3 # Furthest point at which we can see a buoy
#TODO: negative width should be determined to roughly match our FOV

#Size of buoy in meters
#used to estimate buoy distance
BUOY_RADIUS = 0.16

#Factors to give minimum and maximum distance from approximated distance
MIN_DIST_FACTOR = 0.8
MAX_DIST_FACTOR = 1.3
MIN_DIST_FACTOR_INFERRED = 0.4
MAX_DIST_FACTOR_INFERRED = 1.8

# Constants
SIZE = 400 #cell count
LENGTH = 5 # meters

####Locator Task
import numpy
import pylab
import math
import camera
import shm
from mission.tasks import vector
import time
import distance.calc_pos_to_objs as calc_pos

# Enable warning output
# NOTE: the given warnings appear innocuous but loud
import os
os.environ['PYOPENCL_COMPILER_OUTPUT'] = '1'

# We might not have OpenCL installed
# If not, try the NumPy version instead
try:
    #from locator_cl import LocatorCL as Locator
    from locator_numpy import Locator
except ImportError, e:
    print "Unable to import OpenCL, defaulting to NumPy implementation"
    print "For instructions on installing OpenCL, see CUAUV Wiki page: Software/OpenCL"
    from locator_numpy import Locator

north = shm.kalman.north
east = shm.kalman.east
hdg = shm.kalman.heading

buoy_x = {"orange" : shm.orange_results.center_x,
          "green"  : shm.green_results.center_x,
          "yellow" : shm.yellow_results.center_x,
          "led1"   : shm.led_buoy_results.center_x,
          "led2"   : shm.led_buoy_results2.center_x}

buoy_y = {"orange" : shm.orange_results.center_y,
          "green"  : shm.green_results.center_y,
          "yellow" : shm.yellow_results.center_y,
          "led1"   : shm.led_buoy_results.center_y,
          "led2"   : shm.led_buoy_results2.center_y}

buoy_prob = {"orange" : shm.orange_results.probability,
             "green"  : shm.green_results.probability,
             "yellow" : shm.yellow_results.probability,
             "led1"   : shm.led_buoy_results.probability,
             "led2"   : shm.led_buoy_results2.probability}

buoy_area = {"orange" : shm.orange_results.area,
             "green"  : shm.green_results.area,
             "yellow" : shm.yellow_results.area,
             "led1"   : shm.led_buoy_results.area,
             "led2"   : shm.led_buoy_results2.area}

buoy_watch_group = dict(orange=shm.orange_results,
                        green=shm.green_results,
                        yellow=shm.yellow_results,
                        led1=shm.led_buoy_results,
                        led2=shm.led_buoy_results2)
name = dict(orange="orange_buoy", # Used for calc_pos_to_objs
            yellow="orange_buoy",
            green="green_buoy",
            led1="led_buoy1",
            led2="led_buoy2")

buoy_colors = ["orange", "led1", "led2"] #["orange", "green", "yellow"]

camera_w = shm.camera.forward_width
camera_height = shm.camera.forward_height

def x_distance_to_target(target):
        n,e = target
        target_pos = vector.Vector(n,e)
        current_pos = vector.Vector(north.get(), east.get())
        heading = vector.FromAuvAngle(hdg.get())
        to_target = target_pos - current_pos
        forward = vector.ProjectOnto(to_target, heading)
        return vector.Length(forward)

def weight_attenuate(weight, delta_time):
    ''' Determine how much weight to give to an update if the
    previous update was delta_time ago.
    Attempts to make it so that conversion is independent of
    frame-rate of the updates.'''
    w = weight/(1-weight)
    new_weight = w**delta_time/(1+w**delta_time)
    return new_weight

distance_calculator = calc_pos.PositionToObject()

class LocatorRunner(object):
    '''
    Create an object that on each update() call examines vision output
    and vehicle state to update where a colored buoy most likely is.
    '''

    def __init__(self, target, orange_pos=None, green_pos=None, yellow_pos=None,
                 led_pos=None, prior_sigma=2., display=True,
                 locator_var=None, settings_var=None):
        '''
        target is the color of the buoy we are interested in
        orange_pos, green_pos, yellow_pos are tuples of (north,east) coordinates
        prior_sigma gives the variance of the initial guess. Should be something
        like 1 (in meters) and can be gotten from mission layout system or just guessed.

        Display determines whether to show output.
        Warning: display is very slow and can affect results by slowing down the rate
        of updates.
        '''

        self.display = display
        self.output_var = locator_var
        self.settings_var = settings_var
        self.target = target

        if locator_var is None:
            raise Exception("locator_var required")
        if settings_var is None:
            raise Exception("locator_var required")

        #Relative positions of orange, green, and yellow buoys
        #in meters, displacement is arbitrary.
        positions = dict(orange=orange_pos,
                            yellow=yellow_pos,
                            green=green_pos,
                            led1=led_pos,
                            led2=led_pos)
        positions = dict( [(color, pos) for color,pos in positions.items()
                            if pos is not None] )
        target_n, target_e = positions[self.target]

        #Calculate the relative offsets
        self.N_OFFSET = dict((color, target_n - n) for color, (n,e) in positions.items())
        self.E_OFFSET = dict((color, target_e - e) for color, (n,e) in positions.items())

        # The actual Locator
        self.locator = Locator(target_n, target_e, LENGTH, SIZE, prior_sigma)

        self.likely_depths = [] #list of depth guesses, locator probability

        if self.display:
            pylab.ion()
            self.figure = pylab.figure()
            import matplotlib.colors
            norm = matplotlib.colors.LogNorm(1e-7,1)
            self.img = pylab.imshow(self.locator.probabilities.reshape((SIZE,SIZE)),
                                    norm=norm,
                                    picker=True, origin="lower",
                                    extent=(numpy.min(self.locator.easts),
                                            numpy.max(self.locator.easts),
                                            numpy.min(self.locator.norths),
                                            numpy.max(self.locator.norths)))
                                        
            self.img.set_interpolation("nearest")
            #colorbar = pylab.colorbar()
            #ticks =[1,1e-1,1e-2,1e-3,1e-4,1e-5,1e-6]
            #colorbar.set_ticks(ticks)
            #colorbar.set_ticklabels([str(x) for x in ticks])
            self.sub_pos = pylab.plot([], [], "r-")[0] #Plot sub position over time as red line
            self.current_pos = pylab.plot([],[], "ro")[0] #Plot of current position
            self.output_pos = pylab.plot([], [], "go")[0] #Plot of the output point

            # Draw layout positions of the items
            for color, (n,e) in positions.items():
                if not "led" in color: # stupid hack to avoid a problem... FIX
                    pylab.plot([e], [n], "^", color=color)

        # Updates when there is new vision data
        self.vision_watcher = shm.watchers.watcher()
        for color in buoy_colors:
            self.vision_watcher.watch(buoy_watch_group[color])

        # Updates when as the sub moves
        # Useful for updating sub position when drawing
        # even when vision data isn't changing
        tick_watcher = shm.watchers.watcher()
        tick_watcher.watch(shm.kalman)

        self.last_update = time.time()

        # Clear any existing probabilities
        self.output_var.probability.set(0)

    def update(self):
        '''
        Updates the locator output in shared memory by examining vehicle and vision state

        Update should be called regularly, for example on vehicle position updates
        '''
        heading = hdg.get()

        #Get the sub's positions
        curr_east, curr_north = east.get(), north.get()

        if self.display:
            #Draw the sub's position
            xs, ys = self.sub_pos.get_data()
            self.sub_pos.set_data(list(xs)+[curr_east],list(ys)+[curr_north])
            self.current_pos.set_data( [curr_east], [curr_north] )
            self.figure.canvas.draw()

        # Don't perform updates if we haven't gotten new vision results
        if not self.vision_watcher.has_changed():
            if self.display:
                pylab.draw()
            return

        delta_time = max(time.time() - self.last_update, 1)

        # Perform updates for each buoy that we see
        for color in buoy_colors:
            if buoy_prob[color].get() > 0.5:
                x,y = buoy_x[color].get(), buoy_y[color].get()
                angle_x, angle_y = camera.screen_to_angle(x,y)

                area = buoy_area[color].get()
                if area == 0:
                    continue #Can't divide by zero!

                try:
                    dist = distance_calculator.get_distance(name[color])
                except ZeroDivisionError:
                    # Just skip this since
                    # area must have been zero
                    print "oops! got zero area"
                    continue

                if self.target == color:
                    min_dist = dist*MIN_DIST_FACTOR
                    max_dist = dist*MAX_DIST_FACTOR
                    weight = POSITIVE_WEIGHT
                    weight = weight_attenuate(weight,delta_time)
                    width = POSITIVE_WIDTH
                    run = True
                elif self.settings_var.use_inferred_updates.get():
                    min_dist = dist*MIN_DIST_FACTOR_INFERRED
                    max_dist = dist*MAX_DIST_FACTOR_INFERRED
                    weight = INFERRED_WEIGHT
                    weight = weight_attenuate(weight,delta_time)
                    width = INFERRED_WIDTH
                    run = True
                else:
                    run = False

                if run:
                    # Perform the actual update
                    self.locator.update( (curr_north+self.N_OFFSET[color],
                                     curr_east+self.E_OFFSET[color]),
                                    angle_x + heading,
                                    min_dist, max_dist,
                                    width = width,
                                    in_weight = weight,
                                    out_weight = 1-weight)
        
        color = self.target
        # Check if we don't see it
        # note: only doing this for the color we're looking for
        # we could be checking for not-seeing the other colors, too
        # but that strikes me as wild and cavalier
        if buoy_prob[color].get() < 0.1:
            weight = NEGATIVE_WEIGHT
            weight = weight_attenuate(weight,delta_time)

            # We don't see the buoy, so we want a 'negative' update
            self.locator.update( (curr_north+self.N_OFFSET[self.target],
                             curr_east+self.E_OFFSET[self.target]),
                            heading,
                            NEGATIVE_MIN_DIST, NEGATIVE_MAX_DIST,
                            NEGATIVE_WIDTH,
                            in_weight=weight,
                            out_weight=1-weight)
                

        if self.display:
            #Display
            self.img.set_data(self.locator.probabilities.reshape((SIZE,SIZE)))
            #Rescaling colors: (don't use if logparithmic plot enabled)
            #img.set_clim( (locator.probabilities.min(), locator.probabilities.max()) )

        #Tag the most likely position
        north_out, east_out, prob_out = self.locator.get_max()

        self.output_var.target_east.set(east_out)
        self.output_var.target_north.set(north_out)
        self.output_var.probability.set( prob_out )

        self.last_update = time.time()

        # This determines the position of the sub in the vertical direction
        # The actual locator system has no means for determining depth, so
        # this is a little ad hoc

        #Update probable depth list based on y camera pos, pitch, and likely distance
        if buoy_prob[color].get() > 0.5:
            #buoy data is good
            ypos = buoy_y[color].get()

            FORWARD_V_FOV = math.degrees(camera.V_FOV)

            xdist = x_distance_to_target((north_out, east_out))
            cdepth = shm.kalman.depth.get()
            cpitch = shm.kalman.pitch.get()
            #TODO: technically, this math is wrong: angle is not proportional to height from the center
            # example: if we had a very, very tall camera image, then 89degrees would take up a huge part of
            # the screen but 0 degrees would be normal sized
            angl = (((shm.camera.forward_height.get()/2.0 - ypos) / shm.camera.forward_height.get()) *
                    FORWARD_V_FOV + cpitch)
            ydist = xdist * math.tan(math.radians(angl))

            buoy_depth = cdepth - ydist
            self.likely_depths.append((buoy_depth, prob_out))
            expected_depth = self.settings_var.expected_depth.get()

            #Calculate final depth based on probable depth list
            def get_likely_depth():
                # Ignore really way-off depths
                likely_depths = [(x,y) for (x,y) in self.likely_depths if abs(x-expected_depth) < 1.0]

                if len(likely_depths) < 7: #TODO: Remove constant
                    return 0 #TODO: 0 indicates no depth data!
                #Calculate final depth using depth guesses weighted based on their corresponding locator probabilities
                prob_sum = sum(map(lambda (_,y): y, likely_depths))
                final_depth = sum(map(lambda (x,y): x * (y / prob_sum), likely_depths))
                final_depth = max( 0.35 , final_depth) #TODO: Move this constant
                return final_depth

            self.output_var.target_depth.set(get_likely_depth())

        if self.display:
            self.output_pos.set_data([east_out], [north_out])
