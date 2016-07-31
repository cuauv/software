''' Basic implementation of locator for use on computers
that don't have OpenCL.'''


import numpy
from numpy import float32, abs, maximum, minimum
from math import pi, radians

class Locator(object):
    def __init__( self, north, east, length, size, sigma ):
        north = numpy.linspace(north-length/2., north+length/2., size)
        east = numpy.linspace(east-length/2., east+length/2., size)
        easts, norths = numpy.meshgrid(east,north)
        norths = norths.astype(numpy.float32).reshape(-1)
        easts = easts.astype(numpy.float32).reshape(-1)

        # 1D array giving the x coordinate of each point
        self.norths = norths
        # 2D array giving the y coordinate of each point
        self.easts = easts
        self.probabilities = MakeGauss(size, size,
                                       size/2, size/2,
                                       sigma*(size/length),
                                       sigma*(size/length)).astype(float32)
        # probabilities is for internal use and is a 1D array
        self.probabilities = self.probabilities.reshape( -1 )

    def update(self, sub_pos, angle_offset, min_dist, max_dist, width, in_weight, out_weight):
        '''
        Perform one update on the probabilities by using the evidence that
        the sub is at position sub_pos, the target is seen at an absolute heading
        of `angle` and is most likely between min_dist and max_dist away.
        in_weight gives the chance that for every point in the region,
        if the buoy is there then we would get this result
        i.e. in_weight = P(this measurement | buoy at point p) for p in our region
        out_weight is the same but for points outside the region
        '''

        x,y = sub_pos

        # Find relative displacement from the sub
        rel_x = self.norths-x
        rel_y = self.easts-y

        # Determine angle in range (-pi,pi) relative to the direction of interest
        angle = angle_from( rel_x, rel_y, radians(angle_offset))

        # Restrict to the angles of interest
        evidence = angle_clamp(angle, width)

        # Compute distance from sub
        dist = (rel_x)*(rel_x) + (rel_y)*(rel_y);

        # Exclude points outside of our region of interest
        evidence *= (dist > min_dist**2) * (dist < max_dist**2);

        # Actually update the probabilities
        self.probabilities *= (in_weight-out_weight)*evidence + out_weight;

        #Normalize
        total_prob = numpy.sum( self.probabilities )
        self.probabilities /= total_prob

    def get_max(self):
        '''
        Finds the point of maximum probability and returns (north, east, probability)
        '''
        #cl.enqueue_read_buffer(cl_queue, self.prob_buf, self.probabilities).wait()
        #print sum(self.probabilities)

        loc = numpy.argmax(self.probabilities)

        north = self.norths[loc]
        east = self.easts[loc]
        prob = self.probabilities[loc]

        #normalization factor
        total_prob = numpy.sum( self.probabilities )

        return north, east, prob/total_prob

# Utility math functions
import math
num_type = numpy.float32
def Gauss(x, mu, sigma):
    x = float(x)
    mu = float(mu)
    sigma = float(sigma)
    return math.exp(-(x-mu)**2/(2*sigma**2))/math.sqrt(2*math.pi*sigma**2)
def MakeGauss(width,height, x,y, x_sigma, y_sigma):
    ''' Makes a (nearly) Gaussian distribution around point x,y of given
    sigmas and of total size (width,height) '''
    w= width
    h = height

    gxs = numpy.array([Gauss(i-w/2,0,x_sigma) for i in range(w) if i+x-w/2 >= 0 and i+x-w/2 < width], dtype = num_type)
    gys = numpy.array([Gauss(j-h/2,0,y_sigma) for j in range(h) if j+y-h/2 >= 0 and j+y-h/2 < height], dtype = num_type)
    mat = numpy.outer( gxs, gys )
    return mat

def angle_from(x,y, angle):
    theta = numpy.arctan2(y,x)
    angle = (theta-angle) % (2*pi)
    angle -= (2*pi) * (angle > pi)
    return angle

def angle_clamp(angle, coefficient):
    return maximum( minimum( 1.05 - coefficient*abs(angle), 1), 0)
