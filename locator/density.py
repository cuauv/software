import numpy
import pylab 
import math
from math import degrees, atan2, radians, cos, sin
import random
import scipy.ndimage

num_type = numpy.float32

def normalize_volume(array):
    ''' Makes a new array with total density of 1 and the same shape as array '''
    array /= numpy.sum(array)
def normalize_max(array):
    array /= numpy.max(numpy.abs(array))

def Gauss(x, mu, sigma):
    x = float(x)
    mu = float(mu)
    sigma = float(sigma)
    return math.exp(-(x-mu)**2/(2*sigma**2))/math.sqrt(2*math.pi*sigma**2)

def MakeGauss(width,height, x,y, x_sigma, y_sigma):
    ''' Makes a (nearly) Gaussian distribution around point x,y of given
    sigmas and of total size (width,height) '''

    #w = min(6*x_sigma,width)
    #h = min(6*y_sigma,height)
    w= width
    h = height

    gxs = numpy.array([Gauss(i-w/2,0,x_sigma) for i in range(w) if i+x-w/2 >= 0 and i+x-w/2 < width], dtype = num_type)
    gys = numpy.array([Gauss(j-h/2,0,y_sigma) for j in range(h) if j+y-h/2 >= 0 and j+y-h/2 < height], dtype = num_type)
    mat = numpy.outer( gxs, gys )
    bounds = [min(width,max(0,x-w/2)), min(width, max(0,x+w/2)), min(height, max(0,y-h/2)), min(height, max(0,y+h/2))]

    d = numpy.ones((width,height), dtype = num_type)*gxs[0]*gys[0]
    d[bounds[0]:bounds[1], bounds[2]:bounds[3]] = mat
    return d

def MakeUniform(w,h):
    ''' Makes a uniform probability density function of a given size '''
    return numpy.ones((w,h), dtype = num_type) / (w*h)

def Infer( H, EH ):
    '''
    Return a new density which gives the probability of the hypothesis
    at each point given a piece of evidence E which has probability
    P(E|H) = EH at each point
    Applies Bayesian Inference at each point.

    H is the probability of a hypothesis (eg: location of something is (x,y))
    EH is the probability of evidence E arising given that hypothesis H is
        true at the point (x,y)
    '''

    temp = H*EH
    return temp / numpy.sum(temp)

class Evidence:
    def __init__(self, array, prob_false_positive, prob_false_negative, blur=0, center = (0,0)):
        #self.E = array*(1-prob_false_negative) + (1-array)*prob_false_positive
        self.E = array
        self.prob_false_positive = prob_false_positive
        self.prob_false_negative = prob_false_negative
        self.center = center

        if blur != 0:
            self.E = scipy.ndimage.filters.gaussian_filter(self.E, blur, 0, mode="constant")
    
    def InferFor(self, H, x,y, angle):
        ''' Updates H (in place) given the evidence occuring at x,y '''
        E = scipy.ndimage.interpolation.rotate(self.E, -angle)
        E *= (1-self.prob_false_negative-self.prob_false_positive)
        w,h = E.shape

        cx = self.center[0]*cos(radians(angle)) + self.center[1]*sin(radians(angle))
        cy = -self.center[0]*sin(radians(angle)) + self.center[1]*cos(radians(angle))
        ox = E.shape[0]/2+cx #offset x
        oy = E.shape[1]/2+cy #offset y

        # Bayesian Inference
        block = H[x-ox:x+w-ox,y-oy:y+h-oy].copy()
        H *= self.prob_false_positive
        H[x-ox:x+w-ox, y-oy:y+h-oy] += E*block
        H /= numpy.sum(H)

        #pylab.plot( [y-oy, y+h-oy], [x-ox, x+w-ox] , "o")

    def InferAgainst(self, H, x,y, angle):
        ''' Updates H (in place) given the evidence not occuring at x,y '''
        E = scipy.ndimage.interpolation.rotate(self.E, -angle)
        E *= (self.prob_false_negative-(1-self.prob_false_positive))
        w,h = E.shape

        cx = self.center[0]*cos(radians(angle)) + self.center[1]*sin(radians(angle))
        cy = -self.center[0]*sin(radians(angle)) + self.center[1]*cos(radians(angle))
        ox = E.shape[0]/2+cx #offset x
        oy = E.shape[1]/2+cy #offset y

        # Bayesian Inference
        block = H[x-ox:x+w-ox,y-oy:y+h-oy].copy()
        H *= (1-self.prob_false_positive)
        H[x-ox:x+w-ox, y-oy:y+h-oy] += E*block
        H /= numpy.sum(H)


# For timeit testing
test_prep = """
import density
import numpy
num_type = numpy.float32
E = density.Evidence(numpy.ones((100,100), dtype = num_type),0.2,0.2,blur=1)
I = numpy.ones( (1000,1000), dtype = num_type )
"""
test = "E.InferFor(I, 500,500, 0)"
            


#Actual script
if __name__ == "__main__":
    w,h = 500,500 # Should be even
    H = MakeGauss(w,h, 250,250, 50,50)

    I = H

    import matplotlib

    import matplotlib.colors
    norm = matplotlib.colors.LogNorm(1e-7,1)
    imgplot = pylab.imshow(I, cmap=matplotlib.cm.hot, picker=True, norm=norm)
    imgplot.set_interpolation("nearest")
    ticks =[1,1e-1,1e-2,1e-3,1e-4,1e-5,1e-6]
    colorbar = pylab.colorbar(ticks=ticks)
    colorbar.set_ticklabels([str(x) for x in ticks])

    shape = numpy.zeros( (100,100), dtype = num_type )
    shape[10:-10,10:-10] = numpy.ones( (80,80), dtype = num_type )
    shape[40:60,40:60] = numpy.zeros( (20,20), dtype = num_type)
    #shape = numpy.ones( (5,100), dtype = num_type )

    i = 0
    E = Evidence(shape, 0.2,0.2, blur=5, center=(0,-50))
    def on_pick(event):
        mouseevent = event.mouseevent
        my,mx = mouseevent.xdata, mouseevent.ydata

        global I
        if mouseevent.button == 1:
            E.InferFor(I, mx, my, 45)
        elif mouseevent.button == 3:
            E.InferAgainst(I, mx, my, 45)
            
        imgplot.set_data(I)
        pylab.draw()

    pylab.gcf().canvas.mpl_connect("pick_event", on_pick)
    pylab.show()
