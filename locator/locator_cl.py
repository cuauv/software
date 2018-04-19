import pyopencl as cl
from numpy import float32
from math import radians
from locator_numpy import Locator
import numpy

import os
cl_file_name = os.path.join( os.path.dirname(__file__), "evidence.cl" )
program_source = open(cl_file_name).read()

cl_context = cl.create_some_context()
cl_queue = cl.CommandQueue(cl_context)
cl_program = cl.Program(cl_context, program_source).build()

class LocatorCL(Locator):
    def __init__( self, north, east, length, size, sigma ):
        super(LocatorCL,self).__init__(north, east, length, size, sigma)

        # Load memory into OpenCL buffers
        mf = cl.mem_flags
        self.norths_buf = cl.Buffer(cl_context,
                                    mf.READ_ONLY | mf.COPY_HOST_PTR,
                                    hostbuf=self.norths)
        self.easts_buf = cl.Buffer(cl_context,
                                   mf.READ_ONLY | mf.COPY_HOST_PTR,
                                   hostbuf=self.easts)
        self.prob_buf = cl.Buffer(cl_context,
                                  mf.READ_WRITE | mf.COPY_HOST_PTR,
                                  self.probabilities.nbytes,
                                  hostbuf=self.probabilities)

    def update(self, sub_pos, angle, min_dist, max_dist, width, in_weight, out_weight):
        '''
        Perform one update on the probabilities by using the evidence that
        the sub is at position sub_pos, the target is seen at an absolute heading
        of `angle` and is most likely between min_dist and max_dist away.
        in_weight gives the chance that for every point in the region,
        if the buoy is there then we would get this result
        i.e. in_weight = P(this measurement | buoy at point p) for p in our region
        out_weight is the same but for points outside the region
        '''

        n,e = sub_pos
        cl_program.evidence(cl_queue, self.norths.shape, None,
                            self.norths_buf, self.easts_buf, self.prob_buf,
                            float32(n), float32(e),
                            float32(radians(angle)),
                            float32(min_dist**2),
                            float32(max_dist**2),
                            float32(width),
                            float32(in_weight),
                            float32(out_weight))
        #TODO ?
        cl.enqueue_read_buffer(cl_queue, self.prob_buf, self.probabilities).wait()

        #Normalize
        total_prob = numpy.sum( self.probabilities )
        self.probabilities  /= total_prob

        cl.enqueue_write_buffer(cl_queue, self.prob_buf, self.probabilities)
