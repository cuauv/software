import time

import numpy

from misc.log import with_logging
from vision import camera_message_framework as cmf
import shm

#Maps CAVE representation of cameras to the strings
#used for Posix IPC (the camera names within cave_test).
#This may be useful if these vision names are ever changed.
camera_map = {
    "Forward": "forward",
    "ForwardRight": "forward_right",
    "ForwardLeft": "forward_left",
    "Downward": "downward"
}

@with_logging
class CameraLink:

    # Initialize a camera link with the given frame dimensions
    def __init__(self, name, height=None, width=None, nChannels=None):
        self.name = camera_map[name]
        self.height = height
        self.width = width
        self.nChannels = nChannels

        self.dataSize = self.height * self.width * self.nChannels

        self.currentFrame = 0

        #Init shared memory
        self.framework = cmf.Creator(self.name, self.dataSize)

        # Set camera dimensions in SHM
        try:
            getattr(shm.camera, self.name + '_height').set(self.height)
            getattr(shm.camera, self.name + '_width').set(self.width)
        except AttributeError:
            # There are no SHM camera dimension variables
            pass

    def send_image(self, frame):
        if self.framework.valid():
            self.framework.write_frame(frame, int(time.time() * 1000))

    def cleanup(self):
        self.framework.cleanup()
