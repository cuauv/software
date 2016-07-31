#!/usr/bin/env python3

import sys
import time

import cv2

from CaptureSource import CaptureSource

class GenericVideoCapture(CaptureSource):
    def __init__(self, direction, index=0):
        super().__init__(direction)
        self.camera = cv2.VideoCapture(index)

    def acquire_next_image(self):
        _, image = self.camera.read()
        acq_time = time.time()
        return image, acq_time

if __name__ == "__main__":
    if len(sys.argv) == 1:
        direction = "forward"
        print("Defaulting to %s direction." % direction)

    elif len(sys.argv) == 2:
        direction = sys.argv[1]
        print("Running on %s direction." % sys.argv[1])

    else:
        print("GenericVideoCapture only supports one argument: direction name")
        sys.exit(1)

    GenericVideoCapture(direction).acquisition_loop()
