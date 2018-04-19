#!/usr/bin/env python3

import sys
import time

import cv2

from CaptureSource import CaptureSource

class DFK23U445(CaptureSource):
    def __init__(self, direction, index=0):
        super().__init__(direction)
        self.camera = cv2.VideoCapture(index)
        self.camera.set(cv2.CAP_PROP_BRIGHTNESS, 500.0)
        self.camera.set(cv2.CAP_PROP_CONTRAST, 500.0)
        self.camera.set(cv2.CAP_PROP_SATURATION, 500.0)
        self.camera.set(cv2.CAP_PROP_HUE, 500.0)
        self.camera.set(cv2.CAP_PROP_GAIN, 500.0)
        self.camera.set(cv2.CAP_PROP_EXPOSURE, 500.0)

    def acquire_next_image(self):
        _, image = self.camera.read()
        if image is not None:
            image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)

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

    DFK23U445(direction).acquisition_loop()
