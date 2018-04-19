#!/usr/bin/env python3
import os
import sys
import time

import cv2

import shm

from CaptureSource import CaptureSource

class Video(CaptureSource):
    def __init__(self, direction, filename, loop=True, shmlog=False):
        super().__init__(direction)
        self.filename = filename

        if not os.path.exists(self.filename):
            raise OSError('Could not find video file {}'.format(self.filename))
        self.loop = loop
        self.cap = cv2.VideoCapture(self.filename)
        self.last_time = 0

    def acquire_next_image(self):
        _, next_image = self.cap.read()
        if next_image is None:
            if self.loop:
                self.cap.release()
                self.cap = cv2.VideoCapture(self.filename)
                return self.acquire_next_image()
        _time = 1. / self.cap.get(cv2.CAP_PROP_FPS) - (time.time() - self.last_time)
        if _time > 0:
            time.sleep(_time)

        self.last_time = time.time()
        return next_image, self.last_time

if __name__ == "__main__":
    if len(sys.argv) == 3:
        direction = sys.argv[1]
        filename = sys.argv[2]
        print("Running %s on %s direction." % (filename, direction))

    else:
        print("Video needs two arguments: direction filename")
        sys.exit(1)

    Video(direction, filename).acquisition_loop()
