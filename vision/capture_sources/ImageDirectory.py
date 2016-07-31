import os
import time
import itertools

import cv2
import shm

import CaptureSource

class ImageDirectory(CaptureSource.CaptureSource):
    def __init__(self, direction, directory, loop=True, fps=60.0, shmlog=False):
        super().__init__(direction)
        self.directory = directory

        files = [os.path.join(directory, f) for f in os.listdir(directory) if
                 os.path.isfile(os.path.join(directory, f))]
        if loop:
            self.files = itertools.cycle(files)
        else:
            self.files = iter(files)

        self.fps = float(fps)
        self.loop = loop
        self.last_time = time.time()

    def acquire_next_image(self):
        try:
            next_file = next(files)
        except StopIteration:
            return time.time(), None
        next_image = cv2.imread(next_file)

        time_to_sleep = 1 / self.fps - (time.time() - self.last_time)
        if time_to_sleep > 0:
            time.sleep(time_to_sleep)

        acq_time = time.time()
        self.last_time = acq_time

        return next_image, acq_time
