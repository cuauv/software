#!/usr/bin/env python3

import argparse
import os
import time
import itertools
import sys

import cv2
import shm

import CaptureSource

class ImageDirectory(CaptureSource.CaptureSource):
    def __init__(self, direction, directory, loop=True, fps=60.0, shmlog=False):
        super().__init__(direction)
        self.directory = directory

        files = []
        for name in os.listdir(directory):
            fname = os.path.join(directory, name)
            im = cv2.imread(fname)
            if im is not None:
                files.append(fname)

        if loop:
            self.file_iter = itertools.cycle(files)
        else:
            self.file_iter = iter(files)

        self.fps = float(fps)
        self.loop = loop
        self.last_time = time.time()

    def acquire_next_image(self):
        next_file = next(self.file_iter)
        next_image = cv2.imread(next_file)

        time_to_sleep = 1 / self.fps - (time.time() - self.last_time)
        if time_to_sleep > 0:
            time.sleep(time_to_sleep)

        acq_time = time.time()
        self.last_time = acq_time

        return next_image, acq_time

def main():
    parser = argparse.ArgumentParser(description='Process a directory of images as a video')
    parser.add_argument('direction', help='direction to act as a camera')
    parser.add_argument('directory', help='directory of images')
    parser.add_argument('--fps', default=60.0, type=float, help='FPS to run the module at')

    args = parser.parse_args()

    cap = ImageDirectory(args.direction, args.directory, fps=args.fps)
    cap.acquisition_loop()

if __name__ == '__main__':
    main()
