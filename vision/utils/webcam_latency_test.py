#!/usr/bin/env python3

import cv2
import numpy as np
from vision import camera_message_framework
import itertools
import time

cap = cv2.VideoCapture(0)

framework = None

def main():
    global framework
    while True:
        _, im = cap.read()

        if framework is None:
            framework = camera_message_framework.Creator('forward', im.size)
        framework.write_frame(im, int(time.time() * 1000))


if __name__ == '__main__':
    main()
