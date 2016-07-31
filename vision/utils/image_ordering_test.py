#!/usr/bin/env python3

import cv2
import numpy as np
from vision import camera_message_framework
import itertools
import time

shape = (500, 500, 3)
size = 1
for dim in shape:
    size *= dim

def image_of(axes):
    im = np.zeros(shape, dtype=np.uint8)
    im[:, :, axes] = 255
    return im

black = image_of([]), 'black'
red = image_of([2]), 'red'
green = image_of([1]), 'green'
blue = image_of([0]), 'blue'
yellow = image_of([2, 1]), 'yellow'
cyan = image_of([1, 0]), 'cyan'
pink = image_of([0, 2]), 'pink'
white = image_of([0, 1, 2]), 'white'

images = [black, red, green, blue, yellow, cyan, pink, white]

f = camera_message_framework.Creator('forward', size)

def main():
    for im, name in itertools.cycle(images):
        f.write_frame(im, int(time.time() * 1000))
        print('wrote {}'.format(name))
        time.sleep(1)


if __name__ == '__main__':
    main()
