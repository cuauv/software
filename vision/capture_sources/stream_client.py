#!/usr/bin/env python3
import time
import cv2
import argparse
import numpy as np
from nanomsg import Socket, SUB, SUB_SUBSCRIBE
from vision.capture_sources.CaptureSource import CaptureSource


def unpack_image(msg):
    return cv2.imdecode(np.fromstring(msg, dtype='uint8'), cv2.IMREAD_COLOR)


class StreamClient(CaptureSource):
    def __init__(self, server_addr, direction):
        super().__init__(direction + '_stream')
        self.server_addr = 'tcp://{}:8081'.format(server_addr)
        self.sock = Socket(SUB)
        self.sock.connect(self.server_addr)
        self.sock.set_string_option(SUB, SUB_SUBSCRIBE, '')
        print('Started streaming capture source {} from {}'.format(self.direction, self.server_addr))

    def acquire_next_image(self):
        img = unpack_image(self.sock.recv())
        return img, time.time()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Client that streams images from a camera to emulate a local capture source')
    parser.add_argument('direction', metavar='dir', type=str,
                        help='camera direction (e.g., forward or downward)')
    parser.add_argument('server_addr', type=str,
                        help='address of the streaming server')
    args = parser.parse_args()
    StreamClient(args.server_addr, args.direction).acquisition_loop()

