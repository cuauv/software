#!/usr/bin/env python3
import cv2
from vision import camera_message_framework
from nanomsg import Socket, PUB
import argparse


SERVER_ADDR = "tcp://0.0.0.0:8081"


def pack_image(img):
    _, jpeg = cv2.imencode('.jpg', img, (cv2.IMWRITE_JPEG_QUALITY, 100))
    return jpeg.tobytes()


class StreamServer:
    def __init__(self, direction):
        self.capture_source_framework = camera_message_framework.Accessor(direction)

    def run(self):
        with Socket(PUB) as sock:
            sock.bind(SERVER_ADDR)
            while True:
                res = self.capture_source_framework.get_next_frame()
                if res in [camera_message_framework.FRAMEWORK_QUIT,
                           camera_message_framework.FRAMEWORK_DELETED]:
                    continue
                img, _ = res
                sock.send(pack_image(img))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Server to stream images from a camera to clients')
    parser.add_argument('direction', metavar='dir', type=str,
                        help='camera direction (e.g., forward or downward)')
    args = parser.parse_args()
    StreamServer(args.direction).run()

