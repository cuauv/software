#!/usr/bin/env python3

import socket
import struct
import numpy as np
import sys
import cv2
import time
import subprocess

direction_port_map = {'forward': 9000, 'downward': 9001, 'sonar': 9002}

def get_route(address):
    route = subprocess.check_output(['ip', 'route', 'get', address]).decode('utf-8')
    route_split = route.split()
    src_idx = route_split.index('src')
    return route_split[src_idx + 1]

def main(direction):
    if direction not in direction_port_map:
        print('"{}" is not a valid direction. Must be one of: {}'.format(
            direction, ', '.join(direction_port_map.keys())))
        return
    else:
        port = direction_port_map[direction]

    s = socket.socket(type=socket.SOCK_DGRAM)
    s.bind(('', port))
    route = get_route('192.168.0.93')
    s.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, socket.inet_aton('224.0.1.1') + socket.inet_aton(route))
    size = struct.calcsize('iii')

    while cv2.waitKey(25) != 27:
        acq_time, a = s.recvfrom(65535)
        acq_time, = struct.unpack('L', acq_time)
        d, a = s.recvfrom(65535)
        res = cv2.imdecode(np.frombuffer(d, dtype=np.uint8), cv2.IMREAD_COLOR)
        cv2.imshow(direction, res)
        print('latency: {}'.format(time.time()*1000 - acq_time))

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Please provide a camera direction (e.g. "forward" or "downward")')
        sys.exit(0)
    main(sys.argv[1])
