#!/usr/bin/env python3

import queue
import sys

sys.path.insert(0, '../')
import hydrocomms

if __name__ == '__main__':
    q = queue.Queue(maxsize=1)
    r = hydrocomms.Receive(
        q,
        gain_plot=('-gain_plot' in sys.argv),
        correlation_plot=('-correlation_plot' in sys.argv),
        dump=('-dump' in sys.argv)
    )

    while True:
        try:
            print('Got message: ' + str(q.get(timeout=0.1)))
        except queue.Empty:
            pass