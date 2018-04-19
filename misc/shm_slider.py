#!/usr/bin/env python3

"""
Control live shm variables precisely.

Use arrow keys, Vim keys, your scroll wheel, or your touchpad!
"""

import sys
from collections import namedtuple
import curses
import shm

Args = namedtuple('Args', ['shm_path', 'shm_var', 'inc'])

def main(stdscr):
    args = parse_args()
    curses.curs_set(False) # Hide cursor

    def show_shm():
        stdscr.clear()
        stdscr.addstr(0, 0, 'j         k         u         d         q')
        stdscr.addstr(1, 0, 'UP       DOWN    BIG UP    BIG DOWN    QUIT')

        current = args.shm_var.get()
        if isinstance(current, int):
            current_str = str(current)
        else:
            current_str = '{0:.3f}'.format(current)

        stdscr.addstr(3, 0, 'SHM variable {} currently: {}'.format(
            args.shm_path,
            current_str,
        ))
        stdscr.refresh()

    show_shm()

    while True:
        c = stdscr.getch()
        if c in [ord('j'), ord('J'), curses.KEY_DOWN]:
            args.shm_var.set(args.shm_var.get() - args.inc)
        elif c in [ord('k'), ord('K'), curses.KEY_UP]:
            args.shm_var.set(args.shm_var.get() + args.inc)
        elif c in [ord('d'), ord('D')]:
            args.shm_var.set(args.shm_var.get() - args.inc * 10)
        elif c in [ord('u'), ord('U')]:
            args.shm_var.set(args.shm_var.get() + args.inc * 10)
        elif c in [ord('q'), ord('Q')]:
            return
        else:
            continue

        show_shm()

def parse_args():
    if len(sys.argv) < 4:
        raise ValueError('Usage: auv-shm-slider <group> <var> <increment>')

    shm_path = '{}.{}'.format(*sys.argv[1:3])
    try:
        v = shm._eval(shm_path)
    except:
        raise ValueError('Could not evaluate shm path "{}"'.format(shm_path))

    if isinstance(v.get(), int):
        try:
            inc = int(sys.argv[3])
        except ValueError:
            raise ValueError('Increment "{}" is not an int'.format(sys.argv[3]))

    elif isinstance(v.get(), float):
        try:
            inc = float(sys.argv[3])
        except ValueError:
            raise ValueError('Increment "{}" is not a float'.format(sys.argv[3]))

    else:
        raise ValueError('Unknown increment type')

    return Args(shm_path, v, inc)

if __name__ == '__main__':
    parse_args()
    curses.wrapper(main)
