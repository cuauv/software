#!/usr/bin/env python3

''' Run this script to increase laod on a system (useful for stress-testing cpu heating)
    Credits to Stackoverflow for the multiprocessing code:
        http://stackoverflow.com/questions/1408356/keyboard-interrupts-with-pythons-multiprocessing-pool
'''

import multiprocessing
import sys

class KeyboardInterruptError(Exception): pass

def load_function(x):
    try:
        while True:
            pass
    except KeyboardInterrupt:
        raise KeyboardInterruptError()

def spawn_processes(n):
    p = multiprocessing.Pool(processes=n)
    try:
        print('starting the pool map')
        p.map(load_function, range(10))
        p.close()
        print('pool map complete')
    except KeyboardInterrupt:
        print('got ^C while pool mapping, terminating the pool')
        p.terminate()
        print('pool is terminated')
    except Exception as e:
        print('got exception: "{}", terminating the pool'.format(e))
        p.terminate()
        print('pool is terminated')
    finally:
        print('joining pool processes')
        p.join()
        print('join complete')
    print('All processes stopped')

if __name__ == '__main__':
    number_of_infinite_loops = int(sys.argv[1]) if len(sys.argv) > 1 else multiprocessing.cpu_count()
    print("Starting cpu test with {} processes...".format(number_of_infinite_loops))
    spawn_processes(number_of_infinite_loops)
