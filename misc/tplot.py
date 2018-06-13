#!/usr/bin/env python3

import argparse
from subprocess import PIPE, run
from tempfile import NamedTemporaryFile
from time import time, sleep
from curses import wrapper

import shm

parser = argparse.ArgumentParser(description='Graph some SHM')
parser.add_argument('variables', metavar='GROUP.VAR', type=str, nargs='+',
                    help='Variables to graph')
args = parser.parse_args()

tracks = { name: (shm._eval(name), NamedTemporaryFile()) for name in args.variables }
plotfile = NamedTemporaryFile()

with open(plotfile.name, "w") as f:
    f.write('set xlabel "Time"\n')
    f.write('set ylabel "Value"\n')
    f.write('set term dumb\n')

    plots = []
    for name, (var, filename) in tracks.items():
        plots.append('"{}" title "{}" with linespoint'.format(filename.name, name))

    f.write("plot ")
    f.write(",".join(plots))
    f.write("\n")


for name, (var, filename) in tracks.items():
    with open("{}".format(filename.name), "w") as f:
        f.write("# Time {}\n".format(name))

start_time = time()


def main(stdscr):
    while True:
        stdscr.clear()
        for name, (var, filename) in tracks.items():
            with open("{}".format(filename.name), "a") as f:
                f.write("{} {}\n".format(time() - start_time, var.get()))

        ret = run("gnuplot -c {} | grep --color=always -E \"A|B|C|$\"".format(plotfile.name), shell=True, stdout=PIPE, stderr=PIPE)
        ret = run("gnuplot -c {}".format(plotfile.name), shell=True, stdout=PIPE, stderr=PIPE)

        stdscr.addstr(ret.stdout)
        stdscr.refresh()

        sleep(.5)

wrapper(main)
