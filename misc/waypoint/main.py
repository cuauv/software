#!/usr/bin/env python3

import argparse
import sys
import shm
import time
import math

all_dimensions = ['depth', 'heading', 'pitch', 'roll', 'north', 'east']
filepath = '/home/software/cuauv/workspaces/worktrees/master/misc/waypoint/data.csv'

# Find which line (from data.csv) starts with a given name.
# restores the line number if found, -1 otherwise.
def find_line_num(lines, name):
    for i, line in enumerate(lines):
        if line.split(',')[0] == name:
            return i
    return -1

# Create a line for the .csv containing current position info.
def generate_line(name, dimensions=all_dimensions):
    line = name
    kalman = shm.kalman.get()
    for dimension in dimensions:
        line += ',' + str(getattr(kalman, dimension))
    return line + '\n'

# Is the sub within a tolerance of its nav desires?
def close_enough(tolerance, desired_vals, dimensions=all_dimensions):
    kalman = shm.kalman.get()
    nav = shm.auv_nav.get()
    for i, dimension in enumerate(all_dimensions):
        if dimension in dimensions:
            if abs(getattr(kalman, dimension) - desired_vals[i]) > tolerance:
                return False
    return True

# Write a list of coordinates to a shm group.
def set_shm_coords(group, coords, dimensions=all_dimensions):
    result = group.get()
    for i, dimension in enumerate(all_dimensions):
        if dimension in dimensions:
            setattr(result, dimension, coords[i])
    group.set(result)

def exit(status):
    # Turn off position controls.
    settings = shm.navigation_settings.get()
    settings.position_controls = 0
    shm.navigation_settings.set(settings)
    # Zero navigation desires.
    sys.exit(status)

# Take CLI args.
parser = argparse.ArgumentParser()
parser.add_argument('-s', '--save', nargs='?', const='')
parser.add_argument('-r', '--restore', nargs='*')
parser.add_argument('-d', '--dimensions', default='dhprne')
parser.add_argument('-t', '--tolerance', type=float)
args = parser.parse_args()

# Enforce some argument rules.
if (args.save != None) + (args.restore != None) != 1:
    print("Use one of --save and --restore.")
    sys.exit(1)
if (args.restore == None) and (args.dimensions != 'dhprne'):
    print("Only use --dimensions along with --restore.")
    sys.exit(1)
if (args.restore == None) and (args.tolerance != None):
    print("Only use --tolerance along with --restore.")
    sys.exit(1)

if args.restore != None and len(args.restore) == 0:
    args.restore.append('')
if args.tolerance == None and args.restore != None:
    args.tolerance = (0.01 if len(args.restore) == 1 else 0.1)

# Save the sub's position to the correct line in data.csv.
if args.save != None:
    with open(filepath, 'r') as f:
        data = f.readlines()
    line_num = find_line_num(data, args.save)
    if line_num == -1:
        data.append('')
    data[line_num] = generate_line(args.save)
    with open(filepath, 'w') as f:
        f.writelines(data)
    print("Position saved.")
    sys.exit(0)

# restore the sub's position.
if args.restore != None:
    # Enable position controls.
    settings = shm.navigation_settings.get()
    settings.position_controls = 1
    shm.navigation_settings.set(settings)
    # Only consider dimensions specified by the user.
    considered_dimensions = [d for d in all_dimensions if d[0] in args.dimensions]
    with open(filepath, 'r') as f:
        data = f.readlines()
    for name in args.restore:
        line_num = find_line_num(data, name)
        if line_num == -1:
            print("No position found named " + name + ".")
            exit(1)
        coords = [float(x) for x in data[line_num].split(',')[1:]]
        set_shm_coords(shm.navigation_desires, coords, considered_dimensions)
        # Wait until the sub has reached the saved position.
        while not close_enough(args.tolerance, coords, considered_dimensions):
            time.sleep(0.1)
    exit(0)
