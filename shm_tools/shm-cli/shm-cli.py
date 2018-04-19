#!/usr/bin/env python3
import argparse
from ctypes import c_double, c_float, c_int
from os.path import expanduser
import os
import sys
from subprocess import call
from misc.utils import watch_thread_wrapper

from tabulate import tabulate

import shm

__author__ = 'zander'

parser = argparse.ArgumentParser(description='A shared memory command line interface.', prog='auv-shm-cli')

# Positional arguments
parser.add_argument('group', type=str, nargs='?', help='shm group to select')
parser.add_argument('variable', type=str, nargs='?',
                    help='shm variable (in [group]) to select, omit to list all variables')
parser.add_argument('value', nargs='?', help='If provided, [group].[variable] will be set to [value]')

parser.add_argument('--watch', '-w', action='store_true', default=False,
                    help='Watch and print out updates as a group or variable changes')
parser.add_argument('--reset', '-r', action='store_true', default=False,
                    help='Reset a group or variable to the default value')

# Flags
parser.add_argument('--groups', action='store_true', default=False, help='List every shm group.')
parser.add_argument('--all', '-a', action='store_true', default=False,
                    help='List every shm group with all variables')

args = parser.parse_args()

all_shm_groups = shm.__all__[1:]  # "watchers" is the first, hardcoded element which is skipped.


def print_group(group_name, group=None):
    if group == None:
        group = getattr(shm, group_name)

    print("{}".format(group_name))
    print(tabulate(
        ((name, str(c_type).split("'")[1].split('_')[1], getattr(group, name).get()) for name, c_type in group._fields),
        headers=("Variable", "Type", "Value")))
    print()


if args.groups:
    for group_name in sorted(all_shm_groups):
        print(group_name)

    sys.exit()

if args.all:
    for group_name in sorted(all_shm_groups):
        print_group(group_name)

    sys.exit()

if args.group:
    if args.group in all_shm_groups:
        group = getattr(shm, args.group)
    else:
        print("Could not find group {}.".format(args.group))
        sys.exit(1)

    all_group_variables = group._fields

    if args.variable:
        if args.variable in (name for name, c_type in all_group_variables):
            variable = getattr(group, args.variable)
        else:
            print("Could not find variable {} in group {}.".format(args.variable, args.group))
            sys.exit(2)

        if args.reset:
                with open(expanduser("~/trunk/libshm/c/shm.c")) as f:
                    for line in f:
                        if "shm_set({}, {}, ".format(args.group, args.variable) in line:
                            v = line.split("shm_set({}, {}, ".format(args.group, args.variable))[1].split(")")[0]
                            call(["auv-shm-cli", args.group, args.variable, v])
                            exit()

        if args.value:
            variable_c_type = next((c_type for name, c_type in all_group_variables if name == args.variable))
            # Convert the value argument to the correct type.
            if variable_c_type in (c_float, c_double):
                try:
                    value = float(args.value)
                except ValueError:
                    print("Could not convert {} to float.".format(args.value))
                    sys.exit(3)
            elif variable_c_type == c_int:
                try:
                    value = int(args.value)
                except ValueError:
                    print("Could not convert {} to int.".format(args.value))
                    sys.exit(4)
            else:
                value = args.value

            try:
                variable.set(value)
            except TypeError as e:
                print("{}, but another type was assumed. This is most likely a bug with auv-shn-cli.".format(e))

        print(" {}.{} is currently {}".format(args.group, args.variable, variable.get()))
        if args.watch:
            def f(watcher, quit_event):
                watcher.watch(group)

                string = " {}.{} is currently {{}}".format(args.group, args.variable)
                while True:
                    watcher.wait(new_update=False)
                    if quit_event.is_set():
                        break

                    print(string.format(variable.get()))

            watch_thread_wrapper(f)

    # No variable given, display all variables in the group
    else:
        if args.reset:
            with open(os.path.join(os.getenv('CUAUV_SOFTWARE'), 'libshm/c/shm.c')) as f:
                for line in f:
                    if "shm_set({}".format(args.group) in line:
                        variable = line.split("shm_set({}, ".format(args.group))[1].split(")")[0].split(", ")
                        call(["auv-shm-cli", args.group] + variable)
        else:
            print_group(args.group, group)
        if args.watch:
            def f(watcher, quit_event):
                watcher.watch(group)

                while True:
                    watcher.wait(new_update=False)
                    if quit_event.is_set():
                        break

                    print_group(args.group, group)

            watch_thread_wrapper(f)

# No group given, display argparse's help message
else:
    parser.print_help()
