#!/usr/bin/python

from build import ninja_common

build = ninja_common.Build('fishbowl/cw-he-calc')
build.install('auv-cw-he-calc', f='fishbowl/cw-he-calc/main.py')
