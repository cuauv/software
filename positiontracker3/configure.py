#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('positiontracker3')

build.install('auv-position-tracker', f='positiontracker3/main.py')
