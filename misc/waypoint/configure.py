#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('misc/waypoint')

build.install('auv-wp', f='misc/waypoint/main.py')
