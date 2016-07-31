#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('deadman')

build.install('auv-deadman', f = 'deadman/deadman.py')
