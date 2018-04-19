#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('deadman')

build.install('auv-deadman', f = 'deadman/deadman.py')
