#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('pooltest')

build.install('auv-pooltest', f='pooltest/auv-pooltest.py')
