#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('mission')

build.install('auv-mission-runner', f='mission/runner.py')
