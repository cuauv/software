#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('shm_tools/shm-cli')

build.install('auv-shm-cli', f='shm_tools/shm-cli/shm-cli.py')
