#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('system_check')

build.install('auv-syscheck', f='system_check/syscheck.py')
