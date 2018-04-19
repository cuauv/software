#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('control')

build.install('auv-controld3', f='control/auv_controld3.py')
build.install('auv-navigated', f='control/auv_navigated.py')
