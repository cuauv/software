#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('control/control_helm2')

build.install('auv-control-helm2', f='control/control_helm2/control_helm2.py')
