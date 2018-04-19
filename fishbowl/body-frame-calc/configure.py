#!/usr/bin/env python

from build import ninja_common

build = ninja_common.Build("fishbowl/body-frame-calc")

files = [
    'main.cpp',
]

build.build_cmd('auv-body-frame-calc',
        files,
        pkg_confs=['eigen3'],
        auv_deps=[],
        lflags=[],
        cflags=[])
