#!/usr/bin/env python

from build import ninja_common

build = ninja_common.Build("conf/json-decomment")

files = [
    'main.cpp',
]

build.build_cmd('auv-json-decomment',
        files,
        auv_deps=['json'],
        lflags=[],
        cflags=[])
