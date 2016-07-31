#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('auv_math')

build.build_shared('math', ['camera.cpp'],
                   pkg_confs=['eigen3'],
                   cflags=['-Wno-deprecated-declarations'])
