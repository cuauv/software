#!/usr/bin/env python3

from build import ninja_common

build = ninja_common.Build("auv_math/libquat")
build.build_c_shared('quat', ['quat.c'])
