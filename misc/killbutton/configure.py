#!/usr/bin/env python

from build import ninja_common
build = ninja_common.Build('misc/killbutton')

build.build_cmd('auv-killer',
                [
                    'killer.c',
                    'killsubmarine.cpp',
                    'findpowermate.c',
                ],
                auv_deps=['auvstate-cpp', 'auvstate'],
                pkg_confs=['glib-2.0', 'gthread-2.0'])
