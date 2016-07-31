#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('healthdash')

build.build_cmd('auv-health-dash',
                [
                    'main.cpp',
                    'common-funcs.cpp',
                    'power.cpp',
                    'computer.cpp',
                    'other.cpp',
                    'temperature.cpp',
                ],
                auv_deps = ['shm'],
                deps = ['ncurses'])

build.install('auv-batteryalert.py')
