#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('control/controlhelm')

build.build_cmd('auv-control-helm',
                [
                    'battery.cpp',
                    'new_pid_panel.cpp',
                    'row1.cpp',
                    'row3.cpp',
                    'display.cpp',
                    'controlhelm.cpp'
                ],
                auv_deps=['shm'],
                pkg_confs=['ncurses'])
