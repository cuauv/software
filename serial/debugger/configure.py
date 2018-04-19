#!/usr/bin/env python3

from build import ninja_common

build  = ninja_common.Build('serial/debugger')

import subprocess
# Check for a pkg-config of gtkmm-3.0
HAVE_GTKMM = subprocess.call(['pkg-config', 'gtkmm-3.0']) == 0

if HAVE_GTKMM:
    build.build_cmd_with_static('auv-serial-debugger',
                [
                    'main.cpp',
                    'DebuggerWindow.cpp',
                    'StatusFlagWidget.cpp',
                    'VariableWidget.cpp',
                ],
                cflags=['-Wall', '-Wextra', '-Werror', '-Iserial/libserial'],
                auv_deps=['auv-serial'],
                pkg_confs=['gtkmm-3.0'],
                static_extra_deps=['protobuf-lite'])
else:
    print("Could not build serial debugger because you are missing libgtkmm-3.0")
