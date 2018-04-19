#!/usr/bin/env python3

import os
from build import ninja_common
build = ninja_common.Build('lcd')

required_file = "mpsse.h"
if os.path.isfile("/usr/include/%s" % required_file) or \
   os.path.isfile("/usr/local/include/%s" % required_file):

    build.build_cmd('auv-lcdd-test',
                    [
                        'lcdd.cpp',
                    ],
                    deps=['cairo'],
                    auv_deps=['shm', 'lcd'])

    build.build_shared('lcd',
                    [
                        'lcd.cpp',
                        'lcdwriter.cpp',
                    ],
                    deps=['mpsse', 'cairo'])

build.install('auv-lcdd', f='lcd/main.py')
build.install('auv-lcd-set', f='lcd/lcd-set.py')
