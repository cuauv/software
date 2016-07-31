#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('sensors/dvld')

build.build_cmd('auv-dvld',
                [
                    'main.cpp',
                    'dvlSerial.cpp',
                    'pd5Com.cpp',
                    'dvld.cpp',
                ],
                auv_deps=['shm'])
