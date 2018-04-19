#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('shm_tools')

build.build_cmd('auv-shm',
                [
                    'client.cpp',
                    'main.cpp',
                    'ps.cpp',
                    'server.cpp',
                ],
                implicit=['libshm/c/checksum.h'],
                auv_deps=['shm'])
