#!/usr/bin/env python

from build import ninja_common

build = ninja_common.Build('serial/seriald')
build.build_cmd(
        'auv-seriald',
        [
            'main.cpp',
            'config.cpp',
            'device.cpp',
            'device_list.cpp',
            'sub_status.cpp',
        ],
        deps=['nanomsg'],
        auv_deps=['shm', 'auvlog', 'auv-serial', 'fmt'],)

#build.test_gtest(
#        'seriald-new',
#        [
#            'config.cpp',
#            'device.cpp',
#            'device_list.cpp',
#            'sub_status.cpp',
#            # 'test/config/config.cpp',
#            # 'test/device/device.cpp',
#        ],
#        deps=['nanomsg', 'cppformat'],
#        auv_deps=['shm', 'auvlog', 'auv-serial'],)
