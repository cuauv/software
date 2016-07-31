#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('sonard')

build.install('libbvtsdk.so.3', f='sonar/bvtsdk/lib/libbvtsdk.so.3')

build.build_cmd('auv-sonard', ['sonard.cpp'], auv_deps=['shm', 'auv-camera-message-framework'], cflags=['-Isonar/bvtsdk/include -Ofast'], lflags=['-Lsonar/bvtsdk/lib -lbvtsdk'], pkg_confs=['opencv'])
