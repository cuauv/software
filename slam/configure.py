#! /usr/bin/env python3

from build import ninja_common
build = ninja_common.Build("slam")

build.build_cmd(
        'auv-slamd',
        [
            'ekf.cc',
            'slam.cc',
            'slam_filter.cc',
            'slam_server.cc',
            'proto/slam_msg.pb.cc',
        ],
        deps=['zmq', 'protobuf'],
        cflags=['-I /usr/include/eigen3 -Wno-reorder -Wno-sign-compare'],
        auv_deps=['shm'])
