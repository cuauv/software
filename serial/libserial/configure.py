#!/usr/bin/env python3

from build import ninja_common

build  = ninja_common.Build('serial/libserial')

build.generate(['proto/cpp/DeviceConfig.pb.cc', 'proto/cpp/DeviceConfig.pb.h', 'proto/python/DeviceConfig_pb2.py'], 'serial/libserial/build_proto.sh', ['proto/DeviceConfig.proto'])

build.build_shared_static('auv-serial',
                   [
                       'Variable.cpp',
                       'DeviceInfo.cpp',
                       'Manager.cpp',
                       'Packet.cpp',
                       'Register.cpp',
                       'Talker.cpp',
                       'UnixFilePort.cpp',
                       'SerialPort.cpp',
                       'Log.cpp',
                       'packet_types/Hello.cpp',
                       'packet_types/Reset.cpp',
                       'packet_types/Heartbeat.cpp',
                       'packet_types/ReadRange.cpp',
                       'packet_types/WriteRange.cpp',
                       'packet_types/ReadIndexed.cpp',
                       'packet_types/WriteIndexed.cpp',
                       'packet_types/Disconnect.cpp',
                       'packet_types/Error.cpp',
                       'proto/cpp/DeviceConfig.pb.cc',
                   ],
                   implicit=['serial/libserial/proto/cpp/DeviceConfig.pb.h'],
                   cflags=['-Wall', '-Wextra', '-Werror'],
                   pkg_confs=['protobuf-lite'])

build.test_gtest('libserial',
                [
                    'tests/RegisterRange.cpp',
                    'tests/RegisterSet.cpp',
                    'tests/BoundRegisterSet.cpp',
                    'tests/Hello.cpp',
                    'tests/Reset.cpp',
                    'tests/Disconnect.cpp',
                    'tests/DeviceInfo.cpp',
                ],
                auv_deps=['auv-serial'],
                pkg_confs=['protobuf-lite'])
