#!/usr/bin/env python3

from build import ninja_common

build = ninja_common.Build('serial/cli')

build.generate(['program/loader/proto/cpp/AUVFirmware.pb.cc', 'program/loader/proto/cpp/AUVFirmware.pb.h'],
        'serial/cli/program/loader/build_proto.sh',
        ['program/loader/proto/AUVFirmware.proto'])

build.build_cmd_with_static('auv-serial',
                [
                    'reset/reset.cpp',
                    'info/info.cpp',
                    'simpled/simpled.cpp',
                    'fakedev/fakedev.cpp',
                    'fakedev/FakeDevice.cpp',
                    'fakedev/DeviceTalker.cpp',
                    'program/loader/MemoryImage.cpp',
                    'program/loader/hex.cpp',
                    'program/loader/AUVFirmware.cpp',
                    'program/loader/auvfw.cpp',
                    'program/loader/proto/cpp/AUVFirmware.pb.cc',
                    'program/dumpFW.cpp',
                    'program/buildFW.cpp',
                    'program/mergeHex.cpp',
                    'program/flash.cpp',
                    'program/BootTalker.cpp',
                    'Command.cpp',
                    'main.cpp',
                ],
                implicit=['serial/cli/program/loader/proto/cpp/AUVFirmware.pb.h'],
                cflags=['-Wall', '-Wextra', '-Werror', '-Iserial/libserial'],
                auv_deps=['auv-serial'],
                pkg_confs=['ncurses', 'protobuf-lite'])
