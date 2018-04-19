#!/usr/bin/env python3

from build import ninja_common

build  = ninja_common.Build('can')

build.build_static('auv-can',
                   [
                       'lib/CANSocket.cpp',
                       'lib/Log.cpp',
                       'lib/MessageBuilder.cpp',
                       'lib/MessageData.cpp',
                       'lib/Message.cpp',
                       'lib/Selector.cpp',
                   ])

build.generate(['gen/ShmRegistry.cpp', 'gen/ShmDevices.cpp'], 'can/generate.py', ['templates/ShmRegistry.cpp', 'templates/ShmDevices.cpp', 'config/devices.json', 'config/hardware.json'])

build.build_cmd('auv-cand',
                [
                    'gen/ShmRegistry.cpp',
                    'Command.cpp',
                    'Culler.cpp',
                    'Device.cpp',
                    'Group.cpp',
                    'Handler.cpp',
                    'host.cpp',
                    'Manager.cpp',
                    'Poller.cpp',
                    'Registry.cpp',
                    'ShmMessageBuilder.cpp',
                    'SimpleRegistry.cpp',
                    'Thread.cpp',
                ],
                auv_deps=['shm'],
                auv_static_deps=['auv-can'],
                pkg_confs=['ncurses'])

build.build_cmd('auv-thrust-helm',
                [
                    'gen/ShmDevices.cpp',
                    'thrust-helm.cpp',
                ],
                auv_deps=['shm'],
                pkg_confs=['ncurses'])

build.build_cmd('auv-can-vdev',
                [
                    'util/vdev.cpp',
                    'util/FakeDev.cpp',
                ],
                auv_static_deps=['auv-can'],
                pkg_confs=['ncurses'])

build.build_cmd('auv-can-mon',
                [
                    'util/mon.cpp',
                ],
                auv_static_deps=['auv-can'])

build.build_cmd('auv-can-sendParam',
                [
                    'util/sendParam.cpp',
                ],
                auv_static_deps=['auv-can'])

build.build_cmd('auv-can-commitParam',
                [
                    'util/commitParam.cpp',
                ],
                auv_static_deps=['auv-can'])

build.build_cmd('auv-can-getParam',
                [
                    'util/getParam.cpp',
                ],
                auv_static_deps=['auv-can'])

build.build_cmd('auv-can-reset',
                [
                    'util/reset.cpp',
                ],
                auv_static_deps=['auv-can'])
