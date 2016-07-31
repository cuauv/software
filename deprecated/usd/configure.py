#!/usr/bin/python

from build import ninja_common

with open('usd/devices.txt', 'r') as f:
    devices = [line.strip() for line in f if not line.startswith('#')]

build  = ninja_common.Build('usd')

build.build_static('auv-usd-protocol', ['protocol.cpp'])

templates = ['templates/devices.cpp', 'templates/dev.cpp', 'templates/dev.h']
conf_files = ['configs/%s.conf' % d for d in devices]
cpp_files = ['devices/%s.cpp' % d for d in devices] + ['devices/devices.cpp']
h_files = ['devices/%s.h' % d for d in devices]
deps = conf_files + templates + ['devices.txt']
build.generate(cpp_files + h_files, 'usd/generate.py', deps)

build.build_cmd('auv-seriald',
                [
                'usd.cpp',
                'device.cpp',
                'pollblock.cpp',
                'autodetector.cpp',
                'filters.cpp',
                'devices/testDevice.cpp',
                ] + cpp_files,
                auv_deps=['shm', 'auvserial'],
                auv_static_deps=['auv-usd-protocol'])

build.build_cmd('auv-fakedevice',
                ['fakedevice.cpp'],
                deps=['popt'],
                auv_static_deps=['auv-usd-protocol'])

build.install('auv-serial-watch.sh')
