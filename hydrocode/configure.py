#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('hydrocode')
# build.install('libliquid.so', f='hydrocode/libliquid.so')
build.build_cmd('auv-hydromathd', ['hydromathd.cpp', 'udp_receiver.cpp'],
                auv_deps=['shm'], deps=['liquid'])

build.install('auv-hydro-receive', 'hydrocode/scripts/udp_recv_orig.py')
build.install('auv-ping-trigger', 'hydrocode/scripts/udp_recv_ping_trigger.py')
build.install('auv-hydro-heading', 'hydrocode/scripts/hydromath_ui.py')
