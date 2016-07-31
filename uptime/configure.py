#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('uptime')

build.install('auv-uptimed', f='uptime/uptime.py')
build.install('auv-uptime-view', f='uptime/view.py')
