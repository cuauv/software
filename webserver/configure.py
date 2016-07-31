#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('webserver')

build.install('auv-webserver', f='webserver/auv-webserver.py')
