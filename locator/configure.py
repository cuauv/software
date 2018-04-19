#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('locator')


#Install logutils
build.install('auv-locatord', f='locator/locatord.py')
