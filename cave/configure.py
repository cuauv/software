#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('cave')

build.install('auv-cave', f='cave/main.py')
build.install('auv-cave-merge', f='cave/merge.py')


