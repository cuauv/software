#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('object-recognition')

build.install('auv-classifierd', f='object-recognition/classifier.py')
