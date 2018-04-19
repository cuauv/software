#!/usr/bin/env python3

from build import ninja_common
import os

build = ninja_common.Build('aslam')

build.install('auv-aslam-cli', f = 'aslam/auv-aslam-cli.py')

build.generate(['aslam.json'], 'conf/decomment.sh', ['aslam.conf'], depends = ['link-stage/auv-json-decomment'])

from files import files

templates = ['src/templates/%s' % f for f in files]
outputs   = ['src/%s' % f for f in files]

build.generate(outputs, 'aslam/generate.py', templates + ['aslam.json'])

build.build_cmd('auv-aslamd', [
  'src/interface/aslamd.cpp'
  ],
  deps      = ['nanomsg'],
  auv_deps  = ['shm', 'conf', 'auvlog', 'aslam', 'math'],
  pkg_confs = ['eigen3'],
  lflags    = [],
  cflags    = ['-ffast-math', '-Ofast'])

build.build_shared('aslam', [
  'src/math/fastslam.cpp'
  ],
  auv_deps  = ['shm'],
  pkg_confs = ['eigen3'],
  lflags    = [],
  cflags    = ['-ffast-math', '-Ofast'])
