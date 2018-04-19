#!/usr/bin/env python3

from build import ninja_common

build = ninja_common.Build('auvlog')

build.install('auv-lr',   f='auvlog/reader.py')
build.install('auv-ld',   f='auvlog/daemon.py')
build.install('auv-log',  f='auvlog/logger.py')

build.build_shared('auvlog',
  [   
      'client.cpp'
  ])

build.build_cmd('auvlog-example', ['example.cpp'], deps = ['nanomsg'], auv_deps = ['auvlog'])
