#!/usr/bin/python

from build import ninja_common

build = ninja_common.Build('mission/opt')

build.build_shared('opt', [
  'src/run.cpp'
  ],
  deps      = ['nanomsg'],
  auv_deps  = ['shm', 'conf', 'auvlog', 'optmath'],
  pkg_confs = ['python3'],
  lflags    = ['-lboost_python3'],
  cflags    = ['-Wno-deprecated-declarations']
)

build.build_shared('optmath', [
  'src/opt.cpp'
  ],
  auv_deps  = ['shm'],
  pkg_confs = [],
  lflags    = [],
  cflags    = ['-Wno-deprecated-declarations']
)
