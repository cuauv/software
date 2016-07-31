#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('aslam')

build.generate(['aslam.cabal'], 'aslam/aslam.cabal.sh', ['aslam.cabal.sh'])
build.stack('aslam/bin/aslam', 'aslam', ['shm'])
build.install('auv-aslam', f='aslam/bin/aslam')
