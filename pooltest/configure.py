#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('pooltest')

if build.stack('pooltest/bin/pooltest', 'pooltest', ['shm']):
  build.generate(['pooltest.cabal', 'stack.yaml'], 'pooltest/gen.sh', ['gen.sh'])
  build.install('auv-pooltest', f='pooltest/bin/pooltest')
