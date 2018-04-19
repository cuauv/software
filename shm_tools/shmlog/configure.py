#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('shm_tools/shmlog')

build.build_cmd('auv-shmlogd',
                [
                    'logd.cpp',
                ],
                deps=['popt'], auv_deps=['shm'])

#Install logutils
build.install('auv-shmlog-tocsv', f='shm_tools/shmlog/util/log2csv.py')
build.install('auv-shmlog-toarrays', f='shm_tools/shmlog/util/log2arrays.py')
build.install('auv-shmlog-playback', f='shm_tools/shmlog/util/logplayback.py')
build.install('auv-shmlog-view', f='shm_tools/shmlog/util/logview.py')
build.install('auv-shmlog-rebuild', f='shm_tools/shmlog/util/logrebuild.py')
build.install('auv-shmlog-benchmark', f='shm_tools/shmlog/util/logbenchmark.py')

