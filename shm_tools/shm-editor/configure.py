#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('shm_tools/shm-editor')

build.install('auv-shm-editor', f='shm_tools/shm-editor/auv-shm-editor.py')
build.install('auv-plot', f='shm_tools/shm-editor/shm_plotter.py')
