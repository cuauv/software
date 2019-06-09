#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('conf/gui')

build.webpack('static/js/bundle.js', 'webpack.config.js', 'src', 'conf/gui/package.json')

build.install('auv-vehicle-config', f='conf/gui/server.py')
