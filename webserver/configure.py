#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('webserver')

build.webpack('static/bundle.js', 'webpack.config.js', 'src', 'webserver/package.json')

build.install('auv-webserver', f='webserver/auv-webserver.py')
