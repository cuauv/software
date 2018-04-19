#!/usr/bin/env python3

from build import ninja_common

build = ninja_common.Build('trogdor')

build.install('trogdor', f='trogdor/trogdor.sh')
build.install('trogdor_boot', f='trogdor/trogdor_boot.sh')
