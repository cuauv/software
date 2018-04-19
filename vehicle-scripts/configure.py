#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('vehicle-scripts')

build.install("auv-env-set", "vehicle-scripts/auv-env/auv-env-set.sh")
