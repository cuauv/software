#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build("shm_tools/shm-notifier")

build.build_cmd("auv-shm-notifier", [
                    "main.cpp",
                ], auv_deps=["shm"])
