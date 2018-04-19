#!/usr/bin/env python3

from build import ninja_common

build = ninja_common.Build("meow")

build.webpack("meow", "meow")
