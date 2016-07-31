#!/usr/bin/env python3

from build import ninja_common

build = ninja_common.Build("peacock")

build.generate(["regroup"], "peacock/build-regroup.sh", ["regroup.c"])

build.chicken_lib("fishbowl", [
        "fishbowl/queue.scm",
        "fishbowl/fishbowl.scm",
    ], where="peacock/fishbowl")

build.chicken_lib("peacock", [
        "peacock.scm",
        "peacock-internal.scm",
        "peacock-util.scm",
        "peacock-misc.scm"
    ], where="peacock", chicken_deps=["cuauv-shm", "fishbowl"])

build.chicken_exe("peck", [
        "peck.scm",
    ], chicken_deps=["peacock"])
