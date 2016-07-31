#!/usr/bin/env python

from build import ninja_common

build = ninja_common.Build("fishbowl")

build.install("auv-fishbowl-unpause", "fishbowl/unpause")
build.install("auv-fishbowl-pause", "fishbowl/pause")
build.install("auv-fishbowl-reset", "fishbowl/reset")

build.build_cmd("auv-fishbowl",
        ["main.cpp"],
        auv_deps=["shm", "conf", "fishbowl"],
        pkg_confs=["eigen3"],
        lflags=[],
        # XXX -Wno-deprecated-declarations is temporary, to deal with the following Eigen defect:
        # XXX The same is applied in .syntastic_cpp_config and in conf/configure.py.
        # XXX Remove as soon as the defect is resolved!
        # XXX http://eigen.tuxfamily.org/bz/show_bug.cgi?id=872
        cflags=["-Ifishbowl/lib", "-Wno-deprecated-declarations", "-pthread"])

build.build_shared("fishbowl",
        [
            "entity.cpp",
            "passive.cpp",
            "physics.cpp",
            "pid.cpp",
            "screw.cpp",
            "server.cpp",
            "simulator.cpp",
            "thrusters.cpp",
            "vision.cpp",
            "world.cpp",
            "bits.cpp",
            "bitreader.cpp",
            "bitwriter.cpp",
            "geometry.cpp",
            "objects.cpp",
        ],
        auv_deps=["shm", "conf"],
        pkg_confs=["eigen3"],
        lflags=[],
        cflags=["-Ifishbowl/lib", "-Wno-deprecated-declarations", "-pthread"])
