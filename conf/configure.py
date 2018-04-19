#!/usr/bin/env python

from build import ninja_common

build = ninja_common.Build("conf")

def decomment(name):
  build.generate(["%s.json" % name], "conf/decomment.sh", ["%s.conf" % name],
                 depends=['link-stage/auv-json-decomment'])

# Vehicles
decomment("artemis")
decomment("apollo")
decomment("simple")

# Simulator configs (unused)
decomment("simulator")

# Locales (for ASLAM)
decomment("teagle")
decomment("buoyland")

build.build_shared("conf",
        ["vehicle.cpp", "simulator.cpp", "map.cpp"],
        auv_deps=["json"],
        pkg_confs=["eigen3"],
        lflags=[],
        cflags=[])
