#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('misc')

build.install('auv-led', f='misc/led.py')

build.install('cams')

build.build_shared('utils', ['utils.cpp'])

build.build_cmd('auv-human-depth', ['human-depth.cpp'],
        auv_deps=['shm'])

build.install("auv-build-chicken", "misc/auv-build-chicken.sh")
build.install('auv-start-modules', 'misc/start_modules.sh')
build.install("auv-sim-defaults", "misc/sim_defaults.py")
build.install("auv-simulate", "misc/start_sim.sh")
build.install("auv-terminal", "misc/auv_terminal.sh")
build.install("auv-pingalert", "misc/pingalert/pingalert.py")
build.install("auv-fire-actuator", "misc/actuator.py")
build.install("auv-hold-actuator", "misc/hold_actuator.py")
build.install("auv-autotest", "misc/autotest.sh")
build.install("auv-pycheck", "misc/pylint_check.sh")
build.install('auv-mark', f = 'misc/marker/marker.py')
build.install('auv-mark-map', f = 'misc/marker/map.py')
build.install('auv-change-pool', 'misc/change-pool.sh')
build.install('auv-check-navigated', f='misc/check_navigated.py')
build.install('auv-check-pings', f='misc/check_pings.py')
