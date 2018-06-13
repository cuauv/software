#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('misc')

build.install('auv-led', f='misc/led.py')

build.install('cams')

build.build_shared('utils', ['utils.cpp'])

build.install("auv-autotest", "misc/autotest.sh")
build.install("auv-build-chicken", "misc/auv-build-chicken.sh")
build.install("auv-fire-actuator", "misc/actuator.py")
build.install("auv-hold-actuator", "misc/hold_actuator.py")
build.install("auv-pingalert", "misc/pingalert/pingalert.py")
build.install("auv-pycheck", "misc/pylint_check.sh")
build.install("auv-sim-defaults", "misc/sim_defaults.py")
build.install("auv-simulate", "misc/start_sim.sh")
build.install("auv-terminal", "misc/auv_terminal.sh")
build.install('auv-change-pool', 'misc/change-pool.sh')
build.install('auv-check-pings', f='misc/check_pings.py')
build.install('auv-hydro-sim', f='misc/hydro_sim.py')
build.install('auv-mark', f = 'misc/marker/marker.py')
build.install('auv-mark-map', f = 'misc/marker/map.py')
build.install('auv-shm-slider', f='misc/shm_slider.py')
build.install('auv-start-modules', 'misc/start_modules.sh')
build.install('auv-tplot', f='misc/tplot.py')
