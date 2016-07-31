#!/usr/bin/python

from build import ninja_common
build = ninja_common.Build('self_test')

build.install('auv-actuator-test', f='self_test/actuator_test.py')
build.install('auv-thruster-test', f='self_test/thruster_test.py')
build.install('auv-thruster-test-random', f='self_test/thruster-test-random.py')
build.install('auv-stress-thruster', f='self_test/thruster_stress.py')
