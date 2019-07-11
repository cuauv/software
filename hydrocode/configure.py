#!/usr/bin/env python3

from build import ninja_common
build = ninja_common.Build('hydrocode')
build.build_cmd('auv-hydromathd', ['hydromathd.cpp', 'udp_receiver.cpp', 'udp_sender.cpp', 'pinger_tracking.cpp', 'common_dsp.cpp'],
                auv_deps=['shm'], deps=['liquid'])

build.install('auv-hydro-heading', 'hydrocode/scripts/heading_plot.py')
build.install('auv-hydro-raw-plot', 'hydrocode/scripts/raw_plot.py')
build.install('auv-hydro-trigger-plot', 'hydrocode/scripts/trigger_plot.py')
build.install('auv-hydro-dft-plot', 'hydrocode/scripts/dft_plot.py')
build.install('auv-hydro-comms-filtered-plot', 'hydrocode/scripts/comms_filtered_plot.py')
build.install('auv-hydro-correlation-plot', 'hydrocode/scripts/correlation_plot.py')
