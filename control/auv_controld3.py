#!/usr/bin/env python3
import argparse
import sys
import time

from datetime import datetime

import numpy as np

from auvlog.client import log
from control import vehicle
from control.optimizer import Optimizer
from control.pid import PIDLoop
from control.thruster_manager import ThrusterManager
from control.util import zero_motors, set_shm_wrench

from misc.utils import watch_thread_wrapper

from conf.vehicle import control_settings
import shm

# ****************************************************
#
# auv_controld3 --
#   Controller employing multivariable optimization
#   over traditional PID loops
#
#   Jeff Heidel - May 2012
#   Alex Spitzer - 2013
#
# ****************************************************

def timed(f, *args, **kwargs):
    start_time = time.time()
    return f(*args, **kwargs), time.time() - start_time

def load_settings():
    for dof, dof_settings in control_settings.items():
        group = getattr(shm, "settings_" + dof)
        for var, value in dof_settings.items():
            getattr(group, var).set(value)

def main(args):
    if args['rate']:
        step_time = 1.0 / args['rate']
        log("Starting controller at %f HZ, one step every %f seconds" % (
              args['rate'], step_time), copy_to_stdout=True)

    else:
        log("Starting controller with kalman lock", copy_to_stdout=True)
    load_settings()

    def loop(kalman_watcher, quit_event):
        # PID stepping class
        pid = PIDLoop(speed=args['speed'])
        pid.clean()

        # Optimizer class
        opt = Optimizer()
        opt.DEBUG = bool(args['verbose'])

        controller_enabled = True
        tm = ThrusterManager()

        kalman_watcher.watch(shm.kalman)

        # The main loop
        while not quit_event.is_set():
            if not args['rate']:
                kalman_watcher.wait()

            start_time = time.time()

            g = shm.kalman.get()

            dt_qs = timed(tm.update, g)[1]

            # Get passive forces and passive torques on the sub in model frame
            passives, dt_pass = timed(vehicle.passive_forces, g, tm)
            set_shm_wrench(shm.control_passive_forces, passives)

            # Handle controller enable / disable feature
            if not shm.settings_control.enabled.get() or \
                   shm.switches.soft_kill.get() or shm.switches.hard_kill.get():
                # controller is disabled
                if controller_enabled:
                    set_shm_wrench(shm.control_internal_wrench, [0] * 6)
                    zero_motors()
                    controller_enabled = False

                if args['rate']:
                    time.sleep(0.1)

                continue

            if not controller_enabled: # We have been re-enabled
                controller_enabled = True
                pid.clean()

            # Execute one PID step for all PID controllers.
            dt_pid = timed(pid.step, tm)[1]

            # Set motors from PID desires.
            dt_opt = timed(opt.set_motors, tm, passives)[1]

            total_time = time.time() - start_time

            if args['verbose']:
                times = [("Qs", dt_qs), ("Passives", dt_pass),
                         ("PID", dt_pid), ("Optimizer", dt_opt), ("Total", total_time)]
                for name, dt in times:
                    log("%s step ran in %0.2f HZ (%f ms)" % (name, (1/dt), 1000*dt), copy_to_stdout=True)

            if args['rate']:
                if total_time > step_time:
                    log(str(datetime.now()) + \
                 "## WARN: UNABLE TO MEET RATE. RUNNING AT %f HZ" % (1 / total_time), copy_to_stdout=True)
                else:
                    if args['verbose']:
                        log("Rate capped at %f HZ" % args['rate'], copy_to_stdout=True)
                        log("-----", copy_to_stdout=True) #visual break
                    time.sleep(step_time - total_time)

        pid.clean()
        zero_motors()

    watch_thread_wrapper(loop)

if __name__ == "__main__":
    # argparse options
    ap = argparse.ArgumentParser(description='auv_controld3 - optimizing AUV controller')
    ap.add_argument('-r', dest='rate', type=float, help='controller step frequency (in HZ)', required=False)
    ap.add_argument('-v', dest='verbose', action='store_true', default=False, help='enable verbose debug output', required=False)
    ap.add_argument('-s', dest='speed', type=float, default=1.0, help='controller real-time speed factor', required=False)
    main(vars(ap.parse_args()))
