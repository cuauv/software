#!/usr/bin/env python3
import argparse
import functools
import time
import os
import sys
import time
import types
import shm
import signal as _signal
from datetime import datetime
import subprocess

from auvlog.client import log
from misc.utils import register_exit_signals
from mission.framework.primitive import Zero
from mission.framework.task import Task
from auv_python_helpers.misc import reload_self

parser = argparse.ArgumentParser()

parser.add_argument("-f", "--frequency", help="Target tick frequency. Default value of 60.", type=int, default=60)
parser.add_argument("-v", "--verbosity", choices=["verbose", "info", "warn", "error"], default="info",
                    help="Lowest log level to copy to stdout by default. Default value is \"warn\"")
parser.add_argument("-d", "--directory", default="missions",
                    help="Directory where task is found. Default value of \"missions\"")
parser.add_argument("task", help="Root task to run.", nargs='?')
parser.add_argument("args", nargs="*", help="Arguments passed to the task.")
parser.add_argument("--ignore-exceptions", help="Ignore any exceptions. USE WITH CAUTION.", action='store_true')
parser.add_argument("--no-record", help="Don't record any logs", action="store_true")

args = parser.parse_args()

# Run full mission by default
if len(sys.argv) <= 1:
      args.task = "full.Full"

full_name = "{}.{}".format(args.directory, args.task)
module_name, task_name = full_name.rsplit(".", 1)
module = __import__(module_name, fromlist=task_name)
task = getattr(module, task_name)
period = 1 / args.frequency

Task.minimum_printed_log_level = args.verbosity.lower()

logger = log.mission.main

# Save running vision module state
vision_state = shm.vision_modules.get()
logger('Saved running vision module state. Will restore on Ctrl-C or mission completion.', copy_to_stdout = True)

# Save control settings state
control_settings = shm.settings_control.get()
navigation_settings = shm.navigation_settings.get()

#ommit directory from log directory name
module_dir_name, module_no_dir_name = module_name.rsplit(".",1)

# Get new log directory name
cuauv_log = os.environ['CUAUV_LOG']
dirname_base = "{}_{}".format(module_no_dir_name,task_name)
if not args.no_record:
    try:
        ls_output = subprocess.check_output("ls -d %s/current/%s*/" %
                                            (cuauv_log, dirname_base), shell=True)
        dirs = ls_output.decode('utf-8').split('\n')[:-1]
        dirnames = [d.split('/')[-2] for d in dirs]
        get_run_num = lambda x: int(x[len(dirname_base):])
        highest = max(dirnames, key=get_run_num)
        highest_run_num = get_run_num(highest)
    except subprocess.CalledProcessError as ls_except:
        if ls_except.returncode == 2:
            highest_run_num = 0
        else:
            raise ls_except

    this_run_num = highest_run_num + 1
    log_path = "%s/current/%s%02d" % (
        cuauv_log, dirname_base, this_run_num)

    # Make log directory
    subprocess.call("mkdir -p %s" % log_path, shell=True)

    # Setup vision recording
    shm.vision_modules.Record.set(True)
    shm.vision_modules.Debug.set(True)
    logger('Recording video logs', copy_to_stdout = True)

    # Record shmlog
    cmd = "auv-shmlogd --filename=%s/shmlog.shmlog" % log_path
    shmlog_proc = subprocess.Popen("exec " + cmd, stdout=subprocess.PIPE, shell=True)

    # Set active mission shm group to reflect current mission
    try:
        os.makedirs(log_path)
    except:
        pass
    active_mission = shm.active_mission.get()
    active_mission.active = True
    active_mission.name = bytes(task_name, encoding="utf-8")
    active_mission.log_path = bytes(log_path, encoding="utf-8")
    shm.active_mission.set(active_mission)

# Ensure only one mission can run at a time.
LOCK_NAME = ".mission_lock"
lock_dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), LOCK_NAME)
try:
    os.mkdir(lock_dir)
except OSError:
    logger("A MISSION IS ALREADY RUNNING! Aborting...", copy_to_stdout=True)
    logger("If I am mistaken, delete %s or check permissions" % lock_dir,
        copy_to_stdout=True)
    sys.exit(1)

# Store time
start_time = time.time()

# If the name given is a class or function, instantiate it and pass arguments.
if isinstance(task, (type, types.FunctionType, functools.partial)):
    converted_args = []
    for arg in args.args:
        try:
            f_arg = float(arg)
        except ValueError:
            f_arg = arg

        converted_args.append(f_arg)

    task = task(*converted_args)

elif len(args.args) > 0:
    log("Arguments only supported for uninstantiated Tasks.",
        copy_to_stdout=True)
    sys.exit(1)

def release_lock():
    os.rmdir(lock_dir)

initially_killed = shm.switches.hard_kill.get()
was_ever_unkilled = False

def cleanup():
    Zero()()
    release_lock()
    shm.vision_modules.set(vision_state)
    shm.settings_control.set(control_settings)
    shm.navigation_settings.set(navigation_settings)
    print('')
    logger('Mission completed or interrupted. Restored running vision module state and zeroed submarine desires.', copy_to_stdout = True)

    end_time = time.time()
    duration = end_time - start_time
    logger("Mission finished in %i seconds!" % \
               (duration), copy_to_stdout = True)

    if initially_killed and was_ever_unkilled and shm.switches.hard_kill.get():
      reload_self()

    if not args.no_record:
          logger('Disabling "Record" vision module', copy_to_stdout = True)
          shm.vision_modules.Debug.set(False)
          shm.vision_modules.Record.set(False)

          # Stop shmlogging
          shmlog_proc.kill()

          active_mission = shm.active_mission.get()
          active_mission.active = False
          active_mission.name = bytes("", encoding="utf-8")
          active_mission.log_path = bytes("", encoding="utf-8")
          shm.active_mission.set(active_mission)

has_caught_sigint = False
def exit_handler(signal, frame):
    global has_caught_sigint
    if not has_caught_sigint and signal == _signal.SIGINT:
        has_caught_sigint = True
        logger('Caught Ctrl-C. Mission paused. Ctrl-C again to quit, enter to resume.', copy_to_stdout = True)
        while True:
            ch = sys.stdin.readline()
            has_caught_sigint = False
            logger('Resuming mission!', copy_to_stdout = True)
            return
    else:
        cleanup()
        sys.exit(0)

register_exit_signals(exit_handler)

too_long_initial = True

while True:
    begin_time = time.time()
    was_ever_unkilled = was_ever_unkilled or (not shm.switches.hard_kill.get())

    try:
        task()
    except Exception as e:
        if not args.ignore_exceptions:
            cleanup()
            raise
        else:
            import traceback
            traceback.print_exc()
            logger("EXCEPTION ENCOUNTERED! Continuing anyway...")

    if task.has_ever_finished:
        break

    end_time = time.time()
    duration = end_time - begin_time

    if period > duration:
        time.sleep(period - duration)
    else:
        if not too_long_initial:
            logger("MISSION TOOK TOO LONG TO RUN (%f seconds); Can't maintain %d HZ" % \
               (duration, args.frequency), copy_to_stdout=False)
        else:
            too_long_initial = False

cleanup()
