#!/usr/bin/env python3

import argparse
import imp
import os
import re
import signal
import atexit
import sys
import time
import threading
import traceback
import yaml
import subprocess

import shm
from auvlog.client import log as auvlog

import vision.vision_common as vision_common
from vision import camera_message_framework

dirname = os.path.dirname(os.path.realpath(__file__))
config_file_dir = os.path.join(dirname, 'configs')
pid_file_dir = os.path.join(dirname, 'pids')

logger = auvlog.vision

if not os.path.isdir(pid_file_dir):
  logger.log('PID file directory ({}) does not exist; creating'.format(pid_file_dir), copy_to_stdout = True)
  os.mkdir(pid_file_dir)

# run a capture source forever. this is intended to be ran as a separate process
def run_capture_source(capture_source_direction, capture_source_parameters):
    source_type = capture_source_parameters['type']
    del capture_source_parameters['type']
    capture_source_class = getattr(__import__(source_type), source_type)

    status_file = os.path.join(dirname, 'status', capture_source_direction)
    with open(status_file, 'w') as f:
        f.write(source_type)

    try:
        source = capture_source_class(capture_source_direction.lower(),
                                      **capture_source_parameters)

        def signal_handler(*args, **kwargs):
            os.remove(status_file)
            vision_common.cleanup_pid()

        atexit.register(signal_handler)
        source.acquisition_loop()
    except Exception as e:
        print(e)
        traceback.print_exc()
        sys.stdout.flush()
        sys.stderr.flush()

def run_module(module_name):
    print("Running: " + str(module_name))
    m_logger = getattr(logger.module, module_name)
    sys.path.append('{}/modules'.format(dirname))

    # find the module file and the module class within it
    module_file = imp.reload(__import__(module_name))
    module_class = getattr(module_file, module_name)

    m_logger.log('running module')
    module = module_class(getattr(logger.module, module_name))

    atexit.register(vision_common.cleanup_pid)

    # get the capture source direction as declared in the module
    if hasattr(module_file, 'capture_sources'):
        directions = module_file.capture_sources
        requested_single_capture_source = False
    elif hasattr(module_file, 'capture_source'):
        directions = [module_file.capture_source]
        requested_single_capture_source = True
    else:
        print('Neither of capture_source or capture_sources is defined in module file!')
        sys.exit(0)
    m_logger.log('connecting {} to {}'.format(module_name, directions))


    # create the accessor for the capture source framework
    capture_source_frameworks = [camera_message_framework.Accessor(d) for d in directions]
    buffer_size = [f.buffer_size for f in capture_source_frameworks]

    # create the module framework
    module_framework = camera_message_framework.ModuleFrameworkCreator(module_name)

    #initialize default module values
    for option_name in module.options_dict:
        option = module.options_dict[option_name]
        module_framework.create_option(option.name, option.format_str)
        module_framework.write_option(option.name, option.get_pack_values())

    option_lock = threading.Lock()
    # watcher to reflect option updates in the module framework
    def update_option_watcher(option_name, option_value):
        _, option_value = option_value
        option_value = option_value[0]
        with option_lock:
            try:
                module.options_dict[option_name].update(option_value)
            except ValueError:
                # will notify self, but shouldn't loop
                module_framework.write_option(option_name, option.get_pack_values())

    module_framework.register_option_observer(update_option_watcher, False)

    posted_images = set()
    module_shm = getattr(shm.vision_modules, module_name)

    while module_shm.get():
        module.posted_images = []

        next_images, acq_times = zip(*map(lambda f : f.get_next_frame(),
                                          capture_source_frameworks))

        with option_lock:
            try:
                original_options = {option_name: module.options_dict[option_name].value for option_name in module.options_dict}
                curr_time = time.time()*1000
                avg_latency = sum(map(lambda t: curr_time - t, acq_times)) / len(acq_times)
                m_logger.log('{} image latency to {}: {}ms'.format(directions, module_name, avg_latency))

                if requested_single_capture_source:
                    module.acq_time = acq_times[0]
                    module.process(next_images[0])
                else:
                    module.acq_time = acq_times
                    module.process(next_images)

            except Exception as e:
                sys.stderr.write('{}\n'.format(e))
                traceback.print_exc(file=sys.stderr)
            # if the option value has changed, notify the watchers
            for option_name in original_options:
                option = module.options_dict[option_name]
                original_value = original_options[option_name]
                if original_value != option.value:
                    module_framework.write_option(option.name,
                                                  option.get_pack_values())

        for (tag, image) in module.posted_images:
            if tag not in posted_images:
                module_framework.create_image(tag, max(buffer_size))
                img_ordering = {tag: i for (i, (tag, _)) in enumerate(module.posted_images)}
                module_framework.set_image_ordering(lambda x: img_ordering[x])
                posted_images.add(tag)
            module_framework.write_image(tag, image, min(acq_times))

# start the vision daemon from a specified config file
def start_daemon(config_file_name):
    with open(config_file_name) as config_file:
        config = yaml.load(config_file)

    logger.log('parsed options, killing old vision processes')
    stop_daemon(should_wait=True)
    logger.log('killed old vision processes, starting capture sources')

    # fork all capture source daemons
    for capture_source_name in config['capture_sources']:
        logger.log('starting capture source {}'.format(capture_source_name))

        vision_common.fork(run_capture_source,
                           args=(capture_source_name,
                                 config['capture_sources'][capture_source_name]))

    logger.log('started all capture sources')

    modules_to_start = []

    if 'modules' in config:
        for (_, module) in vision_common.all_vision_modules():
            module.set(False)

        for config_module in config['modules']:
            module_name, module = vision_common.module_by_name(config_module)
            modules_to_start.append(module_name)

    logger.log('started all initial modules')
    logger.log('Starting vision processing')
    return modules_to_start

# fork all of the specified modules
def run_modules(modules, kill_old_modules=True):
    for i, module in enumerate(modules):
        try:
            module_name, module_shm = vision_common.module_by_name(module)
        except KeyError:
            logger.log('{} is not a valid module name!'.format(module), True)
            continue

        if kill_old_modules and module_shm.get():
            module_shm.set(False)
            time.sleep(0.5)

        module_shm.set(True)
        vision_common.fork(run_module, args=(module_name,))


def stop_modules(modules):
    for module in modules:
        module_name, module_shm = vision_common.module_by_name(module)
        module_shm.set(False)

def daemon_running():
    if not os.path.isdir(pid_file_dir):
        return False

    running = False
    for pidfile in os.listdir(pid_file_dir):
        pid = pidfile[:-4]
        try:
            os.kill(int(pid), 0)
            running = True
        except:
            pidfile_path = os.path.join(pid_file_dir, pidfile)
            print("Removing .pid for {}".format(pidfile_path))
            os.remove(pidfile_path)

    return running

def status():
    if daemon_running:
        print('Vision daemon is currently running on:')
        #print capture sources
        print('Capture Sources:')
        for filename in os.listdir(os.path.join(dirname, 'status')):
            if filename == '.gitignore':
                continue
            with open(os.path.join(dirname, 'status', filename)) as f:
                print('{}: {}'.format(filename, f.read()))
        #print modules
        print()
        print('Modules:')
        for module_name in (x[0] for x in shm.vision_modules._fields):
            if getattr(shm.vision_modules, module_name).get():
                print(module_name)
    else:
        print('Vision daemon is not running')

def cleanup(force):
    stop_daemon(False)
    if force:
        signal = '-SIGKILL'
    else:
        signal = '-SIGTERM'

    for filename in os.listdir('/dev/shm'):
        if not filename.startswith('auv_visiond'):
            continue
        full = os.path.join('/dev/shm', filename)
        subprocess.call(['fuser', '-k', signal, full],
                        stdout=subprocess.DEVNULL,
                        stderr=subprocess.DEVNULL)
        os.remove(full)

def stop_daemon(should_wait=True):
    stop_modules(x[0] for x in shm.vision_modules._fields)
    # sleep to ensure that modules are actually stopped and cleaned up properly
    time.sleep(0.1)

    # kill all associated pids
    for filename in (filename for filename in os.listdir(pid_file_dir) if '.pid' in filename):
        groups = re.findall('(\d+).pid', filename)
        if groups:
            pid = int(groups[0])
            try:
                os.kill(pid, signal.SIGTERM)
            except OSError:
                pass

    start_time = time.time()
    # really really kill all of the remaining vision modules
    while should_wait and daemon_running():
        time.sleep(0.1)
        all_pids = [int(pid) for pid in os.listdir('/proc') if pid.isdigit()]
        for filename in (filename for filename in os.listdir(pid_file_dir) if '.pid' in filename):
            groups = re.findall('(\d+).pid', filename)
            if groups:
                pid = int(groups[0])
                if pid not in all_pids:
                    os.remove(os.path.join(pid_file_dir, filename))
                elif time.time() - start_time > 0.5:
                    try:
                        os.kill(pid, signal.SIGKILL)
                    except OSError:
                        pass

if __name__ == '__main__':
    logger.log('parsing options')
    parser = argparse.ArgumentParser(description='Start the vision daemon.')
    sp = parser.add_subparsers()

    sp_start = sp.add_parser('start', help='Starts the vision daemon, or an individual module')
    sp_start.set_defaults(which='start')
    sp_start.add_argument('-d', '--daemon', action='store_true', help='Run as a daemon')

    start_group = sp_start.add_argument_group()
    start_group.add_argument('config', type=str, metavar='module_or_config',
                             help='A config file to load, or module if the daemon is already running')
    start_group.add_argument('modules', metavar='module', type=str, help='Additional modules to start', nargs='*')

    sp_stop = sp.add_parser('stop', help='Stops the vision daemon, or an individual module')
    sp_stop.add_argument('modules', metavar='module', type=str,
                         help='A list of modules to stop. If empty, stops the daemon', nargs='*')
    sp_stop.set_defaults(which='stop')

    sp_status = sp.add_parser('status', help='Get vision daemon status')
    sp_status.set_defaults(which='status')

    sp_cleanup = sp.add_parser('cleanup', help='Clean up shared memory files')
    sp_cleanup.set_defaults(which='cleanup')
    sp_cleanup.add_argument('-f', '--force', action='store_true', help='Send SIGKILL signal to persistant processes')

    args = parser.parse_args()
    running = daemon_running()
    starting_daemon = False
    sys.path.append('{}/capture_sources'.format(dirname))

    if not hasattr(args, 'which'):
        parser.print_help()
        sys.exit(1)

    if args.which == 'start':
        modules = args.modules

        config_file_loc = None

        if os.path.isfile(args.config):
            config_file_loc = args.config
        elif os.path.isfile(os.path.join(config_file_dir, args.config)):
            config_file_loc = os.path.join(config_file_dir, args.config)

        if config_file_loc and running:
            logger.log('Vision daemon is already running! To start more modules, specify the module names, not a new configuration file', True)
            sys.exit(1)
        elif config_file_loc and not running:
            modules.extend(start_daemon(config_file_loc))
            running = starting_daemon = True
        elif not config_file_loc and not running:
            logger.log('{} is not a config file. Please specify a valid config file, relative to {}'.format(args.config, config_file_dir), True)
            sys.exit(1)
        elif running and not config_file_loc:
            modules.append(args.config)

        if running:
            if len(modules) > 0:
                logger.log('Starting modules: {}'.format(', '.join(modules)), True)
                run_modules(modules)

                if not args.daemon:
                    def sigh(_sig, _frame):
                        if starting_daemon:
                            logger.log('Killing daemon!', True)
                            stop_daemon()
                        else:
                            logger.log('Killing modules: {}'.format(modules), True)
                            stop_modules(modules)

                        sys.exit(0)

                    signal.signal(signal.SIGINT, sigh)
                    signal.signal(signal.SIGTERM, sigh)
                    signal.pause()

                else:
                    sys.exit(1)
        else:
            logger.log('Vision daemon is not running! Start the vision daemon before you start any modules.', True)
    elif args.which == 'stop':
        if args.modules:
            if running:
                logger.log('Stopping modules {}'.format(', '.join(args.modules)), True)
                stop_modules(args.modules)
            else:
                logger.error('Vision daemon is not running! Start the vision daemon before you stop any modules.', True)
        else:
            logger.log('Stopping vision daemon!', True)
            stop_daemon()
    elif args.which == 'status':
        status()
    elif args.which == 'cleanup':
        cleanup(args.force)
