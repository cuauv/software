#!/usr/bin/env python3

import os
import time
import tomlkit

import shm
from conf.vehicle import VEHICLE

# The directory where the serial toml config files are located
SERIAL_CONF_DIR = '/home/software/cuauv/software/serial/seriald/conf'
# The name of the motor desires SHM group
MOTOR_DESIRES = 'motor_desires'
# The name of the ramp status SHM group
RAMP_STATUS = 'ramp_status'
# Suffix at the end of ramp status SHM variables
RAMP_SUFFIX = '_ramp'
# How fast to spin the thrusters (0 to 255)
TEST_MOTOR_SPEED = 30
# How long to spin the thrusters (in seconds)
TEST_DURATION = 2

# Used for colored output
class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def colored(msg, color):
    """
    Returns a string wrapped in provided color codes. See bcolors for available
    colors.
    """
    return '{}{}{}'.format(color, msg, bcolors.ENDC)

class Failure(Exception):
    """
    A failure mode. Will display the message in red and quit.
    """
    def __init__(self, msg, extra_lines=[], *args, **kwargs):
        super().__init__(msg, *args, **kwargs)
        self.extra_lines = extra_lines

    def extra(self, line):
        """
        Add an extra line to the error output.
        """
        self.extra_lines.append(line)

class TomlParseFailure(Failure):
    """
    A failure in parsing a TOML file.
    """
    pass

def load_serial_conf(fname):
    """
    Load a TOML file from the serial daemon's conf directory. The directory is
    specified by SERIAL_CONF_DIR; :fname: is just the name of the file, e.g.
    'odysseus.toml'.
    """
    full_path = os.path.join(SERIAL_CONF_DIR, fname)
    if not os.path.isfile(full_path):
        raise Failure("File '{}' does not exist on path '{}'"
                      .format(fname, SERIAL_CONF_DIR))
    try:
        with open(full_path) as f:
            return tomlkit.parse(f.read())
    except tomlkit.exceptions.ParseError as e:
        raise TomlParseFailure(str(e))

def find_values(toml, predicate, type_filter=None, lst=[], path=''):
    """
    Perform nested search in :toml: for all values of type :type_filter: (if
    provided) and satisfying :predicate:, appending entries to :lst:. :path:
    used for recursively constructing paths. The returned value is a list of
    (path, value) tuples where the path is a dot-separated list of dictionary
    names.
    """
    def handle(value, handle_path):
        if type_filter is None or isinstance(value, type_filter) \
           and predicate(value):
            # we add an extra dot at the beginning, so remove it
            lst.append((handle_path[1:], value))

    for key, value in toml.items():
        new_path = '{}.{}'.format(path, key)
        handle(value, new_path)
        if isinstance(value, dict):
            # traverse dictionaries
            find_values(value, predicate, type_filter=type_filter, lst=lst,
                        path=new_path)
        if isinstance(value, list):
            # traverse lists
            for val in value:
                handle(val, new_path)

def deep_set(toml, path, value):
    """
    Set a value inside a nested dictionary from a dotted key path, e.g.
    'outer.inner.var'. Sets in the key at :path: in dict :toml: to :value:.
    """
    segments = path.split('.')
    # the last one is the final key
    for segment in segments[:-1]:
        assert segment in toml
        toml = toml[segment]
    toml[segments[-1]] = value

def main():
    # load main serial conf for the current vehicle
    vehicle_conf = load_serial_conf('{}.toml'.format(VEHICLE))

    if not 'includes' in vehicle_conf or \
       not isinstance(vehicle_conf['includes'], list):
        raise Failure("Serial configuration for {} is missing 'includes' field"
                      .format(VEHICLE))

    shm_map = {}
    thruster_files = {}

    # traverse and validate conf files
    for include in vehicle_conf['includes']:
        try:
            board_conf = load_serial_conf(include)
        except TomlParseFailure as e:
            # we will ignore parse errors and just work with the files that
            # parse; e.g. currently ports.toml does not have commas between
            # items in the list, but we don't need ports.toml to re-map
            # thrusters
            print(colored("Error parsing file '{}', ignoring: {}"
                          .format(include, str(e)), bcolors.WARNING))
            continue

        motor_desires = []
        ramp_status = []
        find_values(board_conf, lambda s: s.startswith(MOTOR_DESIRES),
                    type_filter=str, lst=motor_desires)
        find_values(board_conf, lambda s: s.startswith(RAMP_STATUS),
                    type_filter=str, lst=ramp_status)

        if len(motor_desires) != len(ramp_status):
            raise Failure("Invalid thruster conf: '{}', incompatible {} and {}"
                          .format(include, MOTOR_DESIRES, RAMP_STATUS))

        ramp_status_map = {}
        for path, shm_var in ramp_status:
            # chop the ramp suffix at the end, i.e. the '_ramp'
            motor_desire_shm_var = '{}{}'.format(MOTOR_DESIRES,
                                                 shm_var[len(RAMP_STATUS):
                                                         -len(RAMP_SUFFIX)])
            ramp_status_map[motor_desire_shm_var] = path

        for path, shm_var in motor_desires:
            if not shm_var in ramp_status_map:
                raise Failure("Invalid thruster conf: '{}', missing {} for {}"
                              .format(include, RAMP_STATUS,
                                      shm_var[len(MOTOR_DESIRES) + 1:]))
            shm_map[shm_var] = (include, path, ramp_status_map[shm_var])
            if not include in thruster_files:
                thruster_files[include] = board_conf

    shm_motor_desires = getattr(shm, MOTOR_DESIRES)

    shm_names = set([name for name, _ in shm_motor_desires._fields])
    # chop 'motor_desires.' off of the front
    conf_names = set(key[len(MOTOR_DESIRES) + 1:] for key in shm_map.keys())

    if shm_names != conf_names:
        e = Failure('Thruster mismatch between shm.{} and serial conf!'
                    .format(MOTOR_DESIRES))
        e.extra('Found thrusters in these files: {}'
                .format(list(thruster_files.keys())))
        e.extra('SHM names:         {}'.format(sorted(list(shm_names))))
        e.extra('Serial conf names: {}'.format(sorted(list(conf_names))))
        raise e

    if len(shm_names) == 0:
        e = Failure('No thrusters found!')
        e.extra('Searched files: {}'.format(vehicle_conf['includes'].values()))
        raise e

    # TODO check to make sure serial daemon is running

    # check soft kill
    if shm.switches.soft_kill.get():
        msg = 'Soft kill enabled. Override? [yN] '
        if input(colored(msg, bcolors.WARNING)).strip().lower() in ['y', 'yes']:
            shm.switches.soft_kill.set(False)
        else:
            raise Failure('Cannot proceed while soft killed, aborting')

    print()
    print('=== Instructions ===')
    print()
    print('The thruster mapper will spin one thruster at a time.')
    print('Enter the name of the thruster (or a uniquely-defining prefix) to '
          'assign that thruster.')
    print('Enter nothing to have the thruster spin again.')
    print('Ctrl+C to quit.')
    print()
    print('Available thrusters:')
    for shm_name in shm_names:
        print('  {}'.format(shm_name))
    print()
    print(colored('Press ENTER to start, Ctrl+C to abort.', bcolors.OKBLUE))

    # wait for user to press ENTER
    input()

    re_mapping = {} # map from old values to new values
    already_mapped = set() # new values that we have already mapped

    # spin thrusters and ask user for re-mappings
    for shm_name in sorted(shm_names):
        full_shm_name = '{}.{}'.format(MOTOR_DESIRES, shm_name)
        fname = shm_map[full_shm_name][0]
        # 'ECE var' is the name of the variable on the board
        ece_var_name = shm_map[full_shm_name][1].split('.')[-1]

        passed = False # True if we can advance to the next thruster
        spin_again = True
        while not passed:
            if spin_again:
                # spin thruster
                print("{} '{}' > '{}' (currently mapped to {})"
                      .format(colored('Spinning thruster:', bcolors.OKGREEN),
                              fname, ece_var_name, shm_name))
                motor_desire = getattr(shm_motor_desires, shm_name)
                motor_desire.set(TEST_MOTOR_SPEED)
                time.sleep(TEST_DURATION)
                motor_desire.set(0)
                spin_again = False

            user_input = input('Thruster name: ').strip().lower()
            if user_input == '':
                spin_again = True
                continue
            else:
                matches = set(filter(lambda name: name.startswith(user_input),
                                     shm_names))
                if len(matches) == 1:
                    match = tuple(matches)[0]
                    if match in already_mapped:
                        print(colored("Already mapped thruster '{}'"
                                      .format(match), bcolors.WARNING))
                    else:
                        re_mapping[shm_name] = match
                        already_mapped.add(match)
                        passed = True
                elif len(matches) == 0:
                    print(colored("No thruster found for '{}'"
                                  .format(user_input), bcolors.WARNING))
                else:
                    print(colored("Multiple matches found for '{}': {}"
                                  .format(user_input, matches),
                                  bcolors.WARNING))

    # change mappings in TOML data structures
    for prev_name, (fname, motor_desires_path, ramp_status_path) \
        in shm_map.items():
        chopped_prev_name = prev_name[len(MOTOR_DESIRES) + 1:]
        assert chopped_prev_name in re_mapping
        new_motor_desires_name = '{}.{}'.format(MOTOR_DESIRES,
                                                re_mapping[chopped_prev_name])
        new_ramp_status_name = '{}.{}{}'.format(RAMP_STATUS,
                                                re_mapping[chopped_prev_name],
                                                RAMP_SUFFIX)

        # update motor desires
        deep_set(thruster_files[fname], motor_desires_path,
                 new_motor_desires_name)
        # update ramp status
        deep_set(thruster_files[fname], ramp_status_path, new_ramp_status_name)

    # give user option: either overwrite existing files or print out

    print()
    print('Thruster config files: {}'.format(list(thruster_files.keys())))
    overwrite = input(
        colored('Overwrite previous thruster configuration? [yN] ',
                bcolors.WARNING)).strip().lower() in ['y', 'yes']

    for fname, toml in thruster_files.items():
        output = tomlkit.dumps(toml)
        if overwrite:
            with open(os.path.join(SERIAL_CONF_DIR, fname), 'w') as f:
                f.write(output)
        else:
            print()
            print(colored("Proposed contents of '{}' "
                          "[the file has not been changed]:".format(fname),
                          bcolors.OKBLUE))
            print(output)

    if overwrite:
        print()
        print(colored(
            'Saved. Restart serial daemon for changes to take effect.',
            bcolors.OKBLUE))

def cleanup(was_soft_killed):
    shm_motor_desires = getattr(shm, MOTOR_DESIRES)
    if was_soft_killed:
        shm.switches.soft_kill.set(True)
    # zero all motor desires in case we quit halfway through a thruster spin
    for motor_desire, _ in shm_motor_desires._fields:
        getattr(shm_motor_desires, motor_desire).set(0)

if __name__ == '__main__':
    was_soft_killed = shm.switches.soft_kill.get()

    try:
        main()
    except KeyboardInterrupt:
        pass
    except EOFError:
        pass
    except Failure as e:
        print(colored(str(e), bcolors.FAIL))
        for line in e.extra_lines:
            print(line)
    finally:
        # always clean up
        cleanup(was_soft_killed)
