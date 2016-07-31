#!/usr/bin/env python3

import time

import shm

from conf.vehicle import actuators

actuator_group = shm.actuator_desires
FIRE_TIME = 0.1

def set_actuator_num(group, num, value):
    setattr(group, 'trigger_{:02}'.format(num), value)

def set_triggers_by_number(on, off):
    group = actuator_group.get()

    for num in on:
        set_actuator_num(group, num, 1)
    for num in off:
        set_actuator_num(group, num, 0)

    actuator_group.set(group)

def set_triggers_by_name(on, off):
    on_nums = [actuators[t] for t in on]
    off_nums = [actuators[t] for t in off]
    set_triggers_by_number(on_nums, off_nums)

def set_names_on(names):
    set_triggers_by_name(on=names, off=[])

def set_names_off(names):
    set_triggers_by_name(on=[], off=names)

def fire_actuators(names):
    set_names_on(names)
    time.sleep(FIRE_TIME)
    set_names_off(names)

def get_num_from_maybe_name(maybe_name):
    try:
      num = int(maybe_name)
    except ValueError:
      if maybe_name in actuators:
        num = actuators[maybe_name]
      else:
        return None

    return num

def get_cmdline_actuators():
  """ This function also ensures triggers are shut off on exit. """
  import sys
  from misc.utils import register_exit_signals

  if len(sys.argv) < 2:
    print("Please provide actuator numbers on the command line.")
    sys.exit(1)

  nums = []
  for arg in sys.argv[1:]:
    num = get_num_from_maybe_name(arg)
    if num is None:
      print("Unrecognized actuator \"%s\"." % arg)
      sys.exit(1)

    nums.append(num)

  def sigh(sig, frame):
    set_triggers_by_number(on=[], off=nums)

  register_exit_signals(sigh)

  return nums

if __name__ == "__main__":
  nums = get_cmdline_actuators()
  print("Firing {}".format(nums))
  set_triggers_by_number(on=nums, off=[])
  time.sleep(FIRE_TIME)
  set_triggers_by_number(on=[], off=nums)
