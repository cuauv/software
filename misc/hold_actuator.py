#!/usr/bin/env python3

from misc.actuator import get_cmdline_actuators, set_triggers_by_number

if __name__ == "__main__":
  import signal

  nums = get_cmdline_actuators()
  print("Holding {}".format(nums))
  set_triggers_by_number(on=nums, off=[])
  signal.pause()
