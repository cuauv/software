#!/usr/bin/env python3
import time

from vision.modules.base import ModuleBase

class Framerate(ModuleBase):
  def __init__(self, *args, **kwargs):
      super().__init__(*args, **kwargs)
      self.vsp_data = None
      self.times = [] 

  def print_fps(self):
    self.times.insert(0, time.time())
    while time.time() - self.times[-1] > 5:
        self.times.pop()
        print('fps: {}'.format(len(self.times) / 5.))

  def process(self, *mats):
    for i, im in enumerate(mats):
      self.post(str(i), im)
    self.print_fps()

if __name__ == '__main__':
    Framerate()()
