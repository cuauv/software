#!/usr/bin/env python3
import time
import shm

from vision.modules.base import ModuleBase

class Poster(ModuleBase):
  def __init__(self, *args, **kwargs):
      super().__init__(*args, **kwargs)

  def process(self, *mats):
    for i, im in enumerate(mats):
      for dir in self.directions:
        var = shm._eval('poster_status.{}_counter'.format(dir))
        var.set(var.get() + 1)
      self.post(str(i), im)

if __name__ == '__main__':
    Poster()()
