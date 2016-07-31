#!/usr/bin/env python3

from vision.modules.logger import VideoWriter
from vision.modules.base import ModuleBase

import shm

class Record(ModuleBase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.writers = [VideoWriter(direction) for direction in self.directions]
        self.frame_num = 0
        self.frame_num_vars = [getattr(shm.camera, "frame_num_%s" % d) for d in self.directions]

    def process(self, *mats):
        for i, im in enumerate(mats):
            self.writers[i].log_image(im)
            self.frame_num_vars[i].set(self.frame_num)
        self.frame_num += 1

if __name__ == '__main__':
    Record()()
