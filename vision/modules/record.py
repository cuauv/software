#!/usr/bin/env python3

from vision.modules.logger import VideoWriter
from vision.modules.base import ModuleBase

import os
import shm
from datetime import datetime

auv_log_dir = os.path.join(os.environ['CUAUV_LOG'], "current")
print(auv_log_dir)

class Record(ModuleBase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.recording = False

    def process(self, *mats):
        active_mission = shm.active_mission.get()
        if not self.recording and active_mission.active:
            self.recording = True
            log_dir = auv_log_dir if not active_mission.log_path else active_mission.log_path.decode('utf-8')
            make_filename = lambda d: "%s/%s_%s.avi" % (log_dir, d, str(datetime.now().today()))
            for d in self.directions:
                print(make_filename(d))
            self.writers = \
                [VideoWriter(d, filename=make_filename(d)) for d in self.directions]
            self.frame_num = 0
            self.frame_num_vars = \
                [getattr(shm.camera, "frame_num_%s" % d) for d in self.directions]

        elif self.recording and not active_mission.active:
            self.recording = False
            for writer in self.writers:
                writer.close()
            self.writers = []
            self.frame_num_vars = []

        if self.recording:
            for i, im in enumerate(mats):
                self.writers[i].log_image(im)
                self.frame_num_vars[i].set(self.frame_num)
            self.frame_num += 1

if __name__ == '__main__':
    Record()()
