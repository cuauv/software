import os

import shm

import cv2

from datetime import datetime

# log_base_path = os.path.join(os.environ['CUAUV_SOFTWARE'], 'vision', 'video_logs')
log_base_path = '/var/log/auv/current'

class VideoWriter:
    def __init__(self, direction):
        self.fourcc = cv2.VideoWriter_fourcc(*'MJPG')
        self.video_writer = None
        self.frame_count = 0
        self.direction = direction

        # YEAR-MONTH-DAY-HOUR-MINUTES-SECONDS
        time_str = datetime.today().strftime("%Y-%m-%d-%H-%M-%S")
        self.log_path = os.path.join(log_base_path,
                                     '{}_{}.avi'.format(direction, time_str))

        frame_var_name = 'frame_num_{}'.format(self.direction)
        if not hasattr(shm.camera, frame_var_name):
            print("WARNING: no frame number variable in shm for direction {}!".format(self.direction))
            self.frame_var = None
        else:
            self.frame_var = getattr(shm.camera, frame_var_name)

    def log_image(self, mat):
        # write the current frame number to shm. this is useful for shm logging,
        #  and shm log playback
        if self.frame_var is not None:
            self.frame_var.set(self.frame_count)

        self.frame_count += 1
        if self.video_writer is None:
            self.video_writer = cv2.VideoWriter(self.log_path, self.fourcc, 10.,
                                                (mat.shape[1], mat.shape[0]))

        self.video_writer.write(mat)
