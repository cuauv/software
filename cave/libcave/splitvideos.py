from misc.log import with_logging
from executor import Executor

from time import time
import cv2

@with_logging
class SplitVideos(Executor):
    def __init__(self, parent, status_callback, skip, abort_callback=None):
        Executor.__init__(self, parent, status_callback, skip, abort_callback=abort_callback)

    def execute(self):
        start = time()
        writers = {}
        for frame, tag in self.get_frames(vision_enable=False):
            self.log.info("Setting frame %d" % frame)
            self.parent.video_box.set_frame(frame)

            # Code for vid splitting
            (image, _, width, height) = \
              self.parent.video_box.vmt.grab_frame(frame)
            if tag.mission_element not in writers:
                fname = ("%s.avi" % str(tag.mission_element)).replace(" ", "_")
                writer = cv2.VideoWriter(fname, \
                                         cv2.cv.FOURCC('F', 'F', 'V', '1'), \
                                         30, \
                                         (width, height))
                writers[tag.mission_element] = writer

            writers[tag.mission_element].write(image)

        map(lambda x: x.release(), writers.values()) # ew side effects

        if not self.has_aborted():
            self.log.critical("Test complete!")
        else:
            self.log.warning("Test was aborted!")
        dt = time() - start
        self.log.info("Elapsed time: %.1f sec" % dt)
