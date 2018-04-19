import os
from threading import Thread, Event
from videoutils import hash_video
from misc.log import with_logging

@with_logging
class VideoFinder(Thread):
    """
    Thread that searches for videos in the root directory
    """
    def __init__(self, root_dir, callback):
        Thread.__init__(self)
        self.root_dir = root_dir
        self.callback = callback
        self.kill = Event()
        # A dictionary of hashes to video filenames
        self.look_for = {}

    def destroy(self):
        self.kill.set()

    def done_searching(self):
        return all([not v_fname is None for v_fname in self.look_for.values()])

    def run(self):
        self.log.info("Searching %s for missing video files" % self.root_dir)
        for root, dirs, files in os.walk(self.root_dir):
            for filename in files:
                if self.kill.is_set():
                    return

                if any([filename.endswith(ext) for ext in (".avi", ".mp4")]):
                    full_filename = os.path.join(root, filename)
                    try:
                        h = hash_video(full_filename)
                    except:
                        continue
                    if h in self.look_for:
                        self.log.info("Found %s!" % full_filename)
                        self.look_for[h] = os.path.relpath(full_filename, \
                                                           self.root_dir)
                        self.callback(self.look_for, False)
                        if self.done_searching():
                            return

        self.callback(self.look_for, True)

    def request_search(self, video_hash):
        """ Must be called before the thread is started. """
        self.look_for[video_hash] = None
