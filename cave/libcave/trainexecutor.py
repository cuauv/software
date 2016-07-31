from time import sleep, time
from misc.log import with_logging
import shm

from executor import Executor

from tags.registered_tags import get_class_from_tagtype

@with_logging
class TrainExecutor(Executor):

    def __init__(self, parent, status_callback, skip, abort_callback=None):
        Executor.__init__(self, parent, status_callback, skip, abort_callback=abort_callback)
        self.trigger_watcher = shm.watchers.watcher()
        self.trigger_watcher.watch(shm.cave_settings)

    def abort(self):
        Executor.abort(self)
        self.trigger_watcher.broadcast()

    def execute(self):
        shm.cave_settings.trigger.set(False)
        shm.cave_results.in_train_mode.set(True)
        for frame, tag, m_element in self.get_frames():

            # Wait for trigger to be true
            self.log.info("Waiting for training shm trigger")
            while shm.cave_settings.trigger.get() == False:
                if self.has_aborted():
                    shm.cave_results.in_train_mode.set(False)
                    return
                self.trigger_watcher.wait(False)
            
            # Load video frame
            self.log.info("Setting frame %d" % frame)
            self.parent.video_box.set_frame(frame)
            self.parent.logplayer.set_frame(frame)

            # Load tag information into shm cave results
            load_frame_fun = get_class_from_tagtype(tag.tag_type).load_shm_for_frame
            load_frame_fun(frame, tag)

            # Wait for vision results
            self.timeout_set(m_element)
            m_element.wait(train=True) 
            self.timeout_clear()

            shm.cave_settings.trigger.set(False) #Update complete; user can now use tag data & vision results for training
        
        # Done with all test frames; mark completion flag 
        shm.cave_results.in_train_mode.set(False)

        # Wait for final request to signal completion
        while shm.cave_settings.trigger.get() == False:
            self.trigger_watcher.wait(False)
        shm.cave_settings.trigger.set(False)
