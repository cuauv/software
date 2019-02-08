from misc.log import with_logging
from cave.libcave.executor import Executor
from cave.libcave.tags.registered_tags import get_class_from_tagtype

from time import time

@with_logging
class TestExecutor(Executor):

    def __init__(self, parent, status_callback, skip, abort_callback=None, finish_callback=None):
        Executor.__init__(self, parent, status_callback, skip, abort_callback=abort_callback)
        self.finish_callback = finish_callback

    def execute(self):
        start = time()
        correct = 0
        total = 0
        for frame, tag, m_element in self.get_frames():
            test_fun = get_class_from_tagtype(tag.tag_type).test_frame 

            self.log.info("Setting frame %d" % frame)
            if (self.parent != None):
                self.parent.video_box.set_frame(frame)
            #Wait for vision to process & update the variables
            q = time()
            self.timeout_set(m_element)
            m_element.wait() 
            self.timeout_clear()

            self.log.debug("Wait time is %.5f" % (time() - q))
            q = time()
            result = test_fun(frame, tag, m_element)

            if result:
                correct += 1
            total += 1

            tag.register_test_results(frame, result)
            self.log.debug("Tag reg time %.5f" % (time() - q))
            m_element.reset() #reset mission element state for the next frame test
            self.finish_callback(correct)


        if not self.has_aborted():
            self.log.critical("Test complete!")
        else:
            self.log.warning("Test was aborted!")
        dt = time() - start
        self.log.info("Elapsed time: %.1f sec" % dt)
