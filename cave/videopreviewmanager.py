from threading import Thread, Condition, Event, Lock
from time import sleep, time

from gi.repository import Gtk, Gdk, GLib

from misc.log import with_logging

#Params for LRU cleanup of cached video preview images
CACHED_IMAGES_COUNT_CLEAN_THRESH = 20000
CLEAN_PERCENT = 0.5

#A cached video preview container
class PreviewCacheElement:
    preview_frame = None
    last_access_time = None

"""
Class for acquiring and caching video previews

Image previews can be requested by the timeline. This thread will process
these requests in order and do a callback to the timeline to inform that
the image has arrived. The timeline can then retry the image request which
will return a cached image.
"""
@with_logging
class VideoPreviewManager(Thread):

    def __init__(self, videomanagerthread):
        Thread.__init__(self)
        self.vmt = videomanagerthread
        
        self.cached_images = {}
        self.last_access = {}
        self.cache_lock = Lock()

        self.image_requests = []
        self.request_lock = Lock()
        self.request_c = Condition(self.request_lock)

        self.active_video = None
        self.kill = False
        
        self.callback = None #callback function to inform of frame request completion
        self.enabled = True

        self.frame_height = 60 #can be overridden by timeline; change preview height there

        self.start()

    def register_callback(self, callback):
        self.callback = callback

    #Shut down this thread now! (@ app shutdown)
    def destroy(self):
        with self.request_lock:
            self.kill = True
            self.request_c.notify()

    def do_cleanup(self):
        self.log.info("Performing LRU video preview cache cleanup")
        start_time = time()
        with self.cache_lock:
            access_list = []
            for (video,v) in self.cached_images.items():
                for (frame_num, ce) in v.items():
                    access_list.append((ce.last_access_time, (video, frame_num)))
            access_list.sort()
            assert(CLEAN_PERCENT >= 0 and CLEAN_PERCENT <= 1)
            to_remove = access_list[0:int(len(access_list) * CLEAN_PERCENT)] #remove least recently used first
            for (t, (video, frame_num)) in to_remove:
                del self.cached_images[video][frame_num]
        self.log.info("Completed cache cleanup in %.6f seconds" % (time() - start_time))

    def set_enabled(self, enabled):
        with self.request_lock:
            self.enabled = enabled
            self.images_requests = []
        self.log.info("Video preview functionality has been %s." % ("enabled" if enabled else "disabled"))

    def clear_all_requests_except(self, except_list):
        with self.request_lock:
            if len(self.image_requests) > 0:
                for i in self.image_requests:
                    if i not in except_list:
                        self.image_requests.remove(i)

    def request_frame(self, video, frame_number):
        if video is None:
            return

        if not self.enabled:
            # Do not request frame; do not return anything
            return None

        # check the cache for a hit
        with self.cache_lock:
            if video in self.cached_images:
                if frame_number in self.cached_images[video]:
                    #self.log.info("Looked up %d from cache" % frame_number)
                    co = self.cached_images[video][frame_number]
                    co.last_access_time = time()
                    return co.preview_frame

        with self.request_lock:
            if video != self.active_video:
                self.active_video = video
                # clear all outstanding requests
                self.image_requests = []

            #Add our request
            if not frame_number in self.image_requests:
                #self.log.info("Requesting frame %d" % frame_number)
                self.image_requests.append(frame_number)
                self.request_c.notify()

            #Signal that we do not have the image at this time
            return None

    # Continuously process frame requests
    def run(self):
        while not self.kill:
            with self.request_lock:
                while len(self.image_requests) == 0 and (not self.kill):
                    self.request_c.wait()
                if self.kill:
                    return
                frame_to_grab = self.image_requests[0]
                active_video = self.active_video
                self.image_requests.remove(frame_to_grab)

                #TODO: Prevent frame that we're working on from being added to the request list again?
                # is this actually happening / a problem?

            #Grab the desired frame
            if self.vmt is not None:
                if self.vmt.video == active_video:
                    image_result = self.vmt.grab_frame(frame_to_grab, resize=(None, self.frame_height))
                    if image_result is not None:
                        img, frame, width, height = image_result

                        if frame is not None:

                            #Save the grabbed frame in cache
                            with self.cache_lock:
                                if active_video not in self.cached_images:
                                    self.cached_images[active_video] = {}
                                co = PreviewCacheElement()
                                co.preview_frame = frame
                                co.last_access_time = time()
                                self.cached_images[active_video][frame_to_grab] = co
                                #self.log.info("Added frame %d to cache" % frame_to_grab)
                                total_images = sum(len(y) for y in self.cached_images.values())

                            #self.log.info("Cache contains %d images" % total_images)

                            # Do the callback
                            if self.callback is not None:
                                Gdk.threads_enter()
                                self.callback(frame_to_grab, active_video)
                                Gdk.threads_leave()

                            # check for cache cleanup condition
                            if total_images >= CACHED_IMAGES_COUNT_CLEAN_THRESH:
                                with self.request_lock:
                                    requests_left = len(self.image_requests)
                                # only clean when there is no pressing work left to do
                                if requests_left == 0:
                                    self.do_cleanup()
