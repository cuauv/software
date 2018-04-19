
from threading import Thread, Condition, Event
from time import sleep, time

from misc.log import with_logging

import libshm.parse
from shm_tools.shmlog.parser import LogParser, LogParseException

import bisect #python so cool

MAX_FRAME_DIFF = 20

camera_map = {"Forward": "camera.frame_num_forward",
              "Downward": "camera.frame_num_downward"
             }

FPS = 30 #Fps limit for log frame processing

@with_logging
class LogPlayer(Thread):
    """
    Thread in charge of playing logs
    """
    def __init__(self, filter=''):
        Thread.__init__(self)
        self.c = Condition()

        self.frame = None 
        self.log_to_load = None
        self.last_frame = -1

        self.logparser = None
        
        self.kill = Event()

        self.enabled = False


        # Shared variable filter
        self.filter = filter
        self.filtervars = []
        if filter != '':
            gps = libshm.parse.parse(filter)
            for g in gps:
                self.filtervars += map(lambda x : g['groupname'] + "." + x, g['vars'].keys())
            self.log.info("Filter applied")


        self.start()

    def set_frame(self, frame):
        if not self.enabled:
            return
        if self.logparser is None:
            self.log.error("No log file loaded; unable to seek")
            return
        with self.c:
            self.frame = frame
            self.c.notify()

    def set_enabled(self, enabled):
        if self.logparser is None:
            self.log.error("No log file loaded; unable to play log")
            return

        self.enabled = enabled
        if enabled:
            self.log.info("Log playback now synchronized with CAVE")
        else:
            self.log.info("No longer playing log")
    
    def set_camera(self, linked_camera):
        self.camvar = camera_map[linked_camera]

    #Get the snapshot index of a given frame given the snapshot mapping table
    def index_of_frame(self, frame):
        i = bisect.bisect_right(self.snapshot_frames, frame)
        return max(0, i-1)

    #Switch log files
    def set_log_file(self, filename):
        with self.c:
            self.log_to_load = filename
            self.c.notify()

    #Actually set the log file
    #This can be slow so it is done asynchronously
    def set_log_file_async(self, filename):
        self.log.info("Starting async load of log %s" % filename)
        try:
            self.logparser = LogParser(filename)
        except IOError:
            self.log.error("Unable to load log file %s" % filename)
            self.logparser = None
            return
        except LogParseException:
            self.log.error("Invalid log file %s" % filename)
            self.logparser = None
            return

        #Build frame-to-snapshot mapping
        self.snapshot_frames = []
        for i in range(0, self.logparser.get_snapshot_count()):
            self.logparser.seek_snapshot_index(i)
            (dt, var_changes) = self.logparser.parse_one_slice()
            for (q, var_name, r, val) in var_changes:
                if var_name == self.camvar: #TODO: fix; make work with downward camera
                    self.snapshot_frames.append(val)
                if self.kill.is_set():
                    return
            if self.kill.is_set():
                return

        self.logparser.seek_snapshot_index(0) #Seek to start of the log again
        self.last_index = 0
        self.log.info("Switched to log file %s" % filename)

    def destroy(self):
        with self.c:
            self.kill.set()
            self.c.notify()

    def run(self):
        while not self.kill.is_set():
            with self.c:
                while self.frame is None and self.log_to_load is None:
                    self.c.wait()
                    if self.kill.is_set():
                        return
                seek_frame = None
                log_to_load = None
                if self.frame is not None:
                    seek_frame = self.frame
                    self.frame = None
                if self.log_to_load is not None:
                    log_to_load = self.log_to_load
                    self.log_to_load = None

            if log_to_load is not None:
                self.set_log_file_async(log_to_load)

            if seek_frame is not None:
                t1 = time()

                if (seek_frame <= self.last_frame or seek_frame > self.last_frame + MAX_FRAME_DIFF):
                    index = self.index_of_frame(seek_frame)
                    self.logparser.seek_snapshot_index(index) 

                while True:
                    v = self.logparser.parse_one_slice()
                    if v is None:
                        self.log.warning("Frame %d out of range of log" % seek_frame)
                        break
                    changetime, changelist = v
                    exit = False

                    for (svar, varstr, vtype, val) in changelist:
                        if svar is not None:
                            if self.filtervars and varstr not in self.filtervars:
                                continue
                            svar.set(val)
                        if varstr == self.camvar:
                            if val >= seek_frame:
                                exit = True
                    
                    if exit:
                        #Reached desired frame
                        break

                self.last_frame = seek_frame

                dt = time() - t1
                self.kill.wait(1.0 / FPS - dt)
            
