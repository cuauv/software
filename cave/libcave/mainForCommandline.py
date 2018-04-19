#!/usr/bin/env python2

import os, signal, sys
import logging
import argparse
import math

from time import sleep, time
from tags.registered_tags import load_tag_modules, get_class_from_tagtype
load_tag_modules()
from tag import Tag
from video import Video
from database import Database
from misc.log import init_logging, with_logging
from videoutils import hash_video
from registered_elements import get_registered_elements

@with_logging
class Cave:
    config_file = "~/.cave" # the config file has the path to the last database

    # load database from given path
    def load_db(self,path):
        self.db = Database(path)
        if self.db is not None and self.db.error:
            print "Failed to load database %s" % path
        if self.db is not None and path is not None:
            path = os.path.abspath(path)
            print "Opened database %s" % os.path.basename(path)
    #Print out database table
    def printDB(self):
        if (self.db is not None):
            for item in self.db.fetchall():
                print item
    # add new tag to the database
    def addTag(self,name,missionElement,tag_type):
        Tag.tag_new(name=name,missionElement=missionElement,tag_type=tag_type)
        register_tag_type(tag_type) # might be wrong format
        print("Added tag \"%s\" (tag id %d) to video id %d" % (tag.name, tag.id, tag.vid))
    #add video to the database.  Need video name, camera, vidPath, logPath
    def addVideo(self, vidName, camera, vidPath,logPath=None,meta=""):
        vid_hash = hash_video(vidPath)
        # uncertain what meta is, so I'm ignoring for now
        vid = Video.new(name=vidName, meta=meta, linked_camera=camera,video_hash=vid_hash,video_path=vidPath,log_path=logPath) 
    # TODO: finish the init for the commandline main

    # Generator returns tags to test on
    def get_frames(self, vision_enable=True):
        melements = get_registered_elements()
        frame_count = 0

        for vid in Video.get_all():
            tgs = vid.get_tags()
            if len(tgs) > 0:
                self.log.info("Loading video id %d" % vid.id)
                #if(self.parent != None):
                #    self.parent.video_box.load_video(vid)
                # XXX The below sleep was added to prevent a "Bus Error"
                # XXX when running a test with a lot of disabled tags - Alex S.
                # TODO FIX
                sleep(0.1)

            for tag in filter(lambda t: t.active, tgs):
                frame_list = tag.get_frame_list()[::self.skip]
                if len(frame_list) > 0:
                    #Carry out testing on this tag
                    self.log.info("Testing tag #%d (%d frames)" % (tag.id, len(frame_list)))
                    frame_list.sort()
                    tag.clear_test_results()
                    if vision_enable:
                        if not melements.has_key(tag.mission_element):
                            self.log.warning("Skipping tag %s; not a mission element." % tag.mission_element)
                            continue

                        m_element = melements[tag.mission_element]()
                        m_element.init() #Turn on vision for this mission element

                    for frame in frame_list:
                        #The test of the frame
                        if vision_enable:
                            yield frame, tag, m_element
                        else:
                            yield frame, tag

                        frame_count += 1
                        self.status_callback(frame_count)

                        if self.has_aborted():
                            break

                    if vision_enable:
                        m_element.deinit() #Turn off vision for this mission element

                    self.log.info("Waiting for module shutdown")
                    sleep(1) # We sleep here because vision will reset the enabled flag if vision is stopped and started too qucikly
                             # This happens if we immediately stop and start the same vision module
                             # TODO: Refactor to fix this (or fix this behavior in vision)
                             # Testing could potentially time out if a vision module does not shut down within
                             # this period
                if self.has_aborted():
                    break
            if self.has_aborted():
                break

    #def testVideo(self, tag, mission_element,video):

    def execute(self):
        start = time()
        correct = 0
        total = 0
        for frame, tag, m_element in self.get_frames():
            load_tag_modules()
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


    def __init__(self):
        self.db = None
        # Load database file from arguments if present
        #if args.database:
        #    self.load_db(args.database)
        if os.path.exists(os.path.expanduser(self.config_file)):
            f = open(os.path.expanduser(self.config_file), "r")
            self.load_db(f.read())
        self.skip = 3

