#!/usr/bin/env python2

"""
CAVE database merge tool
Designed for seemless integration with git so that CAVE database files
can be version controlled & merged!

TODO: this doesn't currently work; finish implementation

"""

import argparse
import logging
import os
import shutil
import sys

from tags.registered_tags import load_tag_modules
load_tag_modules()

ap = argparse.ArgumentParser(description='CAVE Database Merge Utility') 
ap.add_argument('ancestor', type=str, help='common ancestor of the two diverged databases')
ap.add_argument('current', type=str, help='database from the current branch (HEAD); result will be stored here (overwrite)')
ap.add_argument('other', type=str, help='database from the other branch')
args = ap.parse_args()

from misc.log import with_logging, init_logging
from database import Database
from sql import SqlClass
from video import Video
from tag import Tag

@with_logging
class CaveMerger:
    def __init__(self):
        pass

    #Merge on the assumption that the video path is identifying (videos with the same video path are the same)
    #this can be changed if something else makes more sense (name perhaps?)
    def vid_hash(self, v):
        return getattr(v, "video_path")

    def hash_to_vid(self, hsh):
        vds = Video.get_all(filter_str = "WHERE video_path=\"%s\"" % hsh)
        if len(vds) != 1:
            self.log.error("UNIQUE VIDEO ASSUMPTION INCORRECT") #TODO: assert?
            raise Exception()
        return vds[0]

    def copy_video(self, vhash, source_db, dest_db, copy_tags=False):
        Video.link_sqlite(source_db)
        v = self.hash_to_vid(vhash)
        self.log.info("Copy video %s" % v)
        video_params = {}
        for k in v.__get_table_keys__():
            if k not in v.ban_keys:
                video_params[k] = getattr(v, k)
        Video.link_sqlite(dest_db)
        new_vid = Video.new(**video_params)

        if copy_tags:
            Tag.link_sqlite(source_db)
            for t in v.get_tags():
                new_tag = self.copy_tag(t, source_db, dest_db, copy_tag_data=True)
                new_tag.set_parent(new_vid)

        return new_vid

    def copy_tag(self, t, source_db, dest_db, copy_tag_data=False):
        self.log.info("Copy tag %s" % t)
        Tag.link_sqlite(source_db)
        tag_params = {}
        for k in t.__get_table_keys__():
            if k not in t.ban_keys:
                tag_params[k] = getattr(t, k)
        Tag.link_sqlite(dest_db)
        new_tag = Tag.tag_new(**tag_params)

        if copy_tag_data:
            Tag.link_sqlite(source_db)
            for frame in t.get_frame_list():
                Tag.link_sqlite(source_db)
                frame_info = t.get_frame_info(frame)
                Tag.link_sqlite(dest_db)
                new_tag.add_frame_info(frame, **frame_info)

        return new_tag

    def do_merge(self, ancestor_file, current_file, other_file):

        #Verify input files
        def check_file(x):
            if not os.path.isfile(x):
                self.log.error("Error accessing %s" % x)
                return False
        check_file(ancestor_file)
        check_file(current_file)
        check_file(other_file)

        self.log.info("Creating merge temp file")
        TEMP_FILE = "/tmp/cave_merge_temp" #TODO: use tempfile module
        shutil.copy2(ancestor_file, TEMP_FILE)

        #return tuple of the form:
        # 1: dictionary of video -> tag
        # 2: database
        def load_db(x):
            self.log.info("Loading %s" % x)
            db = Database(x)
            if db is None or db.error:
                self.log.error("Failed to load database %s" % x)
                return (None, None)
            d = dict()
            for v in Video.get_all():
                d[v] = v.get_tags()
            return (d, db)
    
        local_vt, local_db = load_db(current_file)
        remote_vt, remote_db = load_db(other_file)
        base_vt, base_db = load_db(TEMP_FILE)
        if local_vt is None or remote_vt is None or base_vt is None:
            return False

        """
        3-WAY MERGING IS HARD!
        """
        self.log.critical("Starting merge!")

        SqlClass.turn_off_commits()

        #STEP 1, look for any videos to delete and do so!
        self.log.info("Deleting any videos not present in derived files.")
        already_removed = set()
        def affect_base(other_vt, other_db, f, *args):
            base_vids = set(map(self.vid_hash, base_vt.keys()))
            other_vids = set(map(self.vid_hash, other_vt.keys()))

            Video.link_sqlite(base_db)
            f(base_vids, other_vids, *args)

        def remove_deletions(base_vids, other_vids):
            vids_to_remove = (base_vids - other_vids) - already_removed
            for vhash in vids_to_remove:
                v = self.hash_to_vid(vhash)
                self.log.info("Removed %s" % v)
                v.remove()

            already_removed.update(vids_to_remove)

        affect_base(local_vt, local_db, remove_deletions)
        affect_base(remote_vt, remote_db, remove_deletions)

        self.log.info("Deleted %d videos." % len(already_removed))

        ##########

        #STEP 2, look for any videos to add and do so!
        self.log.info("Adding any videos that are new in derived files.")
        already_added = set()
        def add_additions(base_vids, other_vids, other_db):
            vids_to_add = (other_vids - base_vids) - already_added
            for vhash in vids_to_add:
                self.copy_video(vhash, other_db, base_db, copy_tags=True)

            already_added.update(vids_to_add)

        affect_base(local_vt, local_db, add_additions, local_db)
        affect_base(remote_vt, remote_db, add_additions, remote_db)

        self.log.info("Added %d videos." % len(already_added))

        #When merging, query the base directly

        #STEP 3, videos in both other and current must be merged!

        local_vids = set(map(self.vid_hash, local_vt.keys()))
        remote_vids = set(map(self.vid_hash, remote_vt.keys()))
        to_merge = local_vids.intersection(remote_vids)

        #videos in to_merge appear in all databases at this point

        #Determine which videos must be merged
       
        if len(to_merge) > 0:
            self.log.warning("The following videos were not checked for changes (video-level merging not yet implemented!): %s" % str(to_merge))

        SqlClass.turn_on_commits()
        base_db.conn.commit()

        ################

        #shutil.copy2(TEMP_FILE, current_file) #output by overwriting current file!
        self.log.critical("Merge complete!")
        return True


if  __name__ =='__main__':
    init_logging(log_level = logging.DEBUG)
    merger = CaveMerger()
    ret = merger.do_merge(args.ancestor, args.current, args.other)
    sys.exit(0 if ret else 1)
