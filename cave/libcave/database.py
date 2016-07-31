import sqlite3
import os
import shutil
import traceback

from video import Video
from tag import Tag
from videoutils import hash_video

import datetime
import time

from misc.log import with_logging

from sql import SqlClass

#Increment whenever SQL tables change in a way that will break old files.
#You should also specify an upgrade function (see below) to provide
#backwards compatibility
DATABASE_VERSION = 2

# Version 2: Add video_hash key to the video table.

#Database info sqlite table for storing metadata for a database
class DBInfo(SqlClass):
    @classmethod
    def table_setup(cls):
        cls.table_name = "dbinfo"
        cls.c.execute("""CREATE TABLE IF NOT EXISTS %s (
                         id               INTEGER PRIMARY KEY,
                         version          INTEGER
                         )""" % cls.table_name)
        cls.commit()
        if len(cls.get_all()) == 0:
            new = cls.new(version=DATABASE_VERSION)

    @classmethod
    def get_version(cls):
        return cls.get_all()[0].version
    
    @classmethod
    def set_version(cls, v):
        q = cls.get_all()[0]
        q.version = v
        q.update()

@with_logging
class Database:
    def __init__(self, filename):
        if filename is None:
            self.log.warning("Invalid database filename specified")
            self.error = True
            return None

        try:
            self.conn = sqlite3.connect(filename, check_same_thread=False, \
                                                  isolation_level="EXCLUSIVE")
        except sqlite3.OperationalError:
            self.log.error("Failed to open database file: %s" % filename)
            self.error = True
            return None

        self.conn.row_factory = sqlite3.Row #return rows objects instead of raw tuples
        self.c = self.conn.cursor()

        self.c.execute("PRAGMA synchronous = 0")
        self.c.execute("PRAGMA journal_mode = OFF")

        self.filename = os.path.abspath(filename)
        self.root_dir = os.path.dirname(self.filename)

        DBInfo.link_sqlite(self)
        Video.link_sqlite(self)
        Tag.link_sqlite(self)

        ### Upgrade functionality
        db_version = DBInfo.get_version()
        if db_version > DATABASE_VERSION:
            self.log.error("Are you trying to provide a database file from the future? Why would you do that? (provided database version %d; expected <= %d)" % (db_version, DATABASE_VERSION))
            self.error = True
            return None

        if db_version != DATABASE_VERSION:
            self.log.warning("Old database version (database is version %d and most recent is version %d)" % (db_version, DATABASE_VERSION))

            backup_filename = "cave_upgrade_backup"
            self.log.info("Creating database backup at %s" % backup_filename)
            shutil.copy2(filename, backup_filename)
            
            #Methods provided to upgrade an old database version to the newest version
            #Running update_functions[i] must upgrade a database of version i-1 to version i.
            #An upgrade function should be provided whenever the DATABASE_VERSION is updated to
            #ensure old versions are still compatible
            upgrade_functions = {}

            def add_hash():
                SqlClass.turn_off_commits()
                videos = Video.get_all()
                tags = [video.get_tags() for video in videos]
                # We need to get all the frame info before
                # we erase the video table!
                for tag_ls in tags:
                    for tag in tag_ls:
                        tag._populate_frame_dict()

                for video in videos:
                    if not video.present():
                        self.log.error("Not all videos are present, cannot upgrade database!")
                        return False

                [video.remove() for video in videos]
                Video.remove_table()
                Video.table_setup()
                for i, video in enumerate(videos):
                    video.video_hash = \
                      hash_video(self.get_absolute_path(video.video_path))
                    Video.add(video)
                    for tag in tags[i]:
                        self.log.info("Adding tag %s in video %s" %(tag, video.video_path))
                        Tag.tag_add(tag)

                SqlClass.turn_on_commits()
                self.conn.commit()
                return True

            upgrade_functions[2] = add_hash

            while db_version < DATABASE_VERSION:
                i = db_version + 1
                if i not in upgrade_functions.keys():
                    self.error = True
                    self.log.error("Unable to upgrade database to version %d: No upgrade functionality provided." % i)
                    return None
                self.log.info("Upgrading database from version %d to %d" \
                              % (db_version, i))
                try:
                    success = upgrade_functions[i]()
                except Exception as e:
                    traceback.print_exc()
                    success = False

                if success:
                    DBInfo.set_version(i)
                    db_version = i
                else:
                    self.log.error("Upgrading database failed.")
                    shutil.copy2(backup_filename, filename)
                    os.remove(backup_filename)
                    break
        ###

        self.log.info("Database linked to %s" % filename)
        self.error = False

    def get_filename(self):
        return self.filename

    #Get a relative path (for storing in the database) based on the absolute path
    def get_relative_path(self, abspath):
        abspath = os.path.abspath(abspath)
        if os.path.commonprefix([abspath, self.root_dir]) != self.root_dir:
            self.log.error("File chosen outside of root database directory")
            return
        return os.path.relpath(abspath, self.root_dir)

    #Get absolute path from a path stored in the datbase
    def get_absolute_path(self, relpath):
        return os.path.join(self.root_dir, relpath)
