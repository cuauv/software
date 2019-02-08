
from cave.libcave.sql import SqlClass
from misc.log import with_logging

import os

from cave.libcave.tag import Tag

@with_logging
class Video(SqlClass):

    @classmethod
    def table_setup(cls):
        cls.table_name = "videos"
        cls.c.execute("""CREATE TABLE IF NOT EXISTS %s (
                         id              INTEGER PRIMARY KEY,
                         name            TEXT,
                         video_path      TEXT,
                         video_hash      TEXT,
                         log_path        TEXT,
                         linked_camera   TEXT,
                         meta            TEXT
                         )""" % cls.table_name)
        cls.commit()

    def __init__(self):
        pass

    def __str__(self):
        return ("<Video: %s>" % self.name)

    #Is this video present? (the files linked properly)
    def present(self):
        return os.path.isfile(self.db.get_absolute_path(self.video_path))

    #Gets all tags associated with this video
    def get_tags(self):
        return Tag.get_all(filter_str="WHERE vid=%d" % self.id)

    #Overloaded remove method
    def remove(self):
        tgs = self.get_tags()
        SqlClass.remove(self) #Remove this video
        for t in tgs: #Remove all tags
            t.remove()


