from cave.libcave.sql import SqlClass
from misc.log import with_logging

from cave.libcave.tags.registered_tags import get_class_from_tagtype, \
                                              get_tagtype_names

#Maps python types to sqlite keywords
mysql_map = {int: "INTEGER",
             str: "TEXT",
             float: "REAL"}

@with_logging
class Tag(SqlClass):

    @classmethod
    def table_setup(cls):
        cls.table_name = "tags"
        cls.c.execute("""CREATE TABLE IF NOT EXISTS %s (
                         id              INTEGER PRIMARY KEY,
                         vid             INTEGER,
                         name            TEXT,
                         mission_element TEXT,
                         tag_type        TEXT,
                         tag_data        TEXT,
                         FOREIGN KEY(vid) REFERENCES videos(id)
                         )""" % cls.table_name)
        cls.commit()
    
    def __init__(self):
        self.frame_info = None
        self.active = True

        self.test_pos = [] #Positive result frames (pass)
        self.test_neg = [] #Negative result frames (fail)

    def __str__(self):
        return ("<Tag: %s>" % self.name)

    def set_parent(self, video):
        self.vid = video.id
        self.update()

    def get_parent_video(self):
        from cave.libcave.video import Video #need to keep this here to avoid circular import...
        vids = Video.get_all(filter_str="WHERE id=%d" % self.vid)
        if len(vids) != 1:
            self.log.error("ERROR: invalid parent video")
            return None
        else:
            return vids[0]

    def clear_test_results(self):
        self.test_pos = []
        self.test_neg = []

    def register_test_results(self, frame, pn):
        if pn: #positive
            self.test_pos.append(frame)
            self.log.info("Frame %d pass" % frame)
        else: #negative
            self.test_neg.append(frame)
            self.log.info("Frame %d fail" % frame)

    def remove_frame_results(self, frame):
        if frame in self.test_pos:
            self.test_pos.remove(frame)
        if frame in self.test_neg:
            self.test_neg.remove(frame)

    
    def _populate_frame_dict(self):
        if self.frame_info is None:
            #Fetch frame info from the database
            self.frame_info = {}
            self.c.execute("SELECT * FROM %s" % self.tag_data)
            rows = self.c.fetchall()
            for r in rows:
                d = dict(r)
                frame = d['frame']
                del d['frame']
                self.frame_info[frame] = tuple(zip(d.keys(), d.values()))
    
    #Gets a list of frames that have data stored (with optional filtering max / min)
    def get_frame_list(self, frame_min=None, frame_max=None):
        if self.frame_info is None:
            self._populate_frame_dict()
        k = self.frame_info.keys()
        def ftest(f):
            if frame_min is not None and f < frame_min:
                return False
            if frame_max is not None and f > frame_max:
                return False
            return True
        if frame_min is not None or frame_max is not None:
            k = filter(ftest, k)
        return k

    #Gets the frame information for the given frame number; none if not available
    def get_frame_info(self, frame):
        if self.frame_info is None:
            self._populate_frame_dict()
        if frame not in self.frame_info:
            return None
        return dict(self.frame_info[frame])

    #Adds the frame information provided for the given frame.
    #Arguments should match the format given by the appropriate schema
    #for example, a Center Point and Visible tag could be called by
    # tag.add_frame_info(314, x=15, y=92, rad=65)
    def add_frame_info(self, frame, **args):
        if self.frame_info is None:
            self._populate_frame_dict()

        self.remove_frame_info(frame) #Existing data for this tag must be removed

        self.log.info("Frame data added to frame %d on tag %d" % (frame, self.id))

        args["frame"] = frame
        if frame in self.frame_info:
            #Update existing value
            self.c.execute("UPDATE %s SET %s WHERE frame = %s" % (self.tag_data, ','.join("%s = ?" % k for k in args.keys()), frame), args.values())
        else:
            #Add new values
            self.c.execute("INSERT INTO %s(%s) values (%s)" % 
                (self.tag_data, ','.join(args.keys()), ','.join(['?' for k in args.keys()])), tuple(args.values()))

        del args["frame"]
        self.commit()
        self.frame_info[frame] = tuple(zip(args.keys(), args.values()))

    #Remove information for the given frame
    def remove_frame_info(self, frame):
        if self.frame_info is None:
            self._populate_frame_dict()
        if frame in self.frame_info:
            self.log.info("Remove frame %d from tag #%d" % (frame, self.id))
            del self.frame_info[frame]
            self.c.execute("DELETE FROM %s WHERE frame = '%s'" % (self.tag_data, frame))
            self.commit()
        self.remove_frame_results(frame)

    #Specialized method for creating tags; do not use "new" directly
    @classmethod
    def tag_new(cls, **args):
        if "tag_type" not in args or args["tag_type"] not in get_tagtype_names():
            raise ValueError("Invalid tag type specified")

        args["tag_data"] = "" #placeholder
        tag = cls.new(**args)

        #Generate a tag data table
        data_structure = get_class_from_tagtype(tag.tag_type).get_schema()
        tag.tag_data = "tagdata_%d" % tag.id #Unique table name for this data
        ds = ','.join([("%s %s" % (name, mysql_map[typ])) for (name, typ) in data_structure]) #generate DB schema
        if len(ds) > 0:
            ds = "," + ds
        cls.c.execute("""CREATE TABLE IF NOT EXISTS %s (
                         frame      INTEGER PRIMARY KEY
                         %s
                       )""" % (tag.tag_data, ds))
        cls.commit()

        tag.update()

        return tag

    @classmethod
    def tag_add(cls, instance):
        new_instance = cls.tag_new(**cls.get_keys_from_instance(instance))
        # We want to use the instance passed, because it may have frames
        # but we also need to make sure the correct tag_data is used.
        instance.tag_data = new_instance.tag_data
        for frame_num in instance.get_frame_list():
            instance.add_frame_info(frame_num, **instance.get_frame_info(frame_num))

    #Overloaded remove method
    def remove(self):
        SqlClass.remove(self)
        #Remove the data table
        self.c.execute("""DROP TABLE IF EXISTS %s""" % self.tag_data)


