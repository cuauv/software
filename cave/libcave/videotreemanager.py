
from gi.repository import Gtk, Gdk, GLib
from misc.log import with_logging

from cave.libcave.video import Video
from cave.libcave.videofinder import VideoFinder
from cave.libcave.tag import Tag

@with_logging
class VideoTreeManager:
    """
    Handles all video / log tree manipulations
    """

    def __init__(self, tree, parent):
        self.tree = tree
        self.parent = parent
        self.last_video_selected = None
        self.last_tag_selected = None

        self.column_i = 1
        
        def add_column(title):
            cell = Gtk.CellRendererText()
            names = Gtk.TreeViewColumn(title, cell, markup=self.column_i)
            self.tree.append_column(names)
            self.column_i += 1

        add_column("Location")
        add_column("Type")
        add_column("Results")

        self.treestore = Gtk.TreeStore(str)

        # checkbox column
        self.CHECKBOX_INDEX = self.column_i
        self.check_cell = Gtk.CellRendererToggle()
        col = Gtk.TreeViewColumn("Enable", self.check_cell, active=self.column_i)
        self.check_cell.connect("toggled", self.on_toggle, self.treestore)
        self.tree.append_column(col)
        
        self.tree.set_model(self.treestore)

        self.videofinder = None

    def destroy(self):
        if self.videofinder is not None:
            self.videofinder.destroy()

    def on_select(self, tr_select):
        if isinstance(self.get_selection(), Video):
            self.switch_to_tag(None)

        vid_select = self.get_selected_video()
        if vid_select is not None:
            self.switch_to_video(vid_select)

        tag_select = self.get_selected_tag()
        if tag_select is not None:
            self.switch_to_tag(tag_select)


    def get_selection(self):
        item = self.tree.get_selection()
        if item is None:
            return None
        item = item.get_selected()[1]
        if item is None:
            return None
        obj_select = self.treestore.get_value(item, 0)
        return obj_select

    ## Video specific

    def switch_to_video(self, vid_select):
        if not vid_select.present():
            self.log.warning("Selected video not present")
            return

        if vid_select == self.last_video_selected:
            if vid_select is not None and isinstance(self.get_selection(), Video):
                # Selected already active video
                self.parent.timeline.set_length(self.parent.video_box.length)
                self.parent.set_frame_to(0)
            return
        self.last_video_selected = vid_select

        self.log.info("Selected video id %d" % vid_select.id)
        self.parent.video_box.load_video(vid_select)
        #self.parent.tag_tree_manager.redraw()
        self.parent.timeline.set_length(self.parent.video_box.length)
        self.parent.logplayer.set_camera(vid_select.linked_camera)

        if vid_select.log_path:
            filename = Video.db.get_absolute_path(vid_select.log_path)
            self.parent.logplayer.set_log_file(filename)
    
    def get_selected_video(self):
        obj_selected = self.get_selection()
        if isinstance(obj_selected, Video):
            return obj_selected
        elif isinstance(obj_selected, Tag):
            # Get the video associated with this tag
            return obj_selected.get_parent_video()
        else:
            return None

    ## Tag specific

    def on_toggle(self, cell, path, model, *ignore):
        if path is not None:
            it = self.treestore.get_iter(path)
            obj = self.treestore[it][0]
            new_value = not self.treestore[it][self.CHECKBOX_INDEX] #new value for the checkbox

            if isinstance(obj, Tag):
                tag = obj
                self.treestore[it][self.CHECKBOX_INDEX] = new_value
                tag.active = new_value

                # TODO: video logic for global check
                
                parent = tag.get_parent_video()
                tag_status = [x.active for x in parent.get_tags()]
                self._get_row_for_video(parent)[self.CHECKBOX_INDEX] = any(tag_status)

            elif isinstance(obj, Video):
                vid = obj
            
                # Video is toggled directly; set values for all child tags
                self.treestore[it][self.CHECKBOX_INDEX] = new_value
                for tagrow in self._get_tagrows_for_video(vid):
                    tagrow[0].active = new_value
                    self.log.info("Changing tag %s to %s" % (str(tagrow[0]), new_value))
                    tagrow[self.CHECKBOX_INDEX] = new_value


    def _get_tagrows_for_video(self, vid):
        for row in self.treestore:
            if vid == row[0]:
                self.log.warning("Obj is %s" % str(row))
                for tagrow in row.iterchildren():
                    yield tagrow

    def _get_row_for_video(self, vid):
        for row in self.treestore:
            if vid == row[0]:
                return row

    def set_tag_vision_enabled(self, enabled):
        selected = self.get_selected_tag()
        if selected is None:
            self.log.error("No tag is selected")
        me = self.melements[selected.mission_element]()
        if enabled:
            me.init()
            self.log.info("Enabled vision module for %s" % selected.mission_element)
        else:
            me.deinit()
            self.log.info("Disabled vision module for %s" % selected.mission_element)

    def set_selected_tag(self, tag):
        self.log.error("Set selected tag not impl")

    def get_selected_tag(self):
        obj_selected = self.get_selection()
        if isinstance(obj_selected, Tag):
            return obj_selected
        else:
            return None

    def switch_to_tag(self, tag_select):
        if tag_select == self.last_tag_selected:
            return
        self.last_tag_selected = tag_select

        if tag_select is not None:
            self.log.info("Selected tag id %d" % tag_select.id)
            
        tag_select = self.get_selected_tag()
        if tag_select is not None:
            if not tag_select.get_parent_video().present():
                self.log.warning("Tag's video is not present")
                return

            fl = tag_select.get_frame_list()
            if len(fl) > 0:
                vs = min(fl)
                ve = max(fl)

                #Move the timeline into our desired tag range
                self.log.info("Seeking to tag extents (%d - %d)" % (vs, ve))
                self.parent.timeline.set_range(vs, ve)
            self.parent.video_box.set_tag(tag_select)

            #Refresh all drawn information (since the tag may have changed)
            self.parent._change_frame(self.parent.timeline.cursor)
            self.parent.video_box.queue_draw()
            self.parent.timeline.queue_draw()

        else:
            self.parent.video_box.set_tag(None)


    def toggle_all_tags(self):
        vidrows = []
        tagrows = []
        cellvals = []

        for row in self.treestore:
            vidrows.append(row)
            for tagrow in row.iterchildren():
                tagrows.append(tagrow)
                cellvals.append(tagrow[self.CHECKBOX_INDEX])

        new_val = not (any(cellvals))
        for vidrow in vidrows:
            vidrow[self.CHECKBOX_INDEX] = new_val
        for tagrow in tagrows:
            tagrow[0].active = new_val
            self.log.debug("Tag %s is %s" % (tagrow[0].name, tagrow[0].active))
            tagrow[self.CHECKBOX_INDEX] = new_val

    def exclusively_enable_by_element(self, mission_element):
        """
            Will enable the tags that are for the given mission element and
            will disable everything else.

            mission_element is a string of the mission element's name.
        """
        for vidrow in self.treestore:
            vid_enable = False
            for tagrow in vidrow.iterchildren():
                tag = tagrow[0]
                new_val = tag.mission_element == mission_element
                vid_enable = vid_enable or new_val

                tag.active = new_val
                self.log.debug("Tag %s is %s" % (tag.name, tag.active))
                tagrow[self.CHECKBOX_INDEX] = new_val

            vidrow[self.CHECKBOX_INDEX] = vid_enable
    ##

    def missing_found(self, hash_to_video, done):
        for vhash, video_filename in hash_to_video.items():
            if video_filename is not None:
                self.missing_hashes[vhash].video_path = video_filename
                self.missing_hashes[vhash].update()

        if done:
            self.done_searching = True

        self.redraw()

    def redraw(self, filter_str=""):
        #TODO: Sort by creation time
        #TODO: Do some hierarchical organization (by day)

        red_fmt = "<span background='red'>%s</span>"
        treestore = Gtk.TreeStore(object, str, str, str, bool)
        vds = Video.get_all(filter_str)
        if self.videofinder is None:
            self.missing_hashes = {}
        for v in vds:
            if not v.present():
                if hasattr(v, "video_hash"):
                    if self.videofinder is None or not self.done_searching:
                        fmt = red_fmt % "%s [MISSING - Searching...]"
                        if self.videofinder is None:
                            self.missing_hashes[v.video_hash] = v
                    else:
                        fmt = red_fmt % "%s [MISSING - Not found.]"

                else:
                    self.log.warning( \
                  "Video %s is missing but has no hash; won't search" % v)
                    fmt = red_fmt % "%s [MISSING - No Hash.]"

            else:
                fmt = "%s"

            rowparent = treestore.append(None, \
         (v, fmt % v.video_path, "", "", any([t.active for t in v.get_tags()])))                
            for t in v.get_tags():
                if len(t.test_pos) > 0 or len(t.test_neg) > 0:
                    accuracy = "%.1f%%" % \
        (float(len(t.test_pos)) / (len(t.test_pos) + len(t.test_neg)) * 100)

                else:
                    accuracy = ""

                treestore.append(rowparent, \
                   (t, t.mission_element, t.tag_type, accuracy, t.active))

        # Spawn a search for the missing videos.
        if self.videofinder is None and len(self.missing_hashes):
            self.videofinder = VideoFinder(self.parent.db.root_dir, \
                                           self.missing_found)
            map(self.videofinder.request_search, self.missing_hashes.keys())
            self.done_searching = False
            self.videofinder.start()


        self.treestore = treestore
        self.tree.set_model(treestore)
        self.tree.expand_all()
