from cave.libcave.tags.tagtype import TagType
from registered_tags import register_tag_type, \
                            require_mission_elements_to_implement

from misc.log import with_logging

OPACITY = 1.0

@with_logging
@register_tag_type
@require_mission_elements_to_implement("get_visible")
class NotVisible(TagType):

    @classmethod
    def get_name(self):
        return "Not Visible"

    @classmethod
    def get_schema(self):
        return () #No need to store any additional data

    @classmethod
    def test_frame(cls, frame, tag, mission_element_instance):
        frame_info = tag.get_frame_info(frame)
        while True:
            visible = mission_element_instance.get_visible()
            if (visible):
                return False #saw something; test failed
            if not mission_element_instance.get_next():
                return True #nothing was visible = test pass

    @classmethod
    def load_shm_for_frame(cls, frame, tag):
        frame_info = tag.get_frame_info(frame)
        shm.cave_results.visible.set(False)

    ##Drawing related methods
    def __init__(self, tag, get_frame):
        self.tag = tag
        self.get_frame = get_frame
        self.hover_x = 0
        self.hover_y = 0
        self.hover_en = False 

    def hover(self, area, event, frame_x, frame_y):
        self.hover_x = event.x
        self.hover_y = event.y

    def click(self, area, event, frame_x, frame_y):
        #Tag the frame
        self.tag.add_frame_info(self.get_frame())
        self.log.info("Tagged frame %d on tag #%d as not visible" % (self.get_frame(), self.tag.id))
        return True #Done with tagging; can move onto the next frame

    def enter(self, area, event, frame_x, frame_y):
        self.hover_en = True
    
    def exit(self, area, event, frame_x, frame_y):
        self.hover_en = False
    
    def draw(self, widget, cr, x_origin, y_origin):
        #Draw tag text (if data exists)
        frame_info = self.tag.get_frame_info(self.get_frame())
        if frame_info is not None:
            #have data; draw frame tag circle
            cr.set_source_rgba(0.0,1.0,1.0,OPACITY)
            cr.move_to(x_origin + 3, y_origin + 15)
            cr.show_text("%s tagged as NOT VISIBLE for frame %d (this frame)" % (self.tag.mission_element, self.get_frame()))
        
        #Draw hover text
        if self.hover_en:
            cr.set_source_rgba(0.0,1.0,0.0,OPACITY)
            cr.move_to(self.hover_x, self.hover_y)
            cr.show_text("Click to tag frame %d as NOT VISIBLE" % self.get_frame())
