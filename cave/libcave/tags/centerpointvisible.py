import math

import shm

from gi.repository import Gdk

from misc.log import with_logging
from cave.libcave.tags.tagtype import TagType
from registered_tags import register_tag_type, \
                            require_mission_elements_to_implement

MIN_RADIUS = 20
MAX_RADIUS = 450
ZOOM_STEP = 10
DEFAULT_ZOOM = 70

OPACITY = 0.4

@with_logging
@register_tag_type
@require_mission_elements_to_implement("get_x_y", "get_visible")
class CenterPointVisible(TagType):

    @classmethod
    def get_name(self):
        return "Center Point and Visible"

    @classmethod
    def get_schema(self):
        return (('x', int), ('y', int), ('rad', int))

    @classmethod
    def test_frame(cls, frame, tag, mission_element_instance):
        frame_info = tag.get_frame_info(frame)
        while True:
            x, y = mission_element_instance.get_x_y()
            visible = mission_element_instance.get_visible()
            if (visible) and (math.sqrt(abs(x - frame_info['x'])**2 + abs(y - frame_info['y'])**2) <= frame_info['rad']):
                return True #pass
            if not mission_element_instance.get_next():
                return False #no results group passed our test

    @classmethod
    def load_shm_for_frame(cls, frame, tag):
        frame_info = tag.get_frame_info(frame)

        shm.cave_results.visible.set(True)
        shm.cave_results.x.set(frame_info['x'])
        shm.cave_results.y.set(frame_info['y'])
        shm.cave_results.rad.set(frame_info['rad'])

    ##Drawing related methods
    def __init__(self, tag, get_frame):
        self.tag = tag
        self.get_frame = get_frame
        self.hover_en = False
        self.hover_x = 0
        self.hover_y = 0
        self.radius = DEFAULT_ZOOM

    def hover(self, area, event, frame_x, frame_y):
        self.hover_x = event.x
        self.hover_y = event.y

    def click(self, area, event, frame_x, frame_y):
        #Tag the frame
        self.tag.add_frame_info(self.get_frame(), x=frame_x, y=frame_y, rad=self.radius)
        self.log.info("Tagged frame %d on tag #%d" % (self.get_frame(), self.tag.id))
        return True #Done with tagging; can move onto the next frame

    def enter(self, area, event, frame_x, frame_y):
        self.hover_en = True
    
    def exit(self, area, event, frame_x, frame_y):
        self.hover_en = False
    
    def scroll(self, area, event, frame_x, frame_y):
        if event.direction == Gdk.ScrollDirection.UP:
            self.radius += ZOOM_STEP
        elif event.direction == Gdk.ScrollDirection.DOWN:
            self.radius -= ZOOM_STEP
        self.radius = max(MIN_RADIUS, min(MAX_RADIUS, self.radius))

    def draw(self, widget, cr, x_origin, y_origin):
        #Draw tag circle for this frame (if data exists)
        frame_info = self.tag.get_frame_info(self.get_frame())
        if frame_info is not None:
            #have data; draw frame tag circle
            cr.set_source_rgba(0.0,1.0,1.0,OPACITY)
            cr.arc(x_origin + frame_info['x'], y_origin + frame_info['y'], frame_info['rad'], 0, 2 * math.pi)
            cr.fill()
        
        #Draw hover circle
        if self.hover_en:
            cr.set_source_rgba(0.0,1.0,0.0,OPACITY)
            cr.arc(self.hover_x, self.hover_y, self.radius, 0, 2 * math.pi)
            cr.fill()
