import math

from gi.repository import Gdk

from cave.libcave.tags.tagtype import TagType
from registered_tags import register_tag_type, \
                            require_mission_elements_to_implement
from misc.log import with_logging

MIN_RADIUS = 20
MAX_RADIUS = 450
ZOOM_STEP = 10
DEFAULT_ZOOM = 70

OPACITY = 0.4
DEFAULT_HEADING_THRESH = 30 #degrees
HEADING_STEP = 5

def abs_heading_diff(x,y):
    def to180(d):
        if d > 180.:
            return d-360.
        else:
            return d
    return abs(to180((y-x)%360. ))

@with_logging
@register_tag_type
@require_mission_elements_to_implement("get_x_y", "get_visible", "get_heading_delta")
class CenterPointVisibleWithDirection(TagType):

    @classmethod
    def get_name(self):
        return "Center Point / Direction / Visible"

    @classmethod
    def get_schema(self):
        return (('x', int), ('y', int), ('rad', int), ('angle', int), ('angle_thresh', int))

    @classmethod
    def test_frame(cls, frame, tag, mission_element_instance):
        frame_info = tag.get_frame_info(frame)
        while True:
            x, y = mission_element_instance.get_x_y()
            visible = mission_element_instance.get_visible()
            heading_delta = (360 - mission_element_instance.get_heading_delta()) % 360
            # XXX Hack to make this work for pipes.
            correct_angle = (360 - frame_info['angle']) + 90
            if ((visible) and 
                    (math.sqrt(abs(x - frame_info['x'])**2 + abs(y - frame_info['y'])**2) <= frame_info['rad']) and
                    (abs_heading_diff(correct_angle, (heading_delta + 180) % 360) < float(frame_info['angle_thresh']) / 2 or
                    (abs_heading_diff(correct_angle, heading_delta) < float(frame_info['angle_thresh']) / 2))):
                return True
            if not mission_element_instance.get_next():
                return False #no results group passed our test

    @classmethod
    def load_shm_for_frame(cls, frame, tag):
        frame_info = tag.get_frame_info(frame)

        shm.cave_results.visible.set(True)
        shm.cave_results.x.set(frame_info['x'])
        shm.cave_results.y.set(frame_info['y'])
        shm.cave_results.rad.set(frame_info['rad'])
        shm.cave_results.angle.set(frame_info['angle'])
        shm.cave_results.angle_thresh.set(frame_info['angle_thresh'])

    ##Drawing related methods
    def __init__(self, tag, get_frame):
        self.tag = tag
        self.get_frame = get_frame
        self.hover_en = False
        self.hover_x = 0
        self.hover_y = 0
        self.radius = DEFAULT_ZOOM
        self.angle = 0
        self.angle_thresh = DEFAULT_HEADING_THRESH

    def hover(self, area, event, frame_x, frame_y):
        self.hover_x = event.x
        self.hover_y = event.y

    def click(self, area, event, frame_x, frame_y):
        #Tag the frame
        self.tag.add_frame_info(self.get_frame(), x=frame_x, y=frame_y, rad=self.radius, angle=self.angle, angle_thresh=self.angle_thresh)
        self.log.info("Tagged frame %d on tag #%d" % (self.get_frame(), self.tag.id))
        return True #Done with tagging; can move onto the next frame

    def enter(self, area, event, frame_x, frame_y):
        self.hover_en = True
    
    def exit(self, area, event, frame_x, frame_y):
        self.hover_en = False
    
    def scroll(self, area, event, frame_x, frame_y):
        if event.direction == Gdk.ScrollDirection.UP:
            self.angle += HEADING_STEP
        elif event.direction == Gdk.ScrollDirection.DOWN:
            self.angle -= HEADING_STEP
        self.angle %= 360
    

    def draw_wedge(self, cr, angle, delta, x, y, rad):
        def draw_half_wedge(angle_internal):
            sa = math.radians(int((angle_internal - float(delta)/2) % 360))
            ea = math.radians(int((angle_internal + float(delta)/2) % 360))
            ma = math.radians(int((angle_internal) % 360))
            cr.set_source_rgba(0.6,0.0,0.6,OPACITY)
            cr.arc(x, y, rad, sa, ea)
            cr.set_line_width(10.0)
            cr.stroke()
            cr.set_line_width(2.0)
            cr.set_source_rgba(0.6,0.0,0.6,OPACITY)
            cr.arc(x, y, rad, sa, ea)
            cr.line_to(x, y)
            cr.arc(x, y, rad, sa, ea)
            cr.line_to(x, y)
            cr.stroke()
            cr.set_line_width(4.0)
            cr.set_source_rgba(0.9,0.0,0.0,OPACITY)
            cr.arc(x, y, rad * 1.2, ma, ma)
            cr.line_to(x, y)
            cr.stroke()
        draw_half_wedge(angle)
        draw_half_wedge((angle + 180) % 360)

    def draw(self, widget, cr, x_origin, y_origin):
        #Draw tag circle for this frame (if data exists)
        frame_info = self.tag.get_frame_info(self.get_frame())
        if frame_info is not None:
            #have data; draw frame tag circle
            cr.set_source_rgba(0.0,1.0,1.0,OPACITY)
            cr.arc(x_origin + frame_info['x'], y_origin + frame_info['y'], frame_info['rad'], 0, 2 * math.pi)
            cr.fill()
            self.draw_wedge(cr, frame_info['angle'], frame_info['angle_thresh'], x_origin + frame_info['x'], y_origin + frame_info['y'], frame_info['rad'] * 2)
               
        #Draw hover circle
        if self.hover_en:
            cr.set_source_rgba(0.0,1.0,0.0,OPACITY)
            cr.arc(self.hover_x, self.hover_y, self.radius, 0, 2 * math.pi)
            cr.fill()
            self.draw_wedge(cr, self.angle, self.angle_thresh, self.hover_x, self.hover_y, self.radius*2)
