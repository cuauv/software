# encoding: utf-8
"""
vehicle.py

Created by Nick Elser on 2006-09-17. Powered by "The Tears of Small Children" (tm) (c) (r)

"""

import math
from heading import Heading, HeadingDelta
from shm import *

shared_vars = {
    #Sensors
    "heading":              kalman.heading,
    "heading_integrate":    kalman.heading, #Heading used for position integration
    "heading_sensor":       kalman.sensor,
    "pitch":                kalman.pitch,
    "roll":                 kalman.roll,
    "depth":                kalman.depth,
    "speed":                kalman.velx,
    "altitude":             dvl.savg_altitude,
    
    "tag":                  camera.image_tag,
    "forward_width":        camera.forward_width,
    "forward_height":       camera.forward_height,
    "downward_width":       camera.downward_width,
    "downward_height":      camera.downward_height,
   
    #Bins
    "boxd_enabled":         vision_modules.Bins,
    
    #Pipe
    "pipe_x":               pipe_results.center_x,
    "pipe_y":               pipe_results.center_y,
    
    #Silhouettes
    "shaped_enabled":       vision_modules.Bins,

    #Hydropwns
    "angle_thresh":         hydrophones_settings.trackAngleThresh,
    "mag_thresh":           hydrophones_settings.trackMagThresh,
    "search_thresh":        hydrophones_settings.searchThresh,
    "hydrophone_gain":      hydrophones_settings.gain,
    "hydrophone_mode":      hydrophones_settings.dsp_mode,
    "hydrophone_deadtime":  hydrophones_settings.trackDeadtime,
    "pinger_freq":          hydrophones_settings.trackFrequency,
    "hydrophone_freq_bins": hydrophones_results.search_bins,
    "hydrophone_freq_step": hydrophones_settings.searchStep,
    "hydrophone_freq_cent": hydrophones_settings.searchCenter,
    "hydrophone_freq_delt": hydrophones_settings.searchDelta,
    "hydrophone_search_count": hydrophones_results.search_count,
    "hydrophone_uptime":    hydrophones_results.uptime,
    "hydrophone_phase_x":   hydrophones_results.phaseX,
    "hydrophone_phase_y":   hydrophones_results.phaseY,
    "hydrophone_thresh":    hydrophones_settings.searchThresh,
    
    "pinger_heading":       hydrophones_results.heading,
    "pinger_elevation":     hydrophones_results.elevation,
    "pinger_interval":      hydrophones_results.ping_time,
    "pinger_intensity":     hydrophones_results.intensity,
    "ping_count":           hydrophones_results.ping_count,
    
    #DVL
    "dmg_north":            kalman.north,
    "dmg_east":             kalman.east,
   
    "dmg_forward":          kalman.forward,
    "dmg_sway":             kalman.sway,

    "dmg_x":                dvl.dmg_x,
    "dmg_y":                dvl.dmg_y,
    "dmg_z":                dvl.dmg_z,
    "dvl_dmg_north":         dvl.dmg_north,
    "dvl_dmg_east":        dvl.dmg_east,
    "dvl_velx":             dvl.velocity_x,
    "dvl_vely":             dvl.velocity_y,

    # desires
    "desired_speed":        desires.speed,
    "desired_sway_speed":   desires.sway_speed,
    "desired_heading":      desires.heading,
    "desired_depth":        desires.depth,
    "desired_pitch":        desires.pitch,
    "desired_roll":         desires.roll,
    
    #switches
    "torpedo_left":                 actuator_1.trigger,
    "torpedo_right":                actuator_4.trigger,
    "marker_dropper_1":             actuator_3.trigger, #left
    "marker_dropper_2":             actuator_2.trigger, #right
    "grabber_port_grab":            actuator_8.trigger,
    "grabber_port_release":         actuator_7.trigger,
    "grabber_starboard_grab":       actuator_6.trigger,
    "grabber_starboard_release":    actuator_5.trigger,
    "grabber_aft_grab":             actuator_10.trigger,
    "grabber_aft_release":          actuator_9.trigger,
    "grabber_closevent":            actuator_11.trigger,
    "grabber_openvent":             actuator_12.trigger,

    "torpedo_left_fire_time":           (actuator_2.duration, 1000),
    "torpedo_right_fire_time":          (actuator_1.duration, 1000),
    "marker_dropper_1_fire_time":       (actuator_3.duration, 700),
    "marker_dropper_2_fire_time":       (actuator_4.duration, 700),
    "grabber_port_grab_time":           (actuator_5.duration, 10000),
    "grabber_port_release_time":        (actuator_6.duration, 5000),
    "grabber_starboard_grab_time":      (actuator_7.duration, 10000),
    "grabber_starboard_release_time":   (actuator_8.duration, 5000),
    "grabber_aft_grab_time":            (actuator_9.duration, 10000),
    "grabber_aft_release_time":         (actuator_10.duration, 5000),
    "grabber_closevent_time":           (actuator_11.duration, 2000),
    "grabber_openvent_time":            (actuator_12.duration, 2000),

    "wheel_turner_servo":   servo_1.PWM,

    "soft_kill":            switches.soft_kill,
    "hard_kill":            switches.hard_kill,
    "mission_start":        mission_start_switch.mission_start,
    "mission_light":        mission_start_switch.mission_light,
    "heading_enabled":      settings_control.heading_active,
    "pitch_enabled":        settings_control.pitch_active,
    "roll_enabled":        settings_control.roll_active,
    "speed_enabled":        settings_control.velx_active,
    "sway_speed_enabled":   settings_control.vely_active,
    "depth_enabled":        settings_control.depth_active,
    "control_enabled":      settings_control.enabled,

    "heading_kp":           settings_heading.kP,
    
    #mission
    "start_heading":        mission.start_heading,
    "last_pipe_heading":    mission.last_pipe_heading,
    "last_pinger_heading":  mission.last_pinger_heading,
    "marker_1_dropped":     mission.marker_1_dropped,
    "marker_2_dropped":     mission.marker_2_dropped,
    "last_forward":         mission.last_forward,
    "last_heading":         mission.last_heading,
    "last_sway":            mission.last_sway,
    "last_vert":            mission.last_vert,
    "pinger":               mission.pinger,
    "run_number":           mission.run_number,
    
    # mission hover.approach PID values
    "approach_x_kp":        mission_approach.x_kp,
    "approach_x_ki":        mission_approach.x_ki,
    "approach_x_kd":        mission_approach.x_kd,
    "approach_y_kp":        mission_approach.y_kp,
    "approach_y_ki":        mission_approach.y_ki,
    "approach_y_kd":        mission_approach.y_kd,
    "approach_aspect_kp":   mission_approach.aspect_kp,
    "approach_aspect_ki":   mission_approach.aspect_ki,
    "approach_aspect_kd":   mission_approach.aspect_kd,
    "hover_x_kp":           mission_hover.x_kp,
    "hover_x_ki":           mission_hover.x_ki,
    "hover_x_kd":           mission_hover.x_kd,
    "hover_y_kp":           mission_hover.y_kp,
    "hover_y_ki":           mission_hover.y_ki,
    "hover_y_kd":           mission_hover.y_kd,
    "hover_depth_kp":       mission_hover.depth_kp,
    "hover_depth_ki":       mission_hover.depth_ki,
    "hover_depth_kd":       mission_hover.depth_kd,

    #locator
    "locator1_north":        locator1.target_north,
    "locator1_east":         locator1.target_east,
    "locator1_depth":        locator1.target_depth,
    "locator1_probability":  locator1.probability,
    "locator1_stddev":       locator1.stddev,

    "locator2_north":        locator2.target_north,
    "locator2_east":         locator2.target_east,
    "locator2_depth":        locator2.target_depth,
    "locator2_probability":  locator2.probability,
    "locator2_stddev":       locator2.stddev,

    #Other headings
    "dvl_heading":          dvl.heading
}

class SensorCollection(object):
    previous_pipe_headings = {}
    config = None
    
    def __init__(self, config):
        if SensorCollection.config is None:
            SensorCollection.config = config
        
    def __getattr__(self, item):
        if hasattr(SensorCollection, '_'+item): # weird bug in python
            return super(SensorCollection, self).__getattribute__('_'+item)()
        elif item in shared_vars:
            return shared_vars[item].get()

        raise AttributeError
    
    def __setattr__(self, item, value):
        if hasattr(SensorCollection, '_'+item):
            raise Exception, "unable to set ", item
        elif item in shared_vars:
            return shared_vars[item].set(value)

        raise AttributeError
    
    def get(self, item):
        return self.__getattr__(item)
        
    def _heading(self):
        return Heading(shared_vars['heading'].get())
        
    def _desired_heading(self):
        return Heading(shared_vars['desired_heading'].get())
    
    def _start_heading(self):
        return Heading(shared_vars['start_heading'].get())
    
    def _box_heading_delta(self):
        return HeadingDelta(shared_vars['box_heading_delta'].get())

    def _buoy_heading(self):
        # difference in percent of the field of view
        px = (self.buoy_x - shared_vars['forward_width'].get()/2)/(shared_vars['forward_width'].get()/2)
        return HeadingDelta(px * self.config['vision/forward_hor_fov'])

    def _buoy_depth(self):
        # difference in percent of the field of view
        py = (self.buoy_y - shared_vars['forward_height'].get()/2)/(shared_vars['forward_height'].get()/2)
        return (float)(py * self.config['vision/forward_ver_fov'])
        
    def _nest_heading(self):
        # difference in percent of the field of view
        px = (self.nest_x - shared_vars['forward_width'].get()/2)/(shared_vars['forward_width'].get()/2)
        return HeadingDelta(px * self.config['vision/forward_hor_fov'])

    def _nest_depth(self):
        # difference in percent of the field of view
        py = (self.nest_y - shared_vars['forward_height'].get()/2)/(shared_vars['forward_height'].get()/2)
        return (float)(py * self.config['vision/forward_ver_fov'])

    def _wire_heading(self):
        # difference in percent of the field of view
        px = (self.wire_x - shared_vars['forward_width'].get()/2)/(shared_vars['forward_width'].get()/2)
        return HeadingDelta(px * self.config['vision/forward_hor_fov'])

    def _wire_depth(self):
        # difference in percent of the field of view
        py = (self.wire_y - shared_vars['forward_height'].get()/2)/(shared_vars['forward_height'].get()/2)
        return (float)(py * self.config['vision/forward_ver_fov'])

    def _x_heading(self):
        # difference in percent of the field of view
        px = (self.x_x - shared_vars['forward_width'].get()/2)/(shared_vars['forward_width'].get()/2)
        return HeadingDelta(px * self.config['vision/forward_hor_fov'])
        
    def _x_depth(self):
        # difference in percent of the field of view        
        py = (self.x_y - shared_vars['forward_height'].get()/2)/(shared_vars['forward_height'].get()/2)
        return (float)(py * self.config['vision/forward_ver_fov'])

    def _OLDbox_heading(self):
        dx = self.box_x - self.config['vision/dropper_center_x'] 
        dy = self.config['vision/dropper_center_y'] - self.box_y
        
        if dy == 0:
            dy = 0.001 #
            
        deg = math.degrees(math.atan(dx*1.0/ dy*1.0))

        if abs(deg) < 15:
            return HeadingDelta(0)

        return HeadingDelta(deg)

    def _over_box(self):
        dx = abs(self.box_x - self.config['vision/dropper_center_x'])
        dy = abs(self.config['vision/dropper_center_y'] - self.box_y)
        
        return dx < self.config['vision/dropper_distance_threshold'] and dy < self.config['vision/dropper_distance_threshold']
    
    def _pipe_heading(self):
        pipe_heading = shared_vars['pipe_heading'].get()
        return HeadingDelta(pipe_heading) # this this is based off a slope, it should never return > 180
    
    def _speed(self):
        return shared_vars['desired_speed'].get()

    def _shape_heading(self):
        """
        Assumes that there are multiple shapes in frame, and uses their locations to calculate
        the heading of the row of bins
        """
        # Get two points
        x1 = None
        x2 = None
        y1 = None
        y2 = None
        min_prob = self.config['mission/slagathor_interest_threshold']
        
        if shared_vars['net_probability'].get() > min_prob:
            if x1 is None:
                x1 = shared_vars['net_x'].get()
                y1 = shared_vars['net_y'].get()
            elif x2 is None:
                x2 = shared_vars['net_x'].get()
                y2 = shared_vars['net_y'].get()
        if shared_vars['shield_probability'].get() > min_prob:
            if x1 is None:
                x1 = shared_vars['shield_x'].get()
                y1 = shared_vars['shield_y'].get()
            elif x2 is None:
                x2 = shared_vars['shield_x'].get()
                y2 = shared_vars['shield_y'].get()
        if shared_vars['sword_probability'].get() > min_prob:
            if x1 is None:
                x1 = shared_vars['sword_x'].get()
                y1 = shared_vars['sword_y'].get()
            elif x2 is None:
                x2 = shared_vars['sword_x'].get()
                y2 = shared_vars['sword_y'].get()
        if shared_vars['trident_probability'].get() > min_prob:
            if x1 is None:
                x1 = shared_vars['trident_x'].get()
                y1 = shared_vars['trident_y'].get()
            elif x2 is None:
                x2 = shared_vars['trident_x'].get()
                y2 = shared_vars['trident_y'].get()

        # Transform (0,0) to be the center of the frame
        
        if x1 is None or x2 is None or y1 is None or y2 is None:
            return 0

        x1 = x1 - shared_vars['downward_width'].get()/2.0
        y1 = -1*y1 + shared_vars['downward_height'].get()/2.0
        x2 = x2 - shared_vars['downward_width'].get()/2.0
        y2 = -1*y2 + shared_vars['downward_height'].get()/2.0

        # Calculate the angle between the line and the +x-axis
        angle = math.degrees(math.atan2((y2-y1), (x2-x1)))
       
        #Minimize the angle
        if angle > 90:
            angle -= 180
        elif angle < -90:
            angle += 180

        return HeadingDelta(-1*angle) #Flip sign due to coord frame

    def _shape_offset(self):
        """
        Returns the average difference from center (y-dir) of shapes in frame
        """

        delta_y_sum = 0
        delta_x_sum = 0
        count = 0
        x = 0
        y = 0
        min_prob = self.config['mission/slagathor_interest_threshold']

        if shared_vars['net_probability'].get() > min_prob:
            delta_y_sum += (-1*shared_vars['net_y'].get() + shared_vars['downward_height'].get()/2.0)
            delta_x_sum += (shared_vars['net_x'].get() - shared_vars['downward_width'].get()/2.0)
            count += 1
        if shared_vars['shield_probability'].get() > min_prob:
            delta_y_sum += (-1*shared_vars['shield_y'].get() + shared_vars['downward_height'].get()/2.0)
            delta_x_sum += (shared_vars['shield_x'].get() - shared_vars['downward_width'].get()/2.0)
            count += 1
        if shared_vars['sword_probability'].get() > min_prob:
            delta_y_sum += (-1*shared_vars['sword_y'].get() + shared_vars['downward_height'].get()/2.0)
            delta_x_sum += (shared_vars['sword_x'].get() - shared_vars['downward_width'].get()/2.0)
            count += 1
        if shared_vars['trident_probability'].get() > min_prob:
            delta_y_sum += (-1*shared_vars['trident_y'].get() + shared_vars['downward_height'].get()/2.0)
            delta_x_sum += (shared_vars['trident_x'].get() - shared_vars['downward_width'].get()/2.0)
            count += 1
            
        if count > 0:
            y = round(delta_y_sum/count)
            x = round(delta_x_sum/count)
    
        return (x,y)

    def _box_heading(self):
        """
        Returns the heading of a line through the centers of boxes if there is more than one box
        in frame, and None otherwise.
        """
        x_coord = []
        y_coord = []
        box_count = shared_vars['box_count'].get()

        if box_count > 1:
            angle = -math.degrees(math.atan2( (shared_vars['box_x1'].get() - shared_vars['box_x2'].get()), (shared_vars['box_y1'].get() - shared_vars['box_y2'].get()) ))
            if angle > 90: return angle - 180
            if angle < -90: return angle + 180
            return angle
        else:
            return None

            
    def _box_offset(self):
        """
        Returns the average difference from center (y-dir) of boxes in frame
        """

        delta_y_sum = 0
        delta_x_sum = 0
        count = 0
        min_prob = self.config['mission/slagathor_interest_threshold']
        x = 0
        y = 0

        if shared_vars['box_count'].get() > 0:
            delta_y_sum += (-1*shared_vars['box_y1'].get() + shared_vars['downward_height'].get()/2.0)
            delta_x_sum += (shared_vars['box_x1'].get() - shared_vars['downward_width'].get()/2.0)
            count += 1
        if shared_vars['box_count'].get() > 1:
            delta_y_sum += (-1*shared_vars['box_y2'].get() + shared_vars['downward_height'].get()/2.0)
            delta_x_sum += (shared_vars['box_x2'].get() - shared_vars['downward_width'].get()/2.0)
            count += 1
        if shared_vars['box_count'].get() > 2:
            delta_y_sum += (-1*shared_vars['box_y3'].get() + shared_vars['downward_height'].get()/2.0)
            delta_x_sum += (shared_vars['box_x3'].get() - shared_vars['downward_width'].get()/2.0) 
            count += 1
        if shared_vars['box_count'].get() > 3:
            delta_y_sum += (-1*shared_vars['box_y4'].get() + shared_vars['downward_height'].get()/2.0)
            delta_x_sum += (shared_vars['box_x4'].get() - shared_vars['downward_width'].get()/2.0)
            count += 1
                
        if count > 0:
            y = round(delta_y_sum/count)
            x = round(delta_x_sum/count)
        
        return (x,y)

    def _pinger_unit_x(self):
        return math.cos(math.pi/180.0*shared_vars['pinger_heading'].get()) 
    
    def _pinger_unit_y(self):
        return math.sin(math.pi/180.0*shared_vars['pinger_heading'].get()) 
    
    def _pinger_unit_z(self):
        return math.cos(math.pi/180.0*shared_vars['pinger_elevation'].get()) 

    def _hydrophone_detected_frequencies(self):
        det_freqs = []
        index = 0
        mask = shared_vars['hydrophone_freq_bins'].get()
        while mask:
            if mask % 2:
                det_freqs.append(shared_vars['hydrophone_freq_cent'].get() - shared_vars['hydrophone_freq_delt'].get() + (index+0.5)*shared_vars['hydrophone_freq_step'].get())
            index += 1
            mask >>= 1
        return det_freqs

    def give_me_location(self, object=None):
        #Calculates the DVL position of an objects location
        h_fov = self.config['vision/downward_hor_fov']
        v_fov = self.config['vision/downward_ver_fov']
        h_pix = shared_vars['downward_width'].get()
        v_pix = shared_vars['downward_height'].get()
        x = shared_vars[object+'_x'].get()
        y = shared_vars[object+'_y'].get()
        alt = shared_vars['altitude'].get()
        hdg = math.radians(shared_vars['heading'].get())

        #Correct for the camera's coordinate system
        #x = x - h_pix/2.0
        #y = -1*y + v_pix/2.0

        #Time for some math
        dx = x/(h_pix/2.0)
        dy = y/(v_pix/2.0)

        #Find the distance of the frame
        len_x = alt*math.tan(math.radians(h_fov/2.0))
        len_y = alt*math.tan(math.radians(v_fov/2.0))

        # Use a rotation matrix based on heading to calculate position
        pos_x = dx*len_x
        pos_y = dy*len_y
       
        print "X: " + repr(pos_x)
        print "Y: " + repr(pos_y)

        north = pos_y*math.cos(hdg)-pos_x*math.sin(hdg) + shared_vars['dmg_north'].get() 
        east = pos_y*(math.sin(hdg))+pos_x*math.cos(hdg) + shared_vars['dmg_east'].get()

        #Return this
        location = (north, east)
        return location

class SwitchCollection(object):
    class Switch(object):
        def __init__(self, var):
            self.var = var
        def turn_on(self):
            self.var.set(True)
        def turn_off(self):
            self.var.set(False)
        def __nonzero__(self):
            return self.var.get()
        def __str__(self):
            return ["Disabled", "Enabled"][self.var.get()]
        
    def __getattr__(self, item):
        if hasattr(SwitchCollection, '_'+item):
            return super(SwitchCollection, self).__getattribute__('_'+item)()
        elif item in shared_vars:
            return self.Switch(shared_vars[item])

        raise AttributeError

class Vehicle:
    config = None
    
    def __init__(self, config):
        self.config = config
        self.switches = SwitchCollection()
        self.sensors = SensorCollection(config)
        self.shared_vars = shared_vars

    def __getattr__(self, attr):
        """ Allows e.g. vehicle.heading in lieu of
            vehicle.shared_vars['heading'] """
        return self.shared_vars[attr]
       
    def get_sensor(self, name):
        """
        Helper function so that remote objects can read sensor
        values without having full copy of the codebase.
        """
        ## yeah this is ugly, consider killing it....
        return sensor_callbacks[name]()

    def tagLocation(self):
        shared_vars["last_forward"].set(shared_vars["dmg_north"].get())
        shared_vars["last_sway"].set(shared_vars["dmg_east"].get())
        shared_vars["last_vert"].set(shared_vars["desired_depth"].get())
        shared_vars["last_heading"].set(shared_vars["desired_heading"].get()) #tags heading


