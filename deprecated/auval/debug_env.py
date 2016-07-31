import sys
import time

import shm

from control.util import zero_motors
from self_test.thruster_test import test_motor_dockside

from shm import *
from vehicle import *

soft_kill = switches.soft_kill
hard_kill = switches.hard_kill

heading = kalman.heading
pitch = kalman.pitch
roll = kalman.roll

depth_offset = depth.offset

altitude = dvl.savg_altitude

des_depth = desires.depth
des_pitch = desires.pitch
des_speed = desires.speed
des_sway_speed = desires.sway_speed
des_heading = desires.heading
des_roll = desires.roll

depth_on = settings_control.depth_active
pitch_on = settings_control.pitch_active
heading_on = settings_control.heading_active

cpu_temp = diagnostics_cpu.temperature

last_heading = mission.last_pipe_heading
mission_start = mission_start_switch.mission_start
mission_light = mission_start_switch.mission_light

torpedo_left = shared_vars["torpedo_left"]
torpedo_right = shared_vars["torpedo_right"]
marker_1 = shared_vars["marker_dropper_1"]
marker_2 = shared_vars["marker_dropper_2"]
grabber_port_grab = shared_vars["grabber_port_grab"]
grabber_port_release = shared_vars["grabber_port_release"]
grabber_starboard_grab = shared_vars["grabber_starboard_grab"]
grabber_starboard_release = shared_vars["grabber_starboard_release"]
grabber_aft_grab = shared_vars["grabber_aft_grab"]
grabber_aft_release = shared_vars["grabber_aft_release"]
grabber_closevent = shared_vars["grabber_closevent"]
grabber_openvent = shared_vars["grabber_openvent"]

servo = shared_vars["wheel_turner_servo"]

tag = camera.image_tag

def trigger_actuator(a):
    a.set(True)
    time.sleep(0.5)
    a.set(False)

def set_default_actuator_durations():
    keys = ["torpedo_left_fire_time",
            "torpedo_right_fire_time",
            "marker_dropper_1_fire_time",
            "marker_dropper_2_fire_time",
            "grabber_port_grab_time",
            "grabber_port_release_time",
            "grabber_starboard_grab_time",
            "grabber_starboard_release_time",
            "grabber_aft_grab_time",
            "grabber_aft_release_time",
            "grabber_closevent_time",
            "grabber_openvent_time"]
    for k in keys:
        s, v = shared_vars[k]
        s.set(v)

def stress_grabbers():
    set_default_actuator_durations()
    grabbers = [grabber_starboard_grab, \
                grabber_port_grab, \
                grabber_aft_grab, \
                grabber_starboard_release, \
                grabber_port_release, \
                grabber_aft_release]
    while True:
        map(trigger_actuator, grabbers)

def disable_vision():
    raise Exception("Not supported due to lack of updates!")
    buoy_enabled.set(False)
    nest_enabled.set(False)
    pipe_enabled.set(False)
    shape_enabled.set(False)
    wire_enabled.set(False)

    buoy_probability.set(0)
    nest_probability.set(0)
    pipe_probability.set(0)
    shape_1_probability.set(0)
    shape_2_probability.set(0)
    shape_3_probability.set(0)
    shape_4_probability.set(0)
    wire_probability.set(0)

def kill():
    zero_motors()

    depth_on.set(0)
    pitch_on.set(0)
    heading_on.set(0)

    soft_kill.set(1)

def zero_depth():
    depth_offset.set(depth_offset.get() + depth.get())

def enable():
    soft_kill.set(0)

def empty_air():
    print("Now firing everything (minus grabber) to empty air...")

    initial_control = shm.settings_control.enabled.get()
    initial_kill = shm.switches.soft_kill.get()

    shm.settings_control.enabled.set(False)
    shm.switches.soft_kill.set(False)

    shm.actuator_1.duration.set(10000)
    shm.actuator_2.duration.set(10000)
    shm.actuator_4.duration.set(10000)
    shm.actuator_5.duration.set(10000)
    
    shm.actuator_1.trigger.set(1)
    shm.actuator_2.trigger.set(1)
    shm.actuator_4.trigger.set(1)
    shm.actuator_5.trigger.set(1)
   
    time.sleep(14.8)
   
    shm.actuator_1.trigger.set(0)
    shm.actuator_2.trigger.set(0)
    shm.actuator_4.trigger.set(0)
    shm.actuator_5.trigger.set(0)
 
    shm.settings_control.enabled.set(initial_control)
    shm.switches.soft_kill.set(initial_kill)
   
    shm.actuator_1.duration.set(500)
    shm.actuator_2.duration.set(500)
    shm.actuator_4.duration.set(500)
    shm.actuator_5.duration.set(500)

    print("Air empty process complete.")

def fire_ze_missiles():
    v = watchers.watcher()
    v.watch(depth)
    des_depth.set(-0.1)
    while depth.depth.get() > 0.1:
        v.wait()
    des_pitch.set(90)
    time.sleep(5.0415926)
    print("FIRE ZE MISSLES!")
    torpedo_left.set(True)
    time.sleep(5.0415926)
    torpedo_right.set(True)
    time.sleep(3.31415926)
    des_pitch.set(0)
    des_depth.set(1)

def delay_start(tiempo):
    time.sleep(tiempo)
    mission_start.set(True)

def lcd_pr():
    raise Exception("Not supported.")
    while 1:
        # TODO: Using sleep instead of varwatcher since LCD has a limited 
        # update rate. Figure out if this is faster or slower than kalman rate.
        lcd_line1.set("Pitch: %f" % pitch.get())
        lcd_line2.set("Roll : %f" % roll.get())
        time.sleep(.3) 


def oscillate(m, x, t):
    try:
        while True:
            m.set(x)
            time.sleep(t)
            m.set(-x)
            time.sleep(t)
    except:
        zero_motors()

def flash(period=0.5,duration=None):
    try:
        start_time = time.time()
        while duration is None or start_time+duration > time.time():
            mission_light.set(1)
            time.sleep(period)
            mission_light.set(0)
            time.sleep(period)
    except KeyboardInterrupt:
        mission_light.set(0)

def self_destruct():
    raise Exception("Not supported.")
    lcd_line1.set("SELF DESTRUCT IN")
    for i in range(10, 0, -1):
        lcd_line2.set("      " + repr(i) + " SEC")
        time.sleep(1)
    lcd_line1.set("BOOM BOOM BOOM")
    time.sleep(.25)
    lcd_line2.set("BOOM BOOM BOOM")
