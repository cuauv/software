#!/usr/bin/env python3

import shm

# Optimize controls for simulation.
shm.settings_roll.kP.set(0.4)
shm.settings_roll.kI.set(0.0)
shm.settings_roll.kD.set(0.1)

shm.settings_pitch.kP.set(0.4)
shm.settings_pitch.kI.set(0.0)
shm.settings_pitch.kD.set(0.1)

shm.settings_heading.kP.set(0.1)
shm.settings_heading.kI.set(0.0)
shm.settings_heading.kD.set(0.05)

shm.settings_velx.kD.set(0.0)
shm.settings_vely.kD.set(0.0)

shm.settings_depth.kI.set(0.0)
shm.settings_depth.ramp_speed.set(10.0)

# Turn on all PID loops.
shm.settings_control.depth_active.set(1)
shm.settings_control.velx_active.set(1)
shm.settings_control.vely_active.set(1)
shm.settings_control.heading_active.set(1)
shm.settings_control.pitch_active.set(1)
shm.settings_control.roll_active.set(1)

# Un-softkill.
shm.switches.soft_kill.set(0)

shm.desires.speed.set(0)
shm.desires.sway_speed.set(0)
shm.desires.depth.set(0)
shm.desires.roll.set(0)
shm.desires.pitch.set(0)
shm.desires.heading.set(0)
