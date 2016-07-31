#! /usr/bin/env python

import shm

shm.settings_heading.kP.set(.3)
shm.settings_heading.kI.set(.02)
shm.settings_heading.kD.set(.05)

shm.settings_pitch.kP.set(.28)
shm.settings_pitch.kI.set(0.1)
#shm.settings_pitch.kD.set(.08)
shm.settings_pitch.kD.set(.12)

shm.settings_roll.kP.set(0.4)
shm.settings_roll.kI.set(0.03)
shm.settings_roll.kD.set(0.08)
#shm.settings_roll.rD.set(30)

shm.settings_depth.kP.set(100)
shm.settings_depth.kI.set(1.5)
shm.settings_depth.kD.set(60)

shm.settings_velx.kP.set(150)
shm.settings_velx.kI.set(0)
shm.settings_velx.kD.set(5)

shm.settings_vely.kP.set(200)
shm.settings_vely.kI.set(0)
shm.settings_vely.kD.set(5)
