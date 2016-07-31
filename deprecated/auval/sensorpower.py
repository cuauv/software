'''
Provides control over sensor power ports using the desired_status Shared Variable
'''
import sys
import shm
import os

LED_BOARD = 5
HYDROPHONES = 7

def sensor_port_on(portnum):
    if type(portnum) != list:
        portnum = [portnum]
    status = shm.sensor_power.desired_port_status
    port_status = status.get()
    for p in portnum:
        port_status &= ~(1<<(16-int(p)))
    status.set(port_status)

def sensor_port_off(portnum):
    if type(portnum) != list:
        portnum = [portnum]
    status = shm.sensor_power.desired_port_status
    port_status = status.get()
    for p in portnum:
        port_status |= 1<<(16-int(p))
    status.set(port_status)

def is_port_on(portnum):
    port_status = shm.sensor_power.desired_port_status.get()
    return port_status & (1<<(16-int(portnum))) > 0

def sensor_all_on():
    status = shm.sensor_power.desired_port_status
    status.set(0)

def sensor_all_off():
    status = shm.sensor_power.desired_port_status
    status.set(65535)
