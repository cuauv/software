#!/usr/bin/env python3

#while true, ping router, if 5 sec without response, softkill
import time
import os, platform
import shm

from auvlog.client import log
import shm
from mission.framework.primitive import Zero
import subprocess

IP_ADDRESS = '192.168.0.1'
INTERVAL = 1 #seconds between pings
TIMEOUT = 5 #seconds before softkill

def ping(host):
    """
    Returns True if host responds to a ping request
    """

    # Ping parameters as function of OS
    ping_num_param = "-n" if platform.system().lower() == "windows" else "-c"

    # Ping
    return subprocess.run(['ping', ping_num_param, '1', host], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0

walled_time = time.monotonic() - 600
def watch_voltage():
    """
    Posts a wall message to all users who are logged in when the voltage goes
    below safe levels
    """
    global walled_time

    SAFE_VOLTAGE = 14
    if time.monotonic() - walled_time > 600: # wall only once every 10 minutes
        current_voltage = shm.merge_status.total_voltage.get()
        if current_voltage < SAFE_VOLTAGE and current_voltage > 1.0:
            msg = 'WARNNG: Voltage is currently at {}. Battery change recommended!'.format(current_voltage)
            subprocess.run(['wall'], input=msg.encode('utf-8'))
            walled_time = time.monotonic()

def deadman_trigger():
    with open('/tmp/auv-deadman', 'w') as f:
        print('Timeout at {}'.format(time.time()), file=f)

    os.system('killall auv-mission-runner')
    Zero()()
    time.sleep(1)
    shm.switches.soft_kill.set(True)

if __name__ == '__main__':
    last_contact = None
    gx_last = None
    time_start = time.monotonic()
    with open('/tmp/auv-deadman', 'w'):
        pass
    while True:
        if shm.deadman_settings.enabled.get():
            watch_voltage()
            if ping(IP_ADDRESS):
                last_contact = time.monotonic()
                log.deadman('Ping succeeded at {} seconds'.format(last_contact), copy_to_stdout=True)
            else:
                if last_contact:
                    time_elapsed = time.monotonic() - last_contact
                    log.deadman('Last successful ping {} seconds ago'.format(time_elapsed), copy_to_stdout=True)
                else:
                    time_elapsed = time.monotonic() - time_start
                    log.deadman('No successful pings {} seconds since start'.format(time_elapsed), copy_to_stdout=True)

                if time_elapsed >= TIMEOUT:
                    log.deadman.critical('Too long since last ping, zeroing and softkilling now...', copy_to_stdout=True)
                    deadman_trigger()

        gx_new = shm.gx4.packets_received.get()
        if gx_new == gx_last:
            deadman_trigger()
            os.system('trogdor restart gx4')
        gx_last = gx_new
            
        time.sleep(INTERVAL)
