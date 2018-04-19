#!/usr/bin/env python3

#while true, ping router, if 5 sec without response, softkill
import time
import os, platform
import shm

from auvlog.client import log
import shm
from mission.framework.primitive import Zero

IP_ADDRESS = '192.168.0.1'
INTERVAL = 1 #seconds between pings
TIMEOUT = 5 #seconds before softkill

def ping(host):
    """
    Returns True if host responds to a ping request
    """

    # Ping parameters as function of OS
    ping_str = "-n 1" if platform.system().lower()=="windows" else "-c 1"

    # Ping
    return os.system('ping {} {} > /dev/null'.format(ping_str, host)) == 0

walled_time = 0
def watch_voltage():
    """
    Posts a wall message to all users who are logged in when the voltage goes
    below safe levels
    """
    global walled_time

    SAFE_VOLTAGE = 14
    if time.time() - walled_time > 600: # wall only once every 10 minutes
        current_voltage = shm.merge_status.total_voltage.get()
        if current_voltage < SAFE_VOLTAGE and current_voltage > 1.0:
            msg = 'WARNING: Voltage is currently at {}. Battery change recommended!'.format(current_voltage)
            os.system('echo {} | wall'.format(msg))
            walled_time = time.time()

if __name__ == '__main__':
    last_contact = None
    time_start = time.time()
    os.system("> /tmp/auv-deadman")
    while True:
        if shm.deadman_settings.enabled.get():
            watch_voltage()
            if ping(IP_ADDRESS):
                last_contact = time.time()
                log.deadman('Ping succeeded at {} seconds'.format(last_contact), copy_to_stdout=True)
            else:
                if last_contact:
                    time_elapsed = time.time() - last_contact
                    log.deadman('Last successful ping {} seconds ago'.format(time_elapsed), copy_to_stdout=True)
                else:
                    time_elapsed = time.time() - time_start
                    log.deadman('No successful pings {} seconds since start'.format(time_elapsed), copy_to_stdout=True)

                if time_elapsed >= TIMEOUT:
                    log.deadman.critical('Too long since last ping, zeroing and softkilling now...', copy_to_stdout=True)
                    os.system("echo 'Timeout at {}' > /tmp/auv-deadman".format(time.time()))

                    os.system('killall auv-mission-runner')
                    Zero()()
                    time.sleep(1)
                    shm.switches.soft_kill.set(True)

        time.sleep(INTERVAL)
