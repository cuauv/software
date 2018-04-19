#!/usr/bin/env python3

import math
import os
import socket
import signal
import sys

import paramiko
import sqlite3

import shm

from datetime import datetime, date
from threading import Thread, Condition, Event, Lock
from time import time, sleep, gmtime, strftime, localtime

from xml.etree import ElementTree

"""
Uptime tracker
Keeps track of vehicle in-water test time and uploads the data live to chatsworth

TO RESET this uptime information, simply remove (backup first!) the DB_FILE

Additionally, this sqlite database contains information about vehicle runtime
that may be useful. This database can be viewed using any sqlite3 compatible DB reader.

Jeff Heidel 2013
"""

# Local sqlite3 database file for uptime information
# Note: Log directory must exist and be writeable!
DB_FILE = os.path.join(os.environ['CUAUV_LOG'], "uptime.sqlite")

vehicle_env = "CUAUV_VEHICLE"
if vehicle_env not in os.environ:
    print("CUAUV_VEHICLE must be set with the name of the sub and")
    print("that name needs to match that expected by the website.")
    print("Are you trying to spoof in-water time?!")
    sys.exit(-1)

assert(os.environ[vehicle_env].isalnum())
#Uptime xml on remote server
UPTIME_XML = "/srv/www/uptime/uptime_%s.xml" % os.environ[vehicle_env]

DEPTH_THRESH = 0.15 #Vehicle must be below this depth to be considered "test time" #Experimentally derived; should be always above this value in air; always below this value in water (even at surface)

#ssh configs; make sure ssh key is copied so no password authentication is required
SSH_HOST = "cuauv.org"
SSH_PORT = 2222
SSH_USERNAME = "software"

SSH_KEY_FILE = "/home/software/.ssh/id_rsa"

SSH_UPDATE_INTERVAL = 0.5
SQL_UPDATE_INTERVAL = 0.2
depth_shm_group = shm.kalman

####
class mySSHClient(paramiko.SSHClient):
    ## overload the exec_command method (we need timeout)
    def exec_command(self, command, bufsize=64, timeout=None):
        chan = self._transport.open_session()
        chan.settimeout(timeout) #TODO: Timeout doesn't seem to be working properly
        chan.exec_command(command)
        stdin = chan.makefile('wb', bufsize)
        stdout = chan.makefile('rb', bufsize)
        stderr = chan.makefile_stderr('rb', bufsize)
        return stdin, stdout, stderr

class Updater(Thread):
    """
    Thread for periodically sending time updates to chatsworth via SSH
    """
    def __init__(self):
        Thread.__init__(self)
        self.kill = Event()
        self.c = Condition()
        self.hms = None
        self.prev_val = None
        self.start_date = "N/A"

        #SSH init
        self.ssh = mySSHClient()
        self.ssh.set_missing_host_key_policy(paramiko.AutoAddPolicy())

        self.start()

        print("Started chatsworth update thread")

    def set(self, sec):
        sec = int(round(sec))
        hrs = sec // 3600
        sec -= 3600*hrs
        mins = sec // 60
        sec -= mins*60
        with self.c:
            self.hms = (hrs, mins, sec)
            self.c.notify()

    def set_start_date(self, datestr):
        with self.c:
            self.start_date = datestr

    def destroy(self):
        with self.c:
            self.kill.set()
            self.c.notify()

    def run(self):
        print("Sleeping before starting updating thread...")
        self.kill.wait(5) #Wait a bit for system startup before updating
        print("Now updating...")
        with self.c:
            while self.hms is None and not self.kill.is_set():
                self.c.wait() #Wait for first piece of data
        while not self.kill.is_set():

            try:
                #TODO try catch block
                print("Connecting to host")
                socket.setdefaulttimeout(2.0)
                self.ssh.connect(SSH_HOST, port=SSH_PORT, username=SSH_USERNAME, look_for_keys=True, key_filename=SSH_KEY_FILE, timeout=3)
                print("Connected to host")

                while not self.kill.is_set():
                    start = time()

                    root = ElementTree.Element("uptime")
                    seconds = ElementTree.SubElement(root, "seconds")
                    minutes = ElementTree.SubElement(root, "minutes")
                    hours = ElementTree.SubElement(root, "hours")
                    now = ElementTree.SubElement(root, "update")
                    startdate = ElementTree.SubElement(root, "start")

                    with self.c:
                        hours.text = str(self.hms[0])
                        minutes.text = str(self.hms[1])
                        seconds.text = str(self.hms[2])

                    now.text = str(strftime("%Y-%m-%d %H:%M:%S", localtime()))
                    startdate.text = self.start_date

                    xmltext = ElementTree.tostring(root).decode('utf-8')
                    stdin, stdout, stderr = self.ssh.exec_command("echo '%s' > %s" % (xmltext, UPTIME_XML), timeout=2.0)

                    outlines = [str(line) for line in stdout.readlines()]
                    errlines = [str(line) for line in stderr.readlines()]
                    if len(outlines) > 0 or len(errlines) > 0:
                        print("ERROR: failed to write data to server")
                        print("stdout: \t%s" % '\n\t'.join(outlines))
                        print("stderr: \t%s" % '\n\t'.join(errlines))

                    self.kill.wait(max(0,SSH_UPDATE_INTERVAL - (time() - start))) #Wait the rest of the time
            except socket.timeout:
                print("Connection timeout! Retry...")
            except socket.gaierror:
                print("Failed to connect to host. Retry...")

            if not self.kill.is_set():
                sleep(2.0)

        self.ssh.close()


class UptimeMonitor(Thread):
    """
    Monitors vehicle test time for the current session (since this program was started)
    """
    def __init__(self):
        Thread.__init__(self)
        self.time = 0.0 #Total test time, since the last in_water_time
        self.in_water_time = None #time at which the vehicle was put underwater

        self.kill = Event()
        self.c = Condition()

        self.w = shm.watchers.watcher()
        self.w.watch(depth_shm_group)
        self.w.watch(shm.dvl)

        self.start()

        print("Monitoring vehicle depth")

    def get_time(self):
        with self.c:
            if not self.active():
                #Vehicle is out of water
                return self.time
            else:
                #Vehicle is actively being tested
                return self.time + (time() - self.in_water_time)

    def destroy(self):
        self.kill.set()
        self.w.broadcast() #Interrupt wait

    def active(self):
        return shm.uptime.in_water.get()

    def set_active(self, value):
        shm.uptime.in_water.set(value)

    def run(self):
        while not self.kill.is_set():
            #In water if below depth threshold, or a DVL beam is unblocked (and DVL daemon is started)
            in_water = (depth_shm_group.depth.get() > DEPTH_THRESH) or ((shm.dvl.low_amp_1.get() != 1 or shm.dvl.low_amp_2.get() != 1 or shm.dvl.low_amp_3.get() != 1 or shm.dvl.low_amp_4.get() != 1) and shm.dvl.tick.get() != 0)
            with self.c:
                if in_water and not self.active():
                    #Switch to active mode
                    self.set_active(True)
                    self.in_water_time = time()
                if (not in_water) and self.active():
                    #Switch to inactive mode
                    self.set_active(False)
                    self.time += (time() - self.in_water_time)
            self.w.wait()

####

class SqlWriter:
    """
    Manages the sqlite uptime database
    Stores a single table containing rows corresponding to instances this program was run
    """
    def __init__(self):
        #Connect to sqlite database
        self.conn = sqlite3.connect(DB_FILE, check_same_thread = False, detect_types=sqlite3.PARSE_DECLTYPES|sqlite3.PARSE_COLNAMES)
        self.c = self.conn.cursor()

        #Initialize table
        self.c.execute("""CREATE TABLE IF NOT EXISTS utlog (
                            id              INTEGER PRIMARY KEY,
                            start           TIMESTAMP,
                            end             TIMESTAMP,
                            time            REAL
                          )""")

        #Create this instance's row
        self.c.execute("INSERT INTO utlog (start, end, time) VALUES (?,?,?)", (datetime.now(), datetime.now(), 0.0))
        self.id = self.c.lastrowid

        self.conn.commit()

    def get_total_uptime(self): #Get total uptime, not including this instance
        self.c.execute("SELECT SUM(time) FROM utlog WHERE id<>?", (self.id,))
        data = self.c.fetchone()
        if data is None or len(data) == 0 or data[0] is None:
            return 0.0
        else:
            return data[0]

    def get_start_date(self): #Get earliest date in database (date vehicle hit water)
        self.c.execute('SELECT MIN(start) AS "ts [timestamp]" FROM utlog')
        data = self.c.fetchone()
        if data is None or len(data) == 0 or data[0] is None:
            return datetime.now()
        else:
            return data[0]

    def set_uptime(self, seconds): #Set uptime for the current instance
        self.c.execute("UPDATE utlog SET end=?, time=? WHERE id=?", (datetime.now(), seconds, self.id))
        self.conn.commit()

###

monitor = UptimeMonitor()
monitor.set_active(False)
writer = SqlWriter()
updater = Updater()

killed = Event() #Kill signal

previous_time = writer.get_total_uptime() #total test time in previous instances of this program
start_date = writer.get_start_date().strftime("%b %d, %Y") #first date we have data in our db
updater.set_start_date(start_date)

def handler(signum, frame):
    print("INTERRUPT")
    updater.destroy()
    monitor.destroy()
    killed.set()
signal.signal(signal.SIGTERM, handler)
signal.signal(signal.SIGINT, handler)

while not killed.is_set():
    secs = monitor.get_time()
    updater.set(previous_time + secs)
    writer.set_uptime(secs)
    killed.wait(SQL_UPDATE_INTERVAL)

print("Bye!")
