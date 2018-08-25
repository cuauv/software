#!/usr/bin/env python3

import subprocess
import shm
import time
import sys

gh = lambda: shm.switches.hard_kill.get()

args = sys.argv[1:]

subprocess.run(["rmdir", "/home/software/cuauv/workspaces/repo/mission/.mission_lock"])
p = subprocess.Popen(["/home/software/cuauv/workspaces/worktrees/master/link-stage/auv-mission-runner", *args])
h = gh()
t = time.time()
r = True

while True:
    new_h = gh()
    new_t = time.time()
    if not new_h:
        r = False
        t = new_t

    if new_t - t > 10 and not r:
        p.kill()
        subprocess.run(["rmdir", "/home/software/cuauv/workspaces/repo/mission/.mission_lock"])
        p = subprocess.Popen(["/home/software/cuauv/workspaces/worktrees/master/link-stage/auv-mission-runner", *args])
        print("restarting")
        r = True

    time.sleep(1/60)
