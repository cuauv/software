# encoding: utf-8
"""
killWatcher.py

Created by JB Rajsky on 27 March 2008

"""

import sys
from auval.watcher import *
from auval.remote_debug_env import *

dead = SharedVar("/settings/switches/soft_kill")
watcher = VarWatcher(dead)
watcher.watch_var(dead)

while(True):
    # Wait for the variable to change
    watcher.checkpoint()
    watcher.wait_for_change()
    if(dead.get()):
        # Via pyro, run the kill() function on the sub
        soft_kill.set(1)  # Does not work if not connected to sub(Obviously)
        # Don't forget to print the friendly statement from eddy...
        print "'I am dead' -Triton"
    elif(not dead.get()):
        soft_kill.set(0)
        print "I'm ALIVE!"

#
