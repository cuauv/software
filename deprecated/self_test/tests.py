from time import time,sleep
import shm
'''

This file contains all of our shared variable test functions

A function in this file must take a parameter called "var" which is an instance of SharedVar.
It also can be passed parameters from the config file by name, which should default to None.
Be warned, these all these parameters are strings by default.

If a function returns True, the test passes. If it returns False, or a string, the tests fails.
Any string returns will be printed as an error message.

'''


"""Checks if a variable changes within a specified timeout interval"""
def var_changing(var=None):
    TIMEOUT = 0.5 #time to wait for update
    
    start_v = var.get()

    t_start = time()
    while (time()-t_start <= TIMEOUT):
         if(var.get() != start_v):
             return True
         sleep(0.09)
    return "Variable is not changing"

"""Checks if a variable's group is being written to within a specified time interval"""
def group_updating(var=None):
    TIMEOUT = 0.5 #time to wait for update
    
    grp = eval(var.__module__)
    w = shm.watchers.watcher()
    w.watch(grp)

    t_start = time()
    while (time()-t_start <= TIMEOUT):
         if(w.has_changed()==1):
             return True
         sleep(0.09)
    return "Variable's group is not updating"


"""Checks if a variable is within the specified range"""
def in_range(var=None, max_val=None, min_val=None):
    v = float(var.get())
    minv = float(min_val)
    maxv = float(max_val)
    if(v<minv): return "Value " + str(v) + " below threshold"
    if(v>maxv): return "Value " + str(v) + " above threshold"
    return True

"""Checks that a varaible's value is not equal to the input"""
def not_equal(var=None, value=None):
    v = float(var.get())
    vt = float(value)
    if (v==vt): return "Value was equal to " + str(vt)
    return True

def pressure_check(var=None, high_val=None, low_val=None):
    v = float(var.get())
    high_val = float(high_val)
    low_val = float(low_val)
    if v > high_val:
        return "warn: Pressure is high (" + str(round(v,1)) + " psi)"
    if v < low_val:
        return "warn: Pressure is low (" + str(round(v,1)) + " psi)"
    return True
