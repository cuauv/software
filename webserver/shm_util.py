#Shared memory interface for webserver

import shm

shmvars={}

file = open("vars.txt").readlines()
for line in file:
    if "," in line:
        k,v=line.split(",")
        evalS = v.strip() 
        shmvars[k.strip()]=shm._eval(evalS)


def v_get(name):
    if name in shmvars:

        value = shmvars[name].get()

        return value
    else:
        return None


def v_set(name, val):
    if(val!=""):
        try:
            shmvars[name].set(val)
        except:
            shmvars[name].set(eval(val))
        
def kv(key):
            if key in shmvars:
                return key+"&"+str(v_get(key))
            else:
                return ""

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False
