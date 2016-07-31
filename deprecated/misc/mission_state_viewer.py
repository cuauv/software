"""
mission_state_viewer.py

Sends the mission state in human-readable format
so that mission_state_viewer_client.py can display
what the vehicle is 'thinking.'

Run this on vehicle and mission_state_viewer_client.py
on the local machine. Client must have proper host IP.
"""

import time
import socket

HOST = ''
PORT = 32212#Arbitrary port assignment
buffsize = 1024

hostsocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
hostsocket.bind( (HOST, PORT) )
hostsocket.listen(1)

from auval import shmem
from auval.shmem import SharedVar

#Shared vars which state viewer may watch.
mission_vars = { #"current_task": SharedVar("/mission/current_task/"),
                    "running_subtasks": SharedVar("/mission/running_subtasks"),
                    "task_messages": SharedVar("/mission/running_subtasks")
                    }

#Human-readable responses to the various tasks being run.
responses = {"FollowHydrophone":"Tracking pinger.",
    "AttemptToFollowPipe":"Following a pipe.",
    "FindBuoy": "Finding and ramming buoy.",
    "AttemptToFollowPipe" : "Following pipe to next mission element.",
    "FindWire" : "Looking for hedge.",
    "PassWire" : "Going over hedge.",
    "Slagathor" : "Finding bins.",
    "SonOfSlagathor" : "Finding bins.",
    "DropMarkers" : "Dropping marker into bin.",
    "None" : "No mission running."}


try:
    conn, addr = hostsocket.accept()
    print "Connection received from ", addr

    #Main output loop
    while True:
        #Read in currently running subtasks
        subtasks = mission_vars["running_subtasks"].get()
        state = {1:("None",)}
        if subtasks != "":
            state = eval(subtasks)

        #Translate tasks into messages, send them
        for n, task in state.items():
            response = responses.get(task[0], None)
            if response != None:
                conn.send(response)
        else:
            #Placeholder for when no interesting task is running.
            conn.send("Running mission.")
        time.sleep(0.25)
finally:
    hostsocket.close()
