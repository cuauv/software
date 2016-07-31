from shm import *
import time
import os
import urllib

import commands
import socket
import sys

MCAST_GRP = '224.1.1.1'
MCAST_PORT = 5012

# Batteries
port_percent = pod_port.percent
starboard_percent = pod_starboard.percent
port_id = pod_port.board_id
starboard_id = pod_starboard.board_id
port_voltage = pod_port.voltage
starboard_voltage = pod_starboard.voltage

#Temporary
def get_starboard_voltage():
    return pod_starboard.cell1_v.get() + pod_starboard.cell2_v.get() + pod_starboard.cell3_v.get() + pod_starboard.cell4_v.get() + pod_starboard.cell_v5.get() + pod_starboard.cell_v6.get()


#Percentage that is considered "critical battery level"
criticalState = 18

#Name of the router to search for
routerName = "dd-wrt"

#Multicast message
def sendAlert(message):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
    sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 2)
    sock.sendto(message, (MCAST_GRP, MCAST_PORT))

# = people are on the sub and can receive terminal output.
# no point of putting something to the wall if nobody can see it
def who():
    s = commands.getstatusoutput("who")
    return "pts/" in str(s)

#The sub is currently connected to the pool test router
#Avoids alerts while in the lab
def poolRouter():
    try:
        routerpage = urllib.urlopen("http://192.168.0.1")
    except:
        return True #if we can't find the router, we're probably on some sort of static setup
    for lines in routerpage.readlines():
        if routerName in lines:
            return True
    return False



#average percent drops below criticalState or one battery drops to 3/5 of the criticalState
def isCritical():
    return ((pp + sp) < (criticalState*2)) or (pp <= (3.0 * criticalState / 5.0)) or (sp <= (3.0 * criticalState / 5.0))

def initAlert():
    if not poolRouter():
        return
    pps = port_percent.get()
    sps = starboard_percent.get()
    m = ""
    m = m + "**********************" + "\n"
    m = m + "Critical Battery Alert" + "\n"
    m = m + "Port: " +  str(pps) + "%" + (" <--","")[pps>sps] + "\n"
    m = m + "Starboard: " + str(sps) + "%" + (" <--","")[sps>pps]  + "\n" 
    m = m + "**********************" + "\n"
    sendAlert("Vehicle batteries are low (Port: " + str(pps) + "%, Starboard: " + str(sps) + "%)")
    os.system("echo \"" + m + "\" | wall")


def missingAlert():
    if not poolRouter():
        return
    return
    m = ""
    m = m + "**********************" + "\n"
    m = m + "Critical Battery Alert" + "\n"
    m = m + "  - Battery  Error -  " + "\n"
    m = m + "  -Check healthdash-  " + "\n" 
    m = m + "**********************" + "\n"
    os.system("echo \"" + m + "\" | wall")

def voltageAlert():
    if not poolRouter():
        return
    pps = port_percent.get()
    sps = starboard_percent.get()
    m = ""
    m = m + "**********************" + "\n"
    m = m + "Critical Battery Alert" + "\n"
    m = m + "   - Voltage Low  -  " + "\n"
    m = m + "**********************" + "\n"
    os.system("echo \"" + m + "\" | wall")
    sendAlert("Battery voltage is low (Port: " + str(pps) + "%, Starboard: " + str(sps) + "%)")



def calibrateAlert():
    if not poolRouter():
        return
    return
    m = ""
    m = m + "****************************" + "\n"
    m = m + " - Critical Battery Alert - " + "\n"
    m = m + "  Batteries not calibrated  " + "\n"
    m = m + "Alert will not work properly  " + "\n" 
    m = m + "****************************" + "\n"
    os.system("echo \"" + m + "\" | wall")


missingCount = 0
lowstate_f = False

calib_alerted = False
volt_alerted = False
error_alerted = False

while True:
    pp = int(port_percent.get())
    sp = int(starboard_percent.get())
    pid = int(port_id.get())
    sid = int(starboard_id.get())
    
    pv = float(port_voltage.get())
    #sv = float(starboard_voltage.get())
    sv = get_starboard_voltage()
    
    #Prevents battery alerts when power is external or missing
    if pid == 255 or pid == 0:
        pp = 100
    if sid == 255 or sid == 0:
        sp = 100 

    #Voltage alert
    if (pp != 100 and pv < 21.9) or (sp != 100 and sv < 21.9):
        if not volt_alerted: voltageAlert()
        volt_alerted = True
        time.sleep(150)
    else:
        volt_alerted = False


    #Checks for batteries that are missing batteries
    if ((pid == 0 and (not(sid==255))) or (sid == 0 and (not(pid==255)))) and who():
        if missingCount == 3: #only alerts if batteries are missing over a long period (to avoid being triggered by shared memory resets)
            missingCount = 0
            if not error_alerted: missingAlert()
            error_alerted = True
            time.sleep(120)
        else:
            missingCount += 1
    else:
        error_alerted = False


    #Checks for failed calibration
    if (sp > 100 or pp > 100) and who():
        if not calib_alerted:
            calib_alerted = True
            calibrateAlert()
    else:
        calib_alerted = False

    #Check critical state / toggle once it drops 
    if ((not lowstate_f) and isCritical()) and who():
        lowstate_f = True
        initAlert()
        time.sleep(120)
    if lowstate_f and (not isCritical()):
        lowstate_f = False

    time.sleep(10)
