#!/usr/bin/env python2
import cgi
import cgitb

import shm

from self_test.thruster_test import test_motor_dockside
cgitb.enable ()
print "Content-type: text/html"
print '<link rel ="stylesheet" href="./css/style.css">'
print
print "<head></head>"
print "<body>"
print "<title>Test CGI</title>"
print "<p>Info About Argo!</p>"
if shm.switches.soft_kill:
    print "<h2>Sub is Softkilled</h2>"
# should then have a button to un soft-kill the sub
print '<div id="dP">'
print "<p>Depth: {} m</p>".format(shm.depth.depth.get())
print "<p>Pressure: {} psi</p>".format(shm.pressure.internal.get())
print "</div>"
# add button to run thruster test
#"<form action="/
print '<div class="volts">'
print "<p>Voltage: {} Volts</p>".format(shm.pod.voltage.get())
print "<p>Voltage Port: {} Volts</p>".format(shm.pod.voltage_port.get())
print "<p>Voltage Starboard: {} Volts</p>".format(shm.pod.voltage_starboard.get())
print "</div>" # end volts div
print '<div class ="current">'
print "<p>Current: {} Amps</p>".format(shm.pod.current.get())
print "<p>Current Port: {} Amps</p>".format(shm.pod.current_port.get())
print "<p>Current Starboard: {} Amps</p>".format(shm.pod.current_starboard.get())
print "</div>" # end current div

# custom error "holy shit gpio disconnected"
# few buttons for specified speeds for the thrusters
# run thrusters individually or all as a group
form = cgi.FieldStorage()
formInfo = form.getvalue("thrustTest",False) # default bool value is false
if form.getvalue("thrust"):
    subject = form.getvalue('thrust')
    if form.getvalue("thrust") == 'on':
        test_motor_dockside(speed=2.4)
else:
    subject = "not set"
print "<p> Run Thruster Test: {} </p>".format(subject)
#if "thrustTest" not in form:
#    print "<H1>Error</H1>"
print """
<p>

<form method="post" action="test_cgi.py">
    <input type ="radio" name="thrust" value"run">Run Thruster Test
    <br>
    <input type="radio" name="thrust" value="off" checked>Do not run test
    <input type="submit" value="Submit">
</form>
"""
print "<p>name:", formInfo,"</p>"
#print '<input type="submit" value="Button" ui="button huge">'
# the status numbers indicate 0: no connection, 1: ok, 2: invalid device
print "<h1>Motor Status</h1>"
print "<p> The status numbers indicate 0: no connection, 1: ok, 2: invalid device </p>"
print "<p>aft port: {}</p>".format(shm.motor_status.status_aft_port.get())
print "<p>aft starboard: {}</p>".format(shm.motor_status.status_aft_starboard.get())
print "<p>fore port: {}</p>".format(shm.motor_status.status_fore_port.get())
print "<p>fore starboard: {}</p>".format(shm.motor_status.status_fore_starboard.get())
print "<p>port: {}</p>".format(shm.motor_status.status_port.get())
print "<p>starboard: {}</p>".format(shm.motor_status.status_starboard.get())
print "<p>sway aft: {}</p>".format(shm.motor_status.status_sway_aft.get())
print "<p>sway fore: {}</p>".format(shm.motor_status.status_sway_fore.get())


print "</body>"

