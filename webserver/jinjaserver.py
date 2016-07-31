#!/usr/bin/env python2
import sys
import importlib
import mimetypes
import shm
from self_test.selftestengine import SelfTestEngine
from self_test.actuator_test import ActuatorTest
#from self_test.thruster_test import *
from flask import *

# Change default encoding to UTF-8
# We need to reload sys module first, because setdefaultencoding is available
# only at startup time
#importlib.reload(sys) 
#sys.setdefaultencoding('utf-8')
app = Flask(__name__)

# WSGI function that handles HTTP requests to our application
@app.route('/')
def application():

    templateVars = {
            "depth" : shm.depth.depth.get(),
            "pressure" : shm.pressure.internal.get(),
            "voltage" : shm.pod.voltage.get(),
            "pVolts" : shm.pod.voltage_port.get(),
            "sVolts" : shm.pod.voltage_starboard.get(),
            "current" : shm.pod.current.get(),
            "currentPort" : shm.pod.current_port.get(),
            "currentStarboard" : shm.pod.current_starboard.get(),
            "aftPortStat" : shm.motor_status.status_aft_port.get(),
            "aftStarboardStat" : shm.motor_status.status_aft_starboard.get(),
            "forePortStat" : shm.motor_status.status_fore_port.get(),
            "foreStarboardStat" : shm.motor_status.status_fore_starboard.get(),
            "portStat" : shm.motor_status.status_port.get(),
            "starboardStat" : shm.motor_status.status_starboard.get(),
            "swayAftStat" : shm.motor_status.status_sway_aft.get(),
            "swayForeStat" : shm.motor_status.status_sway_fore.get(),
            "STATIC_URL": "/static/"
            }
    # Load template based on URL address
    # PATH_INFO contains information on the webpage address requested by the user
        
    return render_template('/site_template.html', **templateVars)

@app.route('/self-test')
def selfTest():
    # self_test.thruster_test()
    SelfTestEngine().run_tests()
    return "should have run test" 
   # self_test.thruster_test()
   # run tests and put results into json

    print("in self test function")
    testResults = list(SelfTestEngine().run_tests())
    print("testResults are:"+str(testResults))
    return json.dumps({"results": testResults})
@app.route('/actuator-test')
def actuatorTest():
    ActuatorTest.run_test()
    return "Actuator Test Completed"
@app.route('/sway-right')
def swayRight():
    curr = shm.desires.sway_speed.get()
    shm.navigation_desires.sway_speed.set(0.5+curr)
@app.route('/sway-left')
def swayLeft():
    curr = shm.desires.sway_speed.get()
    shm.navigation_desires.sway_speed.set(-0.5+curr)
@app.route('/go-forward')
def goForward():
    curr = shm.desires.speed.get()
    shm.navigation_desires.speed.set(1.0+curr)
@app.route('/go-backward')
def goBackward():
    curr = shm.desires.speed.get()
    shm.navigation_desires.speed.set(-1.0+curr)
@app.route('/zero-out-controls')
def zero():
    shm.navigation_desires.speed.set(0)
    shm.navigation_desires.sway_speed.set(0)
    # Heading should stay the same
@app.route('/soft-kill')
def softkill():
    shm.switches.set(1)
@app.route('/unkill')
def unSoftKill():
    shm.switches.set(0)
@app.route('/update-shm')
def shm_updates():
    d = {
            "#depth" : "Depth: {} m".format(shm.depth.depth.get()),
            "#pressure" :"Pressure: {} psi".format(shm.pressure.internal.get()),
            "#voltage" : "Voltage: {} Volts".format(shm.pod.voltage.get()),
            "#voltage-port" : "Voltage Port: {} Volts".format(shm.pod.voltage_port.get()),
            "#voltage-starboard" : "Voltage Starboard: {} Volts".format(shm.pod.voltage_starboard.get()),
            "#current" : "Current {} Amps".format(shm.pod.current.get()),
            "#current-port" : "Current Port: {} Amps".format(shm.pod.current_port.get()),
            "#current-starboard" : "Current Starboard: {} Amps".format(shm.pod.current_starboard.get()),
            }
    return json.dumps(d)

    
# If you want to run a standalone HTTP server
# instead of configuring a WSGI proxy (for instance Apache)...
if __name__ == '__main__':
    app.run(debug=True,host='0.0.0.0')
