#!/usr/bin/env python2

import tornado.httpserver
import tornado.ioloop
import tornado.web
import threading
import sys, traceback
import subprocess
import signal
import time
import os
from subprocess import Popen, PIPE, STDOUT
import shm
from self_test.actuator_test import ActuatorTest
from self_test.thruster_test import test_motor_dockside
from conf.vehicle import actuators
from control.util import zero_motors
from misc.actuator import fire_actuators
#from auval.debug_env import kill, lcd_pr, \
#        torpedo_left, torpedo_right, \
#        servo, \
#        marker_1, marker_2, \
#        grabber_port_grab, grabber_port_release, \
#        grabber_starboard_grab, grabber_starboard_release, \
#        grabber_aft_grab, grabber_aft_release


from shm_util import v_get, v_set, shmvars, kv #gets variables from shared memory

#server variables
webserverpass = "benath0r"
# Since we no longer run everything as the root user, we can't use port 80.
PORT = 8080

#global program variables
badstr = ""
update = False
runVision = False
updateImages = 0
downwardCam = False
self_test_data = "No Data"
# relative path to vision
VISION_PATH = "../vision"


#implementation to allow for killing threads
class KThread(threading.Thread):
        """A subclass of threading.Thread, with a kill()
        method."""
        def __init__(self, *args, **keywords):
                threading.Thread.__init__(self, *args, **keywords)
                self.killed = False

        def start(self):
                """Start the thread."""
                self.__run_backup = self.run
                self.run = self.__run # Force the Thread to install our trace.
                threading.Thread.start(self)

        def __run(self):
                """Hacked run function, which installs the
                trace."""
                sys.settrace(self.globaltrace)
                self.__run_backup()
                self.run = self.__run_backup

        def globaltrace(self, frame, why, arg):
                if why == 'call':
                        return self.localtrace
                else:
                        return None

        def localtrace(self, frame, why, arg):
                if self.killed:
                        if why == 'line':
                                raise SystemExit()
                                return self.localtrace

        def kill(self):
                self.killed = True

statusBoxID = 1

class MyLog():
    def write(self, string):
        global badstr
        global update
        global statusBoxID
        if len(string) > 1:
            badstr= string
            statusBoxID = statusBoxID + 1
        sys.stdout.write(string)

#Redirect output to the self test data
class SelfTestLog():
    def write(self, string):
        global self_test_data
        if len(string) > 1:
            self_test_data = string 
            sys.stdout.write("A self test been completed.")

#global variable for vision thread
visionBG = None


def visionHandler():
         global updateImages
         global downwardCam
         while True:
                if not downwardCam:
                         v_set("forward_cam","True")
                         while(v_get("forward_cam")=="True"):
                              pass
                else:
                         v_set("downward_cam","True")
                         while(v_get("downward_cam")=="True"):
                              pass
                time.sleep(0.1)
                updateImages = updateImages + 1
                time.sleep(0.2)

sysoutredirecter=sys.stdout;
syserrredirecter=sys.stderr;
        
def visionStart():
        global visionBG
        visionBG = Popen("auv-visiond %s/tests/web_conf" % VISION_PATH, \
                   shell=True, stdout=sysoutredirecter, stderr=syserrredirecter)


#Thruster Test Thread
myLog = MyLog()
thrusterThread = KThread(target = test_motor_dockside, kwargs = {'log': myLog, 'speed': 7.5})
#lcdPrThread = KThread(target = lcd_pr)

lcdline1 = ""
lcdline2 = ""


#Vision Thread
visionThread = KThread(target = visionHandler)


def run_self_test():

    global self_test_data
    first = True

    #from self_test.selftestengine import SelfTestEngine
    #from datetime import datetime

    html_acc = "<table><tr><td>"
    good = 0
    bad = 0

    try:
      test_output = subprocess.check_output(['auv-syscheck'])
    except subprocess.CalledProcessError as ex:
      test_output = ex.output

    html_acc += str(test_output)

    #for result in SelfTestEngine().run_tests():
    #    #handle result
    #    ret = "Test " + result['key'] + ":"
    #    html_acc_local = "<tr><td>" + ret + "</td>"
    #    if result["pass"]:
    #        if result["warn"]:
    #            html_acc_local += "<td bgcolor=YELLOW width=60 align=center><font color=black><b> WARN </b></font></td>"
    #        else:
    #            html_acc_local += "<td bgcolor=GREEN width=60 align=center><font color=white><b> PASS </b></font></td>"
    #        good += 1
    #    else:
    #        html_acc_local += "<td bgcolor=RED width=60 align=center><font color=white><b> FAIL </b></font></td>"
    #        bad += 1

    #    for f in result["messages"]:
    #        html_acc_local += "<tr><td><i><small>" + f + "</small></i></td><td></td></tr>"
    #    html_acc += html_acc_local
    ##Finish HTML printout
    #ts  = datetime.now().strftime("%c") #Timestamp

    #html_acc += "<tr><td colspan=2 align=center><font color=darkblue><b>" + str(good) + "</b> tests passed.  <b>" +  str(bad) + "</b> tests failed.</font></td></tr>"
    #html_acc += "<tr><td colspan=2 align=center><font size=2 color=maroon><i>Generated: " + ts + "<i></font></td></tr>"

    html_acc += "</td></tr></table>"

    self_test_data = html_acc
    time.sleep(0)



#Self Test Thread
self_test_thread = KThread(target=run_self_test)

def run_actuator_test():
    ActuatorTest.run_test()

actuator_test_thread = KThread(target=run_actuator_test)


class MainHandler(tornado.web.RequestHandler):  
  
    def get_data_str(self):
        global statusBoxID
        global updateImages
        global self_test_data

        """Return a list of key/value pairs formatted to be sent back to the page"""
        returnstr= ";".join(map(kv, shmvars.keys())) 
        returnstr = returnstr + ";statusbox&" + badstr + "&" + str(statusBoxID)
        returnstr = returnstr + ";images&" + str(updateImages)  
        returnstr += ";self_test&" + str(self_test_data)

        return returnstr

   # def post(self, val):
        #print self.request.body

    def post(self, val):
        """Respond to POST requests by updating POSTed values and sending values back to the page"""
        #length = int(self.headers.getheader('content-length'))        
        data_string = self.request.body
        #print "RECEIVED: %s" % data_string
        log = MyLog();
        global downwardCam
        global visionThread
        global visionBG
        global updateImages
        global runVision
        if ":" in data_string: # set a value
            k,v = data_string.split(":")
            v_set(k.strip(),v.strip())
            self.write(kv(k))
            if k.strip()=="controller":
                print >> log, "Controller set to " + v.strip()
        elif "#" in data_string: # trigger a control function
            k,v = data_string.split("#")
            actControl = v.strip()
            log = MyLog();
            if actControl in actuators:
                print >> log, "Firing %s..." % actControl
                fire_actuators([actControl])

            elif actControl == "servo_toggle":
                stat = servo.get()
                if stat:
                    print >> log, "Turning off servo"
                    servo.set(0)
                else:
                    print >> log, "Setting servo PWM to 255"
                    servo.set(255)

            elif actControl=="vision":
                if not visionThread.isAlive():                
                        print >> log, "Starting Vision..."
                        visionThread = KThread(target = visionHandler)
                        runVision = True
                        visionThread.start()
                        visionStart()
                        updateImages = 1
                else:
                        print >> log, "Vision Stopping..."
                        visionThread.kill()
                        runVision = False
                        os.kill(visionBG.pid, signal.SIGINT) #kills vision background process nicely w/ interupt
                        updateImages = updateImages + 1 #update image to show "vision not enabled" message
            elif actControl=="camtoggle":
                if downwardCam:
                        downwardCam=False
                else:
                        downwardCam=True

            elif actControl == "camlr":
                shm.camera_settings.right_camera.set(1 - shm.camera_settings.right_camera.get())


            elif actControl=="halt":
                print >> log, "Sub is now shutting down..."
                time.sleep(1)
                os.system("sudo poweroff")
            elif actControl=="reboot":
                print >> log, "Sub is now rebooting..."
                time.sleep(1)
                os.system("sudo reboot")
            elif actControl=="kill":
                shm.switches.soft_kill.set(True)
            elif actControl=="unkill":
                shm.switches.soft_kill.set(False)
        elif data_string=="refresh": # send all vars to page
            self.write(self.get_data_str())
        elif data_string=="lcd_pr": #activate LCD_pr mode 
            global lcdPrThread
            global lcdline1
            global lcdline2
            if not lcdPrThread.isAlive():
                #lcdline1 = lcd_line1.get()
                #lcdline2 = lcd_line2.get()
                lcdPrThread.start()
                print >> log, "LCD Trimming Mode Activated..."
            else:
                #kill it
                lcdPrThread.kill()
                lcdPrThread = None
                lcdPrThread = KThread(target = lcd_pr)   
                #lcd_line1.set(lcdline1)
                #lcd_line2.set(lcdline2)
                print >> log, "LCD Trimming Mode Deactivated."
            self.write("Done.")
        elif data_string=="auv_self_test": # execute a self test
                      
                #Self Test Thread
                global self_test_thread 
                global self_test_data

                self_test_data = "Running..."

                if not self_test_thread.isAlive():
                
                    self_test_thread = KThread(target=run_self_test)
                    self_test_thread.start()
                    print >> log, "Automated sensor test starting..."

        elif data_string == "actuator_test":
            global actuator_test_thread
            if not actuator_test_thread.isAlive():
                actuator_test_thread = KThread(target=run_actuator_test)
                actuator_test_thread.start()
                print >> log, "Automated actuator test starting..."


        elif data_string=="mission_start_off": # executes a thruster test
            shm.mission_start_switch.mission_start.set(False)
        elif data_string=="thruster_test": # executes a thruster test
            global thrusterThread
            if not thrusterThread.isAlive():
                #start thruster test
                thrusterThread.start()
            else:
                #kill thruster test
                thrusterThread.kill()
                thrusterThread = None
                thrusterThread = KThread(target=test_motor_dockside, kwargs = {'log': myLog, 'speed': 7.5}) #create a new KThread
                zero_motors()
                print >> log, "Thruster test aborted."
        else: # toggle the boolean **(assumes that the variable is a boolean so varname:true is not necessary)
            if data_string in shmvars:
                v_set(data_string, v_get(data_string)^ True)
                self.write(kv(data_string))
                
                if(data_string=="soft_kill"):
                    print >> log, "Soft Kill set to " + str(v_get(data_string))
                    if(str(v_get(data_string))=="True"):
                        kill() #uses debug method to also set thrusters to 0 when killed


    def get(self, pth):
        addr = self.request.remote_ip
        
        #return headers
        if pth.endswith(".html") or pth.endswith(".htm") or pth.endswith(".txt") or pth=="":  #Passes headers for html content
           self.set_header("Content-Type", "text/html")
        if pth.endswith(".png"):
            self.set_header("Content-Type", "image/png")
        if pth.endswith(".jpg"):
            self.set_header("Content-Type", "image/jpg")


        """Modified do_GET Method in order to fix slow GET request issue resulting from static IPs
            Avoids the issue by manually passing HTTP headers.
            Will transmit raw data for any file that is requested and found in the directory."""


        #checks for authorized logins
        try:
                if self.get_argument("pass")==webserverpass:
                        f = open("whitelist.txt", "a")
                        f.write(addr + "\n")
                        print "Authorized login; " + addr + " added to whitelist"
                        self.redirect("/")
                        return                
        except:
                pass


        #do page request
        data_file = open("whitelist.txt", 'r')
        read_data = data_file.read()
        if addr.startswith("192.168.") or (addr in read_data) or pth.startswith("cuauvsmall.png") or pth.startswith("bg.jpg"):
                #authorized local connection on internal network or whitelisted address & also accepts image requests for login page
                global downwardCam 
                global runVision   
                try:
                    if pth.startswith("camera.jpg"):
                        if runVision==True:
                                if not downwardCam:
                                        f = open("/ramdisk/forward.jpg", 'r')
                                        self.write(f.read())
                                       # print "GET on Forward Camera : " + self.client_address[0]
                                else:
                                        f = open("/ramdisk/downward.jpg", 'r')
                                        self.write(f.read())
                                       # print "GET on Downward Camera : " + self.client_address[0]
                        else:
                                f = open("no_vision.jpg", 'r')
                                self.write(f.read())
                    elif pth.startswith("camera.mjpeg"):
                        '''
                            Experimental MJPEG streaming code; should result in better vision frame rates
                            Does not work at this point.
                        '''
                        self.clear()
                        self.set_status(200)

                        self.set_header("User-agent", "CUAUV-mjpegstreamer")
                        self.set_header("Connection", "keep-alive")
                        self.set_header("Content-type", "multipart/x-mixed-replace;boundary=CUAUVboundary")

                        self.flush()

                        #Header Information
                        #self.write("HTTP/1.0 200 OK\r\n")
                        #self.write('User-agent: python-httplib\r\n')

                        while True: #TODO: only while the socket is active
                           
                            #Repeatedly send the images
                            self.write(tornado.escape.utf8("--CUAUVboundary\n"))
                            self.write("Content-type: image/jpeg\n")
                            self.write("\n")

                            f = open("no_vision.jpg", 'r')
                            img = f.read()

                            print "DEBUG: Sending Image of length %d" % len(img)
                            
                            self.write(img)

                            self.flush()  #TODO: Use flush callback to determine wait time

                            target_fps = 1
                            time.sleep(1.0 / target_fps)
                            break


                    else:
                        if pth == "":
                            pth = "index.html"
                        #pth = pth[1:len(pth)]
                        f = open(pth, 'r')
                        self.write(f.read())        
                        print "GET on " + pth + " : " + addr
                except:
                    xfail = "Failed GET on " + pth + " : " + addr
                    print xfail
                    traceback.print_exc(file=sys.stdout)
                #self.wfile.close()
        elif addr.startswith("128.253.") or addr.startswith("128.84.") or addr.startswith("132.236.") or addr.startswith("127.0.0.1"): #addresses correspond to CU subnets
                #internal CU connection; login required
                #pth = self.path
                f = open("login.html", 'r')
                self.write(f.read())
                print "Internal CU Request on " + pth + " - displaying login screen : " + addr
        else:
                #unauthorized external connection; deny!
                #pth = self.path                
                f = open("denied.html", 'r')
                self.write(f.read())
                print "External Request on " + pth + " - denied : " + addr


application = tornado.web.Application([
    (r"/(.*)", MainHandler),

])

if __name__ == "__main__":


    try:
        if sys.argv[1].endswith("quiet"):
            #redirects system outputs for quiet operation. 
            #Also redirects errors to an error file located in /var/log/auv/WebServer.log
            #errlog = open("/var/log/auv/WebServer.log", "w")
            sys.stdin = open(os.devnull, 'a')
            sys.stdout = open(os.devnull, 'a')
            sys.stderr = open("/var/log/auv/WebServer.log", "w")
            sysoutredirecter = open(os.devnull, 'a')
            #errlog2 = open("/var/log/auv/WebServer.log", "w")
            syserrredirecter = open("/var/log/auv/WebServer_proc_launch.log", "w") #if this line fails; switch back to null

            # kill sub on system boot; this avoids the bug where the sub is hard_killed but not soft_killed
            kill()
    except:
        pass



    #clear whitelist
    fout = open("whitelist.txt", "w")
    fout.write("")
    fout.close()

    #start server
    print "HTTP Server has now started... waiting for requests."
    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(PORT)
    io_loop = tornado.ioloop.IOLoop.instance()
    try:
        io_loop.start()
    except KeyboardInterrupt:

        #stops vision if running
        if visionThread.isAlive():                
                        print "Vision Stopping for HTTP Server shutdown..."
                        visionThread.kill()
                        runVision = False
                        os.kill(visionBG.pid, signal.SIGINT) #kills vision background process nicely w/ interupt
   
        #stops lcd_pr if running
        if  lcdPrThread.isAlive():
            #kill it
            lcdPrThread.kill()
            lcdPrThread = None
            lcdPrThread = KThread(target = lcd_pr)   
            lcd_line1.set(lcdline1)
            lcd_line2.set(lcdline2)
       
        #stops thruster test if running
        if  thrusterThread.isAlive():
                #kill thruster test
                thrusterThread.kill()
                thrusterThread = None
                thrusterThread = KThread(target=test_motor_dockside, kwargs={'log': myLog, 'speed': 7.5}) #create a new KThread
                zero_motors()        

        time.sleep(0.2) 
        #stops server
        io_loop.stop()
        print "\n\nHTTP Server is now stopping..."

    
