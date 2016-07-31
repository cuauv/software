'''
vehicle.py

Initializes vehicle, maintains the physical model, and interfaces with shared
variables.

'''
import math
import numpy as np

# Panda3D modules
from direct.showbase.ShowBase import ShowBase
from direct.task import Task
from panda3d.core import Mat4
from panda3d.core import NodePath
from panda3d.core import PerspectiveLens
from panda3d.core import LRotationf, LVector3f
from panda3d.physics import ActorNode
from panda3d.physics import ForceNode
from pandac.PandaModules import AngularVectorForce
from pandac.PandaModules import LinearVectorForce

# CUAUV modules
import shm
from auv_math import quat
from control import vehicle
from control.thruster_manager import ThrusterManager

#TODO: These two imports avoid code duplication, but aren't great
import mission.vector as vector
import mission.helper_functions as helpers

# Simulator modules
from camera import Camera
from selectablenode import SelectableNode

from time import time
from random import random

MODEL_PATH = 'tachyon.bam'
MASS = 34 # Vehicle mass, in kilograms.

svSoftKill = shm.switches.soft_kill
svDepth = shm.depth.depth
svHeadingInt = shm.linear_heading.heading

svDvlDmgNorth = shm.dvl.dmg_north
svDvlDmgEast = shm.dvl.dmg_east
svAltitude = shm.dvl.savg_altitude

class Vehicle(ActorNode):

    def __init__(self, parent = None):
        '''
        Create a new Vehicle node. Physics should be initialized before any
        instances of Vehicle are created.

        arguments:
          parent -- A PandaNode for the vehicle to attach to. Default is None,
                    in which case the Vehicle should be added to the scene
                    graph via NodePath.attachNewNode().

        '''
        ActorNode.__init__(self, 'VehiclePhysics')
        base.physicsMgr.attachPhysicalNode(self)

        self.getPhysicsObject().setMass(MASS)
        if parent:
            self.myPath = parent.attachNewNode(self)
        else:
            self.myPath = NodePath(self)

        # Load vehicle model and place in the transparent bin.
        vehicleModel = loader.loadModel(MODEL_PATH)
        hull = vehicleModel.find('**/Hull')
        hull.setBin('transparent', 30)
        pwnsEnclosure = vehicleModel.find('**/Pwns_Enclosure')
        pwnsEnclosure.setBin('transparent', 30)
        self.myPath.setPos(0, 0, -0.0)
        selectable = self.myPath.attachNewNode(SelectableNode('vehicle sel'))
        vehicleModel.reparentTo(selectable)
       
        # ==== Initialize Physics ==== #
        thrusterForceNode = ForceNode('ThrusterForce')
        self.myPath.attachNewNode(thrusterForceNode)

        self.linearForce = LinearVectorForce(0, 0, 0)
        self.linearForce.setMassDependent(1)
        self.angularForce = AngularVectorForce(0, 0, 0)

        thrusterForceNode.addForce(self.linearForce)
        thrusterForceNode.addForce(self.angularForce)

        self.getPhysical(0).addLinearForce(self.linearForce)
        self.getPhysical(0).addAngularForce(self.angularForce)
         
        self.previousXY = (self.myPath.getX(), self.myPath.getY())

        self.tm = ThrusterManager()
        
        # Add self.updatePhysics to the task manager and run this task as
        # frequently as possible.
        self.updatePhysicsTask = taskMgr.add(self.updatePhysics,
                'UpdatePhysics')
        
        # ==== Initialize Cameras ==== #
        lens = PerspectiveLens()
        lens.setNearFar(0.05, 100.0)
        #Use either FocalLength or Fov. Fov ~40 is about what actual forward cameras are
        #lens.setFocalLength(0.8)
        lens.setFov(70,70)
        camera = Camera("Forward_left", lens).getPath()
        camera.reparentTo(vehicleModel.find('**/Forward_Camera'))
        camera.setX(camera.getX() - 0.1) # Forward cameras 20cm apart
        camera.setY(camera.getY() + 0.05) # Move in front of torpedo tubes to avoid abstruction
        camera.setHpr(0, 0, 0)

        camera = Camera("Forward_right", lens).getPath()
        camera.reparentTo(vehicleModel.find('**/Forward_Camera'))
        camera.setX(camera.getX() + 0.1) # Forward cameras 20cm apart
        camera.setY(camera.getY() + 0.05) # Move in front of torpedo tubes to avoid abstruction
        camera.setHpr(0, 0, 0)

        lens = PerspectiveLens()
        lens.setNearFar(0.05, 100.0)
        lens.setFocalLength(0.8)
        camera = Camera("Downward", lens).getPath()
        camera.reparentTo(vehicleModel.find('**/Downward_Camera'))
        camera.setHpr(0, -90, 0)

        #Layout link (to access hydrophone information)
        self.layout = None

        #Hydrophone variables
        self.start_time = time()
        self.last_hydro_update = time()

    def setLayout(self, layout):
        #Add a link to the layout to allow for referencing other objects
        #This is necessary for the hydrophone addition
        self.layout = layout

    def getDepth(self):
        ''' Returns the depth of the vehicle, in meters. '''
        return -0.15 - self.myPath.getZ()

    def getHeading(self):
        ''' Returns the heading of the vehicle, in clockwise degrees. '''
        # Panda uses counter-clockwise degrees, with the range (-180, 180].
        heading = self.myPath.getH()
        if heading < 0:
            return -heading
        elif heading > 0:
            return 360 - heading
        else:
            return 0

    def updatePhysics(self, task):
        '''
        Use the motor PWM values calculated by the controller to apply forces
        to the simulated vehicle.

        This runs at every frame, so it needs to complete quickly.

        '''
        outputs = shm.kalman.get()
        self.tm.update(outputs)
        passive_wrench = vehicle.passive_forces(outputs, self.tm)
        passive_forces, passive_torques = passive_wrench[:3], \
                                          passive_wrench[3:]

        # Get motor thrusts
        thrusts = np.array(self.tm.get_thrusts())

        # Add passive forces and torques to that produced by thrusters,
        # converting them to sub space first.
        force = self.tm.total_thrust(thrusts) + \
                self.tm.orientation.conjugate() * passive_forces
        torque = self.tm.total_torque(thrusts) + \
                self.tm.orientation.conjugate() * passive_torques

        # Finally apply forces and torques to the model
        # we also need to account for panda3d's strange coordinate system
        # (x and y are flipped and z points up (instead of down))
        self.linearForce.setVector(force_subspace[1], \
                                   force_subspace[0], \
                                  -force_subspace[2])

        # We're supposed to use axis angle here, but I'm being sneaky
        # and using the torque vector directly, i.e. non normalized axis angle
        # with the hopes that this LRotationf constructor will figure it out
        self.angularForce.setQuat(\
            LRotationf(LVector3f(torque_subspace[1], \
                                 torque_subspace[0], \
                                -torque_subspace[2]), 1))


        # Update shared variables for controller
        outputs.heading = self.getHeading()
        outputs.pitch = self.myPath.getP()
        outputs.roll = self.myPath.getR()

        # This velocity is in world space
        # We need to put it into THRUST CONVENTION SPACE
        # which we assume kalman outputs in...
        velocity = self.getPhysicsObject().getVelocity()

        # Bring the velocity into THRUST CONVENTION SPACE
        # Don't forget to account for panda's coordinate system
        velocity = self.tm.heading_quat.conjugate() * \
                  np.array((velocity.getY(), velocity.getX(), -velocity.getZ()))

        outputs.velx = velocity[0]
        outputs.vely = velocity[1]
        outputs.depth_rate = velocity[2]

        outputs.depth = self.getDepth()
        outputs.north = self.myPath.getY()
        outputs.east = self.myPath.getX()

        dX = self.myPath.getX() - self.previousXY[0]
        dY = self.myPath.getY() - self.previousXY[1]

        # Forward and sway are in THRUST CONVENTION SPACE
        # don't forget to account for panda's coordinate system
        dF, dS, dD = self.tm.heading_quat.conjugate() * np.array((dY, dX, 0.0))

        outputs.forward += dF
        outputs.sway += dS

        # Output some quaternions, accounting for Panda's coordinate system
        outputs.q0, outputs.q2, outputs.q1, outputs.q3 = self.myPath.getQuat()
        outputs.q3 *= -1.0

        shm.kalman.set(outputs)

        svHeadingInt.set(self.getHeading())
        svDepth.set(self.getDepth())
        #XXX: Approximate altitude assuming that the pool is 12 feet deep
        svAltitude.set(3.6-self.getDepth())
        svDvlDmgNorth.set(self.myPath.getY())
        svDvlDmgEast.set(self.myPath.getX())

        self.previousXY = (self.myPath.getX(), self.myPath.getY()) #update

        self.output_hydro_data()

        return Task.cont


    def output_hydro_data(self):
        #Update simulated hydrophone values
        pingers = [] #Get all pingers from the layout
        for element in self.layout.elements:
            if element.getTypeName() == "Pinger":
                pingers.append(element)

        HYDRO_TICK_PERIOD = 1

        if time() - self.last_hydro_update > HYDRO_TICK_PERIOD:
            dt = time() - self.last_hydro_update
            self.last_hydro_update = time()
            
            if shm.hydrophones_settings.dsp_mode.get() == 1: #Search mode
                #Incr search count
                shm.hydrophones_results.search_count.set(shm.hydrophones_results.search_count.get() + 1)

                #Generate proper "hydrophone bins" marks
                sb = 0
                for p in pingers:
                    f = p.pinger_frequency
                    dc = (f - (shm.hydrophones_settings.searchCenter.get() - shm.hydrophones_settings.searchDelta.get())) / shm.hydrophones_settings.searchStep.get() + 0.5
                    sb |= 1 << int(dc)
                shm.hydrophones_results.search_bins.set(sb)

            else: #Track Mode
                #Incr ping count
                shm.hydrophones_results.ping_count.set(shm.hydrophones_results.ping_count.get() + 1)
                
                #Determine which pinger we are actively tracking (within 0.7khz of target)
                targetp = None
                for p in pingers:
                    if abs(shm.hydrophones_settings.trackFrequency.get() - p.pinger_frequency) < 700:
                        targetp = p

                if targetp is not None:
                    shm.hydrophones_results.intensity.set(int(shm.hydrophones_settings.trackMagThresh.get() + 1e4 * random()))
                    shm.hydrophones_results.ping_time.set(int(dt * 1000))

                    pp = targetp.path.getPos()
                    
                    vv = vector.Vector(self.myPath.getY(), self.myPath.getX())
                    pv = vector.Vector(pp.getY(), pp.getX())
                    
                    #heading
                    dv = pv - vv
                    ang = vector.GetAuvAngle(dv)
                    hdiff = helpers.heading_diff(self.getHeading(), ang)

                    shm.hydrophones_results.heading.set(hdiff)

                    #elevation
                    dh = self.myPath.getZ() - pp.getZ()
                    dist = vector.Length(dv)
                    elev = math.degrees(math.atan2(dist, dh))
                    elev = min(elev, 90)

                    shm.hydrophones_results.elevation.set(elev)

                    #phase calculations 
                    dy = self.myPath.getY() - pp.getY()
                    dx = self.myPath.getX() - pp.getX()
                    yang = math.degrees(math.atan2(dist, dy))
                    xang = math.degrees(math.atan2(dist, dx))
                    
                    shm.hydrophones_results.phaseX.set((90.0 - xang) / 90.0)
                    shm.hydrophones_results.phaseY.set((90.0 - yang) / 90.0)
                    shm.hydrophones_results.phaseZ.set((90.0 - elev) / 90.0)

                else:
                    shm.hydrophones_results.heading.set(0)
                    shm.hydrophones_results.elevation.set(0)
                    shm.hydrophones_results.intensity.set(0)
                    shm.hydrophones_results.ping_time.set(0)

            shm.hydrophones_results.uptime.set(int(time() - self.start_time))

    def __del__(self):
        ''' Remove update tasks from the panda task manager. '''
        taskMgr.remove(self.updatePhysicsTask)
        ActorNode.__del__(self)
