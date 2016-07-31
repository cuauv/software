#!/usr/bin/env python2

import sys
from Tkinter import *
from tkFileDialog import askopenfilename, asksaveasfilename
from tkSimpleDialog import askinteger

from panda3d.core import loadPrcFileData

loadPrcFileData('', 'show-buffers #f')
loadPrcFileData('', 'show-frame-rate-meter #t')
loadPrcFileData('', 'sync-video #t')
loadPrcFileData('', 'window-title CUAUV Simulator')

import direct.directbase.DirectStart
# from direct.gui.DirectGui import DirectButton
# from direct.gui import DirectGuiGlobals
from direct.showbase.DirectObject import DirectObject
from direct.task.Task import Task
from panda3d.core import AntialiasAttrib
from panda3d.core import GeomNode
from panda3d.core import KeyboardButton
from panda3d.core import NodePath
from panda3d.core import Mat4
from panda3d.core import ModifierButtons
from panda3d.core import VBase3
from panda3d.physics import AngularEulerIntegrator
from pandac.PandaModules import WindowProperties

# Simulator modules
from cameracontroller import CameraController
from environment import makeEnvironment
from vehicle import Vehicle
from missionlayout import MissionLayout, MissionElement
from selectionmanager import SelectionManager
from selectionengine import SelectionEngine
from userdata import AppPreferences


def indent(text, level = 1, tab = '    '):
    text = level * tab + text
    text = text.replace('\n', '\n' + level * tab)
    return text

def geom2Str(geom):
    s = str(geom)
    s += '\n' + indent(str(geom.getVertexData()))
    # for i in xrange(geom.getNumPrimitives):
    #     prim = geom.getPrimitive(i)
    return s
        

def geomNode2Str(geomNode):
    s = str(geomNode)
    for i in xrange(geomNode.getNumGeoms()):
        s += '\n' + indent(geom2Str(geomNode.getGeom(i)))
        renderState = geomNode.getGeomState(i)
        s += '\n' + indent(str(renderState), 2)
        renderAttrib = renderState.getAttrib(0)
        for i in xrange(1, 29):
            if (renderState.hasAttrib(i)):
                s += '\n' + str(i) + indent(str(renderState.getAttrib(i)), 3)
    return s

def nodePath2Str(nodePath):
    s = str(nodePath)
    for child in nodePath.getChildren():
        s += '\n' + indent(nodePath2Str(child))
    if len(nodePath.getChildren()) == 0:
        node = nodePath.getNode(0)
        if type(node) == GeomNode:
            s += '\n' + indent(geomNode2Str(node))
        else:
            s += '\n' + indent(str(node))
    return s


def printChildren(nodePath):
    print nodePath2Str(nodePath)

def eventHandler(arg1, arg2):
    print "Handled", arg1, repr(arg2)


class World(DirectObject):
    instance = None

    def __init__(self):
        render.setAntialias(AntialiasAttrib.MAuto)

        # Enable physics - perhaps this should go someplace else, but it must
        # be done before the Vehicle is initialized.
        base.enableParticles()
        aei = AngularEulerIntegrator()
        base.physicsMgr.attachAngularIntegrator(aei)

        SelectionEngine.getDefault().enable()
        SelectionManager.getDefault().enable()
        
        # Make the environment and the vehicle model.
        makeEnvironment()
        self.vehicle = Vehicle(render)

        MissionElement.loadElementConfig('mission_elements.plist')

        #layoutName = AppPreferences.get('last_layout', 'defaultLayout.plist')
        if len(sys.argv) == 2:
            layoutName = sys.argv[1]
            print "Using command line argument %s for layout" %layoutName
        else:
            print "Using default layout file"
            print "Use ./sim2.py [layout file] to use a different layout"
            print "Or press Ctrl+O to open a new layout in the simulator"
            layoutName = 'defaultLayout.plist'

        self.layout = MissionLayout.loadLayout(layoutName)
        render.attachNewNode(self.layout)

        self.vehicle.setLayout(self.layout) #Link the layout 

        # Set up render buffer viewer, to aide debugging.
        self.accept("v", base.bufferViewer.toggleEnable)
        self.accept("V", base.bufferViewer.toggleEnable)
        base.bufferViewer.setPosition("llcorner")
        
        # Set up file saver
        self.accept('s', self._saveLayout)
        self.accept('o', self._openLayout)
        self.accept('f', self._setFreq)
        root = Tk()
        root.withdraw()
        self.modButtons = ModifierButtons()
        self.modButtons.addButton(KeyboardButton.control())
        self.accept('control', self.modButtons.buttonDown,
                [KeyboardButton.control()])
        self.accept('control-up', self.modButtons.buttonUp,
                [KeyboardButton.control()])
        # Add GUI Controls
        '''
        buttonReady = makeGeom('button_ready.png')
        b = DirectButton(geom = (buttonReady,
                                 makeGeom('button_click.png'),
                                 makeGeom('button_rollover.png'),
                                 buttonReady),
                          relief = None)
        b.reparentTo(pixel2d)
        b.hide()
        b.setPos(base.win.getXSize()/2, 0, -base.win.getYSize()/2)
        b.setScale(1, 1, 1)
        b.bind(DirectGuiGlobals.ACCEPT, eventHandler, ['accept'])
        b.bind(DirectGuiGlobals.ACCEPTFAILED, eventHandler, ['accept failed'])
        b.bind(DirectGuiGlobals.ADJUST, eventHandler, ['adjust'])
        b.bind(DirectGuiGlobals.B1CLICK, eventHandler, ['b1click'])
        b.bind(DirectGuiGlobals.B1PRESS, eventHandler, ['b1press'])
        b.bind(DirectGuiGlobals.B1RELEASE, eventHandler, ['b1release'])
        b.bind(DirectGuiGlobals.B2CLICK, eventHandler, ['b2click'])
        b.bind(DirectGuiGlobals.B2PRESS, eventHandler, ['b2press'])
        b.bind(DirectGuiGlobals.B2RELEASE, eventHandler, ['b2release'])
        b.bind(DirectGuiGlobals.B3CLICK, eventHandler, ['b3click'])
        b.bind(DirectGuiGlobals.B3PRESS, eventHandler, ['b3press'])
        b.bind(DirectGuiGlobals.B3RELEASE, eventHandler, ['b3release'])
        b.bind(DirectGuiGlobals.ENTER, eventHandler, ['enter'])
        b.bind(DirectGuiGlobals.EXIT, eventHandler, ['exit'])
        b.bind(DirectGuiGlobals.WITHIN, eventHandler, ['within'])
        b.bind(DirectGuiGlobals.WITHOUT, eventHandler, ['without'])
        b.bind(DirectGuiGlobals.CURSORMOVE, eventHandler, ['cursormove'])
        # b['frameSize'] = (3, 3, 3, 3)
        '''

    @classmethod
    def getInstance(cls):
        if cls.instance == None:
            cls.instance = World()
        return cls.instance

    def _saveLayout(self):
        if self.modButtons.isDown(KeyboardButton.control()):
            self.modButtons.buttonUp(KeyboardButton.control())
            self.modButtons.buttonUp(KeyboardButton.asciiKey('s'))
            filename = asksaveasfilename(filetypes=[('plist files', '*.plist')])
            if filename:
                self.layout.save(filename)
                AppPreferences.set('last_layout', filename)

    def _setFreq(self):
        #TODO: Check pinger selected

        pinger = None
        for element in self.layout.elements:
            if element.getTypeName() == "Pinger" and element.isSelected():
                pinger = element
                break

        if pinger is None:
            return

        newfreq = askinteger("Set Pinger Frequency", "Set this pinger to which frequency? (in Hz)", initialvalue=pinger.pinger_frequency, minvalue=10000, maxvalue=50000)
        if newfreq is None:
            print "No frequency specified, aborting"
            return

        pinger.pinger_frequency = newfreq
        print "Frequency of pinger set to %d Hz" % newfreq


    def _openLayout(self):
        if self.modButtons.isDown(KeyboardButton.control()):
            self.modButtons.buttonUp(KeyboardButton.control())
            self.modButtons.buttonUp(KeyboardButton.asciiKey('o'))
            filename = askopenfilename(filetypes=[('plist files', '*.plist')])
            if filename:
                if self.layout:
                    NodePath(self.layout).detachNode()
                self.layout = MissionLayout.loadLayout(filename)
                render.attachNewNode(self.layout)
                AppPreferences.set('last_layout', filename)

w = World.getInstance()
run()
