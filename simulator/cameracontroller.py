'''
cameracontroller.py

Defines the CameraController class, which handles camera control modes and
camera movement.

'''

# Panda3D modules
from direct.gui.OnscreenText import OnscreenText
from direct.showbase.DirectObject import DirectObject
from direct.task.Task import Task
from panda3d.core import KeyboardButton
from panda3d.core import Mat4
from panda3d.core import ModifierButtons
from panda3d.core import TextNode
from panda3d.core import Vec2
from panda3d.core import Vec3
from pandac.PandaModules import WindowProperties


# Mouselook sensitivity. Must be an integer. Pick something that feels good.
ML_SENSITIVITY = 20

# Multiplier to turn 2**16 into something divisible by 360. This ensures that
# the camera's roation will not jump when the mouse accumulators overflow.
ML_MULT = ML_SENSITIVITY * 360.0/65536.0

MOUSELOOK = 0
TRACKBALL = 1

key2MatArgs = {
    'w':    [1, 1, 1],
    'w-up': [1, 1, 0],
    's':    [1, 1, -1],
    's-up': [1, 1, 0],
    'a':    [0, 0, -1],
    'a-up': [0, 0, 0],
    'd':    [0, 0, 1],
    'd-up': [0, 0, 0],
    'e':    [2, 2, 1],
    'e-up': [2, 2, 0],
    'c':    [2, 2, -1],
    'c-up': [2, 2, 0],
}

EVT_CAMERA_MODE = 0
EVT_CAMERA_MOVE = 1

class EventDispatcher(object):
    def __init__(self):
        self.handlerTable = {}

    def addHandler(self, evtType, handler):
        entry = self.handlerTable.get(evtType, [])
        if handler not in entry:
            entry.append(handler)
            self.handlerTable[evtType] = entry

    def removeHandler(self, evtType, handler):
        entry = self.handlerTable.get(evtType, [])
        if handler in entry:
            entry.remove(handler)
            self.handlerTable[evtType] = entry

    def dispatchEvent(self, evtType, evtObject):
        for handler in self.handlerTable.get(evtType, []):
            handler(evtObject)


def addInstructions(pos, message):
    return OnscreenText(text = message, style = 1, fg = (1, 1, 1, 1),
            pos = (-1.2, pos), align = TextNode.ALeft, scale = 0.05)

class CameraController(DirectObject):
    instance = None

    def __init__(self):
        self.instructionText = addInstructions(0.95,
                '[ESC]: Leave Mouselook mode.')
        self.eventDispatcher = EventDispatcher()
        self.cameraMode = None
        self.clickPos = Vec2()
        self.lastMousePos = Vec2()
        self.focus = Vec3()
        self.mouseDown = False
        self.initialPos = Vec3()
        self.initialHpr = Vec3()
        self.initialMat = None

        # Disable the built-in mouse camera control (it sucks).
        base.disableMouse()
        self.setCameraMode(TRACKBALL)

        # Set the camera's initial position.
        base.camera.setPosHpr(0, 12, 30, 180, -70, 0)
        
        # Turn off events generated with modifier buttons, e.g. 'shift-a'
        # This is to keep keyboard control working after you alt-tab out
        # of the app.
        base.mouseWatcherNode.setModifierButtons(ModifierButtons())
        base.buttonThrowers[0].node().setModifierButtons(ModifierButtons())

        # This is a diagonal matrix that keeps track of movement key
        # state. The first three diagonal entries can be 1, 0, or -1.
        self.mouseLookTransMat = Mat4.scaleMat(Vec3(0.0, 0.0, 0.0))
        
        # Keep track of how many movement keys are currently pressed. This
        # lets us short-circuit past a lot of math when no keys are held.
        self.keysHeld = 0

        # Handle events for the movement keys.       
        for key, value in key2MatArgs.items():
            self.accept(key, self._moveKeyHandler, value)

        self.accept('escape', self._escKeyHandler)
        self.accept('m', self._mKeyHandler)
        self.accept('mouse1', self._mouseDownHandler, [1])
        self.accept('mouse1-up', self._mouseUpHandler, [1])
        self.accept('mouse2', self._mouseDownHandler, [2])
        self.accept('mouse2-up', self._mouseUpHandler, [2])
        self.accept('wheel_up', self._mouseWheelHandler, [1])
        self.accept('wheel_down', self._mouseWheelHandler, [-1])

        self.modButtons = ModifierButtons()
        self.modButtons.addButton(KeyboardButton.control())
        self.accept('control', self.modButtons.buttonDown,
                [KeyboardButton.control()])
        self.accept('control-up', self.modButtons.buttonUp,
                [KeyboardButton.control()])

        self.accept(base.win.getWindowEvent(), self._windowHandler)

    @classmethod
    def getInstance(cls):
        if cls.instance == None:
            cls.instance = cls()
        return cls.instance

    def setFocus(self, position):
        self.focus = position
        base.camera.lookAt(render, position)

    def addEventHandler(self, evtType, handler):
        self.eventDispatcher.addHandler(evtType, handler)

    def getCameraMode(self):
        return self.cameraMode

    def setCameraMode(self, mode):
        if mode == self.cameraMode:
            return

        winProps = WindowProperties()
        
        # Tear-down code for the current mode.
        if self.cameraMode == MOUSELOOK:
            winProps.setCursorHidden(False)
            self.removeTask(self.viewTask)
        elif self.cameraMode == TRACKBALL:
            self.removeTask(self.viewTask)

        # Set-up code for the new mode.
        if mode == MOUSELOOK:
            winProps.setCursorHidden(True)
            
            # Mouse accumulator. Whenever the mouse moves, it is reset to the
            # center of the window. The accumulator keeps track of the total
            # movement
            self.mouseAccX = 0
            self.mouseAccY = 0
            self.recenterMouse = True

            # Add a task that moves the camera based on mouse position.
            self.viewTask = taskMgr.add(self._mlViewTask, 'MLViewTask')
            self.lastFrameTime = 0  # Used to calculate dt in mlViewTask.
            self.instructionText.setText('[ESC]: Leave Mouselook Mode')
        
        elif mode == TRACKBALL:
            self.instructionText.setText('[m]: Enter Mouselook Mode')
            self.viewTask = taskMgr.add(self._trackballTask, 'TrackballTask')

        self.cameraMode = mode
        self.eventDispatcher.dispatchEvent(EVT_CAMERA_MODE, self)
        base.win.requestProperties(winProps)

    def _windowHandler(self, window):
        pos = aspect2d.find('a2dTopLeft').getPos()
        self.instructionText.setPos(pos.getX() + 0.05, pos.getZ() - 0.05)
        hasFocus = window.getProperties().getForeground()
        if not hasFocus:
            self.setCameraMode(TRACKBALL)

    def _escKeyHandler(self):
        self.setCameraMode(TRACKBALL)

    def _mKeyHandler(self):
        if self.cameraMode == MOUSELOOK:
            self.setCameraMode(TRACKBALL)
        else:
            self.setCameraMode(MOUSELOOK)

    def _mouseWheelHandler(self, arg):
        deltY = base.camera.getPos().length() * arg * 0.15
        transl = Mat4.translateMat(0, deltY, 0)
        base.camera.setMat(transl * base.camera.getMat())
        self.eventDispatcher.dispatchEvent(EVT_CAMERA_MOVE, base.camera)

    def _mouseDownHandler(self, button):
        if base.mouseWatcherNode.hasMouse():
            if button == 2 or self.modButtons.isDown(KeyboardButton.control()):
                self.clickPos = Vec2(base.mouseWatcherNode.getMouse())
                self.mouseDown = True
                offset = self.focus - base.camera.getPos()
                self.initialTranslation = offset.length()
                self.initialHpr = base.camera.getHpr()

    def _mouseUpHandler(self, button):
        self.mouseDown = False
            
    def _moveKeyHandler(self, *matArgs):
        self.mouseLookTransMat.setCell(*matArgs)
        
        # matArgs[2] is 0 for key release events.
        if (matArgs[2]):
            self.keysHeld += 1
        else:
            self.keysHeld -= 1

    def _trackballTask(self, task):
        if not self.mouseDown:
            return Task.cont

        if (base.mouseWatcherNode.hasMouse()):
            mpos = base.mouseWatcherNode.getMouse()
            if mpos == self.lastMousePos:
                return Task.cont

            mDelta = mpos - self.clickPos

            heading = -mDelta.x * 100 + self.initialHpr.x
            pitch = mDelta.y * 100 + self.initialHpr.y
            if pitch > 90:
                pitch = 90
            elif pitch < -90:
                pitch = -90
            trans1 = Mat4.translateMat(self.focus)
            rotx = Mat4.rotateMat(heading, Vec3(0, 0, 1))
            roty = Mat4.rotateMat(pitch, Vec3(1, 0, 0))
            trans2 = Mat4.translateMat(0, -self.initialTranslation, 0)
            rotation = trans2 * roty * rotx * trans1
            base.camera.setMat(rotation)
            self.eventDispatcher.dispatchEvent(EVT_CAMERA_MOVE, base.camera)
            
            self.lastMousePos = Vec2(mpos)
        return Task.cont

    def _mlViewTask(self, task):
        t = globalClock.getFrameTime()
        dt = t - self.lastFrameTime
        self.lastFrameTime = t
        if (self.keysHeld):
            # Update camera position based on keyboard controls.
            T = Mat4()
            T.invertAffineFrom(base.camera.getMat())
            cameraMoveMat = T*self.mouseLookTransMat
            xDir = cameraMoveMat.getCol3(0)
            yDir = cameraMoveMat.getCol3(1)
            zDir = cameraMoveMat.getCol3(2)
            currentPos = base.camera.getPos()
            nextPos = currentPos + (xDir + yDir + zDir)*(dt*8)
            base.camera.setPos(nextPos)
            self.eventDispatcher.dispatchEvent(EVT_CAMERA_MOVE, base.camera)

        if (base.mouseWatcherNode.hasMouse()):
            halfWidth = base.win.getProperties().getXSize()/2
            halfHeight = base.win.getProperties().getYSize()/2
            if self.recenterMouse:
                # Don't move the camera, but center the pointer. This is
                # useful when entering mouselook mode, to prevent the camera
                # from jumping to the pointer.
                heading, pitch = base.camera.getH(), base.camera.getP()
                self.mouseAccX = int(heading / ML_MULT)
                self.mouseAccY = int(pitch / ML_MULT)
                self.recenterMouse = False # Only do this once.
            else:
                mpos = base.mouseWatcherNode.getMouse()
                x, y = mpos.getX(), mpos.getY()
                
                # short-circuit if mouse hasn't moved or is outside the window.
                if x == 0 and y == 0 or x > 1.0 or y > 1.0:
                    return Task.cont
                # Convert x and y to pixels.
                self.mouseAccX -= int(round(x*halfWidth))
                self.mouseAccY += int(round(y*halfHeight))

                base.camera.setHpr(self.mouseAccX*ML_MULT,
                        self.mouseAccY*ML_MULT, 0.0)
            base.win.movePointer(0, halfWidth, halfHeight)
            
        return Task.cont

