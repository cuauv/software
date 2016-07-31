

from direct.showbase.DirectObject import DirectObject
from panda3d.core import KeyboardButton
from panda3d.core import MouseButton
from panda3d.core import NodePath
from panda3d.core import Point3
from panda3d.core import Vec4

from mouseevent import MouseEventListener
from selectionengine import SelectionEngine
from handles import Handle

from cameracontroller import CameraController, EVT_CAMERA_MODE, TRACKBALL

class SelectionManager(DirectObject, MouseEventListener):
    defaultMgr = None

    @classmethod
    def getDefault(cls):
        if cls.defaultMgr == None:
            cls.defaultMgr = SelectionManager()
        return cls.defaultMgr

    @classmethod
    def setDefault(cls, manager):
        cls.defaultMgr = manager

    def __init__(self, selectionEngine = None):
        self.selection = []
        self.enabled = False
        self.editMode = None
        if selectionEngine == None:
            selectionEngine = SelectionEngine.getDefault()
        self.engine = selectionEngine
        self.engine.addMouseListener(self)
        self.handle = Handle()
        self.handle.setClients([])
        render.attachNewNode(self.handle)
        CameraController.getInstance().addEventHandler(EVT_CAMERA_MODE,
                self._cameraModeHandler)
        self.accept('f', self._setFocus)

    def getSelectionCenter(self):
        if not self.selection:
            return Point3()
        else:
            min, max = Point3(), Point3()
            tmpmin, tmpmax = Point3(), Point3()
            np = NodePath(self.selection[0])
            np.calcTightBounds(min, max)
            min += np.getPos(render) - np.getPos()
            max += np.getPos(render) - np.getPos()
            for i in xrange(1, len(self.selection)):
                np = NodePath(self.selection[i])
                np.calcTightBounds(tmpmin, tmpmax)
                if np.getParent() != render:
                    tmpmin += np.getPos(render) - np.getPos()
                    tmpmax += np.getPos(render) - np.getPos()
                min = min.fmin(tmpmin)
                max = max.fmax(tmpmax)
            return Point3(min + (max - min)/2)

    def _setFocus(self):
        # This function handles presses of the F key.
        if self.selection:
            CameraController.getInstance().setFocus(self.getSelectionCenter())
        else:
            CameraController.getInstance().setFocus(Point3())

    def _cameraModeHandler(self, cameraController):
        if cameraController.getCameraMode() == TRACKBALL:
            self.enable(True)
        else:
            self.enable(False)

    def enable(self, enabled=True):
        if self.enabled and enabled == False:
            self.deselectAll()
            self.handle.setClients([])

        self.enabled = enabled

    def registerNode(self, node):
        print 'registering new node to selmgr', node
        node.addMouseListener(self)

    def removeNode(self, node):
        node.removeMouseListener(self)
        if node in self.selection:
            node.setSelected(False)
            self.selection.remove(node)
    
    def deselectAll(self):
        for node in self.selection:
            node.setSelected(False)
        self.selection = []

    def _setEditMode(self, editMode):
        if self.editMode == editMode:
            return
        self.editMode = editMode
        if editMode:
            self.engine.setHighlightColor(Vec4(1, 0.8, 0, 1))
        else:
            self.engine.setHighlightColor(Vec4(0.6, 0.6, 1, 1))
        self.deselectAll()

    def mousePressed(self, event):
        print 'got mouse pressed event from', event.sender
        if (not self.enabled or
                event.modifiers.isDown(KeyboardButton.control())):
            print 'short circuiting'
            return

        shiftDown = event.modifiers.isDown(KeyboardButton.shift())
        if event.sender == self.engine:
            if not shiftDown:
                self.deselectAll()
        else: 
            self._setEditMode(event.modifiers.isDown(MouseButton.three()))
            node = event.sender
            if shiftDown:
                # Shift-clicking a node toggles its selected state.
                if node.isSelected():
                    self.selection.remove(node)
                    node.setSelected(False)
                else:
                    self.selection.append(node)
                    node.setSelected(True)
            elif len(self.selection) == 1 and node.isSelected():
                # This is already the only node selected.
                return
            else:
                print 'selecting', node
                self.deselectAll()
                node.setSelected(True)
                self.selection.append(node)
        if self.editMode:
            self.handle.setClients([NodePath(n) for n in self.selection],
                    self.getSelectionCenter())
        else:
            self.handle.setClients([])

