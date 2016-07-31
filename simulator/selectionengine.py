
from direct.showbase.DirectObject import DirectObject
from direct.task.Task import Task
from panda3d.core import AntialiasAttrib
from panda3d.core import BitMask32
from panda3d.core import CullBinEnums
from panda3d.core import CullBinManager
from panda3d.core import GraphicsEngine
from panda3d.core import KeyboardButton
from panda3d.core import ModifierButtons
from panda3d.core import MouseButton
from panda3d.core import NodePath
from panda3d.core import PandaNode
from panda3d.core import PerspectiveLens
from panda3d.core import Vec2
from panda3d.core import Vec4

from mouseevent import MouseEvent

from cameracontroller import CameraController, EVT_CAMERA_MODE, TRACKBALL

tmpColor = Vec4()
gfxEngine = GraphicsEngine.getGlobalPtr()

class ButtonWatcher(DirectObject):
    def __init__(self, keys):
        self.modKeys = ModifierButtons()

        for key in keys:
            self.modKeys.addButton(key)
            self.accept(key.getName(), self.modKeys.buttonDown, [key])
            self.accept(key.getName()+'-up', self.modKeys.buttonUp, [key])

    def getKeys(self):
        return self.modKeys

    def destroy(self):
        self.ignoreAll()


def SelectionEngineInit(cls):
    # Create a cull bin for nodes we don't want to render. This will
    # be the default cull bin for the silhouette buffers; SelectableNodes
    # override it and jump in to the opaque cull bin.
    cbm = CullBinManager.getGlobalPtr()
    cbm.addBin('inactive', CullBinEnums.BTUnsorted, 0)
    cbm.setBinActive('inactive', False)
    return cls

@SelectionEngineInit
class SelectionEngine(DirectObject):
    defaultEngine = None

    @classmethod
    def getDefault(cls):
        if cls.defaultEngine == None:
            cls.defaultEngine = SelectionEngine()
        return cls.defaultEngine

    @classmethod
    def setDefault(cls, engine):
        cls.defaultEngine = engine

    def __init__(self, mainWin=base.win, mainCam=base.cam, scene=render):
        self.lastMousePos = Vec2(-2, -2)
        self.lastId = 0
        self.pressedNode = None
        self.mDownId = 0
        self.idStack = range(1, 256)
        self.idTable = {}
        self.aspectRatio = 1
        self.checkCursorTask = Task(self.checkCursor, 'checkCursorTask')
        self.enabled = True
        self.mouseListeners = []

        self.mainCam = mainCam
        mainCam.node().setCameraMask(BitMask32(1))

        tempnode = NodePath(PandaNode('temp node'))
        tempnode.setShaderAuto()
        tempnode.setShaderInput('hi_id', Vec4(0, 0, 0, 0), 2)
        mainCam.node().setInitialState(tempnode.getState())
        
        # Set up a node with the silhouetteGen shader. We'll apply this node's
        # state to custom cameras below.
        tempnode.setShader(loader.loadShader('silhouetteGen.sha'), 100)
        tempnode.setShaderInput('hi_id', Vec4(0, 0, 0, 0), 0)
        tempnode.setAntialias(AntialiasAttrib.MNone, 100)
        tempnode.setBin('inactive', 0, 1)
        initialState = tempnode.getState()
        tempnode.setBin('opaque', 0, 1)
        selnodeState = tempnode.getState()

        # We'll be using this a few times, so make an easy name for it.
        mainLens = mainCam.node().getLens()

        # Set up a buffer to which we draw a silhouette of any geometry that
        # we want to outline. We draw the outline by applying a Sobel edge
        # detection shader to the contents of this buffer.
        silhouetteBuffer = mainWin.makeTextureBuffer('silhouetteBuffer', 0, 0)
        silhouetteBuffer.setClearColor(Vec4(0, 0, 0, 1))
        self.silhouetteBuffer = silhouetteBuffer
        silhouetteCamera = base.makeCamera(silhouetteBuffer, lens=mainLens)
        silhouetteCamera.node().setScene(scene)
        silhouetteCamera.node().setInitialState(initialState)
        silhouetteCamera.node().setTagState('sel', selnodeState)
        silhouetteCamera.node().setTagStateKey('sel')
        silhouetteCamera.node().setCameraMask(BitMask32(8))

        tempnode.setShader(loader.loadShader('mousePicker.sha'), 100)
        selnodeState = tempnode.getState()
        tempnode.setBin('inactive', 0, 1)
        initialState = tempnode.getState()

        # Set up a 1-by-1 buffer to which we'll just render the pixel under
        # the mouse.
        selectBuffer = mainWin.makeTextureBuffer('selectBuffer', 1, 1)
        selectBuffer.setClearColor(Vec4(0, 0, 0, 1))
        self.selectBuffer = selectBuffer
        selectLens = PerspectiveLens()
        selectLens.setNearFar(mainLens.getNear(), mainLens.getFar())
        selectLens.setFocalLength(mainLens.getFocalLength())
        selectCamera = base.makeCamera(selectBuffer, lens=selectLens)
        selectCamera.node().setScene(scene)
        selectCamera.node().setInitialState(initialState)
        selectCamera.node().setTagState('sel', selnodeState)
        selectCamera.node().setTagStateKey('sel')
        selectCamera.node().setCameraMask(BitMask32(16))
        self.selectLens = selectLens

        self.selectTex = selectBuffer.getTexture()
        self.selectTex.makeRamImage()
        self.gsg = mainWin.getGsg()

        # Set up a texture card to render the silhouette texture with the
        # Sobel shader, which will draw the edges of the silhouettes.
        silhouetteCard = silhouetteBuffer.getTextureCard()
        silhouetteCard.setTransparency(1)
        inkGen = loader.loadShader('sobel.sha')
        silhouetteCard.setShader(inkGen)
        silhouetteCard.setShaderInput('separation', 0.001, 0)
        silhouetteCard.reparentTo(render2d)
        self.silhouetteCard = silhouetteCard
        
        self.accept(mainWin.getWindowEvent(), self.windowEventHandler)
        self.accept('mouse1', self.mouseDownHandler, ['l'])
        self.accept('mouse1-up', self.mouseUpHandler, ['l'])
        self.accept('mouse3', self.mouseDownHandler, ['r'])
        self.accept('mouse3-up', self.mouseUpHandler, ['r'])
        self.buttonWatcher = ButtonWatcher([
                KeyboardButton.shift(),
                KeyboardButton.control(),
                MouseButton.one(),
                MouseButton.three(),
            ])
        
        CameraController.getInstance().addEventHandler(EVT_CAMERA_MODE,
                self._cameraModeHandler)
        
        self.enable(False)

    def _cameraModeHandler(self, cameraController):
        if cameraController.getCameraMode() == TRACKBALL:
            self.enable(True)
        else:
            self.enable(False)

    def setHighlightColor(self, color):
        self.silhouetteCard.setColorScale(color)

    def enable(self, enabled = True):
        if self.enabled and enabled == False:
            # Disable the SelectionEngine
            self.removeTask(self.checkCursorTask)
            if self.lastId:
                node = self.idTable[self.lastId]
                event = MouseEvent('mouseExited', node, self.lastMousePos,
                    self.buttonWatcher.getKeys())
                node.dispatchMouseEvent(event)
            self.silhouetteBuffer.setActive(False)
            self.selectBuffer.setActive(False)
            self.silhouetteCard.hide()
            self.enabled = False
        elif self.enabled == False and enabled:
            # Enable the SelectionEngine
            self.silhouetteBuffer.setActive(True)
            self.selectBuffer.setActive(True)
            self.silhouetteCard.show()
            self.lastMousePos = Vec2(-2, -2)
            self.lastId = 0
            self.checkCursorTask = taskMgr.add(self.checkCursorTask)
            self.enabled = True

    def register(self, selectableNode):
        id = self.idStack.pop()
        selectableNode.setId(id)
        self.idTable[id] = selectableNode
        print 'registering new node %s with id %d' % (selectableNode, id)

    def remove(self, selectableNode):
        id = selectableNode.getId()
        self.idStack.push(id)
        self.idTable[id] = None

    def addMouseListener(self, listener):
        if listener not in self.mouseListeners:
            self.mouseListeners.append(listener)

    def removeMouseListener(self, listener):
        if listener in self.mouseListeners:
            self.mouseListeners.remove(listener)

    def dispatchMouseEvent(self, event):
        for listener in self.mouseListeners:
            funHandle = getattr(listener, event.type)
            funHandle(event)

    def windowEventHandler(self, window):
        mainFilmSize = self.mainCam.node().getLens().getFilmSize()
        mainFocalLength = self.mainCam.node().getLens().getFocalLength()
        self.selectLens.setFilmSize(mainFilmSize.x/window.getXSize(),
                mainFilmSize.y/window.getYSize())
        self.selectLens.setFocalLength(mainFocalLength)
        self.aspectRatio = mainFilmSize.y/mainFilmSize.x


    def mouseDownHandler(self, arg):
        if self.lastId in self.idTable:
            node = self.idTable[self.lastId]
            print 'dispatching event to', node
            event = MouseEvent('mousePressed', node, self.lastMousePos,
                    self.buttonWatcher.getKeys())
            node.dispatchMouseEvent(event)
            self.pressedNode = node
        elif self.lastId == 0:
            print 'dispatching event from self'
            event = MouseEvent('mousePressed', self, self.lastMousePos,
                    self.buttonWatcher.getKeys())
            self.dispatchMouseEvent(event)
        self.mDownId = self.lastId

    def mouseUpHandler(self, arg):
        if self.pressedNode:
            event = MouseEvent('mouseReleased', self.pressedNode,
                    self.lastMousePos, self.buttonWatcher.getKeys())
            self.pressedNode.dispatchMouseEvent(event)
            if self.mDownId == self.lastId:
                event = event.setType('mouseClicked')
                self.pressedNode.dispatchMouseEvent(event)
        self.pressedNode = None 


    def checkCursor(self, task):
        if base.mouseWatcherNode.hasMouse():
            mpos = base.mouseWatcherNode.getMouse()

            if mpos.x < -1 or mpos.y < -1 or mpos.x > 1 or mpos.y > 1:
                return Task.cont

            gfxEngine.extractTextureData(self.selectTex, self.gsg)
            selectPeeker = self.selectTex.peek()
            selectPeeker.lookup(tmpColor, 0.5, 0.5)
            id = int(tmpColor.y * 255)
            
            if self.pressedNode:
                if mpos != self.lastMousePos:
                    event = MouseEvent('mouseMoved', self.pressedNode, mpos.xy,
                            self.buttonWatcher.getKeys())
                    self.pressedNode.dispatchMouseEvent(event)
            elif id != self.lastId:
                if self.lastId in self.idTable:
                    node = self.idTable[self.lastId]
                    event = MouseEvent('mouseExited', node, mpos.xy,
                            self.buttonWatcher.getKeys())
                    node.dispatchMouseEvent(event)

                if id in self.idTable:
                    node = self.idTable[id]
                    event = MouseEvent('mouseEntered', node, mpos.xy,
                            self.buttonWatcher.getKeys())
                    node.dispatchMouseEvent(event)

                self.lastId = id

            if mpos != self.lastMousePos:
                self.lastMousePos.xy = mpos.xy
                self.selectLens.setFilmOffset(mpos.x/2,
                    self.aspectRatio*mpos.y/2)
        elif self.lastId:
            node = self.idTable[self.lastId]
            event = MouseEvent('mouseExited', node, self.lastMousePos,
                    self.buttonWatcher.getKeys())
            node.dispatchMouseEvent(event)
            self.lastId = 0
            
        return Task.cont
