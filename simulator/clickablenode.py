
from panda3d.core import NodePath
from panda3d.core import BitMask32
from panda3d.core import PandaNode
from panda3d.core import Vec4

from selectionengine import SelectionEngine

class ClickableNode(PandaNode):
    def __init__(self, name, engine=None):
        PandaNode.__init__(self, name)
        self.path = NodePath(self)
        self.mouseListeners = []
        self.hi_id = Vec4()
        if engine == None:
            engine = SelectionEngine.getDefault()
        self.engine = engine
        self.engine.register(self)
        self.path.setTag('sel', 'sel')

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

    def setId(self, id):
        self.hi_id.y = id
        self.path.setShaderInput('hi_id', self.hi_id, 1)

    def getId(self):
        return self.hi_id.y

    def setHighlighted(self, highlighted = True):
        if highlighted:
            self.path.show(BitMask32(8))
        else:
            self.path.hide(BitMask32(8))
        self.hi_id.x = int(bool(highlighted))
        self.path.setShaderInput('hi_id', self.hi_id, 1)

    def setDepthLevel(self, level):
        self.hi_id.z = level
        self.path.setShaderInput('hi_id', self.hi_id, 1)

    def __del__(self):
        self.engine.remove(self)
