'''
selectablenode.py

Defines a PandaNode subclass for objects that can be selected by the mouse.

'''

from panda3d.core import NodePath

from clickablenode import ClickableNode
from selectionmanager import SelectionManager

class SelectableNode(ClickableNode):

    def __init__(self, name, mgr=None):
        ClickableNode.__init__(self, name)
        self.path = NodePath(self)
        self.listeners = []
        self.selected = False
        if mgr == None:
            mgr = SelectionManager.getDefault()
        self.manager = mgr
        self.manager.registerNode(self)

    def isSelected(self):
        return self.selected

    def setSelected(self, selected = True):
        if self.selected == selected:
            return
        self.selected = selected
        self.setHighlighted(selected)

    def __del__(self):
        ClickableNode.__del__(self)
        self.manager(removeNode(self))
