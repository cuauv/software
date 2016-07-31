
class MouseEvent(object):
    def __init__(self, type, sender, pos, modifiers):
        self.type = type
        self.sender = sender
        self.pos = pos
        self.modifiers = modifiers

    def setType(self, type):
        return MouseEvent(type, self.sender, self.pos, self.modifiers)

class MouseEventListener(object):

    def mouseEntered(self, event):
        pass

    def mouseExited(self, event):
        pass

    def mousePressed(self, event):
        pass

    def mouseClicked(self, event):
        pass

    def mouseReleased(self, event):
        pass

    def mouseMoved(self, event):
        pass

    def selected(self, event):
        pass

    def deselected(self, event):
        pass

