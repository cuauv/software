'''
missionlayout.py

Implements the MissionElement class, which is responsible for loading,
configuring, and saving mission elements. This is an abstract class, and should not be instantiated. The MissionElement.loadElementConfig dynamically creates
subclasses for the mission elements in the given configuration file.

'''

# Built-in modules
import plistlib

# Panda3D Modules
from panda3d.core import GeomNode
from panda3d.core import NodePath
from panda3d.core import PandaNode
from panda3d.core import TextureAttrib
from panda3d.core import Vec3

from selectablenode import SelectableNode

def stripTextures(model):
    node = model.node()
    if isinstance(node, GeomNode):
        for i in xrange(node.getNumGeoms()):
            renderState = node.getGeomState(i)
            if renderState.hasAttrib(TextureAttrib):
                newRenderState = renderState.removeAttrib(TextureAttrib)
                node.setGeomState(i, newRenderState)
    for child in model.getChildren():
        stripTextures(child)

class MissionElement(SelectableNode):
    elementTypes = {}
    skins = {}
    defaultSkin = None
    model = None
    name = 'MissionElement'

    def __init__(self, pos=Vec3(0, 0, 0), hpr=Vec3(0, 0, 0), skin=None):
        SelectableNode.__init__(self, self.name)
        self.path = NodePath(self)
        modelCopy = NodePath(self.model.node().copySubgraph())
        modelCopy.reparentTo(self.path)
        if skin:
            self.setSkin(skin)
        elif self.defaultSkin:
            self.setSkin(self.defaultSkin)
        self.path.setPos(pos)
        self.path.setHpr(hpr)
    
    def setSkin(self, skin):
        if skin in self.skins:
            self.skin = skin
            self.path.setTexture(self.skins[skin])
        else:
            self.skin = None

    def getSkin(self):
        try:
            x = self.skin
            return x
        except:
            return None #Hackish

    def getTypeName(self):
        try:
            return self.name
        except:
            return None #hackish
    
    @classmethod
    def loadElementConfig(cls, filePath):
        config = plistlib.readPlist(filePath)
        cls.elementTypes.clear()
        for key, value in config.items():
            typeName = key.replace(' ', '')
            model = loader.loadModel(value['model'])
            skins = {}
            defaultSkin = value.get('default_skin', None)
            cls.ts = model.findTextureStage('*')
            if 'skins' in value:
                stripTextures(model)    
                for name, path in value['skins'].items():
                    texture = loader.loadTexture(path)
                    skins[name] = texture
            
            
            elementType = type(typeName, (MissionElement,), dict())
            elementType.model = model
            elementType.skins = skins
            elementType.defaultSkin = defaultSkin
            elementType.name = typeName
            cls.elementTypes[key] = elementType

    @classmethod
    def getElementType(cls, typeName):
        return cls.elementTypes[typeName]


class MissionLayout(PandaNode):

    def __init__(self, name, fromFile=None):
        PandaNode.__init__(self, name)
        self.elements = []
        self.fromFile = fromFile

    def addElement(self, element):
        self.elements.append(element)
        self.addChild(element)

    @classmethod
    def loadLayout(cls, filePath):
        '''
        Load a mission layout from a layout file. Returns a NodePath that
        contains the loaded mission elements.

        '''
        layout = MissionLayout(filePath)
        config = plistlib.readPlist(filePath)
        for element in config:
            pos = Vec3(*element['pos'])
            rot = Vec3(*element['rot'])
            texName = element.get('skin', None)
            typeObj = MissionElement.getElementType(element['type'])
            inst = typeObj(pos=pos, hpr=rot, skin=texName)
            if element['type'] == "Pinger":
                inst.pinger_frequency = element['frequency']
            layout.addElement(inst)
        return layout

    def save(self, filePath=None):
        if filePath == None:
            filePath = self.fromFile

        config = []
        for element in self.elements:
            np = NodePath(element)
            
            od = {}

            od["type"] = element.getTypeName()
            od["pos"] = tuple(np.getPos())
            od["rot"] = tuple(np.getHpr())

            if element.getSkin() is not None:
                od["skin"] = element.getSkin()

            if element.getTypeName() == "Pinger":
                od["frequency"] = element.pinger_frequency

            config.append(od)

        plistlib.writePlist(config, filePath)
