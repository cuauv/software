'''
handles.py

Defines handles used to position objects

'''

# Built-in Packages
from math import sin, cos, pi

# Panda Packages
from direct.showbase import DirectObject
from panda3d.core import AntialiasAttrib
from panda3d.core import BitMask32
from panda3d.core import CSYupRight
from panda3d.core import CullBinEnums
from panda3d.core import CullBinManager
from panda3d.core import Geom
from panda3d.core import GeomLines
from panda3d.core import GeomNode
from panda3d.core import GeomTriangles
from panda3d.core import GeomTristrips
from panda3d.core import GeomVertexData
from panda3d.core import GeomVertexFormat
from panda3d.core import GeomVertexWriter
from panda3d.core import LineSegs
from panda3d.core import Mat4
from panda3d.core import NodePath
from panda3d.core import PandaNode
from panda3d.core import Point2
from panda3d.core import Point3
from panda3d.core import Vec2
from panda3d.core import Vec3
from panda3d.core import Vec4

# Simulator modules
from cameracontroller import CameraController, EVT_CAMERA_MOVE
from clickablenode import ClickableNode
from mouseevent import MouseEventListener

# Size of the handles, in pixels.
HANDLE_SIZE = 100

def makeRotationGeomNode():
    vdata = GeomVertexData('rotHandleData', GeomVertexFormat.getV3(),
            Geom.UHStatic)
    v = GeomVertexWriter(vdata, 'vertex')
    radius = 0.7
    width = 0.08
    res = 30
    innerRad = radius - width

    for i in xrange(res):
        theta = i*(2*pi/res)
        v.addData3f(innerRad*sin(theta), innerRad*cos(theta), width/2.0)
        v.addData3f(innerRad*sin(theta), innerRad*cos(theta), -width/2.0)
        v.addData3f(radius*sin(theta), radius*cos(theta), width/2.0)
        v.addData3f(radius*sin(theta), radius*cos(theta), -width/2.0)

    circle = Geom(vdata)
    # Make prims for the faces of the torus
    faces = [GeomTristrips(Geom.UHStatic) for i in xrange(4)]
    for i in xrange(res):
        i = i*4
        faces[0].addVertices(i + 1, i)
        faces[1].addVertices(i + 2, i + 1)
        faces[2].addVertices(i + 3, i + 2)
        faces[3].addVertices(i, i + 3)
    for i in xrange(4):
        faces[i].addVertices((i + 1) % 4, i)
        faces[i].closePrimitive()
        circle.addPrimitive(faces[i])
    node = GeomNode('geomnode')
    node.addGeom(circle)
    return node


def makeGeomNode():
    vdata = GeomVertexData('handleData', GeomVertexFormat.getV3(),
            Geom.UHStatic)
    v = GeomVertexWriter(vdata, 'vertex')
    length = 1.0
    gapLen = 0.2
    coneLen = 0.18
    coneRad = 0.06
    circRes = 10

    v.addData3f(0, 0, -length/2.0) # back cone butt
    v.addData3f(0, 0, -length/2.0 + coneLen) # back cone point
    v.addData3f(0, 0, -gapLen/2.0)
    v.addData3f(0, 0, gapLen/2.0)
    v.addData3f(0, 0, length/2.0 - coneLen) # front cone butt
    v.addData3f(0, 0, length/2.0) # font cone point
    # Add vertices for the cone's circles.
    for z in [-length/2.0, length/2.0 - coneLen]:
        for i in xrange(circRes):
            theta = i*(2*pi/circRes)
            v.addData3f(coneRad*sin(theta), coneRad*cos(theta), z)

    lines = Geom(vdata)
    # Make prims for the two lines.
    for vertices in [(0, 2), (3, 5)]:
        line = GeomLines(Geom.UHStatic)
        line.addVertices(*vertices)
        line.closePrimitive()
        lines.addPrimitive(line)

    cones = Geom(vdata)
    # Make prims for the cones.
    for back, circ in [(0, 6), (4, 6 + circRes)]:
        point = back + 1
        cone = GeomTriangles(Geom.UHStatic)
        for i in xrange(circRes):
            if i + 1 == circRes:
                cone.addVertices(back, circ, circ + i)
                cone.addVertices(point, circ + i, circ)
            else:
                cone.addVertices(back, circ + i + 1, circ + i)
                cone.addVertices(point, circ + i, circ + i + 1)
        cone.closePrimitive()
        cones.addPrimitive(cone)
    
    node = GeomNode('geomnode')
    node.addGeom(lines)
    node.addGeom(cones)
    return node

def makeClickableGeom():
    vdata = GeomVertexData('handleData', GeomVertexFormat.getV3(),
            Geom.UHStatic)
    v = GeomVertexWriter(vdata, 'vertex')
    length = 1.0
    cylRad = 0.10
    circRes = 8

    # Add vertices cylinder.
    for z in [length/2.0, -length/2.0]:
        for i in xrange(circRes):
            theta = i*(2*pi/circRes)
            v.addData3f(cylRad*sin(theta), cylRad*cos(theta), z)

    geom = Geom(vdata)
    # Make polys for the circles
    CCW = 1
    CW = 0
    for i, wind in ((0, CCW), (circRes, CW)):
        circle = GeomTristrips(Geom.UHStatic)
        l = range(i, i+circRes)
        for i in xrange(1-wind, (len(l)-wind)/2):
            l.insert(2*i+wind, l.pop())
        for v in l:
            circle.addVertex(v)
        circle.closePrimitive()
        geom.addPrimitive(circle)

    # Make polys for the cylinder.
    cyl = GeomTristrips(Geom.UHStatic)
    for i in xrange(circRes):
        cyl.addVertex(i + circRes)
        cyl.addVertex(i)
    cyl.addVertex(circRes)
    cyl.addVertex(0)
    cyl.closePrimitive()
    geom.addPrimitive(cyl)
    node = GeomNode('geomnode')
    node.addGeom(geom)
    return node



cbm = CullBinManager.getGlobalPtr()
cbm.addBin('handles', CullBinEnums.BTFrontToBack, 35)
cbm.setBinActive('handles', True)

tempnode = NodePath(PandaNode('temp node'))
tempnode.setShaderAuto()
tempnode.setShaderInput('hi_id', Vec4(0, 0, 0, 0), 2)
tempnode.setBin('inactive', 0, 50)
tempnode.setAntialias(AntialiasAttrib.MNone, 10)
newCamera = base.makeCamera(base.win, lens=base.cam.node().getLens())
newCamera.node().setInitialState(tempnode.getState())
newCamera.node().getDisplayRegion(0).setSort(1)
newCamera.node().getDisplayRegion(0).setClearDepthActive(1)
newCamera.node().setCameraMask(BitMask32(4))
newCamera.node().setScene(render)


class RotationHandle(PandaNode, MouseEventListener):
    geomNode = makeRotationGeomNode()
    
    def __init__(self, parent, color, hpr, dim):
        PandaNode.__init__(self, dim+'rotHandle')
        self.path = NodePath(self)
        self.parent = parent
        self.dim = dim

        circle = GeomNode('gnode')
        circle.addGeomsFrom(self.geomNode)

        self.clickable = ClickableNode('clickable')
        self.clickable.addChild(circle)
        self.clickable.addMouseListener(self)
        circlenp = self.path.attachNewNode(self.clickable)

        self.path.setColor(color)
        self.path.setHpr(hpr)
        self.mDownPos = Vec2()

    def mouseEntered(self, event):
        self.path.setColorScale(2, 2, 2, 2)

    def mouseExited(self, event):
        self.path.setColorScale(1, 1, 1, 1)

    def mousePressed(self, event):
        self.parent.beginTransformation()
       
        self.lens = base.cam.node().getLens()

        scale = Mat4.scaleMat(self.path.getScale(base.camera))
        descale = Mat4()
        descale.invertAffineFrom(scale)
        mvMat = descale * self.path.getMat(base.camera)

        self.origin = Point3(mvMat.xformPoint(Point3()))
        self.planeNorm = Vec3(mvMat.xformVec(Vec3(0, 0, 1)))
        self.planeNorm.normalize()
        self.d = self.planeNorm.dot(self.origin)

        self.fromCam = base.camera.getMat(self.parent.path) * scale
        
        self.dir = Vec3(self.mouse2Vec(event.pos))
        self.dir.normalize()

    def mouseMoved(self, event):
        transl = Vec3(self.mouse2Vec(event.pos))
        transl.normalize()
        angle = self.dir.signedAngleDeg(transl, self.planeNorm)
        axis = Vec3()
        setattr(axis, self.dim, 1)
        self.parent.transform(Mat4.rotateMatNormaxis(angle, axis))

    def mouse2Vec(self, mpos):
        ray = Vec3()
        ray.xz = self.lens.getFilmSize()
        ray.x = mpos.x * ray.x / 2
        ray.z = mpos.y * ray.z / 2
        ray.y = self.lens.getFocalLength()
        ray.normalize()
        k = self.d/ray.dot(self.planeNorm)
        return ray*k - self.origin


class DirectionHandle(PandaNode, MouseEventListener):
    geomNode = makeGeomNode()
    clickableGeomNode = makeClickableGeom()

    def __init__(self, parent, color, hpr, dim):
        PandaNode.__init__(self, dim+'handle')
        self.path = NodePath(self)
        self.parent = parent
        self.dim = dim

        arrow = GeomNode('gnode')
        arrow.addGeomsFrom(self.geomNode)
        arrownp = self.path.attachNewNode(arrow)
        arrownp.hide(BitMask32(1))
        
        clickNode = ClickableNode('clicknode')
        clickNode.setDepthLevel(0.5)
        clickNode.addMouseListener(self)
        clicknp = self.path.attachNewNode(clickNode)
        
        clickgeom = clicknp.attachNewNode(GeomNode('clicknode'))
        clickgeom.hide(BitMask32(7))
        clickgeom.node().addGeomsFrom(self.clickableGeomNode)

        linesegs = LineSegs()
        linesegs.setColor(color)
        linesegs.setThickness(2)
        linesegs.moveTo(Vec3(0, 0, -30))
        linesegs.drawTo(Vec3(0, 0, -0.5))
        linesegs.moveTo(Vec3(0, 0, 0.5))
        linesegs.drawTo(Vec3(0, 0, 30))
        lines = self.path.attachNewNode(linesegs.create())
        lines.show(BitMask32(1))
        lines.hide(BitMask32(2|4|8|16))
        lines.setBin('opaque', 30, 100)
        lines.setAntialias(AntialiasAttrib.MNone)

        self.path.setColor(color)
        self.path.setHpr(hpr)
        
        self.mDownPos = Vec2()

    def mouseEntered(self, event):
        self.path.setColorScale(2, 2, 2, 2)

    def mouseExited(self, event):
        self.path.setColorScale(1, 1, 1, 1)

    def mousePressed(self, event):
        self.parent.beginTransformation()
       
        lens = base.cam.node().getLens()

        scale = Mat4.scaleMat(self.path.getScale(base.camera))
        descale = Mat4()
        descale.invertAffineFrom(scale)
        mvMat = descale * self.path.getMat(base.camera)

        origin = Point3(mvMat.xformPoint(Point3()))
        dir = Vec3(mvMat.xformVec(Vec3(0, 0, 1)))
        xprod = dir.cross(origin)
        planeNorm = xprod.cross(dir)
        planeNorm.normalize()
        d = planeNorm.dot(origin)
        self.dir = dir
        self.origin = origin
        self.planeNorm = planeNorm
        self.d = d
        self.lens = lens

        self.fromCam = base.camera.getMat(self.parent.path) * scale
        
        transl = self.mouse2Vec(event.pos)
        self.origin = transl + self.origin

    def mouseMoved(self, event):
        transl = self.fromCam.xformVec(self.mouse2Vec(event.pos))
        self.parent.transform(Mat4.translateMat(transl))

    def mouse2Vec(self, mpos):
        ray = Vec3()
        ray.xz = self.lens.getFilmSize()
        ray.x = mpos.x * ray.x / 2
        ray.z = mpos.y * ray.z / 2
        ray.y = self.lens.getFocalLength()
        ray.normalize()
        k = self.d/(ray.dot(self.planeNorm))
        return self.dir * (ray*k - self.origin).dot(self.dir)


class Handle(PandaNode, MouseEventListener):
    
    def __init__(self):
        PandaNode.__init__(self, 'PositionHandle')

        CameraController.getInstance().addEventHandler(EVT_CAMERA_MOVE,
                self._fixScale)
        self.path = NodePath(self)

        self.xHandle = DirectionHandle(self, Vec4(1, 0, 0, 0.6),
                Vec3(0, 0, 90), 'x')
        self.addChild(self.xHandle)

        self.yHandle = DirectionHandle(self, Vec4(0, 1, 0, 0.6),
                Vec3(0, -90, 0), 'y')
        self.addChild(self.yHandle)
        
        self.zHandle = DirectionHandle(self, Vec4(0, 0, 1, 0.6),
                Vec3(0, 0, 0), 'z')
        self.addChild(self.zHandle)
        
        self.hHandle = RotationHandle(self, Vec4(0, 0, 1, 0.6),
                Vec3(0, 0, 0), 'z')
        self.addChild(self.hHandle)
        NodePath(self.hHandle).setScale(1.02)

        self.pHandle = RotationHandle(self, Vec4(0, 1, 0, 0.6),
                Vec3(0, -90, 0), 'y')
        self.addChild(self.pHandle)
        NodePath(self.pHandle).setScale(0.98)

        self.rHandle = RotationHandle(self, Vec4(1, 0, 0, 0.6),
                Vec3(0, 0, 90), 'x')
        self.addChild(self.rHandle)
        self._tPressed()
        
        self.path.setTransparency(1)
        self.path.setBin('transparent', 30, 50)
        #self.path.setDepthTest(False)
        self.path.setLightOff()
        self.path.setRenderModeThickness(2)
        self.setClients([])

        self.evtWatcher = DirectObject.DirectObject()
        self.evtWatcher.accept(base.win.getWindowEvent(),
            self._windowEventHandler)
        self.evtWatcher.accept('r', self._rPressed)
        self.evtWatcher.accept('t', self._tPressed)

        self.originalPos = self.path.getPos()
        self.fLenPx = base.cam.node().getLens().getFocalLength() * base.win.getXSize()
        self._fixScale()

    def _rPressed(self):
        self.xHandle.path.stash()
        self.yHandle.path.stash()
        self.zHandle.path.stash()
        self.hHandle.path.unstash()
        self.pHandle.path.unstash()
        self.rHandle.path.unstash()

    def _tPressed(self):
        self.xHandle.path.unstash()
        self.yHandle.path.unstash()
        self.zHandle.path.unstash()
        self.hHandle.path.stash()
        self.pHandle.path.stash()
        self.rHandle.path.stash()

    def setClients(self, clients, center=Vec3()):
        if clients:
            self.path.show(BitMask32(1|4|16))
            self.path.setPos(center)
            self._fixScale()
        else:
            self.path.hide(BitMask32(1|2|4|8|16))
        self.clients = [[client, None] for client in clients]

    def beginTransformation(self):
        self.originalMat = Mat4(Mat4.identMat())
        self.originalMat.setRow(3, self.path.getMat(render).getRow(3))
        self.originalMatInv = Mat4()
        self.originalMatInv.invertAffineFrom(self.originalMat)
        for i in xrange(len(self.clients)):
            print 'Their matrix * My matrix'
            print self.clients[i][0].getMat(self.path) * self.path.getMat()
            print 'Total matrix:'
            print self.clients[i][0].getMat()
            #self.clients[i][1] = self.clients[i][0].getMat(self.path)
            self.clients[i][1] = (self.clients[i][0].getMat() *
                    self.originalMatInv)

    def transform(self, xform):
        transl = Mat4(Mat4.identMat())
        transl.setRow(3, xform.getRow3(3))
        self.path.setMat(render, transl * self.originalMat)
        self._fixScale()
        xform.setRow(3, Vec3())
        for client, originalMat in self.clients:
            #client.setMat(self.path, originalMat * xform)
            client.setMat(originalMat * xform * transl * self.originalMat)

    def _fixScale(self, camera=base.camera):
        myPos = self.path.getPos()
        dist = (camera.getPos() - myPos).length()
        self.path.setScale(dist * HANDLE_SIZE / self.fLenPx)

    def _windowEventHandler(self, window):
        self.fLenPx = (base.cam.node().getLens().getFocalLength() *
                 window.getXSize())
        self._fixScale()

