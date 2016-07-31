from direct.showbase.ShowBase import ShowBase
import threading
from pandac.PandaModules import AmbientLight
from pandac.PandaModules import Material
from pandac.PandaModules import PointLight
from pandac.PandaModules import Point3
from pandac.PandaModules import VBase4
from panda3d.core import Texture
from panda3d.core import TextureAttrib
from panda3d.core import TextureStage
from panda3d.core import TransparencyAttrib
from panda3d.physics import AngularEulerIntegrator

#HPR = heading, pitch, roll

lightPositions = [
    Point3(6.0, 4.0, 40.0),
    Point3(-6.0, 4.0, 40.0),
    Point3(6, -4, 40),
    Point3(-6, -4, 40),
    Point3(0.0, 0.0, -40.0),
]
def makeEnvironment():
    '''
    Initialize collision variables, make sub, pool, mission element, and
    collision element models.

    '''
    makePoolModel()
    # makeMissionElements()

    '''
    buoyModel = loader.loadModel('buoy.egg')
    
    buoyModel.reparentTo(render)
    buoyModel.setScale(1.0)
    buoyModel.setPos(1.0, 1.0, 1.0)
    geomNode = buoyModel.find('**/Sphere').node()
    newRenderState = geomNode.getGeomState(0).removeAttrib(TextureAttrib)
    geomNode.setGeomState(0, newRenderState)
    
    tex = loader.loadTexture('buoy_green.png')
    buoyModel.setTexture(tex)
    '''

    ambLight = AmbientLight('AmbientLight')
    ambLight.setColor(VBase4(0.8, 0.8, 0.8, 1))
    ambLightPath = render.attachNewNode(ambLight)
    render.setLight(ambLightPath)

    for pos in lightPositions:
        light = PointLight('PointLight')
        light.setColor(VBase4(0.7, 0.7, 0.7, 1.0))
        light.setAttenuation(Point3(0.0, 0.0, 0.003))
        lightPath = render.attachNewNode(light)
        lightPath.setPos(pos)
        render.setLight(lightPath)
    
def makeCollisionElements(self):
#http://www.panda3d.org/manual/index.php/Collision_Solids
    # Initialize the traverser.
    self.cTrav = base.cTrav = CollisionTraverser()
            
    # Initialize the handler
    self.collHandEvent = CollisionHandlerEvent()
    self.collHandEvent.addInPattern('into-%in')
    self.collHandEvent.addOutPattern('outof-%in')

    # Make a variable to store the unique collision string count.
    self.collCount = 0
    sColl = initCollisionSegment(self, self.sub, True)
    base.cTrav.addCollider(sColl[0], self.collHandEvent)	
    self.accept('into-' + sColl[1], collideSub)
    self.accept('outof-' + sColl[1], collideSub)
    print(sColl[1])
    
    tColl = initCollisionPlanes(self,self.env,True)
    for i in range(0, 3):
        self.base.cTrav.addCollider(tColl[i][0], self.collHandEvent)
        self.accept('into-' + tColl[i][1], collide, extraArgs = [i])
        self.accept('outof-' + tColl[i][1], uncollide, extraArgs = [i])
    print("G")        
    print(tColl[1])
 
def getInfo(collEntry):
    if(collEntry.hasContactPos()):
        print(collEntry.getContactPos())
    else:
        print("NO CONTACT POS")	
    if(collEntry.hasContactNormal()):
        print(collEntry.getContactNormal())	
    else:
        print("NO CONTACT NORMaL")
#	print(collEntry.getContactNormal(self.sub))
#	print(collEntry.getContactPos(self.subNodepath))

def collideSub(collEntry):
    print("COLLIDE SUB")

def collide(xyz,collEntry):#colliding
    #print("COLLIDING, xyz = ", xyz, "collentry = ", collEntry)
    self.colliding = True
    if (not self.collisions[xyz]): 
            self.collisions[xyz] = True
            self.collisionEntries[xyz] = collEntry
    else: #already collided, not cleared
            return

def uncollide(xyz,collEntry):
    print("UNCOLLIDE: ", xyz)

def makePoolModel():
    teagleModel = loader.loadModel('teagle.bam')
    
    # Place the model in the transparent bin, so it's rendered back-to-front.
    waterNode = teagleModel.find('**/Water')
    waterNode.setTransparency(TransparencyAttrib.MAlpha)
    waterNode.setBin('transparent', 30)
    teagleModel.reparentTo(render)
            

def makeMissionElements(self):
    makeLaneLines(self)
    makeBuoys(self)
    makeHedge(self)

def makeLaneLines(self):
    self.lanelines = [0,0,0,0,0]
    for i in range(0, 5):	
        self.lanelines[i] = self.loader.loadModel("laneline.egg")
        self.lanelines[i].setScale(.1,.1,.1)	
	self.lanelines[i].setPos(i,0,19)
	self.lanelines[i].setHpr(0,90,0)
	self.lanelines[i].reparentTo(self.render)

def makeBuoys(self):
    pass

def makeHedge(self):
    pass

def initCollisionSphere(self, obj, k, show = False): 
    bounds = obj.getChild(0).getBounds()
    center = bounds.getCenter()
    radius = bounds.getRadius() * 1.1
    collSphereStr = 'CollisionHull' + str(self.collCount) + "_" + obj.getName()
    self.collCount+= 1
    cNode = CollisionNode(collSphereStr)
    cNode.addSolid(CollisionBox(center, k,k,k))	
#cNode.addSolid(CollisionSphere(center, radius))

    cNodepath = obj.attachNewNode(cNode)
    if show:
            cNodepath.show()
    return (cNodepath, collSphereStr)

def initCollisionSegment(self, obj, show = False): 
    bounds = obj.getChild(0).getBounds()
    center = bounds.getCenter()
    radius = bounds.getRadius() * 1.1
    collSphereStr = 'CollisionHull' + str(self.collCount) + "_" + obj.getName()
    self.collCount+= 1
    cNode = CollisionNode(collSphereStr)
    x0 = -1
    y0 = -1
    z0 = -1
    x1 = 1
    y1 = 1
    z1 = 1
    cNode.addSolid(CollisionSegment(x0,y0,z0, x0,y0,z1))
    cNode.addSolid(CollisionSegment(x1,y0,z0, x1,y0,z1))
    cNode.addSolid(CollisionSegment(x0,y1,z0, x0,y1,z1))
    cNode.addSolid(CollisionSegment(x1,y1,z0, x1,y1,z1))
#cNode.addSolid(CollisionSphere(center, radius))

    cNodepath = obj.attachNewNode(cNode)
    if show:
            cNodepath.show()
    self.subNodepath = cNodepath
    return (cNodepath, collSphereStr)

def initCollisionPlanes1(self, obj, show = False): 
    print("INIT COLLISION PLANE")
    bounds = obj.getChild(0).getBounds()
    center = bounds.getCenter()
    radius = bounds.getRadius() * 1.1
    lx = 1
    ly = 1
    lz = 1
    collSphereStr = 'CollisionHull' + str(self.collCount) + "_" + obj.getName()
    self.collCount+= 1
    cNode = CollisionNode(collSphereStr)
    cNode.addSolid(CollisionPlane(Plane(Vec3(1,0,0), Point3(-lx, 0, 0))))
    cNode.addSolid(CollisionPlane(Plane(Vec3(-1,0,0), Point3(lx, 0,0))))
    cNode.addSolid(CollisionPlane(Plane(Vec3(0,1,0), Point3(0, -ly, 0))))
    cNode.addSolid(CollisionPlane(Plane(Vec3(0,-1,0), Point3(0, ly,0))))
    cNode.addSolid(CollisionPlane(Plane(Vec3(0,0,1), Point3(0, 0, -lz))))
    cNode.addSolid(CollisionPlane(Plane(Vec3(0,0,-1), Point3(0, 0,lz))))

    cNodepath = obj.attachNewNode(cNode)

    if show:
        cNodepath.show()
        print("SHOWING plane")
    return (cNodepath, collSphereStr)

def initCollisionPlanes(self, env, show = False): 
    print("INIT COLLISION PLANE")
    #bounds = obj.getChild(0).getBounds()
    #center = bounds.getCenter()
    #radius = bounds.getRadius() * 1.1
    lx = 1
    ly = 1
    lz = 1
    collSphereStr= [None, None, None]
    cNode = [None, None, None]	
    for i in range(0,3):
        collSphereStr[i] = 'CollisionHull' + str(i)
        self.collCount+= 1
        cNode[i] = CollisionNode(collSphereStr[i])

    cNode[0].addSolid(CollisionPlane(Plane(Vec3(1,0,0), Point3(-lx, 0, 0))))
    cNode[0].addSolid(CollisionPlane(Plane(Vec3(-1,0,0), Point3(lx, 0,0))))
    cNode[1].addSolid(CollisionPlane(Plane(Vec3(0,1,0), Point3(0, -ly, 0))))
    cNode[1].addSolid(CollisionPlane(Plane(Vec3(0,-1,0), Point3(0, ly,0))))
    cNode[2].addSolid(CollisionPlane(Plane(Vec3(0,0,1), Point3(0, 0, -lz))))
    cNode[2].addSolid(CollisionPlane(Plane(Vec3(0,0,-1), Point3(0, 0,lz))))

    cNodepath = [None, None, None]
    for i in range (0, 3):		
        cNodepath[i] = env[i].attachNewNode(cNode[i])

        if show:
            cNodepath[i].show()
            print("SHOWING plane")
    return [(cNodepath[0], collSphereStr[0]), (cNodepath[1], collSphereStr[1]),(cNodepath[2], collSphereStr[2])]

def initCollisionSub(self, obj, show = False): 
    bounds = obj.getChild(0).getBounds()
    center = bounds.getCenter()
    radius = bounds.getRadius() * 1.1
    lx = 1
    ly = 1
    lz = 1	

    collSphereStr = 'CollisionHull' + str(self.collCount) + "_" + obj.getName()
    self.collCount+= 1
    cNode = CollisionNode(collSphereStr)
    #cNode.addSolid(CollisionSphere(center, radius))
            

    faces = [[0,0],[0,0],[0,0]]
    faces[0][0] = CollisionPolygon(Point3(-lx, -ly, -lz), Point3(-lx, ly, -lz), Point3(-lx, ly, lz), Point3(-lx, -ly, lz))	
    faces[0][1] = CollisionPolygon(Point3(lx, -ly, -lz), Point3(lx, ly, -lz), Point3(lx, ly, lz), Point3(lx, -ly, lz))
    
    faces[1][0] = CollisionPolygon(Point3(-lx, -ly, -lz), Point3(-lx, -ly, lz), Point3(lx, -ly, lz), Point3(lx, -ly, -lz))	
    faces[1][1] = CollisionPolygon(Point3(-lx, ly, -lz), Point3(-lx, ly, lz), Point3(lx, ly, lz), Point3(lx, ly, -lz))

    faces[2][0] = CollisionPolygon(Point3(-lx, -ly, -lz), Point3(-lx, ly, -lz), Point3(lx, ly, -lz), Point3(lx, -ly, -lz))	
    faces[2][1] = CollisionPolygon(Point3(-lx, -ly, lz), Point3(lx, ly, lz), Point3(lx, ly, lz), Point3(lx, -ly, lz))
    
    for i in (0,1):
        for j in (0,1,2):		
            cNode.addSolid(faces[j][i])
    #cNode.addSolid(collisionSolid(self.sub))
    cNodepath = obj.attachNewNode(cNode)
    if show:
        cNodepath.show()
    return (cNodepath, collSphereStr)

def initCollisionBox(self, obj,lx, ly, lz, show = False): 
    bounds = obj.getChild(0).getBounds()
    center = bounds.getCenter()
    radius = bounds.getRadius() * 1.1
    collSphereStr = 'CollisionHull' + str(self.collCount) + "_" + obj.getName()
    self.collCount+= 1
    cNode = CollisionNode(collSphereStr)
    cNode.addSolid(CollisionBox(0,0,0, lx, ly,lz))

    cNodepath = obj.attachNewNode(cNode)
    if show:
        cNodepath.show()
    return (cNodepath, collSphereStr)

