import shm
import math
    
def isBuoy(obj):
    return (obj == "orange_buoy" or obj == "green_buoy" or obj == "yellow_buoy")

def isSquare(obj):
    return (obj == "emperor" or obj == "torpedo")

#Input: String Object obj
def parseEq(obj, objToConst):
    object_file = open(obj,'r')
    eq = object_file.readlines()
    split = eq[0].split(' ')
    constants = (float(split[0]),float(split[1]))
    objToConst[obj] = constants
    
def parseAllEqs(eq_files):
    objToConst = {}
    for obj in eq_files:
        parseEq(obj, objToConst)
    return objToConst

class PositionToObject:
    def __init__(self):
        self.distEqFiles = [
            'orange_buoy_dist',
            'green_buoy_dist',
           # 'yellow_buoy_dist',
            'torpedo_dist',
           # 'emperor_dist',
           # 'wire_dist',
            ]

        self.angleEqFiles = [
            'torpedo_angle',
            #'emperor_angle',
            #'wire_angle'
            ]

        self.objectToDistConstants = parseAllEqs(self.distEqFiles)
        self.objectToAngleConstants = parseAllEqs(self.angleEqFiles)
        self.tl_x = 0
        self.tl_y = 0
        self.tr_x = 0
        self.tr_y = 0
        self.bl_x = 0
        self.bl_y = 0
        self.br_x = 0
        self.br_y = 0
        self.tw = 0
        self.bw = 0
        self.lh = 0
        self.rh = 0
        self.bigger_height = 0
        self.bigger_width = 0
        self.wire_side = 0    

    def getDistance(self, obj):
        distObj = obj + '_dist'
        if (isBuoy(obj)):
            if (obj == 'orange_buoy'):
                area = shm.orange_results.area.get()
                return self.areaToDist(area, distObj)
            if (obj == 'green_buoy'):
                area = shm.green_results.area.get()
                return self.areaToDist(area, distObj)
            if  (obj == 'yellow_buoy'):
                area = shm.yellow_results.area.get()
                return self.areaToDist(area, distObj)

        if (isSquare(obj)):
            self.retrieveData(obj)       
            avg_height = (self.lh + self.rh) / 2
            return self.distEq(distObj,avg_height)

        if (obj == 'wire'):
            self.retrieveData(obj)
            return self.distEq(distObj, self.bigger_height)

    def getAngle(self, obj):
        angleObj = obj + '_angle'
        self.retrieveData(obj)
        negative = False
        if (obj == 'torpedo' or obj == 'emperor'):
            if (self.lh > self.rh):
                bigger_height = self.lh
                negative = True
            else:
                bigger_height = self.rh

            bigger_width = max(self.tw,self.bw)
            aspect = bigger_height/bigger_width

        if (obj == 'wire'):
            if (self.wire_side == -1):
                negative = True
            aspect = self.bigger_height/self.bigger_width

        if (negative):
            return -1 * self.angleEq(angleObj, aspect)
        else:
            return self.angleEq(angleObj, aspect)

    def areaToDist(self, area, obj):
        radius = math.sqrt(area/math.pi)
        return self.distEq(obj,radius)

    # obj is a string of the object we'd like info about
    def distEq(self, obj, length):
        constants = self.objectToDistConstants[obj]
        c1 = constants[0]
        c2 = constants[1]
        return (c1/length) + c2

    def angleEq(self, obj, aspect):
        constants = self.objectToAngleConstants[obj]
        c1 = constants[0]
        c2 = constants[1]
        return (c1/aspect) + c2

    def retrieveData(self, obj):
        if (obj == 'torpedo'):
            self.tl_x = shm.nest_results.top_left_x.get()
            self.tl_y = shm.nest_results.top_left_y.get()
            self.tr_x = shm.nest_results.top_right_x.get()
            self.tr_y = shm.nest_results.top_right_y.get()
            self.bl_x = shm.nest_results.bottom_left_x.get()
            self.bl_y = shm.nest_results.bottom_left_y.get()
            self.br_x = shm.nest_results.bottom_right_x.get()
            self.br_y = shm.nest_results.bottom_right_y.get()
            
        if (obj == 'emperor'):
            self.tl_x = shm.emperor_results.top_left_x.get()
            self.tl_y = shm.emperor_results.top_left_y.get()
            self.tr_x = shm.emperor_results.top_right_x.get()
            self.tr_y = shm.emperor_results.top_right_y.get()
            self.bl_x = shm.emperor_results.bottom_left_x.get()
            self.bl_y = shm.emperor_results.bottom_left_y.get()
            self.br_x = shm.emperor_results.bottom_right_x.get()
            self.br_y = shm.emperor_results.bottom_right_y.get()
           
        if (obj == 'wire'):
            self.bigger_height = shm.wire_results.bigger_height.get()
            self.bigger_width = shm.wire_results.bigger_width.get()
            self.wire_side = shm.wire_results.sub_side_is_on.get()

        if (obj != 'wire'):
            self.tw = self.tr_x - self.tl_x
            self.bw = self.br_x - self.bl_x
            self.lh = self.tl_y - self.bl_y
            self.rh = self.tr_y - self.br_y

