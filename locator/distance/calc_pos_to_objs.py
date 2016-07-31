import shm
import math
import os
    
def is_square(obj):
    return (obj == "emperor" or obj == "torpedo")
    
class PositionToObject:
    def __init__(self):
        self.eqFiles = [
            'orange_buoy',
            'torpedo_dist',
            'emperor_dist',
            #'wire_dist',
            #'torpedo_angle',
            'emperor_angle',
            #'wire_angle',
            ]

        self.objectToConstants = {}
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
        self.parse_all_eqs(self.eqFiles)
    

    def areaToDist(self, area, obj):
        radius = math.sqrt(area/math.pi)
        return self.eq(obj,radius)

    #Input: String Object
    def parse_eq(self, obj):
        filename = os.path.join( os.path.dirname(__file__), obj)
        object_file = open(filename,'r')
        eq = object_file.readlines()
        split = eq[0].split(' ')
        constants = (float(split[0]),float(split[1]))
        self.objectToConstants[obj] = constants
        
    def parse_all_eqs(self, eq_files):
        for obj in eq_files:
            try:
                self.parse_eq(obj)
            except IOError, e:
                print "Warning:", e
                print "file is non-essential unless that task is being run"

    def get_distance(self, obj):
        if (obj == 'orange_buoy'):
            area = shm.orange_results.area.get()
            return self.areaToDist(area, "orange_buoy")
        elif (obj == 'green_buoy'):
            area = shm.green_results.area.get()
            # Use orange buoy data file for all buoys
            return self.areaToDist(area, "orange_buoy")
        elif  (obj == 'yellow_buoy'):
            area = shm.yellow_results.area.get()
            # Use orange buoy data file for all buoys
            return self.areaToDist(area, "orange_buoy")
        elif (obj == 'led_buoy1'):
            area = shm.led_buoy_results.area.get()
            return self.areaToDist(area, "orange_buoy")
        elif (obj == 'led_buoy2'):
            area = shm.led_buoy_results2.area.get()
            return self.areaToDist(area, "orange_buoy")
        elif (obj == 'emperor'):
            self.retrieve_data(obj)       
            avg_height = (self.lh + self.rh) / 2
            return self.eq(obj,avg_height)
        elif (obj == 'wire'):
            self.retrieve_data(obj)
            return self.eq(obj, self.bigger_height)
        else:
            raise Exception("'%s' is not valid object" % obj)

    def get_angle(self, obj):
        self.retrieve_data(obj)
        negative = False
        if (obj == 'torpedo' or obj == 'emperor'):
            if (self.lh > self.rh):
                print("lh: %d, rh: %d" %(self.lh,self.rh))
                self.bigger_height = self.lh
                negative = True
            else:
                print("lh: %d, rh: %d" %(self.lh,self.rh))
                self.bigger_height = self.rh
            print("bigger hight: " + str(self.bigger_height))
            self.bigger_width = max(self.tw,self.bw)
            print ("height: %d, width: %d" %(self.bigger_height, self.bigger_width))
            aspect = self.bigger_width * 1.0/self.bigger_height
            print("Aspect: " + str(aspect))

        if (obj == 'wire'):
            aspect = self.bigger_height/self.bigger_width

        if not negative:
            return self.eq(obj, aspect)
        else:
            return -1 * self.eq(obj, aspect)

    # obj is a string of the object we'd like info about
    def eq(self, obj, indepVar):
        if obj == "emperor":
            obj = "emperor_dist"

        constants = self.objectToConstants[obj]
        c1 = constants[0]
        c2 = constants[1]
        return (c1/indepVar) + c2

    def retrieve_data(self, obj):
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

        if (obj != 'wire'):
            self.tw = self.tr_x - self.tl_x
            self.bw = self.br_x - self.bl_x
            self.lh = self.bl_y - self.tl_y
            self.rh = self.br_y - self.tr_y

