import rect_data_analysis as analysis
import eq_gen as eq
import position_retrieval as pos

def anal(obj):
    return analysis.RectDataAnalysis(obj)

def genEq(obj):
    return eq.EqGen(obj)

def posFactory():
    return pos.PositionToObject()

torpedo = 'torpedo'
orange = 'orange_buoy'
green = 'green_buoy'

p = posFactory()
d = p.getDistance(orange)
a = p.getAngle(torpedo)

print ("Torpedo Angle ",`a`)
print ("orange buoy distance: ", `d`)

