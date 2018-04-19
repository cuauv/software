
import numpy as np

t = np.load('A_trans')

x = np.matrix([1, 0, 0])
y = np.matrix([0, 1, 0])
z = np.matrix([0, 0, 1])

print "x wrench to thrusts: ", x * t
print "y wrench to thrusts: ", y * t
print "z wrench to thrusts: ", z * t
np.save(open("fx_wrench", "wb"), x * t)
np.save(open("fy_wrench", "wb"), y * t)
np.save(open("fz_wrench", "wb"), z * t)

t = np.load('A_rot')
print "h wrench to thrusts: ", x * t
print "p wrench to thrusts: ", y * t
print "r wrench to thrusts: ", z * t

np.save(open("tx_wrench", "wb"), x * t)
np.save(open("ty_wrench", "wb"), y * t)
np.save(open("tz_wrench", "wb"), z * t)
