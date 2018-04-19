#!/usr/bin/env auv-python2
'''
Automatically calibrates the linearization of the heading readings.

1. Spin motors at constant rate to create a constant rotational spin
2. Headings are recorded periodically throughout the rotation
3. Data is trimmed to a single rotation
4. An inverse mapping is derived taking read headings to the actual headings.
5. The inverse mapping is stored as a LUT in a file
'''

from time import sleep
import sys
import shm
LUTFileName = "linear_heading_LUT"

MOTOR_SPEED = 55

def cmp_headings(h1, h2):
    '''returns 1,0,-1 as comparison of the headings
    -1 is h1 < h2, 1 is h1 > h2'''
    if (h1-h2) % 360. == 0:
        return 0
    elif (h1-h2) % 360. < 180:
        return 1
    else:
        return -1

def StartRotating():
    reading = shm.dvl.heading
    port = shm.motor_desires.port
    starboard = shm.motor_desires.starboard
    port.set(MOTOR_SPEED)
    starboard.set(-MOTOR_SPEED)

    print "Powering up to full rotational speed."

    for i in range(10):
        sleep(1)
        print ".",
        sys.stdout.flush()

def TakeReadings():
    reading = shm.dvl.heading
    port = shm.motor_desires.port
    starboard = shm.motor_desires.starboard
    readings = [reading.get()]
    halfway = False
    for i in range(500):#Arbitrary maximum
        sleep(0.1)
        latest = reading.get()
        if i % 10 == 0:
            print ".",
            sys.stdout.flush()

        diff = (latest - readings[0]) % 360

        if diff > 180 and not halfway:
            halfway = True

        if diff < 180 and halfway == True:
            if( i < 10 ):
                raise Exception, "Either rotating in -heading direction or rotating too quickly."
            print "Readings taken. Done"
            port.set(0)
            starboard.set(0)
            return readings

        readings.append(latest)

    print "Readings failed. Sub did not rotate in complete circle. Timed out."
    raise Exception, "Readings failed"

def ProcessReadings(readings):
    '''Processes list of readings into useable data.'''
    step = 360. / len(readings)
    readings = [(reading, i*step) for i, reading in enumerate(readings)]

    #Remove anything that isn't strictly increasing
    processed = [readings[0]] 
    lastR, lastH = readings[0]
    for r,h in readings:
        if cmp_headings(r,lastR) == 1:
            processed.append( (r,h) )
            lastH = h
            lastR = r

    return processed

def Invert(reading, readings):
    '''Takes an actual heading and determines which reading
    corresponded to it by looking in the list of readings.
    readings is a list of pairs (reading, heading) from 0 to 360.'''

    #Find where the readings cross over the heading
    n = 0
    below = False
    for i,(r,h) in enumerate(readings):
        if cmp_headings(r, reading) <= 0:
            below = True
        if cmp_headings(r,reading) >= 0 and below == True:
            break
        n = i
    belowR,belowH = readings[n]
    
    aboveR,aboveH = readings[(n+1)%len(readings)]

    coeff = ((aboveH-belowH)%360)/((aboveR-belowR)%360.)
    return ((reading-belowR)%360)*coeff + belowH

def BuildLUT(headings):
    return [Invert(i, headings) for i in range(360)] + [Invert(0, headings)]

def WriteLUT(lut):
    outFile = file(LUTFileName, "w")
    for i in lut:
        outFile.write(str(i)+"\n")
    outFile.close()

if __name__ == '__main__':
    StartRotating()
    print "Acheived max speed"
    raw_input("Press enter to start calibration with current heading at zero")
    readings = TakeReadings()
    processed = ProcessReadings(readings)
    print processed 
    lut = BuildLUT(processed)
    WriteLUT(lut)
    print "LUT generated and written. Calibration successful."
