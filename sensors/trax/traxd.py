#!/usr/bin/env python2

import sys
import struct
import signal
import math
import binascii
import serial
import time

# frame ID for trax commands in base 10
frameID = { "kGetModInfo"         : 1,
            "kGetModInfoResp"     : 2,
            "kSetAcqParams"       : 24,
            "kSetAcqParamsDone"   : 26,
            "kGetAcqParams"       : 25,
            "kSetConfig"          : 6,
            "kSetConfigDone"      : 19,
            "kSetResetRef"        : 110,
            "kSetDataComponents"  : 3,
            "kGetData"            : 4,
            "kGetDataResp"        : 5,
            "kSetFunctionalMode"  : 79,
            "kSetFIRFilters"      : 12,
            "kSetFIRFiltersDone"  : 20,
            "kStartCal"           : 10,
            "kStopCal"            : 11,
            "kTakeUserCalSample"  : 31,
            "kUserCalSampleCount" : 17,
            "kUserCalScore"       : 18,
            "kStartContinuousMode": 21,
            "kStopContinuousMode" : 22,
            "kSave"               : 9,
            "kSaveDone"           : 16
}

configID = {"kDeclination"          : 1,
            "kTrueNorth"            : 2,
            "kMountingRef"          : 10,
            "kUserCalNumPoints"     : 12,
            "kUserCalAutoSampling"  : 13,
            "kHPRDuringCal"         : 16,
}

struct_formats = {bool      : "?",
                  int       : ">I", # uint32
                  float     : ">f", # float32
                  "uint8"   : ">B",
}

# TODO: try pyserial's flushInput and flushOutput to get rid of junk data

try:
    import crc16
except ImportError:
    print "The python crc16 module is needed to run the trax sensor daemon"
    print "http://pypi.python.org/pypi/crc16"
    sys.exit(1)

import shm

class Trax(object):
    ''' Below are the packets (list of bytes) to send to the sensor
        the first byte is the frameID, and the rest is the payload
        see the FieldSensor Trax User Manual for more details '''

    kGetConfig = (7, 1) # get declination (id = 1)

    kSetDataComponents_heading = (5, 79) # just heading  and status
    kSetDataComponents_all = (5, 79, 24, 25, 76, 75, 74, \
                              7, 8, 9, 21, 22, 23, 27, 28, 29, 77)

    kSetFIRFilters_0 = (12, 3, 1, 0) # no FIR taps

    def __init__(self, port, baud=38400):
        self.port = serial.Serial(port, baud, timeout=0.5)
        self.port.flush()

    def write_frame(self, frame, response=None):
        ''' writes a frame to the trax
            frame is a list of byte values in decimal
            the byte count and crc are both uint16 and
            are transmitted in Big Endian.
            Response is the frameID of the expected response packet '''

        # don't forget the byte count and crc (2 bytes each)
        num_of_bytes = len(frame) + 4

        byte_count = struct.pack(">H", num_of_bytes) # Big Endian
        byte_string = struct.pack("%dB" % len(frame), *frame)
        # the checksum includes the bytecount
        crc = struct.pack(">H", self.checksum(byte_count + byte_string))

        self.port.write(byte_count + byte_string + crc)

        if response is not None:
            f = self.read_frame()
            if ord(f[0]) != response:
                print "ERROR: expected frameID of %d but got %d" % (response, \
                                                                    ord(f[0]))
                return False
            else:
                print "Successfully got response of frameID %d" % response,
                print "from request with frameID %d" % frame[0]

        return True

    def read_frame(self):
        ''' returns one frame from without the byte count and checksum 
            by default the trax return data in Big Endian '''
        byte_count_packet = self.read(2)
        if byte_count_packet:  # only if it didn't timeout
            byte_count = struct.unpack(">H", byte_count_packet)[0]
            #print "Byte_count:", byte_count
            # byte count includes 2 byte checksum and 2 byte byte count
            frame = self.read(byte_count - 4)
            checksum = struct.unpack(">H", self.read(2))[0]  # 2 byte checksum
            our_checksum = self.checksum(byte_count_packet + frame)
            if checksum != our_checksum:
                print "Error: Checksum FAIL!"

            return frame
        else:
            print "Timed out on byte count packet: no data received from TRAX"
            sys.exit(0)
    
    def read(self, n=0):
        if not n:
            return self.port.read()
        else:
            return self.port.read(n)
    

    def checksum(self, byte_string):
        ''' the xmodem crc16 appears to match the TRAX version... '''
        return crc16.crc16xmodem(byte_string)

    def flush(self):
        ''' discard any data coming from trax '''
        self.stop() # stop continuous mode just in case
        flushed = 0
        while 1:
            f = self.read()

            if not f:  # we timed out; there must be no more data
                break

            flushed += 1

        return flushed

    def get_version(self):
        ''' for some reason junk data is send after the initial write
            so we write a command, flush everything, and start fresh '''
        for i in range(3): # this is necessary when restarting daemon
            print "Flushing",
            self.write_frame((frameID["kGetModInfo"],))
            print self.flush()

        self.write_frame((frameID["kGetModInfo"],), frameID["kGetModInfoResp"])

    def get_acq_params(self):
        self.write_frame((frameID["kGetAcqParams"],))
        print binascii.b2a_hex(self.read_frame())

    def get_config(self):
        self.write_frame(Trax.kGetConfig)
        print binascii.b2a_hex(self.read_frame())

    def set_functional_mode(self, mode):
        ''' mode = 0 for compass
            mode = 1 for ahrs '''
        self.write_frame((frameID['kSetFunctionalMode'], mode))

    def set_acq_params(self, mode, flush_filter, sample_delay=0):
        self.write_frame([frameID["kSetAcqParams"], mode, flush_filter, \
                          0, 0, 0, 0] + self.pack_value(sample_delay), \
                          frameID["kSetAcqParamsDone"])

    def pack_value(self, value, val_type=None):
        if val_type == None:
            pack_format = struct_formats[type(value)]
        else:
            pack_format = struct_formats[val_type]
        return [ord(x) for x in struct.pack(pack_format, value)]

    def set_config(self, config_id, value, value_type=None):
        frame = [frameID["kSetConfig"], config_id] + self.pack_value(value, val_type=value_type)
        self.write_frame(frame, frameID["kSetConfigDone"])


    def setup_continuous_mode(self):
        self.set_acq_params(0, 0) # acqmode = 0, flushFIRfilter = False

        # choose what data we want outputted, see top of class
        self.write_frame([frameID['kSetDataComponents']] + self.pack_value(len(Trax.kSetDataComponents_all), val_type="uint8") + list(Trax.kSetDataComponents_all))

    def setFIR(self):
        self.write_frame(Trax.kSetFIRFilters_0, frameID["kSetFIRFiltersDone"])

    def start(self):
        '''
        The Trax has two modes: Compass and AHRS
        In compass mode only the accelerometer and magnetometer are used
        In AHRS mode a Kalman filter is used to fuse the outputs from the
        accelerometers, magnetometers, AND gyroscopes. This means no gyro rates
        in compass mode
        '''
        self.write_frame((frameID["kSetResetRef"],))
        self.set_functional_mode(1)  # compass mode = 0, AHRS = 1
        #self.setFIR()

        self.set_config(configID["kMountingRef"], 4, value_type="uint8") # Mounting using STD 90 (see user manual)

        # Set magnetic declination to have an accurate north direction
        # XXX: THIS WILL HAVE TO BE CHANGED FOR SAN DIEGO
        # See this website for reference: http://magnetic-declination.com/
        #self.set_config(configID["kDeclination"], -67.58 + 90) # Declination for Ithaca, NY
        #self.set_config(configID["kDeclination"], 57.82 + 90) # Declination for San Diego, CA

        # XXX; true north disabled due to interference with quaternion output
        # Instead, we will deal with declination offset higher up (along with pitch & roll zero)
        self.set_config(configID["kTrueNorth"], False) # Use true north reference

        self.write_frame((frameID["kStartContinuousMode"],))
        while 1: 
            f = self.read_frame()
            if ord(f[0]) == 5:   # kGetDataResp has frameID = 5
                # each value in the bytestring has an uint8 ID preceding it
                data = struct.unpack(">BBBfBBBfBfBfBfBfBfB?B?BfBfBfBfBfBfBffff", f)

                shm.trax.heading.set(data[3]) 
                shm.trax.heading_status.set(data[5]) # heading uncertainty
                shm.trax.pitch.set(data[7])
                shm.trax.roll.set(data[9])

                # rad/s -> deg/s
                shm.trax.heading_rate.set(math.degrees(data[11]))
                shm.trax.pitch_rate.set(math.degrees(data[13]))
                shm.trax.roll_rate.set(math.degrees(data[15]))

                shm.trax.temperature.set(data[17]) # in Celsius
                shm.trax.distortion.set(data[19]) 
                shm.trax.calibrated.set(data[21]) # user calibrated?

                # G -> m/s
                shm.trax.accelx.set(data[23] * 9.8)
                shm.trax.accely.set(data[25] * 9.8)
                shm.trax.accelz.set(data[27] * 9.8)

                # in micro Tesla
                shm.trax.magx.set(data[29])
                shm.trax.magy.set(data[31])
                shm.trax.magz.set(data[33])

                # Set quaternion output
                shm.trax.q0.set(data[37])
                shm.trax.q1.set(data[35])
                shm.trax.q2.set(data[36])
                shm.trax.q3.set(data[38])

            else:
                print "Error: Expected frameID of %d but got %d" % (5, ord(f[0]))

    def calibrate(self):
        # Alternatively, TRAX can be calibrated using the Trax Studio software.
        # See this page for information about how to forward Trax's serial port
        # over TCP: https://cuauv.org/wiki/Software/usbip
        print "TRAX CALIBRATION"
        # calibration can only be done in compass mode
        self.set_functional_mode(0)
        self.setFIR()

        self.set_config(configID["kUserCalNumPoints"], 12) # of calibration points
        self.set_config(configID["kHPRDuringCal"], False) # no data output

        # 2D calibration (cal_option must be uint32)
        self.write_frame([frameID["kStartCal"]] + self.pack_value(20))
        print "\tBegin 2D Calibration:"
        print "\tRotate the sub slowly in a circle"

        self.port.timeout = None # ensure read I/O does not timeout during cal
        while 1:
            f = self.read_frame()
            if ord(f[0]) == frameID["kUserCalSampleCount"]:
                count = struct.unpack(">I", f[1:5])[0]
                print "\t\t%d out of %d calibration points taken" % (count, 12)

            elif ord(f[0]) == frameID["kUserCalScore"]:
                print "Calibration complete."
                score = struct.unpack(">B6f", f)
                mag_cal_score = score[1]
                accel_cal_score = score[3]
                distribution_score = score[4]
                tilt_error = score[5]
                tilt_range = score[6]

                print "RESULTS:\n\tMagnetic Calibration Score: %f\n \
                                 \tAccel Calibration Score: %f\n \
                                 \tDistributio Error: %f\n \
                                 \tTilt Error: %f\n \
                                 \tTilt Range: %f" % (mag_cal_score, \
                                                      accel_cal_score, \
                                                      distribution_score, \
                                                      tilt_error, \
                                                      tilt_range)
                acceptable = raw_input("Acceptable? ") in ['y', 'Y', 'yes' \
                                                           "Yes", "YES"]
                if acceptable:
                    self.write_frame((frameID["kSave"],), frameID["kSaveDone"])
                    print "Saved"
                else:
                    print "Not Saving"

                break

            else:
                print "Got", ord(f[0])
                pass

    def stop(self):
        self.write_frame((frameID["kStopContinuousMode"],))

    def close(self):
        self.port.flush()
        self.port.close()

def usage():
    print "Usage:\ttraxd PORT_PATH [-c]"
    print "\tExample: traxd /dev/ttyUSB0"
    print "\tExample: traxd /dev/ttyUSB_ragnarok_8 -c (to calibrate)"


if __name__ == "__main__":
    def stop_trax(t):
        print "Stopping Trax daemon..."
        t.stop()
        t.close()
        sys.exit(0)

    if len(sys.argv) < 2:
        usage()
        sys.exit(2)

    try:
        t = Trax(sys.argv[1])
    except serial.serialutil.SerialException:
        print "Could not open port: %s" % sys.argv[1]
        sys.exit(1)

    if len(sys.argv) == 3 and sys.argv[2] == "-c":
        t.calibrate()
        sys.exit(0)

    # Register ^C so that continuous mode is stopped on exit
    signal.signal(signal.SIGINT, lambda signal, frame, t=t: stop_trax(t))
    signal.signal(signal.SIGTERM, lambda signal, frame, t=t: stop_trax(t))

    t.get_version()
    t.setup_continuous_mode()
    t.start()
