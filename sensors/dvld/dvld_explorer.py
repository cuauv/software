import serial
import argparse
from time import sleep
import shm

parser = argparse.ArgumentParser(description="DVL Daemon (Python Edition)")
parser.add_argument('port',
                    type=str,
                    help="which serial port to read (eg: /dev/ttyUSB0)")
args = parser.parse_args()

ser = serial.Serial(port=args.port)
#print "Opening serial port:", ser.port

##### Setup
# List of setup commands to be run at the start
setup = ["CRO",
         "PD6",
         "TP00:00.00",
         "TE00:00:00.00",
         "BP001",
         "CF11110",
         "CB411",
         "EA-04500",
         "EZ11011010",
         "EX10111",
         "#BS",
         "ES00",
         "CS"]
# Run setup
ser.sendBreak(0.3)
for cmd in setup:
    ser.write(cmd + "\n")
    sleep(0.1)
ser.flush()

##### Data definitions
# Packet type definitions
formats = dict(
        SA = (float,float,float),
        TS = (int,float,float,float,float,int),
        BI = (float,float,float,float,str),
        BS = (float,float,float,str),
        BE = (float,float,float,str),
        BD = (float, float, float, float, float) )

#which variables to write to
outputs = dict( SA = ('heading', 'pitch', 'roll'),
                TS = ('time', 'salinity', 'temperature', 'depth',
                      'speed_of_sound', 'build_in_test'),
                BI = ('transverse_vel', 'longitudinal_vel',
                      'normal_vel', 'vel_status'),
                BS = ('transverse_vel_ship', 'longitudinal_vel_ship',
                      'normal_vel_ship', 'vel_status_ship'),
                BE = ('east_vel', 'north_vel', 'upward_vel', 'vel_status_earth'),
                BD = ('east_dist', 'north_dist', 'upward',
                      'range_to_bottom', 'time_since_last_good')
                )
class data(object):
    pass
output = data()
for cmd in outputs:
    for fmt, name in zip(formats[cmd],outputs[cmd]):
        default = fmt()
        output.__setattr__(name, default)

# Read loop
while True:
    line = ser.readline()
    print line

    packet_type = line[1:3]
    if packet_type in outputs:
        args = line[4:].split(',')
        try:
            for arg, fmt, out in zip(args, formats[packet_type], outputs[packet_type]):
                output.__setattr__(out, fmt(arg))
        except:
            print "invalid packet data", arg

        print "Good packet"
    else:
        print "Unrecognized type,", packet_type

    # write outputs
    shm.dvl.depth.set(output.depth)
    shm.dvl.temperature.set(output.temperature)
    shm.dvl.velocity_x.set(output.transverse_vel)
    shm.dvl.velocity_y.set(output.longitudinal_vel)
    shm.dvl.velocity_z.set(output.normal_vel)
    shm.dvl.low_correlation_1.set(1 if output.vel_status == "V" else 0)
    shm.dvl.savg_altitude.set(output.range_to_bottom)

ser.close()

