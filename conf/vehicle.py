import json
import numpy as np
import os
import sys

DIR = os.environ.get("CUAUV_SOFTWARE")
if DIR is None:
    sys.stderr.write("vehicle.py: CUAUV_SOFTWARE must be set "
                     "to the root of the software repository.\n")
    sys.exit(1)

d = None
VEHICLE = os.getenv("CUAUV_VEHICLE")
VEHICLE_TYPE = os.getenv("CUAUV_VEHICLE_TYPE")

if VEHICLE is None or not VEHICLE in ["odysseus", "ajax"]:
    sys.stderr.write("vehicle.py: CUAUV_VEHICLE must be set "
                     "to one of { odysseus, ajax }.\n")
    sys.exit(1)
if VEHICLE_TYPE is None or not VEHICLE_TYPE in ["mainsub", "minisub"]:
    sys.stderr.write("vehicle.py: CUAUV_VEHICLE_TYPE must be set "
                     "to one of { mainsub, minisub }.\n")
    sys.exit(1)

is_mainsub = VEHICLE_TYPE == "mainsub"
is_minisub = VEHICLE_TYPE == "minisub"

with open(os.path.join(DIR, "conf", "%s.json" % VEHICLE)) as f:
    d = json.load(f)

center_of_buoyancy = np.array(d['center_of_buoyancy'])
buoyancy_force = d['buoyancy_force']
gravity_force = d['gravity_force']
sub_height = d['sub_height']
dvl_offset = d['dvl_offset']  # TODO
I = np.array(d['I'])
thrusters = d['thrusters']
sensors = d['sensors']
gx_hpr = d['gx_hpr']
measurement_error = d['measurement_error']
control_settings = d['control_settings']
quaternion_filtering = d['quaternion_filtering']

actuators = {}
if 'actuators' in d:
  actuators = d['actuators']

dvl_present = d['dvl_present']

# note: the inherit from object is needed for Python 2 compatibility
# (it makes it a 'new-style' object, which is the default in Python 3)
class DragPlane(object):
    def __init__(self, pos, normal, cD, area):
        self.pos = pos
        self.n = normal
        self.cD = cD
        self.area = area

        self.torque_hat = np.cross(self.pos, self.n)

drag_planes = []
for dp in d['drag_planes']:
    drag_planes.append(DragPlane(np.array(dp['pos']), np.array(dp['normal']), dp['cD'], dp['area']))

components = d['components']
try:
  cameras = d['cameras']
except KeyError:
  print("WARNING: Vehicle %s is missing camera configuration." % VEHICLE)
