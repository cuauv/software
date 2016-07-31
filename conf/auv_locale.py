import json
import numpy as np
import os
import sys 

DIR = os.environ.get("CUAUV_SOFTWARE")
if DIR is None:
    sys.stderr.write('locale.py: CUAUV_SOFTWARE must be set '
                     'to the root of the software repository.\n')
    sys.exit(1)

d = None
LOCALE = os.getenv("CUAUV_LOCALE")
if LOCALE is None:
    sys.stderr.write('locale.py: CUAUV_LOCALE must be set to a valid locale file.\n')
    sys.exit(1)

with open(os.path.join(DIR, "conf", "%s.json" % LOCALE)) as f:
    d = json.load(f)

objects = d['objects']
bounds  = d['bounds']
