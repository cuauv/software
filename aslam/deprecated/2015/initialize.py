#!/usr/bin/env python3.4

from model import slam

import json

# mapfile = 'transdec.json'
mapfile = 'hotel.json'
initial = json.loads(open(mapfile, 'rb').read().decode('ascii'))

for obj in ['start', 'gate', 'gate_pipe', 'red_buoy']:
  rec = initial[obj]
  aslam.observe('id', {'x': pobject(obj)}, vectorr([realr(rec['n']), realr(rec['e']), realr(rec['d'])]), 0.2)

