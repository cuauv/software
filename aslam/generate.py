#!/usr/bin/env python3

import shutil
import filecmp
import os
import json

from lib import pyratemp

from files import files

os.chdir('aslam')

config = json.load(open('aslam.json'))
sub, objects, observables = config['submarine'], config['objects'], config['observables']

sub['state_type'] = 'Vec' + str(sub['dimensionality'])
sub['covariance_type'] = 'Mat' + str(sub['dimensionality'])
sub['shm_group'] = 'aslam_sub'

for o in objects:
  o['state_type'] = 'Vec' + str(o['dimensionality'])
  o['covariance_type'] = 'Mat' + str(o['dimensionality'])
  o['shm_group'] = 'aslam_' + o['name']

dimensions = ['north', 'east', 'depth', 'orientation']

def gen(f_in, f_out):
  f_tmp = '%s.swp' % f_out
  t = pyratemp.Template(filename = f_in)
  out = open(f_tmp, 'w')
  out.write(t(sub = sub, objects = objects, observables = observables, dimensions = dimensions))
  out.close()
  if os.path.exists(f_out):
    if filecmp.cmp(f_tmp, f_out):
      os.remove(f_tmp)
      return
  shutil.move(f_tmp, f_out)

for f in files:
  gen('src/templates/%s' % f, 'src/%s' % f)
