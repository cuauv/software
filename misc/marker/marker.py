#!/usr/bin/env python3

import os
import shm
import json
import aslam
import argparse

parser = argparse.ArgumentParser(description = 'Utility to mark positions of objects and the submarine along with a tag')
parser.add_argument('tag', type = str, help = 'String tag to store along with the position')
parser.add_argument('object',  type = str, help = 'Object to take the position of. Either an object in the locale configuration file or the submarine', default = 'sub')
parser.add_argument('--filename', type = str, help = 'Filename to save the mark to (will be appended to if existent and created otherwise)', default = 'marks.json')

args = parser.parse_args()

grp = getattr(shm, 'aslam_' + args.object)

data = grp.get()

if os.path.exists(args.filename):
  print('Mark file {} already exists; appending mark to it!'.format(args.filename))
  conf = json.load(open(args.filename))
else:
  print('Mark file {} does not exist; creating!'.format(args.filename))
  conf = []

conf.append({
  'tag': args.tag,
  'object': args.object,
  'position': [data.north, data.east, data.depth],
  'uncertainty': [data.north_uncertainty, data.east_uncertainty, data.depth_uncertainty]
})

open(args.filename, 'w').write(json.dumps(conf))

print('Wrote mark of object {} with tag {} at position north {}, east {}, and depth {}!'.format(
  args.object, args.tag, data.north, data.east, data.depth, args.filename
))
