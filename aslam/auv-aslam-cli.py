#!/usr/bin/env python3

import aslam, shm, sys, termcolor, time, numpy

objects = ['sub']
objects += [obj['name'] for obj in aslam.locale.objects]

if len(sys.argv) == 1:
  print('You must specify an object! Usage: aslam <object> [COMMAND]. Available commands: [ position, watch, reset ]. Available objects: \n{}'.format('\n'.join(['> ' + o for o in objects])))
  sys.exit(1)

obj = sys.argv[1]

if obj not in objects:
  print('Unrecognized object! Available objects: \n{}'.format('\n'.join(['> ' + o for o in objects])))
  sys.exit(1)

cmd = 'position' if len(sys.argv) == 2 else sys.argv[2]

pyobj = aslam.sub if obj == 'sub' else getattr(aslam.world, obj)

def position():
  pos = pyobj.position()
  unc = pyobj.uncertainty()
  print('{}\t{}\n{}\tPosition {}m\tUncertainty {}m\n{}\tPosition {}m\tUncertainty {}m\n{}\tPosition {}m\tUncertainty {}m'.format(
    termcolor.colored('Object', 'white'),
    termcolor.colored(obj, 'cyan', attrs = ['bold']),
    '> North\t',
    termcolor.colored('%.4f' % pos[0], 'green'),
    termcolor.colored('%.4f' % unc[0], 'red'),
    '> East\t',
    termcolor.colored('%.4f' % pos[1], 'green'),
    termcolor.colored('%.4f' % unc[1], 'red'),
    '> Depth\t',
    termcolor.colored('%.4f' % pos[2], 'green'),
    termcolor.colored('%.4f' % unc[2], 'red')
    ))

if cmd == 'position':
  position()

if cmd == 'watch':
  while 1:
    position()
    print('\r\b\r\b\r\b\r\b\r', end = '')
    time.sleep(0.1)

if cmd == 'reset':
  if pyobj is not aslam.sub:
    print('Only the sub\'s position can be reset!')
  else:
    shm.aslam_settings.reset.set(True)
    print('Sub position reset!')

if cmd == 'observe':
  if pyobj is aslam.sub:
    print('You can\'t observe the sub!')
  else:
    while 1:
      heading, pitch, distance = map(float, sys.argv[3:6])
      obs = aslam.Observation(aslam.sub, pyobj, numpy.array([heading, pitch, distance]), numpy.array([0.5, 0.5, 0.5]))
      obs.apply()
      sys.exit(1)
