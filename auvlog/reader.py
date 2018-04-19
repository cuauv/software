#!/usr/bin/env python3

"""
Log Reader (duh)
"""

import time
import json
import argparse

import nanomsg
import termcolor
import redis

from auvlog import config


parser = argparse.ArgumentParser()
parser.add_argument(
    '-k',
    '--key',
    help='Filter on messages with keys KEY and display the associated value.')
parser.add_argument('-f', '--filename', help='Filter on filename FILENAME.')
parser.add_argument(
    '-d',
    '--delta',
    help='Time delta in the past to display messages for; default 10.',
    default=10,
    type=int)
parser.add_argument(
    '-w'
    '--width',
    help='Maximum displayed width of logging tree (in characters)',
    default=20,
    type=int)
parser.add_argument(
    'tags',
    nargs='*',
    help='Filter on these tags. \'<x>.<y>\' filters for trees with x the parent of '
    'y. \'<x>+<y>\' filters for trees with x and y. All entered patterns will '
    'be captured.')
parser.add_argument(
    '-n',
    '--nocompress',
    action = 'store_true',
    help = 'Do not compress / join output logging messages')
args = parser.parse_args()

# This nonsense wouldn't be necessary if Python closures worked correctly...
def mf(msg):
    if args.filename is not None and msg['filename'] != args.filename:
        return False
    if not (any(all(x in msg['tree'] for x in t.split(
            '+')) for t in args.tags) or len(args.tags) == 0):
        return False
    if (args.key is not None) and (
            not isinstance(msg['message'], dict) or args.key not in msg['message']):
        return False
    if (time.time() - msg['timestamp']) > args.delta:
        return False
    return True

socket = nanomsg.Socket(nanomsg.SUB)
socket.set_string_option(nanomsg.SUB, nanomsg.SUB_SUBSCRIBE, '')
socket.connect("tcp://127.0.0.1:{0}".format(config.SERVER_PORT))

redis = redis.StrictRedis()

def _tree_fmt(tree):
  if len(tree) <= args.w__width:
    return tree.ljust(args.w__width)
  else:
    modw = args.w__width - 5
    pre  = int(modw / 2)
    post = modw - pre
    tree = tree[:pre] + ' ... ' + tree[-post:]
    return tree 

_time_format = lambda t: time.strftime('%Y/%m/%d %H:%M:%S', time.localtime(t))

_fmt = lambda tree, timestamp, msg, filename, lineno, block, linetxt: \
    '{0} [{1}] {2} {3}'.format(
        termcolor.colored(_tree_fmt(tree), 'red'),
        termcolor.colored('{0}'.format(_time_format(timestamp)), 'green'),
        '({0}:{1} ~ {2})'.format(
            termcolor.colored('/'.join(filename.split('/')[-2:]), 'cyan'),
            termcolor.colored(str(lineno), 'magenta'),
            termcolor.colored(block, 'blue')
        ).ljust(60),
        msg
    )

last = None
last_text = None
similarity_threshold = 0.5

def sim(x, y):
  xw = set(x.split(' '))
  yw = set(y.split(' '))
  return len(xw & yw) / len(xw | yw)

def handle(msg):
    global last, last_text
    if mf(msg):
        text = msg['message'][
            args.key] if args.key is not None else msg['message']
        eqv = all([not args.nocompress, last is not None and len(last['tree']) == len(msg['tree']) and all([x == y for (x, y) in zip(last['tree'], msg['tree'])])
                  , last_text is not None and sim(last_text, text) > similarity_threshold])
        last = msg
        last_text = text
        if eqv:
          print('\033[F', end = '')
        print(
            _fmt(
                msg['tree'],
                msg['timestamp'],
                text,
                msg['filename'],
                msg['lineno'],
                msg['block'],
                msg['linetxt']))

cached = (json.loads(msg.decode()) for msg in redis.lrange(config.KEY, 0, -1))
for message in list(cached)[::-1]:
    handle(message)

while not False:
    message = json.loads(socket.recv().decode('ascii'))
    handle(message)
