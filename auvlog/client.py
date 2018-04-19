"""
Logging Client
Compatible with Python 2 & 3
See "example.py" for usage.

(c) Christopher Goes 2015

"""

from __future__ import print_function

import time
import inspect
import json
import sys
import termcolor

from auvlog import config

try:
  import nanomsg
except ImportError:
  nanomsg = False
  print("Nanomsg not installed; central logging unavailable.")
else:
  socket = nanomsg.Socket(nanomsg.PUB)
  socket.connect("tcp://127.0.0.1:{0}".format(config.CLIENT_PORT))

def _log(obj):
  try:
    data = json.dumps(obj)
  except TypeError as e:
    raise e

  if nanomsg:
    socket.send(data)

_fmt = lambda tree, timestamp, message, filename, lineno, block, linetxt: {
    'tree': tree,
    'timestamp': timestamp,
    'message': message,
    'filename': filename,
    'lineno': lineno,
    'block': block,
    'linetxt': linetxt
}

_print_format = lambda tree, timestamp, msg, filename, lineno, block, linetxt: \
    '{0} [{1}] {2} {3}'.format(
        termcolor.colored(tree[-20:].ljust(20), 'red'),
        termcolor.colored('{0:.2f}'.format(timestamp), 'green'),
        '({0}:{1} ~ {2})'.format(
            termcolor.colored('/'.join(filename.split('/')[-2:]), 'cyan'),
            termcolor.colored(str(lineno), 'magenta'),
            termcolor.colored(block, 'blue')
        ).ljust(60),
        msg
    )

_short_format = lambda tree, timestamp, msg, filename, lineno, block, linetxt: \
    '{0}: {1}'.format(
        termcolor.colored(tree[-25:].ljust(25), 'red'),
        msg
    )

class Logger:

    """
    CUAUV central logging utility. An instance of this class will be able to send logs to the logging daemon.

    To use, make an instance of this class. Any instance will log with it's current prefix. A prefix is a tuple of
    strings that can either be passed in as *args, or a level can be added by accessing an attribute of any current
    instance. Any instance can be called with a message to be logged.
    """

    def __init__(self, tree):
        self.tree = tree

    def __call__(self, message, copy_to_stdout=False, copy_to_file=None):
        frame = inspect.stack()[1]
        formatted = _fmt(
            '.'.join(self.tree),
            time.time(),
            message,
            frame[1],
            frame[2],
            frame[3],
            frame[4][0] if frame[4] is not None else ''
        )
        #not sure why, but this call sometimes freezes without this.
        time.sleep(0)

        _log(formatted)

        if copy_to_stdout or copy_to_file:
            short_format = _short_format(
                formatted['tree'],
                formatted['timestamp'],
                formatted['message'],
                formatted['filename'],
                formatted['lineno'],
                formatted['block'],
                formatted['linetxt']) + '\n'

            if copy_to_stdout:
                sys.stdout.write(short_format)

            if copy_to_file:
                with open(copy_to_file, "a") as f:
                    f.write(short_format)

    def __repr__(self):
        return 'Logger<tree:{0}>'.format(self.tree)

    def __getattr__(self, key):
        return Logger(self.tree + [key])


log = Logger([])
