#!/usr/bin/env python3

"""
Logging Daemon

(c) Christopher Goes 2015
"""

import nanomsg
import redis
import os

from auvlog import config

redis_client = redis.StrictRedis()

socketS = nanomsg.Socket(nanomsg.SUB)
socketS.set_string_option(nanomsg.SUB, nanomsg.SUB_SUBSCRIBE, '')
socketS.bind("tcp://*:{0}".format(config.CLIENT_PORT))

socketP = nanomsg.Socket(nanomsg.PUB)
socketP.bind("tcp://*:{0}".format(config.SERVER_PORT))


def push_to_file(entry):
    try:
        f = open(os.path.join(os.environ['CUAUV_LOG'], 'current/auvlog.log'), 'ab')
        f.write(entry + b'\n')
    except IOError:
        print('Warning: Unable to write log entry to file.', flush=True)


while True:
    res = socketS.recv()
    try:
        redis_client.lpush(config.KEY, res)
        redis_client.ltrim(config.KEY, 0, int(1e5))
    except redis.exceptions.ConnectionError:
        print('Warning: Unable to connect to redis instance', flush=True)
    push_to_file(res)
    socketP.send(res)
