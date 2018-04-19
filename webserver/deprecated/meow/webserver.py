#!/usr/bin/env python3
import os
import sys
import time
from collections import defaultdict
from threading import Thread

import eventlet
from flask import Flask, render_template
from flask_socketio import SocketIO, join_room, leave_room

import shm

eventlet.monkey_patch()

software_path = os.environ['CUAUV_SOFTWARE']
template_path = os.path.join(software_path, 'meow/templates')
static_path = os.path.join(software_path, 'meow/static')

app = Flask(__name__, template_folder=template_path, static_folder=static_path)
app.config['SECRET_KEY'] = 'MEOWCHECK!'

if len(sys.argv) > 1 and sys.argv[1] in ['-d', '--debug']:
    app.debug = True

# Temporary for debugging
app.debug = True
socketio = SocketIO(app, async_mode='eventlet')

room_subscribers = defaultdict(int)

shm_groups = shm.__all__[1:]  # "watchers" is the first, hardcoded element which is skipped.
shm_groups.sort()


def out(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


@socketio.on("listGroups")
def on_list_groups(data):
    return shm_groups


def group_watcher(group_name):
    group = getattr(shm, group_name)
    watcher = shm.watchers.watcher()
    watcher.watch(group)

    data = {name: getattr(group, name).get() for name, _ in group._fields}
    socketio.emit("groupUpdate-{}".format(group_name), data, room=group_name)

    while room_subscribers[group_name]:
        # watcher.wait(new_update=True)
        time.sleep(.1)
        data = {name: getattr(group, name).get() for name, _ in group._fields}
        socketio.emit("groupUpdate-{}".format(group_name), data, room=group_name)


@socketio.on("join")
def subscribe_group(data):
    out(data)
    group = data['group']
    join_room(group)
    room_subscribers[group] += 1

    if room_subscribers[group] == 1:
        Thread(target=group_watcher, args=(group,), daemon=True).start()


@socketio.on("leave")
def subscribe_group(data):
    out(data)
    group = data['group']
    leave_room(group)
    room_subscribers[group] -= 1


@app.route("/")
@app.route('/<path:path>/')
def index(path=""):
    return render_template('index.html')


if __name__ == '__main__':
    port = int(os.environ.get("PORT", 5000))
    socketio.run(app, host='0.0.0.0', port=port, use_reloader=False)
