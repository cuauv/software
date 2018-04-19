import json, time
import threading
from webserver import BaseHandler
import tornado.websocket
import shm

class SHMGroupWatcherThread(threading.Thread):
    def __init__(self, group, ws, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        self.group = group
        self.ws = ws

    def run(self):
        group = getattr(shm, self.group)
        watcher = shm.watchers.watcher()
        watcher.watch(group)

        self.ws_open = True
        while self.ws_open:
            time.sleep(0.1)
            data = {name: getattr(group, name).get() for name, _ in group._fields}
            try:
                self.ws.write_message(json.dumps({
                    "type": "groupWatch",
                    "data": data
                }))
            except tornado.websocket.WebSocketClosedError:
                print("Websocket already closed")
                self.ws_open = False

shm_groups = shm.__all__[1:]  # "watchers" is the first, hardcoded element which is skipped.
shm_groups.sort()

class SHMSocketHandler(tornado.websocket.WebSocketHandler):

    ws_updater = None

    def on_message(self, message):
        print(message)
        data = json.loads(message)
        if data.get("type") == "groupWatch":
            group = data.get("group")
            self.ws_updater = SHMGroupWatcherThread(group, self, daemon=True)
            self.ws_updater.start()
        elif data.get("type") == "groupList":
            self.write_message(json.dumps({
                "type": "groupList",
                "data": shm_groups
            }))

    def on_close(self):
        if self.ws_updater:
            self.ws_updater.ws_open = False

class SHMHandler(BaseHandler):

    def get(self):
        self.write(self.render_template("shm.html"))

