import json
import threading
import tornado.websocket
import shm

class SwitchWatcherThread(threading.Thread):
    def __init__(self, lock, watchers, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        self.lock = lock
        self.watchers = watchers

    def run(self):
        switch_watcher = shm.watchers.watcher()
        switch_watcher.watch(shm.switches)
        while True:
            with self.lock:
                if len(self.watchers) == 0:
                    break
            switch_watcher.wait()
            msg = json.dumps({
                "soft_kill": shm.switches.soft_kill.get(),
                "hard_kill": shm.switches.hard_kill.get(),
            })
            with self.lock:
                for ws in self.watchers:
                    ws.write_message(msg)

class StatusHandler(tornado.websocket.WebSocketHandler):

    ws_clients_lock = threading.Lock()
    ws_clients = set()
    ws_updater = None

    def open(self):
        with self.ws_clients_lock:
            self.ws_clients.add(self)
        if self.ws_updater == None or not self.ws_updater.is_alive():
            self.ws_updater = SwitchWatcherThread(self.ws_clients_lock, self.ws_clients, daemon=True)
            self.ws_updater.start()

    def on_message(self, message):
        msg = json.dumps({
            "soft_kill": shm.switches.soft_kill.get(),
            "hard_kill": shm.switches.hard_kill.get(),
        })
        self.write_message(msg)

    def on_close(self):
        with self.ws_clients_lock:
            self.ws_clients.remove(self)
