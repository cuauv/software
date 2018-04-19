import json, time
import tornado.websocket

class DeadmanHandler(tornado.websocket.WebSocketHandler):

    def on_message(self, message):
        self.write_message(json.dumps({
            "ping": time.time()
        }))
