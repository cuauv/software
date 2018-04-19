import tornado.escape
from webserver import BaseHandler, APIHandler
import shm

class AdminHandler(BaseHandler):

    def get(self):
        self.write(self.render_template("admin.html"))

class KillHandler(APIHandler):

    def post(self):
        try:
            data = tornado.escape.json_decode(self.request.body)
        except:
            data = {}
        if "kill" in data and data["kill"] == 0:
            shm.switches.soft_kill.set(0)
            self.respond_success("Sub un-softkilled!")
        else:
            shm.switches.soft_kill.set(1)
            self.respond_success("Sub softkilled!")
