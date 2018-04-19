#!/usr/bin/env python3

import os
import argparse
import tornado.web
from tornado.web import url

from handlers.index import IndexHandler
from handlers.login import LoginHandler
from handlers.drive import DriveHandler, ZeroHandler, MovementHandler, VelocityHandler
from handlers.test import TestHandler, TestRunHandler, ThrusterList, ActuatorList
from handlers.shm import SHMHandler, SHMSocketHandler
from handlers.status import StatusHandler
from handlers.deadman import DeadmanHandler
from handlers.admin import AdminHandler, KillHandler
from handlers.vision import VisionIndexHandler, VisionModuleHandler, VisionSocketHandler, VisionActiveModulesHandler

DEFAULT_PORT = 8080
SECRET_KEY = "AUV_WEBSERVER"

def make_app(debug=False):

    settings = {
        "static_path": os.path.join(os.path.dirname(os.path.realpath(__file__)), "static"),
        "template_path": os.path.join(os.path.dirname(os.path.realpath(__file__)), "templates"),
        "cookie_secret": SECRET_KEY
    }
    application = tornado.web.Application([
        url(r"/", IndexHandler, name="index"),
        url(r"/login", LoginHandler, name="login"),
        url(r"/deadman", DeadmanHandler, name="deadman"),
        url(r"/drive", DriveHandler, name="drive"),
        url(r"/drive/(zero|0)", ZeroHandler, name="drive_zero"),
        url(r"/drive/movement/([x|y|z|h])/(-?\d*\.?\d+)", MovementHandler, name="drive_movement"),
        url(r"/drive/velocity/([x|y])/(-?\d*\.?\d+)", VelocityHandler, name="drive_velocity"),
        url(r"/test", TestHandler, name="test"),
        url(r"/test/ws", TestRunHandler, name="test websocket"),
        url(r"/test/thruster/list", ThrusterList, name="thruster list"),
        url(r"/test/actuator/list", ActuatorList, name="actuator list"),
        url(r"/shm", SHMHandler, name="shm"),
        url(r"/shm/ws", SHMSocketHandler, name="shm websocket"),
        url(r"/status", StatusHandler, name="status"),
        url(r"/static/(.*)", tornado.web.StaticFileHandler, {"path": "static"}),
        url(r"/admin", AdminHandler, name="admin"),
        url(r"/admin/kill", KillHandler, name="admin_kill"),
        url(r"/vision", VisionIndexHandler, name="vision_index"),
        url(r"/vision/([^/]+)", VisionModuleHandler, name="vision_module"),
        url(r"/vision/modules/active", VisionActiveModulesHandler, name="vision active modules"),
        url(r"/vision/ws/([^/]+)", VisionSocketHandler, name="vision websocket"),
        url(r"/(favicon.ico)", tornado.web.StaticFileHandler, {"path": "static"})
    ], debug=debug, **settings)
    return application

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='auv-webserver')
    parser.add_argument('-q', '--quiet', help="Silence all output to stdout and stderr", action='store_true')
    parser.add_argument('-p', '--port', help="Port to listen for requests", type=int, default=DEFAULT_PORT)
    parser.add_argument('-d', '--debug', help="Enable debugging", action='store_true')
    args = parser.parse_args()

    print("AUV Webserver 0.0.0.0:{}... waiting for requests".format(args.port))

    auv_webserver = make_app(debug=args.debug)
    auv_webserver.listen(args.port)
    io_loop = tornado.ioloop.IOLoop.instance()

    try:
        io_loop.start()
    except:
        io_loop.stop()
        print("AUV Webserver is stopping...")
