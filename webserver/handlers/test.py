import json
import threading
import subprocess
import shlex
import io
import fcntl
import os
import tornado.websocket
from webserver import BaseHandler, APIHandler
from control.thrusters import all_thrusters
from conf.vehicle import actuators

TEST_COMMANDS = set([
    "auv-thruster-test",
    "auv-actuator-test",
    "auv-syscheck",
    "auv-fire-actuator",
    "auv-spin-thruster",
    "cat"
])

class TestHandler(BaseHandler):

    def get(self):
        self.write(self.render_template("test.html"))


class TestRunnerThread(threading.Thread):

    def __init__(self, cmd, ws, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        self.cmd = cmd
        self.ws = ws
        self.process = None

    def write_input(self, msg):
        if self.process:
            bin_input = bytes(msg, 'ascii')
            print("Sending to child: {}".format(bin_input))
            try:
                # manually write to the process stdin instead of using
                # Popen.communicate(), which closes stdin
                self.process.stdin.write(bin_input)
                self.process.stdin.flush()
            except subprocess.TimeoutExpired:
                pass

    def run(self):
        try:
            print("Running '{}'".format(" ".join(self.cmd)))
            self.process = subprocess.Popen(self.cmd, bufsize=0, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            # set stdout to non-blocking
            fd = self.process.stdout.fileno()
            fl = fcntl.fcntl(fd, fcntl.F_GETFL)
            fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NONBLOCK)
            # keep polling for output until the command thread dies
            while self.process.poll() == None:
                line = self.process.stdout.read()
                if line:
                    print(line)
                    data = {"type": self.cmd,
                            "data": line.decode('ascii')}
                    self.ws.write_message(json.dumps(data))
            # send EOF to notify frontend that it should reprompt
            print("Finished running '{}'".format(" ".join(self.cmd)))
            self.ws.write_message(json.dumps({
                "type": "EOF",
                "data": ""
            }))
        except tornado.websocket.WebSocketClosedError:
            pass

    def stop(self):
        if self.process:
            self.process.terminate()
            # hopefully, the process terminates, otherwise kill it
            self.process.kill()

class TestRunHandler(tornado.websocket.WebSocketHandler):

    test_runner = None

    def on_message(self, message):
        print(message)
        data = json.loads(message)

        # check for existence of command thread
        if self.test_runner and not self.test_runner.isAlive():
            self.test_runner = None

        # there exists a child thread running the previous command
        # send this current message as stdin to the previous command
        if self.test_runner:
            self.test_runner.write_input(data.get("data"))

        # create a new child thread to run command
        else:
            cmd = data.get("type")
            try:
                tokens = shlex.split(cmd)
            except ValueError as e:
                self.write_message(json.dumps({
                    "type": "ERROR",
                    "data": "Command error: {}\n".format(e)
                }))
                return

            if tokens[0] in TEST_COMMANDS:
                self.test_runner = TestRunnerThread(tokens, self, daemon=True)
                self.test_runner.start()
            else:
                self.write_message(json.dumps({
                    "type": "ERROR",
                    "data": "Cannot run command: {}\n".format(data.get("type"))
                }))

    def on_close(self):
        if self.test_runner:
            self.test_runner.stop()

class ActuatorList(APIHandler):

    def get(self):
        actuator_names = list(actuators)
        self.respond_success(actuator_names)

class ThrusterList(APIHandler):

    def get(self):
        thruster_names = list(map(lambda t: t.name, all_thrusters))
        self.respond_success(thruster_names)
