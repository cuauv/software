import shlex
import signal
import subprocess
import sys

class ProcessWrapper:
  """
      Allows you to spawn a list of processes, and kill them all at once
      with wait_and_bind_exit. Kills processes by sending SIGTERM.
  """
  def __init__(self, *cmds):
    self.args = [shlex.split(cmd) for cmd in cmds]

  def run(self):
    self.processes = []
    for args in self.args:
      self.processes.append(subprocess.Popen(args, stdout=sys.stdout,
                                                   stderr=sys.stderr))

  def wait(self):
    for proc in self.processes:
      proc.wait()

  def _sigh(self, sig, frame):
    self.stop()
    # There is an issue here with child processes calling sys.exit
    # that I'm not sure how to fix.
    sys.exit(0)

  def wait_and_bind_exit(self):
    signal.signal(signal.SIGINT, self._sigh)
    signal.signal(signal.SIGTERM, self._sigh)
    self.wait()

  def stop(self):
    for proc in self.processes:
      proc.terminate()
