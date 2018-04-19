import os
import sys
import watchdog
import watchdog.events
import watchdog.observers
import auvlog.client

log = auvlog.client.log.misc

def reload_self():
  args = sys.argv[:]
  args.insert(0, sys.executable)

  log('Restarting {}'.format(' '.join(args)), copy_to_stdout = True)
  os.execv(sys.executable, args)

def detect_changes_to_self(callback):
  fn    = sys.argv[0]
  direc = os.getcwd()
  path  = os.path.join(direc, fn)
  log('Watching path {} for changes'.format(path), copy_to_stdout = True)
  class Handler(watchdog.events.FileSystemEventHandler):
    def on_modified(self, evt):
      equal = os.path.abspath(evt.src_path) == os.path.abspath(path)
      if equal:
        log('Detected change on path {}.'.format(path))
        callback()

  handler = Handler()
  obs = watchdog.observers.Observer()
  obs.schedule(handler, os.path.dirname(path))
  obs.start()
