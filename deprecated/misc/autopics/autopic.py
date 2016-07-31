import facebook
import sys
from auval.watcher import *

YOUR_API_KEY = '34fc282cb0e81f80666828404745b31c'
YOUR_SECRET_KEY = '1aabc0c42c09234bf77b16ec7c0df825'

SESSION_KEY = 'f6a8c6f1b8974d5e35cac528-411158'
SESSION_SECRET = 'fde8e3fc97ce96b6e89b28b10d571b94'

fb = facebook.Facebook(YOUR_API_KEY, YOUR_SECRET_KEY)
fb.session_key = SESSION_KEY
fb.secret = SESSION_SECRET

box_prob = SharedVar('/vision/box/results/probability')
box_enabled = SharedVar('/vision/box/settings/enabled')
downward_path = SharedVar('/vision/camera/downward')
watcher = VarWatcher(box_prob)

box_enabled.set(True)

while 1:
    watcher.wait_for_change()
    watcher.watch_var(box_prob)
    if box_prob.get() > 0.9 and box_enabled.get():
        box_enabled.set(False)
        box_prob.set(0.0)
        res = fb.photos.upload(downward_path.get())
        print "Photo uploaded!"
        print res['link']
        sys.exit()
    else:
        print "Not uploaded, prob is only %f" % box_prob.get()
