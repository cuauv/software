#!/usr/bin/env python3

import sqlite3
from datetime import datetime
from table import Table
import sys
import os

DB_FILE = os.path.join(os.environ['CUAUV_LOG'], "uptime.sqlite")
conn = sqlite3.connect(DB_FILE, check_same_thread = False, detect_types=sqlite3.PARSE_DECLTYPES|sqlite3.PARSE_COLNAMES)
c = conn.cursor()
c.execute("SELECT * FROM utlog ORDER BY id DESC")

STIME = "%m/%d/%y %I:%M:%S %p"

entries = [("Start", "Stop", "Uptime", "Test time")]

if len(sys.argv) >= 2:
    try:
        count = int(sys.argv[1])
    except:
        print("Usage: view.py [number of elements]")
        sys.exit(1)
else:
    count = 5 #default count to display

def hms(sec):
    h, r = divmod(sec, 3600)
    m, s = divmod(r, 60)
    return "%d:%02d:%02d" % (h, m, s)

for i in range(count):
    element  = c.fetchone()
    if element is None:
        count = i
        break
    (id, st, end, time) = element
    dt = end - st
    running = abs((datetime.now() - end).seconds) < 2
    entries.append((st.strftime(STIME), end.strftime(STIME) if not running else "[Active]", hms(dt.seconds), hms(time)))

c.execute("SELECT COUNT(*) FROM utlog")
all_count, = c.fetchone()

print("Displaying %d most recent uptime entries (%d entries total)" % (count, all_count))
print(Table(tuple(entries)).create_table())

