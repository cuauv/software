#!/usr/bin/env python2

'''
Plotting utility for output from uptime logs
'''

import sqlite3
from time import strptime, mktime
import datetime

DB_FILE = "/home/thomas/cuauv/uptime.sqlite" #Local sqlite3 database file for uptime information

conn = sqlite3.connect(DB_FILE)

str2time = lambda s: strptime(s[:s.index('.')], "%Y-%m-%d %H:%M:%S")

with conn:
    cur = conn.cursor()
    cur.execute("SELECT * FROM utlog")
    data = cur.fetchall()
    #Data is in formation:
    # id, start-time, end-time, in-water time
    

data = [(id, str2time(start_time), str2time(end_time), in_water)
            for (id, start_time, end_time, in_water) in data]

# hour-by-hour plot
import numpy as np
hours_time = np.zeros((24,))
hours_tot = np.zeros((24,))
for id,start,end,time in data:
    elapsed =  (mktime(end) - mktime(start)) / (60*60.)
    in_water = time / (60 * 60.)
    num_hours = int(elapsed) +  1
    for h in range(start.tm_hour, start.tm_hour+num_hours):
        hours_time[h%24] += in_water / num_hours
        hours_tot[h%24] += elapsed / num_hours

import pylab
fig = pylab.figure()
ax = fig.add_subplot(111)
ax.bar(np.arange(0,24),hours_tot,color="g",label="total uptime")
ax.bar(np.arange(0,24),hours_time,color="b",label="in-water")
ax.set_xticks(np.arange(0,24)+0.5)
ax.set_title("Ragnarok Testing Time by time of day")
hours_of_the_day = ["12 AM"] + ["%d AM"%h for h in np.arange(1,12)] + ["12 PM"] + ["%d PM"%h for h in np.arange(1,12)]
ax.set_xticklabels(hours_of_the_day, rotation='vertical')
ax.legend()
fig.savefig("uptime_by_hour.png")

# Over time plot
days = []
times = []
tot_times = []
for id,start,end,time in data:
    day = datetime.date(*start[:3])
    elapsed =  (mktime(end) - mktime(start)) / (60*60.)
    in_water = time / (60 * 60.)
    if len(days) == 0.0 or days[-1] != day:
        days.append(day)
        times.append(in_water)
        tot_times.append(elapsed)
    else:
        times[-1] += in_water
        tot_times[-1] += elapsed 

fig2 = pylab.figure()
ax2 = fig2.add_subplot(111)
ax2.set_title("Ragnarok Testing Time by day")
ax2.bar(days, tot_times, color="g", label="total uptime")
ax2.bar(days, times, label="in-water")
ax2.legend()
fig2.savefig("uptime_by_day.png")

pylab.show()
