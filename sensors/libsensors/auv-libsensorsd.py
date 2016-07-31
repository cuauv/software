#! /usr/bin/python

import subprocess, os, codecs, re
import shm
from time import sleep

command = "sensors"
g = shm.diagnostics_cpu

d = {"VCore":[r"(\d+\.\d+) V  \(", g.vcore],
     "+12V":[r"(\d+\.\d+) V  \(", g.plus_12v],
     "+3.3V":[r"(\d+\.\d+) V  \(", g.plus_3_3v],
     "+5V":[r"(\d+\.\d+) V  \(", g.plus_5v],
     "-12V":[r"(\d+\.\d+) V  \(", g.minus_12v],
     "V5SB":[r"(\d+\.\d+) V  \(", g.v5sb],
     "VBat":[r"(\d+\.\d+) V  \(", g.vbat],
     "CPU Fan":[r"(\d+) RPM  \(", g.fan],
     "M/B Temp":[r"(\d+(\.?\d+)?).C  \(", g.m_b_temp],
     "CPU Temp":[r"(\d+(\.?\d+)?).C  \(", g.temperature],
     "temp3":[r"(\d+(\.?\d+)?).C  \(", g.temp3],
     "vid":[r"(\d+\.\d+) V  \(", g.vid_v]}


for k in d.keys():
    d[k][0] = re.compile(d[k][0])
    
if __name__ == '__main__':
    while 1:
        process = subprocess.Popen(command, stdout=subprocess.PIPE, shell=False)
        process.stdout = codecs.getreader('utf8')(process.stdout)
        os.waitpid(process.pid, 0)
        output = process.stdout.read().split('\n')

        for x in output:
            s = x.split(':')
    
            if len(s) > 1:
                k, v = s
                
                if d.has_key(k):
                    d[k][1].set(float(d[k][0].search(v).group(1)))
        sleep(1)
        
