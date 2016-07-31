#!/usr/bin/env python2

import filecmp
import os
import shutil
import string

import parse
import pyratemp

if os.getcwd().split('/')[-1] != 'usd':
    os.chdir('usd')

with open('devices.txt', 'r') as f:
    fmt = 'configs/%s.conf'
    CONFIGS = [fmt % line.strip() for line in f if not line.startswith('#')]

def make_blocks(dev):
    dev.vars.sort(key=lambda v: v.address)
    if len(dev.vars) == 0:
        return []

    blocks = []
    start_address = dev.vars[0].address
    expected_address = dev.vars[0].address
    current_size = 0
    last_interval = dev.vars[0].interval

    for v in dev.vars:
        if v.address != expected_address or v.interval != last_interval:
            # Done with current block; append it to list, make a new one
            blocks.append((start_address, current_size, last_interval))
            start_address = v.address
            current_size = 0
            last_interval = v.interval
        current_size += v.size
        expected_address = v.address + v.size
    blocks.append((start_address, current_size, last_interval))

    return blocks

# read a file, replace stuff, write it out
# only overwrite files if the contents will change
def make_file(f_in, f_out, **args):
    f_tmp = '%s.swp' % f_out
    t = pyratemp.Template(filename=f_in)
    with open(f_tmp, 'w') as out:
        out.write(t(**args).encode())
    if os.path.exists(f_out) and filecmp.cmp(f_tmp, f_out):
        # Nothing changed, don't touch the file.
        os.remove(f_tmp)
        return
    shutil.move(f_tmp, f_out)

devs = []

for config_file in CONFIGS:
    with open(config_file, 'r') as f:
        dev = parse.parse(f)
    cpp_name = 'devices/%s.cpp' % dev.name
    h_name = 'devices/%s.h' % dev.name
    make_file('templates/dev.cpp', cpp_name, dev=dev, blocks=make_blocks(dev))
    make_file('templates/dev.h', h_name, dev=dev, blocks=make_blocks(dev))
    devs.append(dev)

make_file('templates/devices.cpp', 'devices/devices.cpp', devs=devs)
