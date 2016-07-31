#!/usr/bin/env python3

import os
import filecmp
import shutil
import argparse

from lib import pyratemp

import config

def valid_template(fname):
    return len(fname) > 0 and fname[0] != '.'

def make_file(f_in, f_out, cfg):
    f_tmp = '%s.swp' % f_out
    t = pyratemp.Template(filename=f_in)
    with open(f_tmp, 'w') as out:
        out.write(t(**cfg))
    if os.path.exists(f_out) and filecmp.cmp(f_tmp, f_out):
        # Nothing changed, don't touch the file
        os.remove(f_tmp)
        return
    shutil.move(f_tmp, f_out)

if os.getcwd().split('/')[-1] != 'can':
    os.chdir('can')

def main():
    gen_dir = "gen"
    if not os.path.exists(gen_dir):
        os.makedirs(gen_dir)

    cfg = config.load_config('config/hardware.json', 'config/devices.json')
    for filename in filter(valid_template, os.listdir('templates')):
        in_file = os.path.join('templates', filename)
        out_file = os.path.join(gen_dir, filename)
        make_file(in_file, out_file, cfg)

if __name__ == '__main__':
    main()
