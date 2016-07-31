#!/usr/bin/env python3

import filecmp
import os
import shutil
import sys

from lib import pyratemp
from libshm import parse

if os.getcwd().split('/')[-1] != 'libshm':
    os.chdir('libshm')

# create directories to store generated files if they don't exist
generated_dirs = ['c', 'py', 'haskell']
for d in generated_dirs:
    if not os.path.exists(d):
        os.makedirs(d)

# constants to use in file generation
ctypes=dict(double='double', float='float', int='int32_t', bool='int32_t', int16='int16_t', int32='int32_t', string='char*')
ptypes=dict(double='c_double', float='c_float', int='c_int', bool='c_int', string='c_char_p')
scmtypes=dict(double='double', float='float', int='int32', bool='bool', int16='int16', int32='int32', string='c-string')
ocamlctypes=dict(double='double', float='float', int='int', bool='bool', int16='int16_t', int32='int32_t', string='string')
ocamltypes=dict(double='float', float='float', int='int', bool='bool', int16='Int16.t', int32='Int32.t', string='string')
haskelltypes=dict(double='Double', float='Double', int='Int', bool='Bool', int16='Int', int32='Int', string='CString')

# forward declaration for try/catch
groups = {}
#read the config file
try:
    groups = parse.parse('vars.conf')
except parse.ParseError as e:
    print(("Failed while parsing vars.conf: " + str(e)))
    sys.exit(1)

# read a file, replace stuff, write it out
# only overwrite files if the contents will change
def make_file(f_in, f_out, old_files=None):
    f_tmp = '%s.swp' % f_out
    t = pyratemp.Template(filename=f_in)
    out=open(f_tmp, 'w')
    out.write(t(groups=groups))
    out.close()
    if os.path.exists(f_out):
        if old_files is not None:
            old_files.remove(f_out)
        if filecmp.cmp(f_tmp, f_out):
            # Nothing changed, don't touch the file.
            os.remove(f_tmp)
            return
    shutil.move(f_tmp, f_out)


def make_py_file(f_in, f_out, g, old_files=None):
    t = pyratemp.Template(filename=f_in)
    out = open(f_out, 'w')
    out.write(t(g=g))
    out.close()

    if old_files is not None and f_out in old_files:
        old_files.remove(f_out)

def make_scm_file(f_in, f_out):
    t = pyratemp.Template(
            filename=f_in,
            data={"srename": lambda gn, n: gn.replace("_", "-") + "." + n.replace("_", "-"),
                  "srenameg": lambda gn: gn.replace("_", "-")})
    with open(f_out, 'w') as out:
        out.write(t(groups=groups))

def make_ocaml_file(f_in, f_out):
    t = pyratemp.Template(
            filename=f_in,
            data={"renameg": lambda g: g['groupname'].title()})
    with open(f_out, 'w') as out:
        out.write(t(groups=groups))

def make_haskell_file(f_in, f_out):
    t = pyratemp.Template(
            filename=f_in,
            data={})
    with open(f_out, 'w') as out:
      out.write(t(groups=groups))

for g in groups:
    g['varnames'] = sorted(g['vars'].keys())
    g['varnames'].sort()
    for k in g['vars']:
        g['vars'][k]['ctype'] = ctypes[g['vars'][k]['type']]
        g['vars'][k]['ptype'] = ptypes[g['vars'][k]['type']]
        g['vars'][k]['scmtype'] = scmtypes[g['vars'][k]['type']]
        g['vars'][k]['ocamlctype'] = ocamlctypes[g['vars'][k]['type']]
        g['vars'][k]['ocamltype'] = ocamltypes[g['vars'][k]['type']]
        g['vars'][k]['haskelltype'] = haskelltypes[g['vars'][k]['type']]

files = ['shm.c',
         'shm.h',
         'log.cpp',
         'log.h',
         'serialize.cpp',
         'serialize.h',
         'shm_diagnose.cpp',
         'vars.h',
         'vars.c',
         'watcher.h',
         'watcher.c',
         'dynamic.h',
         'dynamic.cpp',
    ]

old_files = set()
for d in generated_dirs:
    old_files = old_files.union(set([os.path.join(d, f) for f in os.listdir(d)]))

for f in files:
    make_file('templates/' + f, 'c/' + f, old_files)

make_file('templates/init.py', 'py/__init__.py', old_files)
make_file('templates/watchers.py', 'py/watchers.py', old_files)
make_file('templates/base.py', 'py/base.py', old_files)

# make py files
for g in groups:
    file_name = g['groupname'] + '.py'
    make_py_file('templates/group.py', os.path.join('py', file_name), g, old_files)

make_scm_file('templates/shm.scm', 'scm/shm.scm')
make_ocaml_file('templates/CUAUV_shm.ml', 'ocaml/CUAUV_shm.ml')
make_ocaml_file('templates/CUAUV_shm.mli', 'ocaml/CUAUV_shm.mli')

make_haskell_file('templates/SHM.hs', 'haskell/SHM.hs')

# XXX is this necessary? jyc57
#for f in old_files:
#    print(f)
#    try:
#        if os.path.isdir(f):
#            os.rmdir(f)
#        else:
#            os.remove(f)
#    except OSError:
#        print "Could not remove %s" % f
