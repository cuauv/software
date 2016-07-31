#!/usr/bin/env python3

# generate_dshm.py parses the file at libshm/vars.conf using libshm/parse.py
# and generates:
# - dshm_index
# - c/dshm.h
# - c/dshm.c

# Note: this imposes the following additional "restrictions" on vars.conf:
# - groups and variables names are case insensitive
# - "__" (double underscore) is illegal in group or variable names
# - <group name>__<variable name> cannot be the same for any two variables

import sys
import os

from libshm.parse import parse, ParseError
from lib import pyratemp as pyra

GROUP_MEMBER_DELIMITER = "__"
TYPES = ["int", "float", "double", "string", "bool"]

TPL_DSHM_H = pyra.Template(filename="libshm/dtemplates/dshm.h")
TPL_DSHM_C = pyra.Template(filename="libshm/dtemplates/dshm.c")
TPL_DSHM_SCM = pyra.Template(
        filename="libshm/dtemplates/dshm.scm",
        data={"srename": lambda gn, n: gn.replace("_", "-") + "." + n.replace("_", "-")})

OUT_DSHM_H = "libshm/c/dshm.h"
OUT_DSHM_C = "libshm/c/dshm.c"
OUT_DSHM_SCM = "libshm/scm/dshm.scm"
OUT_DSHM_IDX = "libshm/index/dshm_index"

def fatal(msg):
    print("generate_dshm.py: " + msg, file=sys.stderr)
    sys.exit(1)

# forward declaration for try/catch
groups = {}
# Input
try:
    groups = parse("libshm/vars.conf")
except ParseError as e:
    print ("Failed while parsing vars.conf: " + str(e))
    sys.exit(1)

# Output
# list of (flattened_name, groupname, var)
flattened = []

# Intermediate

# map of types to their index in TYPES
typemap = {}
for i, t in enumerate(TYPES):
    typemap[t] = i

# map of types to the number of variables with that type
typecount = {}
for t in TYPES:
    typecount[t] = 0

# list of (groupname, varname), to check for duplicate flattened names
flattened_names = {}

for g in groups:
    gn = g["groupname"]

    if GROUP_MEMBER_DELIMITER in gn:
        fatal("Group name '" + gn + "' contains '" + GROUP_MEMBER_DELIMITER + "', the group-member delimiter.")

    for vn, v in sorted(g["vars"].items()):
        if GROUP_MEMBER_DELIMITER in vn:
            fatal("Variable name '" + vn + "' (in group '" + gn + "') contains '" + GROUP_MEMBER_DELIMITER + "', the group-member delimiter.")

        flattened_name = (gn + GROUP_MEMBER_DELIMITER + vn).upper()

        if flattened_name in flattened_names:
            dup = flattened_names[flattened_name]
            fatal("Flattened name '" + flattened_name + "' was generated for both '" + gn + "." + vn + "' and '" + dup[0] + "." + dup[1] + "'.")

        if v["type"] not in TYPES:
            fatal("Variable " + gn + "." + vn + " has unexpected type '" + v["type"] + "'.")

        typecount[v["type"]] += 1

        flattened_names[flattened_name] = (gn, vn)
        flattened.append((flattened_name, gn, v))

# sort flattened, using the index of the variable type in TYPES as the sort index
# types earlier in TYPES will have lower indices & sorted sorts in increasing order
flattened = sorted(flattened, key=lambda x: typemap[x[2]["type"]])

# map from types to list of (flattened_name, groupname, name, id) of that type
tvmap = {}
# map from types to their bounds [a, b)
tbmap = {}

# current offset, the number of variables of previous types we have gone through
i = 0
for t in TYPES:
    tvmap[t] = [(x[0], x[1], x[2]["name"], j) for j, x in enumerate(flattened[i:i+typecount[t]], start=i)]
    tbmap[t] = (i, i+typecount[t])
    i += typecount[t]
i = None

# Output

if not os.path.exists("libshm/scm"):
    os.makedirs("libshm/scm")

if not os.path.exists("libshm/index"):
    os.makedirs("libshm/index")

# write the names, one per line, to OUT_DSHM_IDX
with open(OUT_DSHM_IDX, 'w') as f:
    f.write("\n".join(["%s.%s" % (x[1], x[2]["name"]) for x in flattened]))


with open(OUT_DSHM_H, 'w') as f:
    f.write(TPL_DSHM_H(tvmap=tvmap, tbmap=tbmap))

with open(OUT_DSHM_SCM, 'w') as f:
    f.write(TPL_DSHM_SCM(tvmap=tvmap))

with open(OUT_DSHM_C, 'w') as f:
    f.write(TPL_DSHM_C(tvmap=tvmap, tbmap=tbmap))
