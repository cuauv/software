#!/usr/bin/env bash

# Called by the chicken-lib rule in configure.py.
# $1: The directory of the CHICKEN extension to chicken-install in.
# $2: The path of a fake file to touch so that the build system can have some
#     idea of when things were last built.
# All paths are relative to $CUAUV_SOFTWARE.

set -e

EXTENSION_DIR=$1

cd $CUAUV_SOFTWARE$EXTENSION_DIR
CSC_OPTIONS="-C -Wno-cpp $CSC_OPTIONS" chicken-install >/dev/null

touch $CUAUV_SOFTWARE$2
