#!/usr/bin/env bash

set -e

export CSC_OPTIONS="-C -Wno-cpp $CSC_OPTIONS"

# Because ninja tries to be too smart and buffers output...
# This means we can't have things build and call sudo.

if [ -z $1 ] || echo $@ | grep "\bshm\b" >/dev/null ; then
    echo "CUAUV-SHM"
    cd $CUAUV_SOFTWARE/libshm/scm
    chicken-install -s >/dev/null
fi

if [ -z $1 ] || echo $@ | grep "\bfishbowl\b" >/dev/null ; then
    echo "FISHBOWL"
    cd $CUAUV_SOFTWARE/peacock/fishbowl
    chicken-install -s >/dev/null
fi

if [ -z $1 ] || echo $@ | grep "\bpeacock\b" >/dev/null ; then
    echo "PEACOCK"
    cd $CUAUV_SOFTWARE/peacock
    chicken-install -s >/dev/null
fi

if [ -z $1 ] || echo $@ | grep "\bpeck\b" >/dev/null ; then
    echo "PECK"
    csc $CUAUV_SOFTWARE/peacock/peck -o $CUAUV_SOFTWARE/link-stage/peck
fi
