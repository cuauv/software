#!/usr/bin/env bash


LOGS=$CUAUV_LOG/current
MDIR=vision/modules

while :; do
    CMD="${CUAUV_SOFTWARE}/${MDIR}/$1.py $2"
    stdbuf -oL -eL $CMD >> $LOGS/$3.log
    sleep 1;
done
