#!/usr/bin/env bash


LOGS=$CUAUV_LOG/current
MDIR=vision/modules
CMD="${CUAUV_SOFTWARE}/${MDIR}/$1.py $2"

shutdown() {
    kill $child_pid
    exit 0
}

trap shutdown INT TERM HUP

while :; do
    stdbuf -oL -eL $CMD >> $LOGS/$3.log &
    child_pid=$!
    wait
done
