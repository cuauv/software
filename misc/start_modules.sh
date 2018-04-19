#!/usr/bin/env bash

LOGS=$CUAUV_LOG/current
MDIR=vision/modules
GREEN="\033[1;32m"
ENDCOLOR="\033[0m"

shutdown() {
  kill $pids
  exit 0
}

trap 'log "Caught interrupt, crash, or kill; killing all subprocesses and exiting!" && shutdown' INT TERM HUP

log () {
  STR="[$GREEN`date -u +"%Y/%m/%d %H:%M:%S UTC"`$ENDCOLOR] $1"
  echo -e $STR
}

pids=""
startm() {
  CMD="${CUAUV_SOFTWARE}/${MDIR}/$1.py $2"
  LGN="$1""@""$2""-module"
  log "Forking \"$CMD &> $LOGS/$LGN.log\"."
  echo "Starting $CMD at `date -u +"%Y/%m/%d %H:%M:%S UTC"`" >> $LOGS/$LGN.log
  stdbuf -oL -eL $CMD &>> $LOGS/$LGN.log &
  pids="$pids $!"
}

ALL=`cat ${CUAUV_SOFTWARE}/conf/$CUAUV_VEHICLE.json | jq -c ".vision_modules | .[]"`

for MODULE in ${ALL}
do
  CAPTURE_SOURCES=`echo "$MODULE" | jq -c -r ".capture_sources | .[]"`
  MODULE_NAME=`echo "$MODULE" | jq -c -r ".name"`
  for SOURCE in ${CAPTURE_SOURCES}
  do
    log "Attempting to start module \"$MODULE_NAME\" with capture source \"$SOURCE\""
    startm $MODULE_NAME $SOURCE
  done
done

while true
do
  sleep 1
done
