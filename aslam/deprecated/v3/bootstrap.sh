#!/bin/sh

function log () {
  echo "[`date -u +"%Y/%m/%d %H:%M:%S UTC"`] (ASLAM-BOOTSTRAP) $1"
}

function invoke () {
  log "Invoking \"$1\"."
  /bin/sh -c "$1"
}
