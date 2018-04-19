#!/usr/bin/env bash

LOCALE=teagle
TASK=$1
TEST=$2

CYAN="\033[0;36m"
RED="\033[1;31m"
BLUE="\033[0;34m"
YELLOW="\033[0;33m"
GREEN="\033[1;32m"
ENDCOLOR="\033[0m"

log() {
  STR="[$CYAN""AUTOTEST""$ENDCOLOR] $1"
  echo -e $STR
}

log "Setting up Ctrl-C handler (will kill all spawned processes when any one is killed)"

trap 'trap - SIGTERM && log "Caught interrupt, crash, or kill; killing all subprocesses and exiting!" && kill -- -$$' SIGINT SIGTERM EXIT

if [[ -z "$TASK" ]]; then
  TASK=Full
  log "No task specified - defaulting to $TASK."
fi

if [[ -z "$TEST" ]]; then
  TEST=$(echo "$TASK" | tr [:upper:] [:lower:])
  TEST="${CUAUV_SOFTWARE}/mission/tests/$TEST.scm"
  log "No test specified - defaulting to $TEST."
fi

log "Resetting SHM; killing any attached processes!"
auv-shm RESET

# loki
log "Setting up control covariances..."
if true; then
  auv-shm-cli aslam_settings control_covariance_x 0.5
  auv-shm-cli aslam_settings control_covariance_y 0.5
fi

log "Testing task ""$BLUE""$TASK""$ENDCOLOR"" in locale ""$YELLOW""$LOCALE.""$ENDCOLOR"

export CUAUV_LOCALE=$LOCALE

log "Setting simulator defaults..."

auv-sim-defaults

log "Enabling positional controls with heading optimization..."

auv-shm-cli navigation_settings enabled 1
auv-shm-cli navigation_settings position_controls 1
auv-shm-cli navigation_settings optimize 1

auv-shm-cli gpio wall_1 1
auv-shm-cli gpio wall_2 1
auv-shm-cli gpio wall_3 1

log "Starting auv-aslamd..."

auv-aslamd > /dev/null &
aslam_pid=$!

log "Starting log reader..."

auv-terminal auv-lr &
lr_pid=$1

log "Selecting and starting vision modules..."

launch() {
  STR="Launching vision module ""$CYAN""$1""$ENDCOLOR"" with capture source "$YELLOW""$2""$ENDCOLOR""
  log $STR
  $1 $2 &
}

mod_dir="${CUAUV_SOFTWARE}/vision/modules"

case $TASK in
  buoys|Buoys)
    launch "${mod_dir}/buoys.py" forward_left
  ;;
  bins|Bins)
    launch "${mod_dir}/bins.py" downward
  ;;
  wire|Wire)
    launch "${mod_dir}/wire.py" forward_right
  ;;
  torpedoes|Torpedoes)
    launch "${mod_dir}/torpedoes.py" forward_left
  ;;
  pipes|Pipes)
    launch "${mod_dir}/pipes.py" forward_left
  ;;
  full|Full)
    launch "${mod_dir}/pipes.py" downward
    launch "${mod_dir}/buoys.py" forward_left
    launch "${mod_dir}/buoys.py" forward_right
    launch "${mod_dir}/wire.py" forward_right
  ;;
  recovery|Recovery)
    launch "${mod_dir}/recovery.py" downward
  ;;
  *)
    log "Task unknown, not launching any vision modules."
  ;;
esac

log "Initiating Peacock test..."

peck i "$TEST" &
peacock_pid=$!

sleep 1

log "Starting visualizer with simulated vision enabled..."

case $TASK in
  recovery|Recovery) VCFG="recovery.cfg" ;;
  *) VCFG="world.cfg" ;;
esac

auv-visualizer "${CUAUV_SOFTWARE}/visualizer/${VCFG}" -v &
viz_pid=$!

log "Setup complete. Waiting for Peacock test to complete..."

wait $peacock_pid

EXIT=$?
log "Peacock exit code: $EXIT"

if [[ "$EXIT" -ne "0" ]]; then
  log "$RED""Peacock exit code nonzero! ""$ENDCOLOR"
  log "Ensure that the testfile $TEST and the task $TASK exist in the correct locations."
  log "Presuming that they are, your task must have failed!"
  exit 1
else
  log "$GREEN""Peacock exit code OK.""$ENDCOLOR"
fi
