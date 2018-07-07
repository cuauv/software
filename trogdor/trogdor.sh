#!/usr/bin/env bash

# Hack to allow devices to appear in /dev when running on boot.
early=$(echo "$(cat /proc/uptime | cut -d' ' -f 1) < 10" | bc)

if [[ $early = "1" ]]; then
  sleep 10
fi

# CONFIGURATION

ROOT=$CUAUV_SOFTWARE
LOGS=$CUAUV_LOG/current # Should be set up by auv-pooltest
BIN=$ROOT/link-stage

# Very important variables
export PATH=$PATH:$ROOT/link-stage
export PYTHONPATH=$ROOT

# PORT MAPPINGS

# GX_PORT=$(readlink -f /dev/serial/by-id/usb-CUAUV_Pastor_2_AUV-PASTOR2-if00-port0)
GX_PORT=$(readlink -f /dev/serial/by-id/usb-FTDI_Quad_RS232-HS-if00-port0)
DVL_PORT=/dev/serial/by-id/usb-CUAUV_PASTOR_4_AUV-PASTOR4-if03-port0

# CONFIGS

VISION_CONFIG=$ROOT/vision/configs/master.yaml

# SERVICES

SUBMARINE=$CUAUV_VEHICLE

if [ "$SUBMARINE" = "castor" ]; then
  SERVICES=(seriald gx4d linearizerd dvld kalmand navigated controld3 shmserver ueye
  logging visiongui cameras webgui hydromathd modules deadman uptime )
elif [ "$SUBMARINE" = "pollux" ]; then
  SERVICES=(seriald gx4d kalmand navigated controld3 shmserver logging
  visiongui cameras webgui modules deadman uptime)
else
  echo "Unsupported submarine! Must be set to one of { artemis, apollo }!"
fi

# COLORS

GRAY="\033[0;30m"
CYAN="\033[0;36m"
RED="\033[1;31m"
BLUE="\033[0;34m"
YELLOW="\033[0;33m"
GREEN="\033[1;32m"
ENDCOLOR="\033[0m"

# FUNCTIONS

log () {
  STR="[$CYAN`date -u +"%Y/%m/%d %H:%M:%S UTC"`$ENDCOLOR] ($YELLOW""TROGDOR""$ENDCOLOR) $1"
  echo -e $STR
  echo $STR &>> $LOGS/trogdor.log
}

invoke () {
  log "Invoking \"$1\"."
  $1
}

fork () {
  log "Forking \"$1 &> $LOGS/$2.log\"."
  echo "Starting $1 at `date -u +"%Y/%m/%d %H:%M:%S UTC"`" >> $LOGS/$2.log
  stdbuf -oL -eL $1 &>> $LOGS/$2.log &
}

pkill () {
  log "Killing \"$1\"."
  PIDS=`pids $1`
  if [ -z "$PIDS" ]; then
      log "No PIDs found for \"$1\"."
  else
      invoke "kill $PIDS"
  fi
}

pids () {
  pgrep -fl "$*" | grep -v "grep" | grep -v "vim" | grep -v "emacs" | cut -d' ' -f1
}

usage () {
  echo "Usage: {t / trogdor} {start | stop | restart | status} SERVICE"
}

servicestatus () {
  if [ -z "`pids $1`" ]; then
    log "$RED""$2""$ENDCOLOR"
  else
    log "$GREEN""$2""$ENDCOLOR"
  fi
}

assertservice () {
  if [ -z "`pids $2`" ]; then
    log "$1 seems to be ""$RED""DOWN""$ENDCOLOR""; restarting."
    trogdor hidden_start $1
  else
    log "$1 seems to be ""$GREEN""UP""$ENDCOLOR""."
  fi
}

COMMAND=$1
SERVICE=$2

if [ -z "$COMMAND" ]; then
    COMMAND="status"
fi

if [ -z "$SERVICE" ]; then
    log "No service specified; executing on all known."
    for SERVICE in ${SERVICES[@]}
    do
        trogdor $COMMAND $SERVICE
    done
    exit 0
fi

ueyeCmd=""
if [[ -n "$NIXOS" ]]; then
	ueyeCmd="ueyeethdrc"
else
	ueyeCmd="/etc/init.d/ueyeethdrc"
fi

case $COMMAND in
    hidden_start)
        case $SERVICE in
            aslam|aslamd) fork "auv-aslamd" "aslamd" ;;
            linearizer|linearizerd) fork "auv-linearizerd" "linearizerd" ;;
            seriald|serial) fork "auv-seriald" "seriald" ;;
            gx4d|gx4) fork "auv-3dmgx4d $GX_PORT" "gx4d" ;;
            gx1d|gx1) fork "auv-3dmgd $GX_PORT" "gx1d" ;;
            dvld|dvl) fork "auv-dvld $DVL_PORT" "dvld" ;;
            kalmand|kalman) fork "auv-kalmand" "kalmand" ;;
            navigated|navigate) fork "auv-navigated" "navigated" ;;
            controld3|controld|control) fork "auv-controld3" "controld3" ;;
            shmserver) fork "auv-shm server" "shmserver" ;;
            log|logs|logger|logging) fork "auv-ld" "auv-ld" ;;
            ueye) invoke "sudo $ueyeCmd start" ;;
            led) fork "auv-led daemon" "led" ;;
            deadman) fork "auv-deadman" "deadman" ;;
            uptime) fork "auv-uptimed" "uptime" ;;
            visiongui) fork "auv-vision-gui" "vision-gui" ;;
            webgui) invoke "cd /home/software/cuauv/software/webserver" && fork "auv-webserver" "webserver" ;;
            hydromathd) fork "auv-hydromathd" "hydromathd" ;;
            cameras) fork "auv-start-cameras" "start-cameras" ;;
            modules) fork "auv-start-modules" "start-modules" ;;
            *) log "Service \"$SERVICE\" not found; aborting." ;;
        esac
    ;;

    stop)
        case $SERVICE in
            aslam|aslamd) pkill "auv-aslamd" ;;
            linearizer|linearizerd) pkill "auv-linearizerd" ;;
            seriald|serial) pkill "auv-seriald" ;;
            gx4d|gx4) pkill "auv-3dmgx4d" ;;
            gx1d|gx1) pkill "auv-3dmgd" ;;
            dvld|dvl) pkill "auv-dvld" ;;
            kalmand|kalman) pkill "auv-kalmand" ;;
            navigated|navigate) pkill "auv-navigated" ;;
            controld3|controld|control) pkill "auv-controld3" ;;
            shmserver) pkill "auv-shm server" ;;
            log|logs|logger|logging) pkill "auv-ld" ;;
            ueye) invoke "sudo $ueyeCmd stop" ;;
            led) pkill "/home/software/misc/led.py" ;;
            deadman) pkill "auv-deadman" ;;
            uptime) pkill "auv-uptimed" ;;
            visiongui) pkill "auv-vision-gui" ;;
            webgui) pkill "auv-webserver" ;;
            hydromathd) pkill "auv-hydromathd" ;;
            cameras) pkill "auv-start-cameras" ;;
            modules) pkill "auv-start-modules" ;;
            *) log "Service \"$SERVICE\" not found; aborting." ;;
        esac
    ;;

    restart)
        case $SERVICE in
          seriald|serial)
            trogdor stop $SERVICE
            sleep 3
            trogdor hidden_start $SERVICE
          ;;
          *)
            trogdor stop $SERVICE
            trogdor hidden_start $SERVICE
          ;;
        esac
    ;;

    status)
        case $SERVICE in
            aslamd|aslam) servicestatus "auv-aslamd" "aslamd" ;;
            linearizer|linearizerd) servicestatus "auv-linearizerd" "linearizerd" ;;
            seriald|serial) servicestatus "auv-seriald" "seriald" ;;
            gx4d|gx4) servicestatus "auv-3dmgx4d" "gx4d" ;;
            gx1d|gx1) servicestatus "auv-3dmgd" "gx1d" ;;
            dvld|dvl) servicestatus "auv-dvld" "dvld" ;;
            kalmand|kalman) servicestatus "auv-kalmand" "kalmand" ;;
            navigated|navigate) servicestatus "auv-navigated" "navigated" ;;
            controld3|controld|control) servicestatus "auv-controld3" "controld3" ;;
            log|logs|logger|logging) servicestatus "auv-ld" "logging" ;;
            ueye) servicestatus "ueyeethd" "ueye" ;;
            shmserver) servicestatus "auv-shm server" "shmserver" ;;
            led) servicestatus "/home/software/trunk/misc/hydro_reset.py" "led" ;;
            deadman) servicestatus "auv-deadman" "deadman" ;;
            uptime) servicestatus "auv-uptimed" "uptime" ;;
            visiongui) servicestatus "auv-vision-gui" "visiongui" ;;
            webgui) servicestatus "auv-webserver" "webgui" ;;
            hydromathd) servicestatus "auv-hydromathd" "hydromathd" ;;
            cameras) servicestatus "auv-start-cameras" "cameras" ;;
            modules) servicestatus "auv-start-modules" "modules" ;;
            *) log "Service \"$SERVICE\" not found; aborting." ;;
        esac
    ;;

    start)
        case $SERVICE in
            aslamd|aslam) assertservice "aslamd" "auv-aslamd" ;;
            linearizer|linearizerd) assertservice "linearizerd" "auv-linearizerd" ;;
            seriald|serial) assertservice "serial" "auv-seriald" ;;
            gx4d|gx4) assertservice "gx4d" "auv-3dmgx4d $GX4_PORT" ;;
            gx1d|gx1) assertservice "gx1d" "auv-3dmgd $GX1_PORT" ;;
            dvld|dvl) assertservice "dvld" "auv-dvld $DVL_PORT" ;;
            kalmand|kalman) assertservice "kalmand" "auv-kalmand" ;;
            navigated|navigate) assertservice "navigated" "auv-navigated" ;;
            controld3|controld|control) assertservice "controld3" "auv-controld3" ;;
            log|logs|logger|logging) assertservice "logging" "auv-ld" ;;
            shmserver) assertservice "shmserver" "auv-shm server" ;;
            led) assertservice "led" "auv-led daemon" ;;
            deadman) assertservice "deadman" "auv-deadman" ;;
            uptime) assertservice "uptime" "auv-uptimed" ;;
            visiongui) assertservice "visiongui" "auv-vision-gui" ;;
            webgui) assertservice "webgui" "auv-webserver" ;;
            hydromathd) assertservice "hydromathd" "auv-hydromathd" ;;
            cameras) assertservice "cameras" "auv-start-cameras" ;;
            modules) assertservice "modules" "auv-start-modules" ;;
            ueye)
                if [ -z "`pids ueyeethd`" ]; then
                    trogdor stop ueye
                    trogdor hidden_start ueye
                else
                    log "ueye seems to be ""$GREEN""UP""$ENDCOLOR""."
                fi
            ;;
            *) log "Service \"$SERVICE\" not found; aborting." ;;
        esac
    ;;

    *)
        usage
    ;;
esac
