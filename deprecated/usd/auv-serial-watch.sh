#! /bin/bash

sleeptime=2

while getopts 's:' OPTION; do
    case $OPTION in
    s)  sleeptime="$OPTARG"
        ;;
    esac
done
shift $(($OPTIND - 1))

while true; do
    if [ -z $(pgrep auv-seriald) ]; then
        echo Restarting auv-seriald at `date` >> /var/log/auv/seriald.log
        pushd /root/trunk/usd &> /dev/null
        auv-seriald master.conf 2>> /var/log/auv/seriald.log 1>&2 &
        popd &> /dev/null
    fi
    sleep $sleeptime
done
