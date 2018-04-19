#!/usr/bin/env bash

vehicle=${CUAUV_VEHICLE}

filename="50-${CUAUV_VEHICLE}serial.rules"
CUAUV_VEHICLE=$vehicle sudo ln -s "$(pwd)/$filename" "/etc/udev/rules.d/$filename"
