#!/bin/bash

sudo ip link set can0 down

trogdor start

sleep 5

# Screen is awesome!
screen -d -m auv-mission-runner

while true; do
  killed=$(auv-shm-cli switches hard_kill | cut -d' ' -f 5)
  if [ ${killed} -eq 0 ]; then
    sudo ip link set can0 down
    sleep 1;
    continue;
  fi

  auv-syscheck
  if [ $? -ne 0 ]; then
    sudo ip link set can0 down
  else
    sudo ip link set can0 up type can bitrate 200000
  fi
done
