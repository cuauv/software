#!/usr/bin/env bash
export PATH=$PATH:/home/software/trunk/link-stage
export PYTHONPATH=/home/software/trunk

sudo ip link set can0 up type can bitrate 200000
auv-cand can0 
