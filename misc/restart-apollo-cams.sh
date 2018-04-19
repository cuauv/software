#!/usr/bin/env bash

trogdor stop visiongui
sleep 1
trogdor stop cameras
sleep 1
kill -TERM $(pgrep ximea)
kill -TERM $(pgrep ueye)
sleep 2
trogdor start cameras
trogdor start cameras
sleep 1
trogdor start visiongui
