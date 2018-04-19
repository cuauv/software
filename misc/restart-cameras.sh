#!/usr/bin/env bash

trogdor stop visiongui
sleep 1
trogdor stop cameras
sleep 1
trogdor stop ueye
trogdor stop ueye
sudo /etc/init.d/ueyeethdrc stop
sleep 2
trogdor start ueye
sleep 1
trogdor start cameras
trogdor start cameras
sleep 1
trogdor start visiongui
