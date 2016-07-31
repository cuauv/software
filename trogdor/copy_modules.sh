#!/bin/sh

sudo rm /etc/systemd/system/auv-*
sudo cp auv-systemd-units/* /etc/systemd/system/
sudo systemctl daemon-reload

