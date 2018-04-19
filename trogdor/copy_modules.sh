#!/usr/bin/env bash

sudo rm /etc/systemd/system/auv-*
sudo cp auv-systemd-units/* /etc/systemd/system/
sudo systemctl daemon-reload

