#!/usr/bin/env bash
set -xeuo pipefail

export DEBIAN_FRONTEND=noninteractive
rm -rf /var/lib/apt/lists/*
apt-get update -y -o Acquire::CompressionTypes::Order::=gz
apt-get upgrade -y

. $@

apt-get -y clean all
rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /root/.cache/
