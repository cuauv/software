#!/usr/bin/env bash
set -xeuo pipefail

export DEBIAN_FRONTEND=noninteractive
rm -rf /var/lib/apt/lists/*
apt-get update -o Acquire::CompressionTypes::Order::=gz -y
apt-get upgrade -y

. $@

apt-get clean
rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /root/.cache/
